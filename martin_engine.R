# =============================================================================
# MARTIN (RBA) -- PART B / STEP 2: THE MODEL ENGINE
# -----------------------------------------------------------------------------
# Turns the model text decoded from the RBA workfile (martin_workfile.R) into
# R functions and solves it quarter by quarter, the way EViews does.
#
# Paper reference: RDP 2019-07 Section 2.2 "Model Framework" (p. 2-4): MARTIN is
# "an economy-wide model ... most variables are endogenous ... determined
# jointly as a system".  Section 4 (p. 8-9) describes the equation forms: error-
# correction models (ECMs) plus identities; Online Appendix lists every equation.
#
# How it works
#   1. PARSE   each line "LHS = RHS" into an expression tree (AST).
#   2. SOLVE-FOR the left-hand-side variable.  EViews equations come in five
#      shapes; each is inverted to give the variable's level this quarter:
#          X          = rhs         ->  X_t = rhs
#          LOG(X)     = rhs         ->  X_t = exp(rhs)
#          D(X)       = rhs         ->  X_t = X_{t-1} + rhs
#          D(LOG(X))  = rhs         ->  X_t = X_{t-1} * exp(rhs)
#          (DLOG(X) is the same as D(LOG(X)))
#      An "add factor" (@ADD X X_A in the model text) is added to rhs first;
#      it is the "judgement" term discussed on p. 4 of the paper.
#   3. ORDER   the equations: if equation A uses variable B in the same quarter,
#      B is solved first.  Groups of equations that depend on each other form a
#      simultaneous block (EViews reports 3 blocks: 49 recursive, 53
#      simultaneous, 115 recursive equations -- see the solve log in the file).
#   4. SOLVE   each block by Newton's method with a numerical Jacobian.
#
# Exogenous variables (population trend targets, dummies, world variables,
# policy-rule weights ...) are simply columns of the data matrix whose values
# are read, never solved.
# =============================================================================

# ---- 1. Tokeniser and parser ----------------------------------------------------

tokenise <- function(s) {
  pat <- "[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?|[A-Za-z_][A-Za-z0-9_]*(?:\\.@[A-Za-z]+)?|[-+*/^()]"
  toks <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  toks
}

FUNCS <- c("LOG", "D", "DLOG", "EXP")

# Recursive-descent parser.  Returns an AST made of nested lists:
#   num(v) | var(name, lag) | op(o, a, b) | neg(a) | fn(f, a) | coef(obj, i)
parse_expr <- function(s) {
  toks <- tokenise(toupper(s)); pos <- 1L
  peek <- function() if (pos <= length(toks)) toks[pos] else ""
  take <- function() { t <- toks[pos]; pos <<- pos + 1L; t }
  expect <- function(x) { if (take() != x) stop("parse error: expected ", x, " in: ", s) }

  expr <- function() {
    a <- term()
    while (peek() %in% c("+", "-")) { o <- take(); a <- list(t = "op", o = o, a = a, b = term()) }
    a
  }
  term <- function() {
    a <- unary()
    while (peek() %in% c("*", "/")) { o <- take(); a <- list(t = "op", o = o, a = a, b = unary()) }
    a
  }
  unary <- function() {
    if (peek() == "-") { take(); return(list(t = "neg", a = unary())) }
    if (peek() == "+") { take(); return(unary()) }
    power()
  }
  power <- function() {
    a <- atom()
    if (peek() == "^") { take(); a <- list(t = "op", o = "^", a = a, b = unary()) }
    a
  }
  atom <- function() {
    tk <- take()
    if (grepl("^[0-9.]", tk)) return(list(t = "num", v = as.numeric(tk)))
    if (tk == "(") { e <- expr(); expect(")"); return(e) }
    if (grepl("\\.@COEF$", tk)) {                         # EQ_xxx.@COEF(i): estimation-object coefficient
      expect("("); i <- as.integer(take()); expect(")")
      return(list(t = "coef", obj = sub("\\.@COEF$", "", tk), i = i))
    }
    if (tk %in% FUNCS) {
      expect("("); e <- expr(); expect(")")
      return(switch(tk,
        LOG  = list(t = "fn", f = "log", a = e),
        EXP  = list(t = "fn", f = "exp", a = e),
        D    = diff1(e),                                   # D(e)    = e - e(-1)
        DLOG = diff1(list(t = "fn", f = "log", a = e))))   # DLOG(e) = log(e) - log(e(-1))
    }
    # variable, optionally followed by a lag "(-k)"
    lag <- 0L
    if (peek() == "(") { take(); expect("-"); lag <- as.integer(take()); expect(")") }
    list(t = "var", n = tk, lag = lag)
  }

  out <- expr()
  if (pos <= length(toks)) stop("trailing tokens in: ", s)
  out
}

# Shift every variable in an expression back k quarters
shift_ast <- function(a, k) {
  switch(a$t,
    num = a, coef = a,
    var = { a$lag <- a$lag + k; a },
    neg = { a$a <- shift_ast(a$a, k); a },
    fn  = { a$a <- shift_ast(a$a, k); a },
    op  = { a$a <- shift_ast(a$a, k); a$b <- shift_ast(a$b, k); a })
}
diff1 <- function(e) list(t = "op", o = "-", a = e, b = shift_ast(e, 1L))

ast_vars <- function(a, lag0_only = FALSE) {
  switch(a$t,
    num = , coef = character(0),
    var = if (!lag0_only || a$lag == 0L) a$n else character(0),
    neg = , fn = ast_vars(a$a, lag0_only),
    op  = c(ast_vars(a$a, lag0_only), ast_vars(a$b, lag0_only)))
}

# ---- 2. Turn an AST into an R function --------------------------------------------

# 'cols' maps variable name -> column of the data matrix Z.  Z[t - lag, col].
ast_to_r <- function(a, cols, coefs) {
  switch(a$t,
    num  = format(a$v, digits = 17),
    var  = {
      j <- cols[[a$n]]; if (is.null(j)) stop("variable not in data: ", a$n)
      if (a$lag == 0L) sprintf("Z[t,%dL]", j) else sprintf("Z[t-%dL,%dL]", a$lag, j)
    },
    coef = format(coefs[[a$obj]][a$i], digits = 17),
    neg  = paste0("(-", ast_to_r(a$a, cols, coefs), ")"),
    fn   = paste0(a$f, "(", ast_to_r(a$a, cols, coefs), ")"),
    op   = paste0("(", ast_to_r(a$a, cols, coefs), " ", a$o, " ", ast_to_r(a$b, cols, coefs), ")"))
}

# ---- 3. Read the model text into a list of equations -------------------------------

# Returns list of equations; each has: target, form, identity, add (name or NA),
# innov (RBA residual std. dev.), rhs (AST), text (the original line).
read_model <- function(model_text) {
  blocks <- strsplit(model_text, "\n\\s*\n")[[1]]
  eqs <- list()
  for (blk in blocks) {
    ln <- trimws(strsplit(blk, "\n")[[1]]); ln <- ln[nzchar(ln)]
    if (!length(ln)) next
    ident <- grepl("^@IDENTITY", ln[1], ignore.case = TRUE)
    eqline <- sub("^@IDENTITY\\s*", "", ln[1], ignore.case = TRUE)
    add <- NA_character_
    innov <- NA_real_
    for (x in ln[-1]) {
      if (grepl("^@ADD", x))   add   <- strsplit(x, "\\s+")[[1]][3]
      if (grepl("^@INNOV", x)) innov <- as.numeric(strsplit(x, "\\s+")[[1]][3])   # RBA residual std. dev.
    }
    sides <- regmatches(eqline, regexpr("=", eqline), invert = TRUE)[[1]]
    lhs <- toupper(gsub("\\s+", "", sides[1])); rhs <- sides[2]
    form <- NULL
    for (pf in list(c("^D\\(LOG\\((\\w+)\\)\\)$", "dlog"), c("^DLOG\\((\\w+)\\)$", "dlog"),
                    c("^LOG\\((\\w+)\\)$", "log"), c("^D\\((\\w+)\\)$", "diff"), c("^(\\w+)$", "level"))) {
      if (grepl(pf[1], lhs)) { form <- pf[2]; target <- sub(pf[1], "\\1", lhs); break }
    }
    if (is.null(form)) stop("unrecognised left-hand side: ", lhs)
    eqs[[length(eqs) + 1L]] <- list(target = target, form = form, identity = ident,
                                    add = if (is.na(add)) NA_character_ else toupper(add), innov = innov,
                                    rhs = parse_expr(rhs), text = eqline)
  }
  names(eqs) <- vapply(eqs, `[[`, "", "target")
  eqs
}

# Data matrix for the model: every workfile series as a column, plus empty columns for
# any model variable the RBA did not save (e.g. IBPER, which is an identity).
build_matrix <- function(data, eqs) {
  Z <- as.matrix(data[, setdiff(names(data), "period")]); rownames(Z) <- data$period
  miss <- setdiff(names(eqs), colnames(Z))
  if (length(miss)) Z <- cbind(Z, matrix(NA_real_, nrow(Z), length(miss), dimnames = list(NULL, miss)))
  Z
}

# ---- 4. Compile: build solvable equation functions ----------------------------------

# Given the equations, the data matrix Z (rows = quarters, cols = variables) and
# any estimation-object coefficients, return a "model" object ready to solve.
compile_model <- function(eqs, Z, coefs = list()) {
  cols <- setNames(as.list(seq_len(ncol(Z))), colnames(Z))
  targets <- names(eqs)
  stopifnot(!anyDuplicated(targets))
  for (i in seq_along(eqs)) {
    e <- eqs[[i]]
    code <- ast_to_r(e$rhs, cols, coefs)
    # Inverse of the left-hand-side transformation, including the add factor.
    add  <- if (is.na(e$add)) "0" else sprintf("{a <- Z[t,%dL]; if (is.na(a)) 0 else a}", cols[[e$add]])
    j    <- cols[[e$target]]
    body <- switch(e$form,
      level = sprintf("(%s) + %s", code, add),
      log   = sprintf("exp((%s) + %s)", code, add),
      diff  = sprintf("Z[t-1L,%dL] + (%s) + %s", j, code, add),
      dlog  = sprintf("Z[t-1L,%dL] * exp((%s) + %s)", j, code, add))
    eqs[[i]]$col <- j
    eqs[[i]]$f   <- eval(parse(text = sprintf("function(Z, t) %s", body)))
    eqs[[i]]$dep <- intersect(ast_vars(e$rhs, lag0_only = TRUE), targets)   # same-quarter dependencies
    eqs[[i]]$dep <- setdiff(eqs[[i]]$dep, e$target)
  }
  # Order equations into blocks: strongly connected components of the dependency graph
  blocks <- order_blocks(eqs)
  list(eqs = eqs, blocks = blocks, cols = cols, targets = targets)
}

# Tarjan's algorithm -> list of blocks (vectors of equation indices) in solve order
order_blocks <- function(eqs) {
  n <- length(eqs); targets <- names(eqs)
  adj <- lapply(eqs, function(e) match(e$dep, targets))        # i depends on adj[[i]]
  index <- integer(n); low <- integer(n); onst <- logical(n); stack <- integer(0)
  counter <- 0L; out <- list()
  visit <- function(v) {
    counter <<- counter + 1L; index[v] <<- counter; low[v] <<- counter
    stack <<- c(stack, v); onst[v] <<- TRUE
    for (w in adj[[v]]) {
      if (index[w] == 0L) { visit(w); low[v] <<- min(low[v], low[w]) }
      else if (onst[w])   low[v] <<- min(low[v], index[w])
    }
    if (low[v] == index[v]) {
      comp <- integer(0)
      repeat { w <- stack[length(stack)]; stack <<- stack[-length(stack)]; onst[w] <<- FALSE
               comp <- c(comp, w); if (w == v) break }
      out[[length(out) + 1L]] <<- sort(comp)
    }
  }
  for (v in seq_len(n)) if (index[v] == 0L) visit(v)
  out        # Tarjan emits a component only after all it depends on: already in solve order
}

# ---- 5. Solve --------------------------------------------------------------------------

# Solve quarters 'from':'to' (indices into Z's rows).  Z must already hold history
# before 'from'.  'fix' is an optional matrix the same shape as Z: any non-NA entry
# pins that variable in that quarter (EViews' "exclude" -- the equation is switched
# off and the value imposed; used for the policy shocks of Section 5, p. 31-37).
solve_model <- function(m, Z, from, to, fix = NULL, tol = 1e-10, maxit = 60L, verbose = FALSE) {
  eqs <- m$eqs; blocks <- m$blocks
  cols <- vapply(eqs, `[[`, 0L, "col")
  for (t in from:to) {
    # starting guess: last quarter's value (unless something is already there)
    for (i in seq_along(eqs)) if (is.na(Z[t, cols[i]])) Z[t, cols[i]] <- Z[t - 1L, cols[i]]
    pinned <- logical(length(eqs))
    if (!is.null(fix)) {
      fv <- fix[t, cols]; pinned <- !is.na(fv)
      Z[t, cols[pinned]] <- fv[pinned]
    }
    for (b in blocks) {
      act <- b[!pinned[b]]
      if (!length(act)) next
      if (length(act) == 1L && !(names(eqs)[act] %in% eqs[[act]]$dep)) {
        Z[t, cols[act]] <- eqs[[act]]$f(Z, t)                        # recursive equation: direct evaluation
      } else {
        Z <- newton_block(eqs, act, cols, Z, t, tol, maxit)          # simultaneous block
      }
    }
  }
  Z
}

# Newton iteration on  F_k(x) = x_k - g_k(x) = 0  for one simultaneous block.
# Variables range from ~0.1 (interest rates) to ~1e6 (GDP), so the iteration is done on
# x / scale with scale = max(1, |x|) to keep the Jacobian well conditioned.
newton_block <- function(eqs, act, cols, Z, t, tol, maxit) {
  cj <- cols[act]; k <- length(act)
  x0 <- Z[t, cj]; sc <- pmax(1, abs(x0))
  resid <- function(z) {
    Z[t, cj] <<- z * sc                  # modifies this function's Z in place (no matrix copy)
    x <- z * sc
    vapply(seq_len(k), function(i) (x[i] - eqs[[act[i]]]$f(Z, t)) / sc[i], 0)
  }
  z <- rep(1, k) * sign(x0 + (x0 == 0))  # z = x / scale  (starts at +-1 or smaller)
  z <- x0 / sc
  for (it in seq_len(maxit)) {
    F <- resid(z)
    if (!all(is.finite(F))) stop("non-finite residual in block at t = ", t, ": ",
                                 paste(names(eqs)[act][!is.finite(F)], collapse = ","))
    if (max(abs(F)) < tol) break
    J <- matrix(0, k, k)
    for (c in seq_len(k)) {
      h <- 1e-7 * max(1, abs(z[c])); zp <- z; zp[c] <- z[c] + h
      J[, c] <- (resid(zp) - F) / h
    }
    z <- z - solve(J, F)
  }
  if (max(abs(resid(z))) > 1e-6) warning("block did not converge at t = ", t)
  Z[t, cj] <- z * sc
  Z
}

# ---- 6. Time-varying export constants --------------------------------------------------
# XM_C, XS_C and XRE_C are Kalman-filtered "time-varying constants" (paper Appendix A, p. 39).
# Their stored arrays in the public workfile are not readable, but each enters its export
# equation linearly, so the value that makes the equation hold exactly in the first
# forecast quarter (2018Q2, where every lag is real data) can be solved for directly.
# After that they are held flat, as the RBA's own history shows (xm_c = xm_c(-1)).
calibrate_tvc <- function(m, Z, at = qindex(2018, 2)) {
  pairs <- list(c("XM", "XM_C", 0L), c("XS", "XS_C", 0L), c("XRE", "XRE_C", 1L))   # (equation, constant, lag used)
  for (p in pairs) {
    e <- m$eqs[[p[1]]]; j <- m$cols[[p[2]]]; lag <- as.integer(p[3])
    res <- function(Zc) { g <- e$f(Zc, at); x <- Zc[at, e$col]
      switch(e$form, level = x - g, diff = x - g, log = log(x) - log(g), dlog = log(x / g)) }
    Z0 <- Z; Z0[, j] <- 0; Z1 <- Z; Z1[, j] <- 1
    r0 <- res(Z0); r1 <- res(Z1)
    c_star <- r0 / (r0 - r1)                         # residual is linear in the constant
    Z[(at - lag):nrow(Z), j] <- c_star
  }
  Z
}

# ---- 7. Coefficients that live in estimation objects --------------------------------------
# Three model lines refer to coefficients of EViews equation objects (EQ_PW, EQ_RTWI,
# EQ_WR2R) rather than printing the numbers.  The RBA's stored forecast (2018Q2 onward) was
# produced by the full model, so it satisfies those equations exactly, and the coefficients
# can be read back out of it.  Checks: the values agree with the Online Appendix
# (eq. 88: 0.43; eq. 140: 0.22, 0.65, 0.22) and the fitted residual is at float32 precision.
recover_eq_constants <- function(d, model_text, window = c(qindex(2018, 2), qindex(2040, 4))) {
  rows <- window[1]:window[2]; g <- function(n, lag = 0) d[[n]][rows - lag]

  # EQ_PW.@COEF(1): the model's own wage equation prints it as the first number (Appendix eq. 88)
  m_pw <- regmatches(model_text, regexpr("D\\(LOG\\(PW\\)\\) = [0-9.]+\\*\\(TDLLA", model_text))
  pw1  <- as.numeric(sub("^D\\(LOG\\(PW\\)\\) = ([0-9.]+)\\*\\(TDLLA$", "\\1", m_pw))

  # EQ_RTWI (Section 4.9.3, p. 30; Appendix eq. 140):
  #   dlog(rtwi) = const - c2*(log rtwi(-1) - c3*log tot(-1) + 0.035*(wr2sp(-1) - gap)) + c4*dlog(tot) - 0.05*d(wr2sp)
  y  <- log(g("RTWI")) - log(g("RTWI", 1)) - g("RTWI_CONST") + 0.05 * (g("WR2SP") - g("WR2SP", 1))
  A  <- log(g("RTWI", 1)) + 0.035 * (g("WR2SP", 1) - d$WR2R_GAP_AVG[1])
  xl <- log(g("TOT", 1)); dtot <- log(g("TOT")) - log(g("TOT", 1))
  ok <- complete.cases(y, A, xl, dtot)
  fit <- lm(y[ok] ~ 0 + A[ok] + xl[ok] + dtot[ok]); b <- coef(fit)
  rtwi <- c(NA, -b[[1]], b[[2]] / -b[[1]], b[[3]])          # coefficients 1..4 (1 is replaced by rtwi_const)

  # EQ_WR2R.@COEF(1): wr2r = wrr + 0.99*(wr2r(-1) - wrr(-1)) + 0.01*c   ->  c
  wr2r <- median((g("WR2R") - 0.99 * (g("WR2R", 1) - g("WRR", 1)) - g("WRR")) / 0.01, na.rm = TRUE)

  list(coefs = list(EQ_PW = pw1, EQ_RTWI = c(0, rtwi[-1]), EQ_WR2R = wr2r),
       fit_sd = sd(resid(fit)))
}

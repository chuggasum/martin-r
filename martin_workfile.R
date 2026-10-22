# =============================================================================
# MARTIN (RBA) -- PART B / STEP 1: READ THE RBA's PUBLIC EVIEWS WORKFILE
# -----------------------------------------------------------------------------
# The RBA released "martin_public.wf1" with RDP 2019-07 (supplementary
# information, https://www.rba.gov.au/publications/rdp/2019/2019-07/).  It is a
# proprietary EViews binary.  EViews is not needed here: this file decodes the
# two things we need straight from the bytes.
#
#   1. DATA   -- every series, quarterly 1959Q1-2118Q4 (640 observations).
#   2. MODEL  -- the text of the solved model object M_MARTIN: ~215 equations
#                with every estimated coefficient already substituted in at
#                full precision (the Online Appendix prints them rounded).
#
# File layout reverse-engineered for this project (not an official spec):
#   * Object table: each object is a 70-byte entry.  It begins with the 8 bytes
#     16 14 00 00 00 14 00 00, then 8 bytes of ids, then the object NAME as a
#     zero-padded string.  A 4-byte little-endian file offset to the object's
#     record sits either 32 bytes after the start of the name, or (for some
#     objects) 8 bytes before the name.
#   * Series record: somewhere within 0x90 bytes of that offset is the marker
#     80 02 00 00 01 00 00 00 (+8 zero bytes) 80 02 00 00 00 00.  The 640
#     8-byte little-endian doubles follow it immediately.  EViews NA is stored
#     as the value 1e-37.
#   * Date index: observation 1 = 1959Q1 (checked against Figure 4 of the paper).
#
# Outputs (cached next to this file so later runs are instant):
#   martin_data.rds  -- data.frame, one row per quarter, one column per series
#   model_text.txt   -- the model equations as text
# =============================================================================

WF1_FILE   <- "rba_supplement/extracted/martin_public.wf1"
WF1_NOBS   <- 640L                       # 1959Q1 .. 2118Q4
WF1_START  <- c(1959, 1)                 # first observation (year, quarter)
WF1_NA     <- 1e-37                      # EViews NA sentinel

# ---- helpers ------------------------------------------------------------------

# Quarter index <-> label.  qlabel(1) = "1959Q1"; qindex(2018, 2) = 237
qindex <- function(year, q) (year - WF1_START[1]) * 4L + q - WF1_START[2] + 1L
qlabel <- function(i) {
  y <- WF1_START[1] + (i - 1L + WF1_START[2] - 1L) %/% 4L
  q <- (i - 1L + WF1_START[2] - 1L) %% 4L + 1L
  sprintf("%dQ%d", y, q)
}
qdate <- function(i) {               # quarter-end date, handy for plotting
  y <- WF1_START[1] + (i - 1L) %/% 4L
  q <- (i - 1L) %% 4L + 1L
  nm <- q * 3L + 1L; ny <- y + (nm > 12L); nm <- ifelse(nm > 12L, 1L, nm)
  as.Date(sprintf("%d-%02d-01", ny, nm)) - 1
}

# Little-endian reads from a raw vector --------------------------------------
u32 <- function(raw, pos) sum(as.integer(raw[pos:(pos + 3L)]) * c(1, 256, 65536, 16777216))

# ---- 1. Locate every object in the file -----------------------------------------

wf1_object_table <- function(raw) {
  # Search a copy with NUL bytes swapped for 0x01 so the bytes can be treated as
  # a string (R strings cannot hold NUL).  Positions stay identical.
  txt <- raw; txt[txt == as.raw(0)] <- as.raw(1)
  txt <- rawToChar(txt); Encoding(txt) <- "latin1"

  scan_table <- function(pat, ptr_fun) {
    m  <- gregexpr(pat, txt, perl = TRUE, useBytes = TRUE)[[1]]
    ns <- attr(m, "capture.start")[, ncol(attr(m, "capture.start"))]
    nl <- attr(m, "capture.length")[, ncol(attr(m, "capture.length"))]
    nm <- vapply(seq_along(ns), function(i) rawToChar(raw[ns[i]:(ns[i] + nl[i] - 1L)]), "")
    list(name = nm, ptr = vapply(ns, ptr_fun, numeric(1)))
  }

  # Layout 1 (most objects): header 16 14 00 00 00 14 00 00 + 8 id bytes, then the name.
  t1 <- scan_table("(?s)\x16\x14\x01\x01\x01\x14\x01\x01.{8}([A-Z0-9_ ]{1,32})\x01",
                   function(p) {
                     v <- u32(raw, p + 0x20L)               # pointer 32 bytes after the name ...
                     if (v == 0) v <- u32(raw, p - 8L)      # ... or 8 bytes before it
                     v
                   })
  # Layout 2 (scalars / parameters such as TR_PTM, RCR_TARGET): header
  # 20 00 08 00 00 00 00 00 00 00, a 4-byte pointer, 4 zero bytes, then the name.
  t2 <- scan_table("(?s)\x20\x01\x08\x01{7}.{4}\x01{4}([A-Z0-9_]{1,31})\x01",
                   function(p) u32(raw, p - 8L))

  names <- c(t1$name, t2$name); ptr <- c(t1$ptr, t2$ptr)
  scalar <- c(rep(FALSE, length(t1$name)), rep(TRUE, length(t2$name)))
  keep <- ptr > 0x40 & ptr < length(raw) - 100 & !duplicated(names)
  structure(setNames(ptr[keep], names[keep]), scalar = setNames(scalar[keep], names[keep]))
}

# ---- 2. Decode the series records ---------------------------------------------

wf1_read_series <- function(raw, ptrs) {
  marker <- as.raw(c(0x80, 0x02, 0, 0, 0x01, 0, 0, 0, rep(0, 8), 0x80, 0x02, 0, 0, 0, 0))
  find_marker <- function(p) {
    win <- raw[p:(p + 0x90L)]
    cand <- which(win == marker[1])
    for (c0 in cand) {
      if (c0 + length(marker) - 1L <= length(win) &&
          all(win[c0:(c0 + length(marker) - 1L)] == marker)) return(p + c0 - 1L + length(marker))
    }
    NA_integer_
  }
  is_scalar <- attr(ptrs, "scalar")
  out <- list()
  for (nm in names(ptrs)) {
    if (is_scalar[[nm]]) {                       # scalar objects: one double at the pointer
      out[[nm]] <- rep(readBin(raw[(ptrs[[nm]] + 1L):(ptrs[[nm]] + 8L)], "double", 1, endian = "little"), WF1_NOBS)
      next
    }
    d <- find_marker(ptrs[[nm]])
    if (is.na(d) || d + WF1_NOBS * 8L - 1L > length(raw)) next      # not a series object
    x <- readBin(raw[d:(d + WF1_NOBS * 8L - 1L)], "double", WF1_NOBS, endian = "little")
    x[x == WF1_NA] <- NA_real_
    out[[nm]] <- x
  }
  as.data.frame(out, check.names = FALSE)
}

# ---- 3. Pull out the model text -----------------------------------------------

wf1_read_model_text <- function(raw) {
  txt <- raw; txt[txt == as.raw(0)] <- as.raw(1)
  txt <- rawToChar(txt); Encoding(txt) <- "latin1"
  # The model object's text begins with its first equation and ends at the next NUL.
  s <- regexpr("log(lhpp)  = tllhpp", txt, fixed = TRUE, useBytes = TRUE)
  e <- regexpr("\\x01", substr(txt, s, nchar(txt, "bytes")), perl = TRUE, useBytes = TRUE)
  substr(txt, s, s + e - 2L)
}

# ---- 3b. Rebuild the "recipe" series ----------------------------------------------
# About 25 exogenous series (dummies, deterministic trends, the inflation target) are
# stored by EViews in a compressed form that the reader above cannot decode.  They
# are not data: the workfile records the command that generated each one (its
# "[History]" text), and the repo https://github.com/DavidAStephan/MARTIN lists the
# same recipes in Programs/modify_data.prg.  We replay those recipes here.
#
#   rule: sample A..B  x = const | x(-1) | x(-1)+1 | @trend | @elem(x, date)
#
# @trend counts from zero at the FIRST OBSERVATION OF THE WORKFILE (1959Q1), whatever sample it is
# run on.  (Tested: this reading reproduces the RBA's stored forecasts to 1e-6; counting from the
# start of each sample does not.)
rebuild_recipe_series <- function(d, trend_base = "workfile") {
  n  <- nrow(d); ix <- function(y, q) qindex(y, q)
  new <- function() rep(NA_real_, n)
  rng <- function(a, b) seq(ix(a[1], a[2]), ix(b[1], b[2]))
  set  <- function(x, a, b, v)  { x[rng(a, b)] <- v; x }
  cumup <- function(x, a, b) { for (t in rng(a, b)) x[t] <- x[t - 1L] + 1; x }   # x = x(-1) + 1
  hold  <- function(x, a, b) { for (t in rng(a, b)) x[t] <- x[t - 1L];     x }   # x = x(-1)
  trend <- function(x, a, b) { r <- rng(a, b)
    x[r] <- if (trend_base == "sample") r - r[1] else r - 1L; x }                # x = @trend
  last <- c(2118, 4); first <- c(1959, 1)
  dummy <- function(one_from = NULL, one_to = NULL, base = 0) {
    x <- rep(base, n); if (!is.null(one_from)) x <- set(x, one_from, one_to, 1 - base); x }

  # impulse / step dummies: the stored arrays carry stray values in the forecast period
  d$DUM_2008Q4     <- dummy(c(2008, 4), c(2008, 4))
  d$D_IBRE_1       <- dummy(c(1999, 4), c(1999, 4))
  d$D_IBRE_2       <- dummy(c(2001, 4), c(2001, 4))
  d$D_IBRE_3       <- dummy(c(2002, 1), c(2002, 1))
  d$D_IBRE_4       <- dummy(c(2002, 2), c(2002, 2))
  d$D_IBRE_5       <- dummy(c(2002, 3), c(2002, 3))
  d$D_NSP          <- dummy(c(1998, 1), c(2007, 4))
  d$D_OLY          <- dummy(c(2000, 4), c(2000, 4))
  d$D_OLYX         <- dummy(c(2000, 3), c(2000, 3)); d$D_OLYX[ix(2000, 4)] <- -1
  d$PI_TARGET      <- 2.5                                                    # pi_target = 2.5
  d$DUM_RC         <- dummy(first, c(2007, 4))                              # 1 before 2008Q1, 0 after
  d$LUR_DUM        <- dummy(c(2031, 4), last)                               # 0 until 2031Q3, 1 after
  d$RTWI_CONST_DUM <- dummy(c(2031, 4), last)
  d$PTM_DUM        <- dummy(c(2019, 1), last)                               # 0 until 2018Q4, 1 from 2019Q1
  d$D_RBAGOLD      <- dummy(c(1997, 2), c(1997, 2))                         # 1 in 1997Q2 only
  d$PIBRE_DUM1     <- dummy(c(2000, 4), c(2000, 4))
  d$PIBRE_DUM2     <- dummy(c(2001, 1), c(2001, 1))
  d$PIBRE_DUM3     <- dummy(c(2001, 2), c(2001, 2))
  d$PIBRE_DUM4     <- dummy(c(2001, 3), c(2001, 3))

  x <- new(); x <- trend(x, first, c(2011, 4)); x <- set(x, c(2012, 1), last, x[ix(2011, 4)]); d$TADP <- x
  x <- new(); x <- trend(x, c(1982, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last); d$PC_TREND  <- x
  x <- new(); x <- trend(x, c(1982, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last); d$PID_TREND <- x
  x <- new(); x <- trend(x, c(1986, 1), c(2000, 4)); x <- hold(x, c(2001, 1), last); d$PXM_TREND <- x

  zero <- function() rep(0, n)
  x <- zero(); x <- cumup(x, c(1980, 1), c(2000, 4)); x <- hold(x, c(2001, 1), last);       d$WPX_TREND_1  <- x
  x <- zero(); x <- cumup(x, c(2001, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last);       d$WPX_TREND_2  <- x
  x <- zero(); x <- cumup(x, c(1985, 1), c(2012, 4)); x <- hold(x, c(2013, 1), last);       d$PIBN_TREND_1 <- x
  x <- zero(); x <- cumup(x, c(2013, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last);       d$PIBN_TREND_2 <- x
  x <- zero(); x <- cumup(x, c(1986, 1), c(2000, 4)); x <- hold(x, c(2001, 1), last);       d$PXS_TREND_1  <- x
  x <- zero(); x <- cumup(x, c(2001, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last);       d$PXS_TREND_2  <- x
  x <- zero(); x <- cumup(x, c(1992, 1), c(2007, 4)); x <- hold(x, c(2008, 1), last);       d$POTC_TREND_1 <- x
  x <- zero(); x <- cumup(x, c(2008, 1), c(2022, 2)); x <- hold(x, c(2022, 3), last);       d$POTC_TREND_2 <- x

  # time-varying export constant for resources: data to 2019Q1, then flat ("xre_c = xre_c(-1)")
  x <- d$XRE_C; lastobs <- max(which(!is.na(x[seq_len(ix(2019, 1))]))); x[(lastobs + 1L):n] <- x[lastobs]; d$XRE_C <- x
  # The corporate tax rate is stored in per cent; the workfile's own history says
  # "ibctr = ibctr1/100", so the model (which uses 1 - ibctr) needs the fraction.
  if (max(d$IBCTR, na.rm = TRUE) > 1) d$IBCTR <- d$IBCTR / 100
  d
}

# ---- 4. Main entry point ------------------------------------------------------

load_martin_workfile <- function(wf1 = WF1_FILE, cache = TRUE) {
  rds <- file.path(dirname(dirname(wf1)), "martin_data.rds")
  mt  <- file.path(dirname(dirname(wf1)), "model_text.txt")
  if (cache && file.exists(rds) && file.exists(mt)) {
    return(list(data = readRDS(rds), model_text = paste(readLines(mt, warn = FALSE), collapse = "\n")))
  }
  stopifnot("martin_public.wf1 not found; edit WF1_FILE" = file.exists(wf1))
  message("Decoding ", wf1, " (one-off, ~1 min) ...")
  raw   <- readBin(wf1, "raw", file.size(wf1))
  ptrs  <- wf1_object_table(raw)
  dat   <- wf1_read_series(raw, ptrs)
  model <- wf1_read_model_text(raw)
  dat <- rebuild_recipe_series(dat)
  dat <- cbind(period = qlabel(seq_len(WF1_NOBS)), dat, stringsAsFactors = FALSE)
  if (cache) { saveRDS(dat, rds); writeLines(model, mt) }
  list(data = dat, model_text = model)
}

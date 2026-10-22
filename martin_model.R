# =============================================================================
# MARTIN (RBA) -- PART B: RE-RUN THE MODEL IN R AND COMPARE WITH THE RBA
# -----------------------------------------------------------------------------
# Reference : Ballantyne et al. (2019), "MARTIN Has Its Place: A Macroeconometric
#             Model of the Australian Economy", RBA RDP 2019-07  (rdp2019-07.pdf).
#             "p." = page printed on the page.  "App." = the paper's Online Appendix
#             (rdp-2019-07-online-appendix.pdf), which lists every equation.
#
# What this script does
#   1. Reads the RBA's public EViews workfile (data + all model equations with the
#      estimated coefficients) -- no EViews needed.                    [martin_workfile.R]
#   2. Turns the equations into R functions and solves them quarter by quarter [martin_engine.R]
#   3. CHECKS that the R model reproduces the RBA's own stored numbers.   (section 3)
#   4. Solves the baseline projection 2019Q1-2029Q4.                     (section 4)
#   5. Runs the paper's Section 5 experiments, exactly as the RBA's script
#      martin_rdp_irfs_public.prg defines them:                          (section 5)
#         monetary policy shock (Figs 12-14), exchange rate shock (Fig 15),
#         housing price shock (Fig 16).
#   6. Plots our impulse responses against the RBA's published ones, plus the gap
#      between them, and prints a comparison table.                      (sections 6-7)
#
# How to read the comparison charts
#   * RBA's published curve  = thick, pale line   (data from the paper's workbook)
#   * Our R re-run           = thin, dark line on top
#   If the dark line hugs the middle of the pale one, we have reproduced the RBA's
#   result independently.  The "gap" charts plot (ours - RBA) on the same units.
#
# Run in RStudio: open this file, Ctrl+Shift+S (Source).  First run decodes the
# workfile (~1 minute), later runs are quicker.  Needs: readxl, ggplot2, dplyr, tidyr.
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------------

# Folder that holds this script.  (In RStudio it is found automatically.)
PROJECT_DIR <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    dirname(rstudioapi::getSourceEditorContext()$path) else getwd()
}, error = function(e) getwd())
if (!file.exists(file.path(PROJECT_DIR, "martin_engine.R"))) PROJECT_DIR <- getwd()
setwd(PROJECT_DIR)

library(readxl); library(ggplot2); library(dplyr); library(tidyr); library(scales)
source("martin_workfile.R")                 # step 1: workfile reader
source("martin_engine.R")                   # step 2: model engine
options(martin.draw = FALSE); source("martin_figures.R"); options(martin.draw = TRUE)
                                            # (loads helpers + the RBA's published figure data, no plots)

SIM_START <- qindex(2019, 1)                # shocks and simulations start 2019Q1  (prg: %shock_start = 2019:1)
SIM_END   <- qindex(2029, 4)                # simulation ends 2029Q4               (prg: sim1_end = 2029Q4)

# ---- 1. Data and equations -----------------------------------------------------------
# The workfile holds 649 series (1959Q1-2118Q4).  Everything after 2018Q1 is the RBA's own
# stored forecast / assumption paths.  Series whose values the workfile stores in a compact
# form (dummies, trends, the inflation target) are rebuilt from the generating commands the
# workfile records for them -- see rebuild_recipe_series() in martin_workfile.R.

wf    <- load_martin_workfile()
dat   <- wf$data                            # data.frame: period + one column per series
eqs   <- read_model(wf$model_text)          # 217 equations: 95 stochastic + 122 identities
cat(sprintf("Workfile: %d series, %d model equations (%d estimated, %d identities)\n",
            ncol(dat) - 1, length(eqs), sum(!sapply(eqs, `[[`, "identity")),
            sum(sapply(eqs, `[[`, "identity"))))

# ---- 2. Build the model -----------------------------------------------------------------
# Section 2.2 (p. 2-4) / Section 4 (p. 8-9): MARTIN is a system of error-correction equations
# plus identities, solved jointly.  compile_model() orders the equations into blocks; EViews
# reports 49 recursive + 53 simultaneous + 115 recursive equations, and so do we.

Z0 <- build_matrix(dat, eqs)

# Three coefficients are stored inside EViews equation objects (see engine, section 7).
rc    <- recover_eq_constants(dat, wf$model_text)
model <- compile_model(eqs, Z0, rc$coefs)
Z0    <- calibrate_tvc(model, Z0)           # time-varying export constants (Appendix A, p. 39)
sizes <- sapply(model$blocks, length)
cat(sprintf("Model blocks: %d recursive equations + one simultaneous block of %d equations\n",
            sum(sizes == 1), max(sizes)))
cat(sprintf("EQ_RTWI coefficients recovered from the RBA's stored forecast: speed %.4f, terms-of-trade %.4f, short-run %.4f\n",
            rc$coefs$EQ_RTWI[2], rc$coefs$EQ_RTWI[3], rc$coefs$EQ_RTWI[4]))

# ---- 3. Does our R model reproduce the RBA's numbers?  --------------------------------------
# Test A (exactness): the RBA's stored forecast for 2018Q2-2019Q1 was produced by this model.
#   Feeding those stored numbers through OUR equation functions should return them again.
# Test B (in-sample fit): over 2010Q1-2018Q1 (real data) the residual of each estimated
#   equation should be about as large as the RBA's own recorded residual std. deviation.

check_equations <- function(m, Z) {
  rows <- qindex(2018, 2):qindex(2019, 1)
  a <- do.call(rbind, lapply(names(m$eqs), function(n) {
    e <- m$eqs[[n]]; g <- sapply(rows, function(t) e$f(Z, t)); x <- Z[rows, e$col]
    err <- switch(e$form, level = , diff = (x - g) / (1 + abs(x)), log = , dlog = log(x / g))
    data.frame(eq = n, identity = e$identity, max_err = max(abs(err), na.rm = TRUE))
  }))
  rows2 <- qindex(2010, 1):qindex(2018, 1)
  b <- do.call(rbind, lapply(names(m$eqs), function(n) {
    e <- m$eqs[[n]]; if (e$identity || is.na(e$innov)) return(NULL)
    g <- sapply(rows2, function(t) e$f(Z, t)); x <- Z[rows2, e$col]
    r <- switch(e$form, level = , diff = x - g, log = , dlog = log(x / g))
    data.frame(eq = n, resid_sd = sd(r, na.rm = TRUE), rba_sd = e$innov)
  }))
  list(exact = a, fit = b)
}
chk <- check_equations(model, Z0)
n_exact <- sum(chk$exact$max_err < 1e-3, na.rm = TRUE)
cat(sprintf("Test A: %d of %d equations return the RBA's stored 2018Q2-2019Q1 numbers (tolerance 1e-3; the workfile stores floats to ~7 digits)\n",
            n_exact, nrow(chk$exact)))
fit_ok <- with(chk$fit, resid_sd / rba_sd <= 2)
cat(sprintf("Test B: %d of %d estimated equations have in-sample residual sd within 2x the RBA's recorded value (median ratio %.2f)\n",
            sum(fit_ok), nrow(chk$fit), median(with(chk$fit, resid_sd / rba_sd))))
# (Equations that miss are the ones driven by the 21 series the RBA replaced with simulated
#  data in the public release, plus statistical-discrepancy / capital-stock terms that are
#  only used in forecasting.)

# ---- 4. Baseline projection 2019Q1-2029Q4 -----------------------------------------------------
# The RBA's script (martin_rdp_irfs_public.prg) first solves a baseline from 2019Q1 using the
# workfile as starting point, then solves each shock scenario and reports deviations from it.
# Add factors (the "judgement" terms of Section 2.3, p. 4) are zero in the public workfile.

base <- solve_model(model, Z0, SIM_START, SIM_END)

baseline_plot <- function() {
  vars <- c(APC_Y = "GDP growth, year-ended (%)", LUR = "Unemployment rate (%)",
            PI_PTM = "Trimmed mean inflation, year-ended (%)", NCR = "Cash rate (%)")
  r <- qindex(2013, 1):SIM_END
  mk <- function(M, label) {
    data.frame(Date = qdate(r), source = label, M[r, names(vars), drop = FALSE]) |>
      pivot_longer(-c(Date, source), names_to = "var", values_to = "value")
  }
  long <- bind_rows(mk(base, "R re-run (2019Q1 on)"), mk(Z0, "RBA workfile (stored path)")) |>
    mutate(var = factor(vars[var], levels = vars))
  ggplot(long, aes(Date, value, colour = source, linewidth = source, alpha = source)) +
    geom_vline(xintercept = qdate(SIM_START), colour = "grey60", linetype = "dotted") +
    geom_line() +
    facet_wrap(~var, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = c("R re-run (2019Q1 on)" = "#0B3C5D", "RBA workfile (stored path)" = "#7EA6D6")) +
    scale_linewidth_manual(values = c("R re-run (2019Q1 on)" = 0.7, "RBA workfile (stored path)" = 2.4)) +
    scale_alpha_manual(values = c("R re-run (2019Q1 on)" = 1, "RBA workfile (stored path)" = 0.45)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Baseline projection: R model vs the RBA workfile's own path",
         subtitle = "Public workfile uses simulated stand-ins for 21 proprietary series; the projection is illustrative, not an RBA forecast",
         caption = "Sources: RBA public MARTIN workfile (RDP 2019-07); R re-run.  Dotted line = start of simulation (2019Q1).") +
    theme_rba() + theme(strip.text = element_text(face = "bold"))
}
print(baseline_plot())

# ---- 5. Experiments of Section 5 (p. 31-37) ----------------------------------------------------
# An "experiment" pins one or more variables to a path for some quarters (EViews: exclude the
# equation, impose the value), then releases them so the model's own equation takes over again.
# pins: list of list(var, type, size, length, start)
#   type "ppt": baseline + size      type "pct": baseline * (1 + size/100)
#   'size' may be a vector (a path).  'start' defaults to the first simulated quarter.

run_scenario <- function(pins, Z = Z0, from = SIM_START, to = SIM_END, add_factors = NULL) {
  fix <- matrix(NA_real_, nrow(Z), ncol(Z), dimnames = dimnames(Z))
  for (p in pins) {
    st <- if (is.null(p$start)) from else p$start
    r  <- st:(st + p$length - 1L)
    fix[r, p$var] <- if (p$type == "ppt") base[r, p$var] + p$size else base[r, p$var] * (1 + p$size / 100)
  }
  if (!is.null(add_factors)) for (v in names(add_factors)) Z[add_factors[[v]]$rows, v] <- add_factors[[v]]$values
  solve_model(model, Z, from, to, fix = fix)
}

# Variables that are always held at baseline for 40 quarters in these experiments (prg lines
# shock_name_s1 etc.): the neutral rate and public demand do not respond to the shock.
hold_exo <- list(list(var = "RSTAR", type = "ppt", size = 0, length = 40),
                 list(var = "GC",    type = "pct", size = 0, length = 40),
                 list(var = "GI",    type = "pct", size = 0, length = 40))

# Monetary policy shock, Section 5.1 (p. 31-34): cash rate +100bp for four quarters, then the rule.
s_mp   <- run_scenario(c(list(list(var = "NCR", type = "ppt", size = 1, length = 4)), hold_exo))
# ... the same with the real (and nominal) exchange rate held at baseline  -> Figure 14 "No exchange rate channel"
s_noer <- run_scenario(c(list(list(var = "NCR", type = "ppt", size = 1, length = 4),
                              list(var = "RTWI", type = "ppt", size = 0, length = 40),
                              list(var = "NTWI", type = "ppt", size = 0, length = 40)), hold_exo))
# ... and with housing and equity prices held as well -> "No exchange rate or asset price channels"
s_noap <- run_scenario(c(list(list(var = "NCR", type = "ppt", size = 1, length = 4),
                              list(var = "RTWI", type = "ppt", size = 0, length = 40),
                              list(var = "NTWI", type = "ppt", size = 0, length = 40),
                              list(var = "PH",   type = "ppt", size = 0, length = 40),
                              list(var = "IBEY", type = "ppt", size = 0, length = 40)), hold_exo))
# Exchange rate shock, Section 5.2 (p. 35): real TWI 10% lower for four quarters.
s_fx   <- run_scenario(c(list(list(var = "RTWI", type = "pct", size = -10, length = 4)), hold_exo))

# Housing price shock, Section 5.3 (p. 36-37): a persistent 10% exogenous fall, building up by
# 2.5% a quarter from the second quarter (the RBA's chart starts at zero in quarter 0).
#   The RBA's public script (martin_rdp_irfs_public.prg, scenario "Housing Price fall - no ncr
#   or rtwi") also holds the real exchange rate at baseline.  HOUSING_PIN_RTWI = TRUE follows
#   that script; FALSE lets the exchange rate respond.  (TRUE matches the RBA's published
#   fixed-cash-rate chart best.)
HOUSING_PIN_RTWI <- TRUE
hp_extra <- if (HOUSING_PIN_RTWI) list(list(var = "RTWI", type = "ppt", size = 0, length = 40)) else list()
#   Fixed cash rate: housing prices pinned to the path; cash rate pinned at baseline throughout.
ph_path  <- c(0, -2.5, -5, -7.5, rep(-10, 20))                      # quarters 0..23
H        <- length(ph_path)
s_hp_fix <- run_scenario(c(list(list(var = "PH",  type = "pct", size = ph_path, length = H),
                                list(var = "NCR", type = "ppt", size = 0,       length = H)), hp_extra, hold_exo))
#   Flexible cash rate: the same *exogenous shock* (the housing-price equation's add factor that
#   delivers the fixed-rate path), but now the cash rate may respond.  The paper imposes a 75bp
#   cut spread over four quarters (-25, -50, -75, -75, -75bp in quarters 2-6), then the rule.
a_ph <- log(s_hp_fix[SIM_START:(SIM_START + H - 1), "PH"] /
            sapply(SIM_START:(SIM_START + H - 1), function(t) model$eqs$PH$f(s_hp_fix, t)))
a_ph[1] <- 0
s_hp_flex <- run_scenario(
  c(list(list(var = "NCR", type = "ppt", size = c(-0.25, -0.5, -0.75, -0.75, -0.75), length = 5, start = SIM_START + 2L)),
    hp_extra, hold_exo),
  add_factors = list(PH_A = list(rows = SIM_START:(SIM_START + H - 1), values = a_ph)))
# NOTE: the paper does not say exactly how the housing shock was imposed in the flexible case;
# our reconstruction reproduces the RBA's first ~8-10 quarters closely but drifts later.

# ---- 6. Turn the simulations into the paper's figures ------------------------------------------------
# Each panel is a deviation from baseline: percentage-point difference for rates, per cent
# difference for levels (as in the RBA script: y_s1/y_0*100-100, lur_s1-lur_0, ...).

dev_pct <- function(v) function(s, b) (s[, v] / b[, v] - 1) * 100     # per cent deviation from baseline
dev_ppt <- function(v) function(s, b) s[, v] - b[, v]                 # percentage-point deviation
panels_agg <- list("Cash rate" = dev_ppt("NCR"), "GDP" = dev_pct("Y"), "Unemployment rate" = dev_ppt("LUR"),
                   "Trimmed mean inflation; year-ended" = dev_ppt("PI_PTM"),
                   "WPI growth; year-ended" = dev_ppt("PI_PW"), "Real exchange rate" = dev_pct("RTWI"))
panels_exp <- list("Consumption" = dev_pct("RC"), "Dwelling investment" = dev_pct("ID"), "Business investment" = dev_pct("IB"),
                   "Non-resource exports" = function(s, b) (((s[, "X"] - s[, "XRE"]) / (b[, "X"] - b[, "XRE"])) - 1) * 100,
                   "Resource exports" = dev_pct("XRE"), "Imports" = dev_pct("M"))
panels_fx  <- list("Real TWI" = dev_pct("RTWI"), "GDP" = dev_pct("Y"), "Unemployment rate" = dev_ppt("LUR"),
                   "Trimmed mean inflation; year-ended" = dev_ppt("PI_PTM"),
                   "WPI growth; year-ended" = dev_ppt("PI_PW"), "Cash rate" = dev_ppt("NCR"))
panels_hp  <- list("Housing prices" = dev_pct("PH"), "GDP" = dev_pct("Y"), "Trimmed mean inflation; year-ended" = dev_ppt("PI_PTM"),
                   "Unemployment rate" = dev_ppt("LUR"), "Consumption" = dev_pct("RC"), "Cash rate" = dev_ppt("NCR"))

build_irf <- function(panels, scen, series_name, nq) {
  q <- SIM_START:(SIM_START + nq - 1L); b <- base[q, , drop = FALSE]; s <- scen[q, , drop = FALSE]
  bind_rows(lapply(names(panels), function(p)
    data.frame(Quarters = 0:(nq - 1L), panel = p, series = series_name, value = panels[[p]](s, b))))
}

ours <- list(
  fig12 = build_irf(panels_agg, s_mp, "MARTIN", 40),
  fig13 = build_irf(panels_exp, s_mp, "MARTIN", 40),
  fig14 = bind_rows(build_irf(list(GDP = dev_pct("Y")), s_mp,   "All channels", 40),
                    build_irf(list(GDP = dev_pct("Y")), s_noer, "No exchange rate channel", 40),
                    build_irf(list(GDP = dev_pct("Y")), s_noap, "No exchange rate or asset price channels", 40)),
  fig15 = build_irf(panels_fx, s_fx, "MARTIN", 40),
  fig16 = bind_rows(build_irf(panels_hp, s_hp_fix,  "Fixed cash rate",    H),
                    build_irf(panels_hp, s_hp_flex, "Flexible cash rate", H)))

# RBA's published curves in the same long format.  (MSM, the RBA's DSGE comparison line, is dropped.)
rba <- list(
  fig12 = REF_IRF$fig12 |> filter(series == "MARTIN"),
  fig13 = REF_IRF$fig13 |> filter(series == "MARTIN"),
  fig14 = REF_IRF$fig14 |> mutate(panel = "GDP"),
  fig15 = REF_IRF$fig15 |> filter(series == "MARTIN"),
  fig16 = REF_IRF$fig16)
rba <- lapply(rba, function(x) x |> filter(!is.na(value), !is.na(Quarters), Quarters <= 39) |>
                mutate(Quarters = as.numeric(Quarters)))
rba$fig16 <- rba$fig16 |> filter(Quarters <= H - 1)

# ---- 7. Side-by-side charts and comparison table -----------------------------------------------------

PANEL_UNITS_ALL <- c(PANEL_UNITS, "Real TWI" = "%")
src_lab <- c(rba = "RBA published", ours = "R re-run")

plot_overlay <- function(key, title, subtitle, colours) {
  d <- bind_rows(rba[[key]] |> mutate(source = src_lab[["rba"]]),
                 ours[[key]] |> mutate(source = src_lab[["ours"]])) |>
    mutate(panel_lab = paste0(sub("; ", "\n", panel), "  (", PANEL_UNITS_ALL[panel], ")"),
           panel_lab = factor(panel_lab, levels = unique(panel_lab[order(match(panel, unique(ours[[key]]$panel)))])),
           source = factor(source, levels = unname(src_lab)))
  ggplot(d, aes(Quarters, value, colour = series, linewidth = source, alpha = source)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_line(na.rm = TRUE) +
    facet_wrap(~panel_lab, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = colours) +
    scale_linewidth_manual(values = c("RBA published" = 2.6, "R re-run" = 0.7)) +
    scale_alpha_manual(values = c("RBA published" = 0.40, "R re-run" = 1)) +
    scale_x_continuous(breaks = seq(0, 40, 8)) +
    guides(colour = if (length(colours) > 1) guide_legend(order = 1) else "none",
           linewidth = guide_legend(order = 2, override.aes = list(colour = "grey25")),
           alpha = guide_legend(order = 2)) +
    labs(title = title, subtitle = subtitle, x = "Quarters",
         caption = "Thick pale line: RBA's published data (RDP 2019-07 graph-data workbook). Thin dark line: R re-run of the public MARTIN workfile.") +
    theme_rba() + theme(axis.title.x = element_text(), panel.spacing.y = unit(1.3, "lines"))
}

plot_gap <- function(key, title) {
  g <- inner_join(ours[[key]] |> rename(ours = value), rba[[key]] |> rename(rba = value),
                  by = c("Quarters", "panel", "series")) |>
    mutate(gap = ours - rba,
           panel_lab = paste0(sub("; ", "\n", panel), "  (", PANEL_UNITS_ALL[panel], ")"),
           panel_lab = factor(panel_lab, levels = unique(panel_lab)))
  ggplot(g, aes(Quarters, gap, colour = series)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~panel_lab, ncol = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 40, 8)) +
    labs(title = title, subtitle = "Gap = R re-run minus RBA published, same units as the panel (free y-axis)", x = "Quarters",
         caption = "Compare the size of the gap with the size of the response in the matching overlay chart.") +
    theme_rba() + theme(axis.title.x = element_text(), panel.spacing.y = unit(1.3, "lines"))
}

navy <- "#0B3C5D"; red <- "#C0392B"; teal <- "#1B998B"; gold <- "#E0A100"
p12 <- plot_overlay("fig12", "Figure 12: Monetary Policy Shock -- RBA vs R re-run", "Aggregate variables; +100bp cash rate for four quarters", c(MARTIN = navy))
p13 <- plot_overlay("fig13", "Figure 13: Monetary Policy Shock -- RBA vs R re-run", "Expenditure components", c(MARTIN = navy))
p14 <- plot_overlay("fig14", "Figure 14: GDP by transmission channel -- RBA vs R re-run", "Deviation from baseline, per cent",
                    c("All channels" = navy, "No exchange rate channel" = gold, "No exchange rate or asset price channels" = teal))
p15 <- plot_overlay("fig15", "Figure 15: Exchange Rate Depreciation -- RBA vs R re-run", "Aggregate variables; real TWI 10% lower for four quarters", c(MARTIN = navy))
p16 <- plot_overlay("fig16", "Figure 16: Housing Price Shock -- RBA vs R re-run", "Deviation from baseline; persistent 10% fall in housing prices",
                    c("Fixed cash rate" = navy, "Flexible cash rate" = red))
for (p in list(p12, p13, p14, p15, p16)) print(p)
for (k in c("fig12", "fig13", "fig14", "fig15", "fig16"))
  print(plot_gap(k, sprintf("Figure %s: gap between R re-run and RBA published", sub("fig", "", k))))

compare_table <- bind_rows(lapply(names(ours), function(k) {
  inner_join(ours[[k]] |> rename(ours = value), rba[[k]] |> rename(rba = value), by = c("Quarters", "panel", "series")) |>
    group_by(figure = sub("fig", "Figure ", k), panel, series) |>
    summarise(rba_peak = rba[which.max(abs(rba))], ours_peak = ours[which.max(abs(ours))],
              max_gap = max(abs(ours - rba)), gap_pct_of_peak = 100 * max(abs(ours - rba)) / max(abs(rba)),
              correlation = suppressWarnings(cor(ours, rba)), .groups = "drop")
}))
cat("\nComparison of R re-run with RBA published responses (peak = largest absolute deviation):\n")
print(as.data.frame(compare_table |> transmute(figure, panel = substr(panel, 1, 22), series = substr(series, 1, 14),
                                               rba_peak = round(rba_peak, 3), ours_peak = round(ours_peak, 3),
                                               gap_pct_of_peak = round(gap_pct_of_peak, 1), correlation = round(correlation, 3))),
      row.names = FALSE)
summ <- function(tb, label) cat(sprintf("%s: %d panels/series, median gap = %.1f%% of the RBA's peak response, worst gap = %.1f%%, correlation >= %.3f\n",
                                       label, nrow(tb), median(tb$gap_pct_of_peak, na.rm = TRUE), max(tb$gap_pct_of_peak, na.rm = TRUE),
                                       min(tb$correlation, na.rm = TRUE)))
cat("\n"); summ(filter(compare_table, figure != "Figure 16"), "Figures 12-15 (monetary policy / exchange rate shocks)")
summ(filter(compare_table, figure == "Figure 16"), "Figure 16 (housing price shock)         ")

# ---- 8. Figures 2-11 recomputed from the workfile -----------------------------------------------------
# Part A (martin_figures.R) simply plots the RBA's published numbers.  Here we RECOMPUTE the data
# figures from the raw workfile series using the long-run relationships of Section 4, and overlay
# them on the RBA's published chart data.  Same colour scheme as above: thick pale = RBA data,
# thin dark = our calculation.  (Figure 5 uses Corelogic data the RBA withholds.)

LAGV <- function(x, k) c(rep(NA_real_, k), head(x, -k))
rebase2000 <- function(x) 100 * x / mean(x[qindex(2000, 1):qindex(2000, 4)], na.rm = TRUE)
V  <- function(v) dat[[v]]
LV <- function(v) log(dat[[v]])

# Long-run determinants, straight from the equations (paper page in brackets):
lr_consumption <- 0.837962933547104 * LV("HDY") + (1 - 0.837962933547104) * LAGV(LV("HNW"), 2) -
  0.5 / 100 * V("RCR") + 0.00834918476350798 / 0.0770674319067467 * V("DUM_RC")        # Section 4.2.1 (p. 14)
lr_employment  <- LV("Y") - 0.4 * (LV("RLC") - V("TLLA")) - V("TLLA") - V("TLLHPP")      # Section 4.8.1 (p. 27)

recomputed <- list(
  "Figure 2" = list(title = "Figure 2: Exogenous Trends (annualised)", panel = NULL, zero = TRUE,                 # Section 4.1 (p. 12-13)
    cols = list(`Labour productivity` = 400 * V("TDLLA"), `Working-age population` = 400 * V("TDLLPOP"),
                `Hours worked` = 400 * V("TDLLHPP"),
                `Potential growth` = 400 * (V("TDLLA") + V("TDLLPOP") + V("TDLLHPP")))),
  "Figure 3" = list(title = "Figure 3: Long-run Determinants of Consumption (2000 = 100, log scale)", log = TRUE,
    # the RBA draws the determinants on their own right-hand axis, so only their shape is
    # comparable: ours is rescaled by a fitted log-linear map (slope reported in the console)
    rescale = "Long-run determinants(a)",
    cols = list(Consumption = rebase2000(V("RC")), `Long-run determinants(a)` = rebase2000(exp(lr_consumption)))),
  "Figure 4" = list(title = "Figure 4: Dwelling Investment-to-consumption Ratio (%)",
    cols = list(Series = V("NID") / V("NC") * 100)),                                             # Section 4.3.1 (p. 16-17)
  "Figure 6" = list(title = "Figure 6: Business Investment Quantities and Prices",               # Section 4.4.1 (p. 18-20)
    panel = c(`Non-mining` = "Share of GDP (%)", Total = "Share of GDP (%)",
              `Non-mining investment deflator to GNE deflator` = "Relative price (2000 = 100)"),
    cols = list(`Non-mining` = V("NIBN") / V("NY") * 100, Total = V("NIB") / V("NY") * 100,
                `Non-mining investment deflator to GNE deflator` = rebase2000(V("PIBN") / V("PGNE")))),
  "Figure 7" = list(title = "Figure 7: Export Values (share of GDP, %)",                         # Section 4.5.1 (p. 21-22)
    panel = c(Agriculture = "Non-resource", Manufacturing = "Non-resource", Other = "Non-resource",
              Services = "Non-resource", Resource = "Resource"),
    cols = list(Agriculture = V("NXAG") / V("NY") * 100, Manufacturing = V("NXM") / V("NY") * 100,
                Other = V("NXO") / V("NY") * 100, Services = V("NXS") / V("NY") * 100,
                Resource = V("NXRE") / V("NY") * 100)),
  "Figure 8" = list(title = "Figure 8: Long-run Determinants of Imports (2000 = 100, log scale)", log = TRUE,   # Section 4.5.3 (p. 22-23)
    cols = list(Imports = rebase2000(V("M")), `Import-adjusted demand` = rebase2000(V("IAD")),
                `Domestic-to-import prices` = rebase2000(V("PDFD") / V("PM")))),
  "Figure 9" = list(title = "Figure 9: Nominal Wage Measures (year-ended growth, %)", zero = TRUE,            # Section 4.7 (p. 24-25)
    cols = list(`Wage price index` = V("PI_PW"), AENA = V("PI_PAE"))),
  "Figure 10" = list(title = "Figure 10: Long-run Determinants of Underlying Inflation (2000 = 100, log scale)", log = TRUE,  # Section 4.7.3 (p. 25-26)
    cols = list(`Nominal unit labour costs` = rebase2000(V("NULCBS")),
                `Imported consumption goods prices` = rebase2000(V("PMCG")),
                `CPI excl volatiles` = rebase2000(V("PEX")))),
  "Figure 11" = list(title = "Figure 11: Long-run Determinants of Employment (2000 = 100, log scale)", log = TRUE,         # Section 4.8.1 (p. 27-28)
    cols = list(Employment = rebase2000(V("LE")), `Long-run determinants(a)` = rebase2000(exp(lr_employment)))))

compare_data_figure <- function(sheet, spec) {
  r   <- read_fig(sheet)
  r   <- r[r$Date <= as.Date("2018-04-01"), ]          # workfile holds data to 2018Q1; later quarters are forecasts
  # The RBA's workbook labels a quarter by a date in its last month (sometimes the 1st, sometimes
  # the 31st); map each to the workfile's quarter index.
  idx <- qindex(as.integer(format(r$Date, "%Y")), (as.integer(format(r$Date, "%m")) - 1L) %/% 3L + 1L)
  keep <- names(r)[-1]
  rba_l <- r |> pivot_longer(-Date, names_to = "series", values_to = "value") |> mutate(source = "RBA published")
  our_l <- bind_rows(lapply(keep, function(k) data.frame(Date = r$Date, series = k, value = spec$cols[[k]][idx], source = "R recomputation")))
  for (k in intersect(keep, spec$rescale)) {            # series the RBA plots on its own (right-hand) scale
    y <- r[[k]]; x <- our_l$value[our_l$series == k]; ok <- !is.na(x) & !is.na(y)
    fit <- lm(log(y[ok]) ~ log(x[ok]))
    cat(sprintf("  %s, %s: shown on the RBA's own axis, log-linear rescale slope %.3f (R-squared %.4f)
",
                sheet, k, coef(fit)[2], summary(fit)$r.squared))
    our_l$value[our_l$series == k] <- exp(coef(fit)[1] + coef(fit)[2] * log(x))
  }
  rba_l$Date <- qdate(idx)[match(rba_l$Date, r$Date)]       # put both on the same quarter-end dates
  our_l$Date <- qdate(idx)
  d <- bind_rows(rba_l, our_l) |> filter(!is.na(value)) |>
    mutate(series = factor(series, levels = keep), source = factor(source, levels = c("RBA published", "R recomputation")))
  if (!is.null(spec$panel)) d$panel <- factor(spec$panel[as.character(d$series)], levels = unique(spec$panel))
  p <- ggplot(d, aes(Date, value, colour = series, linewidth = source, alpha = source)) +
    geom_line() +
    scale_colour_manual(values = unname(c(navy, red, teal, gold, "#7A3E9D"))[seq_along(keep)]) +
    scale_linewidth_manual(values = c("RBA published" = 2.6, "R recomputation" = 0.7)) +
    scale_alpha_manual(values = c("RBA published" = 0.40, "R recomputation" = 1)) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = spec$title, caption = "Thick pale line: RBA's published chart data. Thin dark line: recomputed here from the workfile series using the Section 4 relationships.") +
    theme_rba()
  if (isTRUE(spec$log)) p <- p + scale_y_log10(breaks = scales::pretty_breaks(8))
  if (!is.null(spec$panel)) p <- p + facet_wrap(~panel, ncol = 1, scales = "free_y")
  if (isTRUE(spec$zero)) p <- p + geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.2)
  if (length(keep) == 1) p <- p + guides(colour = "none")
  met <- bind_rows(lapply(keep, function(k) {
    a <- filter(rba_l, series == k); b <- filter(our_l, series == k)
    j <- inner_join(a, b, by = "Date", suffix = c("_rba", "_ours")) |> filter(!is.na(value_rba), !is.na(value_ours))
    data.frame(figure = sheet, series = k, n = nrow(j), max_abs_gap = max(abs(j$value_ours - j$value_rba)),
               gap_pct_of_range = if (diff(range(j$value_rba)) < 1e-9) NA_real_ else 100 * max(abs(j$value_ours - j$value_rba)) / diff(range(j$value_rba)),
               correlation = suppressWarnings(cor(j$value_ours, j$value_rba))) }))
  list(plot = p, metrics = met)
}

data_fig_results <- lapply(names(recomputed), function(sh) compare_data_figure(sh, recomputed[[sh]]))
for (res in data_fig_results) print(res$plot)
data_fig_table <- bind_rows(lapply(data_fig_results, `[[`, "metrics"))
cat("\nFigures 2-11 recomputed from the workfile vs RBA chart data:\n")
print(as.data.frame(data_fig_table |> transmute(figure, series = substr(series, 1, 26), n, max_abs_gap = signif(max_abs_gap, 3),
                                                gap_pct_of_range = round(gap_pct_of_range, 2), correlation = round(correlation, 4))),
      row.names = FALSE)

# ---- 9. Save the charts (optional) ----------------------------------------------------------------------
# Set SAVE_FIGURES <- TRUE before sourcing (or here) to write every chart as a PNG into ./output_figures/
if (!exists("SAVE_FIGURES")) SAVE_FIGURES <- FALSE
if (SAVE_FIGURES) {
  dir.create("output_figures", showWarnings = FALSE)
  to_save <- c(list(baseline = baseline_plot(), fig12_overlay = p12, fig13_overlay = p13, fig14_overlay = p14,
                    fig15_overlay = p15, fig16_overlay = p16),
               setNames(lapply(c("fig12", "fig13", "fig14", "fig15", "fig16"), function(k) plot_gap(k, paste("Figure", sub("fig", "", k), "gap"))),
                        paste0(c("fig12", "fig13", "fig14", "fig15", "fig16"), "_gap")),
               setNames(lapply(data_fig_results, `[[`, "plot"), paste0("data_", gsub(" ", "", names(recomputed)))))
  for (nm in names(to_save)) ggsave(file.path("output_figures", paste0(nm, ".png")), to_save[[nm]], width = 10, height = 7.5, dpi = 150, bg = "white")
  message("Saved ", length(to_save), " charts to ", normalizePath("output_figures"))
}

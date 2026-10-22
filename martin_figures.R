# =============================================================================
# MARTIN (RBA) -- PART A: FIGURES FROM THE PAPER
# -----------------------------------------------------------------------------
# Reference : Ballantyne et al. (2019), "MARTIN Has Its Place: A Macroeconometric
#             Model of the Australian Economy", RBA Research Discussion Paper
#             2019-07.  File: rdp2019-07.pdf  ("p." below = the page number
#             printed on the page, not the PDF viewer's page counter).
# Data      : RBA's public plotting data, rdp-2019-07-graph-data.xlsx, from
#             https://www.rba.gov.au/publications/rdp/2019/2019-07/supplementary-information.html
#             (already unzipped into rba_supplement/extracted/).
#
# What this script does
#   * Reads every sheet in the graph-data workbook (Figures 2-4, 6-16).
#   * Redraws each figure in ggplot2 so it appears in the RStudio Plots pane.
#     Use the blue back/forward arrows in the Plots pane to flip through them.
#   * Figures 1 (schematic) and 5 (third-party data) are not published.
#
# How to run in RStudio
#   Open this file, press Ctrl+Shift+S (Source) or Ctrl+Alt+R (Run all).
#   Needs: readxl, ggplot2, dplyr, tidyr, scales  (install.packages(...) once).
#
# Part B (martin_model.R) will re-solve the model itself and overlay it on the
# Figure 12-16 reference curves loaded here.
# =============================================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ---- 0. Settings ------------------------------------------------------------

# When this file is sourced by martin_model.R only for its helpers and the RBA reference
# data, the figures are not redrawn: options(martin.draw = FALSE).
show_fig <- function(p) if (isTRUE(getOption("martin.draw", TRUE))) print(p)

# Path to the RBA workbook.  Edit if you moved things.  If the default does not
# exist we try the folder this script lives in (works when sourced in RStudio).
DATA_FILE <- "rba_supplement/extracted/rdp-2019-07-graph-data.xlsx"
if (!file.exists(DATA_FILE) && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  DATA_FILE <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), DATA_FILE)
}
stopifnot("Cannot find graph-data workbook; edit DATA_FILE" = file.exists(DATA_FILE))

# Colours used throughout (MARTIN = dark blue, comparison / second series = red)
COL <- c(navy = "#0B3C5D", red = "#C0392B", teal = "#1B998B", gold = "#E0A100",
         grey = "#6B7280", purple = "#7A3E9D")

theme_rba <- function(base = 10) {
  theme_minimal(base_size = base) %+replace%
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      axis.line.x        = element_line(colour = "grey30", linewidth = 0.4),
      axis.ticks.x       = element_line(colour = "grey30", linewidth = 0.4),
      axis.title         = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title         = element_text(face = "bold", hjust = 0.5, size = base + 2),
      plot.subtitle      = element_text(hjust = 0.5, colour = "grey30"),
      plot.caption       = element_text(hjust = 0, colour = "grey40", size = base - 2),
      strip.text         = element_text(face = "bold", hjust = 0),
      plot.margin        = margin(8, 12, 8, 8)
    )
}

# ---- 1. Reading helpers -----------------------------------------------------
# Every sheet has the same layout: rows 1-9 are title/source blurb; row 2-3 of
# the *data block* hold column headers; the numbers start two rows below.
# We read everything as text, locate the header row (the one that starts with
# "Date" or "Quarters"), then coerce the rest to numbers.

read_sheet_raw <- function(sheet) {
  suppressMessages(read_excel(DATA_FILE, sheet = sheet, col_names = FALSE,
                              col_types = "text"))
}

# Returns a tibble: first column = Date (time-series sheets) or Quarters (IRF
# sheets); remaining columns are named by their header row.  For sheets with a
# panel-title row above the series-name row, the panel is stored in attr "panels".
read_fig <- function(sheet) {
  raw <- read_sheet_raw(sheet)
  hdr <- which(raw[[1]] %in% c("Date", "Quarters"))[1]
  names_row  <- as.character(unlist(raw[hdr, ]))
  panels_row <- as.character(unlist(raw[hdr - 1, ]))
  body <- raw[-(1:(hdr + 1)), ]                    # skip header + blank spacer row
  body <- body[!is.na(body[[1]]), ]
  num  <- as.data.frame(lapply(body, as.numeric))
  out  <- as_tibble(num); names(out) <- make.unique(names_row)
  if (names_row[1] == "Date")
    out[[1]] <- as.Date(out[[1]], origin = "1899-12-30")   # Excel serial -> Date
  names(out)[1] <- names_row[1]
  attr(out, "panels") <- zoo_fill(panels_row)
  out
}

# Forward-fill the panel-title row (titles sit above the first column of a group).
zoo_fill <- function(x) {
  x[x == "NA"] <- NA
  for (i in seq_along(x)) if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]
  x
}

# Standard single-panel time-series line chart ---------------------------------
plot_ts <- function(df, title, subtitle = NULL, caption = NULL,
                    cols = unname(COL), log_y = FALSE, zero_line = FALSE) {
  long <- df |> pivot_longer(-Date, names_to = "series", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(series = factor(series, levels = setdiff(names(df), "Date")))
  p <- ggplot(long, aes(Date, value, colour = series)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = cols) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme_rba()
  if (zero_line) p <- p + geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3)
  if (log_y)     p <- p + scale_y_log10(breaks = pretty_breaks(8))
  p
}

SRC_ABS <- "Sources: ABS; Authors' calculations; RBA (data: RDP 2019-07 graph data)"

# ---- 2. Section 4.1 -- Supply side ------------------------------------------

# Figure 2: Exogenous trends, annualised (paper p. 13; text in Section 4.1, p. 12-13)
# Trend labour productivity, population and hours are Kalman-filtered
# random-walk-with-drift trends (equations in Section 4.1, p. 12).
fig2 <- read_fig("Figure 2") |> filter(!is.na(`Potential growth`))
p_fig2 <- plot_ts(fig2, "Figure 2: Exogenous Trends", "Annualised, per cent",
                  SRC_ABS, cols = c(COL[["teal"]], COL[["gold"]], COL[["red"]], COL[["navy"]]),
                  zero_line = TRUE)
show_fig(p_fig2)

# ---- 3. Section 4.2 -- Consumption and household sector ---------------------

# Figure 3: Consumption vs its long-run determinants (income, wealth, cash rate)
# (paper p. 15; long-run relationship in Section 4.2.1, p. 14)
fig3 <- read_fig("Figure 3") |> filter(!is.na(Consumption))
p_fig3 <- plot_ts(fig3, "Figure 3: Long-run Determinants of Consumption",
                  "2000 = 100, log scale", SRC_ABS,
                  cols = c(COL[["navy"]], COL[["red"]]), log_y = TRUE)
show_fig(p_fig3)

# ---- 4. Section 4.3 -- Housing sector ---------------------------------------

# Figure 4: Dwelling investment-to-consumption ratio, per cent (paper p. 17)
# Motivates the long-run dwelling investment equation (Section 4.3.1, p. 16-17).
fig4 <- read_fig("Figure 4") |> filter(!is.na(Series)) |>
  rename(`Dwelling investment / consumption` = Series)
p_fig4 <- plot_ts(fig4, "Figure 4: Dwelling Investment-to-consumption Ratio",
                  "Per cent", "Sources: ABS; RBA", cols = COL[["navy"]]) +
  theme(legend.position = "none")
show_fig(p_fig4)

# Figure 5 (housing prices vs rents, p. 18) uses Corelogic data the RBA withheld,
# so it is not in the public workbook and is skipped.

# ---- 5. Section 4.4 -- Business investment ----------------------------------

# Figure 6: Business investment quantities and prices (paper p. 20)
# Top panel: investment shares of GDP.  Bottom: relative price of non-mining
# investment (the term with the restricted -1 coefficient, Section 4.4.1, p. 19).
fig6 <- read_fig("Figure 6") |> filter(!is.na(Total))
fig6_l <- fig6 |>
  pivot_longer(-Date, names_to = "series", values_to = "value") |>
  mutate(panel = ifelse(series == "Non-mining investment deflator to GNE deflator",
                        "Relative prices (2000 = 100)",
                        "Business investment as a share of GDP (per cent)"),
         panel = factor(panel, levels = unique(panel)),
         series = factor(series, levels = c("Total", "Non-mining",
                                            "Non-mining investment deflator to GNE deflator")))
p_fig6 <- ggplot(fig6_l, aes(Date, value, colour = series)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c(COL[["navy"]], COL[["red"]], COL[["teal"]])) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Figure 6: Business Investment Quantities and Prices",
       caption = "Sources: ABS; Authors' calculations") + theme_rba()
show_fig(p_fig6)

# ---- 6. Section 4.5 -- Foreign trade ----------------------------------------

# Figure 7: Export values as a share of GDP (paper p. 22)
# Time-varying constants (Appendix A, p. 39) account for the drifting shares.
fig7 <- read_fig("Figure 7") |> filter(!is.na(Services))
fig7_l <- fig7 |>
  pivot_longer(-Date, names_to = "series", values_to = "value") |>
  mutate(panel = ifelse(series == "Resource", "Resource exports", "Non-resource exports"),
         panel = factor(panel, levels = c("Non-resource exports", "Resource exports")))
p_fig7 <- ggplot(fig7_l, aes(Date, value, colour = series)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c(Agriculture = COL[["teal"]], Manufacturing = COL[["gold"]],
                                 Other = COL[["grey"]], Services = COL[["navy"]],
                                 Resource = COL[["red"]])) +
  scale_x_date(date_breaks = "6 years", date_labels = "%Y") +
  labs(title = "Figure 7: Export Values", subtitle = "Share of GDP, per cent",
       caption = "Sources: ABS; RBA") + theme_rba()
show_fig(p_fig7)

# Figure 8: Imports vs import-adjusted demand and relative prices (paper p. 23;
# long-run import equation, Section 4.5.3, p. 22-23)
fig8 <- read_fig("Figure 8") |> filter(!is.na(Imports))
p_fig8 <- plot_ts(fig8, "Figure 8: Long-run Determinants of Imports",
                  "2000 = 100, log scale", SRC_ABS,
                  cols = c(COL[["navy"]], COL[["red"]], COL[["teal"]]), log_y = TRUE)
show_fig(p_fig8)

# ---- 7. Section 4.7 -- Prices and wages -------------------------------------

# Figure 9: Wage price index and AENA, year-ended growth (paper p. 24)
fig9 <- read_fig("Figure 9") |> filter(!is.na(AENA))
p_fig9 <- plot_ts(fig9, "Figure 9: Nominal Wage Measures", "Year-ended growth, per cent",
                  "Sources: ABS; RBA", cols = c(COL[["navy"]], COL[["red"]]))
show_fig(p_fig9)

# Figure 10: Underlying inflation vs input costs (paper p. 26; mark-up model,
# Section 4.7.3, p. 25-26)
fig10 <- read_fig("Figure 10") |> filter(!is.na(`CPI excl volatiles`))
p_fig10 <- plot_ts(fig10, "Figure 10: Long-run Determinants of Underlying Inflation",
                   "2000 = 100, log scale", "Sources: ABS; RBA",
                   cols = c(COL[["navy"]], COL[["teal"]], COL[["red"]]), log_y = TRUE)
show_fig(p_fig10)

# ---- 8. Section 4.8 -- Labour market ----------------------------------------

# Figure 11: Employment vs its long-run determinants (paper p. 28; Section 4.8.1, p. 27-28)
fig11 <- read_fig("Figure 11") |> filter(!is.na(Employment))
p_fig11 <- plot_ts(fig11, "Figure 11: Long-run Determinants of Employment",
                   "2000 = 100, log scale", SRC_ABS,
                   cols = c(COL[["navy"]], COL[["red"]]), log_y = TRUE)
show_fig(p_fig11)

# ---- 9. Section 5 -- Model dynamics (impulse responses) ---------------------
# Figures 12-16 are deviations from baseline after a shock.  The sheets list
# each panel with two columns (MARTIN, MSM) or (fixed, flexible cash rate).

# Axis units for each panel title (ppt = percentage points)
PANEL_UNITS <- c(
  "Cash rate" = "ppt", "GDP" = "%", "Unemployment rate" = "ppt",
  "Trimmed mean inflation; year-ended" = "ppt", "WPI growth; year-ended" = "ppt",
  "Real exchange rate" = "%", "Real TWI" = "%", "Consumption" = "%",
  "Dwelling investment" = "%", "Business investment" = "%",
  "Non-resource exports" = "%", "Resource exports" = "%", "Imports" = "%",
  "Housing prices" = "%")

# Read an impulse-response sheet into long format: quarter, panel, series, value.
read_irf <- function(sheet) {
  d  <- read_fig(sheet)
  pn <- attr(d, "panels")
  d |> pivot_longer(-Quarters, names_to = "col", values_to = "value") |>
    mutate(series = sub("\\.\\.\\.[0-9]+$|\\.[0-9]+$", "", col),
           panel  = pn[match(col, names(d))]) |>
    select(-col)
}

plot_irf <- function(df, title, subtitle, colours, caption = "Sources: Authors' calculations; RBA") {
  df <- df |>
    mutate(panel_lab = paste0(sub("; ", "\n", panel), "  (", PANEL_UNITS[panel], ")"),
           panel_lab = factor(panel_lab, levels = unique(panel_lab)))
  ggplot(df, aes(Quarters, value, colour = series)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~panel_lab, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = colours) +
    scale_x_continuous(breaks = seq(0, 40, 8)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = "Quarters") +
    theme_rba() + theme(axis.title.x = element_text())
}

# Figure 12: 100bp cash-rate rise held for four quarters (paper p. 32; Section 5.1, p. 31-34)
fig12 <- read_irf("Figure 12")
p_fig12 <- plot_irf(fig12, "Figure 12: Monetary Policy Shock", "Aggregate variables",
                    c(MARTIN = COL[["navy"]], MSM = COL[["red"]]))
show_fig(p_fig12)

# Figure 13: expenditure components, same shock (paper p. 33)
fig13 <- read_irf("Figure 13")
p_fig13 <- plot_irf(fig13, "Figure 13: Monetary Policy Shock", "Expenditure components",
                    c(MARTIN = COL[["navy"]], MSM = COL[["red"]]))
show_fig(p_fig13)

# Figure 14: GDP response by transmission channel (paper p. 34)
# 'No exchange rate channel' holds the real TWI fixed; the third line also
# switches off housing and equity price responses.
fig14 <- read_fig("Figure 14") |>
  pivot_longer(-Quarters, names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = c("No exchange rate or asset price channels",
                                            "No exchange rate channel", "All channels")))
p_fig14 <- ggplot(fig14, aes(Quarters, value, colour = series)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(COL[["teal"]], COL[["gold"]], COL[["navy"]])) +
  scale_x_continuous(breaks = seq(0, 40, 4)) +
  labs(title = "Figure 14: Gross Domestic Product", subtitle = "Deviation from baseline, per cent",
       caption = "Sources: Authors' calculations; RBA", x = "Quarters") +
  theme_rba() + theme(axis.title.x = element_text())
show_fig(p_fig14)

# Figure 15: transitory 10% real exchange-rate depreciation (paper p. 35; Section 5.2)
fig15 <- read_irf("Figure 15")
p_fig15 <- plot_irf(fig15, "Figure 15: Exchange Rate Depreciation", "Aggregate variables",
                    c(MARTIN = COL[["navy"]], MSM = COL[["red"]]))
show_fig(p_fig15)

# Figure 16: persistent 10% fall in housing prices (paper p. 36; Section 5.3, p. 36-37)
fig16 <- read_irf("Figure 16")
p_fig16 <- plot_irf(fig16, "Figure 16: Housing Price Shock", "Deviation from baseline",
                    c(`Fixed cash rate` = COL[["navy"]], `Flexible cash rate` = COL[["red"]]))
show_fig(p_fig16)

# Keep the reference IRFs for Part B to overlay against
REF_IRF <- list(fig12 = fig12, fig13 = fig13, fig14 = fig14, fig15 = fig15, fig16 = fig16)
if (isTRUE(getOption("martin.draw", TRUE))) message("Part A done: figures 2-4 and 6-16 drawn. Use the Plots-pane arrows to browse.")

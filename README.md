# MARTIN in R

Re-running the RBA's MARTIN model (RDP 2019-07, "MARTIN Has Its Place") in R, and comparing the
results with the charts published in the paper.

Full notes (method, results, caveats, setup): see [`MARTIN_NOTES.md`](MARTIN_NOTES.md).

## Run it (RStudio)

1. Open `martin_model.R`, press **Ctrl+Shift+S** (Source). Charts appear in the Plots pane (use the
   blue arrows to flip through them); the console prints the checks and comparison tables.
   First run decodes the RBA workfile (~1 min); after that it takes ~45 s.
2. Optional: `SAVE_FIGURES <- TRUE` before sourcing writes every chart as a PNG to `output_figures/` (git-ignored: the charts contain the RBA's published curves).
3. `martin_figures.R` on its own just redraws the paper's Figures 2-16 from the RBA's published chart data (Part A).

Needs R with `readxl ggplot2 dplyr tidyr scales`. No EViews needed.

## Files

| File | What it is |
|---|---|
| `martin_model.R` | **Main script.** Builds the model, checks it, runs the Section 5 experiments, draws RBA-vs-R charts |
| `martin_engine.R` | Turns the model's equations into R functions and solves them quarter by quarter |
| `martin_workfile.R` | Decodes the RBA's EViews workfile (`martin_public.wf1`) into data and equations |
| `martin_figures.R` | Part A: the paper's figures drawn from the RBA's published chart data |
| `rba_supplement/` | The RBA's supplementary download (workfile, `.prg` scripts, graph data) |
| `rdp-2019-07-online-appendix.pdf` | Every equation with estimates |
| `github_ref/` | Unzipped third-party EViews rebuild (reference only; not used by the code) |

## What the checks show

* **Equations**: 213 of 217 return the RBA's stored 2018Q2-2019Q1 numbers (the workfile stores ~7 digits).
  79 of 86 estimated equations have in-sample residual spread within 2x the RBA's recorded value.
* **Figures 12-15** (monetary policy and exchange-rate shocks): our impulse responses sit on the RBA's
  within a few per cent of the peak response (median gap about 2-3%, worst about 14%; correlation >= 0.97).
* **Figures 2-4, 6-11** recomputed from the raw workfile series match the RBA's chart data (correlation >= 0.98).
* **Figure 16** (housing price shock): the first ~8-10 quarters match closely; later quarters drift
  (median gap about 37% of peak; the small inflation panel is worst). The paper does not say exactly how the shock was imposed, so this is
  our best reconstruction.

## Caveats

* The RBA's public workfile replaces 21 proprietary series with simulated data, and its add factors
  ("judgement") are zero. The baseline projection is illustrative, **not** an RBA forecast.
* Several workfile series are stored in a compact form that can't be decoded directly. They are rebuilt
  from the generating commands the workfile itself records (see `rebuild_recipe_series()`), and the
  time-varying export constants and three equation-object coefficients are recovered from the RBA's
  stored forecast (see `calibrate_tvc()` and `recover_eq_constants()`).
* The file-format decoding is reverse-engineered, not an official specification.

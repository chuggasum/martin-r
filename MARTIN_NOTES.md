# MARTIN in R: project notes

Re-running the RBA's MARTIN macroeconometric model (Ballantyne et al., RDP 2019-07,
*MARTIN Has Its Place*) in R, and comparing the results with the charts in the paper.

## 1. What this project does

1. Reads the RBA's public EViews workfile (`martin_public.wf1`) directly from its bytes. This gives the
   data (649 series, 1959Q1-2118Q4) and the full model text: 217 equations with every estimated
   coefficient substituted in at full precision.
2. Turns the equations into R functions and solves them quarter by quarter.
3. Checks the R model against the RBA's own stored numbers.
4. Runs the baseline projection (2019Q1-2029Q4) and the paper's Section 5 experiments
   (monetary-policy, exchange-rate and housing-price shocks), exactly as the RBA's script
   `martin_rdp_irfs_public.prg` defines them.
5. Plots our impulse responses against the RBA's published ones, with the gap between them.
6. Recomputes the data figures (Figures 2-4, 6-11) from the raw workfile series and overlays them
   on the RBA's chart data.

## 2. Setup and running

The RBA's files are **not** included in this repository (see section 8). Download once:

1. `https://www.rba.gov.au/publications/rdp/2019/2019-07/rdp-2019-07-supplementary-information.zip`
2. Unzip into `rba_supplement/extracted/` so that `rba_supplement/extracted/martin_public.wf1` and
   `rba_supplement/extracted/rdp-2019-07-graph-data.xlsx` exist.
3. In R: `install.packages(c("readxl", "ggplot2", "dplyr", "tidyr", "scales"))`.
4. Open `martin_model.R` in RStudio and press **Ctrl+Shift+S**. Charts appear in the Plots pane.
   The first run decodes the workfile (~1 min); later runs take ~45 s.
   Set `SAVE_FIGURES <- TRUE` first to write every chart to `output_figures/`.

`martin_figures.R` on its own redraws Figures 2-16 from the RBA's published chart data (no model).

## 3. Files

| File | Role |
|---|---|
| `martin_model.R` | Main script: build, check, baseline, shocks, comparison charts and tables |
| `martin_engine.R` | Equation parser, block ordering, Newton solver, constant recovery |
| `martin_workfile.R` | Decoder for the RBA workfile; rebuilds the compactly stored series |
| `martin_figures.R` | Part A: the paper's figures from the RBA's chart data |
| `output_figures/` | Created when `SAVE_FIGURES <- TRUE` (20 PNGs). Not committed: kept in a separate private repo |

Page references in the code comments are the page numbers printed in the paper, checked against its
table of contents. "App." means the paper's Online Appendix.

## 4. Results

**Checks against the RBA's numbers**

| Check | Result |
|---|---|
| Equations returning the RBA's stored 2018Q2-2019Q1 values (tolerance 1e-3) | 213 of 217 |
| Estimated equations with in-sample residual sd within 2x the RBA's recorded value | 79 of 86 |
| Model blocks | 164 recursive equations + 1 simultaneous block of 53 (EViews reports the same) |

**Impulse responses, R re-run vs RBA published** (gap = largest absolute difference as a share of the
RBA's peak response)

| Figures | Panels | Median gap | Worst gap | Correlation |
|---|---|---|---|---|
| 12-15 (monetary policy and exchange rate shocks) | 21 | 2.3% | 13.6% | 0.973 or better |
| 16 (housing price shock) | 12 | 36.9% | 146% | down to -0.23 |

Example: after a 1-point cash-rate rise held for four quarters, GDP falls by 0.806% (RBA) vs 0.821%
(ours) at the trough, unemployment rises by 0.293 vs 0.297 points.

**Data figures recomputed from the workfile** match the RBA's chart data with correlation 0.989 or
better (gaps under about 1% of the series range, except Figure 2 productivity/potential growth at
about 7%, Figure 3 determinants at 11% after rescaling, and the wage price index at 3%).

## 5. How the numbers were obtained

- **Coefficients.** Taken from the model text stored in the workfile (full precision). No equation
  was re-estimated.
- **Three coefficients not printed in the model text** (the model refers to `EQ_PW`, `EQ_RTWI` and
  `EQ_WR2R` equation objects):
  - `EQ_PW.@COEF(1)` is the first number in the model's own wage equation (App. eq. 88: 0.43).
  - `EQ_RTWI` (App. eq. 140) was recovered by fitting the exchange-rate equation to the RBA's stored
    forecast, which satisfies it exactly (fit residual about 3e-7). Result: speed 0.2181,
    terms-of-trade 0.6465, short-run 0.2158. These agree with the appendix (0.22, 0.65, 0.22).
  - `EQ_WR2R.@COEF(1)` was recovered the same way from the stored forecast.
- **Time-varying export constants** (`XM_C`, `XS_C`, `XRE_C`, paper Appendix A, p. 39): their stored
  arrays are unreadable, so the value that makes each export equation hold exactly in 2018Q2 is
  solved for and then held flat, as the RBA's own history records.
- **Rebuilt "recipe" series.** About 25 exogenous series (dummies, trends, the inflation target of 2.5,
  the corporate tax rate) are rebuilt from the generating commands the workfile records for them.
  `@trend` counts from zero at 1959Q1; this reading reproduces the RBA's stored forecasts to 1e-6,
  while counting from each sample's start does not.
- **Scalars and parameters** (policy-rule weights of 2, neutral real rate target 0.689, steady-state
  growth rates and so on) are read from the workfile's scalar objects.
- **Workfile decoding method.** Reverse-engineered by inspecting the file's bytes: an object table
  with names and file offsets, series records marked by a fixed byte pattern followed by 640
  little-endian doubles, EViews NA stored as 1e-37. No EViews, and no third-party reverse-engineering
  tools were used.
- **Scenarios** follow `martin_rdp_irfs_public.prg`: a pinned variable is imposed for N quarters
  (EViews "exclude") and then released. Neutral rate, public consumption and public investment are
  held at baseline for 40 quarters in every scenario.

## 6. Things to know

1. **The baseline is not an RBA forecast.** The public workfile replaces 21 proprietary series with
   simulated data (expected earnings, housing prices, overseas price level, Brent oil, bond yields, the
   neutral rate, world GDP and world real rates), and the RBA's judgement adjustments (add factors)
   are zero. The projection is illustrative. The RBA says its own scenarios from this file "will not
   match the paper exactly".
2. **Housing price shock (Figure 16) is a partial match.** The first 8-10 quarters match closely; later
   quarters drift. The paper does not say exactly how the shock was imposed. Our reconstruction pins
   housing prices (and the real exchange rate, as in the RBA's public script) for the fixed-rate case,
   and for the flexible-rate case applies the housing-equation add factors implied by the fixed case,
   with the paper's 75bp cash-rate cut imposed. Variants I tried (leaving the exchange rate free,
   pinning other variables, different add-factor lengths) did not improve the flexible case.
   `HOUSING_PIN_RTWI` in `martin_model.R` switches the exchange-rate treatment.
3. **Decoding is unofficial.** The file-format knowledge is reverse-engineered, not a specification.
   It worked for this workfile; other EViews files may differ.
4. **Some stored series are not exact solutions.** A few trend series (trend productivity and
   population growth) in the stored forecast do not follow their own equations, so the stored path and
   a fresh solve differ slightly. Over 2019-2020 our baseline stays close to the RBA's stored path
   (GDP within about 0.1%, unemployment within about 0.07 points).
5. **Float precision.** The workfile stores forecast series to about 7 significant digits, so exact
   checks use tolerances of 1e-3 or tighter only where meaningful.
6. **Figure 3** shows the long-run determinants on the RBA's own right-hand axis. Only their shape can
   be compared, so ours is rescaled by a fitted log-linear map (slope 0.442, R-squared 0.984). The
   console prints the fit.
7. **Figure 5 is not reproduced.** It uses Corelogic housing data the RBA withholds. Figure 1 is a
   schematic.
8. **Cash rate in the baseline goes slightly negative around 2019** because of the simulated stand-in
   data; the RBA's stored path does too. A third-party EViews rebuild
   (`github.com/DavidAStephan/MARTIN`) puts a 0.1 floor on the cash rate; the RBA's public model does
   not, and neither does this project.
9. **Third-party repo.** That EViews rebuild was used only as a reference (it documents how several
   dummy and trend series are defined and how its author sourced data from the ABS and FRED).
   Nothing in this code depends on it, and its numbers are not used.

## 7. Where the RBA's data comes from (for context)

The paper's Table 1 (p. 11-12) lists the sources: the ABS for national accounts, labour market, CPI and
wages; RBA-built series for the neutral rate, NAIRU, inflation expectations, trend productivity and
population, and net asset transfers; and paid sources (Corelogic, Bloomberg, MSCI, Refinitiv, BIS,
Yieldbroker) for housing prices, oil, equities, exchange rates and bond yields. The workfile's history
strings show imports from internal RBA files that are not public. The RBA estimates its equations on
its own data vintages (to 2018Q2), so rebuilding from today's ABS releases would not give identical
coefficients. That is why this project uses the RBA's coefficients rather than re-estimating.

## 8. What is not in this repository, and why

The RBA's files carry a notice that they may not be reproduced or distributed without the Bank's
consent, so they are excluded (see `.gitignore`): the supplementary-information zip and everything
extracted from it (workfile, graph data, scripts), the RBA PDFs (paper, online appendix, read-me), and
the cached decoded data. The third-party repo zip and its unzipped copy are also excluded. The saved
comparison charts (`output_figures/`) contain the RBA's published curves, so they are kept out of this repository too (in a separate private repo); regenerate them with `SAVE_FIGURES <- TRUE`.

## 9. Ideas for next steps

- Settle the housing-shock specification (needs the RBA's exact shock profile for Figure 16).
- Replace the 21 simulated series with real public data (ABS, FRED, World Bank) for a genuine baseline
  forecast, following the data sources documented in the third-party repo's `data_sources.xlsx`.
- Re-estimate the equations in R on current data and compare with the RBA's coefficients.
- Add stochastic simulation using the recorded residual standard deviations (`@INNOV` in the model
  text) for probability bands, as described on p. 5 of the paper.

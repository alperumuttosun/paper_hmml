# Dating precision in the vHMML Reading Room catalogue

R code and outputs for a study of how precisely manuscripts are dated in the
vHMML Reading Room catalogue of the Hill Museum & Manuscript Library.

## Data

`vhmml_rr_fulldata.zip` is the full vHMML Reading Room dataset (JSON), as
downloaded on 8 November 2026 from https://www.vhmml.org/datasets/.
It is published by the Hill Museum & Manuscript Library under a
CC BY 4.0 licence and is included here unchanged so that the results can be
reproduced with the same version. The dataset on the vHMML site is updated
over time.

## How to run

1. Put `hmml_step00-07.R` and `vhmml_rr_fulldata.zip` in a folder named `paper_hmml`.
2. In step00 of the script, set `bases` to the folder that contains `paper_hmml`.
3. Run the whole script in R (tested with R 4.5.2). Missing packages are installed.

If the zip file is present, the download is skipped. Fitted models are cached as
`.rds` files, so the first run is slow and later runs are fast.

## What each step does

| Step | Task |
|---|---|
| step00 | Load packages, set the project folder |
| step01 | Download and unzip the dataset (only if missing) |
| step02 | Parse the JSON into one row per manuscript part |
| step03 | Classify dating precision, find the placeholder block, check heaping |
| step04 | Multilevel logistic model, temporal reconstructions, Figures 1–4 |
| step05 | Robustness checks, Figure 5 |
| step06 | Observed-scale ICC, script × country model, random-draw century assignment |
| step07 | Full Laplace fit, models without printed or papyrus parts, centre-weighted weights |

## Files

| File | Content |
|---|---|
| `hmml_step00-07.R` | All code |
| `vhmml_rr_fulldata.zip` | Input data (see above) |
| `hmml_s03_*.csv` … `hmml_s07_*.csv` | Output tables; the comment after each `out()` call in the script says what each file contains |
| `hmml_fig1_*.png` … `hmml_fig5_*.png` | Figures 1–5 |

## Licence

Code: MIT. Data: CC BY 4.0, © Hill Museum & Manuscript Library.

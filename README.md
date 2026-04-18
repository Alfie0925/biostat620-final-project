# BRFSS Mental Distress Website

This repository contains a Quarto website for a BIOSTAT 620 final project on frequent mental distress among U.S. adults using BRFSS 2020-2024.

Live site:

- <https://alfie0925.github.io/biostat620-final-project/>

## Repository layout

- `index.qmd`: main report homepage
- `data-processing.qmd`: data acquisition, cleaning, and exploratory analysis page
- `brfss_subset.rds`: analytic dataset used to render the site
- `prep_brfss_subset.R`: data preparation script
- `docs/`: rendered GitHub Pages output

## Local rendering

Run the site locally with:

```bash
quarto render
```

If you want to rebuild the analytic dataset from raw CDC BRFSS files, download `LLCP2020.XPT` through `LLCP2024.XPT` from the CDC BRFSS annual data pages and place them at the repository root before running:

```bash
Rscript prep_brfss_subset.R
```

## GitHub Pages

This site is intended to be deployed from the `main` branch using the `/docs` folder in GitHub Pages settings.

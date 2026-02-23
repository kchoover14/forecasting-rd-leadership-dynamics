# Time Series Models Forecast a Shift in Global R&D Leadership Away from the West

This project combines V-Dem and World Bank data to examine how aging demographics are reshaping global R&D leadership across six world regions from 1996 to 2019. Time series analysis tracks trends in old age dependency, R&D expenditure, scientific publications, and researchers in R&D; Jenks natural breaks classification groups countries by dependency profile to compare expenditure trajectories; and linear regression models with region interactions generate predicted values for two R&D productivity measures as aging increases. The core finding: the West has already plateaued in researcher growth and publication output, while Asia, Latin America, and the Middle East continue to grow -- and predictive models confirm the divergence will deepen if current demographic and investment trajectories continue.

## Portfolio Page

The [portfolio page](https://kchoover14.github.io/forecasting-rd-leadership-dynamics) includes a full project narrative, key findings, and figures.

## Tools & Technologies

- **Languages:** R
- **Tools:** RStudio, GitHub
- **Packages:** `vdemdata`, `wbstats`, `countrycode`, `dplyr`, `tidyr`, `ggplot2`, `plotly`, `ggeffects`, `classInt`, `scales`, `htmlwidgets`

## Reproducibility

This project uses [`renv`](https://rstudio.github.io/renv/) for package version management. To restore the environment:

```r
renv::restore()
```

## Expertise

Translating demographic trend data into actionable forecasts for R&D investment strategy -- building regression models that reveal where structural workforce shifts are underway before they are visible in aggregate statistics.

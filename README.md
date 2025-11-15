# leadtimefluxR

An R implementation of the core ideas in *Lead Times in Flux*: empirical lead-time distributions by cohort,
normalized L1 divergence against baselines, STL decomposition of the divergence series, pickup curves, the
pickup error bound, and a small set of decision rules. Includes a synthetic generator.

## Install for local development

```
# from the parent directory of leadtimefluxR/
install.packages(c("devtools","dplyr","tidyr","lubridate","ggplot2"), repos="https://cloud.r-project.org")
devtools::load_all("leadtimefluxR")
```

## Quick start

See `scripts/quickstart.R` for a minimal end-to-end example.

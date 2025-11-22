#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(leadtimefluxR))
leadtimefluxR::make_paper_artifacts("paper_artifacts")
leadtimefluxR::make_bdarma_impact_artifacts("paper/impact_case")

#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
out_dir  <- if (length(args) >= 1) args[1] else "paper_artifacts"
horizons <- if (length(args) >= 2) as.integer(strsplit(args[2], ",")[[1]]) else c(7L,14L,21L)
seed     <- if (length(args) >= 3) as.integer(args[3]) else 20251115

leadtimefluxR::make_paper_artifacts(out_dir = out_dir, horizons = horizons, seed = seed)

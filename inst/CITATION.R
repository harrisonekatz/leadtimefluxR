citHeader("To cite leadtimefluxR in publications, use:")

meta  <- utils::packageDescription("leadtimefluxR")
yr    <- if (!is.null(meta$Date)) sub("-.*", "", meta$Date) else format(Sys.Date(), "%Y")
vers  <- if (!is.null(meta$Version)) meta$Version else "dev"
ttl   <- if (!is.null(meta$Title)) meta$Title else "Lead-time divergence and pickup risk toolkit in R"

bib <- bibentry(
  bibtype = "Manual",
  title   = sprintf("leadtimefluxR: %s", ttl),
  author  = c(person(given = "Harrison", family = "Katz", role = c("aut","cre"))),
  year    = yr,
  note    = sprintf("R package version %s", vers),
  url     = "https://github.com/harrisonekatz/leadtimefluxR"
  # After you mint a DOI with Zenodo, add for example:
  # , doi = "10.5281/zenodo.1234567"
)

print(bib, style = "Bibtex")

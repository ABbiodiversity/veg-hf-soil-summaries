## long format summary
cat("Processing long format summaries ... ")
d_long <- try(make_vegHF_wide_v6(d,
  col.label=UID_COL,
  col.year=BASE_YR,
  col.HFyear="YEAR",
  col.HABIT=VEG_COL,
  col.SOIL="Soil_Type_1",
  sparse=TRUE, HF_fine=TRUE, wide=FALSE,
  tol=TOL, unround=UNROUND), silent=TRUE)
if (inherits(d_long, "try-error")) {
  cat("ERROR\n\n")
  stop(paste("Check your settings\n", d_long))
}
cat("OK\n\n")

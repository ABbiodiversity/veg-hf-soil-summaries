## do not do this when Shape_Area is missing
if (AREA) {
  cat("Processing wide format summaries ... ")
  ## wide format summary with >=0 unknown age area
  d_wide0 <- make_vegHF_wide_v6(d_long,
    col.label=UID_COL,
    col.year=BASE_YR,
    col.HFyear="YEAR",
    col.HABIT="Combined_ChgByCWCS",
    col.SOIL="Soil_Type_1",
    sparse=TRUE, HF_fine=TRUE, widen_only=TRUE,
    tol=TOL, unround=UNROUND)
  ## wide format summary with unknown age area redistributed
  cat("OK\n\nRedistributing unknown ages:\n\n")
  dx <- nonDuplicated(d, d[[UID_COL]], TRUE)[rownames(d_wide0[[1]]),]
  d_wide <- fill_in_0ages_v6(d_wide0, dx$NSRNAME, ages_list)
} else {
  cat("OK\n\nSkipping wide format summaries")
  d_wide <- NULL
}
cat("\n\n")

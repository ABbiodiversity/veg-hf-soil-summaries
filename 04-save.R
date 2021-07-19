## output file name
if (is.null(OUTPUT)) {
  stub <- strsplit(FILE, "\\.")[[1]]
  stub <- paste0(stub[-length(stub)], collapse="_")
  OUTPUT <- paste0(stub, "_", Sys.Date(), ".RData")
}

## session info etc
.veghf_settings <- list(
  input=list(
    UID_COL=UID_COL,
    BASE_YR=BASE_YR,
    FILE=FILE,
    TABLE=TABLE,
    SUB_COL=SUB_COL,
    SUB_VAL=SUB_VAL,
    OUTPUT=OUTPUT,
    AREA=AREA,
    COMMENTS=COMMENTS,
    TOL=TOL,
    SAVE=SAVE,
    UNROUND=UNROUND),
  session=list(date=Sys.time(), info=sessionInfo())
)

## save output
if (!is.na(OUTPUT)) {
    cat("Saving results:\n", OUTPUT)
    save(list=c("d_long", "d_wide", SAVE), .veghf_settings, file=OUTPUT)
}
cat("\n\nDONE\n\n")

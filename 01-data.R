## read in data
if (endsWith(tolower(FILE), ".csv")) {
  cat("Reading CSV file:\n", FILE, "... ")
  d <- read.csv(FILE)
} else {
  cat("Connecting to SQLite database:\n", FILE)
  db <- dbConnect(RSQLite::SQLite(), FILE)
  cat("\n\nFound the following tables:\n")
  cat(paste(dbListTables(db), collapse="\n"))
  cat("\n\nLoading table:\n", TABLE)
  d <- dbReadTable(db, TABLE)
  cat("\n\nDisconnecting ... ")
  dbDisconnect(db)
  d <- make_char2fact(d)
}

## take a subset if needed
if (!is.null(SUB_COL)) {
  cat("OK\n\nTaking subset ... ")
  d <- d[d[[SUB_COL]] %in% SUB_VAL,]
}

if (AREA_COL != "Shape_Area") {
  d[["Shape_Area"]] <- d[[AREA_COL]]
  d[[AREA_COL]] <- NULL
}
cat("OK\n\n")

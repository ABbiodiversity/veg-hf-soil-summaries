## unique ID (can be all the same value)
UID_COL    = "ABMI_ID_Wi"

## vegetation column
VEG_COL    = "Combined_ChgByCWCS"

## shape area when AREA=TRUE
AREA_COL   = "Shape_Area"

## base year for surveys or HF inventory
## this is used to calculate years since last disturbance
## (i.e. base year - origin year)
## use a numeric value when it is the same for each record
## use a character value to indicate a field when it
## varies by record
BASE_YR    = 2019 # or column name as character

## input file name, can contain the path (i.e. /dir/file.csv)
## file type can be .csv or .sqlite
FILE       = "sites-example.sqlite"

## table name for SQLite database
## ignored for csv files
TABLE      = "Summary_Buffers"

## optional, field name to be used for subsetting
## can be NULL (ignored)
SUB_COL    = "Section" # or NULL
## values in the <SUB_COL> field to keep
## can be a single value or a character vector
SUB_VAL    = c("NE","NW","SE","SW")

## optional, the name of the output file
## it can contain path as well (e.g. /dir/file.RData)
## if NULL, <FILE>_YYYY-MM-DD.RData is used
OUTPUT     = NULL

## keep as TRUE when a Shape_Area field is present
## (long and wide format summaries can be calculated)
## set it to FALSE when e.g. doing point intersections
## (only long summary can be calculated)
AREA       = TRUE

## add comments here, e.g. describing the characteristics
## of the input (backfilled v6.1 + 2017 HFI) when it is
## not trivial from file name, or describe purpose of
## the summaries as a reminder
COMMENTS   = ""

## tolerance level for excluding unknown aged harvest areas
## 0 means no tolerance, 1 means it is OK to exlude all the landbase
TOL        = 0

## optional, object names to save as character vector or NULL
SAVE       = NULL

## wether to unround rounded origin year values pre-2000
UNROUND    = FALSE


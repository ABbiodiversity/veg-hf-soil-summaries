# Veg/soil/HF processing

This script takes input as the attributes from the backfilled veg/soil/HF data
and produces output in long and wide format (also deals with unknown ages).

## How to get it

Clone this repo or download as a zip file and extract.
Now you can use the `/veghf` folder as required.
Keep files in the folder in the same folder.

## Usage

### Interactive use with R GUI or RStudio

Open R GUI, set work directory to the `/veghf` folder in R GUI.
Open the project via the `veghf.Rproj` file in RStudio, this will set the
working directory to point to the `/veghf` folder.

Edit the top section in the `index.R` file (settings are explained below).

However, if your are processing multiple projects, it might make more sense
to have the project specific file somewhere else, that is calling
the script in the `/veghf` folder. Here is an example file that you can use
(these are real ones I used to troubleshoot).

This one run fine: note that I am temporarily resetting the working directory
to the current location of the `/veghf` folder, then resetting.

``` R
## 20200130_SC_veghf2017_NWSAR
FILE       = "s:/GC_eric/FromEric/IC_Reporting_Recovery/NWSAR/20200130_SC_veghf2017_NWSAR.sqlite"
TABLE      = "SC_NWSAR_project_Summary_Vegetation_HFI_rawdata"
SUB_COL    = NULL
SUB_VAL    = ""
UID_COL    = "Region"
VEG_COL    = "Vegetation"
BASE_YR    = 2017
AREA_COL   = "Shape_Area"
AREA       = TRUE
OUTPUT     = NULL
COMMENTS   = "NWSAR Reporting - veg+hf veg+vhf3by7 // 2020-01-30"
TOL        = 0
UNROUND    = FALSE
SAVE       = NULL

od <- setwd("~/repos/recurring/veghf")
source("function.R")
setwd(od)
```

The script gave me the following output:

```
VEGHF processing started

Loading packages:

Loading objects ... OK

Loading functions ... OK

Connecting to SQLite database:
 s:/GC_eric/FromEric/IC_Reporting_Recovery/NWSAR/20200130_SC_veghf2017_NWSAR.sqlite

Found the following tables:
SC_NWSAR_project_Summary_Vegetation_HFI_rawdata

Loading table:
 SC_NWSAR_project_Summary_Vegetation_HFI_rawdata

Disconnecting ... OK

Processing long format summaries ... OK

Processing wide format summaries ... OK

Redistributing unknown ages:

current: Central Mixedwood --- OK
current: Lower Boreal Highlands --- OK
current: Northern Mixedwood --- OK
reference: Central Mixedwood --- OK
reference: Lower Boreal Highlands
	type: Decid | diff = 0 --- OK
reference: Northern Mixedwood --- OK


Saving results:
 s:/GC_eric/FromEric/IC_Reporting_Recovery/NWSAR/20200130_SC_veghf2017_NWSAR_2020-02-04.RData

DONE
```

If you run into any issues, read the error message, use `traceback()`, etc.
If you need to add new HF feature type to the lookup table,
please submit a PR so we all have the same lookup table.

Here is a script that goes over multiple tables from the same SQLite database:

``` R
od <- setwd("~/repos/recurring/veghf")

FILE       = "s:/GC_eric/FromEric/IC_Reporting_Recovery/AB_LUF_NR_OSR_Caribou_7Gen/20200131_SC_veghf2017_AB_LUF_NR_OSR_Caribou_7Gen.sqlite"
SUB_COL    = NULL
SUB_VAL    = ""
UID_COL    = "Region"
VEG_COL    = "Vegetation"
BASE_YR    = 2017
AREA_COL   = "Shape_Area"
AREA       = TRUE
OUTPUT     = NULL
TOL        = 0
UNROUND    = FALSE
SAVE       = NULL

TABLES <- c("SC_Caribou_Range_Vegetation_HFI2017_rawdata",
  "SC_LAND_USE_FRAMEWORK_Vegetation_HFI2017_rawdata",
  "SC_NaturalSubRegion_Vegetation_HFI2017_rawdata",
  "SC_Oilsand3Region_Vegetation_HFI2017_rawdata",
  "SC_Oilsand_Mineable_Vegetation_HFI2017_rawdata",
  "SC_SevenGeneration_Vegetation_HFI2017_rawdata")
for (i in TABLES) {

  TABLE      = i
  COMMENTS   = "AB_LUF_NR_OSR_Caribou_7Gen // 2020-01-31"
  OUTPUT     = paste0("s:/GC_eric/FromEric/IC_Reporting_Recovery/AB_LUF_NR_OSR_Caribou_7Gen/", TABLE, ".RData")
  
  source("function.R")
}
setwd(od)
```

### Non-interactive use

`cd` into the folder, then run the R script in a vanilla session, 
passing the settings file name as the only argument.
We use `settings.R` here but you can rename it to anything 
that makes sense for a project, this way storing multiple settings files:

``` bash
cd /veghf
Rscript --vanilla index.R settings.R
```

### Settings

* `UID_COL`: unique ID (can be all the same value)
* `VEG_COL`: vegetation column (usually `"Combined_ChgByCWCS"` but sometimes `"Vegetation"`)
* `AREA_COL`: field name for shape area when `AREA = TRUE` (usually `"Shape_Area"` but sometimes `"Area_m2"`)
* `BASE_YR`:
  base year for surveys or HF inventory
  this is used to calculate years since last disturbance
  (i.e. base year - origin year)
  use a numeric value when it is the same for each record
  use a character value to indicate a field when it
  varies by record
* `FILE`:
  input file name, can contain the path (i.e. /dir/file.csv)
  file type can be .csv or .sqlite
* `TABLE`:
  table name for SQLite database
  ignored for csv files
* `SUB_COL`:
  optional, field name to be used for subsetting
  can be `NULL` (ignored)
* `SUB_VAL`:
  values in the <SUB_COL> field to keep
  can be a single value or a character vector
* `OUTPUT`:
  optional, the name of the output file
  it can contain path as well (e.g. /dir/file.RData)
  if `NULL`, `<FILE>_YYYY-MM-DD.RData` is used, 
  if `NA`, no output file is produced (i.e. when accumulating 
  multiple objects in a single file).
* `AREA`:
  keep as TRUE when a Shape_Area field is present
  (long and wide format summaries can be calculated)
  set it to FALSE when e.g. doing point intersections
  (only long summary can be calculated)
* `COMMENTS`:
  add comments here, e.g. describing the characteristics
  of the input (backfilled v6.1 + 2017 HFI) when it is
  not trivial from file name, or describe purpose of
  the summaries as a reminder
* `TOL`:
  tolerance level for excluding unknown aged harvest areas:
  0 means no tolerance, 1 means it is OK to exlude all the landbase.
* `SAVE`:
  optional, object names to save as character vector or `NULL`
* `UNROUND`:
  wether to unround rounded origin year values pre-2000

### Advanced use

If you need more control, use the following steps instead of
`source("function.R")`:

``` R
source("00-setup.R")
source("01-data.R")
source("02-long.R")
source("03-wide.R")
source("04-save.R")
```

Feel free to inser additional processing steps between the steps.
For example, we want to modify the UID and save multiple subsets of the data
(e.g. by years):

``` R
source("00-setup.R")
source("01-data.R")

table(d$year_3by7)
d$UID <- paste0(d$Region, "_", d$year_3by7)
UID_COL    = "UID"

source("02-long.R")
source("03-wide.R")
source("04-save.R")
```


## Output

The output file is a binary RData file that can be loaded into using R as:

``` R
load("output-file-name.RData")
```

Once loaded, there are 3 objects:

* `d_long`: this is a data frame with same fields as the input with some new fields added, the most important ones are:
  - `"VEGAGEclass"`: reference labels based on backfilled veg (includes stand age)
  - `"VEGHFAGEclass"`: current veg + HF labels (includes stand age)
  - `"SOILclass"`: reference soil classes
  - `"SOILHFclass"`: current soil + HF classes
* `d_wide`: wide format summaries (can be `NULL` when `AREA = FALSE`), a list with 5 elements:
  - `"veg_current"`: UID x veg/HF labels sparse matrix, cell values are areas
  - `"veg_reference"`: UID x veg labels sparse matrix, cell values are areas
  - `"soil_current"`: UID x soil/HF labels sparse matrix, cell values are areas
  - `"soil_reference"`: UID x soil labels sparse matrix, cell values are areas
  - `"sample_year"`: sample (or base) year associated with each UID
* `.veghf_settings`: hidden object storing the user inputs, date, and session info


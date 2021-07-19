cat("\n\nVEGHF processing started\n")
## install required species if needed
if (!requireNamespace("mefa4"))
  install.packages("mefa4")
if (!requireNamespace("DBI"))
  install.packages("DBI")
if (!requireNamespace("RSQLite"))
  install.packages("RSQLite")

## load required packages
cat("\nLoading packages:\n")
library(mefa4)
library(DBI)
library(RSQLite)

cat("\nLoading objects ... ")
## reclass for natural veg
recl <- structure(list(V6_COMB = structure(1:30, .Label = c("Alkali",
  "AlpineLarch", "Bare", "Conif", "Decid", "Fir", "GraminoidFen",
  "GrassHerb", "Marsh", "Mixedwood", "Pine", "Shrub", "ShrubbyBog",
  "ShrubbyFen", "ShrubbySwamp", "SnowIce", "Spruce", "TreedBog-BSpr",
  "TreedFen-BSpr", "TreedFen-Decid", "TreedFen-Larch", "TreedFen-Mixedwood",
  "TreedSwamp-Conif", "TreedSwamp-Decid", "TreedSwamp-Fir", "TreedSwamp-Forest",
  "TreedSwamp-Mixedwood", "TreedSwamp-Spruce", "TreedWetland-Mixedwood",
  "Water"), class = "factor"), MERGED = structure(c(4L, 13L, 1L,
  13L, 2L, 13L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
  14L, 15L, 15L, 15L, 15L, 16L, 16L, 16L, 16L, 16L, 16L, 15L, 17L
  ), .Label = c("Bare", "Decid", "GraminoidFen", "GrassHerb", "Marsh",
  "Mixedwood", "Pine", "Shrub", "ShrubbyBog", "ShrubbyFen", "ShrubbySwamp",
  "SnowIce", "Spruce", "TreedBog", "TreedFen", "TreedSwamp", "Water"
  ), class = "factor")), class = "data.frame", row.names = c(NA,
  -30L))

## HF types
hftypes <- read.csv("lookup-hf-type-v2014.csv")
hftypes <- droplevels(hftypes[!is.na(hftypes$HF_GROUP_COMB) &
  !duplicated(hftypes$FEATURE_TY),])
hfgroups <- read.csv("lookup-hf-class-v2014.csv")
hflt <- hfgroups[match(hftypes$HF_GROUP_COMB, hfgroups$HF_GROUP_COMB),]
rownames(hflt) <- hftypes$FEATURE_TY

## age distribution by NR, NSR
load("ages-by-nsr.RData")

## functions
cat("OK\n\nLoading functions ... ")

## SQL tables represent character not factor
make_char2fact <- function(x) {
    if (is.null(dim(x)))
        if (is.character(x))
            return(as.factor(x))
    for (i in seq_len(ncol(x)))
        if (is.character(x[,i]))
            x[,i] <- as.factor(x[,i])
        x
}

## function to address origin year rounding in a reproducible manner
age_unround <- function(y) {
    j <- c(-4L, 3L, 4L, 2L, -3L, 5L, 0L, -1L, 1L, -2L)
    s <- y %% 10 == 0
    yy <- y[s]
    i <- as.integer(round(10*(yy/100 - floor(yy/100))))
    i[i == 0L] <- 10L
    y[s] <- y[s] + j[i]
    y
}
#y <- 191:200*10
#matrix(age_unround(y)-y, 10, 10)
#data.frame(orig=y, unrounded=age_unround(y))

## this function processes the input data
make_vegHF_wide_v6 <-
function(d, col.label, col.year=NULL, col.HFyear=NULL,
col.HABIT=NULL, col.SOIL=NULL, wide=TRUE, sparse=FALSE,
HF_fine=TRUE, widen_only=FALSE, tol=0, unround=FALSE) {

    TreedClassesCC <- c("Decid", "Mixedwood", "Pine", "Spruce")
    TreedClasses   <- c(TreedClassesCC, "TreedBog", "TreedFen", "TreedSwamp")
    NontreedClasses <- c("GrassHerb", "Shrub",
        "GraminoidFen", "Marsh",
        "ShrubbyBog", "ShrubbyFen", "ShrubbySwamp",
        "Bare", "SnowIce", "Water")
    if (!HF_fine) {
        HFLab <- c("BorrowpitsDugoutsSumps", "Canals", "CultivationCropPastureBareground",
            "CutBlocks", "HighDensityLivestockOperation", "IndustrialSiteRural",
            "MineSite", "MunicipalWaterSewage", "OtherDisturbedVegetation",
            "PeatMine", "Pipeline", "RailHardSurface", "RailVegetatedVerge",
            "Reservoirs", "RoadHardSurface", "RoadTrailVegetated", "RoadVegetatedVerge",
            "RuralResidentialIndustrial", "SeismicLine", "TransmissionLine",
            "Urban", "WellSite", "WindGenerationFacility")
    } else {
        HFLab <- c("UrbanIndustrial", "UrbanResidence", "RuralResidentialIndustrial",
            "IndustrialSiteRural", "WindGenerationFacility", "OtherDisturbedVegetation",
            "MineSite", "PeatMine", "WellSite", "Pipeline", "TransmissionLine",
            "SeismicLineNarrow", "SeismicLineWide", "RoadHardSurface", "RailHardSurface",
            "RoadTrailVegetated", "RoadVegetatedVerge", "RailVegetatedVerge",
            "CultivationCrop", "CultivationAbandoned", "CultivationRoughPasture",
            "CultivationTamePasture", "HighDensityLivestockOperation",
            "CutBlocks", "BorrowpitsDugoutsSumps", "MunicipalWaterSewage",
            "Reservoirs", "Canals")
    }
    RfLab <- c(paste0(rep(TreedClasses, each=11),
        c("0","R","1","2","3","4","5","6","7","8","9")),
        NontreedClasses)
    ## use necessary CC-forest classes once evaluated
    CCOnlyLab <- paste0("CC", paste0(rep(TreedClassesCC, each=5),
        c("R","1","2","3","4")))
    CrOnlyLab <- c(HFLab, CCOnlyLab)
    HLEVS <- c(TreedClasses, NontreedClasses)
    AllLabels <- c(RfLab, CrOnlyLab)

    SoilLab <- c("UNK", "Water", #"Wetland",
        "BdL", "BlO", "CS", "Cy", "Gr", "LenA", "LenSP",
        "LenT", "LenS", "Li", "Lo", "LtcC", "LtcD", "LtcH", "LtcS", "Ov",
        "Sa", "Sb", "SL", "SwG", "Sy", "TB")

    ## designate a label column (there are different column names in use)
    d$LABEL <- d[,col.label]

    if (!widen_only) {

        if (is.null(d[,col.HABIT]))
            stop("Shoot -- check the damn HABIT column...")
        d <- droplevels(d[!is.na(d[,col.HABIT]) & d[,col.HABIT] != "",])
        d$HABIT <- reclass(d[,col.HABIT], as.matrix(recl), all=TRUE)

        d$HF_Year <- d[,col.HFyear]
        if (any(is.na(d$LABEL)))
            stop("missing LABEL")
        #    d <- d[!is.na(d$LABEL),]
        ## designate a year column
        if (is.null(col.year)) {
            THIS_YEAR <- as.POSIXlt(Sys.Date())$year + 1900
            d$SampleYear <- THIS_YEAR
        } else {
            if (is.numeric(col.year)) {
                if (length(col.year) > 1)
                    stop("length of col.year > 1")
                THIS_YEAR <- col.year
                d$SampleYear <- THIS_YEAR
            } else {
                THIS_YEAR <- NA
                d$SampleYear <- d[,col.year]
            }
        }
        ## use upper-case labels for FEATURE_TY
        levels(d$FEATURE_TY) <- toupper(levels(d$FEATURE_TY))

        d$ORIGIN_YEAR <- d$Origin_Year
        if (unround)
          d$ORIGIN_YEAR[d$ORIGIN_YEAR < 2000] <- age_unround(d$ORIGIN_YEAR[d$ORIGIN_YEAR < 2000])

        #### Footprint classes:
        ## check if we have all the feature types in the lookup table
        ## "" blank is for non-HF classes in current veg
        levels(d$FEATURE_TY)[levels(d$FEATURE_TY) == "''"] <- ""
        levels(d$FEATURE_TY)[levels(d$FEATURE_TY) == " "] <- ""
        if (!all(setdiff(levels(d$FEATURE_TY), rownames(hflt)) == "")) {
            print(setdiff(levels(d$FEATURE_TY), c("", rownames(hflt))))
            stop("HF diff found, see above")
        }
        ## classify feature types according to the chosen level of HF designation
        ## which comes from hf.level column of hflt (HF lookup table)
        if (HF_fine) {
            d$HFclass <- hflt$HF_GROUP_COMB[match(d$FEATURE_TY, rownames(hflt))]
        } else {
            d$HFclass <- hflt$HF_GROUP[match(d$FEATURE_TY, rownames(hflt))]
        }
        d$HFclass <- as.factor(d$HFclass)
        ## HFclass inherits all levels from hflt[,hf.level]
        ## need to add in the blank for further parsing
        levels(d$HFclass) <- c(levels(d$HFclass), "")
        d$HFclass[is.na(d$HFclass)] <- ""

        ## slivers (tiny polys with no veg info):
        #stopifnot(max(d$Shape_Area[d$VEGclass == ""]) < 1)
        if (any(d$HABIT == ""))
            warning(paste("blank HABIT:", sum(d$Shape_Area[d$HABIT == ""]), "m^2"))
        #d <- d[d$HABIT != "",]
        d$HABIT <- droplevels(d$HABIT)

        #### HABIT/EC classes:
        ## follow HABIT/EC classes, but there are few oddities when outside of AVI
        d$VEGclass <- d$HABIT
        if (length(setdiff(d$VEGclass, HLEVS)) > 0)
            stop(paste("check HABIT classes", setdiff(d$VEGclass, HLEVS)))

        #### Age info for backfilled (Rf) and current (Cr)
        ## reference age class 0=no age (either not forest or no info)
        ## 1=0-19, 2=20-39, etc.
        d$ORIGIN_YEAR[!is.na(d$ORIGIN_YEAR) & d$ORIGIN_YEAR == 9999] <- NA
        d$ORIGIN_YEAR[!is.na(d$ORIGIN_YEAR) & d$ORIGIN_YEAR > d$SampleYear] <- NA
        d$AgeRf <- as.integer(sign(d$ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$ORIGIN_YEAR) / 20)))
        ## truncate reference age classes at 9 = 160+
        d$AgeRf[d$AgeRf > 9L] <- 9L
        ## catching origin_year > sample_year instances: this defaults to old
        d$AgeRf[d$AgeRf < 1] <- 9L
        ## placeholder for recent burn (0-9 years)
        tmp <- as.integer(sign(d$ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$ORIGIN_YEAR) / 10)))
        d$AgeRf[tmp == 1L] <- 999L

        ## incorporate HF year for cutblocks
        d$CC_ORIGIN_YEAR <- d$ORIGIN_YEAR
        ii <- d$HFclass == "CutBlocks"
        ii[ii & !is.na(d$ORIGIN_YEAR) & d$HF_Year >= d$ORIGIN_YEAR] <- TRUE
        ii[ii & is.na(d$ORIGIN_YEAR)] <- TRUE
        d$CC_ORIGIN_YEAR[ii] <- d$HF_Year[ii]
        d$CC_ORIGIN_YEAR[!is.na(d$CC_ORIGIN_YEAR) & d$CC_ORIGIN_YEAR == 9999] <- NA
        ## age for current with cutblock ages
        d$AgeCr <- as.integer(sign(d$CC_ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$CC_ORIGIN_YEAR) / 20)))
        ## truncate current age classes at 9
        d$AgeCr[d$AgeCr > 9L] <- 9L
        ## catching origin_year > sample_year instances: this defaults to old
        d$AgeCr[d$AgeCr < 1] <- 9L
        ## placeholder for recent CC (0-9 years)
        tmp <- as.integer(sign(d$CC_ORIGIN_YEAR) * (1 + floor((d$SampleYear - d$CC_ORIGIN_YEAR) / 10)))
        d$AgeCr[tmp == 1L] <- 999L
        ## unknown age is set to 0
        #table(d$AgeCr, d$VEGclass, useNA="a") # check NA ORIGIN_YEAR values
        #d$AgeCr[is.na(d$AgeCr)] <- 0L
        #table(d$AgeCr,useNA="a")

        d$AgeRf[is.na(d$AgeRf)] <- 0L
        #table(rf=d$AgeRf,cr=d$AgeCr,useNA="a")
        ## turning age values into factor:
        ## 0=no age info,
        ## 1:9=valid age classes for treed veg types,
        ## ""=non-treed
        ## 999=placeholder for _R_ecent burn "R"
        d$AgeRf <- factor(d$AgeRf, levels=c(as.character(c(0:9, 999)), ""))
        ## NA --> "0" as unknown age class
        d$AgeRf[is.na(d$AgeRf)] <- "0"
        ## age is not relevant in non-treed veg types
        d$AgeRf[!(d$VEGclass %in% TreedClasses)] <- ""
        ## burn
        levels(d$AgeRf)[levels(d$AgeRf)=="999"] <- "R"

        ## making current age as factor
        d$AgeCr <- factor(d$AgeCr, levels=c(as.character(c(0:9, 999)), ""))
        ## NA --> "0" as unknown age class
        d$AgeCr[is.na(d$AgeCr)] <- "0"
        ## age is not relevant in non-treed veg types (no HF)
        d$AgeCr[d$VEGclass %in% NontreedClasses & d$HFclass == ""] <- ""
        ## age is not relevant outside of cutblocks
        d$AgeCr[!(d$HFclass %in% c("", "CutBlocks"))] <- ""
        ## recent CC
        levels(d$AgeCr)[levels(d$AgeCr)=="999"] <- "R"
        #table(current=d$AgeCr, reference=d$AgeRf)

        #### Combining VEG, HF and Age:
        ## reference VEG + Age labels:
        d$VEGAGEclass <- interaction(d$VEGclass, d$AgeRf, drop=TRUE, sep="", lex.order=TRUE)
        levels(d$VEGAGEclass) <- c(levels(d$VEGAGEclass),
            setdiff(RfLab, levels(d$VEGAGEclass)))

        ## manage CC labels
        ## current veg+hf
        d$VEGHFclass <- d$VEGclass
        #CClabels <- paste0("CC", levels(d$VEGclass)[levels(d$VEGclass) != ""])
        CClabels <- paste0("CC", levels(d$VEGclass))
        tmp <- setdiff(levels(d$HFclass), levels(d$VEGclass))
        tmp <- tmp[!(tmp %in% c("", "CutBlocks"))]
        levels(d$VEGHFclass) <- c(levels(d$VEGHFclass), tmp, CClabels)
        ## add non-CC HF types
        d$VEGHFclass[!(d$HFclass %in% c("", "CutBlocks"))] <- d$HFclass[!(d$HFclass %in% c("", "CutBlocks"))]
        ## should later the non-merchendisable forests with CC should be redistributed?
        ## e.g. after producing the wide format
        ## update CC labels obly for <= 80 yr CC (usually this does not happen
        ## just to make sure labels are OK)
        ## anything above age class >4 is turned into 4 to avoid labeling issues (shrubland)
        d$AgeCr[d$HFclass == "CutBlocks" & d$AgeCr %in% c("5","6","7","8","9")] <- "4"
        ii <- d$HFclass == "CutBlocks" & d$AgeCr %in% c("0","R","1","2","3","4")
        if (sum(ii) > 0)
            d$VEGHFclass[ii] <- paste0("CC", as.character(d$VEGclass[ii]))

        ## current VEG + HF + Age labels:
        d$VEGHFAGEclass <- interaction(d$VEGHFclass, d$AgeCr, drop=TRUE, sep="", lex.order=TRUE)
        ## Labels for output columns
        levels(d$VEGHFAGEclass) <- c(levels(d$VEGHFAGEclass), setdiff(AllLabels, levels(d$VEGHFAGEclass)))

        #### soils:
        if (is.null(d[,col.SOIL]))
            stop("Shoot -- check the damn SOIL column...")
        d$SOILclass <- d[,col.SOIL]
        ## need to have the UNKnown class to be able to deal with NAs
        if (!is.factor(d$SOILclass))
            d$SOILclass <- as.factor(d$SOILclass)
        if (!any(levels(d$SOILclass) == ""))
            levels(d$SOILclass) <- c(levels(d$SOILclass), "")
        ## dealing with NAs
        d$SOILclass[is.na(d$SOILclass)] <- ""
        ## unknown soil type outside of GVI and Dry Mixedwood
        levels(d$SOILclass)[levels(d$SOILclass) == ""] <- "UNK"
        levels(d$SOILclass)[levels(d$SOILclass) == " "] <- "UNK"
        ## get rid of modifiers
        levels(d$SOILclass) <- sapply(strsplit(levels(d$SOILclass), "-"), function(z) z[1L])
        ## add in Water label
        levels(d$SOILclass) <- c(levels(d$SOILclass), "Water")

        ## treat these as Water or Wetland?
        levels(d$SOILclass)[levels(d$SOILclass) %in% c("Len","LenW","Ltc","LtcR")] <- "Water"
    #    levels(d$SOILclass)[levels(d$SOILclass) %in% c("Len","LenW","Ltc","LtcR")] <- "Wetland"
        ## DEM/EC based Water class overrides soil
        d$SOILclass[d$VEGclass == "Water"] <- "Water"

        levels(d$SOILclass) <- c(levels(d$SOILclass), setdiff(SoilLab, levels(d$SOILclass)))
        d$SOILHFclass <- d$SOILclass
        levels(d$SOILHFclass) <- c(levels(d$SOILHFclass), levels(d$HFclass)[levels(d$HFclass) != ""])
        d$SOILHFclass[d$HFclass != ""] <- d$HFclass[d$HFclass != ""]
        ## NOTE: current UNK can be smaller than reference UNK, it can be turned into HF
        ## currently this is not tracked

        ## for point intersection or transition matrix processing, etc.
        if (!wide)
            return(d)
    }

    SoilHFLab <- levels(d$SOILHFclass)
    #### crosstabs
    ## veg reference
    VegRf <- Xtab(Shape_Area ~ LABEL + VEGAGEclass, d)
    ## veg + HF current
    VegCr <- Xtab(Shape_Area ~ LABEL + VEGHFAGEclass, d)
    ## soils (`reference`)
    SoilRf <- Xtab(Shape_Area ~ LABEL + SOILclass, d)
    ## soils (`current`, soil + HF)
    SoilCr <- Xtab(Shape_Area ~ LABEL + SOILHFclass, d)

    ## removing unknown aged forest harvest within tolerance level
    if (tol > 0) {
      #CC0 <- sum(VegCr[,startsWith(colnames(VegCr), "CC") & endsWith(colnames(VegCr), "0")])
      ISSUECOL <- startsWith(colnames(VegCr), "CC") & !(colnames(VegCr) %in% CCOnlyLab)
      CC0 <- sum(VegCr[,ISSUECOL])
      CC0 <- CC0 / sum(VegCr)
      if (CC0 > 0)
        cat("Unknown age or non merchentable CC found: ", round(100*CC0,4), "%\n", sep="")
      if (CC0 <= tol)
        VegCr <- VegCr[,!ISSUECOL]
    }

    if (!all(colnames(VegRf) %in% RfLab)) {
        cat(colnames(VegRf)[!(colnames(VegRf) %in% RfLab)], sep="\n")
        stop("Unexpected VegRf label")
    }
    if (!all(colnames(VegCr) %in% AllLabels)) {
        cat(colnames(VegCr)[!(colnames(VegCr) %in% AllLabels)], sep="\n")
        stop("Unexpected VegCr label")
    }
    if (!all(colnames(SoilRf) %in% SoilLab)) {
        cat(colnames(SoilRf)[!(colnames(SoilRf) %in% SoilLab)], sep="\n")
        stop("Unexpected SoilRf label")
    }
    if (!all(colnames(SoilCr) %in% SoilHFLab)) {
        cat(colnames(SoilCr)[!(colnames(SoilCr) %in% SoilHFLab)], sep="\n")
        stop("Unexpected SoilCr label")
    }

    rn <- rownames(VegRf) # make sure row labels are identical across tables
    VegRf <- VegRf[rn, RfLab, drop=FALSE]
    VegCr <- VegCr[rn, AllLabels, drop=FALSE]
    SoilRf <- SoilRf[rn, SoilLab, drop=FALSE]
    SoilCr <- SoilCr[rn, SoilHFLab, drop=FALSE]

    out <- list(veg_current=VegCr,
        veg_reference=VegRf,
        soil_current=SoilCr,
        soil_reference=SoilRf)
    if (!sparse)
        out <- lapply(out, as.matrix)

    ## year for each row
    tmp <- nonDuplicated(d, LABEL, TRUE)
    tmp <- tmp[rownames(VegCr),]
    #out$sample_year <- THIS_YEAR
    out$sample_year <- tmp$SampleYear

    out
}

## this function redistributes unknown ages in wide form summaries
fill_in_0ages_v6 <- function(x, NSR, ages_list, rm0=TRUE) {
  Target <- names(ages_list)
  Ages <- c("0", "R", as.character(1:9))
  NSR <- droplevels(as.factor(NSR))
  NSRs <- levels(NSR)
  for (current in c(TRUE, FALSE)) {
    xx <- if (current)
      x$veg_current else x$veg_reference
    xx <- as.matrix(xx)
    ag <- if (current)
      AvgAgesNSROld$current else AvgAgesNSROld$reference
    ag2 <- if (current)
      AvgAgesAllOld$current else AvgAgesAllOld$reference
    for (nsr in NSRs) {
      cat(ifelse(current, "current:", "reference:"), nsr)
      flush.console()
      for (i in Target) {
        Cols <- paste0(i, Ages)
        j <- NSR == nsr
        if (any(j)) {
          p0 <- ag[[i]][nsr,]
          if (sum(p0) == 0)
            p0 <- ag2[[i]]
          Mat <- xx[j, Cols, drop=FALSE]
          Mat0 <- Mat
          ## multiply Mat[,1] (unknown age) with this matrix
          Unk <- Mat[,1] * t(matrix(p0, length(Ages), sum(j)))
          Mat[,1] <- 0 # will be 0 and redistributed from Unk
          Mat <- Mat + Unk
          xx[j, Cols] <- Mat # ridiculously slow as sparse matrix
          if (sum(Mat0)-sum(Mat) > 10^-6)
            cat("\n\ttype:", i, "| diff =", round((sum(Mat0)-sum(Mat))/10^6))
        }
      }
      cat(" ... OK\n")
    }
    if (current) {
      x$veg_current <- as(xx, "dgCMatrix")
    } else {
      x$veg_reference <- as(xx, "dgCMatrix")
    }
  }
  if (rm0) {
    excl <- c("Decid0", "Mixedwood0", "Pine0", "Spruce0", "TreedBog0", "TreedFen0",
      "TreedSwamp0", "CutBlocks")
    x$veg_current <- x$veg_current[,!(colnames(x$veg_current) %in% excl)]
    x$veg_reference <- x$veg_reference[,!(colnames(x$veg_reference) %in% excl)]
  }
  x
}

cat("OK\n\n")

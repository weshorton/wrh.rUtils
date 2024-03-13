#!/usr/bin/Rscript
library(optparse)
library(wrh.rUtils)
library(data.table)

optlist <- list(
  make_option(
    c("-d", "--dir"),
    type = "character",
    default = NULL,
    help = "Optional path to directory that wkbk1 and wkbk2 are in."
  ),
  make_option(
    c("--wkbk1"),
    type = "character",
    help = "If dir_v == NULL, path to first workbook. If dir_v is set, then file name within dir_v that points to first workbook."
  ),
  make_option(
    c("--wkbk2"),
    type = "character",
    help = "If dir_v == NULL, path to second workbook. If dir_v is set, then file name within dir_v that points to second workbook."
  ),
  make_option(
    c("-o", "--outDir"),
    type = "character",
    default = NULL,
    help = "optional directory to write output if any is created. Default, NULL, will write to working directory."
  )
)

### Parse command line
p <- OptionParser(usage = "%proj -d dir --wkbk1 --wkbk2 -o outDir",
                  option_list = optlist)
args <- parse_args(p)
opt <- args$options

### Assign to variables
dir_v <- args$dir
wkbk1_v <- args$wkbk1
wkbk2_v <- args$wkbk2
outDir_v <- args$outDir

### Run function
wrh.rUtils::checkExcel(dir_v = dir_v, wkbk1_v = wkbk1_v, wkbk2_v = wkbk2_v, outDir_v = outDir_v)

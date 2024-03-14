## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
biospheremetrics::list_outputs(metric = "ecorisk_nitrogen")

## -----------------------------------------------------------------------------
biospheremetrics::list_outputs(metric = "ecorisk_nitrogen")

## -----------------------------------------------------------------------------
run_folder <- paste0(system.file("extdata","run","lu_1500_2016",package = "biospheremetrics"),"/")
pnv_folder <- paste0(system.file("extdata","run","pnv_1500_2016",package = "biospheremetrics"),"/")
out_folder <- paste0(tempdir(),"/")


rm(list = ls())
library(devtools)
library(lpjmlkit)
devtools::load_all("/home/stenzel/biospheremetrics/")
devtools::install("/home/stenzel/biospheremetrics/")
library(biospheremetrics)

run_folder <- paste0(system.file("extdata","run","lu_1500_2016",package = "biospheremetrics"),"/")
pnv_folder <- paste0(system.file("extdata","run","pnv_1500_2016",package = "biospheremetrics"),"/")
out_folder <- paste0(tempdir(),"/")

grid <- lpjmlkit::read_io(
  paste0(run_folder,"grid.bin.json")
)

biocol <- biospheremetrics::calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1500,
  stop_year = 2016,
  reference_npp_time_span = 1510:1539,
  read_saved_data = FALSE,
  save_data = FALSE
)

biospheremetrics::plot_biocol_ts(
  biocol_data = biocol,
  plot_years = c(1510,2016),
  first_year = 1510,
  min_val = 0,
  max_val = 0.005,
  legendpos = "left",
  highlight_years = NA,
  eps = FALSE
)

ecorisk <- ecorisk_wrapper(
  path_ref = pnv_folder,
  path_scen = run_folder,
  read_saved_data = FALSE,
  nitrogen = TRUE,
  save_data = NULL,
  save_ecorisk = NULL,
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1987:2016)
  )
ecorisk$ecorisk_total

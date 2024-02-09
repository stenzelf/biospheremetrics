rm(list = ls())
library(devtools)
library(lpjmlkit)
library(magrittr)

devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_review_paper/")

run_folder <- paste0(system.file("extdata","run","lu_1500_2016",package = "biospheremetrics"),"/")
pnv_folder <- paste0(system.file("extdata","run","pnv_1500_2016",package = "biospheremetrics"),"/")
out_folder <- paste0(tempdir(),"/")

# the output variable names and their composition for the computation of biocol 
# and ecorisk are defined in inst/extdata/metric_files.yml

################# calculate BioCol ################
# 16GB of RAM are enough to calculate BioCol for a smaller analysis window (~40 years)
# for longer spans (500 years) - use separate script ("read_in_BioCol_data.R") 
# and submit as cluster job using "sbatch R_read_in_BioCol_data.sh" - analysis for "biocol overtime" below
# read grid
grid <- lpjmlkit::read_io(paste0(run_folder, "grid.bin.json"))$data
# calculate cell area
lat <- grid[, , 2]
lon <- grid[, , 1]
cellarea <- drop(lpjmlkit::read_io(filename = paste0(run_folder,"terr_area.bin.json"))$data) # in m2

paletteNew = c("white",RColorBrewer::brewer.pal(9,"YlOrRd"))


################# calculate BioCol ################
# 16GB of RAM are enough to calculate BioCol for a smaller analysis window (~40 years)
# for longer spans (500 years) - use separate script ("read_in_BioCol_data.R") 
# and submit as cluster job using "sbatch R_read_in_BioCol_data.sh"

biocol <- biospheremetrics::calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1500,
  stop_year = 2016,
  reference_npp_time_span = 1510:1539,
  reference_npp_file = paste0(pnv_folder,"npp.bin.json"),
  read_saved_data = FALSE,
  save_data = TRUE,
  npp_threshold = 1,
  data_file = paste0(out_folder, "BioCol_test_202401_1980-2016.RData"),
  external_fire = FALSE,
  external_wood_harvest = FALSE,
  external_fire_file = "",
  external_wood_harvest_file = "",
  varnames = vars_metrics,
  grass_scaling = FALSE,
  include_fire = FALSE
)

biospheremetrics::plot_biocol_ts(
  biocol_data = biocol,
  file = paste0(out_folder,"plots/BioCol_overtime.png"),
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
  varnames = vars_metrics,
  weighting = "equal",
  save_data = paste0(out_folder, "ecorisk_test_202401_data.RData"),
  save_ecorisk = paste0(out_folder, "ecorisk_test_202401_gamma.RData"),
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1987:2016),
  dimensions_only_local = FALSE
)

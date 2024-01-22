rm(list = ls())
library(devtools)
library(lpjmlkit)
library(magrittr)

devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_review_paper/")

run_folder <- paste0(system.file("extdata","run","lu_1500_2016",package = "biospheremetrics"),"/")
pnv_folder <- paste0(system.file("extdata","run","pnv_1500_2016",package = "biospheremetrics"),"/")
out_folder <- tempdir()

vars_metrics <- data.frame(
  row.names = c("grid","fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff",
                "transp", "vegc", "firef", "rh", "harvestc", "rharvestc",
                "pft_harvestc", "pft_rharvestc", "evap", "interc", "discharge",
                "soilc", "litc", "swc", "swc_vol", "vegn", "soilnh4", "soilno3",
                "leaching", "n2o_denit", "n2o_nit", "n2_emis", "bnf",
                "n_volatilization", "gpp", "res_storage", "lakevol", "ndepos",
                "rd", "prec", "terr_area", "irrig", "nfert_agr", "nmanure_agr", 
                "firen", "harvestn", "rivervol", "irrig_stor","pft_npp",
                "timber_harvest", "rootmoist"),
  outname = c("grid.bin.json", "fpc.bin.json", "fpc_bft.bin.json",
              "cftfrac.bin.json", "firec.bin.json", "npp.bin.json",
              "runoff.bin.json", "transp.bin.json", "vegc.bin.json",
              "firef.bin.json", "rh.bin.json", "harvestc.bin.json",
              "rharvestc.bin.json", "pft_harvest.pft.bin.json",
              "pft_rharvest.pft.bin.json", "evap.bin.json",
              "interc.bin.json", "discharge.bin.json", "soilc.bin.json",
              "litc.bin.json", "swc.bin.json", "swc_vol.bin.json", "vegn.bin.json",
              "soilnh4.bin.json", "soilno3.bin.json", "leaching.bin.json",
              "n2o_denit.bin.json", "n2o_nit.bin.json", "n2_emis.bin.json",
              "bnf.bin.json", "n_volatilization.bin.json", "gpp.bin.json",
              "res_storage.bin.json", "lakevol.bin.json", "ndepos.bin.json",
              "rd.bin.json", "prec.bin.json", "terr_area.bin.json",
              "irrig.bin.json", "nfert_agr.bin.json", "nmanure_agr.bin.json",
              "firen.bin.json", "harvestn.bin.json", "rivervol.bin.json",
              "irrig_stor.bin.json", "pft_npp.bin.json",
              "timber_harvestc.bin.json","rootmoist.bin.json")
)

# read grid
grid <- read_io(paste0(run_folder, "grid.bin.json"))
# calculate cell area
lat <- grid$data[, , 2]
lon <- grid$data[, , 1]
cellarea <- calc_cellarea(grid)

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

biocol <- calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1980,
  stop_year = 2016,
  reference_npp_time_span = 1510:1539,
  reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202308/output/pnv_1500_2016/npp.bin.json",
  read_saved_data = FALSE,
  save_data = FALSE,
  npp_threshold = 1,
  data_file = "/p/projects/open/Fabian/Metrics/data/BioCol_202308_1980-2016.RData",
  external_fire = FALSE,
  external_wood_harvest = TRUE,
  external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
  external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2016_extended_67420.RData",
  varnames = vars_metrics,
  grass_scaling = FALSE,
  include_fire = FALSE
)

plot_biocol(
  biocol_data = biocol,
  path_write = paste0(out_folder,"BioCol_test/"),
  plotyears = c(1980,2014),
  min_val = 0,
  max_val = 90,
  legendpos = "left",
  start_year = 1980,
  mapyear = 2000,
  highlightyear = 2000,
  eps = FALSE
)



ecorisk <- ecorisk_wrapper(
  path_ref = pnv_folder,
  path_scen = run_folder,
  read_saved_data = FALSE,
  nitrogen = TRUE,
  varnames = vars_ecorisk,
  weighting = "equal",
  save_data = "/p/projects/open/Jannes/tests/metrics/ecorisk_202306_data.RData",
  save_ecorisk = "/p/projects/open/Jannes/tests/metrics/ecorisk_202306_gamma.RData",
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1985:2014),
  dimensions_only_local = FALSE
)

# plot ecorisk
plot_ecorisk_map(
  ecorisk$ecorisk_total,
  file = paste0(out_folder, "EcoRisk/ecorisk.png"),
  title = "ecorisk"
)

plot_ecorisk_map(
  ecorisk$vegetation_structure_change,
  file = paste0(out_folder, "EcoRisk/vs.png"),
  title = "vegetation structure change"
)

plot_ecorisk_map(
  ecorisk$local_change,
  file = paste0(out_folder, "EcoRisk/lc.png"),
  title = "local change"
)

plot_ecorisk_map(
  ecorisk$global_importance,
  file = paste0(out_folder, "EcoRisk/gi.png"),
  title = "global importance"
)

plot_ecorisk_map(
  ecorisk$ecosystem_balance,
  file = paste0(out_folder, "EcoRisk/eb.png"),
  title = "ecosystem balance")

plot_ecorisk_map(
  ecorisk$carbon_stocks,
  file = paste0(out_folder, "EcoRisk/cs.png"),
  title = "carbon_stocks"
)

plot_ecorisk_map(
  ecorisk$carbon_fluxes,
  file = paste0(out_folder, "EcoRisk/cf.png"),
  title = "carbon_fluxes"
)

plot_ecorisk_map(
  ecorisk$water_stocks,
  file = paste0(out_folder, "EcoRisk/ws.png"),
  title = " water_stocks"
)

plot_ecorisk_map(
  ecorisk$water_fluxes,
  file = paste0(out_folder, "EcoRisk/wf.png"),
  title = " water_fluxes"
)

plot_ecorisk_map(
  ecorisk$nitrogen_stocks,
  file = paste0(out_folder, "EcoRisk/ns.png"),
  title = " nitrogen_stocks"
)

plot_ecorisk_map(
  ecorisk$nitrogen_fluxes,
  file = paste0(out_folder, "EcoRisk/nf.png"),
  title = " nitrogen_fluxes"
)

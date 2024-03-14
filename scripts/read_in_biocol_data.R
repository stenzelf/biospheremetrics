library(devtools)
library(magrittr)
library(biospheremetrics)
library(lpjmlkit)

run_folder <- "./output/lu_1500_2014/"
pnv_folder <- "./output/pnv_1500_2014/"
out_folder <- "./Metrics/"
lpj_input <- "./historical/"

# read grid
grid <- read_io(paste0(run_folder, "grid.bin.json"))$data %>% drop()
# calculate cell area
lat <- grid[, 2]
lon <- grid[, 1]

################# mcol ################

vars_biocol <- data.frame(
  row.names = c(
    "grid", "npp", "pft_npp", "pft_harvest", "pft_rharvest",
    "firec", "timber_harvest", "cftfrac", "fpc"
  ),
  outname = c(
    "grid.bin.json", "mnpp.bin.json", "pft_npp.bin.json",
    "pft_harvest.pft.bin.json", "pft_rharvest.pft.bin.json",
    "firec.bin.json", "timber_harvestc.bin.json",
    "cftfrac.bin.json", "fpc.bin.json"
  ),
  timestep = c("Y", "M", "Y", "Y", "Y", "Y", "Y", "Y", "Y")
)

biocol <- calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1500,
  stop_year = 2014,
  reference_npp_time_span = 1550:1579,
  reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
  read_saved_data = FALSE,
  save_data = TRUE,
  npp_threshold = 20,
  data_file = "/p/projects/open/Fabian/Metrics/data/BioCol_202306.RData",
  external_fire = FALSE,
  external_wood_harvest = TRUE,
  external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
  external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
  varnames = vars_biocol,
  grass_scaling = FALSE,
  include_fire = FALSE
)

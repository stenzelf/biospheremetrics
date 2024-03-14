library(devtools)
library(magrittr)
devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_paper/")

runFolder <- "/p/projects/open/Fabian/runs/metrics_202306/output/lu_1500_2014/"
pnvFolder <- "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/"
outFolder <- "/p/projects/open/Fabian/Metrics/"
lpjInput <- "/p/projects/lpjml/input/historical/"

# read grid
grid <- lpjmlkit::read_io(paste0(runFolder,"grid.bin.json"))$data %>% drop()
# calculate cell area
lat <- grid[,2]
lon <- grid[,1]

################# mcol ################

vars_biocol <- data.frame(row.names = c("grid",         "npp",     "pft_npp",    "pft_harvest",        "pft_rharvest",        "firec",    "timber_harvest",     "cftfrac",    "fpc"),
                          outname = c("grid.bin.json",  "mnpp.bin.json","pft_npp.bin.json","pft_harvest.pft.bin.json","pft_rharvest.pft.bin.json","firec.bin.json","timber_harvestc.bin.json","cftfrac.bin.json","fpc.bin.json"),
                          timestep = c("Y",             "M",       "Y",          "Y",                  "Y",                   "Y",        "Y",                  "Y",          "Y"))

biocol <- calcBioCol(inFol_lu = runFolder,inFol_pnv = pnvFolder, gridbased = T,
                     startyr = 1500, stopyr = 2014, reference_npp_time_span = 1550:1579, 
                     reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
                     readPreviouslySavedData = F, saveDataFile = T,npp_threshold = 20,
                     dataFile = "/p/projects/open/Fabian/Metrics/data/BioCol_202306.RData",
                     external_fire = F, external_wood_harvest = T,
                     external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
                     external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
                     varnames = vars_biocol, grass_scaling = F, include_fire = F)

library(devtools)
library(lpjmlkit)
library(magrittr)

devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_paper/")

runFolder <- "/p/projects/open/Fabian/runs/metrics_202306/output/lu_1500_2014/"
pnvFolder <- "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/"
outFolder <- "/p/projects/open/Fabian/Metrics/"
lpjInput <- "/p/projects/lpjml/input/historical/"

# read grid
grid <- lpjmlkit::read_io(paste0(runFolder, "grid.bin.json"))$data %>% drop()
# calculate cell area
lat <- grid[, 2]
lon <- grid[, 1]
cellarea <- lpjmlkit::calc_cellarea(lat)

vars_ecorisk <- data.frame(
  row.names = c("grid", "fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff", "transp", "vegc", "firef", "rh", "harvestc", "rharvestc", "pft_harvestc", "pft_rharvestc", "evap", "interc", "discharge", "soilc", "litc", "swc", "vegn", "soilnh4", "soilno3", "leaching", "n2o_denit", "n2o_nit", "n2_emis", "bnf", "n_volatilization"),
  outname = c("grid.bin.json", "fpc.bin.json", "fpc_bft.bin.json", "cftfrac.bin.json", "firec.bin.json", "mnpp.bin.json", "mrunoff.bin.json", "mtransp.bin.json", "vegc.bin.json", "firef.bin.json", "mrh.bin.json", "flux_harvest.bin.json", "flux_rharvest.bin.json", "pft_harvest.pft.bin.json", "pft_rharvest.pft.bin.json", "mevap.bin.json", "minterc.bin.json", "mdischarge.bin.json", "soilc.bin.json", "litc.bin.json", "mswc.bin.json", "vegn.bin.json", "soilnh4.bin.json", "soilno3.bin.json", "mleaching.bin.json", "mn2o_denit.bin.json", "mn2o_nit.bin.json", "mn2_emis.bin.json", "mbnf.bin.json", "mn_volatilization.bin.json")
)
ecorisk <- ecoriskWrapper(
  folderRef = pnvFolder,
  folderScen = runFolder,
  readPreviouslySavedData = F,
  nitrogen = TRUE,
  varnames = vars_ecorisk,
  weighting = "equal",
  saveFileData = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_overtime_data.RData",
  saveFileEcoRisk = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_overtime_gamma.RData",
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1500:2014),
  dimensionsOnlyLocal = F, window = 30
)

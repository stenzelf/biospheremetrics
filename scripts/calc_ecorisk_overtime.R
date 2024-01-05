library(devtools)
library(lpjmlkit)
library(magrittr)
library(biospheremetrics)

run_folder <- "./output/lu_1500_2014/"
pnv_folder <- "./output/pnv_1500_2014/"
out_folder <- "./Metrics/"
lpj_input <- "./historical/"

# read grid
grid <- read_io(paste0(run_folder, "grid.bin.json"))
# calculate cell area
lat <- grid[, , 2]
lon <- grid[, , 1]
cellarea <- calc_cellarea(grid)

vars_ecorisk <- data.frame(
  row.names = c("grid", "fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff",
                "transp", "vegc", "firef", "rh", "harvestc", "rharvestc",
                "pft_harvestc", "pft_rharvestc", "evap", "interc", "discharge",
                "soilc", "litc", "swc", "vegn", "soilnh4", "soilno3",
                "leaching", "n2o_denit", "n2o_nit", "n2_emis", "bnf",
                "n_volatilization"),
  outname = c("grid.bin.json", "fpc.bin.json", "fpc_bft.bin.json",
              "cftfrac.bin.json", "firec.bin.json", "mnpp.bin.json",
              "mrunoff.bin.json", "mtransp.bin.json", "vegc.bin.json",
              "firef.bin.json", "mrh.bin.json", "flux_harvest.bin.json",
              "flux_rharvest.bin.json", "pft_harvest.pft.bin.json",
              "pft_rharvest.pft.bin.json", "mevap.bin.json", "minterc.bin.json",
              "mdischarge.bin.json", "soilc.bin.json", "litc.bin.json",
              "mswc.bin.json", "vegn.bin.json", "soilnh4.bin.json",
              "soilno3.bin.json", "mleaching.bin.json", "mn2o_denit.bin.json",
              "mn2o_nit.bin.json", "mn2_emis.bin.json", "mbnf.bin.json",
              "mn_volatilization.bin.json")
)

ecorisk <- ecorisk_wrapper(
  path_ref = pnv_folder,
  path_scen = run_folder,
  read_saved_data = FALSE,
  nitrogen = TRUE,
  varnames = vars_ecorisk,
  weighting = "equal",
  save_data = "./data/ecorisk_202306_overtime_data.RData",
  save_ecorisk = "./data/ecorisk_202306_overtime_gamma.RData",
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1500:2014),
  dimensions_only_local = FALSE,
  window = 30
)

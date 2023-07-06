# biospheremetrics


*The goal of biospheremetrics is to provide functions to calculate and plot 
the biosphere integrity metrics M-ECO and M-ECO in an R package based on 
outputs of [LPJmL](https://gitlab.pik-potsdam.de/lpjml/LPJmL_internal).
biospheremetrics utilizes the read functions of the 
[lpjmlkit package](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit).*

## Installation

You can install `biospheremetrics` by git cloning this repository:

```bash
git clone https://gitlab.pik-potsdam.de/stenzel/biospheremetrics.git <path_to_biospheremetrics>
```

and install via  [`devtools`](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf):

```R
devtools::install("<path_to_biospheremetrics>")
library("biospheremetrics")
```

alternatively, you can also load it from source:

```R
devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_paper/")
```

## Scripts

The `./scripts` folder contains scripts to be used on the PIK cluster to 
compute longer timeseries with higher RAM demand.

## Example

The following application example calculates the metrics BioCol and EcoRisk:

```R
library(devtools)
library(lpjmlkit)

devtools::load_all("/p/projects/open/Fabian/LPJbox/biospheremetrics_paper/")

run_folder <- "/p/projects/open/Fabian/runs/metrics_202306/output/lu_1500_2014/"
pnv_folder <- "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/"
out_folder <- "/p/projects/open/Fabian/Metrics/"
lpj_input <- "/p/projects/lpjml/input/historical/"

# read grid
grid <- lpjmlkit::read_io(paste0(run_folder,"grid.bin.json"))$data
# calculate cell area
lat <- grid[, , 2]
lon <- grid[, , 1]
cellarea <- lpjmlkit::calc_cellarea(grid)

################# calculate BioCol ################
# 16GB of RAM are enough to calculate BioCol for a smaller analysis window (~40 years)
# for longer spans (500 years) - use separate script ("read_in_BioCol_data.R") 
# and submit as cluster job using "sbatch R_read_in_BioCol_data.sh" - analysis for "biocol overtime" below
vars_biocol <- data.frame(
  row.names = c("grid", "npp", "pft_npp", "pft_harvest", "pft_rharvest",
                "firec", "timber_harvest", "cftfrac", "fpc"),
  outname = c("grid.bin.json", "mnpp.bin.json", "pft_npp.bin.json",
              "pft_harvest.pft.bin.json","pft_rharvest.pft.bin.json",
              "firec.bin.json","timber_harvestc.bin.json","cftfrac.bin.json",
              "fpc.bin.json")
)

biocol <- calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1980,
  stop_year = 2014,
  reference_npp_time_span = 1510:1539, 
  reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
  read_saved_data = FALSE,
  save_data = TRUE,
  npp_threshold = 20,
  data_file = "/p/projects/open/Fabian/Metrics/BioCol_202306.RData",
  external_fire = FALSE,
  external_wood_harvest = TRUE,
  external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
  external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
  varnames = vars_biocol,
  grass_scaling = FALSE,
  include_fire = FALSE
)

plot_biocol(
  biocol_data = biocol,
  path_write = paste0(out_folder,"BioCol/"),
  plotyears = c(1980,2014),
  min_val = 0,
  max_val = 90,
  legendpos = "left",
  start_year = 1980,
  mapyear = 2000,
  highlightyear = 2000,
  eps = FALSE
)

############## analyse and plot biocol overtime #################
# first submit `R_read_BioCol_data.sh` to cluster via slurm to read in and process the input files, a lot of memory is required for this
# then here only read the preprocessed data file (read_saved_data = TRUE)
biocol_overtime <- calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1500,
  stop_year = 2014,
  reference_npp_time_span = 1550:1579,
  reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
  read_saved_data = TRUE,
  save_data = FALSE,
  npp_threshold = 20,
  data_file = "/p/projects/open/Fabian/Metrics/data/BioCol_202306_overtime.RData",
  external_fire = FALSE,
  external_wood_harvest = TRUE,
  external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
  external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
  varnames = vars_biocol,
  grass_scaling = FALSE,
  include_fire = FALSE
)

plot_biocol(
  biocol_data = biocol_overtime,
  path_write = paste0(out_folder,"BioCol/"),
  plotyears = c(1550,2014),
  min_val = 0,
  max_val = 90,
  legendpos = list(x=1550,y=23),
  start_year = 1500,
  mapyear = 2000,
  highlightyear = 2000,
  eps = FALSE
)

################# compute EcoRisk ################
vars_ecorisk <- data.frame(
  row.names = c("grid","fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff",
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
              "pft_rharvest.pft.bin.json", "mevap.bin.json",
              "minterc.bin.json", "mdischarge.bin.json", "soilc.bin.json",
              "litc.bin.json", "mswc.bin.json", "vegn.bin.json",
              "soilnh4.bin.json", "soilno3.bin.json", "mleaching.bin.json",
              "mn2o_denit.bin.json", "mn2o_nit.bin.json", "mn2_emis.bin.json",
              "mbnf.bin.json", "mn_volatilization.bin.json")
)

ecorisk <- ecorisk_wrapper(
  path_ref = pnv_folder, 
  path_scen = run_folder, 
  read_saved_data = TRUE,
  nitrogen = TRUE,
  varnames = vars_ecorisk,
  weighting = "equal",
  save_data = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData",
  save_ecorisk = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_gamma.RData",
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1985:2014),
  dimensions_only_local = FALSE
)

# plot ecorisk
plot_ecorisk_map(
  ecorisk$ecorisk_total,
  file = paste0(out_folder,"EcoRisk/ecorisk.png"),
  title="ecorisk"
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

################# ecorisk biomes ################

biome_classes <- classify_biomes(
  path_reference = pnv_folder,
  files_reference = list(
    grid = paste0(pnv_folder,"grid.bin.json"),
    fpc = paste0(pnv_folder,"fpc.bin.json"),
    vegc = paste0(pnv_folder,"vegc.bin.json"),
    pft_lai = paste0(pnv_folder,"pft_lai.bin.json"),
    temp = "/p/projects/lpjml/input/historical/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm",
    elevation = "/p/projects/lpjml/input/historical/input_VERSION2/elevation.bin"
  ),
  time_span_reference = as.character(1985:2014), 
  savanna_proxy = list(pft_lai = 6),
  montane_arctic_proxy = list(elevation = 1000) 
)

biome_classes_pi <- classify_biomes(
  path_reference = pnv_folder,
  files_reference = list(
    grid = paste0(pnv_folder,"grid.bin.json"),
    fpc = paste0(pnv_folder,"fpc.bin.json"),
    vegc = paste0(pnv_folder,"vegc.bin.json"),
    pft_lai = paste0(pnv_folder,"pft_lai.bin.json"),
    temp = "/p/projects/lpjml/input/historical/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm",
    elevation = "/p/projects/lpjml/input/historical/input_VERSION2/elevation.bin"
  ),
  time_span_reference = as.character(1901:1910), 
  savanna_proxy = list(pft_lai = 6),
  montane_arctic_proxy = list(elevation = 1000) 
)

plot_biomes(biome_data = biome_classes,
            display_area = TRUE,
            cellarea = cellarea,
            file_name = paste0(out_folder,"EcoRisk/biomes_2005-2014.png"),
            order_legend = 1:19,
            to_robinson = FALSE)
plot_biomes(biome_data=biome_classes_pi,
            display_area = TRUE,
            cellarea = cellarea,
            file_name = paste0(out_folder,"EcoRisk/biomes_1901-1910.png"),
            order_legend = 1:19,
            to_robinson = FALSE)

# compute median ecorisk values for biomes/large worldregions
ecorisk_disaggregated_full <- disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)
ecorisk_disaggregated_full[is.na(meco_disaggregated_full)] <- 0

ecorisk_disaggregated_4regions <- disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "4biomes"
)

plot_ecorisk_radial_panel(
  data = ecorisk_disaggregated_full[-c(17,18,19),,], 
  biomeNames = get_biome_names(1)[-c(17,18,19)],
  file = paste0(out_folder,"EcoRisk/EcoRisk_panel_1564_vs_2002.png"),
  quantile = TRUE,
  eps = TRUE
)

plot_ecorisk_radial_panel(
  data = ecorisk_disaggregated_4regions[,,], 
  biomeNames = c("tropics","temperate","boreal","arctic"),
  file = paste0(out_folder,"EcoRisk/EcoRisk_4regions_1564_vs_2002.png"),
  quantile = TRUE,
  eps = TRUE
)

################# ecorisk overtime ################
# first use the script `R_calc_ecorisk_overtime.sh` to read in and process the data
# on the PIK cluster this takes about a day for 100 years and 80GB of memory

load("/p/projects/open/Fabian/Metrics/data/ecorisk_202306_overtime_gamma.RData")

ecorisk_overtime_allbiomes <- disaggregate_into_biomes(
  ecorisk = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)

plot_ecorisk_over_time_panel(
  data = ecorisk_overtime_allbiomes,
  timerange = c(1916,2003),
  biomeNames = c("tropic","temperate","boreal","arctic"),
  file = paste0(out_folder,"overtime_panel.png"),
  eps=TRUE
)

ecorisk_overtime_biome16 <- disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)

plot_ecorisk_over_time_panel(
  data = ecorisk_overtime_biome16[-c(3,17,18),,,],
  timerange = c(1916,2003),
  biomeNames = get_biome_names(1)[-c(3,17,18)], 
  file = paste0(out_folder,"overtime_panel_16.png"),
  eps=TRUE
)



################# compare to average PI biome cell #################
intra_biome_distrib_PI <- calculate_within_biome_diffs(
  biome_classes = biome_classes_pi,
  intra_biome_distrib_file = "/p/projects/open/Fabian/Metrics/data/ecorisk_PNV_intra_biome_distrib_file_202306.RData",
  dataFile_base = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData",
  create = TRUE, plotting = TRUE, res = 0.02, vars_ecorisk = vars_ecorisk,
  plot_folder = out_folder, time_span_reference = as.character(1891:1920))

plot_biome_internal_distribution(
  data = intra_biome_distrib_PI[,"ecorisk_total",],
  file = paste0(out_folder,"EcoRisk_newCol/distribution_PI_within_biome_differences.png"),
  biomes_abbrv = get_biome_names(1),
  scale = 4,
  eps=TRUE,
  palette = paletteNew
)

################# cross table average biomes today #################

dataFile_base = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData"
data_file = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_crosstable_data.RData"
ecoriskFile = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_crosstable_gamma.RData"

ecorisk_cross_table(dataFileIn = dataFile_base, 
                    dataFileOut = data_file, 
                    biome_classes_in = biome_classes) #pickCells = pickcells)

nbiomes <- length(biome_classes$biome_names)
ecorisk_crosstable_today <- ecorisk_wrapper(
  path_ref = NULL, 
  path_scen = NULL, 
  read_saved_data = TRUE,
  save_data = data_file, 
  save_ecorisk = ecoriskFile, 
  varnames = vars_ecorisk,
  time_span_reference = as.character(1985:2014),
  time_span_scenario = as.character(1985:2014)
  #ncells = nbiomes^2
)

# if written previously, load crosstable data
if (FALSE) {
  load(ecoriskFile)
  ecorisk_crosstable_today <- ecorisk
}

crosstable <- ecorisk_crosstable_today$ecorisk_total
dim(crosstable) <- c(nbiomes,nbiomes)
colnames(crosstable) <- get_biome_names(1)
rownames(crosstable) <- get_biome_names(2)

plot_ecorisk_cross_table(
  data = crosstable[-c(3,8,18,19),-c(3,8,18,19)], 
  file = paste0(out_folder,"/EcoRisk_newCol/crosstable_today.png"),
  lmar=12,
  palette = paletteNew
)

```

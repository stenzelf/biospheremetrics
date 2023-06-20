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

## Example

The `./scripts` folder contains scripts to be used on the PIK cluster to 
compute longer timeseries with higher RAM demand.

## Example

The following application example calculates the metrics BioCol and EcoRisk:

```R
library(devtools)
library(lpjmlkit)
library(sf)
library(terra)

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
cellarea <- lpjmlkit::calc_cellarea(lat)

################# calculate BioCol ################
# 16GB of RAM are enough to calculate BioCol for a smaller analysis window (~40 years)
# for longer spans (500 years) - use separate script ("read_in_BioCol_data.R") 
# and submit as cluster job using "sbatch R_read_in_BioCol_data.sh" - analysis for "biocol overtime" below
vars_biocol <- data.frame(row.names = c("grid",         "npp",     "pft_npp",    "pft_harvest",        "pft_rharvest",        "firec",    "timber_harvest",     "cftfrac",    "fpc"),
                        outname = c("grid.bin.json",  "mnpp.bin.json","pft_npp.bin.json","pft_harvest.pft.bin.json","pft_rharvest.pft.bin.json","firec.bin.json","timber_harvestc.bin.json","cftfrac.bin.json","fpc.bin.json"))

biocol <- calcBioCol(inFol_lu = runFolder,inFol_pnv = pnvFolder, gridbased = T,
                 startyr = 1980, stopyr = 2014, reference_npp_time_span = 1510:1539, 
                 reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
                 readPreviouslySavedData = F, saveDataFile = T,npp_threshold = 20,
                 dataFile = "/p/projects/open/Fabian/Metrics/BioCol_202306.RData",
                 external_fire = F, external_wood_harvest = T,
                 external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
                 external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
                 varnames = vars_biocol, grass_scaling = F, include_fire = F)

plotBioCol(biocolData = biocol,outFol = paste0(outFolder,"BioCol/"),
         plotyears = c(1980,2014), minVal = 0, maxVal = 90, legendpos = "left",
         startyr = 1980, mapyear = 2000, highlightyear = 2000, eps = F)

############## analyse and plot biocol overtime #################
# first submit `R_read_BioCol_data.sh` to cluster via slurm to read in and process the input files, a lot of memory is required for this
# then here only read the preprocessed data file (readPreviouslySavedData = T)
biocol_overtime <- calcBioCol(inFol_lu = runFolder,inFol_pnv = pnvFolder, gridbased = T,
                 startyr = 1500, stopyr = 2014, reference_npp_time_span = 1550:1579, 
                 reference_npp_file = "/p/projects/open/Fabian/runs/metrics_202306/output/pnv_1500_2014/mnpp.bin.json",
                 readPreviouslySavedData = T, saveDataFile = F,npp_threshold = 20,
                 dataFile = "/p/projects/open/Fabian/Metrics/data/BioCol_202306_overtime.RData",
                 external_fire = F, external_wood_harvest = T,
                 external_fire_file = "/p/projects/open/Fabian/LPJbox/human_ignition_fraction.RData",
                 external_wood_harvest_file = "/p/projects/open/LanduseData/LUH2_v2h/wood_harvest_biomass_sum_1500-2014_67420.RData",
                 varnames = vars_biocol, grass_scaling = F, include_fire = F)

plotBioCol(biocolData = biocol_overtime,outFol = paste0(outFolder,"BioCol/"),
         plotyears = c(1550,2014), minVal = 0, maxVal = 90, legendpos = list(x=1550,y=23),
         startyr = 1500, mapyear = 2000, highlightyear = 2000, eps = F)

################# compute EcoRisk ################
vars_ecorisk <- data.frame(row.names = c("grid",       "fpc",         "fpc_bft",         "cftfrac",          "firec",        "npp",          "runoff",           "transp",         "vegc",         "firef",          "rh",         "harvestc",             "rharvestc",              "pft_harvestc",           "pft_rharvestc",            "evap",           "interc",  "discharge",        "soilc",          "litc",         "swc",          "vegn",         "soilnh4",        "soilno3",          "leaching",         "n2o_denit",          "n2o_nit",          "n2_emis",          "bnf",          "n_volatilization"     ),
                        outname = c("grid.bin.json","fpc.bin.json","fpc_bft.bin.json","cftfrac.bin.json","firec.bin.json","mnpp.bin.json","mrunoff.bin.json","mtransp.bin.json","vegc.bin.json","firef.bin.json","mrh.bin.json","flux_harvest.bin.json","flux_rharvest.bin.json","pft_harvest.pft.bin.json","pft_rharvest.pft.bin.json","mevap.bin.json","minterc.bin.json","mdischarge.bin.json","soilc.bin.json","litc.bin.json","mswc.bin.json","vegn.bin.json","soilnh4.bin.json","soilno3.bin.json","mleaching.bin.json","mn2o_denit.bin.json","mn2o_nit.bin.json","mn2_emis.bin.json","mbnf.bin.json","mn_volatilization.bin.json")
)

ecorisk <- ecoriskWrapper(folderRef = pnvFolder, 
                    folderScen = runFolder, 
                    readPreviouslySavedData = T,
                    nitrogen = TRUE,
                    varnames = vars_ecorisk,
                    weighting = "equal",
                    saveFileData = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData",
                    saveFileEcoRisk = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_gamma.RData",
                    time_span_reference = c(1550:1579),
                    time_span_scenario = c(1985:2014),
                    dimensionsOnlyLocal = F)

# plot ecorisk
plotEcoRiskmap(ecorisk$ecorisk_total, file = paste0(outFolder,"EcoRisk/ecorisk.png"), title="ecorisk")
plotEcoRiskmap(ecorisk$vegetation_structure_change, file = paste0(outFolder,"EcoRisk/vs.png"), title="vegetation structure change")
plotEcoRiskmap(ecorisk$local_change, file = paste0(outFolder,"EcoRisk/lc.png"), title="local change")
plotEcoRiskmap(ecorisk$global_importance, file = paste0(outFolder,"EcoRisk/gi.png"), title="global importance")
plotEcoRiskmap(ecorisk$ecosystem_balance, file = paste0(outFolder,"EcoRisk/eb.png"), title="ecosystem balance")
plotEcoRiskmap(ecorisk$carbon_stocks, file = paste0(outFolder,"EcoRisk/cs.png"), title="carbon_stocks")
plotEcoRiskmap(ecorisk$carbon_fluxes, file = paste0(outFolder,"EcoRisk/cf.png"), title="carbon_fluxes")
plotEcoRiskmap(ecorisk$water_stocks, file = paste0(outFolder,"EcoRisk/ws.png"), title=" water_stocks")
plotEcoRiskmap(ecorisk$water_fluxes, file = paste0(outFolder,"EcoRisk/wf.png"), title=" water_fluxes")
plotEcoRiskmap(ecorisk$nitrogen_stocks, file = paste0(outFolder,"EcoRisk/ns.png"), title=" nitrogen_stocks")
plotEcoRiskmap(ecorisk$nitrogen_fluxes, file = paste0(outFolder,"EcoRisk/nf.png"), title=" nitrogen_fluxes")

################# ecorisk biomes ################

biome_classes <- classify_biomes(path_reference = pnvFolder,
              files_reference = list(
                grid = paste0(pnvFolder,"grid.bin.json"),
                fpc = paste0(pnvFolder,"fpc.bin.json"),
                vegc = paste0(pnvFolder,"vegc.bin.json"),
                pft_lai = paste0(pnvFolder,"pft_lai.bin.json"),
                temp = "/p/projects/lpjml/input/historical/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm",
                elevation = "/p/projects/lpjml/input/historical/input_VERSION2/elevation.bin"
              ),
              time_span_reference = as.character(1985:2014), 
              savanna_proxy = list(pft_lai = 6),
              montane_arctic_proxy = list(elevation = 1000) 
              )

biome_classes_PI <- classify_biomes(path_reference = pnvFolder,
              files_reference = list(
                grid = paste0(pnvFolder,"grid.bin.json"),
                fpc = paste0(pnvFolder,"fpc.bin.json"),
                vegc = paste0(pnvFolder,"vegc.bin.json"),
                pft_lai = paste0(pnvFolder,"pft_lai.bin.json"),
                temp = "/p/projects/lpjml/input/historical/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm",
                elevation = "/p/projects/lpjml/input/historical/input_VERSION2/elevation.bin"
              ),
              time_span_reference = as.character(1901:1910), 
              savanna_proxy = list(pft_lai = 6),
              montane_arctic_proxy = list(elevation = 1000) 
              )

plot_biomes(biome_data=biome_classes,display_area = T, cellarea = cellarea,
            file_name = paste0(outFolder,"EcoRisk/biomes_2005-2014.png"),
            order_legend = 1:19,to_robinson = F)
plot_biomes(biome_data=biome_classes_PI,display_area = T, cellarea = cellarea,
            file_name = paste0(outFolder,"EcoRisk/biomes_1901-1910.png"),
            order_legend = 1:19,to_robinson = F)

# compute median ecorisk values for biomes/large worldregions
ecorisk_disaggregated_full <- disaggregateIntoBiomes(data = ecorisk,
                          biome_class = biome_classes,
                          type = "quantile",classes = "allbiomes")
ecorisk_disaggregated_full[is.na(meco_disaggregated_full)] <- 0

ecorisk_disaggregated_4regions <- disaggregateIntoBiomes(data = ecorisk,
                                                      biome_class = biome_classes,
                                                      type = "quantile",classes = "4biomes")

plotEcoRiskradialPanel(data = ecorisk_disaggregated_full[-c(17,18,19),,], 
                    biomeNames = get_biome_names(1)[-c(17,18,19)],
                    file = paste0(outFolder,"EcoRisk/EcoRisk_panel_1564_vs_2002.png"),
                    quantile = T,eps = T)
plotEcoRiskradialPanel(data = ecorisk_disaggregated_4regions[,,], 
                    biomeNames = c("tropics","temperate","boreal","arctic"),
                    file = paste0(outFolder,"EcoRisk/EcoRisk_4regions_1564_vs_2002.png"),
                    quantile = T,eps = T)
################# ecorisk overtime ################
# first use the script `R_calc_ecorisk_overtime.sh` to read in and process the data
# on the PIK cluster this takes about a day for 100 years and 80GB of memory
load("/p/projects/open/Fabian/Metrics/data/ecorisk_202306_overtime_gamma.RData")
ecorisk_overtime_allbiomes <- biospheremetrics::disaggregateEcoRiskintoBiomes(ecorisk = ecorisk,
                                               biome_class = biome_classes,
                                               type = "quantile",
                                               classes = "allbiomes")

plotMECOovertimePanel(data = ecorisk_overtime_allbiomes,
                      timerange = c(1916,2003),
                      biomeNames = c("tropic","temperate","boreal","arctic"),
                      file = paste0(outFolder,"overtime_panel.png"), eps=T)

ecorisk_overtime_biome16 <- disaggregateIntoBiomes(data = ecorisk,
                                                biome_class = biome_classes,
                                                type = "quantile",
                                                classes = "allbiomes")

plotMECOovertimePanel(data = ecorisk_overtime_biome16[-c(3,17,18),,,],
                      timerange = c(1916,2003),
                      biomeNames = get_biome_names(1)[-c(3,17,18)], 
                      file = paste0(outFolder,"overtime_panel_16.png"),
                      eps=T)



################# compare to average PI biome cell #################
intra_biome_distrib_PI <- calculateWithinBiomeDiffs(
                          biome_classes = biome_classes_PI, 
                          intra_biome_distrib_file = "/p/projects/open/Fabian/Metrics/data/ecorisk_PNV_intra_biome_distrib_file_202306.RData",
                          dataFile_base = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData",
                          create = T, plotting = T, res = 0.02, vars_ecorisk = vars_ecorisk,
                          plot_folder = outFolder, time_span_reference = as.character(1891:1920))

plotBiomeInternalDistribution(data = intra_biome_distrib_PI[,"ecorisk_total",],
                              file = paste0(outFolder,"EcoRisk_newCol/distribution_PI_within_biome_differences.png"),
                              biomes_abbrv = get_biome_names(1), scale = 4,eps=T, palette = paletteNew)

################# cross table average biomes today #################

dataFile_base = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_data.RData"
dataFile = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_crosstable_data.RData"
ecoriskFile = "/p/projects/open/Fabian/Metrics/data/ecorisk_202306_crosstable_gamma.RData"

ecoriskCrossTable(dataFileIn = dataFile_base, 
               dataFileOut = dataFile, 
               biome_classes_in = biome_classes) #pickCells = pickcells)
nbiomes <- length(biome_classes$biome_names)
ecorisk_crosstable_today <- ecoriskWrapper(folderRef = NULL, 
                                   folderScen = NULL, 
                                   readPreviouslySavedData = TRUE,
                                   saveFileData = dataFile, 
                                   saveFileEcoRisk = ecoriskFile, 
                                   varnames = vars_ecorisk,
                                   time_span_reference = as.character(1985:2014),
                                   time_span_scenario = as.character(1985:2014)
                                   #ncells = nbiomes^2
                                   )
# if written previously, load crosstable data
if (F) {
  load(ecoriskFile)
  ecorisk_crosstable_today <- ecorisk
}

crosstable <- ecorisk_crosstable_today$ecorisk_total
dim(crosstable) <- c(nbiomes,nbiomes)
colnames(crosstable) <- get_biome_names(1)
rownames(crosstable) <- get_biome_names(2)

plotEcoRiskcrossTable(data = crosstable[-c(3,8,18,19),-c(3,8,18,19)], 
                      file = paste0(outFolder,"/EcoRisk_newCol/crosstable_today.png"),
                      lmar=12, palette = paletteNew)
```

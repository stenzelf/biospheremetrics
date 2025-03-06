run_folder <- "./output/lu_1500_2014/"
pnv_folder <- "./output/pnv_1500_2014/"
out_folder <- "./biospheremetrics/"
lpj_input <- "/path/to/historical/lpjml/input/data/"

################# calculate BioCol ################
# 16GB of RAM are enough to calculate BioCol for a smaller analysis window (~40 years)
# for longer spans (500 years) - use separate script ("read_in_BioCol_data.R")
# and submit as cluster job using "sbatch R_read_in_BioCol_data.sh" - analysis for "biocol overtime" below

biocol <- biospheremetrics::calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1980,
  stop_year = 2014,
  reference_npp_time_span = 1510:1539,
  read_saved_data = FALSE,
  save_data = FALSE,
  npp_threshold = 20,
)

biospheremetrics::plot_biocol(
  biocol_data = biocol,
  path_write = paste0(out_folder, "BioCol/"),
  plotyears = c(1980, 2014),
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
biospheremetrics::biocol_overtime <- calc_biocol(
  path_lu = run_folder,
  path_pnv = pnv_folder,
  gridbased = TRUE,
  start_year = 1500,
  stop_year = 2014,
  reference_npp_time_span = 1550:1579,
  read_saved_data = TRUE,
  save_data = FALSE,
  npp_threshold = 20,
  data_file = "</path/to/saved/data/file>" # from script read_in_biocol_data
)

biospheremetrics::plot_biocol(
  biocol_data = biocol_overtime,
  path_write = paste0(out_folder, "BioCol/"),
  plotyears = c(1550, 2014),
  min_val = 0,
  max_val = 90,
  legendpos = list(x = 1550, y = 23),
  start_year = 1500,
  mapyear = 2000,
  highlightyear = 2000,
  eps = FALSE
)

################# compute EcoRisk ################

ecorisk <- biospheremetrics::ecorisk_wrapper(
  path_ref = pnv_folder,
  path_scen = run_folder,
  read_saved_data = FALSE,
  nitrogen = TRUE,
  weighting = "equal",
  save_data = NULL,
  save_ecorisk = NULL,
  time_span_reference = c(1550:1579),
  time_span_scenario = c(1985:2014),
  dimensions_only_local = FALSE
)

# plot ecorisk
biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "ecorisk_total",
  file = paste0(out_folder, "EcoRisk/ecorisk.png"),
  title = "ecorisk"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "vegetation_structure_change",
  file = paste0(out_folder, "EcoRisk/vs.png"),
  title = "vegetation structure change"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "local_change",
  file = paste0(out_folder, "EcoRisk/lc.png"),
  title = "local change"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "global_importance",
  file = paste0(out_folder, "EcoRisk/gi.png"),
  title = "global importance"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "ecosystem_balance",
  file = paste0(out_folder, "EcoRisk/eb.png"),
  title = "ecosystem balance"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "carbon_stocks",
  file = paste0(out_folder, "EcoRisk/cs.png"),
  title = "carbon_stocks"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "carbon_fluxes",
  file = paste0(out_folder, "EcoRisk/cf.png"),
  title = "carbon_fluxes"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "water_stocks",
  file = paste0(out_folder, "EcoRisk/ws.png"),
  title = " water_stocks"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "water_fluxes",
  file = paste0(out_folder, "EcoRisk/wf.png"),
  title = " water_fluxes"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "nitrogen_stocks",
  file = paste0(out_folder, "EcoRisk/ns.png"),
  title = " nitrogen_stocks"
)

biospheremetrics::plot_ecorisk_map(
  ecorisk_object = ecorisk,
  plot_dimension = "nitrogen_fluxes",
  file = paste0(out_folder, "EcoRisk/nf.png"),
  title = " nitrogen_fluxes"
)

################# ecorisk biomes ################

biome_classes <- biospheremetrics::classify_biomes(
  path_reference = pnv_folder,
  files_reference = list(
    grid = paste0(pnv_folder, "grid.bin.json"),
    fpc = paste0(pnv_folder, "fpc.bin.json"),
    vegc = paste0(pnv_folder, "vegc.bin.json"),
    pft_lai = paste0(pnv_folder, "pft_lai.bin.json"),
    temp = paste0(lpj_input, "/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm"),
    elevation = paste0(lpj_input, "/input_VERSION2/elevation.bin")
  ),
  time_span_reference = as.character(1985:2014),
  savanna_proxy = list(pft_lai = 6),
  montane_arctic_proxy = list(elevation = 1000)
)

biome_classes_pi <- biospheremetrics::classify_biomes(
  path_reference = pnv_folder,
  files_reference = list(
    grid = paste0(pnv_folder, "grid.bin.json"),
    fpc = paste0(pnv_folder, "fpc.bin.json"),
    vegc = paste0(pnv_folder, "vegc.bin.json"),
    pft_lai = paste0(pnv_folder, "pft_lai.bin.json"),
    temp = paste0(lpj_input, "/GSWP3-W5E5/tas_gswp3-w5e5_1901-2016.clm"),
    elevation = paste0(lpj_input, "/input_VERSION2/elevation.bin")
  ),
  time_span_reference = as.character(1901:1910),
  savanna_proxy = list(pft_lai = 6),
  montane_arctic_proxy = list(elevation = 1000)
)

biospheremetrics::plot_biomes_mercator(
  biome_ids = biome_classes$biome_id,
  file = paste0(out_folder, "EcoRisk/biomes_2005-2014.png"),
  order_legend = seq_len(19)
)
biospheremetrics::plot_biomes_mercator(
  biome_ids = biome_classes_pi$biome_id,
  file = paste0(out_folder, "EcoRisk/biomes_1901-1910.png"),
  order_legend = seq_len(19)
)

# compute median ecorisk values for biomes/large worldregions
ecorisk_disaggregated_full <- biospheremetrics::disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)
ecorisk_disaggregated_full[is.na(ecorisk_disaggregated_full)] <- 0

ecorisk_disaggregated_4regions <- biospheremetrics::disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "4biomes"
)

biospheremetrics::plot_ecorisk_radial_panel(
  data = ecorisk_disaggregated_full[-c(17, 18, 19), , ],
  biome_names = get_biome_names(1)[-c(17, 18, 19)],
  file = paste0(out_folder, "EcoRisk/EcoRisk_panel_1564_vs_2002.png"),
  use_quantile = TRUE,
  eps = TRUE
)

biospheremetrics::plot_ecorisk_radial_panel(
  data = ecorisk_disaggregated_4regions[, , ],
  biome_names = c("tropics", "temperate", "boreal", "arctic"),
  file = paste0(out_folder, "EcoRisk/EcoRisk_4regions_1564_vs_2002.png"),
  use_quantile = TRUE,
  eps = TRUE
)

################# ecorisk overtime ################
# first use the script `R_calc_ecorisk_overtime.sh` to read in and process the data
# on the PIK cluster this takes about a day for 100 years and 80GB of memory

load("/path/to/ecorisk_overtime_gamma.RData") # containing ecorisk object

ecorisk_overtime_allbiomes <- biospheremetrics::disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)

biospheremetrics::plot_ecorisk_over_time_panel(
  data = ecorisk_overtime_allbiomes,
  timerange = c(1916, 2003),
  biome_names = c("tropic", "temperate", "boreal", "arctic"),
  file = paste0(out_folder, "overtime_panel.png"),
  eps = TRUE
)

ecorisk_overtime_biome16 <- biospheremetrics::disaggregate_into_biomes(
  data = ecorisk,
  biome_class = biome_classes,
  type = "quantile",
  classes = "allbiomes"
)

biospheremetrics::plot_ecorisk_over_time_panel(
  data = ecorisk_overtime_biome16[-c(3, 17, 18), , , ],
  timerange = c(1916, 2003),
  biome_names = get_biome_names(1)[-c(3, 17, 18)],
  file = paste0(out_folder, "overtime_panel_16.png"),
  eps = TRUE
)

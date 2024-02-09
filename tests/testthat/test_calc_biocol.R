# outputs in CLM format
test_that("test calc_biocal", {

  vars_biocol <- data.frame(
    row.names = c(
      "grid", "fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff",
      "transp", "vegc", "firef", "rh", "harvestc", "rharvestc",
      "pft_harvestc", "pft_rharvestc", "evap", "interc", "discharge",
      "soilc", "litc", "swc", "vegn", "soilnh4", "soilno3",
      "leaching", "n2o_denit", "n2o_nit", "n2_emis", "bnf",
      "n_volatilization", "gpp", "res_storage", "lakevol", "ndepos",
      "rd", "prec", "terr_area", "irrig", "nfert_agr", "nmanure_agr",
      "firen", "harvestn", "rivervol", "irrig_stor", "swc_vol", "rootmoist",
      "pft_npp", "timber_harvest"
    ),
    outname = c(
      "grid.bin.json", "fpc.bin.json", "fpc_bft.bin.json",
      "cftfrac.bin.json", "firec.bin.json", "npp.bin.json",
      "runoff.bin.json", "transp.bin.json", "vegc.bin.json",
      "firef.bin.json", "rh.bin.json", "harvestc.bin.json",
      "rharvestc.bin.json", "pft_harvest.pft.bin.json",
      "pft_rharvest.pft.bin.json", "evap.bin.json",
      "interc.bin.json", "discharge.bin.json", "soilc.bin.json",
      "litc.bin.json", "swc.bin.json", "vegn.bin.json",
      "soilnh4.bin.json", "soilno3.bin.json", "leaching.bin.json",
      "n2o_denit.bin.json", "n2o_nit.bin.json", "n2_emis.bin.json",
      "bnf.bin.json", "n_volatilization.bin.json", "gpp.bin.json",
      "res_storage.bin.json", "lakevol.bin.json", "ndepos.bin.json",
      "rd.bin.json", "prec.bin.json", "terr_area.bin.json",
      "irrig.bin.json", "nfert_agr.bin.json", "nmanure_agr.bin.json",
      "firen.bin.json", "harvestn.bin.json", "rivervol.bin.json",
      "irrig_stor.bin.json", "swc_vol.bin.json", "rootmoist.bin.json",
      "pft_npp.bin.json", "timber_harvestc.bin.json"
    )
  )

  save_data_file <- file.path(
    tempdir(),
    "biocol_202306_data.RData"
  )

  pnv_folder <- paste0(
    system.file(
      "extdata/run/pnv_1500_2016",
      package = "biospheremetrics"
    ),
    "/"
  )

  biocol <- calc_biocol(
    path_lu = paste0(
      system.file(
        "extdata/run/lu_1500_2016",
        package = "biospheremetrics"
      ),
      "/"
    ),
    path_pnv = pnv_folder,
    gridbased = TRUE,
    start_year = 1500,
    stop_year = 2016,
    reference_npp_time_span = 1510:1539,
    reference_npp_file = paste0(pnv_folder, "npp.bin.json"),
    read_saved_data = FALSE,
    save_data = TRUE,
    npp_threshold = 1,
    data_file = save_data_file,
    external_fire = FALSE,
    external_wood_harvest = FALSE,
    external_fire_file = "",
    external_wood_harvest_file = "",
    varnames = vars_biocol,
    grass_scaling = FALSE,
    include_fire = FALSE
  )

  # test if values are between 0 and 1
  testthat::expect_true(
    all(biocol$biocol < biocol$npp_potential)
  )

  # test for expected variables
  expect_setequal(
    names(biocol),
    vars_biocol <- c(
      "biocol_overtime",
      "biocol_overtime_abs",
      "biocol_overtime_abs_frac_piref",
      "biocol_overtime_frac_piref",
      "biocol_overtime_frac",
      "biocol_overtime_abs_frac",
      "npp_harv_overtime",
      "npp_luc_overtime",
      "npp_act_overtime",
      "npp_pot_overtime",
      "npp_eco_overtime",
      "harvest_grasslands_overtime",
      "harvest_bioenergy_overtime",
      "harvest_cft_overtime",
      "rharvest_cft_overtime",
      "fire_overtime",
      "timber_harvest_overtime",
      "wood_harvest_overtime",
      "biocol",
      "biocol_frac",
      "npp",
      "biocol_frac_piref",
      "npp_potential",
      "npp_ref",
      "harvest_cft",
      "rharvest_cft",
      "biocol_harvest",
      "biocol_luc"
    )
  )

  # test for expected array dimensions
  expect_true(
    sapply(biocol, function(x) {
      all(dim(x) == c(2, 517)) || all(dim(x) == c(2, 30) || all(dim(x) == NULL))
    }) %>%
      all() %>%
      suppressWarnings()
  )

  expected_biocol <- readRDS("../testdata/calc_biocol.rds")

  # test for expected output
  expect_equal(biocol, expected_biocol)

  unlink(save_data_file)
})

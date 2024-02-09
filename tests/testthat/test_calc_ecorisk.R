# outputs in CLM format
test_that("test calc_ecorisk", {
  # read grid
  #   grid <- system.file(
  #     "extdata/run/lu_1500_2016",
  #     "grid.bin.json",
  #     package = "biospheremetrics"
  #   ) %>%
  #     lpjmlkit::read_io()
  # 
  #   test_that("grid object exists", {
  #     expect_true(exists("grid"))
  #   })
  # 
  #   # calculate cell area
  #   lat <- grid$data[, , 2]
  #   lon <- grid$data[, , 1]
  #   cellarea <- lpjmlkit::calc_cellarea(grid)

  vars_ecorisk <- data.frame(
    row.names = c(
      "grid", "fpc", "fpc_bft", "cftfrac", "firec", "npp", "runoff",
      "transp", "vegc", "firef", "rh", "harvestc", "rharvestc",
      "pft_harvestc", "pft_rharvestc", "evap", "interc", "discharge",
      "soilc", "litc", "swc", "vegn", "soilnh4", "soilno3",
      "leaching", "n2o_denit", "n2o_nit", "n2_emis", "bnf",
      "n_volatilization", "gpp", "res_storage", "lakevol", "ndepos",
      "rd", "prec", "terr_area", "irrig", "nfert_agr", "nmanure_agr",
      "firen", "harvestn", "rivervol", "irrig_stor", "swc_vol", "rootmoist"
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
      "irrig_stor.bin.json", "swc_vol.bin.json", "rootmoist.bin.json"
    )
  )

  save_data_file <- file.path(
    tempdir(),
    "ecorisk_202306_data.RData"
  )

  save_gamma_file <- file.path(
    tempdir(),
    "ecorisk_202306_gamma.RData"
  )

  ecorisk <- ecorisk_wrapper(
    path_ref = paste0(
      system.file(
        "extdata/run/pnv_1500_2016",
        package = "biospheremetrics"
      ),
      "/"
    ),
    path_scen = paste0(
      system.file(
        "extdata/run/lu_1500_2016",
        package = "biospheremetrics"
      ),
      "/"
    ),
    read_saved_data = FALSE,
    nitrogen = TRUE,
    varnames = vars_ecorisk,
    weighting = "equal",
    save_data = save_data_file,
    save_ecorisk = save_gamma_file,
    time_span_reference = c(1550:1579),
    time_span_scenario = c(1987:2016),
    dimensions_only_local = FALSE,
  )

  # test if values are between 0 and 1
  testthat::expect_true(
    sapply(ecorisk, function(x) all(dplyr::between(x, 0, 1))) %>%
      all()
  )

  # test for expected variables
  expect_setequal(
    names(ecorisk),
    vars_ecorisk <- c(
      "ecorisk_total", "vegetation_structure_change", "local_change",
      "global_importance", "ecosystem_balance", "c2vr", "carbon_stocks",
      "carbon_fluxes", "carbon_total", "water_total", "water_fluxes",
      "nitrogen_stocks", "nitrogen_fluxes", "nitrogen_total"
    )
  )

  # test for expected array dimensions
  expect_true(
    sapply(ecorisk, function(x) {
      all(dim(x) == c(4, 2, 1)) || all(dim(x) == c(2, 1))
    }) %>%
      all() %>%
      suppressWarnings()
  )

  expected_ecorisk <- readRDS("../testdata/calc_ecorisk.rds")

  # test for expected output
  expect_equal(ecorisk, expected_ecorisk)

  unlink(save_data_file)
  unlink(save_gamma_file)
})

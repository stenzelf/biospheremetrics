# outputs in CLM format
test_that("test calc_ecorisk_overtime", {
  # read grid
  grid <- system.file(
    "extdata/run/lu_1500_2016",
    "grid.bin.json",
    package = "biospheremetrics"
  ) %>%
    lpjmlkit::read_io()

  test_that("grid object exists", {
    expect_true(exists("grid"))
  })

  # calculate cell area
  lat <- grid$data[, , 2]
  lon <- grid$data[, , 1]
  cellarea <- lpjmlkit::calc_cellarea(grid)

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
      "pft_rharvest.pft.bin.json", "mevap.bin.json",
      "interc.bin.json", "discharge.bin.json", "soilc.bin.json",
      "litc.bin.json", "swc.bin.json", "vegn.bin.json",
      "soilnh4.bin.json", "soilno3.bin.json", "leaching.bin.json",
      "n2o_denit.bin.json", "n2o_nit.bin.json", "n2_emis.bin.json",
      "bnf.bin.json", "n_volatilization.bin.json", "gpp.bin.json",
      "res_storage.bin.json", "lakevol.bin.json", "ndepos.bin.json",
      "rd.bin.json", "mprec.bin.json", "terr_area.bin.json",
      "irrig.bin.json", "nfert_agr.bin.json", "nmanure_agr.bin.json",
      "firen.bin.json", "harvestn.bin.json", "rivervol.bin.json",
      "irrig_stor.bin.json", "swc_vol.bin.json", "rootmoist.bin.json"
    )
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
    save_data = file.path(
      tempdir(),
      "ecorisk_202306_overtime_data.RData"
    ),
    save_ecorisk = file.path(
      tempdir(),
      "ecorisk_202306_overtime_gamma.RData"
    ),
    time_span_reference = c(1550:1579),
    time_span_scenario = c(1550:1579),
    dimensions_only_local = FALSE,
  )
  print(ecorisk)
})

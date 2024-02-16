# outputs in CLM format
test_that("test calc_ecorisk", {

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
    weighting = "equal",
    save_data = save_data_file,
    save_ecorisk = save_gamma_file,
    time_span_reference = c(1550:1579),
    time_span_scenario = c(1987:2016),
    dimensions_only_local = FALSE,
  )

  # test if values are between 0 and 1
  vars_ecorisk <- c(
    "ecorisk_total", "vegetation_structure_change", "local_change",
    "global_importance", "ecosystem_balance", "c2vr", "carbon_stocks",
    "carbon_fluxes", "carbon_total", "water_total", "water_fluxes",
    "nitrogen_stocks", "nitrogen_fluxes", "nitrogen_total"
  )
  testthat::expect_true(
    sapply(ecorisk[vars_ecorisk], function(x) all(dplyr::between(x, 0, 1))) %>%
      all()
  )

  # test for expected variables
  expect_setequal(
    names(ecorisk),
    c(vars_ecorisk, "lat", "lon")
  )

  # test for expected array dimensions
  expect_true(
    sapply(ecorisk[vars_ecorisk], function(x) {
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


# test for read grid
test_that("test read grid", {
  # read grid
  grid <- system.file(
    "extdata/run/lu_1500_2016",
    "grid.bin.json",
    package = "biospheremetrics"
  ) %>%
    lpjmlkit::read_io()

  expect_true(exists("grid"))
})

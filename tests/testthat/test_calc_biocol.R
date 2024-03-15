# outputs in CLM format
test_that("test calc_biocal", {

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
    start_year = 1501,
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
      "biocol_luc",
      "biocol_overtime_pos",
      "biocol_overtime_pos_frac_piref",
      "biocol_overtime_pos_frac",
      "lat",
      "lon",
      "cellarea"
    )
  )

  # test for expected array dimensions
  expect_true(
    sapply(biocol, function(x) {
      all(dim(x) == c(2, 516)) || all(dim(x) == c(2, 30)) || all(dim(x) == NULL)
    }) %>%
      all() %>%
      suppressWarnings()
  )

  expected_biocol <- readRDS("../testdata/calc_biocol.rds")

  # test for expected output
  expect_equal(biocol, expected_biocol)

  unlink(save_data_file)
})

# written by Fabian Stenzel
# 2022-2023 - stenzel@pik-potsdam.de

################# BioCol calc functions  ###################

#' Calculate BioCol based on a PNV run and LU run of LPJmL
#'
#' Function to calculate BioCol based on a PNV run and LU run of LPJmL
#' @param files_scenario list with variable names and corresponding file paths
#' (character string) of the scenario LPJmL run. All needed files are
#' provided in XXX. E.g.: list(npp = "/temp/npp.bin.json")
#' @param files_baseline list with variable names and corresponding file paths
#' (character string) of the baseline LPJmL run. All needed files are
#' provided in XXX. E.g.: list(npp = "/temp/npp.bin.json"). If not
#' needed for the applied method, set to NULL.
#' @param files_reference list with npp file path (character string) of the
#' reference LPJmL run (usually Holocene/preindustrial).
#' E.g.: list(npp = "/temp/npp.bin.json"). If NULL uses baseline npp.
#' @param time_span_scenario time span to be used for the scenario run, defined
#' as a character vector, e.g. `as.character(1982:2011)` (required)
#' @param time_span_baseline time span to be used for the baseline run, defined
#' as a character vector, e.g. `as.character(1901:1930)`. Can differ in offset
#' and length from `time_span_scenario`! If `NULL` value of `time_span_scenario`
#' is used
#' @param time_span_reference time span to read reference npp from, using
#'     index years 10:39 from potential npp input if set to NULL (default: NULL)
#' @param gridbased logical are pft outputs gridbased or pft-based?
#' @param read_saved_data flag whether to read previously saved data
#'        instead of reading it in from output files (default FALSE)
#' @param save_data whether to save input data to file (default FALSE)
#' @param data_file file to save/read input data to/from (default NULL)
#' @param include_fire boolean include firec in calculation of BioCol?
#' (default TRUE)
#' @param external_fire instead of reading in firec for fire emissions, read in
#'        this external firec file from a separate spitfire run with disabled
#'        lighning. this will then include only human induced fires
#' (default FALSE)
#' @param external_wood_harvest include external wood harvest from LUH2_v2h
#'        (default FALSE)
#' @param grass_scaling whether to scale pasture harvest according to
#'        data given via grass_harvest_file (default FALSE)
#' @param npp_threshold lower threshold for npp (to mask out non-lu areas
#'        according to Haberl et al. 2007). Below BioCol will be set to 0.
#'        (default: 20 gC/m2)
#' @param epsilon minimum value for npp, below which it will be set to 0
#' @param grass_harvest_file file containing grazing data to rescale the
#'        grassland harvests according to Herrero et al. 2013. File contains:
#'        grazing_data list object with $name and $id of 29 world regions, and
#'        $Herrero_2000_kgDM_by_region containing for each of these regions and
#'        mapping_lpj67420_to_grazing_regions array with a mapping between 67420
#'        LPJmL cells and the 29 regions
#' @param external_fire_file path to external file with human induced fire
#'        fraction c(cell,month,year) since 1500
#' @param external_wood_harvest_file path to R-file containing processed
#'        timeline of maps for LUH2_v2h woodharvest
#'
#' @return list data object containing BioCol and components as arrays: 
#'         biocol_overtime, biocol_overtime_abs, biocol_overtime_abs_frac_piref, 
#'         biocol_overtime_frac_piref, biocol_overtime_frac, 
#'         biocol_overtime_abs_frac, npp_harv_overtime, npp_luc_overtime, 
#'         npp_act_overtime, npp_pot_overtime, npp_eco_overtime, 
#'         harvest_grasslands_overtime, harvest_bioenergy_overtime, 
#'         harvest_cft_overtime, rharvest_cft_overtime, fire_overtime, 
#'         timber_harvest_overtime, wood_harvest_overtime, biocol, biocol_frac, 
#'         npp, biocol_frac_piref, npp_potential, npp_ref, harvest_cft, 
#'         rharvest_cft, biocol_harvest, biocol_luc
#'
#' @export
read_calc_biocol <- function(
    # nolint
    files_scenario,
    files_baseline,
    files_reference = NULL,
    time_span_scenario,
    time_span_baseline = NULL,
    time_span_reference = NULL,
    gridbased = TRUE,
    read_saved_data = FALSE,
    save_data = FALSE,
    data_file = NULL,
    include_fire = FALSE,
    external_fire = FALSE,
    external_wood_harvest = FALSE,
    grass_scaling = FALSE,
    npp_threshold = 20,
    epsilon = 0.001, # gC/m2
    grass_harvest_file = NULL,
    external_fire_file = NULL,
    external_wood_harvest_file = NULL) {
  if (is.null(files_reference)) {
    files_reference <- list(npp = baseline_npp_file)
  }
  if (is.null(time_span_baseline)) {
    time_span_baseline <- time_span_scenario
  }
  if (is.null(time_span_reference)) {
    time_span_reference <- time_span_scenario[3:12]
  }
  if (grass_scaling && !file.exists(grass_harvest_file)) {
    stop(
      paste0("Grass harvest scaling enabled, but grass_harvest_file \
              does not exist in: ", grass_harvest_file)
    )
  }
  if (external_wood_harvest && !file.exists(external_wood_harvest_file)) {
    stop(
      paste0("External wood harvest enabled, but external_wood_harvest_file \
              does not exist in: ", external_wood_harvest_file)
    )
  }
  if (external_fire && !file.exists(external_fire_file)) {
    stop(
      paste0("External fire fraction file enabled, but external_fire_file \
              does not exist in: ", external_fire_file)
    )
  }
  # reading required data
  if (read_saved_data) {
    if (file.exists(data_file)) {
      message("Reading in data from previously saved data file")
      load(data_file)
      wood_harvest[is.na(wood_harvest)] <- 0
    } else {
      stop(
        paste0(
          "data_file: '",
          data_file,
          "' does not exist but is required since reading is set to FALSE."
        )
      )
    }
    if (save_data) {
      save_data <- FALSE
      message(
        "Both read_saved_data and save_data have been set to TRUE. ",
        "Overwriting with the same data does not make sense, saving ",
        "disabled. "
      )
    }
  } else {
    message("Reading in data from outputs")

    file_type <- tools::file_ext(files_baseline$grid)

    if (file_type %in% c("json", "clm")) {
      # read grid
      grid <- lpjmlkit::read_io(
        files_baseline$grid
      )
      # calculate cell area
      cellarea <- drop(lpjmlkit::read_io(
        filename = files_baseline$terr_area
      )$data) # in m2
      lat <- grid$data[, , 2]
      lon <- grid$data[, , 1]

      npp <- lpjmlkit::read_io(
        files_scenario$npp,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        drop() # gC/m2
      npp[npp < epsilon] <- 0

      if (!is.null(files_reference)) {
        npp_ref <- lpjmlkit::read_io(
          files_reference$npp,
          subset = list(year = as.character(time_span_reference))
        ) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum)) %>%
          drop()
        npp_ref[npp_ref < epsilon] <- 0
      }

      pftnpp <- lpjmlkit::read_io(
        files_scenario$pft_npp,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        suppressWarnings()
      pftnpp[pftnpp < epsilon] <- 0


      harvest <- lpjmlkit::read_io(
        files_scenario$pft_harvestc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        suppressWarnings()

      rharvest <- lpjmlkit::read_io(
        files_scenario$pft_rharvestc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        suppressWarnings()

      timber <- lpjmlkit::read_io(
        files_scenario$timber_harvestc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        drop() %>%
        suppressWarnings()

      if (include_fire) {
        # read fire in monthly res. if possible, then multiply with monthly
        # human/total ignition frac and aggregate to yearly. Otherwise aggregate
        # human/total ignition frac to yearly and multiply with yearly firec
        fire_raw <- lpjmlkit::read_io(
          files_scenario$firec,
          subset = list(year = as.character(time_span_scenario))
        ) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(band = sum)) %>%
          drop() %>%
          suppressWarnings()


        if (external_fire) {
          load(external_fire_file) # frac = c(cell,month,year)
        }

        if ("month" %in% names(dim(fire_raw))) {
          if (external_fire) {
            fire <- apply(
              fire_raw * frac[, , year = time_span_scenario],
              c("cell", "year"),
              sum,
              na.rm = TRUE
            ) # gC/m2
            rm(frac)
          } else {
            fire <- apply(
              fire_raw,
              c("cell", "year"),
              sum,
              na.rm = TRUE
            ) # gC/m2
          }
          rm(fire_raw)
        } else {
          if (external_fire) {
            frac_yearly <- apply(
              frac[, , year = time_span_scenario],
              c("cell", "year"),
              mean,
              na.rm = TRUE
            )
            fire <- fire_raw * frac_yearly
            rm(frac_yearly, frac)
          }
        }
        gc()
      } else {
        fire <- timber * 0
      }

      if (external_wood_harvest) {
        load(external_wood_harvest_file) # wh_lpj in kgC
        wh_years <- names(wh_lpj[1, ])
        # from kgC to gC/m2
        wood_harvest <- (
          wh_lpj[, match(time_span_scenario, wh_years)] * 10^3 / cellarea
        )
        # the division can lead to NAs
        wood_harvest[is.na(wood_harvest)] <- 0
        rm(wh_lpj, wh_years)
        gc()
      } else {
        wood_harvest <- fire * 0
      }

      cftfrac <- lpjmlkit::read_io(
        files_scenario$cftfrac,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        suppressWarnings()

      npp_potential <- lpjmlkit::read_io(
        files_baseline$npp,
        subset = list(year = as.character(time_span_baseline))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        drop() # gC/m2
      npp_potential[npp_potential < epsilon] <- 0

      fpc <- lpjmlkit::read_io(
        files_scenario$fpc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(subset = list(band = "natural stand fraction"))

      pftbands <- lpjmlkit::read_meta(files_scenario$fpc)$nbands - 1
    } else if (file_type == "nc") { # to be added
      stop(
        "nc reading has not been updated to latest functionality.",
        " Please contact Fabian Stenzel"
      )
    } else {
      stop(
        "Unrecognized file type (",
        file_type,
        ")"
      )
    }

    bp_bands <- c(15, 16, 31, 32)
    grass_bands <- c(14, 30)
    nat_bands <- seq_len(pftbands)

    if (!gridbased) { # needs to be scaled with standfrac
      pftnpp[, , nat_bands] <- pftnpp[, , nat_bands] * fpc[, , band = rep("natural stand fraction", pftbands)]
      pftnpp[, , -c(nat_bands)] <- pftnpp[, , -c(nat_bands)] * cftfrac
      harvest <- harvest * cftfrac
      rharvest <- rharvest * cftfrac
    }

    pftnpp_grasslands <- apply(
      pftnpp[, , pftbands + grass_bands],
      c(1, 2),
      sum
    ) # gC/m2 only from grassland bands

    pftnpp_cft <- apply(
      pftnpp[, , -c(nat_bands, pftbands + grass_bands, pftbands + bp_bands)],
      c(1, 2), sum
    ) # gC/m2 not from grassland and bioenergy bands

    pftnpp_bioenergy <- apply(
      pftnpp[, , pftbands + bp_bands],
      c(1, 2),
      sum
    ) # gC/m2 only from bioenergy bands

    pftnpp_nat <- apply(
      pftnpp[, , nat_bands], c(1, 2), sum
    ) # gC/m2

    if (is.null(files_reference)) {
      pi_window <- 3:32
      npp_ref <- npp_potential[, pi_window]
    } # npp_ref

    harvest_grasslands <- apply(
      harvest[, , grass_bands],
      c(1, 2),
      sum
    ) # gC/m2 only from grassland bands

    harvest_bioenergy <- apply(
      harvest[, , bp_bands],
      c(1, 2),
      sum
    ) # gC/m2 only from bioenergy bands

    harvest_cft <- apply(
      harvest[, , -c(grass_bands, bp_bands)],
      c(1, 2),
      sum
    ) # gC/m2 not from grassland and bioenergy bands

    rharvest_cft <- apply(
      rharvest[, , -c(grass_bands, bp_bands)],
      c(1, 2),
      sum
    ) # gC/m2 not from grassland and bioenergy bands

    if (save_data) {
      if (!file.exists(data_file)) {
        message("Writing data file: ", data_file)
      } else {
        message(
          "Data file (",
          data_file,
          ") already exists, old file renamed to: ",
          data_file,
          "_sav"
        )
        file.rename(data_file, paste0(data_file, "_sav"))
      }

      save(npp_potential,
        npp,
        npp_ref,
        pftnpp_cft,
        pftnpp_nat,
        pftnpp_grasslands,
        pftnpp_bioenergy,
        harvest_cft,
        rharvest_cft,
        fire,
        timber,
        fpc,
        cftfrac,
        harvest_grasslands,
        harvest_bioenergy,
        wood_harvest,
        lat,
        lon,
        cellarea,
        file = data_file
      )
    }
  }

  message("Calculating data")

  if (grass_scaling) {
    load(grass_harvest_file)

    nregs <- length(grazing_data$name)

    lpj_grass_harvest_region <- array(0, dim = nregs)

    lpj_grass_harvest_2000 <- rowMeans(
      harvest_grasslands[, (1995 - start_year + 1):(2005 - start_year + 1)]
    ) * cellarea / 1000 * 2 # from gC/m2 to kgDM

    grassland_scaling_factor_cellwise <- array(1, dim = grid$ncells)

    for (r in seq_len(nregs)) {
      lpj_grass_harvest_region[r] <- sum(
        lpj_grass_harvest_2000[which(mapping_lpj67420_to_grazing_regions == r)]
      )
    }

    scaling_factor <- (
      grazing_data$Herrero_2000_kgDM_by_region / lpj_grass_harvest_region
    )

    for (r in seq_len(nregs)) {
      grassland_scaling_factor_cellwise[
        which(mapping_lpj67420_to_grazing_regions == r)
      ] <- scaling_factor[r]
    }
    harvest_grasslands <- harvest_grasslands * rep(
      grassland_scaling_factor_cellwise,
      times = length(harvest_grasslands[1, ])
    )
  }

  npp_act_overtime <- colSums(npp * cellarea) / 10^15 # gC/m2 to GtC
  npp_pot_overtime <- colSums(npp_potential * cellarea) / 10^15 # gC/m2 to GtC
  npp_eco_overtime <- colSums(pftnpp_nat * cellarea) / 10^15 # gC/m2 to GtC
  npp_luc_overtime <- npp_pot_overtime - npp_act_overtime

  harvest_cft_overtime <- colSums(
    harvest_cft * cellarea
  ) / 10^15 # gC/m2 to GtC
  rharvest_cft_overtime <- colSums(
    rharvest_cft * cellarea
  ) / 10^15 # gC/m2 to GtC
  harvest_grasslands_overtime <- colSums(
    harvest_grasslands * cellarea
  ) / 10^15 # gC/m2 to GtC
  harvest_bioenergy_overtime <- colSums(
    harvest_bioenergy * cellarea
  ) / 10^15 # gC/m2 to GtC

  timber_harvest_overtime <- colSums(
    timber * cellarea
  ) / 10^15 # gC/m2 to GtC
  fire_overtime <- colSums(
    fire * cellarea
  ) / 10^15 # gC/m2 to GtC
  wood_harvest_overtime <- colSums(
    wood_harvest * cellarea
  ) / 10^15 # gC/m2 to GtC

  if (include_fire) {
    npp_harv_overtime <- harvest_cft_overtime + rharvest_cft_overtime +
      harvest_grasslands_overtime + harvest_bioenergy_overtime +
      timber_harvest_overtime + fire_overtime + wood_harvest_overtime
  } else {
    npp_harv_overtime <- harvest_cft_overtime + rharvest_cft_overtime +
      harvest_grasslands_overtime + harvest_bioenergy_overtime +
      timber_harvest_overtime + wood_harvest_overtime
  }
  biocol_overtime <- npp_harv_overtime + npp_luc_overtime
  biocol_overtime_frac_piref <- (
    biocol_overtime / mean(colSums(npp_ref * cellarea) / 10^15)
  )
  biocol_overtime_frac <- (
    biocol_overtime / npp_pot_overtime
  )
  biocol_luc <- npp_potential - npp
  # biocol_luc2 <- (npp_potential - pftnpp_cft) * apply(cftfrac[, , -c(grass_bands, bp_bands)], c("cell", "year"), sum) +
  #               (npp_potential - pftnpp_grasslands) * apply(cftfrac[, , grass_bands], c("cell", "year"), sum) +
  #               (npp_potential - pftnpp_bioenergy) * apply(cftfrac[, , bp_bands], c("cell", "year"), sum)

  # pick a PI window that excludes onset effects, but is reasonable early

  if (include_fire) {
    biocol_harvest <- (
      harvest_cft + rharvest_cft + harvest_grasslands + harvest_bioenergy +
        timber + fire + wood_harvest
    )
  } else {
    biocol_harvest <- (
      harvest_cft + rharvest_cft + harvest_grasslands + harvest_bioenergy +
        timber + wood_harvest
    )
  }

  biocol <- biocol_harvest + biocol_luc
  # set to 0 below lower threshold of NPP
  biocol[abs(npp_potential) < npp_threshold] <- 0
  # actual NPPpot as ref
  biocol_frac <- biocol / npp_potential

  # NPPpi as ref
  biocol_frac_piref <- biocol / rowMeans(npp_ref)

  # take the abs of biocol and sum that up for overtime
  biocol_overtime_abs <- colSums(abs(biocol * cellarea)) / 10^15
  biocol_overtime_abs_frac_piref <- biocol_overtime_abs * 10^15 /
    mean(colSums(npp_ref * cellarea))
  biocol_overtime_abs_frac <- biocol_overtime_abs / npp_pot_overtime

  return(list(
    biocol_overtime = biocol_overtime,
    biocol_overtime_abs = biocol_overtime_abs,
    biocol_overtime_abs_frac_piref = biocol_overtime_abs_frac_piref,
    biocol_overtime_frac_piref = biocol_overtime_frac_piref,
    biocol_overtime_frac = biocol_overtime_frac,
    biocol_overtime_abs_frac = biocol_overtime_abs_frac,
    npp_harv_overtime = npp_harv_overtime,
    npp_luc_overtime = npp_luc_overtime,
    npp_act_overtime = npp_act_overtime,
    npp_pot_overtime = npp_pot_overtime,
    npp_eco_overtime = npp_eco_overtime,
    harvest_grasslands_overtime = harvest_grasslands_overtime,
    harvest_bioenergy_overtime = harvest_bioenergy_overtime,
    harvest_cft_overtime = harvest_cft_overtime,
    rharvest_cft_overtime = rharvest_cft_overtime,
    fire_overtime = fire_overtime,
    timber_harvest_overtime = timber_harvest_overtime,
    wood_harvest_overtime = wood_harvest_overtime,
    biocol = biocol,
    biocol_frac = biocol_frac,
    npp = npp,
    biocol_frac_piref = biocol_frac_piref,
    npp_potential = npp_potential,
    npp_ref = npp_ref,
    harvest_cft = harvest_cft,
    rharvest_cft = rharvest_cft,
    biocol_harvest = biocol_harvest,
    biocol_luc = biocol_luc
  )) # , biocol_luc_piref = biocol_luc_piref))
}

#' Calculate BioCol
#'
#' Wrapper function to calculate BioCol
#'
#' @param path_lu folder of landuse scenario run
#' @param path_pnv folder of pnv reference run
#' @param start_year first year of simulations
#' @param stop_year last year of simulations
#' @param reference_npp_time_span time span to read reference npp from, using
#'     index years 10:39 from potential npp input if set to NULL (default: NULL)
#' @param reference_npp_file file to read reference npp from, using
#'        potential npp input if set to NULL (default: NULL)
#' @param gridbased logical are pft outputs gridbased or pft-based?
#' @param read_saved_data flag whether to read previously saved data
#'        instead of reading it in from output files (default FALSE)
#' @param save_data whether to save input data to file (default FALSE)
#' @param data_file file to save/read input data to/from (default NULL)
#' @param include_fire boolean include firec in calculation of BioCol?
#'        (default TRUE)
#' @param external_fire instead of reading in firec for fire emissions, read in
#'        this external firec file from a separate spitfire run with disabled
#'        lighning. this will then include only human induced fires
#'        (default FALSE)
#' @param external_wood_harvest include external wood harvest from LUH2_v2h
#'        (default FALSE)
#' @param grass_scaling whether to scale pasture harvest according to
#'        data given via grass_harvest_file (default FALSE)
#' @param npp_threshold lower threshold for npp (to mask out non-lu areas
#'        according to Haberl et al. 2007). Below BioCol will be set to 0.
#'        (default: 20 gC/m2)
#' @param grass_harvest_file file containing grazing data to rescale the
#'        grassland harvests according to Herrero et al. 2013. File contains:
#'        grazing_data list object with $name and $id of 29 world regions, and
#'        $Herrero_2000_kgDM_by_region containing for each of these regions and
#'        mapping_lpj67420_to_grazing_regions array with a mapping between 67420
#'        LPJmL cells and the 29 regions
#' @param external_fire_file path to external file with human induced fire
#'        fraction c(cell,month,year) since 1500
#' @param external_wood_harvest_file path to R-file containing processed
#'        timeline of maps for LUH2_v2h woodharvest
#'
#' @return list data object containing BioCol and components as arrays: biocol,
#'         biocol_overtime, biocol_overtime_piref, biocol_frac, npp_potential,
#'         biocol_overtime_abs_frac_piref, biocol_frac_piref, npp_act_overtime,
#'         npp_pot_overtime, npp_eco_overtime, npp_ref, harvest_cft_overtime,
#'         npp_luc_overtime, rharvest_cft_overtime, fire_overtime,
#'         timber_harvest_overtime, harvest_cft, rharvest_cft,
#'         wood_harvest_overtime, biocol_harvest, biocol_luc
#'
#' @export
calc_biocol <- function(
    path_lu,
    path_pnv,
    start_year,
    stop_year,
    reference_npp_time_span = NULL,
    reference_npp_file = NULL,
    varnames = NULL,
    gridbased = TRUE,
    read_saved_data = FALSE,
    save_data = FALSE,
    data_file = NULL,
    include_fire = FALSE,
    external_fire = FALSE,
    external_wood_harvest = FALSE,
    grass_scaling = FALSE,
    npp_threshold = 20,
    grass_harvest_file = NULL,
    external_fire_file = NULL,
    external_wood_harvest_file = NULL) {
  
  metric_files <- system.file(
    "extdata",
    "metric_files.yml",
    package = "biospheremetrics"
  ) %>%
    yaml::read_yaml()

  file_extension <- get_major_file_ext(paste0(path_lu))
  outputs <- metric_files$metric$biocol$output

  # translate output names (from metric_files.yml) and folders to files_scenarios/reference lists
  files_scenario <- list(
    grid = paste0(path_lu, outputs$grid$name, ".", file_extension),
    terr_area = paste0(path_lu, outputs$terr_area$name, ".", file_extension),
    npp = paste0(path_lu, outputs$npp$name, ".", file_extension),
    pft_npp = paste0(path_lu, outputs$pft_npp$name, ".", file_extension),
    pft_harvestc = paste0(path_lu, outputs$pft_harvestc$name, ".", file_extension),
    pft_rharvestc = paste0(path_lu, outputs$pft_rharvestc$name, ".", file_extension),
    firec = paste0(path_lu, outputs$firec$name, ".", file_extension),
    timber_harvestc = paste0(path_lu, outputs$timber_harvestc$name, ".", file_extension),
    cftfrac = paste0(path_lu, outputs$cftfrac$name, ".", file_extension),
    fpc = paste0(path_lu, outputs$fpc$name, ".", file_extension)
  )
  files_baseline <- list(
    grid = paste0(path_pnv, outputs$grid$name, ".", file_extension),
    terr_area = paste0(path_pnv, outputs$terr_area$name, ".", file_extension),
    npp = paste0(path_pnv, outputs$npp$name, ".", file_extension),
    pft_npp = paste0(path_pnv, outputs$pft_npp$name, ".", file_extension),
    pft_harvestc = paste0(path_pnv, outputs$pft_harvestc$name, ".", file_extension),
    pft_rharvestc = paste0(path_pnv, outputs$pft_rharvestc$name, ".", file_extension),
    firec = paste0(path_pnv, outputs$firec$name, ".", file_extension),
    timber_harvestc = paste0(path_pnv, outputs$timber_harvestc$name, ".", file_extension),
    cftfrac = paste0(path_pnv, outputs$cftfrac$name, ".", file_extension),
    fpc = paste0(path_pnv, outputs$fpc$name, ".", file_extension)
  )
  if (is.null(reference_npp_file)) reference_npp_file <- files_baseline$npp
  files_reference <- list(
    npp = reference_npp_file
  )
  return(
    read_calc_biocol(
      files_scenario = files_scenario,
      files_baseline = files_baseline,
      files_reference = files_reference,
      time_span_scenario = as.character(start_year:stop_year),
      time_span_baseline = as.character(start_year:stop_year),
      time_span_reference = reference_npp_time_span,
      gridbased = gridbased,
      read_saved_data = read_saved_data,
      save_data = save_data,
      data_file = data_file,
      include_fire = include_fire,
      external_fire = external_fire,
      external_wood_harvest = external_wood_harvest,
      grass_scaling = grass_scaling,
      npp_threshold = npp_threshold,
      grass_harvest_file = grass_harvest_file,
      external_fire_file = external_fire_file,
      external_wood_harvest_file = external_wood_harvest_file
    )
  )
}
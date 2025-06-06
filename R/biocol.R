# written by Fabian Stenzel
# 2022-2023 - stenzel@pik-potsdam.de

################# BioCol calc functions  ###################

#' Calculate BioCol based on file lists from a PNV run and LU run of LPJmL.
#' Do not use this function directly, unless you are instructed to do so, there
#' is a wrapper called calc_biocol() which is for use of endusers.
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
#' @param suppress_warnings suppress warnings when reading files (default: TRUE)
#'
#' @return list data object containing BioCol and components as arrays:
#'         biocol_overtime, biocol_overtime_abs, biocol_overtime_abs_frac_piref,
#'         biocol_overtime_abs_frac, biocol_overtime_pos,
#'         biocol_overtime_pos_frac_piref,biocol_overtime_pos_frac,
#'         biocol_overtime_frac_piref, biocol_overtime_frac, npp_harv_overtime,
#'         npp_luc_overtime,npp_act_overtime, npp_pot_overtime,npp_eco_overtime,
#'         harvest_grasslands_overtime, harvest_bioenergy_overtime,
#'         harvest_cft_overtime, rharvest_cft_overtime, fire_overtime,
#'         timber_harvest_overtime, wood_harvest_overtime, biocol, biocol_frac,
#'         npp, biocol_frac_piref, npp_potential, npp_ref, harvest_cft,
#'         rharvest_cft, biocol_harvest, biocol_luc, lat, lon, cellarea
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
    external_wood_harvest_file = NULL,
    suppress_warnings = TRUE) {
  if (is.null(files_reference)) {
    files_reference <- list(npp = files_baseline$npp)
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
  start_year <- time_span_scenario[1]
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
      ) %>% suppressWarnings()
      # calculate cell area
      cellarea <- drop(lpjmlkit::read_io(
        filename = files_baseline$terr_area
      )$data) %>% suppressWarnings() # in m2
      lat <- grid$data[, , 2]
      lon <- grid$data[, , 1]

      npp <- abind::adrop(lpjmlkit::read_io(
        files_scenario$npp,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        # drop() %>% suppressWarnings()
        suppressWarnings(), drop = "band") # gC/m2
      npp[npp < epsilon] <- 0

      if (!is.null(files_reference)) {
        npp_ref <- abind::adrop(lpjmlkit::read_io(
          files_reference$npp,
          subset = list(year = as.character(time_span_reference))
        ) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum)) %>%
          # drop() %>% suppressWarnings()
          suppressWarnings(), drop = "band")
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

      timber <- abind::adrop(lpjmlkit::read_io(
        files_scenario$timber_harvestc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        # drop() %>%
        suppressWarnings(), drop = "band")

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
          # drop() %>%
          suppressWarnings()


        if (external_fire) {
          frac <- NULL
          load(external_fire_file) # frac = c(cell,month,year)
        }

        if ("month" %in% names(dim(fire_raw))) {
          if (external_fire) {
            fire <- apply(
              fire_raw *
                lpjmlkit::asub(frac,
                  year = time_span_scenario,
                  drop = FALSE
                ),
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
              lpjmlkit::asub(frac,
                year = time_span_scenario,
                drop = FALSE
              ),
              c("cell", "year"),
              mean,
              na.rm = TRUE
            ) # gC/m2
            fire <- fire_raw * frac_yearly
            rm(frac_yearly, frac)
          }
        }
        gc()
      } else {
        fire <- timber * 0
      }

      if (external_wood_harvest) {
        wh_lpj <- NULL
        load(external_wood_harvest_file) # wh_lpj in kgC

        # from kgC to gC/m2
        wood_harvest <- (
          lpjmlkit::asub(wh_lpj, year = time_span_scenario, drop = FALSE) *
            10^3 / cellarea
        )
        # the division can lead to NAs
        wood_harvest[is.na(wood_harvest)] <- 0
        rm(wh_lpj)
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

      npp_potential <- abind::adrop(lpjmlkit::read_io(
        files_baseline$npp,
        subset = list(year = as.character(time_span_baseline))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>%
        # drop() %>% suppressWarnings()
        suppressWarnings(), drop = "band") # gC/m2
      npp_potential[npp_potential < epsilon] <- 0

      fpc <- lpjmlkit::read_io(
        files_scenario$fpc,
        subset = list(year = as.character(time_span_scenario))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(subset = list(band = "natural stand fraction")) %>%
        suppressWarnings()

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
      pftnpp[, , nat_bands] <- lpjmlkit::asub(pftnpp,
        band = nat_bands,
        drop = FALSE
      ) *
        lpjmlkit::asub(fpc, band = rep(
          "natural stand fraction",
          pftbands
        ), drop = FALSE)
      pftnpp[, , -c(nat_bands)] <- lpjmlkit::asub(pftnpp,
        band = -c(nat_bands),
        drop = FALSE
      ) * cftfrac
      harvest <- harvest * cftfrac
      rharvest <- rharvest * cftfrac
    }

    pftnpp_grasslands <- apply(
      lpjmlkit::asub(pftnpp, band = (pftbands + grass_bands), drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2 only from grassland bands

    pftnpp_cft <- apply(
      lpjmlkit::asub(pftnpp,
        band = -c(nat_bands, pftbands + grass_bands, pftbands + bp_bands),
        drop = FALSE
      ),
      c("cell", "year"),
      sum
    ) # gC/m2 not from grassland and bioenergy bands

    pftnpp_bioenergy <- apply(
      lpjmlkit::asub(pftnpp, band = pftbands + bp_bands, drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2 only from bioenergy bands

    pftnpp_nat <- apply(
      lpjmlkit::asub(pftnpp, band = nat_bands, drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2

    if (is.null(files_reference)) {
      pi_window <- 3:32
      npp_ref <- lpjmlkit::asub(npp_potential, year = pi_window, drop = FALSE)
    } # npp_ref

    # lpjmlkit::asub(INARRAY, band = BANDS, drop = FALSE)
    harvest_grasslands <- apply(
      lpjmlkit::asub(harvest, band = grass_bands, drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2 only from grassland bands

    harvest_bioenergy <- apply(
      lpjmlkit::asub(harvest, band = bp_bands, drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2 only from bioenergy bands

    harvest_cft <- apply(
      lpjmlkit::asub(harvest, band = -c(grass_bands, bp_bands), drop = FALSE),
      c("cell", "year"),
      sum
    ) # gC/m2 not from grassland and bioenergy bands

    rharvest_cft <- apply(
      lpjmlkit::asub(rharvest, band = -c(grass_bands, bp_bands), drop = FALSE),
      c("cell", "year"),
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
    grazing_data <- mapping_lpj67420_to_grazing_regions <- NULL
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

  # take the abs of biocol and sum that up for overtime
  biocol_pos <- biocol
  biocol_pos[biocol_pos < 0] <- 0
  biocol_overtime_pos <- colSums(biocol_pos * cellarea) / 10^15
  biocol_overtime_pos_frac_piref <- biocol_overtime_pos * 10^15 /
    mean(colSums(npp_ref * cellarea))
  biocol_overtime_pos_frac <- biocol_overtime_pos / npp_pot_overtime

  return(list(
    biocol_overtime = biocol_overtime,
    biocol_overtime_abs = biocol_overtime_abs,
    biocol_overtime_abs_frac_piref = biocol_overtime_abs_frac_piref,
    biocol_overtime_abs_frac = biocol_overtime_abs_frac,
    biocol_overtime_pos = biocol_overtime_pos,
    biocol_overtime_pos_frac_piref = biocol_overtime_pos_frac_piref,
    biocol_overtime_pos_frac = biocol_overtime_pos_frac,
    biocol_overtime_frac_piref = biocol_overtime_frac_piref,
    biocol_overtime_frac = biocol_overtime_frac,
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
    biocol_luc = biocol_luc,
    lat = lat,
    lon = lon,
    cellarea = cellarea
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
#' @param replace_input_file_names list with alternative names for output
#'        identifiers to replace the ones in inst/ext_files/metric_files.yml.
#'        e.g. list(npp="mnpp") would replace the expected output for npp with
#'        mnpp followed by the automatically detected file extension (.bin.json)
#' @param suppress_warnings suppress warnings when reading files (default: TRUE)
#'
#' @return list data object containing BioCol and components as arrays: biocol,
#'         biocol_overtime, biocol_overtime_piref, biocol_frac, npp_potential,
#'         biocol_overtime_abs_frac_piref, biocol_frac_piref, npp_act_overtime,
#'         npp_pot_overtime, npp_eco_overtime, npp_ref, harvest_cft_overtime,
#'         npp_luc_overtime, rharvest_cft_overtime, fire_overtime,
#'         timber_harvest_overtime, harvest_cft, rharvest_cft,
#'         wood_harvest_overtime, biocol_harvest, biocol_luc, lat, lon, cellarea
#'
#' @examples
#' \dontrun{
#' calc_biocol(
#'   path_lu = run_folder,
#'   path_pnv = pnv_folder,
#'   gridbased = TRUE,
#'   start_year = 1980,
#'   stop_year = 2014,
#'   reference_npp_time_span = 1510:1539,
#'   read_saved_data = FALSE,
#'   save_data = FALSE,
#'   npp_threshold = 20,
#' )
#' }
#'
#' @md
#' @export
calc_biocol <- function(
    path_lu,
    path_pnv,
    start_year,
    stop_year,
    reference_npp_time_span = NULL,
    reference_npp_file = NULL,
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
    external_wood_harvest_file = NULL,
    replace_input_file_names = NULL,
    suppress_warnings = TRUE) {
  metric_files <- system.file(
    "extdata",
    "metric_files.yml",
    package = "biospheremetrics"
  ) %>%
    yaml::read_yaml()

  # translate output names (from metric_files.yml)
  # and folders to files_scenarios/reference lists
  file_extension <- get_major_file_ext(paste0(path_lu))
  files_scenario <- list()
  files_baseline <- list()

  for (output in names(metric_files$metric$biocol$output)) {
    # Iterate over all outputs
    if (is.null(replace_input_file_names[[output]])) {
      for (file in metric_files$file_name[[output]]) {
        full_file_path_lu <- paste0(path_lu, file, ".", file_extension)
        if (file.exists(full_file_path_lu)) {
          files_scenario[[output]] <- full_file_path_lu
        }
        full_file_path_pnv <- paste0(path_pnv, file, ".", file_extension)
        if (file.exists(full_file_path_pnv)) {
          files_baseline[[output]] <- full_file_path_pnv
        }
      }
      if (is.null(files_scenario[[output]])) {
        stop(
          "None of the default file names for ", output,
          " were found in ", path_lu, "please check or define manually",
          " using argument 'replace_input_file_names'. Stopping."
        )
      }
      if (is.null(files_baseline[[output]])) {
        stop(
          "None of the default file names for ", output,
          " were found in ", path_pnv, "please check or define manually",
          " using argument 'replace_input_file_names'. Stopping."
        )
      }
    } else {
      files_scenario[[output]] <- paste0(
        path_lu,
        replace_input_file_names[[output]], ".", file_extension
      )
      files_baseline[[output]] <- paste0(
        path_pnv,
        replace_input_file_names[[output]], ".", file_extension
      )
    }
  }
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
      external_wood_harvest_file = external_wood_harvest_file,
      suppress_warnings = suppress_warnings
    )
  )
}

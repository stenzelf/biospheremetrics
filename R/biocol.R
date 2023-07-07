# written by Fabian Stenzel
# 2022-2023 - stenzel@pik-potsdam.de

################# BioCol calc functions  ###################

#' Calculate BioCol based on a PNV run and LU run of LPJmL
#'
#' Function to calculate BioCol based on a PNV run and LU run of LPJmL
#' @param files_scenario list with variable names and corresponding file paths
#' (character string) of the scenario LPJmL run. All needed files are
#' provided in XXX. E.g.: list(leaching = "/temp/leaching.bin.json")
#' @param files_reference list with variable names and corresponding file paths
#' (character string) of the reference LPJmL run. All needed files are
#' provided in XXX. E.g.: list(leaching = "/temp/leaching.bin.json"). If not
#' needed for the applied method, set to NULL.
#' @param time_span_scenario time span to be used for the scenario run, defined
#' as a character string, e.g. `as.character(1982:2011)` (default)
#' @param time_span_reference time span to be used for the scenario run, defined
#' as an integer vector, e.g. `as.character(1901:1930)`. Can differ in offset
#' and length from `time_span_scenario`! If `NULL` value of `time_span_scenario`
#' is used
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
read_calc_biocol <- function( # nolint
  files_scenario,
  files_reference,
  time_span_scenario,
  time_span_reference = NULL,
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
  grass_harvest_file = "grazing_data.RData",
  external_fire_file = "human_ignition_fraction.RData",
  external_wood_harvest_file = "wood_harvest_biomass_sum_1500-2014_67420.RData"
) {
  if (is.null(time_span_reference)) time_span_reference <- time_span_scenario
  if (grass_scaling && !file.exists(grass_harvest_file)) {
    stop(
      paste0("Grass harvest scaling enabled, but grass_harvest_file does not exist in: ", # nolint
             grass_harvest_file)
    )
  }
  if (external_wood_harvest && !file.exists(external_wood_harvest_file)) {
    stop(
      paste0("External wood harvest enabled, but external_wood_harvest_file does not exist in: ", # nolint
             external_wood_harvest_file)
    )
  }
  if (external_fire && !file.exists(external_fire_file)) {
    stop(
      paste0("External fire fraction file enabled, but external_fire_file does not exist in: ", # nolint
             external_fire_file)
    )
  }
  # reading required data
  if (read_saved_data) {
    if (file.exists(data_file)) {
      print(paste0("Reading in data from previously saved data file"))
      load(data_file)
      wood_harvest[is.na(wood_harvest)] <- 0
    } else {
      stop(
        paste0("data_file: '",
               data_file,
               "' does not exist but is required since reading is set to FALSE."
               )
      )
    }
    if (save_data) {
      save_data <- FALSE
      print(
        paste0("Both read_saved_data and save_data have been set to TRUE. ",
            "Overwriting with the same data does not make sense, saving ",
            "disabled. ")
      )
    }
  } else {
    print("Reading in data from outputs")

    file_type <- tools::file_ext(files_reference$grid)

    if (file_type %in% c("json", "clm")) {
      # read grid
      grid <- lpjmlkit::read_io(
        files_reference$grid
      )
      # calculate cell area
      cellarea <- lpjmlkit::calc_cellarea(grid)
      lat <- grid$data[, , 2]
      lon <- grid$data[, , 1]

      npp <- lpjmlkit::read_io(
        files_scenario$npp,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop() # gC/m2

      if (!is.null(reference_npp_file)) {
        npp_ref <- lpjmlkit::read_io(
          reference_npp_file,
          subset = list(year = as.character(reference_npp_time_span))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum)
        ) %>% drop() # remaining bands
      }

      pftnpp <- lpjmlkit::read_io(
        files_scenario$pft_npp,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)
      )

      harvest <- lpjmlkit::read_io(
        files_scenario$pft_harvestc,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)
      )

      rharvest <- lpjmlkit::read_io(
        files_scenario$pft_rharvestc,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum))

      timber <- lpjmlkit::read_io(
        files_scenario$timber_harvestc,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)
      ) %>% drop() # remaining bands

      if (include_fire) {

        # read fire in monthly res. if possible, then multiply with monthly
        # human/total ignition frac and aggregate to yearly. Otherwise aggregate
        # human/total ignition frac to yearly and multiply with yearly firec
        fire_raw <- lpjmlkit::read_io(
          files_scenario$firec,
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(band = sum)
        ) # gC/m2

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
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)
      )

      npp_potential <- lpjmlkit::read_io(
        files_reference$npp,
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)
      ) %>% drop() # gC/m2

      fpc <- lpjmlkit::read_io(
        files_scenario$fpc,
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(band = sum)
      )

      pftbands <- lpjmlkit::read_meta(files_scenario$fpc)$nbands - 1

    } else if (file_type == "nc") { # to be added
      stop(
        "nc reading has not been updated to latest functionality.",
        " Please contact Fabian Stenzel"
      )

    } else {
      stop("Unrecognized file type (",
           file_type,
           ")")
    }

    bp_bands <- c(15, 16, 31, 32)
    grass_bands <- c(14, 30)
    nat_bands <- 1:pftbands

    if (!gridbased) { # needs to be scaled with standfrac
      pftnpp[, , nat_bands] <- pftnpp[, , nat_bands] * fpc[, , 2:(pftbands + 1)]
      pftnpp[, , -c(nat_bands)] <- pftnpp[, , -c(nat_bands)] * cftfrac
      harvest <- harvest * cftfrac
    }

    pftnpp_grasslands <- apply(
      pftnpp[, , pftbands + grass_bands],
      c(1, 2),
      sum
    ) #gC/m2 only from grassland bands

    pftnpp_cft <- apply(
      pftnpp[, , -c(nat_bands, pftbands + grass_bands, pftbands + bp_bands)],
      c(1, 2), sum
    ) #gC/m2 not from grassland and bioenergy bands

    pftnpp_bioenergy <- apply(
      pftnpp[, , pftbands + bp_bands],
      c(1, 2),
      sum
    ) #gC/m2 only from bioenergy bands

    pftnpp_nat <- apply(
      pftnpp[, , nat_bands], c(1, 2), sum) # gC/m2

    if (is.null(reference_npp_file)){
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
        print(paste0("Writing data file: ", data_file))
      } else {
        print(
          paste0(
            "Data file (",
            data_file,
            ") already exists, old file renamed to: ",
            data_file,
            "_sav")
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
           file = data_file)
    }
  }

  print(paste0("Calculating data"))

  if (grass_scaling) {
    load(grass_harvest_file)

    nregs <- length(grazing_data$name)

    lpj_grass_harvest_region <- array(0, dim = nregs)

    lpj_grass_harvest_2000 <- rowMeans(
      harvest_grasslands[, (1995 - start_year + 1) : (2005 - start_year + 1)]
    ) * cellarea / 1000 * 2 # from gC/m2 to kgDM

    grassland_scaling_factor_cellwise <- array(1, dim = grid$ncells)

    for (r in 1:nregs) {
      lpj_grass_harvest_region[r] <- sum(
        lpj_grass_harvest_2000[which(mapping_lpj67420_to_grazing_regions == r)]
      )
    }

    scaling_factor <- (
      grazing_data$Herrero_2000_kgDM_by_region / lpj_grass_harvest_region
    )

    for (r in 1:nregs) {
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
    biocol_overtime <- harvest_cft_overtime + rharvest_cft_overtime +
      harvest_grasslands_overtime + harvest_bioenergy_overtime +
      timber_harvest_overtime + fire_overtime + npp_luc_overtime +
      wood_harvest_overtime
  } else {
    biocol_overtime <- harvest_cft_overtime + rharvest_cft_overtime +
      harvest_grasslands_overtime + harvest_bioenergy_overtime +
      timber_harvest_overtime + npp_luc_overtime +
      wood_harvest_overtime
  }

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
  biocol_overtime_abs_frac_piref <- colSums(abs(biocol * cellarea)) / 
                mean(colSums(npp_ref * cellarea))

  return(list(biocol_overtime = biocol_overtime, #absolute 
              biocol_overtime_abs_frac_piref = biocol_overtime_abs_frac_piref,
              biocol_overtime_frac_piref = biocol_overtime_frac_piref,
              biocol_overtime_frac = biocol_overtime_frac,
              biocol = biocol,
              biocol_frac = biocol_frac,
              npp = npp,
              biocol_frac_piref = biocol_frac_piref,
              npp_potential = npp_potential,
              npp_act_overtime = npp_act_overtime,
              npp_pot_overtime = npp_pot_overtime,
              npp_eco_overtime = npp_eco_overtime,
              npp_ref = npp_ref,
              harvest_cft_overtime = harvest_cft_overtime,
              npp_luc_overtime = npp_luc_overtime,
              rharvest_cft_overtime = rharvest_cft_overtime,
              fire_overtime = fire_overtime,
              timber_harvest_overtime = timber_harvest_overtime,
              harvest_cft = harvest_cft,
              rharvest_cft = rharvest_cft,
              wood_harvest_overtime = wood_harvest_overtime,
              biocol_harvest = biocol_harvest,
              biocol_luc = biocol_luc)) #, biocol_luc_piref = biocol_luc_piref))

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
  grass_harvest_file = "grazing_data.RData",
  external_fire_file = "human_ignition_fraction.RData",
  external_wood_harvest_file = "wood_harvest_biomass_sum_1500-2014_67420.RData"
) {
  if (is.null(varnames)) {
    print(
      paste0("Varnames not given, using standard values, which might not fit ",
             "this specific configuration. Please check!")
    )
    varnames <- data.frame(
      row.names = c(
        "grid",
        "npp",
        "pft_npp",
        "pft_harvest",
        "pft_rharvest",
        "firec",
        "timber_harvest",
        "cftfrac",
        "fpc"
      ),
      outname = c(
        "grid.bin.json",
        "mnpp.bin.json",
        "pft_npp.bin.json",
        "pft_harvest.bin.json",
        "pft_rharvest.bin.json",
        "firec.bin.json",
        "timber_harvestc.bin.json",
        "cftfrac.bin.json",
        "fpc.bin.json"
      ),
      timestep = c("Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y")
    )
  }

  # translate varnames and folders to files_scenarios/reference lists
  files_scenario <- list(
    grid = paste0(path_lu, varnames["grid", "outname"]),
    npp = paste0(path_lu, varnames["npp", "outname"]),
    pft_npp = paste0(path_lu, varnames["pft_npp", "outname"]),
    pft_harvestc = paste0(path_lu, varnames["pft_harvest", "outname"]),
    pft_rharvestc = paste0(path_lu,varnames["pft_rharvest", "outname"]),
    firec = paste0(path_lu, varnames["firec", "outname"]),
    timber_harvestc = paste0(path_lu, varnames["timber_harvest", "outname"]),
    cftfrac = paste0(path_lu, varnames["cftfrac", "outname"]),
    fpc = paste0(path_lu, varnames["fpc", "outname"])
  )
  files_reference <- list(
    grid = paste0(path_pnv, varnames["grid", "outname"]),
    npp = paste0(path_pnv, varnames["npp", "outname"]),
    pft_npp = paste0(path_pnv, varnames["pft_npp", "outname"]),
    pft_harvestc = paste0(path_pnv, varnames["pft_harvest", "outname"]),
    pft_rharvestc = paste0(path_pnv, varnames["pft_rharvest", "outname"]),
    firec = paste0(path_pnv, varnames["firec", "outname"]),
    timber_harvestc = paste0(path_pnv, varnames["timber_harvest", "outname"]),
    cftfrac = paste0(path_pnv, varnames["cftfrac", "outname"]),
    fpc = paste0(path_pnv, varnames["fpc", "outname"])
  )
  return(
    read_calc_biocol(
      files_scenario = files_scenario,
      files_reference = files_reference,
      time_span_scenario = as.character(start_year:stop_year),
      time_span_reference = as.character(start_year:stop_year),
      reference_npp_time_span = reference_npp_time_span,
      reference_npp_file = reference_npp_file,
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


#' Plot absolute BioCol, overtime, maps, and npp into given folder
#'
#' Wrapper function to plot absolute biocol, overtime, maps, and npp into given
#' folder
#'
#' @param biocol_data biocol data list object (returned from calc_biocol)
#' containing biocol, npp_eco_overtime, npp_act_overtime, npp_pot_overtime,
#' npp_bioenergy_overtime, biocol_overtime, npp_harv_overtime,
#' biocol_overtime_perc_piref, biocol_perc, biocol_perc_piref, npp all in GtC
#' @param path_write folder to write into
#' @param plotyears range of years to plot over time
#' @param min_val y-axis minimum value for plot over time
#' @param max_val y-axis maximum value for plot over time
#' @param legendpos position of legend
#' @param start_year first year of biocol_data object
#' @param mapyear year to plot biocol map for
#' @param mapyear_buffer +- years around mapyear to average biocol
#' (make sure these years exist in biocol_data)
#' @param highlightyear year(s) that should be highlighted in overtime plot
#' @param eps write plots as eps, instead of png (default = FALSE)
#'
#' @return none
#' @export
plot_biocol <- function(
  biocol_data,
  path_write,
  plotyears,
  min_val,
  max_val,
  legendpos,
  start_year,
  mapyear,
  mapyear_buffer = 5,
  highlightyear,
  eps = FALSE
) {
  mapindex <- mapyear - start_year
  print(paste0("Plotting BioCol figures"))
  dir.create(file.path(path_write), showWarnings = FALSE)

  plot_global(
    data = rowMeans(
      biocol_data$biocol[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "BioCol_absolute_", mapyear, ".png"),
    type = "exp",
    title = "",
    # paste0("BioCol_abs in ", mapyear),
    pow2min = 0,
    pow2max = 12,
    legendtitle = "GtC",
    leg_yes = TRUE,
    only_pos = FALSE,
    eps = eps
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_luc[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "BioCol_luc_", mapyear, ".png"),
    type = "exp",
    title = "",
    # paste0("BioCol_luc in ", mapyear),
    pow2min = 0,
    pow2max = 12,
    legendtitle = "GtC",
    leg_yes = TRUE,
    only_pos = FALSE,
    eps = eps
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_harvest[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "BioCol_harv_", mapyear, ".png"),
    type = "exp",
    title = "",
    # paste0("BioCol_harv in ", mapyear), 
    pow2min = 0,
    pow2max = 12,
    legendtitle = "GtC",
    leg_yes = TRUE,
    only_pos = FALSE,
    eps = eps
  )

  plot_biocol_ts(
    biocol_data = biocol_data,
    file = paste0(
      path_write, "BioCol_overtime_LPJmL_", plotyears[1], "-", plotyears[2], ".png" # nolint
    ),
    first_year = start_year,
    plot_years = plotyears,
    min_val = min_val,
    ref = "pi",
    legendpos = legendpos,
    max_val = max_val,
    eps = eps,
    highlight_years = highlightyear
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_frac[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "BioCol_frac_LPJmL_", mapyear, ".png"),
    legendtitle = "frac of NPPpot",
    type = "lin",
    min=-1,
    max=1,
    col_pos = "Reds",
    col_neg = "Blues",
    leg_yes = TRUE,
    eps = FALSE,
    n_legend_ticks = 11
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_frac_piref[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "BioCol_frac_piref_LPJmL_", mapyear, ".png"),
    title = "",
    legendtitle = "frac of NPPref",
    type = "lin",
    min=-1,
    max=1,
    col_pos = "Reds",
    col_neg = "Blues",
    leg_yes = TRUE,
    eps = FALSE,
    n_legend_ticks = 11
  )

  plot_global(
    data = rowMeans(
      biocol_data$npp[, (mapindex - mapyear_buffer) : (mapindex + mapyear_buffer)] # nolint
    ),
    file = paste0(path_write, "NPP_LPJmL_", mapyear, ".png"),
    type = "lin",
    only_pos = TRUE,
    title = "",
    legendtitle = "gC/m2",
    leg_yes = TRUE,
    min = 0,
    max = 1800
  )
}


#' Plot global map of BioCol to file
#'
#' Plot global map of BioCol to file with legend colors similar to
#' Haberl et al. 2007
#'
#' @param data array containing BioCol percentage value for each gridcell
#' @param file character string for location/file to save plot to
#' @param plotyears range of years to plot over time
#' @param title character string title for plot
#' @param legendtitle character string legend title
#' @param zero_threshold smallest value to be distinguished from 0 in legend,
#'        both for negative and positive values (default: 0.1)
#' @param eps write eps file instead of PNG (boolean) - (default: FALSE)
#'
#' @return none
#'
#' @export
plot_biocol_map <- function(
  data, file,
  title = "",
  legendtitle = "",
  zero_threshold = 0.001,
  eps = FALSE,
  haberllegend = FALSE
) {
  path_write <- dirname(file)
  dir.create(file.path(path_write), showWarnings = FALSE)

  if (haberllegend){
    brks <- c(-400, -200, -100, -50, -zeroThreshold,
              zeroThreshold, 10, 20, 30, 40, 50, 60, 70, 80, 100)
    classes <- c("<-200", "-200 - -100", "-100 - -50",
                 paste0("-50 - -",zeroThreshold),
                 paste0("-",zeroThreshold," - ",zeroThreshold),
                 paste0(zeroThreshold," - 10"), "10 - 20", "20 - 30", "30 - 40", 
                 "40 - 50", "50 - 60", "60 - 70", "70 - 80", "80 - 100")
    palette <- c("navy", "royalblue3", "royalblue1", "skyblue1",
                 "grey80", "yellowgreen", "greenyellow", "yellow",
                 "gold", "orange", "orangered", "orangered4",
                 "brown4", "black")
  } else{
    brks <- c(-400,seq(-100,-10,10),-zeroThreshold,
              zeroThreshold,seq(10,100,10),400)/100
    classes <- c("<-1", "-1 - -0.9", "-0.9 - -0.8", "-0.8 - -0.7", 
                 "-0.7 - -0.6", "-0.6 - -0.5", "-0.5 - -0.4", "-0.4 - -0.3", 
                 "-0.3 - -0.2", "-0.2 - -0.1",paste("-0.1 - -",zeroThreshold),
                 paste("-",zeroThreshold," - ",zeroThreshold),
                 paste(zeroThreshold," - 0.1"),"0.1 - 0.2", "0.2 - 0.3", 
                 "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                 "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1", ">1")    
    palette <- grDevices::colorRampPalette(rev(
               RColorBrewer::brewer.pal(11,"RdBu")))(length(brks)-1)
  }

  data[data < brks[1]] <- brks[1]
  data[data > brks[length(brks)]] <- brks[length(brks)]

  if (eps) {
    file <- strsplit(file, ".", fixed = TRUE)[[1]]
    file <- paste(c(file[1 : (length(file) - 1)], "eps"), collapse = ".")
    grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
    grDevices::postscript(file, horizontal = FALSE, onefile = FALSE, width = 22,
                          height = 8.5, paper = "special")

  } else {
    grDevices::png(file, width = 7.25, height = 3.5, units = "in", res = 300,
                   pointsize = 6, type = "cairo")
  }
  ra <- raster::raster(ncols = 720, nrows = 360)
  range <- range(data)
  ra[raster::cellFromXY(ra, cbind(lon, lat))] <-  data
  extent <- raster::extent(c(-180, 180, -60, 90))
  graphics::par(bty = "n", oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), xpd = TRUE)
  raster::plot(ra, ext = extent, breaks = brks, col = palette, main = "",
               legend = FALSE, axes = FALSE)
  graphics::title(title, line = -2)
  maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))
  graphics::legend(x = -180, y = 50, fill = palette, border = palette,
                   legend = classes, title = legendtitle)
  grDevices::dev.off()
}

#' Plot absolute BioCol, overtime, maps, and npp into given folder
#'
#' Plot to file a comparison over time of global sums of BioCol, NPPpot, NPPeco,
#' and NPPact, with legend similar to Krausmann et al. 2013
#'
#' @param biocol_data biocol data list object (returned from calc_biocol)
#' containing biocol, npp_eco_overtime, npp_act_overtime, npp_pot_overtime,
#' npp_bioenergy_overtime, biocol_overtime, npp_harv_overtime,
#' biocol_overtime_perc_piref, biocol_perc, biocol_perc_piref, npp
#' all in GtC
#' @param file character string for location/file to save plot to
#' @param first_year first year of biocol object
#' @param plot_years range of years to plot over time
#' @param highlight_years year(s) that should be highlighted in overtime plot
#' (default: 2000)
#' @param min_val y-axis minimum value for plot over time (default: 0)
#' @param max_val y-axis maximum value for plot over time (default: 100)
#' @param legendpos position of legend (default: "topleft")
#' @param highlight_years year(s) that should be highlighted in overtime plot
#' (default: 2000)
#' @param ref reference period for biocol ("pi" or "act"), to either use
#'        biocol_data$biocol_overtime_perc_piref or biocol_data$biocol_overtime
#' @param eps write plots as eps, instead of png (default = FALSE)
#'
#' @return none
#'
#' @export
plot_biocol_ts <- function(
  biocol_data,
  file,
  first_year,
  plot_years,
  highlight_years = 2000,
  min_val = 0,
  max_val = 100,
  legendpos = "topleft",
  ext = FALSE,
  eps = FALSE,
  ref = "pi"
) {
  path_write <- dirname(file)
  dir.create(file.path(path_write), showWarnings = FALSE)

  last_year <- first_year + length(biocol_data$npp_act_overtime) - 1
  colz <- c("slateblue", "gold", "green3", "darkorange", "black",
            "red3", "green", "brown", "yellow", "turquoise",
            "darkgreen")

  if (eps) {
    file <- strsplit(file, ".", fixed = TRUE)[[1]]
    file <- paste(c(file[1 : (length(file) - 1)], "eps"), collapse = ".")
    grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
    grDevices::postscript(file, horizontal = FALSE, onefile = FALSE, width = 22,
                          height = 8.5, paper = "special")

  } else {
    grDevices::png(file, width = 3.5, height = 3, units = "in", res = 300,
                   pointsize = 6, type = "cairo")
  }

  graphics::par(bty = "o", oma = c(0, 0, 0, 0), mar = c(4, 5, 1, 3))
  graphics::plot(NA, ylab = "GtC/yr", xlab = "Year", xlim = plot_years,
       ylim = c(min_val, max_val), xaxs = "i", yaxs = "i")
  graphics::grid()
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$npp_pot_overtime,
    type = "l",
    col = colz[1]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$npp_act_overtime,
    type = "l",
    col = colz[2])
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$npp_eco_overtime,
    type = "l",
    col = colz[3]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$npp_luc_overtime,
    type = "l",
    col = colz[4]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$biocol_overtime,
    type = "l",
    col = colz[5]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$harvest_cft_overtime,
    type = "l",
    col = colz[7]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$rharvest_cft_overtime,
    type = "l",
    col = colz[8]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$fire_overtime,
    type = "l", col = colz[9]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$timber_harvest_overtime,
    type = "l",
    col = colz[10]
  )
  graphics::lines(
    x = seq(first_year, last_year, 1),
    y = biocol_data$wood_harvest_overtime,
    type = "l",
    col = colz[11]
  )

  graphics::par(bty = "n", oma = c(0, 0, 0, 0), mar = c(4, 5, 1, 3), new = TRUE)
  if (ref == "pi") {
    graphics::plot(
      x = seq(first_year, last_year, 1),
      y = biocol_data$biocol_overtime_abs_frac_piref,
      ylab = "",
      xlab = "",
      xlim = plot_years,
      ylim = c(0, 0.4),
      type = "l",
      col = colz[6],
      xaxs = "i",
      yaxs = "i",
      axes = FALSE
    )
  } else if (ref == "act") {
    graphics::plot(
      x = seq(first_year, last_year, 1),
      y = biocol_data$biocol_overtime,
      ylab = "",
      xlab = "",
      xlim = plot_years,
      ylim = c(0, 0.4),
      type = "l",
      col = colz[6],
      xaxs = "i",
      yaxs = "i",
      axes = FALSE
    )
  }else {
    stop(paste0("Unknown value for parameter ref: ", ref, " - Aborting."))
  }

  graphics::axis(side = 4, col = colz[6], col.axis = colz[6])
  graphics::mtext(text = "fraction of NPPref", col = colz[6], side = 4, line = 2)

  if (!is.null(highlight_years)) {
    for (y in highlight_years) {
      lines(x = c(y, y), y = c(min_val, max_val), col = "grey40")
    }
  }

  graphics::legend(
    legendpos,
    legend = c(
      "NPPpot (PNV)", "NPPact (landuse)", "NPPeco", "NPPluc", "HANPP",
      "BioCol [frac NPPref]", "harvestc", "rharvest", "firec", "timber_harvest",
      "wood_harvest"
    ), col = colz, lty = 1, cex = 1)
  grDevices::dev.off()
}

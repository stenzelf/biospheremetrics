# written by Fabian Stenzel, based on work by Sebastian Ostberg
# 2022-2023 - stenzel@pik-potsdam.de

################# EcoRisk calc functions  ###################

#' Wrapper for calculating the ecosystem change metric EcoRisk
#'
#' Function to read in data for ecorisk, and call the calculation function once,
#' if overtime is FALSE, or for each timeslice of length window years, if
#' overtime is TRUE
#'
#' @param path_ref folder of reference run
#' @param path_scen folder of scenario run
#' @param read_saved_data whether to read in previously saved data
#'        (default: FALSE)
#' @param save_data file to save read in data to (default NULL)
#' @param save_ecorisk file to save EcoRisk data to (default NULL)
#' @param nitrogen include nitrogen outputs for pools and fluxes into EcoRisk
#'        calculation (default FALSE)
#' @param weighting apply "old" (Ostberg-like), "new", or "equal" weighting of
#'        vegetation_structure_change weights (default "equal")
#' @param time_span_reference vector of years to use as scenario period
#' @param time_span_scenario vector of years to use as scenario period
#' @param dimensions_only_local flag whether to use only local change component
#'        for water/carbon/nitrogen fluxes and pools, or use an average of
#'        local change, global change and ecosystem balance (default FALSE)
#' @param overtime logical: calculate ecorisk as time-series? (default: FALSE)
#' @param window integer, number of years for window length (default: 30)
#' @param debug write out all nitrogen state variables (default FALSE)
#' @param suppressWarnings suppress warnings - default: TRUE
#'
#' @return list data object containing arrays of ecorisk_total,
#'         vegetation_structure_change, local_change, global_importance,
#'         ecosystem_balance, carbon_stocks, carbon_fluxes, water_fluxes
#'         (+ nitrogen_stocks and nitrogen_fluxes)
#'
#' @examples
#' \dontrun{
#' ecorisk_wrapper(
#'   path_ref = pnv_folder,
#'   path_scen = run_folder,
#'   read_saved_data = FALSE,
#'   nitrogen = TRUE,
#'   save_data = NULL,
#'   save_ecorisk = NULL,
#'   time_span_reference = c(1550:1579),
#'   time_span_scenario = c(1987:2016)
#'   )
#' }
#'
#' @md
#' @export
ecorisk_wrapper <- function(path_ref,
                            path_scen,
                            read_saved_data = FALSE,
                            save_data = NULL,
                            save_ecorisk = NULL,
                            nitrogen = TRUE,
                            weighting = "equal",
                            time_span_reference,
                            time_span_scenario,
                            dimensions_only_local = FALSE,
                            overtime = FALSE,
                            window = 30,
                            debug = FALSE,
                            external_variability = FALSE,
                            c2vr = NULL,
                            suppressWarnings = TRUE) {
  # check timespan consistency
  nyears <- length(time_span_reference)
  nyears_scen <- length(time_span_scenario)
  if ((!nyears == window) || nyears_scen < window) {
    stop(
      "Timespan in reference is not equal to window size (",
      window,
      "), or scenario timespan is smaller than window size."
    )
  }

  # translate output names (from metric_files.yml) and folders to files_scenarios/reference lists
  metric_files <- system.file(
    "extdata",
    "metric_files.yml",
    package = "biospheremetrics"
  ) %>%
    yaml::read_yaml()

  file_extension <- get_major_file_ext(paste0(path_scen))
  outputs <- metric_files$metric$ecorisk_nitrogen$output

  files_scenario <- list(
    grid = paste0(path_scen, outputs$grid$name, ".", file_extension),
    terr_area = paste0(path_scen, outputs$terr_area$name, ".", file_extension),
    fpc = paste0(path_scen, outputs$fpc$name, ".", file_extension),
    fpc_bft = paste0(path_scen, outputs$fpc_bft$name, ".", file_extension),
    cftfrac = paste0(path_scen, outputs$cftfrac$name, ".", file_extension),
    firec = paste0(path_scen, outputs$firec$name, ".", file_extension),
    npp = paste0(path_scen, outputs$npp$name, ".", file_extension),
    runoff = paste0(path_scen, outputs$runoff$name, ".", file_extension),
    transp = paste0(path_scen, outputs$transp$name, ".", file_extension),
    vegc = paste0(path_scen, outputs$vegc$name, ".", file_extension),
    firef = paste0(path_scen, outputs$firef$name, ".", file_extension),
    harvestc = paste0(path_scen, outputs$harvestc$name, ".", file_extension),
    evap = paste0(path_scen, outputs$evap$name, ".", file_extension),
    interc = paste0(path_scen, outputs$interc$name, ".", file_extension),
    soilc = paste0(path_scen, outputs$soilc$name, ".", file_extension),
    litc = paste0(path_scen, outputs$litc$name, ".", file_extension),
    swc = paste0(path_scen, outputs$swc$name, ".", file_extension),
    swc_vol = paste0(path_scen, outputs$swc_vol$name, ".", file_extension),
    swe = paste0(path_scen, outputs$swe$name, ".", file_extension),
    vegn = paste0(path_scen, outputs$vegn$name, ".", file_extension),
    soilnh4 = paste0(path_scen, outputs$soilnh4$name, ".", file_extension),
    soilno3 = paste0(path_scen, outputs$soilno3$name, ".", file_extension),
    leaching = paste0(path_scen, outputs$leaching$name, ".", file_extension),
    n2o_denit = paste0(path_scen, outputs$n2o_denit$name, ".", file_extension),
    n2o_nit = paste0(path_scen, outputs$n2o_nit$name, ".", file_extension),
    n2_emis = paste0(path_scen, outputs$n2_emis$name, ".", file_extension),
    bnf = paste0(path_scen, outputs$bnf$name, ".", file_extension),
    n_volatilization = paste0(path_scen, outputs$n_volatilization$name, ".", file_extension),
    gpp = paste0(path_scen, outputs$gpp$name, ".", file_extension),
    res_storage = paste0(path_scen, outputs$res_storage$name, ".", file_extension),
    lakevol = paste0(path_scen, outputs$lakevol$name, ".", file_extension),
    prec = paste0(path_scen, outputs$prec$name, ".", file_extension),
    irrig = paste0(path_scen, outputs$irrig$name, ".", file_extension),
    nfert_agr = paste0(path_scen, outputs$nfert_agr$name, ".", file_extension),
    nmanure_agr = paste0(path_scen, outputs$nmanure_agr$name, ".", file_extension),
    ndepos = paste0(path_scen, outputs$ndepos$name, ".", file_extension),
    firen = paste0(path_scen, outputs$firen$name, ".", file_extension),
    harvestn = paste0(path_scen, outputs$harvestn$name, ".", file_extension),
    irrig_stor = paste0(path_scen, outputs$irrig_stor$name, ".", file_extension),
    rivervol = paste0(path_scen, outputs$rivervol$name, ".", file_extension)
  )
  files_reference <- list(
    grid = paste0(path_ref, outputs$grid$name, ".", file_extension),
    terr_area = paste0(path_ref, outputs$terr_area$name, ".", file_extension),
    fpc = paste0(path_ref, outputs$fpc$name, ".", file_extension),
    fpc_bft = paste0(path_ref, outputs$fpc_bft$name, ".", file_extension),
    cftfrac = paste0(path_ref, outputs$cftfrac$name, ".", file_extension),
    firec = paste0(path_ref, outputs$firec$name, ".", file_extension),
    npp = paste0(path_ref, outputs$npp$name, ".", file_extension),
    runoff = paste0(path_ref, outputs$runoff$name, ".", file_extension),
    transp = paste0(path_ref, outputs$transp$name, ".", file_extension),
    vegc = paste0(path_ref, outputs$vegc$name, ".", file_extension),
    firef = paste0(path_ref, outputs$firef$name, ".", file_extension),
    harvestc = paste0(path_ref, outputs$harvestc$name, ".", file_extension),
    evap = paste0(path_ref, outputs$evap$name, ".", file_extension),
    interc = paste0(path_ref, outputs$interc$name, ".", file_extension),
    soilc = paste0(path_ref, outputs$soilc$name, ".", file_extension),
    litc = paste0(path_ref, outputs$litc$name, ".", file_extension),
    swc = paste0(path_ref, outputs$swc$name, ".", file_extension),
    swc_vol = paste0(path_ref, outputs$swc_vol$name, ".", file_extension),
    swe = paste0(path_ref, outputs$swe$name, ".", file_extension),
    vegn = paste0(path_ref, outputs$vegn$name, ".", file_extension),
    soilnh4 = paste0(path_ref, outputs$soilnh4$name, ".", file_extension),
    soilno3 = paste0(path_ref, outputs$soilno3$name, ".", file_extension),
    leaching = paste0(path_ref, outputs$leaching$name, ".", file_extension),
    n2o_denit = paste0(path_ref, outputs$n2o_denit$name, ".", file_extension),
    n2o_nit = paste0(path_ref, outputs$n2o_nit$name, ".", file_extension),
    n2_emis = paste0(path_ref, outputs$n2_emis$name, ".", file_extension),
    bnf = paste0(path_ref, outputs$bnf$name, ".", file_extension),
    n_volatilization = paste0(path_ref, outputs$n_volatilization$name, ".", file_extension),
    gpp = paste0(path_ref, outputs$gpp$name, ".", file_extension),
    res_storage = paste0(path_ref, outputs$res_storage$name, ".", file_extension),
    lakevol = paste0(path_ref, outputs$lakevol$name, ".", file_extension),
    prec = paste0(path_ref, outputs$prec$name, ".", file_extension),
    irrig = paste0(path_ref, outputs$irrig$name, ".", file_extension),
    nfert_agr = paste0(path_ref, outputs$nfert_agr$name, ".", file_extension),
    nmanure_agr = paste0(path_ref, outputs$nmanure_agr$name, ".", file_extension),
    ndepos = paste0(path_ref, outputs$ndepos$name, ".", file_extension),
    firen = paste0(path_ref, outputs$firen$name, ".", file_extension),
    harvestn = paste0(path_ref, outputs$harvestn$name, ".", file_extension),
    irrig_stor = paste0(path_ref, outputs$irrig_stor$name, ".", file_extension),
    rivervol = paste0(path_ref, outputs$rivervol$name, ".", file_extension)
  )

  if (overtime && (window != nyears)) stop("Overtime is enabled, but window \
                  length (", window, ") does not match the reference nyears.")

  if (read_saved_data) {
    if (!is.null(save_data)) {
      message("Loading saved data from:", save_data)
      load(file = save_data)
    } else {
      stop(
        "save_data is not specified as parameter, ",
        "nothing to load ... exiting"
      )
    }
  } else {
    # first read in all lpjml output files required for computing EcoRisks
    returned_vars <- read_ecorisk_data(
      files_reference = files_reference,
      files_scenario = files_scenario,
      save_file = save_data,
      nitrogen = nitrogen,
      time_span_reference = time_span_reference,
      time_span_scenario = time_span_scenario,
      debug = debug,
      suppressWarnings = suppressWarnings
    )
    # extract variables from return list object and give them proper names
    state_ref <- returned_vars$state_ref
    state_scen <- returned_vars$state_scen
    fpc_ref <- returned_vars$fpc_ref
    fpc_scen <- returned_vars$fpc_scen
    bft_ref <- returned_vars$bft_ref
    bft_scen <- returned_vars$bft_scen
    cft_ref <- returned_vars$cft_ref
    cft_scen <- returned_vars$cft_scen
    lat <- returned_vars$lat
    lon <- returned_vars$lon
    cell_area <- returned_vars$cell_area
    rm(returned_vars)
  }

  ncells <- length(cell_area)
  slices <- (nyears_scen - window + 1)
  ecorisk <- list(
    ecorisk_total = array(0, dim = c(ncells, slices)),
    vegetation_structure_change = array(0, dim = c(ncells, slices)),
    local_change = array(0, dim = c(ncells, slices)),
    global_importance = array(0, dim = c(ncells, slices)),
    ecosystem_balance = array(0, dim = c(ncells, slices)),
    c2vr = array(0, dim = c(4, ncells, slices)),
    carbon_stocks = array(0, dim = c(ncells, slices)),
    carbon_fluxes = array(0, dim = c(ncells, slices)),
    carbon_total = array(0, dim = c(ncells, slices)),
    water_total = array(0, dim = c(ncells, slices)),
    water_fluxes = array(0, dim = c(ncells, slices)),
    nitrogen_stocks = array(0, dim = c(ncells, slices)),
    nitrogen_fluxes = array(0, dim = c(ncells, slices)),
    nitrogen_total = array(0, dim = c(ncells, slices)),
    lat = lat,
    lon = lon
  )
  for (y in seq_len(slices)) {
    message("Calculating time slice ", y, " of ", slices)
    returned <- calc_ecorisk(
      fpc_ref = fpc_ref,
      fpc_scen = fpc_scen[, , y:(y + window - 1)],
      bft_ref = bft_ref,
      bft_scen = bft_scen[, , y:(y + window - 1)],
      cft_ref = cft_ref,
      cft_scen = cft_scen[, , y:(y + window - 1)],
      state_ref = state_ref,
      state_scen = state_scen[, y:(y + window - 1), ],
      weighting = weighting,
      lat = lat,
      lon = lon,
      cell_area = cell_area,
      dimensions_only_local = dimensions_only_local,
      nitrogen = nitrogen,
      external_variability = external_variability,
      c2vr = c2vr
    )
    ecorisk$ecorisk_total[, y] <- returned$ecorisk_total
    ecorisk$vegetation_structure_change[, y] <- (
      returned$vegetation_structure_change
    )
    ecorisk$local_change[, y] <- returned$local_change
    ecorisk$global_importance[, y] <- returned$global_importance
    ecorisk$ecosystem_balance[, y] <- returned$ecosystem_balance
    ecorisk$c2vr[, , y] <- returned$c2vr
    ecorisk$carbon_stocks[, y] <- returned$carbon_stocks
    ecorisk$carbon_fluxes[, y] <- returned$carbon_fluxes
    ecorisk$carbon_total[, y] <- returned$carbon_total
    ecorisk$water_total[, y] <- returned$water_total
    ecorisk$water_fluxes[, y] <- returned$water_fluxes
    if (nitrogen) {
      ecorisk$nitrogen_stocks[, y] <- returned$nitrogen_stocks
      ecorisk$nitrogen_fluxes[, y] <- returned$nitrogen_fluxes
      ecorisk$nitrogen_total[, y] <- returned$nitrogen_total
    }
  }

  ############## export and save data if requested #############
  if (!(is.null(save_ecorisk))) {
    message("Saving EcoRisk data to: ", save_ecorisk)
    save(ecorisk, file = save_ecorisk)
  }
  #
  ###
  return(ecorisk)
}

#' Calculate the ecosystem change metric EcoRisk between 2 sets of states
#' This function is called by the wrapper function (ecorisk_wrapper), 
#' unless you know what you are doing, don't use this function directly.
#'
#' Function to calculate the ecosystem change metric EcoRisk, based on
#' gamma/vegetation_structure_change
#' work from Sykes (1999), Heyder (2011), and Ostberg (2015,2018).
#' This is a reformulated version in R, not producing 100% similar values
#' than the C/bash version from Ostberg et al. 2018, but similar the methodology
#'
#' @param fpc_ref reference run data for fpc
#' @param fpc_scen scenario run data for fpc
#' @param bft_ref reference run data for fpc_bft
#' @param bft_scen scenario run data for fpc_bft
#' @param cft_ref reference run data for cftfrac
#' @param cft_scen scenario run data for cftfrac
#' @param state_ref reference run data for state variables
#' @param state_scen scenario run data for state variables
#' @param weighting apply "old" (Ostberg-like), "new", or "equal" weighting of
#'        vegetation_structure_change weights (default "equal")
#' @param lat latitude array
#' @param lon longitude array
#' @param cell_area cellarea array
#' @param dimensions_only_local flag whether to use only local change component
#'        for water/carbon/nitrogen fluxes and pools, or use an average of
#'        local change, global change and ecosystem balance (default FALSE)
#' @param nitrogen include nitrogen outputs (default: TRUE)
#' @param external_variability include external change_to_variability_ratio?
#'        (default: FALSE)
#' @param c2vr list with external change_to_variability_ratios for each
#'        component (default: NULL)
#'
#' @return list data object containing arrays of ecorisk_total,
#'         vegetation_structure_change, local_change, global_importance,
#'         ecosystem_balance, carbon_stocks, carbon_fluxes, water_fluxes
#'         (+ nitrogen_stocks and nitrogen_fluxes)
#'
#' @export
calc_ecorisk <- function(fpc_ref,
                         fpc_scen,
                         bft_ref,
                         bft_scen,
                         cft_ref,
                         cft_scen,
                         state_ref,
                         state_scen,
                         weighting = "equal",
                         lat,
                         lon,
                         cell_area,
                         dimensions_only_local = FALSE,
                         nitrogen = TRUE,
                         external_variability = FALSE,
                         c2vr = NULL) {
  if (external_variability && is.null(c2vr)) {
    stop("external_variability enabled, but not supplied (c2vr). Aborting.")
  }
  di_ref <- dim(fpc_ref)
  di_scen <- dim(fpc_scen)
  ncells <- di_ref[1]
  nyears <- di_ref[3]
  if (di_ref[3] != di_scen[3]) {
    stop("Dimension year does not match between fpc_scen and fpc_ref.")
  }
  # calc vegetation_structure_change and variability of
  #   vegetation_structure_change within
  # reference period S(vegetation_structure_change,
  #   sigma_vegetation_structure_change)
  fpc_ref_mean <- apply(fpc_ref, c(1, 2), mean)
  bft_ref_mean <- apply(bft_ref, c(1, 2), mean)
  cft_ref_mean <- apply(cft_ref, c(1, 2), mean)


  sigma_vegetation_structure_change_ref_list <- array(
    0,
    dim = c(ncells, nyears)
  )
  # calculate for every year of the reference period,
  #   vegetation_structure_change between that year and the average reference
  #   period year
  # this gives the variability of vegetation_structure_change within the
  #   reference period
  for (y in seq_len(nyears)) {
    sigma_vegetation_structure_change_ref_list[, y] <- calc_delta_v( # nolint
      fpc_ref = fpc_ref_mean,
      fpc_scen = fpc_ref[, , y],
      bft_ref = bft_ref_mean,
      bft_scen = bft_ref[, , y],
      cft_ref = cft_ref_mean,
      cft_scen = cft_ref[, , y],
      weighting = weighting
    )
  }

  # calculate the std deviation over the reference period for each gridcell
  vegetation_structure_changesd <- apply(
    sigma_vegetation_structure_change_ref_list,
    c(1),
    stats::sd
  )

  # calculate vegetation_structure_change between average reference and average
  #   scenario period
  vegetation_structure_change <- calc_delta_v(
    fpc_ref = fpc_ref_mean,
    fpc_scen = apply(fpc_scen, c(1, 2), mean),
    bft_ref = bft_ref_mean,
    bft_scen = apply(bft_scen, c(1, 2), mean),
    cft_ref = cft_ref_mean,
    cft_scen = apply(cft_scen, c(1, 2), mean),
    weighting = weighting
  )
  #
  ####
  ############## calc EcoRisk components ################
  # dimensions in the state vector
  # 1 "vegetation_carbon_pool"
  # 2 "soil_carbon_pool"
  # 3 "carbon_influx"
  # 4 "carbon_outflux"
  # 5 "soil_water_pool"
  # 6 "water_influx"
  # 7 "water_outflux"
  # 8 "other"
  # 9 "vegetation_nitrogen_pool"
  # 10 "soil_mineral_nitrogen_pool"
  # 11 "nitrogen_influx"
  # 12 "nitrogen_outflux"

  delta_var <- s_change_to_var_ratio(
    vegetation_structure_change,
    vegetation_structure_changesd
  )
  nitrogen_dimensions <- c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")
  all_dimensions <- dimnames(state_scen)$class
  non_nitrogen_dimensions <- setdiff(all_dimensions, nitrogen_dimensions)
  if (nitrogen) {
    lc_raw <- calc_component(
      ref = state_ref,
      scen = state_scen,
      local = TRUE,
      cell_area = cell_area
    ) # local change

    gi_raw <- calc_component(
      ref = state_ref,
      scen = state_scen,
      local = FALSE,
      cell_area = cell_area
    ) # global importance

    eb_raw <- calc_ecosystem_balance(
      ref = state_ref,
      scen = state_scen
    ) # ecosystem balance
  } else {
    lc_raw <- calc_component(
      ref = state_ref[, , non_nitrogen_dimensions],
      scen = state_scen[, , non_nitrogen_dimensions],
      local = TRUE,
      cell_area = cell_area
    ) # local change

    gi_raw <- calc_component(
      ref = state_ref[, , non_nitrogen_dimensions],
      scen = state_scen[, , non_nitrogen_dimensions],
      local = FALSE,
      cell_area = cell_area
    ) # global importance

    eb_raw <- calc_ecosystem_balance(
      ref = state_ref[, , non_nitrogen_dimensions],
      scen = state_scen[, , non_nitrogen_dimensions]
    ) # ecosystem balance
  }
  if (dimensions_only_local == TRUE) {
    # carbon stocks (local change)
    cs <- calc_component(
      ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
      scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
      local = TRUE,
      cell_area = cell_area
    )$full
    # carbon fluxes (local change)
    cf <- calc_component(
      ref = state_ref[, , c("carbon_influx", "carbon_outflux")],
      scen = state_scen[, , c("carbon_influx", "carbon_outflux")],
      local = TRUE,
      cell_area = cell_area
    )$full
    # total carbon (local change)
    ct <- calc_component(
      ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
      scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
      local = TRUE,
      cell_area = cell_area
    )$full
    # water fluxes (local change)
    wf <- calc_component(
      ref = state_ref[, , c("water_influx", "water_outflux")],
      scen = state_scen[, , c("water_influx", "water_outflux")],
      local = TRUE,
      cell_area = cell_area
    )$full
    # total water (local change)
    wt <- calc_component(
      ref = state_ref[, , c("water_influx", "water_outflux", "soil_water_pool")],
      scen = state_scen[, , c("water_influx", "water_outflux", "soil_water_pool")],
      local = TRUE,
      cell_area = cell_area
    )$full

    # nitrogen stocks (local change)
    if (nitrogen) {
      ns <- calc_component(
        ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
        scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
        local = TRUE,
        cell_area = cell_area
      )$full
      # nitrogen fluxes (local change)
      nf <- calc_component(
        ref = state_ref[, , c("nitrogen_influx", "nitrogen_outflux")],
        scen = state_scen[, , c("nitrogen_influx", "nitrogen_outflux")],
        local = TRUE,
        cell_area = cell_area
      )$full
      # total nitrogen (local change)
      nt <- calc_component(
        ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
        scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
        local = TRUE,
        cell_area = cell_area
      )$full
    }
  } else { # local == FALSE
    cf <- (
      calc_component(
        ref = state_ref[, , c("carbon_influx", "carbon_outflux")],
        scen = state_scen[, , c("carbon_influx", "carbon_outflux")],
        local = TRUE,
        cell_area = cell_area
      )$full + # carbon fluxes
        calc_component(
          ref = state_ref[, , c("carbon_influx", "carbon_outflux")],
          scen = state_scen[, , c("carbon_influx", "carbon_outflux")],
          local = FALSE,
          cell_area = cell_area
        )$full +
        calc_ecosystem_balance(
          ref = state_ref[, , c("carbon_influx", "carbon_outflux")],
          scen = state_scen[, , c("carbon_influx", "carbon_outflux")]
        )$full
    ) / 3

    # carbon stocks
    cs <- (
      calc_component(
        ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
        scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
        local = TRUE,
        cell_area = cell_area
      )$full +
        calc_component(
          ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
          scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
          local = FALSE,
          cell_area = cell_area
        )$full +
        calc_ecosystem_balance(
          ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool")],
          scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool")]
        )$full
    ) / 3

    # carbon total
    ct <- (
      calc_component(
        ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
        scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
        local = TRUE,
        cell_area = cell_area
      )$full +
        calc_component(
          ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
          scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
          local = FALSE,
          cell_area = cell_area
        )$full +
        calc_ecosystem_balance(
          ref = state_ref[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")],
          scen = state_scen[, , c("vegetation_carbon_pool", "soil_carbon_pool", "carbon_influx", "carbon_outflux")]
        )$full
    ) / 3

    # water fluxes
    wf <- (
      calc_component(
        ref = state_ref[, , c("water_influx", "water_outflux")],
        scen = state_scen[, , c("water_influx", "water_outflux")],
        local = TRUE,
        cell_area = cell_area
      )$full +
        calc_component(
          ref = state_ref[, , c("water_influx", "water_outflux")],
          scen = state_scen[, , c("water_influx", "water_outflux")],
          local = FALSE,
          cell_area = cell_area
        )$full + calc_ecosystem_balance(
          ref = state_ref[, , c("water_influx", "water_outflux")],
          scen = state_scen[, , c("water_influx", "water_outflux")]
        )$full
    ) / 3

    # water total
    wt <- (
      calc_component(
        ref = state_ref[, , c("water_influx", "water_outflux", "soil_water_pool")],
        scen = state_scen[, , c("water_influx", "water_outflux", "soil_water_pool")],
        local = TRUE,
        cell_area = cell_area
      )$full +
        calc_component(
          ref = state_ref[, , c("water_influx", "water_outflux", "soil_water_pool")],
          scen = state_scen[, , c("water_influx", "water_outflux", "soil_water_pool")],
          local = FALSE,
          cell_area = cell_area
        )$full +
        calc_ecosystem_balance(
          ref = state_ref[, , c("water_influx", "water_outflux", "soil_water_pool")],
          scen = state_scen[, , c("water_influx", "water_outflux", "soil_water_pool")]
        )$full
    ) / 3

    if (nitrogen) {
      # nitrogen stocks (local change)
      ns <- (
        calc_component(
          ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
          scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
          local = TRUE,
          cell_area = cell_area
        )$full +
          calc_component(
            ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
            scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
            local = FALSE, cell_area = cell_area
          )$full +
          calc_ecosystem_balance(
            ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")],
            scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool")]
          )$full
      ) / 3

      # nitrogen fluxes (local change)
      nf <- (
        calc_component(
          ref = state_ref[, , c("nitrogen_influx", "nitrogen_outflux")],
          scen = state_scen[, , c("nitrogen_influx", "nitrogen_outflux")],
          local = TRUE,
          cell_area = cell_area
        )$full +
          calc_component(
            ref = state_ref[, , c("nitrogen_influx", "nitrogen_outflux")],
            scen = state_scen[, , c("nitrogen_influx", "nitrogen_outflux")],
            local = FALSE,
            cell_area = cell_area
          )$full +
          calc_ecosystem_balance(
            ref = state_ref[, , c("nitrogen_influx", "nitrogen_outflux")],
            scen = state_scen[, , c("nitrogen_influx", "nitrogen_outflux")]
          )$full
      ) / 3

      nt <- (
        calc_component(
          ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
          scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
          local = TRUE,
          cell_area = cell_area
        )$full +
          calc_component(
            ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
            scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
            local = FALSE,
            cell_area = cell_area
          )$full +
          calc_ecosystem_balance(
            ref = state_ref[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")],
            scen = state_scen[, , c("vegetation_nitrogen_pool", "soil_mineral_nitrogen_pool", "nitrogen_influx", "nitrogen_outflux")]
          )$full
      ) / 3
    }
  }
  if (external_variability) {
    delta <- vegetation_structure_change * c2vr["vs", ] # vegetation_structure_change
    lc <- lc_raw$value * c2vr["lc", ]
    gi <- gi_raw$value * c2vr["gi", ]
    eb <- eb_raw$value * c2vr["eb", ]
  } else {
    delta <- vegetation_structure_change * delta_var # vegetation_structure_change
    lc <- lc_raw$value * lc_raw$var
    gi <- gi_raw$value * gi_raw$var
    eb <- eb_raw$value * eb_raw$var
    c2vr <- rbind(delta_var, lc_raw$var, gi_raw$var, eb_raw$var) # dim=(4,ncells)
    dimnames(c2vr) <- list(component = c("vs", "lc", "gi", "eb"), cell = 0:(ncells - 1))
  }

  # calc total EcoRisk as the average of the 4 components
  ecorisk_full <- (delta + lc + gi + eb) / 4 # check for NAs

  if (nitrogen) {
    ecorisk <- list(
      ecorisk_total = ecorisk_full,
      vegetation_structure_change = delta,
      local_change = lc,
      global_importance = gi,
      ecosystem_balance = eb,
      c2vr = c2vr,
      carbon_stocks = cs,
      carbon_fluxes = cf,
      carbon_total = ct,
      water_fluxes = wf,
      water_stocks = NA,
      water_total = wt,
      nitrogen_stocks = ns,
      nitrogen_fluxes = nf,
      nitrogen_total = nt
    )
  } else {
    ecorisk <- list(
      ecorisk_total = ecorisk_full,
      vegetation_structure_change = delta,
      local_change = lc,
      global_importance = gi,
      ecosystem_balance = eb,
      c2vr = c2vr,
      carbon_stocks = cs,
      carbon_fluxes = cf,
      carbon_total = ct,
      water_fluxes = wf,
      water_stocks = NA,
      water_total = wt,
      nitrogen_stocks = NA,
      nitrogen_fluxes = NA,
      nitrogen_total = NA
    )
  }
  ###
  return(ecorisk)
}

#' Read in output data from LPJmL to calculate the ecosystem change metric
#' EcoRisk. This function is called by the wrapper function (ecorisk_wrapper), 
#' unless you know what you are doing, don't use this function directly.
#'
#' Utility function to read in output data from LPJmL for calculation of EcoRisk
#'
#' @param files_reference folder of reference run
#' @param files_scenario folder of scenario run
#' @param save_file file to save read in data to (default NULL)
#' @param time_span_reference vector of years to use as scenario period
#' @param time_span_scenario vector of years to use as scenario period
#' @param nitrogen include nitrogen outputs for pools and fluxes into EcoRisk
#'                 calculation (default FALSE)
#' @param debug write out all nitrogen state variables (default FALSE)
#' @param suppressWarnings suppress writing of Warnings, default: TRUE
#'
#' @return list data object containing arrays of state_ref, mean_state_ref,
#'         state_scen, mean_state_scen, fpc_ref, fpc_scen, bft_ref, bft_scen,
#'         cft_ref, cft_scen, lat, lon, cell_area
#'
#' @export
read_ecorisk_data <- function(
    files_reference, # nolint
    files_scenario,
    save_file = NULL,
    time_span_reference,
    time_span_scenario,
    nitrogen,
    debug = FALSE,
    suppressWarnings = TRUE) {
  file_type <- tools::file_ext(files_reference$grid)

  if (file_type %in% c("json", "clm")) {
    # read grid
    grid <- lpjmlkit::read_io(
      files_reference$grid
    )
    # calculate cell area
    cell_area <- drop(lpjmlkit::read_io(
      filename = files_reference$terr_area
    )$data) # in m2
    lat <- grid$data[, , 2]
    lon <- grid$data[, , 1]
    ncells <- length(lat)
    nyears <- length(time_span_scenario)

    ### read in lpjml output
    # for vegetation_structure_change (fpc,fpc_bft,cftfrac)
    message("Reading in fpc, fpc_bft, cftfrac")

    cft_scen <- aperm(lpjmlkit::read_io(
      files_scenario$cftfrac,
      subset = list(year = as.character(time_span_scenario))
    ) %>%
      lpjmlkit::transform(to = c("year_month_day")) %>%
      lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
      suppressWarnings()

    bft_scen <- aperm(lpjmlkit::read_io(
      files_scenario$fpc_bft,
      subset = list(year = as.character(time_span_scenario))
    ) %>%
      lpjmlkit::transform(to = c("year_month_day")) %>%
      lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
      suppressWarnings()

    fpc_scen <- aperm(lpjmlkit::read_io(
      files_scenario$fpc,
      subset = list(year = as.character(time_span_scenario))
    ) %>%
      lpjmlkit::transform(to = c("year_month_day")) %>%
      lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
      suppressWarnings()

    if (file.exists(files_reference$cftfrac)) {
      cft_ref <- aperm(lpjmlkit::read_io(
        files_reference$cftfrac,
        subset = list(year = as.character(time_span_reference))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
        suppressWarnings()
    } else {
      cft_ref <- cft_scen * 0
    }

    if (file.exists(files_reference$fpc_bft)) {
      bft_ref <- aperm(lpjmlkit::read_io(
        files_reference$fpc_bft,
        subset = list(year = as.character(time_span_reference))
      ) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
        suppressWarnings()
    } else {
      bft_ref <- bft_scen * 0
    }

    fpc_ref <- aperm(lpjmlkit::read_io(
      files_reference$fpc,
      subset = list(year = as.character(time_span_reference))
    ) %>%
      lpjmlkit::transform(to = c("year_month_day")) %>%
      lpjmlkit::as_array(aggregate = list(month = sum)), c(1, 3, 2)) %>%
      suppressWarnings()

    #### new input reading ###
    metric_files <- system.file(
      "extdata",
      "metric_files.yml",
      package = "biospheremetrics"
    ) %>%
      yaml::read_yaml()
    nclasses <- length(metric_files$metric$ecorisk_nitrogen$metric_class)
    nstate_dimensions <- 0
    for (i in seq_len(nclasses)) {
      nstate_dimensions <- nstate_dimensions +
        length(metric_files$metric$ecorisk_nitrogen$metric_class[[i]])
    }
    state_ref <- array(0, dim = c(ncells, nyears, nstate_dimensions))
    state_scen <- array(0, dim = c(ncells, nyears, nstate_dimensions))
    class_names <- seq_len(nstate_dimensions)
    index <- 1
    # iterate over main classes (carbon pools, water fluxes ...)
    for (c in seq_len(nclasses)) {
      classe <- metric_files$metric$ecorisk_nitrogen$metric_class[[c]]
      nsubclasses <- length(classe)
      # iterate over subclasses (vegetation carbon, soil water ...)
      for (s in seq_len(nsubclasses)) {
        subclass <- classe[s]
        class_names[index] <- names(subclass)
        vars <- split_sign(unlist(subclass))
        for (v in seq_len(length(vars[, 1]))) {
          path_scen_file <- files_scenario[[vars[v, "variable"]]]
          if (file.exists(path_scen_file)) {
            header_scen <- lpjmlkit::read_meta(filename = path_scen_file)
            message(
              "Reading in ", path_scen_file, " with unit ", header_scen$unit,
              " -> as part of ", class_names[index]
            )
            var_scen <- lpjmlkit::read_io(
              path_scen_file,
              subset = list(year = as.character(time_span_scenario))
            ) %>%
              lpjmlkit::transform(to = c("year_month_day")) %>%
              lpjmlkit::as_array(aggregate = list(month = sum, band = sum), ) %>%
              drop() %>%
              suppressWarnings()
          } else {
            stop(paste("Couldn't read in:", path_scen_file, " - stopping!"))
          }
          path_ref_file <- files_reference[[vars[v, "variable"]]]
          if (file.exists(path_ref_file)) {
            header_ref <- lpjmlkit::read_meta(path_ref_file)
            message(
              "Reading in ", path_ref_file, " with unit ", header_ref$unit,
              " -> as part of ", class_names[index]
            )
            var_ref <- lpjmlkit::read_io(
              path_ref_file,
              subset = list(year = as.character(time_span_reference))
            ) %>%
              lpjmlkit::transform(to = c("year_month_day")) %>%
              lpjmlkit::as_array(aggregate = list(month = sum, band = sum)) %>%
              drop() %>%
              suppressWarnings()
          } else {
            stop(paste("Couldn't read in:", path_ref_file, " - stopping!"))
          }
          # if (window > 30){
          #  if (vars[v,"sign"] == "+"){
          #    state_scen[,,index,] <- state_scen[,,index,] + var_scen
          #    state_ref[,,index,] <- state_ref[,,index,] + var_ref
          #  } else { # vars[v,"sign"] == "-"
          #    state_scen[,,index,] <- state_scen[,,index,] - var_scen
          #    state_ref[,,index,] <- state_ref[,,index,] - var_ref
          #  }
          # }else{
          if (vars[v, "sign"] == "+") {
            state_scen[, , index] <- state_scen[, , index] + var_scen
            state_ref[, , index] <- state_ref[, , index] + var_ref
          } else { # vars[v,"sign"] == "-"
            state_scen[, , index] <- state_scen[, , index] - var_scen
            state_ref[, , index] <- state_ref[, , index] - var_ref
          }
          # }
        }
        index <- index + 1
      }
    }

    dimnames(state_scen) <- list(cell = 0:(ncells - 1), year = as.character(time_span_scenario), class = class_names)
    dimnames(state_ref) <- list(cell = 0:(ncells - 1), year = as.character(time_span_reference), class = class_names)
  } else if (file_type == "nc") { # to be added
    stop(
      "nc reading has not been updated to latest functionality. ",
      "Please contact Fabian Stenzel"
    )
  } else {
    stop("Unrecognized file type (", file_type, ")")
  }

  if (!(is.null(save_file))) {
    message("Saving data to: ", save_file)
    save(state_ref, state_scen, fpc_ref, fpc_scen,
      bft_ref, bft_scen, cft_ref, cft_scen, lat, lon, cell_area,
      file = save_file
    )
  }
  return(
    list(
      state_ref = state_ref,
      state_scen = state_scen,
      fpc_ref = fpc_ref,
      fpc_scen = fpc_scen,
      bft_ref = bft_ref,
      bft_scen = bft_scen,
      cft_ref = cft_ref,
      cft_scen = cft_scen,
      lat = lat,
      lon = lon,
      cell_area = cell_area
    )
  )
}

#' Calculates changes in vegetation structure (vegetation_structure_change)
#'
#' Utility function to calculate changes in vegetation structure
#' (vegetation_structure_change) for calculation of EcoRisk
#'
#' @param fpc_ref reference fpc array (dim: [ncells,npfts+1])
#' @param fpc_scen scenario fpc array (dim: [ncells,npfts+1])
#' @param bft_ref reference bft array (dim: [ncells,nbfts])
#' @param bft_scen scenario bft array (dim: [ncells,nbfts])
#' @param cft_ref reference cft array (dim: [ncells,ncfts])
#' @param cft_scen scenario cft array (dim: [ncells,ncfts])
#' @param weighting apply "old" (Ostberg-like), "new", or "equal" weighting of
#'                  vegetation_structure_change weights (default "equal")
#'
#' @return vegetation_structure_change array of size ncells with the
#'         vegetation_structure_change value [0,1] for each cell
#'
#' @examples
#' \dontrun{
#' vegetation_structure_change <- calc_delta_v(
#'   fpc_ref = fpc_ref_mean,
#'   fpc_scen = apply(fpc_scen, c(1, 2), mean),
#'   bft_ref = bft_ref_mean,
#'   bft_scen = apply(bft_scen, c(1, 2), mean),
#'   cft_ref = cft_ref_mean,
#'   cft_scen = apply(cft_scen, c(1, 2), mean),
#'   weighting = "equal"
#' )
#' }
#' @export
calc_delta_v <- function(fpc_ref, # nolint
                         fpc_scen,
                         bft_ref,
                         bft_scen,
                         cft_ref,
                         cft_scen,
                         weighting = "equal") {
  di <- dim(fpc_ref)
  ncells <- di[1]
  npfts <- di[2] - 1

  fpc_ref[fpc_ref < 0] <- 0
  fpc_scen[fpc_scen < 0] <- 0
  bft_ref[bft_ref < 0] <- 0
  bft_scen[bft_scen < 0] <- 0
  cft_ref[cft_ref < 0] <- 0
  cft_scen[cft_scen < 0] <- 0

  if (npfts == 9) {
    # barren = 1 - crop area - natural vegetation area +
    #   barren under bioenergy trees
    barren_area_ref <- (
      1 - rowSums(cft_ref) -
        rowSums(fpc_ref[, 2:10]) * fpc_ref[, 1] +
        rowSums(cft_ref[, c(16, 32)]) * (1 - rowSums(bft_ref[, c(1:4, 7:10)]))
    )

    barren_area_ref[barren_area_ref < 0] <- 0

    tree_area_ref <- array(0, dim = c(ncells, 11))

    # natural tree area fractions scaled by total natural frac
    tree_area_ref[, 1:7] <- (
      fpc_ref[, 2:8] * fpc_ref[, 1]
    )

    # fraction of rainfed tropical and temperate BE trees scaled by total
    #   rainfed bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_ref[, 8:9] <- (
      cft_ref[, 16] * bft_ref[, 1:2] / rowSums(bft_ref[, c(1, 2, 4)])
    )

    # fraction of irrigated tropical and temperate BE trees scaled by total
    #   irrigated bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_ref[, 10:11] <- (
      cft_ref[, 32] * bft_ref[, 7:8] / rowSums(bft_ref[, c(7, 8, 10)])
    )

    grass_area_ref <- array(0, dim = c(ncells, 20))

    # natural grass
    grass_area_ref[, 1:2] <- fpc_ref[, 9:10] * fpc_ref[, 1]

    # crops
    grass_area_ref[, 3:15] <- cft_ref[, 1:13] + cft_ref[, 17:29]

    # managed grass rf
    grass_area_ref[, 16] <- cft_ref[, 14]

    # managed grass irr
    grass_area_ref[, 17] <- cft_ref[, 30]

    # bioenergy grass
    grass_area_ref[, 18] <- cft_ref[, 15] + cft_ref[, 31]

    # fraction of rainfed grass under bioenergy trees
    grass_area_ref[, 19] <- (
      cft_ref[, 16] * bft_ref[, 4] / rowSums(bft_ref[, c(1, 2, 4)])
    )

    # fraction of irrigated grass under bioenergy trees
    grass_area_ref[, 20] <- (
      cft_ref[, 32] * bft_ref[, 10] / rowSums(bft_ref[, c(7, 8, 10)])
    )

    # barren
    barren_area_scen <- (
      1 - rowSums(cft_scen) -
        rowSums(fpc_scen[, 2:10]) * fpc_scen[, 1] +
        rowSums(cft_scen[, c(16, 32)]) * (1 - rowSums(bft_scen[, c(1:4, 7:10)]))
    )

    barren_area_scen[barren_area_scen < 0] <- 0

    tree_area_scen <- array(0, dim = c(ncells, 11))

    # natural tree area fractions scaled by total natural frac
    tree_area_scen[, 1:7] <- (
      fpc_scen[, 2:8] * fpc_scen[, 1]
    )

    # fraction of rainfed tropical and temperate BE trees scaled by total
    #   rainfed bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_scen[, 8:9] <- (
      cft_scen[, 16] * bft_scen[, 1:2] / rowSums(bft_scen[, c(1, 2, 4)])
    )

    # fraction of irrigated tropical and temperate BE trees scaled by total
    #   irrigated bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_scen[, 10:11] <- (
      cft_scen[, 32] * bft_scen[, 7:8] / rowSums(bft_scen[, c(7, 8, 10)])
    )
    grass_area_scen <- array(0, dim = c(ncells, 20))

    # natural grass
    grass_area_scen[, 1:2] <- fpc_scen[, 9:10] * fpc_scen[, 1]

    # crops
    grass_area_scen[, 3:15] <- cft_scen[, 1:13] + cft_scen[, 17:29]

    # managed grass rf
    grass_area_scen[, 16] <- cft_scen[, 14]

    # managed grass irr
    grass_area_scen[, 17] <- cft_scen[, 30]

    # bioenergy grass
    grass_area_scen[, 18] <- cft_scen[, 15] + cft_scen[, 31]

    # fraction of rainfed grass under bioenergy trees
    grass_area_scen[, 19] <- (
      cft_scen[, 16] * bft_scen[, 4] / rowSums(bft_scen[, c(1, 2, 4)])
    )

    # fraction of irrigated grass under bioenergy trees
    grass_area_scen[, 20] <- (
      cft_scen[, 32] * bft_scen[, 10] / rowSums(bft_scen[, c(7, 8, 10)])
    )

    # evergreenness, needleleavedness, tropicalness, borealness, naturalness
    tree_attributes <- matrix(
      c(
        c(1, 0, 1, 0, 1), # 1 TrBE
        c(0, 0, 1, 0, 1), # 2 TrBR
        c(1, 1, 0, 0, 1), # 3 TeNE
        c(1, 0, 0, 0, 1), # 4 TeBE
        c(0, 0, 0, 0, 1), # 5 TeBS
        c(1, 1, 0, 1, 1), # 6 BoNE
        c(0, 0.25, 0, 1, 1), # 7 BoS (including larchs)
        c(1, 0, 1, 0, 0), # 8 TrBi tropical bioenergy rainfed
        c(0, 0, 0, 0, 0), # 9 TeBi temperate bioenergy rainfed
        c(1, 0, 1, 0, 0), # 10 TrBi tropical bioenergy irrigated
        c(0, 0, 0, 0, 0) # 11 TeBi temperate bioenergy irrigated
      ),
      nrow = 11,
      byrow = TRUE
    )

    if (weighting == "equal") {
      tree_weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)

      # changed compared to Sebastian Ostberg's method
    } else if (weighting == "new") {
      tree_weights <- c(0.2, 0.2, 0.3, 0.3, 0.3) / 1.3

      # Sebastian's method (no downscaling to weightsum 1)
    } else if (weighting == "old") {
      tree_weights <- c(0.2, 0.2, 0.3, 0.3, 0.3)
    } else {
      stop("Unknown method of weighting.")
    }

    grass_attributes <- array(0, dim = c(ncells, 20, 2))
    # 1 C3grass
    # 2 C4grass
    # 3 TemperateCereals
    # 4 Rice
    # 5 Maize
    # 6 TropicalCereals
    # 7 Pulses
    # 8 TemperateRoots
    # 9 TropicalRoots
    # 10 Sunflower
    # 11 Soybean
    # 12 Groundnut
    # 13 Rapeseed
    # 14 Sugarcane
    # 15 Others
    # 16 Managed grass rainfed
    # 17 Managed grass irrigated
    # 18 Bioenergy grass
    # 19 Grass under rainfed Bioenergy trees
    # 20 Grass under irrigated Bioenergy trees

    # tropicalness
    grass_attributes[, , 1] <- rep(
      c(0, 1, 0, 1, 1, 1, 0.5, 0, 1, 0.5, 1, 1, 0.5, 1, 0.5, NA, NA, 1, NA, NA),
      each = ncells
    )

    # naturalness
    grass_attributes[, , 2] <- rep(
      c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      each = ncells
    )

    # dynamic share of tropicalness for rf/irr grasslands taken from ratio of
    #   bioenergy grasses
    dyn_grass_attributes <- cbind(
      bft_scen[, 6] / rowSums(bft_scen[, 5:6]),
      bft_scen[, 12] / rowSums(bft_scen[, 11:12])
    )

    dyn_grass_attributes[!is.finite(dyn_grass_attributes)] <- 0

    # managed grass rf/irr
    grass_attributes[, 16:17, 1] <- dyn_grass_attributes

    # grass under biotrees rf/irr (taken from managed grass)
    grass_attributes[, 19:20, 1] <- dyn_grass_attributes

    if (weighting == "equal") {
      grass_weights <- c(0.2, 0.2)

      # changed compared to Sebastian Ostberg's method
    } else if (weighting == "new") {
      grass_weights <- c(0.5, 0.5)

      # Sebastian Ostbergs's method (no downscaling to weightsum 1)
    } else if (weighting == "old") {
      grass_weights <- c(0.3, 0.3)
    } else {
      stop("Unknown method of weighting.")
    }
  } else if (npfts == 11) {
    # barren = 1 - crop area - natural vegetation area +
    #   barren under bioenergy trees
    barren_area_ref <- (
      1 - rowSums(cft_ref) -
        rowSums(fpc_ref[, 2:12]) * fpc_ref[, 1] +
        rowSums(cft_ref[, c(16, 32)]) * (1 - rowSums(bft_ref[, c(4:9, 13:18)]))
    )

    barren_area_ref[barren_area_ref < 0] <- 0

    tree_area_ref <- array(0, dim = c(ncells, 12))

    # natural tree area fractions scaled by total natural frac
    tree_area_ref[, 1:8] <- fpc_ref[, 2:9] * fpc_ref[, 1]

    # fraction of rainfed tropical and temperate BE trees scaled by total
    #   rainfed bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_ref[, 9:10] <- (
      cft_ref[, 16] * bft_ref[, 7:8] / rowSums(bft_ref[, 4:8])
    )

    # fraction of irrigated tropical and temperate BE trees scaled by total
    #   irrigated bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_ref[, 11:12] <- (
      cft_ref[, 32] * bft_ref[, 16:17] / rowSums(bft_ref[, 13:17])
    )

    grass_area_ref <- array(0, dim = c(ncells, 21))

    # natural grass
    grass_area_ref[, 1:3] <- fpc_ref[, 10:12] * fpc_ref[, 1]

    # crops
    grass_area_ref[, 4:16] <- cft_ref[, 1:13] + cft_ref[, 17:29]

    # managed grass rf
    grass_area_ref[, 17] <- cft_ref[, 14]

    # managed grass irr
    grass_area_ref[, 18] <- cft_ref[, 30]

    # bioenergy grass
    grass_area_ref[, 19] <- cft_ref[, 15] + cft_ref[, 31]

    # fraction of rainfed grass under bioenergy trees
    grass_area_ref[, 20] <- (
      cft_ref[, 16] * rowSums(bft_ref[, 4:6]) / rowSums(bft_ref[, 4:8])
    )

    # fraction of irrigated grass under bioenergy trees
    grass_area_ref[, 21] <- (
      cft_ref[, 32] * rowSums(bft_ref[, 13:15]) / rowSums(bft_ref[, 13:17])
    )

    # barren = 1 - crop area - natural vegetation area +
    #   barren under bioenergy trees
    barren_area_scen <- (
      1 - rowSums(cft_scen) -
        rowSums(fpc_scen[, 2:12]) * fpc_scen[, 1] +
        rowSums(cft_scen[, c(16, 32)]) * (1 - rowSums(bft_scen[, c(4:9, 13:18)]))
    )

    barren_area_scen[barren_area_scen < 0] <- 0

    tree_area_scen <- array(0, dim = c(ncells, 12))

    # natural tree area fractions scaled by total natural frac
    tree_area_scen[, 1:8] <- fpc_scen[, 2:9] * fpc_scen[, 1]

    # fraction of rainfed tropical and temperate BE trees scaled by total
    #   rainfed bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_scen[, 9:10] <- (
      cft_scen[, 16] * bft_scen[, 7:8] / rowSums(bft_scen[, 4:8])
    )

    # fraction of irrigated tropical and temperate BE trees scaled by total
    #   irrigated bioenergy tree area and relative fpc of bioenergy trees and
    #   grass under bioenergy trees
    tree_area_scen[, 11:12] <- (
      cft_scen[, 32] * bft_scen[, 16:17] / rowSums(bft_scen[, 13:17])
    )

    grass_area_scen <- array(0, dim = c(ncells, 21))

    # natural grass
    grass_area_scen[, 1:3] <- fpc_scen[, 10:12] * fpc_scen[, 1]

    # crops
    grass_area_scen[, 4:16] <- cft_scen[, 1:13] + cft_scen[, 17:29]

    # managed grass rf
    grass_area_scen[, 17] <- cft_scen[, 14]

    # managed grass irr
    grass_area_scen[, 18] <- cft_scen[, 30]

    # bioenergy grass
    grass_area_scen[, 19] <- cft_scen[, 15] + cft_scen[, 31]

    # fraction of rainfed grass under bioenergy trees
    grass_area_scen[, 20] <- (
      cft_scen[, 16] * rowSums(bft_scen[, 4:6]) / rowSums(bft_scen[, 4:8])
    )

    # fraction of irrigated grass under bioenergy trees
    grass_area_scen[, 21] <- (
      cft_scen[, 32] * rowSums(bft_scen[, 13:15]) / rowSums(bft_scen[, 13:17])
    )

    # evergreenness, needleleavedness, tropicalness, borealness, naturalness
    tree_attributes <- matrix(
      c(
        c(1, 0, 1, 0, 1), # 1 TrBE
        c(0, 0, 1, 0, 1), # 2 TrBR
        c(1, 1, 0, 0, 1), # 3 TeNE
        c(1, 0, 0, 0, 1), # 4 TeBE
        c(0, 0, 0, 0, 1), # 5 TeBS
        c(1, 1, 0, 1, 1), # 6 BoNE
        c(0, 0, 0, 1, 1), # 7 BoBS
        c(0, 1, 0, 1, 1), # 8 BoNS
        c(1, 0, 1, 0, 0), # 9 TrBi tropical bioenergy rainfed
        c(0, 0, 0, 0, 0), # 10 TeBi temperate bioenergy rainfed
        c(1, 0, 1, 0, 0), # 11 TrBi tropical bioenergy irrigated
        c(0, 0, 0, 0, 0) # 12 TeBi temperate bioenergy irrigated
      ),
      nrow = 12,
      byrow = TRUE
    )

    if (weighting == "equal") {
      tree_weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)

      # changed compared to Sebastian Ostberg's method
    } else if (weighting == "new") {
      tree_weights <- c(0.2, 0.2, 0.3, 0.3, 0.3) / 1.3

      # Sebastian Ostberg's method (no downscaling to weightsum 1)
    } else if (weighting == "old") {
      tree_weights <- c(0.2, 0.2, 0.3, 0.3, 0.3)
    } else {
      stop("Unknown method of weighting.")
    }

    grass_attributes <- array(0, dim = c(ncells, 21, 3))
    # 1 C4grass tropic
    # 2 C3grass temperate
    # 3 C3grass polar
    # 4 TemperateCereals
    # 5 Rice
    # 6 Maize
    # 7 TropicalCereals
    # 8 Pulses
    # 9 TemperateRoots
    # 10 TropicalRoots
    # 11 Sunflower
    # 12 Soybean
    # 13 Groundnut
    # 14 Rapeseed
    # 15 Sugarcane
    # 16 Others
    # 17 Managed grass rainfed
    # 18 Managed grass irrigated
    # 19 Bioenergy grass
    # 20 Grass under rainfed Bioenergy trees
    # 21 Grass under irrigated Bioenergy trees

    # tropicalness
    grass_attributes[, , 1] <- rep(
      c(1, 0, 0, 0, 1, 1, 1, 0.5, 0, 1, 0.5, 1, 1, 0.5, 1, 0.5, NA, NA, 1, NA, NA), # nolint
      each = ncells
    )

    # borealness
    grass_attributes[, , 2] <- rep(
      c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, 0, NA, NA),
      each = ncells
    )

    # naturalness
    grass_attributes[, , 3] <- rep(
      c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      each = ncells
    )

    # dynamic share of tropicalness for grass under irr biotrees
    dyn_trop_grass_attributes <- cbind(
      # dynamic share of tropicalness for rf grasslands
      bft_scen[, 1] / rowSums(bft_scen[, 1:3]),
      # dynamic share of tropicalness for irr grasslands
      bft_scen[, 10] / rowSums(bft_scen[, 10:12]),
      # dynamic share of tropicalness for grass under rf biotrees
      bft_scen[, 4] / rowSums(bft_scen[, 4:6]),
      bft_scen[, 13] / rowSums(bft_scen[, 13:15])
    )

    dyn_trop_grass_attributes[!is.finite(dyn_trop_grass_attributes)] <- 0

    # managed grass rf/irr, grass under biotrees rf/irr
    grass_attributes[, c(17, 18, 20, 21), 1] <- dyn_trop_grass_attributes

    # dynamic share of borealness for grass under irr biotrees
    dyn_boreal_grass_attributes <- cbind(
      # dynamic share of borealness for rf grasslands
      bft_scen[, 3] / rowSums(bft_scen[, 1:3]),
      # dynamic share of borealness for irr grasslands
      bft_scen[, 12] / rowSums(bft_scen[, 10:12]),
      # dynamic share of borealness for grass under rf biotrees
      bft_scen[, 6] / rowSums(bft_scen[, 4:6]),
      bft_scen[, 15] / rowSums(bft_scen[, 13:15])
    )

    dyn_boreal_grass_attributes[!is.finite(dyn_boreal_grass_attributes)] <- 0

    # managed grass rf/irr, grass under biotrees rf/irr
    grass_attributes[, c(17, 18, 20, 21), 2] <- dyn_boreal_grass_attributes

    if (weighting == "equal") {
      grass_weights <- c(0.2, 0.2, 0.2)
    } else if (weighting == "old" || weighting == "new") {
      grass_weights <- c(0.3333333, 0.3333333, 0.3333333)
    } else {
      stop("Unknown method of weighting.")
    }
  } else {
    stop("Unknown number of pfts.")
  }

  # compute vegetation_structure_change
  barren_v <- fBasics::rowMins(cbind(barren_area_ref, barren_area_scen))

  trees_v <- fBasics::rowMins(
    cbind(
      rowSums(tree_area_ref, na.rm = TRUE),
      rowSums(tree_area_scen, na.rm = TRUE)
    )
  )

  grass_v <- fBasics::rowMins(
    cbind(
      rowSums(grass_area_ref, na.rm = TRUE),
      rowSums(grass_area_scen, na.rm = TRUE)
    )
  )

  inner_sum_trees <- (
    # evergreenness
    abs(
      rowSums(tree_area_ref[, ] * rep(tree_attributes[, 1], each = ncells), na.rm = TRUE) - # nolint
        rowSums(tree_area_scen[, ] * rep(tree_attributes[, 1], each = ncells), na.rm = TRUE) # nolint
    ) * tree_weights[1] +
      # needleleavedness
      abs(
        rowSums(tree_area_ref[, ] * rep(tree_attributes[, 2], each = ncells), na.rm = TRUE) - # nolint
          rowSums(tree_area_scen[, ] * rep(tree_attributes[, 2], each = ncells), na.rm = TRUE) # nolint
      ) * tree_weights[2] +
      # tropicalness
      abs(
        rowSums(tree_area_ref[, ] * rep(tree_attributes[, 3], each = ncells), na.rm = TRUE) - # nolint
          rowSums(tree_area_scen[, ] * rep(tree_attributes[, 3], each = ncells), na.rm = TRUE) # nolint
      ) * tree_weights[3] +
      # borealness
      abs(
        rowSums(tree_area_ref[, ] * rep(tree_attributes[, 4], each = ncells), na.rm = TRUE) - # nolint
          rowSums(tree_area_scen[, ] * rep(tree_attributes[, 4], each = ncells), na.rm = TRUE) # nolint
      ) * tree_weights[4] +
      # naturalness
      abs(
        rowSums(tree_area_ref[, ] * rep(tree_attributes[, 5], each = ncells), na.rm = TRUE) - # nolint
          rowSums(tree_area_scen[, ] * rep(tree_attributes[, 5], each = ncells), na.rm = TRUE) # nolint
      ) * tree_weights[5]
  )

  if (npfts == 9) {
    inner_sum_grasses <- (
      # tropicalness
      abs(
        rowSums(grass_area_ref[, ] * grass_attributes[, , 1], na.rm = TRUE) -
          rowSums(grass_area_scen[, ] * grass_attributes[, , 1], na.rm = TRUE)
      ) * grass_weights[1] +
        # naturalness
        abs(
          rowSums(grass_area_ref[, ] * grass_attributes[, , 2], na.rm = TRUE) -
            rowSums(grass_area_scen[, ] * grass_attributes[, , 2], na.rm = TRUE)
        ) * grass_weights[2]
    )
  } else if (npfts == 11) {
    inner_sum_grasses <- (
      # tropicalness
      abs(
        rowSums(grass_area_ref[, ] * grass_attributes[, , 1], na.rm = TRUE) -
          rowSums(grass_area_scen[, ] * grass_attributes[, , 1], na.rm = TRUE)
      ) * grass_weights[1] +
        # borealness
        abs(
          rowSums(grass_area_ref[, ] * grass_attributes[, , 2], na.rm = TRUE) -
            rowSums(grass_area_scen[, ] * grass_attributes[, , 2], na.rm = TRUE)
        ) * grass_weights[2] +
        # naturalness
        abs(
          rowSums(grass_area_ref[, ] * grass_attributes[, , 3], na.rm = TRUE) -
            rowSums(grass_area_scen[, ] * grass_attributes[, , 3], na.rm = TRUE)
        ) * grass_weights[3]
    )
  } else {
    stop("Unknown number of pfts.")
  }

  vegetation_structure_change <- (
    1 - barren_v -
      trees_v * (1 - inner_sum_trees) -
      grass_v * (1 - inner_sum_grasses)
  )

  vegetation_structure_change[vegetation_structure_change < 0] <- 0

  vegetation_structure_change[!is.finite(vegetation_structure_change)] <- 0

  return(vegetation_structure_change)
}


################# further EcoRisk utility functions ##################

t_sigmoid_trafo <- function(x) {
  return(-1 / exp(3) + (1 + 1 / exp(3)) / (1 + exp(-6 * (x - 0.5))))
}


balance <- function(v1, v2) {
  return(1 - sum(v1 * v2) / (sqrt(sum(v1 * v1)) * sqrt(sum(v2 * v2))))
}


std_cellwise <- function(a) {
  return(apply(a, 1, stats::sd))
}


global_yearly_weighted_mean <- function(a, cell_area) {
  # a is matrix with dim=c(cells,years)
  # cell_area the corresponding cell_area array with dim=c(cells)
  return(
    sum(a * cell_area, na.rm = TRUE) /
      (length(a[1, ]) * sum(cell_area, na.rm = TRUE))
  )
}


globally_weighted_mean_foreach_var <- function(x, cell_area) { # nolint
  # x is matrix with dim=c(ncells,vars)
  # if vars==1 inflate x
  le <- length(x)
  if (le == length(cell_area)) dim(x) <- c(le, 1)

  # cell_area the corresponding cell_area array to x with dim=c(ncells)
  return(colSums(x * cell_area, na.rm = TRUE) / sum(cell_area, na.rm = TRUE))
}


s_change_to_var_ratio <- function(x, s) {
  return(1 / (1 + exp(-4 * (x / s - 2))))
}


#' based on Heyder 2011 eq. 6-9; epsilon case handling from code
#'   by Sebastian Ostberg (not documented in papers)
#' @param ref mean reference state vector of dimension c(ncells,variables)
#' @param scen mean scenario state vector of dimension c(ncells,variables)
#' @param epsilon threshold for variables to be treated as 0
#'
#' @returns the length of the difference vector for each cell
state_diff_local <- function(ref, scen, epsilon = 10^-4) {
  # Ostberg code: case change_metric_lu_comparison_jun2013.c
  di <- dim(ref)
  # generally normalize the scenario state vector by the reference state
  s_scen <- scen / ref
  s_ref <- array(1, dim = di) # initialize

  # for variables in places, where ref is small (<epsilon),
  #   but scen larger (Ostberg code, line 798)
  # Sebastian set back scenario and reference vector, to keep the unscaled
  #   values (Ostberg code, line 804)
  cells_ref0 <- abs(ref) < epsilon & abs(scen) > epsilon
  s_scen[cells_ref0] <- scen[cells_ref0]
  s_ref[cells_ref0] <- ref[cells_ref0]

  # for variables in places, where ref and scen are small (<epsilon),
  #   return 0 (both are 1, difference is 0) (Ostberg code, line 809)
  s_scen[abs(ref) < epsilon & abs(scen) < epsilon] <- 1 # no change

  # normalize both state vectors by the sqrt(amount of state variables) to
  #   ensure length(s_ref)==1 (this is part of the weighting in the Ostberg
  #   code)
  s_ref <- s_ref / sqrt(di[2])
  s_scen <- s_scen / sqrt(di[2])

  # length of the local difference vector s_scen (sl2) - s_ref (sl1)
  return(sqrt(rowSums((s_scen - s_ref) * (s_scen - s_ref))))
}



#' c based on Heyder 2011 eq. 10-13
#'
#' @param ref mean reference state vector of dimension c(ncells,variables)
#' @param scen mean scenario state vector of dimension c(ncells,variables)
#' @param cell_area area of each cell as a vector of dim=c(ncells)
#' @param epsilon threshold for variables to be treated as 0
#'
#' @returns the length of the difference vector for each cell
state_diff_global <- function(ref, scen, cell_area, epsilon = 10^-4) {
  di <- dim(ref)
  ncells <- di[1]
  global_mean_ref <- globally_weighted_mean_foreach_var(ref, cell_area)
  global_mean_scen <- globally_weighted_mean_foreach_var(scen, cell_area)

  # if global mean state in ref period is 0 (e.g. for landuse vars in pnv run?)
  # take the mean scen state instead
  cells_ref0 <- abs(global_mean_ref) < epsilon & abs(global_mean_scen) > epsilon
  global_mean_ref[cells_ref0] <- global_mean_scen[cells_ref0]
  # if both are 0 take 1, then the division is defined but 0 - 0 leads
  # to no change, which is what EcoRisk should show
  cells_both0 <- (
    abs(global_mean_ref) < epsilon & abs(global_mean_scen) < epsilon
  )
  global_mean_ref[cells_both0] <- 1

  norm <- rep(global_mean_ref, each = ncells)
  dim(norm) <- dim(ref)
  s_scen <- scen / norm
  s_ref <- ref / norm

  # normalize both state vectors by the sqrt(amount of state variables) to
  #   ensure length(s_ref)==1
  # (this is part of the weighting in the Ostberg code)
  s_ref <- s_ref / sqrt(di[2])
  s_scen <- s_scen / sqrt(di[2])

  # length of the difference vector s_scen (sl2) - s_ref (sl1) for each cell
  return(sqrt(rowSums((s_scen - s_ref) * (s_scen - s_ref))))
}

calc_component <- function(ref, scen, local, cell_area) {
  di <- dim(ref)
  ncells <- di[1]
  nyears <- di[2]

  # calc mean ref and scen state
  if (length(di) == 2) {
    dim(ref) <- c(di, 1)
    dim(scen) <- c(di, 1)
  }
  ref_mean <- apply(ref, c(1, 3), mean)
  scen_mean <- apply(scen, c(1, 3), mean)


  if (local) {
    x <- t_sigmoid_trafo(state_diff_local(ref = ref_mean, scen = scen_mean))
  } else {
    x <- t_sigmoid_trafo(
      state_diff_global(ref = ref_mean, scen = scen_mean, cell_area = cell_area)
    )
  }
  # calculation of the change-to-variability ratio in my view is mathematically
  #   not correctly described in Heyder and Ostberg
  # - the way I understand it: recalculate the c/g/b value for each year of the
  #   ref period compared to the mean of the ref period as "scenario" and then
  #   calc the variability (sd) of that
  sigma_x_ref_list <- array(0, dim = c(ncells, nyears))
  for (i in seq_len(nyears)) {
    if (local) {
      sigma_x_ref_list[, i] <- t_sigmoid_trafo(
        state_diff_local(ref = ref_mean, scen = ref[, i, ])
      )
    } else {
      sigma_x_ref_list[, i] <- t_sigmoid_trafo(
        state_diff_global(
          ref = ref_mean,
          scen = ref[, i, ],
          cell_area = cell_area
        )
      )
    }
  }

  sigma_x_ref <- apply(sigma_x_ref_list, 1, stats::sd)
  c2vr <- s_change_to_var_ratio(x, sigma_x_ref)
  return(
    list(
      full = x * c2vr,
      value = x,
      var = c2vr
    )
  )
}


balance_shift <- function(ref, scen, epsilon = 10^-4) {
  # param ref with dimension c(ncells,nvars)
  # param scen with dimension c(ncells,nvars)

  if (is.null(dim(ref))) dim(ref) <- c(length(ref), 1)


  # first normalize as for local change
  s_scen <- scen / ref
  s_ref <- array(1, dim = dim(ref))

  # for variables in places, where ref is small (<epsilon), but scen larger
  # (Ostberg code, line 798/vector length calc in line 837)
  # set back scenario vector, to keep the unscaled values (Ostberg code,
  #   line 805)
  s_scen[abs(ref) < epsilon & abs(scen) > epsilon] <- (
    scen[abs(ref) < epsilon & abs(scen) > epsilon]
  )

  # for variables in places, where ref and scen are small (<epsilon),
  # set scen to 1 (both are 1, difference is 0 -- no change) (Ostberg code,
  #   line 809)

  # results in no change
  s_scen[abs(ref) < epsilon & abs(scen) < epsilon] <- 1
  # absa(_ts) in Sebastians Ostberg's code
  abs_ref <- sqrt(rowSums(s_ref * s_ref))
  # absb(_ts) in Sebastian Ostberg's code
  abs_scen <- sqrt(rowSums(s_scen * s_scen))

  # =1-angle_ts
  b1 <- 1 - (rowSums(s_ref * s_scen) / abs_ref / abs_scen) # todo: hier wird alles 0

  # restrain to the maximum range for the acos function
  b1[b1 < 0] <- 0
  b1[b1 > 2] <- 2
  angle <- acos(1 - b1) * 360 / 2 / pi
  angle[b1 == 1] <- 0
  b <- b1 * 2
  b[angle > 60] <- 1

  return(b)
}


calc_ecosystem_balance <- function(ref, scen) {
  di <- dim(ref)
  ncells <- di[1]
  nyears <- di[2]

  if (length(di) == 2) {
    dim(ref) <- c(di, 1)
    dim(scen) <- c(di, 1)
  }
  ref_mean <- apply(ref, c(1, 3), mean)
  scen_mean <- apply(scen, c(1, 3), mean)


  b <- balance_shift(ref = ref_mean, scen = scen_mean)
  # calculation of the change-to-variability ratio in my view is mathematically
  #   not correctly described in Heyder and Ostberg
  # - the way I understand it: recalculate the c/g/b value for each year of the
  #   ref period compared to the mean
  # of the ref period as "scenario" and then calc the variability (sd) of that
  sigma_b_ref_list <- array(0, dim = c(ncells, nyears))
  for (i in seq_len(nyears)) {
    sigma_b_ref_list[, i] <- balance_shift(ref = ref_mean, scen = ref[, i, ])
  }
  sigma_b_ref <- apply(sigma_b_ref_list, 1, stats::sd)
  c2vr <- s_change_to_var_ratio(b, sigma_b_ref)
  return(
    list(
      full = b * c2vr,
      value = b,
      var = c2vr
    )
  )
}


#' Create modified EcoRisk data file
#'
#' Function to create a modified EcoRisk data file where each reference cell is
#' compared to the average reference biome cell. The scenario period is
#' overwritten with the original reference period and all reference cells are
#' set to the average cell of the prescribed reference biome ref_biom
#'
#' @param data_file_in path to input data
#' @param data_file_out path to save modified data to
#' @param biome_classes_in biome classes object as returned from classify_biomes
#' @param ref_biom reference biome from biome classes that all cells should
#'        be compared to
#'
#' @export
replace_ref_data_with_average_ref_biome_cell <- function(
    # nolint
    data_file_in,
    data_file_out,
    biome_classes_in,
    ref_biom) {
  if (data_file_in == data_file_out) {
    stop(
      "Same file for input and output of data, would overwrite ",
      "original data. Aborting."
    )
  }

  load(data_file_in)

  ref_cells <- which(biome_classes_in$biome_id == ref_biom)

  # first set all scen vars to the ref vars # [1:64240, 1:30, 1:10]
  state_scen <- state_ref

  fpc_scen <- fpc_ref
  bft_scen <- bft_ref
  cft_scen <- cft_ref

  di_state <- dim(state_scen)
  di_fpc <- dim(fpc_scen)
  di_bft <- dim(bft_scen)
  di_cft <- dim(cft_scen)

  # now replace all ref cells with that of the mean ref biom cell
  # FS 2022-08-10: keeping the year-to-year variation
  if (length(ref_cells) == 1) {
    av_year_state <- state_scen[ref_cells, , ]
    fpc_ref <- rep(fpc_scen[ref_cells, , ], each = di_fpc[1])
    bft_ref <- rep(bft_scen[ref_cells, , ], each = di_bft[1])
    cft_ref <- rep(cft_scen[ref_cells, , ], each = di_cft[1])
  } else {
    av_year_state <- apply(state_scen[ref_cells, , ], c(2, 3), mean)
    fpc_ref <- rep(
      apply(fpc_scen[ref_cells, , ], c(2, 3), mean),
      each = di_fpc[1]
    )
    bft_ref <- rep(
      apply(bft_scen[ref_cells, , ], c(2, 3), mean),
      each = di_bft[1]
    )
    cft_ref <- rep(
      apply(cft_scen[ref_cells, , ], c(2, 3), mean),
      each = di_cft[1]
    )
  }
  state_ref <- rep(av_year_state, each = di_state[1])
  dim(state_ref) <- di_state
  dimnames(state_ref) <- dimnames(state_scen)


  # is the same for each year, thus for the mean just take one year
  # mean_state_ref <- rep(colMeans(av_year_state), each = di_state[1])
  # FS: mean states were removed from data file, removing also here

  dim(fpc_ref) <- di_fpc
  dimnames(fpc_ref) <- dimnames(fpc_scen)
  dim(bft_ref) <- di_bft
  dimnames(bft_ref) <- dimnames(bft_scen)
  dim(cft_ref) <- di_cft
  dimnames(cft_ref) <- dimnames(cft_scen)


  # and write out the modified data
  # save(state_ref,mean_state_ref,state_scen,mean_state_scen,fpc_ref,fpc_scen,
  # bft_ref,bft_scen,cft_ref,cft_scen,lat,lon,cell_area,file = data_file_out)
  save(state_ref,
    state_scen,
    fpc_ref,
    fpc_scen,
    bft_ref,
    bft_scen,
    cft_ref,
    cft_scen,
    lat,
    lon,
    cell_area,
    file = data_file_out
  )
}


#' Create modified EcoRisk data for crosstable
#'
#' Function to create a modified EcoRisk data file where for each biome
#' the average scenario cell is compared to the average scenario cell of all
#' other biomes. This can then be used to compute a crosstable with the average
#' difference between each of them as in the SI of Ostberg et al. 2013
#' (Critical impacts of global warming on land ecosystems)
#'
#' @param data_file_in path to input data
#' @param data_file_out path to save modified data to
#' @param biome_classes_in biome classes object as returned from classify_biomes
#' @param pick_cells pick one specific cell as representative for the biome
#'        instead of computing the average state
#' @param baseline_ref logical, use reference state as baseline?
#'        default is FALSE - use scenario state
#'
#' @return c2vr array to be used in the ecorisk call
#'
#' @export
ecorisk_cross_table <- function(data_file_in,
                                data_file_out,
                                biome_classes_in,
                                pick_cells = NULL,
                                baseline_ref = FALSE) {
  if (data_file_in == data_file_out) {
    stop(
      "Same file for input and output of data, would overwrite original data. ",
      "Aborting."
    )
  }
  load(data_file_in)

  # calculate ecorisk to get the variability
  ecorisk_c2vr <- drop(ecorisk_wrapper(
    path_ref = NULL,
    path_scen = NULL,
    read_saved_data = TRUE,
    nitrogen = TRUE,
    weighting = "equal",
    save_data = data_file_in,
    save_ecorisk = NULL,
    time_span_reference = as.character(1550:1579),
    time_span_scenario = as.character(1985:2014),
    dimensions_only_local = FALSE
  )$c2vr)

  if (baseline_ref) {
    # save reference state vectors, they contain relevant data (scen can go)
    state_sav <- state_ref
    fpc_sav <- fpc_ref
    bft_sav <- bft_ref
    cft_sav <- cft_ref
  } else {
    # save scenario state vectors, they contain relevant data (ref can go)
    state_sav <- state_scen
    fpc_sav <- fpc_scen
    bft_sav <- bft_scen
    cft_sav <- cft_scen
  }


  nbiomes <- max(biome_classes_in$biome_id) # by default 19
  state_ref <- array(0, dim = c(nbiomes, nbiomes, dim(state_sav)[2:3]))
  state_scen <- state_ref
  fpc_ref <- array(0, dim = c(nbiomes, nbiomes, dim(fpc_sav)[2:3]))
  fpc_scen <- fpc_ref
  bft_ref <- array(0, dim = c(nbiomes, nbiomes, dim(bft_sav)[2:3]))
  bft_scen <- bft_ref
  cft_ref <- array(0, dim = c(nbiomes, nbiomes, dim(cft_sav)[2:3]))
  cft_scen <- cft_ref
  c2vr <- array(0, dim = c(4, nbiomes))

  # now replace all ref cells with that of the mean ref biome cell
  for (b in sort(unique(biome_classes_in$biome_id))) {
    ref_cells <- which(biome_classes_in$biome_id == b)

    if (is.null(pick_cells)) {
      if (length(ref_cells) == 1) {
        # average over cells, keeping the average year-to-year variation
        av_state <- state_sav[ref_cells, , ]
        av_fpc <- fpc_sav[ref_cells, , ]
        av_bft <- bft_sav[ref_cells, , ]
        av_cft <- cft_sav[ref_cells, , ]
        av_c2vr <- ecorisk_c2vr[, ref_cells]
      } else {
        # average over cells, keeping the average year-to-year variation
        av_state <- apply(state_sav[ref_cells, , ], c(2, 3), mean)
        av_fpc <- apply(fpc_sav[ref_cells, , ], c(2, 3), mean)
        av_bft <- apply(bft_sav[ref_cells, , ], c(2, 3), mean)
        av_cft <- apply(cft_sav[ref_cells, , ], c(2, 3), mean)
        av_c2vr <- apply(ecorisk_c2vr[, ref_cells], c(1), mean)
      }
    } else {
      av_state <- state_sav[pick_cells[b], , ]
      av_fpc <- fpc_sav[pick_cells[b], , ]
      av_bft <- bft_sav[pick_cells[b], , ]
      av_cft <- cft_sav[pick_cells[b], , ]
      av_c2vr <- ecorisk_c2vr[, pick_cells[b]]
    }

    state_ref[b, , , ] <- rep(av_state, each = nbiomes)
    state_scen[, b, , ] <- rep(av_state, each = nbiomes)

    mean_state_ref <- apply(state_ref, c(1, 3), mean)
    mean_state_scen <- apply(state_scen, c(1, 3), mean)

    fpc_ref[b, , , ] <- rep(av_fpc, each = nbiomes)
    fpc_scen[, b, , ] <- rep(av_fpc, each = nbiomes)

    bft_ref[b, , , ] <- rep(av_bft, each = nbiomes)
    bft_scen[, b, , ] <- rep(av_bft, each = nbiomes)

    cft_ref[b, , , ] <- rep(av_cft, each = nbiomes)
    cft_scen[, b, , ] <- rep(av_cft, each = nbiomes)
    c2vr[, b] <- av_c2vr
  }
  dim(state_ref) <- c(nbiomes * nbiomes, dim(state_sav)[2:3])
  dim(state_scen) <- c(nbiomes * nbiomes, dim(state_sav)[2:3])
  dim(fpc_ref) <- c(nbiomes * nbiomes, dim(fpc_sav)[2:3])
  dim(fpc_scen) <- c(nbiomes * nbiomes, dim(fpc_sav)[2:3])
  dim(bft_ref) <- c(nbiomes * nbiomes, dim(bft_sav)[2:3])
  dim(bft_scen) <- c(nbiomes * nbiomes, dim(bft_sav)[2:3])
  dim(cft_ref) <- c(nbiomes * nbiomes, dim(cft_sav)[2:3])
  dim(cft_scen) <- c(nbiomes * nbiomes, dim(cft_sav)[2:3])
  dimnames(state_ref) <- list(
    cell = as.character(seq_len(nbiomes * nbiomes)),
    year = dimnames(state_sav)$year,
    class = dimnames(state_sav)$class
  )
  dimnames(state_scen) <- dimnames(state_ref)
  dimnames(fpc_ref) <- list(
    cell = as.character(seq_len(nbiomes * nbiomes)),
    band = dimnames(fpc_sav)$band,
    year = dimnames(fpc_sav)$year
  )
  dimnames(fpc_scen) <- dimnames(fpc_ref)
  dimnames(bft_ref) <- list(
    cell = as.character(seq_len(nbiomes * nbiomes)),
    band = dimnames(bft_sav)$band,
    year = dimnames(bft_sav)$year
  )
  dimnames(bft_scen) <- dimnames(bft_ref)
  dimnames(cft_ref) <- list(
    cell = as.character(seq_len(nbiomes * nbiomes)),
    band = dimnames(cft_sav)$band,
    year = dimnames(cft_sav)$year
  )
  dimnames(cft_scen) <- dimnames(cft_ref)
  dimnames(c2vr) <- list(component = c("vs", "lc", "gi", "eb"), biome = biome_classes_in$biome_names)


  lat <- rep(0, nbiomes * nbiomes)
  lon <- rep(1, nbiomes * nbiomes)
  cell_area <- rep(2, nbiomes * nbiomes)

  # and write out the modified data
  save(state_ref,
    mean_state_ref,
    state_scen,
    mean_state_scen,
    fpc_ref,
    fpc_scen,
    bft_ref,
    bft_scen,
    cft_ref,
    cft_scen,
    lat,
    lon,
    cell_area,
    file = data_file_out
  )
  return(c2vr)
}


################# biome (dis-)aggregation functions ##################

#' Get biome names
#'
#' Returns biome names with variable length (abbreviated, short, or full)
#'
#' @param biome_name_length integer chose from 1,2,3 for abbreviated, short,
#'                        or full biome names
#'
#' @export
get_biome_names <- function(biome_name_length = 2) {
  biome_mapping <- utils::read.csv(
    file = system.file(
      "extdata",
      "biomes.csv",
      package = "biospheremetrics"
    ),
    sep = ";"
  )

  if (biome_name_length == 1) {
    biome_class_names <- biome_mapping$abbreviation
  } else if (biome_name_length == 2) {
    biome_class_names <- biome_mapping$short_name
  } else if (biome_name_length == 3) {
    biome_class_names <- biome_mapping$name
  } else {
    stop(
      "Value for parameter biome_name_length out of range 1,2,3 - ",
      "was given as: ",
      biome_name_length
    )
  }

  return(biome_class_names)
}


#' Averages EcoRisk values across regions
#'
#' Returns the average value across either 4 regions or all (19) biomes for
#' EcoRisk and each of the subcomponents for each
#'
#' @param data List object, of which every item should be disaggregated
#' @param biome_class biome class list object as returned by classify_biomes
#' @param type string controlling whether to return  minimum, mean, maximum
#'        ("minmeanmax") or Q10,Q50,Q90 ("quantile") - default: "quantile"
#' @param classes string for into how many regions should be disaggregated
#'        "4biomes" (tropics/temperate/boreal/arctic) or "allbiomes"
#'
#' @examples
#' \dontrun{
#' disaggregate_into_biomes(
#'   ecorisk = ecorisk,
#'   biome_class = biome_classes,
#'   type = "quantile", classes = "4biomes"
#' )
#' }
#' @export
disaggregate_into_biomes <- function(data, # nolint
                                     biome_class,
                                     type = "quantile",
                                     classes = "4biomes") {
  di <- dim(data[[1]])
  comp_names <- names(data)

  if (type == "minmeanmax") {
    type_names <- c("min", "mean", "max")
  } else if (type == "quantile") {
    type_names <- c("Q10", "Q50", "Q90")
  }

  if (length(di) > 1) {
    slices <- di[2]
  } else {
    slices <- 1
  }

  if (classes == "4biomes") {
    tropics <- c(1, 2, 9, 10, 11)
    temperate <- c(3, 4, 5, 12, 13, 14)
    boreal <- c(6, 7, 8)
    arctic <- c(15, 16)
    cell_list <- list(
      tropical_cells = which(biome_class$biome_id %in% tropics),
      temperate_cells = which(biome_class$biome_id %in% temperate),
      boreal_cells = which(biome_class$biome_id %in% boreal),
      arctic_cells = which(biome_class$biome_id %in% arctic)
    )
    nclasses <- 4
  } else if (classes == "allbiomes") {
    nclasses <- max(unique(biome_class$biome_id))
  } else {
    stop(
      "Unknown parameter classes: ",
      classes,
      ", should be either '4biomes' or 'allbiomes'"
    )
  }

  data_dims <- length(data)

  data_biomes <- array(0, dim = c(nclasses, data_dims, 3, slices))

  if (classes == "4biomes") { # aggregate to trop/temp/boreal/arctic
    for (s in seq_len(slices)) {
      for (b in seq_len(nclasses)) {
        for (cc in seq_len(data_dims)) {
          if (type == "minmeanmax") {
            data_biomes[b, cc, , s] <- c(
              min(data[[cc]][cell_list[[b]], s], na.rm = TRUE),
              mean(data[[cc]][cell_list[[b]], s], na.rm = TRUE),
              max(data[[cc]][cell_list[[b]], s], na.rm = TRUE)
            )
          } else if (type == "quantile") {
            data_biomes[b, cc, , s] <- c(
              stats::quantile(
                data[[cc]][cell_list[[b]], s],
                probs = c(0.1, 0.5, 0.9),
                na.rm = TRUE
              )
            )
          } else {
            stop(paste(
              "type", type,
              "unknown. please choose either 'quantile' or 'minmeanmax'"
            ))
          } # end if
        } # end for
      } # end for
    } # end for

    biome_names <- c("tropics", "temperate", "boreal", "arctic")
    dimnames(data_biomes) <- list(biome_names, comp_names, type_names, seq_len(slices))
  } else if (classes == "allbiomes") { # calculate all biomes separately
    for (s in seq_len(slices)) {
      for (b in seq_len(nclasses)) {
        for (cc in seq_len(data_dims)) {
          if (type == "minmeanmax") {
            data_biomes[b, cc, , s] <- c(
              min(data[[cc]][which(biome_class$biome_id == b), s], na.rm = TRUE),
              mean(data[[cc]][which(biome_class$biome_id == b), s], na.rm = TRUE), # nolint
              max(data[[cc]][which(biome_class$biome_id == b), s], na.rm = TRUE)
            )
          } else if (type == "quantile") {
            data_biomes[b, cc, , s] <- c(
              stats::quantile(
                data[[cc]][which(biome_class$biome_id == b), s],
                probs = c(0.1, 0.5, 0.9),
                na.rm = TRUE
              )
            )
          } else {
            stop(paste(
              "type", type,
              "unknown. please choose either 'quantile' or 'minmeanmax'"
            ))
          } # end if
        } # end for
      } # end for
    } # end for

    biome_names <- biome_class$biome_names
    dimnames(data_biomes) <- list(
      biome_names,
      comp_names,
      type_names,
      seq_len(slices)
    )
  } else {
    stop(
      "Unknown parameter classes: ",
      classes,
      ", should be either '4biomes' or 'allbiomes'"
    )
  }
  return(drop(data_biomes))
}


#' Calculate ecorisk with each biomes average cell
#'
#' Function to calculate ecorisk with each biomes average cell
#' as a measure of internal variability
#'
#' @param biome_classes biome classes object as returned by classify biomes,
#'                      calculated for data_file_base
#' @param data_file_base base EcoRisk to compute differences with (only ref is
#'                      relevant)
#' @param intra_biome_distrib_file file to additionally write results to
#' @param create create new modified files, or read already existing ones?
#' @param res how finegrained the distribution should be (resolution)
#' @param plotting whether plots for each biome should be created
#' @param plot_folder folder to plot into
#' @param time_span_reference suitable 30 year reference period (e.g.
#'                            c(1901,1930), c(1550,1579))

#' @return data object with distibution - dim: c(biomes,ecorisk_variables,bins)
#'
#' @export
calculate_within_biome_diffs <- function(biome_classes, # nolint
                                         data_file_base,
                                         intra_biome_distrib_file,
                                         create = FALSE,
                                         nitrogen = TRUE,
                                         res = 0.05,
                                         plotting = FALSE,
                                         plot_folder,
                                         time_span_reference,
                                         vars_ecorisk,
                                         ecorisk_components = 13) {
  biomes_abbrv <- get_biome_names(1)
  # nbiomes, nEcoRiskvars, nHISTclasses
  intra_biome_distrib <- array(
    0,
    dim = c(length(biome_classes$biome_names), ecorisk_components, 1 / res)
  )

  # start
  for (b in sort(unique(biome_classes$biome_id))) {
    filebase <- strsplit(data_file_base, "_data.RData")[[1]]
    message(
      "Calculating differences with biome ", b, " (",
      biome_classes$biome_names[b], ")"
    )

    data_file <- paste0(
      filebase, "_compared_to_average_", biomes_abbrv[b], "_data.RData"
    )
    ecorisk_file <- paste0(
      filebase, "_compared_to_average_", biomes_abbrv[b], "_gamma.RData"
    )

    if (create) {
      replace_ref_data_with_average_ref_biome_cell(
        data_file_in = data_file_base,
        data_file_out = data_file,
        biome_classes_in = biome_classes,
        ref_biom = b
      )
      ecorisk <- ecorisk_wrapper(
        # does not need to be specified, as data is read from file
        path_ref = NULL,
        # does not need to be specified, as data is read from file
        path_scen = NULL,
        read_saved_data = TRUE,
        nitrogen = nitrogen,
        weighting = "equal",
        save_data = data_file,
        save_ecorisk = ecorisk_file,
        time_span_reference = time_span_reference,
        time_span_scenario = time_span_reference,
        dimensions_only_local = FALSE
      )
    } else {
      # contains ecorisk list object
      load(ecorisk_file)
    }

    # compute average values per focus biom
    ref_cells <- which(biome_classes$biome_id == b)
    for (v in seq_len(10)) {
      intra_biome_distrib[b, v, ] <- graphics::hist(
        ecorisk[[v]][ref_cells],
        breaks = seq(0, 1, res), plot = FALSE
      )$counts
      intra_biome_distrib[b, v, ] <- (
        intra_biome_distrib[b, v, ] / sum(intra_biome_distrib[b, v, ])
      )
    }

    if (plotting) {
      plot_ecorisk_map(
        file = paste0(
          plot_folder, "/compare_ecorisk_to_", biomes_abbrv[b], ".png"
        ),
        focus_biome = b, biome_classes = biome_classes$biome_id,
        data = ecorisk$ecorisk_total, title = biome_classes$biome_names[b],
        legendtitle = "",
        eps = FALSE,
        title_size = 2,
        leg_yes = TRUE
      )

      plot_ecorisk_map(
        file = paste0(
          plot_folder, "/compare_vegetation_structure_change_to_", biomes_abbrv[b], ".png" # nolint
        ),
        focus_biome = b, biome_classes = biome_classes$biome_id,
        data = ecorisk$vegetation_structure_change,
        title = biome_classes$biome_names[b],
        legendtitle = "",
        eps = FALSE,
        title_size = 2,
        leg_yes = TRUE
      )

      plot_ecorisk_map(
        file = paste0(
          plot_folder, "/compare_gi_to_", biomes_abbrv[b], ".png"
        ),
        focus_biome = b,
        biome_classes = biome_classes$biome_id,
        data = ecorisk$global_importance,
        title = biome_classes$biome_names[b],
        legendtitle = "",
        eps = FALSE,
        title_size = 2,
        leg_yes = TRUE
      )

      plot_ecorisk_map(
        file = paste0(
          plot_folder, "/compare_lc_to_", biomes_abbrv[b], ".png"
        ),
        focus_biome = b,
        biome_classes = biome_classes$biome_id,
        data = ecorisk$local_change,
        title = biome_classes$biome_names[b],
        legendtitle = "",
        eps = FALSE,
        title_size = 2,
        leg_yes = TRUE
      )

      plot_ecorisk_map(
        file = paste0(
          plot_folder, "/compare_eb_to_", biomes_abbrv[b], ".png"
        ),
        focus_biome = b,
        biome_classes = biome_classes$biome_id,
        data = ecorisk$ecosystem_balance,
        title = biome_classes$biome_names[b],
        legendtitle = "",
        eps = FALSE,
        title_size = 2,
        leg_yes = TRUE
      )
    } # end if plotting
  }
  ecorisk_dimensions <- names(ecorisk)

  dim(intra_biome_distrib) <- c(biome = length(biome_classes$biome_names), variable = ecorisk_components, bin = 1 / res)
  dimnames(intra_biome_distrib) <- list(
    biome = biomes_abbrv, variable = ecorisk_dimensions, bin = seq(res, 1, res)
  )
  save(intra_biome_distrib, file = intra_biome_distrib_file)

  return(intra_biome_distrib)
}


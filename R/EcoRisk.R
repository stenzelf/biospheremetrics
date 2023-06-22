# written by Fabian Stenzel, based on work by Sebastian Ostberg
# 2022-2023 - stenzel@pik-potsdam.de

################# EcoRisk calc functions  ###################

#' Wrapper for calculating the ecosystem change metric EcoRisk
#'
#' Function to read in data for ecorisk, and call the calculation function once,
#' if overtime is F, or for each timeslice of length window years, if
#' overtime is T
#'
#' @param folderRef folder of reference run
#' @param folderScen folder of scenario run
#' @param readPreviouslySavedData whether to read in previously saved data
#'        (default: FALSE)
#' @param saveFileData file to save read in data to (default NULL)
#' @param saveFileEcoRisk file to save EcoRisk data to (default NULL)
#' @param nitrogen include nitrogen outputs for pools and fluxes into EcoRisk
#'        calculation (default F)
#' @param weighting apply "old" (Ostberg-like), "new", or "equal" weighting of
#'        vegetation_structure_change weights (default "equal")
#' @param varnames data.frame with names of output files (outname) and time res.
#'        (timestep) -- can be specified to account for variable file names
#'        (default NULL -- standard names as below)
#'        varnames <- data.frame(row.names = c("grid",    "fpc",    "fpc_bft",    "cftfrac",    "firec",    "rh_harvest",    "npp",    "evapinterc",    "runoff",    "transp",    "soillitc",    "vegc",    "swcsum",    "firef",    "rh",    "harvestc",    "evap",    "interc",    "soilc",    "litc",    "swc",    "vegn",    "soilnh4",    "soilno3",    "leaching",    "n2o_denit",    "n2o_nit",    "n2o_denit",    "n2_emis",    "bnf",    "n_volatilization"),
#'        outname = c("grid.bin","fpc.bin","fpc_bft.bin","cftfrac.bin","firec.bin","rh_harvest.bin","npp.bin","evapinterc.bin","runoff.bin","transp.bin","soillitc.bin","vegc.bin","swcsum.bin","firef.bin","rh.bin","harvestc.bin","evap.bin","interc.bin","soilc.bin","litc.bin","swc.bin","vegn.bin","soilnh4.bin","soilno3.bin","leaching.bin","n2o_denit.bin","n2o_nit.bin","n2o_denit.bin","n2_emis.bin","bnf.bin","n_volatilization.bin"),
#'        timestep = c("Y",       "Y",      "Y",          "Y",          "Y",        "Y",             "Y",      "Y",             "Y",         "Y",         "Y",           "Y",       "Y",         "Y",        "Y",     "Y",           "Y",       "Y",         "Y",        "Y",       "Y",      ,"Y",      "Y",          "Y",          "Y",           "Y",            "Y",          "Y",            "Y",          "Y",      "Y"))
#' @param time_span_reference vector of years to use as scenario period
#' @param time_span_scenario vector of years to use as scenario period
#' @param dimensionsOnlyLocal flag whether to use only local change component
#'        for water/carbon/nitrogen fluxes and pools, or use an average of
#'        local change, global change and ecosystem balance (default F)
#' @param overtime logical: calculate ecorisk as time-series? (default: F)
#' @param window integer, number of years for window length (default: 30)
#' @param debug write out all nitrogen state variables (default F)
#'
#' @return list data object containing arrays of ecorisk_total, vegetation_structure_change, 
#'         local_change, global_importance, ecosystem_balance, carbon_stocks, 
#'         carbon_fluxes, water_fluxes (+ nitrogen_stocks and nitrogen_fluxes)
#'
#' @examples
#' \dontrun{
#' }
#' @export
ecoriskWrapper <- function(folderRef,
                     folderScen,
                     readPreviouslySavedData = FALSE,
                     saveFileData = NULL,
                     saveFileEcoRisk = NULL,
                     nitrogen = TRUE,
                     weighting = "equal",
                     varnames = NULL,
                     time_span_reference,
                     time_span_scenario,
                     dimensionsOnlyLocal = F,
                     overtime = F,
                     window = 30,
                     debug = FALSE
                     ) {
  if (is.null(varnames)) {
    print("variable name list not provided, using standard list, which might 
          not be applicable for this case ...")
    varnames <- data.frame(row.names = c("grid",    "fpc",    "fpc_bft",    "cftfrac",    "firec",    "rh_harvest",    "npp",    "evapinterc",    "runoff",    "transp",    "soillitc",    "vegc",    "swcsum",    "firef",    "rh",    "harvestc",        "rharvestc",        "pft_harvestc",       "pft_rharvestc",       "evap",    "interc", "discharge",   "soilc",    "litc",    "swc",    "vegn",    "soilnh4",    "soilno3",    "leaching",    "n2o_denit",    "n2o_nit",    "n2o_denit",    "n2_emis",    "bnf",    "n_volatilization"),
                             outname = c("grid.bin","fpc.bin","fpc_bft.bin","cftfrac.bin","firec.bin","rh_harvest.bin","npp.bin","evapinterc.bin","runoff.bin","transp.bin","soillitc.bin","vegc.bin","swcsum.bin","firef.bin","rh.bin","flux_harvest.bin","flux_rharvest.bin","pft_harvest.pft.bin","pft_rharvest.pft.bin","evap.bin","interc.bin","discharge.bin","soilc.bin","litc.bin","swc.bin","vegn.bin","soilnh4.bin","soilno3.bin","leaching.bin","n2o_denit.bin","n2o_nit.bin","n2o_denit.bin","n2_emis.bin","bnf.bin","n_volatilization.bin"),
                            timestep = c("Y",       "Y",      "Y",          "Y",          "Y",        "Y",             "Y",      "Y",             "Y",         "Y",         "Y",           "Y",       "Y",         "Y",        "Y",     "Y",               "Y",                "Y",                  "Y",                   "Y",       "Y",        ,"Y", "Y",        "Y",       "Y",      ,"Y",      "Y",          "Y",          "Y",           "Y",            "Y",          "Y",            "Y",          "Y",      "Y"))
  }

  nyears <- length(time_span_reference)
  nyears_scen <- length(time_span_scenario)
  if (nyears < 30 || nyears_scen <30) {
    stop("Warning: timespan in reference or scenario is smaller than 30 years.")
  }
        # translate varnames and folders to files_scenarios/reference lists
  files_scenario <- list( 
    grid = paste0(folderScen,varnames["grid","outname"]),
    fpc = paste0(folderScen,varnames["fpc","outname"]),
    fpc_bft = paste0(folderScen,varnames["fpc_bft","outname"]),
    cftfrac = paste0(folderScen,varnames["cftfrac","outname"]),
    firec = paste0(folderScen,varnames["firec","outname"]),
    npp = paste0(folderScen,varnames["npp","outname"]),
    runoff = paste0(folderScen,varnames["runoff","outname"]),
    transp = paste0(folderScen,varnames["transp","outname"]),
    vegc = paste0(folderScen,varnames["vegc","outname"]),
    firef = paste0(folderScen,varnames["firef","outname"]),
    rh = paste0(folderScen,varnames["rh","outname"]),
    harvestc = paste0(folderScen,varnames["harvestc","outname"]),
    rharvestc = paste0(folderScen,varnames["rharvestc","outname"]),
    pft_harvestc = paste0(folderScen,varnames["pft_harvest","outname"]),
    pft_rharvestc = paste0(folderScen,varnames["pft_rharvest","outname"]),
    evap = paste0(folderScen,varnames["evap","outname"]),
    interc = paste0(folderScen,varnames["interc","outname"]),
    discharge = paste0(folderScen,varnames["discharge","outname"]),
    soilc = paste0(folderScen,varnames["soilc","outname"]),
    litc = paste0(folderScen,varnames["litc","outname"]),
    swc = paste0(folderScen,varnames["swc","outname"]),
    vegn = paste0(folderScen,varnames["vegn","outname"]),
    
    soilnh4 = paste0(folderScen,varnames["soilnh4","outname"]),
    soilno3 = paste0(folderScen,varnames["soilno3","outname"]),
    leaching = paste0(folderScen,varnames["leaching","outname"]),
    n2o_denit = paste0(folderScen,varnames["n2o_denit","outname"]),
    n2o_nit = paste0(folderScen,varnames["n2o_nit","outname"]),
    n2_emis = paste0(folderScen,varnames["n2_emis","outname"]),
    bnf = paste0(folderScen,varnames["bnf","outname"]),
    n_volatilization = paste0(folderScen,varnames["n_volatilization","outname"])
  )
  files_reference <- list(
     grid = paste0(folderRef,varnames["grid","outname"]),
     fpc = paste0(folderRef,varnames["fpc","outname"]),
     fpc_bft = paste0(folderRef,varnames["fpc_bft","outname"]),
     cftfrac = paste0(folderRef,varnames["cftfrac","outname"]),
     firec = paste0(folderRef,varnames["firec","outname"]),
     npp = paste0(folderRef,varnames["npp","outname"]),
     runoff = paste0(folderRef,varnames["runoff","outname"]),
     transp = paste0(folderRef,varnames["transp","outname"]),
     vegc = paste0(folderRef,varnames["vegc","outname"]),
     firef = paste0(folderRef,varnames["firef","outname"]),
     rh = paste0(folderRef,varnames["rh","outname"]),
     harvestc = paste0(folderRef,varnames["harvestc","outname"]),
     rharvestc = paste0(folderRef,varnames["rharvestc","outname"]),
     pft_harvestc = paste0(folderRef,varnames["pft_harvest","outname"]),
     pft_rharvestc = paste0(folderRef,varnames["pft_rharvest","outname"]),
     evap = paste0(folderRef,varnames["evap","outname"]),
     interc = paste0(folderRef,varnames["interc","outname"]),
     discharge = paste0(folderRef,varnames["discharge","outname"]),
     soilc = paste0(folderRef,varnames["soilc","outname"]),
     litc = paste0(folderRef,varnames["litc","outname"]),
     swc = paste0(folderRef,varnames["swc","outname"]),
     vegn = paste0(folderRef,varnames["vegn","outname"]),
     soilnh4 = paste0(folderRef,varnames["soilnh4","outname"]),
     soilno3 = paste0(folderRef,varnames["soilno3","outname"]),
     leaching = paste0(folderRef,varnames["leaching","outname"]),
     n2o_denit = paste0(folderRef,varnames["n2o_denit","outname"]),
     n2o_nit = paste0(folderRef,varnames["n2o_nit","outname"]),
     n2_emis = paste0(folderRef,varnames["n2_emis","outname"]),
     bnf = paste0(folderRef,varnames["bnf","outname"]),
     n_volatilization = paste0(folderRef,varnames["n_volatilization","outname"])
  )

   if (overtime && window != nyears) stop("Overtime is enabled, but window \
                  length (",window,") does not match the reference nyears.") 

   if (readPreviouslySavedData) {
     if (!is.null(saveFileData)) {
       print(paste("Loading saved data from:",saveFileData))
       load(file = saveFileData)
     }else{
       stop("saveFileData is not specified as parameter, \
             nothing to load ... exiting")
     }

   }else{
     #first read in all lpjml output files required for computing EcoRisk
     returned_vars <- readEcoRiskData(files_reference = files_reference,
                                   files_scenario = files_scenario,
                                   saveFile = saveFileData,
                                   export = F,
                                   nitrogen = nitrogen,
                                   time_span_reference = time_span_reference,
                                   time_span_scenario = time_span_scenario,
                                   debug = debug
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
     cellArea <- returned_vars$cellArea
     rm(returned_vars)
   }

   ncells <- length(cellArea)
   slices <- (nyears_scen - window + 1)
   ecorisk <- list(ecorisk_total = array(0,dim=c(ncells,slices)),
                vegetation_structure_change = array(0,dim=c(ncells,slices)),
                local_change = array(0,dim=c(ncells,slices)),
                global_importance = array(0,dim=c(ncells,slices)),
                ecosystem_balance = array(0,dim=c(ncells,slices)),
                carbon_stocks = array(0,dim=c(ncells,slices)),
                carbon_fluxes = array(0,dim=c(ncells,slices)),
                water_stocks = array(0,dim=c(ncells,slices)),
                water_fluxes = array(0,dim=c(ncells,slices)),
                nitrogen_stocks = array(0,dim=c(ncells,slices)),
                nitrogen_fluxes = array(0,dim=c(ncells,slices))
                )
   for (y in 1:slices) {
    print(paste0("Calculating time slice ",y," of ",slices))
    returned <- calcEcoRisk(fpc_ref = fpc_ref, 
                        fpc_scen = fpc_scen[,,y:(y+window-1)],
                        bft_ref = bft_ref, 
                        bft_scen = bft_scen[,,y:(y+window-1)],
                        cft_ref = cft_ref, 
                        cft_scen = cft_scen[,,y:(y+window-1)],
                        state_ref = state_ref, 
                        state_scen = state_scen[,y:(y+window-1),],
                        weighting = weighting,
                        lat = lat,
                        lon = lon,
                        cellArea = cellArea,
                        dimensionsOnlyLocal = dimensionsOnlyLocal,
                        nitrogen = nitrogen)
    ecorisk$ecorisk_total[,y] <- returned$ecorisk_total
    ecorisk$vegetation_structure_change[,y] <- returned$vegetation_structure_change
    ecorisk$local_change[,y] <- returned$local_change
    ecorisk$global_importance[,y] <- returned$global_importance
    ecorisk$ecosystem_balance[,y] <- returned$ecosystem_balance
    ecorisk$carbon_stocks[,y] <- returned$carbon_stocks
    ecorisk$carbon_fluxes[,y] <- returned$carbon_fluxes
    ecorisk$water_stocks[,y] <- returned$water_stocks
    ecorisk$water_fluxes[,y] <- returned$water_fluxes
    if (nitrogen){
      ecorisk$nitrogen_stocks[,y] <- returned$nitrogen_stocks
      ecorisk$nitrogen_fluxes[,y] <- returned$nitrogen_fluxes
    }
   }


   ############## export and save data if requested #############
   if (!(is.null(saveFileEcoRisk))) {
      print(paste0("Saving EcoRisk data to: ",saveFileEcoRisk))
      save(ecorisk, file = saveFileEcoRisk)
   }
   #
   ###
   return(ecorisk)
}

#' Calculate the ecosystem change metric EcoRisk between 2 sets of states
#'
#' Function to calculate the ecosystem change metric EcoRisk, based on gamma/vegetation_structure_change
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
#' @param cellArea cellarea array
#' @param dimensionsOnlyLocal flag whether to use only local change component
#'        for water/carbon/nitrogen fluxes and pools, or use an average of
#'        local change, global change and ecosystem balance (default F)
#' @param nitrogen include nitrogen outputs (default: TRUE)
#'
#' @return list data object containing arrays of ecorisk_total, vegetation_structure_change, 
#'         local_change, global_importance, ecosystem_balance, carbon_stocks, 
#'         carbon_fluxes, water_fluxes (+ nitrogen_stocks and nitrogen_fluxes)
#'
#' @examples
#' \dontrun{
#' }
#' @export
calcEcoRisk <- function(fpc_ref,
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
                     cellArea,
                     dimensionsOnlyLocal = F,
                     nitrogen = T
                     ) {
  
  di_ref <- dim(fpc_ref)
  di_scen <- dim(fpc_scen)
  ncells <- di_ref[1]
  nyears <- di_ref[3]
  if (di_ref[3] != di_scen[3]){
    stop("Dimension year does not match between fpc_scen and fpc_ref.")
  }
  ######### calc vegetation_structure_change and variability of vegetation_structure_change within      ##########
  ######### reference period S(vegetation_structure_change,sigma_vegetation_structure_change)           ##########
  fpc_ref_mean <- apply(fpc_ref, c(1,2), mean)
  bft_ref_mean <- apply(bft_ref, c(1,2), mean)
  cft_ref_mean <- apply(cft_ref, c(1,2), mean)

  mean_state_ref <- apply(state_ref,c(1,3),mean)
  mean_state_scen <- apply(state_scen,c(1,3),mean)

  sigma_vegetation_structure_change_ref_list <- array(0, dim = c(ncells, nyears))
  # calculate for every year of the reference period, vegetation_structure_change between 
  # that year and the average reference period year
  # -> this gives the variability of vegetation_structure_change within the reference period
  for (y in 1:nyears) {
    sigma_vegetation_structure_change_ref_list[,y] <- calcDeltaV(fpcRef = fpc_ref_mean,
                                              fpcScen = fpc_ref[,,y],
                                              bftRef = bft_ref_mean,
                                              bftScen = bft_ref[,,y],
                                              cftRef = cft_ref_mean,
                                              cftScen = cft_ref[,,y], 
                                              weighting = weighting
                                              )
  }
  # calculate the std deviation over the reference period for each gridcell
  vegetation_structure_changesd <- apply(sigma_vegetation_structure_change_ref_list,c(1),sd)
  # calculate vegetation_structure_change between average reference and average scenario period
  vegetation_structure_change <- calcDeltaV(fpcRef = fpc_ref_mean, 
                        fpcScen = apply(fpc_scen,c(1,2),mean),
                        bftRef = bft_ref_mean, 
                        bftScen = apply(bft_scen,c(1,2),mean), 
                        cftRef = cft_ref_mean,
                        cftScen = apply(cft_scen,c(1,2),mean), 
                        weighting = weighting)
  #
  ####
  ############## calc EcoRisk components ################
  # variable names for the state vector
  # 1:3 carbon fluxes
  # 4:6 water fluxes
  # 7:8 carbon pools/stocks,
  # 9:10 water pools
  # 11 additional variables for global/local difference, but not included in stocks/fluxes
  # 12:13 nitrogen pools/stocks
  # 14:16 nitrogen fluxes
  #                 1         2         3         4         5        6          7        8      9       10          11      12       13      14        15               16
  #var_names <- c("firec","rh_harvest","npp","evapinterc","runoff","transp","soillitc","vegc","swcsum","discharge","firef","soiln","vegn","leaching","bnf","aggregated_n_emissions")
  var_names <- dimnames(state_ref)[[3]]
  delta <- vegetation_structure_change*S_change_to_var_ratio(vegetation_structure_change, vegetation_structure_changesd)# vegetation_structure_change
  lc <- calcComponent(ref = state_ref, scen = state_scen, local = TRUE, cellArea = cellArea)         # local change
  gi <- calcComponent(ref = state_ref, scen = state_scen, local = FALSE, cellArea = cellArea)         # global importance
  eb <- calcEcosystemBalance(ref = state_ref, scen = state_scen)                                # ecosystem balance
  if (dimensionsOnlyLocal == T){
    cf <- calcComponent(ref = state_ref[,,1:3], scen = state_scen[,,1:3], local = TRUE, cellArea = cellArea)     # carbon fluxes (local change)
    cs <- calcComponent(ref = state_ref[,,7:8], scen = state_scen[,,7:8], local = TRUE, cellArea = cellArea)     # carbon stocks (local change)
    wf <- calcComponent(ref = state_ref[,,4:6], scen = state_scen[,,4:6], local = TRUE, cellArea = cellArea)     # water fluxes (local change)
    ws <- calcComponent(ref = state_ref[,,9:10], scen = state_scen[,,9:10], local = TRUE, cellArea = cellArea)     # water pools (local change)
    if (nitrogen) {
      ns <- calcComponent(ref = state_ref[,,12:13], scen = state_scen[,,12:13], local = TRUE, cellArea = cellArea) # nitrogen stocks (local change)
      nf <- calcComponent(ref = state_ref[,,14:16], scen = state_scen[,,14:16], local = TRUE, cellArea = cellArea) # nitrogen fluxes (local change)
    }
  }else{
    cf <- (   calcComponent(ref = state_ref[,,1:3], scen = state_scen[,,1:3], local = TRUE, cellArea = cellArea)     # carbon fluxes
               + calcComponent(ref = state_ref[,,1:3], scen = state_scen[,,1:3], local = FALSE, cellArea = cellArea)
               + calcEcosystemBalance(ref = state_ref[,,1:3], scen = state_scen[,,1:3]) )/3
    cs <- (   calcComponent(ref = state_ref[,,7:8], scen = state_scen[,,7:8], local = TRUE, cellArea = cellArea)     # carbon stocks
               + calcComponent(ref = state_ref[,,7:8], scen = state_scen[,,7:8], local = FALSE, cellArea = cellArea)
               + calcEcosystemBalance(ref = state_ref[,,7:8], scen = state_scen[,,7:8]) )/3
    wf <- (   calcComponent(ref = state_ref[,,4:6], scen = state_scen[,,4:6], local = TRUE, cellArea = cellArea)     # water fluxes
               + calcComponent(ref = state_ref[,,4:6], scen = state_scen[,,4:6], local = FALSE, cellArea = cellArea)
               + calcEcosystemBalance(ref = state_ref[,,4:6], scen = state_scen[,,4:6]) )/3
    ws <- (   calcComponent(ref = state_ref[,,9:10], scen = state_scen[,,9:10], local = TRUE, cellArea = cellArea)     # water pools
               + calcComponent(ref = state_ref[,,9:10], scen = state_scen[,,9:10], local = FALSE, cellArea = cellArea)
               + calcEcosystemBalance(ref = state_ref[,,9:10], scen = state_scen[,,9:10]) )/3
    if (nitrogen) {
      ns <- (   calcComponent(ref = state_ref[,,12:13], scen = state_scen[,,12:13], local = TRUE, cellArea = cellArea)  # nitrogen stocks (local change)
                 + calcComponent(ref = state_ref[,,12:13], scen = state_scen[,,12:13], local = FALSE, cellArea = cellArea)
                 + calcEcosystemBalance(ref = state_ref[,,12:13], scen = state_scen[,,12:13]) )/3
      nf <- (   calcComponent(ref = state_ref[,,14:16], scen = state_scen[,,14:16], local = TRUE, cellArea = cellArea) # nitrogen fluxes (local change)
                 + calcComponent(ref = state_ref[,,14:16], scen = state_scen[,,14:16], local = FALSE, cellArea = cellArea)
                 + calcEcosystemBalance(ref = state_ref[,,14:16], scen = state_scen[,,14:16]) )/3
    }
  }

  # calc total EcoRisk as the average of the 4 components
  ecoriskFull <- (delta + lc + gi + eb)/4 #check for NAs

  if (nitrogen) {
    ecorisk <- list(ecorisk_total = ecoriskFull, 
                    vegetation_structure_change = delta, 
                    local_change = lc, 
                    global_importance = gi, 
                    ecosystem_balance = eb,
                    carbon_stocks = cs, 
                    carbon_fluxes = cf, 
                    water_fluxes = wf, 
                    water_stocks = ws, 
                    nitrogen_stocks = ns, 
                    nitrogen_fluxes = nf)
  }else {
    ecorisk <- list(ecorisk_total = ecoriskFull, 
                    vegetation_structure_change = delta, 
                    local_change = lc, 
                    global_importance = gi, 
                    ecosystem_balance = eb,
                    carbon_stocks = cs, 
                    carbon_fluxes = cf, 
                    water_fluxes = wf, 
                    water_stocks = ws)
  }
  ###
  return(ecorisk)
}

#' Read in output data from LPJmL to calculate the ecosystem change metric EcoRisk
#'
#' Utility function to read in output data from LPJmL for calculation of EcoRisk
#'
#' @param files_reference folder of reference run
#' @param files_scenario folder of scenario run
#' @param saveFile file to save read in data to (default NULL)
#' @param export flag whether to export réad in data to global environment (default F)
#' @param time_span_reference vector of years to use as scenario period
#' @param time_span_scenario vector of years to use as scenario period
#' @param nitrogen include nitrogen outputs for pools and fluxes into EcoRisk calculation (default F)
#' @param debug write out all nitrogen state variables (default F)
#'
#' @return list data object containing arrays of state_ref, mean_state_ref,
#'         state_scen, mean_state_scen, fpc_ref, fpc_scen, bft_ref, bft_scen,
#'         cft_ref, cft_scen, lat, lon, cellArea
#'
#' @examples
#' \dontrun{
#' }
#' @export
readEcoRiskData <- function( files_reference,
                          files_scenario,
                          saveFile = NULL,
                          export = FALSE,
                          time_span_reference,
                          time_span_scenario,
                          nitrogen,
                          debug = FALSE
                          ) {

  nyears_ref <- length(time_span_reference)
  nyears_scen <- length(time_span_scenario)

  fileType <- tools::file_ext(files_reference$grid)
    
  if (fileType %in% c("json","clm")) {
    # read grid
    grid <- lpjmlkit::read_io(
      files_reference$grid,
    )$data %>% drop()
    # calculate cell area
    cellArea <- lpjmlkit::calc_cellarea(grid[, 2])
    lat <- grid[,2]
    lon <- grid[,1]

    ncells <- length(lat)
    ncfts <- lpjmlkit::read_meta(files_scenario$cftfrac)$nbands
    nbfts <- lpjmlkit::read_meta(files_scenario$fpc_bft)$nbands
    npfts <- lpjmlkit::read_meta(files_scenario$fpc)$nbands - 1
    soillayers <- lpjmlkit::read_meta(files_scenario$swc)$nbands
    ### read in lpjml output
    # for vegetation_structure_change (fpc,fpc_bft,cftfrac)
    print("Reading in fpc,fpc_bft,cftfrac")

    cft_scen <- aperm(lpjmlkit::read_io(
        files_scenario$cftfrac, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))
            
    bft_scen <- aperm(lpjmlkit::read_io(
        files_scenario$fpc_bft, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))

    fpc_scen <- aperm(lpjmlkit::read_io(
        files_scenario$fpc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))
    
    if (file.exists(files_reference$cftfrac)){
        cft_ref <- aperm(lpjmlkit::read_io(
            files_reference$cftfrac, 
            subset = list(year = as.character(time_span_reference))) %>%
            lpjmlkit::transform(to = c("year_month_day")) %>%
            lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))
    }else{
       cft_ref <- cft_scen*0
    }

    if (file.exists(files_reference$fpc_bft)){
    bft_ref <- aperm(lpjmlkit::read_io(
        files_reference$fpc_bft, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))
    }else{
       bft_ref <- bft_scen*0
    }     

    fpc_ref <- aperm(lpjmlkit::read_io(
        files_reference$fpc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)), c(1,3,2))

    # cffiles = ( firec rh_harvest npp ) yearly carbon fluxes
    print("Reading in firec, rh_harvest, npp")

    rh_scen <- lpjmlkit::read_io(
        files_scenario$rh, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    if (file.exists(files_scenario$harvestc)){
      harvest_scen <- lpjmlkit::read_io(
          files_scenario$harvestc, 
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    }else if (file.exists(files_scenario$pft_harvestc)){
      harvest_scen <- lpjmlkit::read_io(
          files_scenario$pft_harvestc, 
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum,band = sum))%>% drop()  
    }else{
      stop("Missing harvestc output in scenario folder.")
    }

    if (file.exists(files_scenario$rharvestc)){
      rharvest_scen <- lpjmlkit::read_io(
          files_scenario$rharvestc, 
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    }else if (file.exists(files_scenario$pft_rharvestc)){
      rharvest_scen <- lpjmlkit::read_io(
          files_scenario$pft_rharvestc,
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum,band = sum))%>% drop()
    }else{
      stop("Missing rharvestc output in scenario folder.")
    }

    rh_harvest_scen  <-  rh_scen + harvest_scen + rharvest_scen

    firec_scen <- lpjmlkit::read_io(
        files_scenario$firec, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    npp_scen <- lpjmlkit::read_io(
        files_scenario$npp, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    rh_ref <- lpjmlkit::read_io(
        files_reference$rh, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    if (file.exists(files_reference$harvestc)){
    harvest_ref <- lpjmlkit::read_io(
        files_reference$harvestc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    }else if (file.exists(files_reference$pft_harvestc)){
      harvest_ref <- lpjmlkit::read_io(
          files_reference$pft_harvestc, 
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum,band = sum))%>% drop()
    }else{
       print("No harvest output available for reference period, setting to 0.")
       harvest_ref <- harvest_scen*0
    }

    if (file.exists(files_reference$rharvestc)){
    rharvest_ref <- lpjmlkit::read_io(
        files_reference$rharvestc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    }else if (file.exists(files_reference$pft_rharvestc)){
      rharvest_ref <- lpjmlkit::read_io(
          files_reference$pft_rharvestc, 
          subset = list(year = as.character(time_span_scenario))) %>%
          lpjmlkit::transform(to = c("year_month_day")) %>%
          lpjmlkit::as_array(aggregate = list(month = sum,band = sum))%>% drop()
    }else{
      print("No rharvest output available for reference period, setting to 0.")
      rharvest_ref <- rharvest_scen*0
    }

    rh_harvest_ref  <-  rh_ref + harvest_ref + rharvest_ref

    firec_ref <- lpjmlkit::read_io(
        files_reference$firec, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    npp_ref <- lpjmlkit::read_io(
        files_reference$npp, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    # wffiles = (evapinterc runoff transp) - yearly water fluxes
    print("Reading in evapinterc, runoff, transp")

    evap_ref <- lpjmlkit::read_io(
        files_reference$evap, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    interc_ref <- lpjmlkit::read_io(
        files_reference$interc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    evapinterc_ref <- evap_ref + interc_ref
 
    runoff_ref <- lpjmlkit::read_io(
        files_reference$runoff, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    transp_ref <- lpjmlkit::read_io(
        files_reference$transp, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    evap_scen <- lpjmlkit::read_io(
        files_scenario$evap, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    interc_scen <- lpjmlkit::read_io(
        files_scenario$interc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    evapinterc_scen <- evap_scen + interc_scen
 
    runoff_scen <- lpjmlkit::read_io(
        files_scenario$runoff, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
    transp_scen <- lpjmlkit::read_io(
        files_scenario$transp, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    # csfiles = ( soillitc vegc_avg ) #carbon pools
    print("Reading in soillitc, vegc")

    soil_ref <- lpjmlkit::read_io(
        files_reference$soilc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    litc_ref <- lpjmlkit::read_io(
        files_reference$litc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    soillitc_ref <- soil_ref + litc_ref

    vegc_ref <- lpjmlkit::read_io(
        files_reference$vegc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    soil_scen <- lpjmlkit::read_io(
        files_scenario$soilc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    litc_scen <- lpjmlkit::read_io(
        files_scenario$litc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    soillitc_scen <- soil_scen + litc_scen

    vegc_scen <- lpjmlkit::read_io(
        files_scenario$vegc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()
  
    # water pools = (swcsum discharge)
    print("Reading in swcsum, discharge")
    swcsum_ref <- lpjmlkit::read_io(
        files_reference$swc, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum, band = sum)) %>% drop()

    swcsum_scen <- lpjmlkit::read_io(
        files_scenario$swc, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum, band = sum)) %>% drop()

    discharge_ref <- lpjmlkit::read_io(
        files_reference$discharge, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    discharge_scen <- lpjmlkit::read_io(
        files_scenario$discharge, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()


    # allfiles = (firef) # other system-internal processes
    print("Reading in  firef")

    firef_ref <- lpjmlkit::read_io(
        files_reference$firef, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

    firef_scen <- lpjmlkit::read_io(
        files_scenario$firef, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()


   # nitrogen variables
   if (nitrogen) {
      print("Reading in n-pools: soilnh4, soilno3 + fluxes: leaching, bnf, n_volatilization, n2o_nit, n2o_denit n2_emis")
      # reference state
      # pools: soilnh4, soilno3
      soilnh4_ref <- lpjmlkit::read_io(
        files_reference$soilnh4, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      soilno3_ref <- lpjmlkit::read_io(
        files_reference$soilno3, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      vegn_ref <- lpjmlkit::read_io(
        files_reference$vegn, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      # fluxes: leaching, n2o_nit, n2o_denit n2_emis, bnf, n_volatilization
      leaching_ref <- lpjmlkit::read_io(
        files_reference$leaching, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2o_denit_ref <- lpjmlkit::read_io(
        files_reference$n2o_denit, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2o_nit_ref <- lpjmlkit::read_io(
        files_reference$n2o_nit, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2_emis_ref <- lpjmlkit::read_io(
        files_reference$n2_emis, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      bnf_ref <- lpjmlkit::read_io(
        files_reference$bnf, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n_volatilization_ref <- lpjmlkit::read_io(
        files_reference$n_volatilization, 
        subset = list(year = as.character(time_span_reference))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      # Calculating compound n emissions vector
      aggregated_n_emissions_ref <- n_volatilization_ref + n2o_nit_ref + n2o_denit_ref + n2_emis_ref
      soiln_ref <- soilno3_ref + soilnh4_ref

      # scenario state
      # pools: soilnh4, soilno3
      soilnh4_scen <- lpjmlkit::read_io(
        files_scenario$soilnh4, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      soilno3_scen <- lpjmlkit::read_io(
        files_scenario$soilno3, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      vegn_scen <- lpjmlkit::read_io(
        files_scenario$vegn, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      # fluxes: leaching, n2o_nit, n2o_denit n2_emis, bnf, n_volatilization
      leaching_scen <- lpjmlkit::read_io(
        files_scenario$leaching, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2o_denit_scen <- lpjmlkit::read_io(
        files_scenario$n2o_denit, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2o_nit_scen <- lpjmlkit::read_io(
        files_scenario$n2o_nit, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n2_emis_scen <- lpjmlkit::read_io(
        files_scenario$n2_emis, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      bnf_scen <- lpjmlkit::read_io(
        files_scenario$bnf, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      n_volatilization_scen <- lpjmlkit::read_io(
        files_scenario$n_volatilization, 
        subset = list(year = as.character(time_span_scenario))) %>%
        lpjmlkit::transform(to = c("year_month_day")) %>%
        lpjmlkit::as_array(aggregate = list(month = sum)) %>% drop()

      # Calculating compound n emissions vector
      aggregated_n_emissions_scen <- n_volatilization_scen + n2o_nit_scen + n2o_denit_scen + n2_emis_scen
      soiln_scen <- soilno3_scen + soilnh4_scen


      if (debug){
        nitrogen_scen <- list(n_volatilization = n_volatilization_scen,
                              n2o_nit = n2o_nit_scen,
                              n2o_denit = n2o_denit_scen, 
                              n2_emis = n2_emis_scen,
                              leaching = leaching_scen,
                              bnf = bnf_scen
                              )
        nitrogen_ref <- list(n_volatilization = n_volatilization_ref,
                              n2o_nit = n2o_nit_ref,
                              n2o_denit = n2o_denit_ref, 
                              n2_emis = n2_emis_ref,
                              leaching = leaching_ref,
                              bnf = bnf_ref
                              )
        save(nitrogen_scen,nitrogen_ref,file="/p/projects/open/Fabian/Metrics/nitrogen_states_debug.RData")
                
        # load("/p/projects/open/Fabian/Metrics/nitrogen_states_debug.RData")
        # test_fol <- "/p/projects/open/Fabian/Metrics/test/"
        # for (i in 1:length(names(nitrogen_ref))){
        #   name <- names(nitrogen_ref)[i]
        #   mean_ref <- rowMeans(nitrogen_ref[[i]])
        #   mean_scen <- rowMeans(nitrogen_scen[[i]])
        #   change <- mean_scen/mean_ref
        #   change_abs <- mean_scen-mean_ref
        #   pow2max_val <- round(log2(max(change_abs)),0)
        #   lpjmliotools::plotGlobal(file = paste0(test_fol,name,"_change.png"), data = change*100, title = paste(name," change in %"), pow2min = 1, pow2max = 12,type="exp")
        #   lpjmliotools::plotGlobal(file = paste0(test_fol,name,"_change_abs.png"), data = change_abs, title = paste(name," absolute change in gN/m2"), pow2min = -5, pow2max = 10,type="exp")
        # }
      
      }
    } #end if nitrogen
  }else if (fileType == "nc") { # to be added
      stop("nc reading has not been updated to latest functionality. \
            please contact Fabian")
  }else{
      stop(paste0("Unrecognized file type (",fileType,")"))
  }

  if (nitrogen) {
    require(abind) 
    state_ref <- abind(  firec_ref,                  #  1
                         rh_harvest_ref,             #  2
                         npp_ref,                    #  3
                         evapinterc_ref,             #  4
                         runoff_ref,                 #  5
                         transp_ref,                 #  6   
                         soillitc_ref,               #  7
                         vegc_ref,                   #  8
                         swcsum_ref,                 #  9
                         discharge_ref,              #  10
                         firef_ref,                  #  11
                         soiln_ref,                  #  12
                         vegn_ref,                   #  13
                         leaching_ref,               #  14
                         bnf_ref,                    #  15
                         aggregated_n_emissions_ref, #  16
                         along = 3)

    state_scen <- abind( firec_scen,                  #  1
                         rh_harvest_scen,             #  2
                         npp_scen,                    #  3
                         evapinterc_scen,             #  4
                         runoff_scen,                 #  5
                         transp_scen,                 #  6
                         soillitc_scen,               #  7
                         vegc_scen,                   #  8
                         swcsum_scen,                 #  9
                         discharge_scen,              #  10
                         firef_scen,                  #  11
                         soiln_scen,                  #  12
                         vegn_scen,                   #  13
                         leaching_scen,               #  14
                         bnf_scen,                    #  15
                         aggregated_n_emissions_scen, #  16
                         along = 3)
    di <- dimnames(state_ref)
    var_names <- c("firec","rh_harvest","npp","evapinterc","runoff","transp","soillitc","vegc","swcsum","discharge","firef","soiln","vegn","leaching","bnf","aggregated_n_emissions")
    di[[3]] <- var_names
    dimnames(state_ref) <- di
    dimnames(state_scen) <- di
  }else{
    require(abind)
    state_ref <- abind( firec_ref,
                        rh_harvest_ref,
                        npp_ref,
                        evapinterc_ref,
                        runoff_ref,
                        transp_ref,
                        soillitc_ref,
                        vegc_ref,
                        swcsum_ref,
                        discharge_ref,
                        firef_ref,
                        along = 3)
    state_scen <- abind(firec_scen,
                        rh_harvest_scen, 
                        npp_scen,
                        evapinterc_scen,
                        runoff_scen,
                        transp_scen,
                        soillitc_scen,
                        vegc_scen,
                        swcsum_scen,
                        discharge_scen,
                        firef_scen,
                        along = 3)
    di <- dimnames(state_ref)
    var_names <- c("firec","rh_harvest","npp","evapinterc","runoff","transp","soillitc","vegc","swcsum","discharge","firef")
    di[[3]] <- var_names
    dimnames(state_ref) <- di
    dimnames(state_scen) <- di
  }

  if (!(is.null(saveFile))) {
    print(paste0("Saving data to: ",saveFile))
    save(state_ref,state_scen,fpc_ref,fpc_scen,
          bft_ref,bft_scen,cft_ref,cft_scen,lat,lon,cellArea,file = saveFile
        )
  }
  return(list(state_ref = state_ref, state_scen = state_scen, 
               fpc_ref = fpc_ref, fpc_scen = fpc_scen, bft_ref = bft_ref,
               bft_scen = bft_scen, cft_ref = cft_ref, cft_scen = cft_scen,
               lat = lat, lon = lon, cellArea = cellArea
               )
        )
}

#' Calculates changes in vegetation structure (vegetation_structure_change)
#'
#' Utility function to calculate changes in vegetation structure (vegetation_structure_change)
#' for calculation of EcoRisk
#'
#' @param fpcRef reference fpc array (dim: [ncells,npfts+1])
#' @param fpcScen scenario fpc array (dim: [ncells,npfts+1])
#' @param bftRef reference bft array (dim: [ncells,nbfts])
#' @param bftScen scenario bft array (dim: [ncells,nbfts])
#' @param cftRef reference cft array (dim: [ncells,ncfts])
#' @param cftScen scenario cft array (dim: [ncells,ncfts])
#' @param weighting apply "old" (Ostberg-like), "new", or "equal" weighting of vegetation_structure_change weights (default "equal")
#'
#' @return vegetation_structure_change array of size ncells with the vegetation_structure_change value [0,1] for each cell
#'
#' @examples
#' \dontrun{
#' vegetation_structure_change <- calcDeltaV(fpcRef = fpc_ref_mean,fpcScen = apply(fpc_scen,c(1,2),mean),
#'           bftRef = bft_ref_mean,bftScen = apply(bft_scen,c(1,2),mean),
#'           cftRef = cft_ref_mean, cftScen = apply(cft_scen,c(1,2),mean),
#'           weighting = "equal")
#' }
#' @export
calcDeltaV <- function(fpcRef, 
                       fpcScen, 
                       bftRef, 
                       bftScen, 
                       cftRef, 
                       cftScen,
                       weighting = "equal") {
   di <- dim(fpcRef)
   ncells <- di[1]
   npfts <- di[2] - 1

   fpcRef[fpcRef < 0] = 0
   fpcScen[fpcScen < 0] = 0
   bftRef[bftRef < 0] = 0
   bftScen[bftScen < 0] = 0
   cftRef[cftRef < 0] = 0
   cftScen[cftScen < 0] = 0
   if (npfts == 9) {
      # barren     =      crop area             natural vegetation area               barren under bioenergy trees
      barrenAreaRef <- 1 - rowSums(cftRef) - rowSums(fpcRef[,2:10])*fpcRef[,1] + rowSums(cftRef[,c(16,32)])*(1 - rowSums(bftRef[,c(1:4,7:10)]))
      barrenAreaRef[barrenAreaRef < 0] <- 0
      treeAreaRef <- array(0,dim = c(ncells,11))
      treeAreaRef[,1:7] <- fpcRef[,2:8]*fpcRef[,1] # natural tree area fractions scaled by total natural frac
      treeAreaRef[,8:9] <- cftRef[,16]*bftRef[,1:2]/rowSums(bftRef[,c(1,2,4)]) # # fraction of rainfed tropical and temperate BE trees scaled by total rainfed bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      treeAreaRef[,10:11] <- cftRef[,32]*bftRef[,7:8]/rowSums(bftRef[,c(7,8,10)]) # fraction of irrigated tropical and temperate BE trees scaled by total irrigated bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      grassAreaRef <- array(0,dim = c(ncells,20))
      grassAreaRef[,1:2] <- fpcRef[,9:10]*fpcRef[,1] # natural grass
      grassAreaRef[,3:15] <- cftRef[,1:13] + cftRef[,17:29] # crops
      grassAreaRef[,16] <- cftRef[,14] # managed grass rf
      grassAreaRef[,17] <- cftRef[,30] # managed grass irr
      grassAreaRef[,18] <- cftRef[,15] + cftRef[,31] # bioenergy grass
      grassAreaRef[,19] <- cftRef[,16]*bftRef[,4]/rowSums(bftRef[,c(1,2,4)]) # fraction of rainfed grass under bioenergy trees
      grassAreaRef[,20] <- cftRef[,32]*bftRef[,10]/rowSums(bftRef[,c(7,8,10)]) # fraction of irrigated grass under bioenergy trees

      barrenAreaScen <- 1 - rowSums(cftScen) - rowSums(fpcScen[,2:10])*fpcScen[,1] + rowSums(cftScen[,c(16,32)])*(1 - rowSums(bftScen[,c(1:4,7:10)])) # barren
      barrenAreaScen[barrenAreaScen < 0] = 0
      treeAreaScen <- array(0,dim = c(ncells,11))
      treeAreaScen[,1:7] <- fpcScen[,2:8]*fpcScen[,1] # natural tree area fractions scaled by total natural frac
      treeAreaScen[,8:9] <- cftScen[,16]*bftScen[,1:2]/rowSums(bftScen[,c(1,2,4)]) # fraction of rainfed tropical and temperate BE trees scaled by total rainfed bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      treeAreaScen[,10:11] <- cftScen[,32]*bftScen[,7:8]/rowSums(bftScen[,c(7,8,10)]) # fraction of irrigated tropical and temperate BE trees scaled by total irrigated bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      grassAreaScen <- array(0,dim = c(ncells,20))
      grassAreaScen[,1:2] <- fpcScen[,9:10]*fpcScen[,1] # natural grass
      grassAreaScen[,3:15] <- cftScen[,1:13] + cftScen[,17:29] # crops
      grassAreaScen[,16] <- cftScen[,14] # managed grass rf
      grassAreaScen[,17] <- cftScen[,30] # managed grass irr
      grassAreaScen[,18] <- cftScen[,15] + cftScen[,31] # bioenergy grass
      grassAreaScen[,19] <- cftScen[,16]*bftScen[,4]/rowSums(bftScen[,c(1,2,4)]) # fraction of rainfed grass under bioenergy trees
      grassAreaScen[,20] <- cftScen[,32]*bftScen[,10]/rowSums(bftScen[,c(7,8,10)]) # fraction of irrigated grass under bioenergy trees

      treeAttributes <- matrix(c( #evergreenness,needleleavedness,tropicalness,borealness,naturalness
         c(  1,    0,  1,  0,  1),#1 TrBE
         c(  0,    0,  1,  0,  1),#2 TrBR
         c(  1,    1,  0,  0,  1),#3 TeNE
         c(  1,    0,  0,  0,  1),#4 TeBE
         c(  0,    0,  0,  0,  1),#5 TeBS
         c(  1,    1,  0,  1,  1),#6 BoNE
         c(  0, 0.25,  0,  1,  1),#7 BoS (including larchs)
         c(  1,    0,  1,  0,  0),#8 TrBi tropical bioenergy rainfed
         c(  0,    0,  0,  0,  0),#9 TeBi temperate bioenergy rainfed
         c(  1,    0,  1,  0,  0),#10 TrBi tropical bioenergy irrigated
         c(  0,    0,  0,  0,  0) #11 TeBi temperate bioenergy irrigated
      ),nrow = 11,byrow = T)

      if (weighting == "equal") {
        treeWeights <- c(0.2, 0.2 ,0.2, 0.2, 0.2)
      }else if (weighting == "new") { # changed compared to Sebastian
        treeWeights <- c(0.2, 0.2 ,0.3, 0.3, 0.3)/1.3
      }else if (weighting == "old") { # Sebastian's method (no downscaling to weightsum 1)
        treeWeights <- c(0.2, 0.2 ,0.3, 0.3, 0.3)
      }else{
        stop("Unknown method of weighting.")
      }

      grassAttributes <- array(0,dim = c(ncells,20,2))
      #1 C3grass
      #2 C4grass
      #3 TemperateCereals
      #4 Rice
      #5 Maize
      #6 TropicalCereals
      #7 Pulses
      #8 TemperateRoots
      #9 TropicalRoots
      #10 Sunflower
      #11 Soybean
      #12 Groundnut
      #13 Rapeseed
      #14 Sugarcane
      #15 Others
      #16 Managed grass rainfed
      #17 Managed grass irrigated
      #18 Bioenergy grass
      #19 Grass under rainfed Bioenergy trees
      #20 Grass under irrigated Bioenergy trees
      grassAttributes[,,1] <- rep(c(0,1,0,1,1,1,0.5,0,1,0.5,1,1,0.5,1,0.5,NA,NA,1,NA,NA),each = ncells) #tropicalness
      grassAttributes[,,2] <- rep(c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),each = ncells) #naturalness
      dynGrassAttributes <- cbind(bftScen[,6]/rowSums(bftScen[,5:6]),bftScen[,12]/rowSums(bftScen[,11:12])) # dynamic share of tropicalness for rf/irr grasslands taken from ratio of bioenergy grasses
      dynGrassAttributes[!is.finite(dynGrassAttributes)] <- 0
      grassAttributes[,16:17,1] <- dynGrassAttributes # managed grass rf/irr
      grassAttributes[,19:20,1] <- dynGrassAttributes # grass under biotrees rf/irr (taken from managed grass)

      if (weighting == "equal") {
        grassWeights <- c(0.2, 0.2)
      }else if (weighting == "new") { # changed compared to Sebastian
        grassWeights <- c(0.5,0.5)
      }else if (weighting == "old") { # Sebastian's method (no downscaling to weightsum 1)
        grassWeights <- c(0.3,0.3)
      }else{
        stop("Unknown method of weighting.")
      }

   }else if (npfts == 11) {
      # barren     =      crop area             natural vegetation area               barren under bioenergy trees
      barrenAreaRef <- 1 - rowSums(cftRef) - rowSums(fpcRef[,2:12])*fpcRef[,1] + rowSums(cftRef[,c(16,32)])*(1 - rowSums(bftRef[,c(4:9,13:18)]))
      barrenAreaRef[barrenAreaRef < 0] <- 0
      treeAreaRef <- array(0,dim = c(ncells,12))
      treeAreaRef[,1:8] <- fpcRef[,2:9]*fpcRef[,1] # natural tree area fractions scaled by total natural frac
      treeAreaRef[,9:10] <- cftRef[,16]*bftRef[,7:8]/rowSums(bftRef[,4:8]) # fraction of rainfed tropical and temperate BE trees scaled by total rainfed bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      treeAreaRef[,11:12] <- cftRef[,32]*bftRef[,16:17]/rowSums(bftRef[,13:17]) # fraction of irrigated tropical and temperate BE trees scaled by total irrigated bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      grassAreaRef <- array(0,dim = c(ncells,21))
      grassAreaRef[,1:3] <- fpcRef[,10:12]*fpcRef[,1] # natural grass
      grassAreaRef[,4:16] <- cftRef[,1:13] + cftRef[,17:29] # crops
      grassAreaRef[,17] <- cftRef[,14] # managed grass rf
      grassAreaRef[,18] <- cftRef[,30] # managed grass irr
      grassAreaRef[,19] <- cftRef[,15] + cftRef[,31] # bioenergy grass
      grassAreaRef[,20] <- cftRef[,16]*rowSums(bftRef[,4:6])/rowSums(bftRef[,4:8]) # fraction of rainfed grass under bioenergy trees
      grassAreaRef[,21] <- cftRef[,32]*rowSums(bftRef[,13:15])/rowSums(bftRef[,13:17]) # fraction of irrigated grass under bioenergy trees

      # barren     =      crop area             natural vegetation area               barren under bioenergy trees
      barrenAreaScen <- 1 - rowSums(cftScen) - rowSums(fpcScen[,2:12])*fpcScen[,1] + rowSums(cftScen[,c(16,32)])*(1 - rowSums(bftScen[,c(4:9,13:18)]))
      barrenAreaScen[barrenAreaScen < 0] <- 0
      treeAreaScen <- array(0,dim = c(ncells,12))
      treeAreaScen[,1:8] <- fpcScen[,2:9]*fpcScen[,1] # natural tree area fractions scaled by total natural frac
      treeAreaScen[,9:10] <- cftScen[,16]*bftScen[,7:8]/rowSums(bftScen[,4:8]) # fraction of rainfed tropical and temperate BE trees scaled by total rainfed bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      treeAreaScen[,11:12] <- cftScen[,32]*bftScen[,16:17]/rowSums(bftScen[,13:17]) # fraction of irrigated tropical and temperate BE trees scaled by total irrigated bioenergy tree area and relative fpc of bioenergy trees and grass under bioenergy trees
      grassAreaScen <- array(0,dim = c(ncells,21))
      grassAreaScen[,1:3] <- fpcScen[,10:12]*fpcScen[,1] # natural grass
      grassAreaScen[,4:16] <- cftScen[,1:13] + cftScen[,17:29] # crops
      grassAreaScen[,17] <- cftScen[,14] # managed grass rf
      grassAreaScen[,18] <- cftScen[,30] # managed grass irr
      grassAreaScen[,19] <- cftScen[,15] + cftScen[,31] # bioenergy grass
      grassAreaScen[,20] <- cftScen[,16]*rowSums(bftScen[,4:6])/rowSums(bftScen[,4:8]) # fraction of rainfed grass under bioenergy trees
      grassAreaScen[,21] <- cftScen[,32]*rowSums(bftScen[,13:15])/rowSums(bftScen[,13:17]) # fraction of irrigated grass under bioenergy trees

      treeAttributes <- matrix(c( #evergreenness,needleleavedness,tropicalness,borealness,naturalness
         c(  1,    0,  1,  0,  1),#1 TrBE
         c(  0,    0,  1,  0,  1),#2 TrBR
         c(  1,    1,  0,  0,  1),#3 TeNE
         c(  1,    0,  0,  0,  1),#4 TeBE
         c(  0,    0,  0,  0,  1),#5 TeBS
         c(  1,    1,  0,  1,  1),#6 BoNE
         c(  0,    0,  0,  1,  1),#7 BoBS
         c(  0,    1,  0,  1,  1),#8 BoNS
         c(  1,    0,  1,  0,  0),#9 TrBi tropical bioenergy rainfed
         c(  0,    0,  0,  0,  0),#10 TeBi temperate bioenergy rainfed
         c(  1,    0,  1,  0,  0),#11 TrBi tropical bioenergy irrigated
         c(  0,    0,  0,  0,  0) #12 TeBi temperate bioenergy irrigated
      ),nrow = 12,byrow = T)
      if (weighting == "equal") {
        treeWeights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
      }else if (weighting == "new") { # changed compared to Sebastian
        treeWeights <- c(0.2, 0.2, 0.3, 0.3, 0.3)/1.3
      }else if (weighting == "old") { # Sebastian's method (no downscaling to weightsum 1)
        treeWeights <- c(0.2, 0.2, 0.3, 0.3, 0.3)
      }else{
        stop("Unknown method of weighting.")
      }

      grassAttributes <- array(0,dim = c(ncells,21,3))
      #1 C4grass tropic
      #2 C3grass temperate
      #3 C3grass polar
      #4 TemperateCereals
      #5 Rice
      #6 Maize
      #7 TropicalCereals
      #8 Pulses
      #9 TemperateRoots
      #10 TropicalRoots
      #11 Sunflower
      #12 Soybean
      #13 Groundnut
      #14 Rapeseed
      #15 Sugarcane
      #16 Others
      #17 Managed grass rainfed
      #18 Managed grass irrigated
      #19 Bioenergy grass
      #20 Grass under rainfed Bioenergy trees
      #21 Grass under irrigated Bioenergy trees
      grassAttributes[,,1] <- rep(c(1,0,0,0,1,1,1,0.5,0,1,0.5,1,1,0.5,1,0.5,NA,NA,1,NA,NA), each = ncells) #tropicalness
      grassAttributes[,,2] <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,0,NA,NA), each = ncells) #borealness
      grassAttributes[,,3] <- rep(c(1,1,1,0,0,0,0,0  ,0,0,0  ,0,0,0  ,0,0  ,0 ,0 ,0,0 ,0), each = ncells) #naturalness
      dynTropGrassAttributes <- cbind(bftScen[,1]/rowSums(bftScen[,1:3]),    # dynamic share of tropicalness for rf grasslands
                                   bftScen[,10]/rowSums(bftScen[,10:12]), # dynamic share of tropicalness for irr grasslands
                                   bftScen[,4]/rowSums(bftScen[,4:6]),    # dynamic share of tropicalness for grass under rf biotrees
                                   bftScen[,13]/rowSums(bftScen[,13:15])) # dynamic share of tropicalness for grass under irr biotrees
      dynTropGrassAttributes[!is.finite(dynTropGrassAttributes)] <- 0
      grassAttributes[,c(17,18,20,21),1] <- dynTropGrassAttributes # managed grass rf/irr, grass under biotrees rf/irr
      dynBorealGrassAttributes <- cbind(bftScen[,3]/rowSums(bftScen[,1:3]),    # dynamic share of borealness for rf grasslands
                                     bftScen[,12]/rowSums(bftScen[,10:12]), # dynamic share of borealness for irr grasslands
                                     bftScen[,6]/rowSums(bftScen[,4:6]),    # dynamic share of borealness for grass under rf biotrees
                                     bftScen[,15]/rowSums(bftScen[,13:15])) # dynamic share of borealness for grass under irr biotrees
      dynBorealGrassAttributes[!is.finite(dynBorealGrassAttributes)] <- 0
      grassAttributes[,c(17,18,20,21),2] <- dynBorealGrassAttributes # managed grass rf/irr, grass under biotrees rf/irr
      if (weighting == "equal"){
        grassWeights <- c(0.2, 0.2, 0.2)
      }else if (weighting == "old" || weighting == "new"){
        grassWeights <- c(0.3333333,0.3333333,0.3333333)
      }else{
        stop("Unknown method of weighting.")
      }
   }else{
      stop("Unknown number of pfts.")
   }

   # compute vegetation_structure_change
   require(fBasics) # for rowMins
   barrenV <- rowMins(cbind(barrenAreaRef,barrenAreaScen))
   treesV <- rowMins(cbind(rowSums(treeAreaRef,na.rm = T),rowSums(treeAreaScen,na.rm = T)))
   grassV <- rowMins(cbind(rowSums(grassAreaRef,na.rm = T),rowSums(grassAreaScen,na.rm = T)))
   innerSumTrees <- abs(rowSums(treeAreaRef[,]*rep(treeAttributes[,1],each = ncells),na.rm = T) - rowSums(treeAreaScen[,]*rep(treeAttributes[,1],each = ncells),na.rm = T))*treeWeights[1] +       # evergreenness
                  abs(rowSums(treeAreaRef[,]*rep(treeAttributes[,2],each = ncells),na.rm = T) - rowSums(treeAreaScen[,]*rep(treeAttributes[,2],each = ncells),na.rm = T))*treeWeights[2] +       # needleleavedness
                  abs(rowSums(treeAreaRef[,]*rep(treeAttributes[,3],each = ncells),na.rm = T) - rowSums(treeAreaScen[,]*rep(treeAttributes[,3],each = ncells),na.rm = T))*treeWeights[3] +       # tropicalness
                  abs(rowSums(treeAreaRef[,]*rep(treeAttributes[,4],each = ncells),na.rm = T) - rowSums(treeAreaScen[,]*rep(treeAttributes[,4],each = ncells),na.rm = T))*treeWeights[4] +       # borealness
                  abs(rowSums(treeAreaRef[,]*rep(treeAttributes[,5],each = ncells),na.rm = T) - rowSums(treeAreaScen[,]*rep(treeAttributes[,5],each = ncells),na.rm = T))*treeWeights[5]         # naturalness
   if (npfts == 9) {
      innerSumGrasses <- abs(rowSums(grassAreaRef[,]*grassAttributes[,,1],na.rm = T) - rowSums(grassAreaScen[,]*grassAttributes[,,1],na.rm = T))*grassWeights[1] + # tropicalness
                        abs(rowSums(grassAreaRef[,]*grassAttributes[,,2],na.rm = T) - rowSums(grassAreaScen[,]*grassAttributes[,,2],na.rm = T))*grassWeights[2]   # naturalness
   }else if (npfts == 11) {
      innerSumGrasses <- abs(rowSums(grassAreaRef[,]*grassAttributes[,,1],na.rm = T) - rowSums(grassAreaScen[,]*grassAttributes[,,1],na.rm = T))*grassWeights[1] + # tropicalness
                        abs(rowSums(grassAreaRef[,]*grassAttributes[,,2],na.rm = T) - rowSums(grassAreaScen[,]*grassAttributes[,,2],na.rm = T))*grassWeights[2] + # borealness
                        abs(rowSums(grassAreaRef[,]*grassAttributes[,,3],na.rm = T) - rowSums(grassAreaScen[,]*grassAttributes[,,3],na.rm = T))*grassWeights[3]   # naturalness
   }else{
      stop("Unknown number of pfts.")
   }
   vegetation_structure_change <- 1 - barrenV - treesV*(1 - innerSumTrees) - grassV*(1 - innerSumGrasses)
   vegetation_structure_change[vegetation_structure_change < 0] <- 0
   vegetation_structure_change[!is.finite(vegetation_structure_change)] <- 0
   #plotEcoRisktoScreen(data=vegetation_structure_change,title = "vegetation_structure_change")
   return(vegetation_structure_change)
}

################# further EcoRisk utility functions ##################
T_sigmoid_Trafo <- function(x) {
   return( -1/exp(3) + (1 + 1/exp(3))/(1 + exp(-6*(x - 0.5))) )
}
balance <- function(v1, v2) {
   return( 1 - sum(v1*v2)/( sqrt(sum(v1*v1)) * sqrt(sum(v2*v2)) ))
}
std_cellwise <- function(a) {
   return(apply(a,1,sd))
}
global_yearly_weighted_mean <- function(a, cellArea) {
   # a is matrix with dim=c(cells,years)
   # cellArea the corresponding cellArea array with dim=c(cells)
   return( sum(a*cellArea, na.rm = T)/(length(a[1,])*sum(cellArea, na.rm = T)) )
}
globally_weighted_mean_foreach_var <- function(x, cellArea) {
   # x is matrix with dim=c(ncells,vars)
   # cellArea the corresponding cellArea array to x with dim=c(ncells)
   return( colSums(x*cellArea, na.rm = T)/sum(cellArea, na.rm = T) )
}
S_change_to_var_ratio <- function(x, s) {
   return( 1/(1 + exp(-4*(x/s - 2))) )
}
state_Diff_local <- function(ref, scen, epsilon = 10^-4) { # based on Heyder 2011 eq. 6-9; epsilon case handling from code by Ostberg (not documented in papers)
   #' @param ref mean reference state vector of dimension c(ncells,variables)
   #' @param scen mean scenario state vector of dimension c(ncells,variables)
   #' @param epsilon threshold for variables to be treated as 0
   
   #' @returns the length of the difference vector for each cell

   # Ostberg code: case change_metric_lu_comparison_jun2013.c
   di <- dim(ref)
   s_scen <- scen/ref #generally normalize the scenario state vector by the reference state
   s_ref <- array(1, dim = di) #initialize

   # for variables in places, where ref is small (<epsilon), but scen larger (Ostberg code, line 798)
   # Sebastian set back scenario and reference vector, to keep the unscaled values (Ostberg code, line 804)
   cells_ref0 <- abs(ref) < epsilon & abs(scen) > epsilon
   s_scen[cells_ref0] <- scen[cells_ref0]
   s_ref[cells_ref0] <- ref[cells_ref0]

   # for variables in places, where ref and scen are small (<epsilon), return 0 (both are 1, difference is 0) (Ostberg code, line 809)
   s_scen[abs(ref) < epsilon & abs(scen) < epsilon] <- 1 # no change

   # normalize both state vectors by the sqrt(amount of state variables) to ensure length(s_ref)==1
   # (this is part of the weighting in the Ostberg code)
   s_ref <- s_ref/sqrt(di[2])
   s_scen <- s_scen/sqrt(di[2])

   return( sqrt(rowSums((s_scen - s_ref)*(s_scen - s_ref))) ) #length of the local difference vector s_scen (sl2) - s_ref (sl1)
}
state_Diff_global <- function(ref, scen, cellArea, epsilon = 10^-4) { #c based on Heyder 2011 eq. 10-13
   #' @param ref mean reference state vector of dimension c(ncells,variables)
   #' @param scen mean scenario state vector of dimension c(ncells,variables)
   #' @param cellArea area of each cell as a vector of dim=c(ncells)
   #' @param epsilon threshold for variables to be treated as 0

   #' @returns the length of the difference vector for each cell
   di <- dim(ref)
   ncells <- di[1]
   global_mean_ref <- globally_weighted_mean_foreach_var(ref,cellArea)
   global_mean_scen <- globally_weighted_mean_foreach_var(scen,cellArea)

   # if global mean state in ref period is 0 (e.g. for landuse vars in pnv run?) 
   # take the mean scen state instead
   cells_ref0 <- abs(global_mean_ref) < epsilon & abs(global_mean_scen) > epsilon
   global_mean_ref[cells_ref0] <- global_mean_scen[cells_ref0] 
   # if both are 0 take 1, then the division is defined but 0 - 0 leads 
   # to no change, which is what EcoRisk should show
   cells_both0 <- abs(global_mean_ref) < epsilon & abs(global_mean_scen) < epsilon
   global_mean_ref[cells_both0] <- 1

   norm <- rep(global_mean_ref, each = ncells)
   dim(norm) <- dim(ref)
   s_scen <- scen/norm
   s_ref <- ref/norm

   # normalize both state vectors by the sqrt(amount of state variables) to ensure length(s_ref)==1
   # (this is part of the weighting in the Ostberg code)
   s_ref <- s_ref/sqrt(di[2])
   s_scen <- s_scen/sqrt(di[2])

   return( sqrt(rowSums((s_scen - s_ref)*(s_scen - s_ref))) ) #length of the difference vector s_scen (sl2) - s_ref (sl1) for each cell
}
calcComponent <- function(ref, scen, local, cellArea, export = FALSE) {
   # calc mean ref and scen state
   ref_mean <- apply(ref, c(1,3), mean)
   scen_mean <- apply(scen, c(1,3), mean)
   di <- dim(ref)
   ncells <- di[1]
   nyears <- di[2]
   nvars <- di[3]

   if (local) {
      x <- T_sigmoid_Trafo(state_Diff_local(ref = ref_mean, scen = scen_mean))
   }else{
      x <- T_sigmoid_Trafo(state_Diff_global(ref = ref_mean, scen = scen_mean, cellArea = cellArea))
   }
   # calculation of the change-to-variability ratio in my view is mathematically not correctly described in Heyder and Ostberg
   # the way I understand it: recalculate the c/g/b value for each year of the ref period compared to the mean
   # of the ref period as "scenario" and then calc the variability (sd) of that
   sigma_x_ref_list <- array(0,dim = c(ncells, nyears))
   for (i in 1:nyears) {
      if (local) {
         sigma_x_ref_list[,i] <- T_sigmoid_Trafo(state_Diff_local(ref = ref_mean, scen = ref[,i,])) #todo think about
      }else{
         sigma_x_ref_list[,i] <- T_sigmoid_Trafo(state_Diff_global(ref = ref_mean, scen = ref[,i,], cellArea = cellArea))
      }
   }
   sigma_x_ref <- apply(sigma_x_ref_list, 1, sd)
   if (export) exportVars2GlobalEnv(x,sigma_x_ref)

   return(x*S_change_to_var_ratio(x, sigma_x_ref))
}
balanceShift <- function(ref, scen, epsilon = 10^-4) {
   # param ref with dimension c(ncells,nvars)
   # param scen with dimension c(ncells,nvars)

   # first normalize as for local change
   s_scen <- scen/ref
   s_ref <- array(1, dim = dim(ref))

   # for variables in places, where ref is small (<epsilon), but scen larger
   # (Ostberg code, line 798/vector length calc in line 837)
   # set back scenario vector, to keep the unscaled values (Ostberg code, line 805)
   s_scen[abs(ref) < epsilon & abs(scen) > epsilon] <- scen[abs(ref) < epsilon & abs(scen) > epsilon]

   # for variables in places, where ref and scen are small (<epsilon),
   # set scen to 1 (both are 1, difference is 0 -- no change) (Ostberg code, line 809)
   s_scen[abs(ref) < epsilon & abs(scen) < epsilon] <- 1 # results in no change
   abs_ref <- sqrt(rowSums(s_ref*s_ref)) # absa(_ts) in Sebastians code
   abs_scen <- sqrt(rowSums(s_scen*s_scen)) # absb(_ts) in Sebastians code
   b1 <- 1 - (rowSums(s_ref*s_scen)/abs_ref/abs_scen) #=1-angle_ts

   # restrain to the maximum range for the acos function
   b1[b1<0] <- 0 
   b1[b1>2] <- 2
   angle <- acos(1 - b1)*360/2/pi
   angle[b1 == 1] <- 0
   b <- b1*2
   b[angle > 60] <- 1
   return(b)
}
calcEcosystemBalance <- function(ref, scen, export=FALSE) {
  ref_mean <- apply(ref,c(1,3),mean)
  scen_mean <- apply(scen,c(1,3),mean)
  di <- dim(ref)
  ncells <- di[1]
  nyears <- di[2]
  nvars <- di[3]

  b <- balanceShift(ref = ref_mean, scen = scen_mean)
  # calculation of the change-to-variability ratio in my view is mathematically not correctly described in Heyder and Ostberg
  # the way I understand it: recalculate the c/g/b value for each year of the ref period compared to the mean
  # of the ref period as "scenario" and then calc the variability (sd) of that
  sigma_b_ref_list <- array(0,dim = c(ncells, nyears))
  for (i in 1:nyears) {
    sigma_b_ref_list[,i] <- balanceShift(ref = ref_mean, scen = ref[,i,])
  }
  sigma_b_ref <- apply(sigma_b_ref_list, 1, sd)
  if (export) exportVars2GlobalEnv(b, sigma_b_ref)

  return(b*S_change_to_var_ratio(b, sigma_b_ref))
}

#' Create modified EcoRisk data file
#'
#' Function to create a modified EcoRisk data file where each reference cell is 
#' compared to the average reference biome cell. The scenario period is
#' overwritten with the original reference period and all reference cells are 
#' set to the average cell of the prescribed reference biome refBiom
#'
#' @param dataFileIn path to input data
#' @param dataFileOut path to save modified data to
#' @param biome_classes_in biome classes object as returned from classify_biomes
#' @param refBiom reference biome from biome classes that all cells should 
#'        be compared to 
#'
#' @examples
#' \dontrun{
#' }
#' @export
replaceRefDataWithAverageRefBiomeCell <- function(dataFileIn, dataFileOut, biome_classes_in, refBiom){
  if (dataFileIn == dataFileOut) {
    stop("Same file for input and output of data, would overwrite original data. Aborting.")
  }
  load(dataFileIn)

  ref_cells <- which(biome_classes_in$biome_id == refBiom)

  # first set all scen vacrossrs to the ref vars
  state_scen <- state_ref # [1:64240, 1:30, 1:10]
  # mean_state_scen <- mean_state_ref # FS: mean states were removed from data file, removing also here
  fpc_scen <- fpc_ref #
  bft_scen <- bft_ref
  cft_scen <- cft_ref

  di_state <- dim(state_scen)
  di_fpc <- dim(fpc_scen)
  di_bft <- dim(bft_scen)
  di_cft <- dim(cft_scen)
  # now replace all ref cells with that of the mean ref biom cell
  # FS 2022-08-10: keeping the year-to-year variation
  if (length(ref_cells)==1) {
    av_year_state <- state_scen[ref_cells,,]
    fpc_ref <- rep(fpc_scen[ref_cells,,], each = di_fpc[1])
    bft_ref <- rep(bft_scen[ref_cells,,], each = di_bft[1])
    cft_ref <- rep(cft_scen[ref_cells,,], each = di_cft[1])
  }else {
    av_year_state <- apply(state_scen[ref_cells,,],c(2,3),mean)
    fpc_ref <- rep(apply(fpc_scen[ref_cells,,],c(2,3),mean), each = di_fpc[1])
    bft_ref <- rep(apply(bft_scen[ref_cells,,],c(2,3),mean), each = di_bft[1])
    cft_ref <- rep(apply(cft_scen[ref_cells,,],c(2,3),mean), each = di_cft[1])
  }
  state_ref <- rep(av_year_state, each = di_state[1])
  dim(state_ref) <- di_state
  # is the same for each year, thus for the mean just take one year
  # mean_state_ref <- rep(colMeans(av_year_state), each = di_state[1]) 
  # FS: mean states were removed from data file, removing also here
  
  dim(fpc_ref) <- di_fpc
  dim(bft_ref) <- di_bft
  dim(cft_ref) <- di_cft

  # and write out the modified data
  # save(state_ref,mean_state_ref,state_scen,mean_state_scen,fpc_ref,fpc_scen,
  # bft_ref,bft_scen,cft_ref,cft_scen,lat,lon,cellArea,file = dataFileOut)
  save(state_ref,state_scen,fpc_ref,fpc_scen,bft_ref,bft_scen,cft_ref,
       cft_scen,lat,lon,cellArea,file = dataFileOut)

}

#' Create modified EcoRisk data for crosstable
#'
#' Function to create a modified EcoRisk data file where for each biome
#' the average scenario cell is compared to the average scenario cell of all 
#' other biomes. This can then be used to compute a crosstable with the average 
#' difference between each of them as in the SI of Ostberg et al. 2013
#' (Critical impacts of global warming on land ecosystems)
#'
#' @param dataFileIn path to input data
#' @param dataFileOut path to save modified data to
#' @param biome_classes_in biome classes object as returned from classify_biomes
#' @param pickCells pick one specific cell as representative for the biome 
#'        instead of computing the average state 
#'
#' @examples
#' \dontrun{
#' }
#' @export
ecoriskCrossTable <- function(dataFileIn, dataFileOut, biome_classes_in, pickCells = NULL){
  if (dataFileIn == dataFileOut) {
    stop("Same file for input and output of data, would overwrite original data. Aborting.")
  }
  load(dataFileIn)
  
  #save scenario state vectors, they contain relevant data (ref can go)
  state_scen_sav <- state_scen
  fpc_scen_sav <- fpc_scen
  bft_scen_sav <- bft_scen
  cft_scen_sav <- cft_scen

  nbiomes <- max(biome_classes_in$biome_id) # by default 19
  state_ref <- array(0,dim = c(nbiomes,nbiomes,dim(state_scen_sav)[2:3]))
  state_scen <- state_ref
  fpc_ref <- array(0,dim = c(nbiomes,nbiomes,dim(fpc_scen_sav)[2:3]))
  fpc_scen <- fpc_ref
  bft_ref <- array(0,dim = c(nbiomes,nbiomes,dim(bft_scen_sav)[2:3]))
  bft_scen <- bft_ref
  cft_ref <- array(0,dim = c(nbiomes,nbiomes,dim(cft_scen_sav)[2:3]))
  cft_scen <- cft_ref

  # now replace all ref cells with that of the mean ref biome cell
  for (b in sort(unique(biome_classes_in$biome_id))) {
    ref_cells <- which(biome_classes_in$biome_id == b)

    if (is.null(pickCells)) {
      if (length(ref_cells)==1) {
        # average over cells, keeping the average year-to-year variation
        av_state <- state_scen_sav[ref_cells,,]
        av_fpc <- fpc_scen_sav[ref_cells,,]
        av_bft <- bft_scen_sav[ref_cells,,]
        av_cft <- cft_scen_sav[ref_cells,,]
      }else {
        # average over cells, keeping the average year-to-year variation
        av_state <- apply(state_scen_sav[ref_cells,,],c(2,3),mean) 
        av_fpc <- apply(fpc_scen_sav[ref_cells,,],c(2,3),mean)
        av_bft <- apply(bft_scen_sav[ref_cells,,],c(2,3),mean)
        av_cft <- apply(cft_scen_sav[ref_cells,,],c(2,3),mean)
      }
    }else{
      av_state <- state_scen_sav[pickCells[b],,]
      av_fpc <- fpc_scen_sav[pickCells[b],,]
      av_bft <- bft_scen_sav[pickCells[b],,]
      av_cft <- cft_scen_sav[pickCells[b],,]
    }

    state_ref[b,,,] <- rep(av_state, each = nbiomes)
    state_scen[,b,,] <- rep(av_state, each = nbiomes)

    mean_state_ref <- apply(state_ref,c(1,3),mean)
    mean_state_scen <- apply(state_scen,c(1,3),mean)

    fpc_ref[b,,,] <- rep(av_fpc, each = nbiomes)
    fpc_scen[,b,,] <- rep(av_fpc, each = nbiomes)

    bft_ref[b,,,] <- rep(av_bft, each = nbiomes)
    bft_scen[,b,,] <- rep(av_bft, each = nbiomes)

    cft_ref[b,,,] <- rep(av_cft, each = nbiomes)
    cft_scen[,b,,] <- rep(av_cft, each = nbiomes)

  }
  dim(state_ref) <- c(nbiomes*nbiomes,dim(state_scen_sav)[2:3])
  dim(state_scen) <- c(nbiomes*nbiomes,dim(state_scen_sav)[2:3])
  dim(fpc_ref) <- c(nbiomes*nbiomes,dim(fpc_scen_sav)[2:3])
  dim(fpc_scen) <- c(nbiomes*nbiomes,dim(fpc_scen_sav)[2:3])
  dim(bft_ref) <- c(nbiomes*nbiomes,dim(bft_scen_sav)[2:3])
  dim(bft_scen) <- c(nbiomes*nbiomes,dim(bft_scen_sav)[2:3])
  dim(cft_ref) <- c(nbiomes*nbiomes,dim(cft_scen_sav)[2:3])
  dim(cft_scen) <- c(nbiomes*nbiomes,dim(cft_scen_sav)[2:3])

  lat <- rep(0,nbiomes*nbiomes)
  lon <- rep(1,nbiomes*nbiomes)
  cellArea <- rep(2,nbiomes*nbiomes)
  # and write out the modified data
  save(state_ref,mean_state_ref,state_scen,mean_state_scen,fpc_ref,fpc_scen,
       bft_ref,bft_scen,cft_ref,cft_scen,lat,lon,cellArea,file = dataFileOut)

}
################# biome (dis-)aggregation functions ##################

#' Get biome names
#'
#' Returns biome names with variable length (abbreviated, short, or full)
#'
#' @param biomeNameLength integer chose from 1,2,3 for abbreviated, short, 
#'                        or full biome names
#'
#' @examples
#' \dontrun{
#' }
#' @export
get_biome_names <- function(biomeNameLength = 2) {
  biome_mapping <- read.csv(file = system.file("extdata", "biomes.csv", package = "biospheremetrics"),sep = ";")
  if (biomeNameLength == 1) biome_class_names <- biome_mapping$abbreviation
  else if (biomeNameLength == 2) biome_class_names <- biome_mapping$short_name
  else if (biomeNameLength == 3) biome_class_names <- biome_mapping$name
  else stop(paste0("Value for parameter biomeNameLength out of range 1,2,3 - was given as: ",biomeNameLength))
  
  return(biome_class_names) 
}

#' Averages EcoRisk values across regions
#'
#' Returns the average value across either 4 regions or all (19) biomes for EcoRisk
#' and each of the subcomponents for each
#'
#' @param data List object, of which every item should be disaggregated 
#' @param biome_class biome class list object as returned by classify_biomes
#' @param type string controlling whether to return  minimum,mean,maximum 
#'        ("minmeanmax") or Q10,Q50,Q90 ("quantile") - default: "quantile"
#' @param classes string for into how many regions should be disaggregated 
#'        "4biomes" (tropics/temperate/boreal/arctic) or "allbiomes"
#'
#' @examples
#' \dontrun{
#' disaggregateIntoBiomes(ecorisk = ecorisk,
#'                    biome_class = biome_classes,
#'                    type = "quantile",classes = "4biomes")
#' }
#' @export
disaggregateIntoBiomes <- function(data,
                                   biome_class,
                                   type = "quantile",
                                   classes = "4biomes"
                                  ) {
  
  di <- dim(data[[1]])
  comp_names <- names(data)
  if (type == "minmeanmax") {
    type_names <- c("min","mean","max")
  }else if (type == "quantile"){
    type_names <- c("Q10","Q50","Q90")
  }
  if (length(di) > 1){
    slices <- di[2]
  }else{
    slices <- 1
  }
  if (classes == "4biomes") {
    tropics <- c(1,2,9,10,11)
    temperate <- c(3,4,5,12,13,14)
    boreal <- c(6,7,8)
    arctic <- c(15,16)
    cell_list <- list(
                  tropical_cells = which(biome_class$biome_id %in% tropics),
                  temperate_cells = which(biome_class$biome_id %in% temperate),
                  boreal_cells = which(biome_class$biome_id %in% boreal),
                  arctic_cells = which(biome_class$biome_id %in% arctic) 
                  )
    nclasses <- 4
  }else if (classes == "allbiomes") {
    nclasses <- max(unique(biome_class$biome_id))
  }else{
    stop(paste0("Unknown parameter classes: ",classes,
                ", should be either '4biomes' or 'allbiomes'"))
  }

  data_dims <- length(data)
  # c(biome,data_components,min/median/max)
  data_biomes <- array(0,dim = c(nclasses,data_dims,3,slices)) 
  if (classes == "4biomes") { # aggregate to trop/temp/boreal/arctic
    for (s in 1:slices) {
      for (b in 1:nclasses) {
        for (c in 1:data_dims) {
          if (type == "minmeanmax") {
            data_biomes[b,c,,s] <- c( min(data[[c]][cell_list[[b]],s],na.rm = T),
                                    mean(data[[c]][cell_list[[b]],s],na.rm = T),
                                    max(data[[c]][cell_list[[b]],s],na.rm = T) )
          }else if (type == "quantile") {
            data_biomes[b,c,,s] <- c( quantile(data[[c]][cell_list[[b]],s], 
                                            probs = c(0.1,0.5,0.9), na.rm = T) )
          }else{stop(paste("type",type,
                    "unknown. please choose either 'quantile' or 'minmeanmax'"))
          }# end if
        }# end for
      }# end for
    }# end for 
    biome_names <- c("tropics","temperate","boreal","arctic")
    dimnames(data_biomes) <- list(biome_names,comp_names,type_names,1:slices)
  }else if (classes == "allbiomes") { #calculate all biomes separately
    for (s in 1:slices) {
      for (b in 1:nclasses) {
        for (c in 1:data_dims) {
          if (type == "minmeanmax") {
            data_biomes[b,c,,s] <- c(
                      min(data[[c]][which(biome_class$biome_id == b),s],na.rm = T),
                      mean(data[[c]][which(biome_class$biome_id == b),s],na.rm = T),
                      max(data[[c]][which(biome_class$biome_id == b),s],na.rm = T) )
          }else if (type == "quantile") {
            data_biomes[b,c,,s] <- c(
                            quantile(data[[c]][which(biome_class$biome_id == b),s], 
                                      probs = c(0.1,0.5,0.9), na.rm = T
                                      ) 
                                  )
          }else{stop(paste("type",type,
                    "unknown. please choose either 'quantile' or 'minmeanmax'"))
          }# end if
        }# end for
      }# end for
    }# end for 
    biome_names <- biome_class$biome_names
    dimnames(data_biomes) <- list(biome_names,comp_names,type_names,1:slices)
  }else{
    stop(paste0("Unknown parameter classes: ",classes,
                ", should be either '4biomes' or 'allbiomes'"))
  }
  return(drop(data_biomes))
}

#' Calculate ecorisk with each biomes average cell
#'
#' Function to calculate ecorisk with each biomes average cell
#' as a measure of internal variability
#' 
#' @param biome_classes biome classes object as returned by classify biomes, 
#'                      calculated for dataFile_base
#' @param dataFile_base base EcoRisk to compute differences with (only ref is relevant)
#' @param intra_biome_distrib_file file to additionally write results to
#' @param create create new modified files, or read already existing ones?
#' @param res how finegrained the distribution should be (resolution)
#' @param plotting whether plots for each biome should be created
#' @param plot_folder folder to plot into
#' @param time_span_reference suitable 30 year reference period (e.g. c(1901,1930), c(1550,1579))

#' @return data object with distibution - dim: c(biomes,ecorisk_variables,bins)
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
calculateWithinBiomeDiffs <- function(biome_classes, 
                                      dataFile_base, 
                                      intra_biome_distrib_file,
                                      create = FALSE, 
                                      res = 0.05, 
                                      plotting = FALSE, 
                                      plot_folder, 
                                      time_span_reference, 
                                      vars_ecorisk, 
                                      nitrogen = TRUE
                                      ) {
  biomes_abbrv <- get_biome_names(1)
  intra_biome_distrib <- array(0,dim = c(length(biome_classes$biome_names),10,1/res)) # nbiomes,nEcoRiskvars,nHISTclasses

  # start
  for (b in sort(unique(biome_classes$biome_id))) {
    filebase <- strsplit(dataFile_base, "_data.RData")[[1]]
    print(paste0("Calculating differences with biome ",b," (",biome_classes$biome_names[b],")"))
    dataFile = paste0(filebase,"_compared_to_average_",biomes_abbrv[b],"_data.RData")
    ecoriskFile = paste0(filebase,"_compared_to_average_",biomes_abbrv[b],"_gamma.RData")
    if (create){
      replaceRefDataWithAverageRefBiomeCell(dataFileIn = dataFile_base, 
                                            dataFileOut = dataFile,
                                            biome_classes_in = biome_classes,
                                            refBiom = b)
      ecorisk <- ecoriskWrapper(folderRef = NULL, # does not need to be specified, as data is read from file
                                folderScen = NULL, # does not need to be specified, as data is read from file
                                readPreviouslySavedData = TRUE, 
                                saveFileData = dataFile, 
                                saveFileEcoRisk = ecoriskFile, 
                                varnames = vars_ecorisk,
                                time_span_reference = time_span_reference,
                                time_span_scenario = time_span_reference,
                                dimensionsOnlyLocal = F,
                                nitrogen = nitrogen
      )
    }else{
      load(ecoriskFile) #contains ecorisk list object
    }

    #compute average values per focus biom
    ref_cells <- which(biome_classes$biome_id == b)
    for (v in 1:10) {
      intra_biome_distrib[b,v,] <- hist(ecorisk[[v]][ref_cells], breaks = seq(0,1,res),plot = F)$counts
      intra_biome_distrib[b,v,] <- intra_biome_distrib[b,v,]/sum(intra_biome_distrib[b,v,])
    }
    if (plotting){
      plotEcoRiskmap(file = paste0(plot_folder,"EcoRisk/compare_ecorisk_to_",biomes_abbrv[b],".png"),
                  focusBiome = b, biome_classes = biome_classes$biome_id,
                  data = ecorisk$ecorisk_total, title = biome_classes$biome_names[b],
                  legendtitle = "", eps = F,titleSize = 2,legYes = T)
      plotEcoRiskmap(file = paste0(plot_folder,"EcoRisk/compare_vegetation_structure_change_to_",biomes_abbrv[b],".png"),
                  focusBiome = b, biome_classes = biome_classes$biome_id,
                  data = ecorisk$vegetation_structure_change, title = biome_classes$biome_names[b],
                  legendtitle = "", eps = F,titleSize = 2,legYes = T)
      plotEcoRiskmap(file = paste0(plot_folder,"EcoRisk/compare_gi_to_",biomes_abbrv[b],".png"),
                  focusBiome = b, biome_classes = biome_classes$biome_id,
                  data = ecorisk$global_importance, title = biome_classes$biome_names[b],
                  legendtitle = "", eps = F,titleSize = 2,legYes = T)
      plotEcoRiskmap(file = paste0(plot_folder,"EcoRisk/compare_lc_to_",biomes_abbrv[b],".png"),
                  focusBiome = b, biome_classes = biome_classes$biome_id,
                  data = ecorisk$local_change, title = biome_classes$biome_names[b],
                  legendtitle = "", eps = F,titleSize = 2,legYes = T)
      plotEcoRiskmap(file = paste0(plot_folder,"EcoRisk/compare_eb_to_",biomes_abbrv[b],".png"),
                  focusBiome = b, biome_classes = biome_classes$biome_id,
                  data = ecorisk$ecosystem_balance, title = biome_classes$biome_names[b],
                  legendtitle = "", eps = F,titleSize = 2,legYes = T)
    }# end if plotting
  }
  ecorisk_dimensions <- c("ecorisk_total", "vegetation_structure_change", "local_change", "global_importance", "ecosystem_balance",
                    "carbon_stocks", "carbon_fluxes", "water_fluxes", "nitrogen_stocks", "nitrogen_fluxes")
  dim(intra_biome_distrib) <- c(biome = 19, variable = 10, bin = 1/res)
  dimnames(intra_biome_distrib) <- list(biome = biomes_abbrv, variable = ecorisk_dimensions, bin = seq(res,1,res))
  save(intra_biome_distrib, file = intra_biome_distrib_file)
  return(intra_biome_distrib)
}
################# EcoRisk plotting functions ##################
#' Plot distribution of similarity within biomes
#'
#' Function to plot the distribution of similarity within biomes
#' 
#' @param data data object with distibution - as returned by 
#'             calculateWithInBiomeDiffs for each subcategory of ecorisk.
#'             dim: c(biomes,bins)
#' @param biomes_abbrv to mask the focusBiome from
#' @param scale scaling factor for distribution. defaults to 1
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title, default empty
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomeInternalDistributionToScreen <- function(data, biomes_abbrv,title = "", 
                                                  legendtitle = "", scale = 1, palette = NULL) {
  di = dim(data)
  bins = di["bin"]
  res = 1/bins
  biomes = di["biome"]
  if (is.null(palette)) palette <- c("white","steelblue1","royalblue",RColorBrewer::brewer.pal(7,"YlOrRd"))
  colIndex <- floor(seq(res/2,1-res/2,res)*10) + 1
  par(mar=c(2,4,0,0),oma=c(0,0,0,0))#bltr
  plot(NA, xlim=c(0,1), ylim=c(0,20), xlab = "EcoRisk", main = title, axes = F, ylab = "")
  axis(side = 2,  labels = F, at = 1:biomes)
  #axis(side = 1, at = seq(0,1,0.1))
  brks <- seq(0,1,0.1)
  fields::image.plot(legend.only=TRUE,col = palette,
               useRaster=FALSE, breaks=brks, horizontal = T,
               lab.breaks=brks,   legend.shrink = 0.925, #legend.width = 1.2, 
               legend.args=list("",side=3, font=2, line=1.5))
  mtext(biomes_abbrv, side = 2, line = 1, at = 1:biomes,las = 2)
  for (b in 1:biomes) {
    rect(xleft = seq(0,1-res,res),xright = seq(res,1,res), ybottom = b, ytop = b+data[b,]*scale, col = palette[colIndex])
  }
}

#' Plot to file distribution of similarity within biomes
#'
#' Function to plot to file the distribution of similarity within biomes
#' 
#' @param data data object with distibution - as returned by 
#'             calculateWithInBiomeDiffs. dim: c(biomes,bins)
#' @param file to write into
#' @param biomes_abbrv to mask the focusBiome from
#' @param scale scaling factor for distribution. defaults to 1
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title, default empty
#' @param eps write as eps or png (default: F -> png)
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomeInternalDistribution <- function(data, file, biomes_abbrv, scale, title = "", legendtitle = "", eps = FALSE, palette = NULL) {
   if (eps) {
      file <- strsplit(file,".",fixed = TRUE)[[1]]
      file <- paste(c(file[1:(length(file) - 1)],"eps"),collapse = ".")
      ps.options(family = c("Helvetica"), pointsize = 18)
      postscript(file, horizontal = FALSE, onefile = FALSE, width = 8, height = 16, paper = "special")
   }else{
      png(file, width = 3, height = 6, units = "in", res = 300, pointsize = 6,type = "cairo")
   }
   plotBiomeInternalDistributionToScreen(data = data, biomes_abbrv = biomes_abbrv, scale = scale, title = title, legendtitle = legendtitle, palette = palette)
   dev.off()
}
#' Plot EcoRisk map to screen
#'
#' Function to plot a global map of EcoRisk values [0-1] per grid cell to screen
#'
#' @param data folder of reference run
#' @param focusBiome highlight the biome with this id and desaturate all other (default NULL -- no highlight)
#' @param biome_classes to mask the focusBiome from
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title
#' @param legYes logical. whether to plot legend or not. defaults to TRUE
#' @param legScale scaling factor for legend. defaults to 1
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskmapToScreen <- function(data,focusBiome = NULL, biome_classes = NULL,
                                 title = "", legendtitle = "", titleSize = 1, legYes = T, palette = NULL) {
   brks <- seq(0,1,0.1)
   data[data < brks[1]] <- brks[1]
   data[data > brks[length(brks)]] <- brks[length(brks)]
   if (is.null(palette)) palette <- c("white","steelblue1","royalblue",RColorBrewer::brewer.pal(7,"YlOrRd"))
   if (!is.null(focusBiome)){
     focus <- data
     focus[!(biome_classes == focusBiome)] <- NA
     palette_lowSat <- adjustcolor(palette,alpha.f = 0.25)
     ra_f <- raster::raster(ncols = 720, nrows = 360)
     ra_f[raster::cellFromXY(ra_f,cbind(lon,lat))] <-  focus
   }
   ra <- raster::raster(ncols = 720, nrows = 360)
   ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
   range <- range(data)
   extent <- raster::extent(c(-180, 180, -60, 90))
   par(mar = c(0,0,1,3), oma = c(0,0,0,0),bty = "n")
   if (is.null(focusBiome)) {
     raster::plot(ra, ext = extent, breaks = brks, col = palette, main = "", legend = FALSE, axes = FALSE)
   }else{
     raster::plot(ra, ext = extent, breaks = brks, col = palette_lowSat, main = "", legend = FALSE, axes = FALSE)
     raster::plot(ra_f, ext = extent, breaks = brks, col = palette, main = "", legend = FALSE, axes = FALSE,add = T)
   }
   title(main = title, line = -2, cex.main = titleSize)
   maps::map('world', add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60,90))
   if (legYes) {
      fields::image.plot(legend.only = TRUE, zlim = range, col = palette, breaks = brks, lab.breaks = brks, legend.shrink = 0.7,
                         legend.args = list(legendtitle, side = 3, font = 2, line = 1)) # removed zlim
   }
}
#' Plot EcoRisk map to file
#'
#' Function to plot a global map of EcoRisk values [0-1] per grid cell to file
#'
#' @param data folder of reference run
#' @param file to write into
#' @param focusBiome highlight the biome with this id and desaturate all other (default NULL -- no highlight)
#' @param biome_classes to mask the focusBiome from
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title
#' @param eps write as eps or png
#' @param legYes logical. whether to plot legend or not. defaults to TRUE
#' @param legScale scaling factor for legend. defaults to 1
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskmap <- function(data, file, focusBiome = NULL, biome_classes = NULL,
                         title = "", legendtitle = "", eps = FALSE, 
                         titleSize = 1, legYes = T, palette = NULL) {
  outFol <- dirname(file)
  dir.create(file.path(outFol),showWarnings = F)
  if (eps) {
    file <- strsplit(file,".",fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)],"eps"),collapse = ".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file, horizontal = FALSE, onefile = FALSE, width = 22, height = 8.5, paper = "special")
  }else{
      png(file, width = 7.25, height = 3.5, units = "in", res = 300, pointsize = 6,type = "cairo")
  }
  plotEcoRiskmapToScreen(data = data, focusBiome = focusBiome, 
      biome_classes = biome_classes, title = title, legendtitle = legendtitle, 
      titleSize = titleSize, legYes = legYes, palette = palette)
  dev.off()
}
#' Plot radial EcoRisk plot to screen
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to screen
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],3[min,mean,max])
#' @param title character string title for plot, default empty
#' @param zoom scaling factor for circle plot. defaults to 1
#' @param type plot type, 'legend1' for variable and color legend,
#'             'legend2' for value legend, or 'regular' (default setting)
#'             for the regular EcoRisk plot
#' @param titleSize scaling factor for tile. defaults to 1
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskradialToScreen <- function(data, 
                                      title = "", 
                                      zoom = 1.0, 
                                      type = "regular", 
                                      titleSize = 2, 
                                      titleline = -2,
                                      quantile = T
                                      ) {
    suppressPackageStartupMessages(require(circlize))
    require(RColorBrewer)
    ecorisk_dims <- length(data[,1])
    if (ecorisk_dims == 11) {
      names <- c( ecorisk = "ecorisk", 
                  deltav = "vegetation\nstructure", local = "local\nchange", 
                  global = "global\nimportance", balance =  "ecosystem\nbalance", 
                  cstocks = "carbon\nstocks", cfluxes = "carbon\nfluxes", 
                  wstocks = "water stocks", wfluxes = "water fluxes", 
                  nstocks = "nitrogen\nstocks", nfluxes = "nitrogen\nfluxes")
      set <- brewer.pal(12, "Set3") #c(blue-green, yellow,violet,red,blue,orange,green,pink,grey,purple,green-blue,yellow-orange)
      colz <- set[c(4,7,8,11,2,3,10,5,1,12,6)] # missing 2,9
      #set <- brewer.pal(9, "Set1") #c(red,blue,green,purple,orange,yellow,brown,pink,grey)
      #colz <- c("limegreen", "darkgreen", "maroon","orchid4","bisque4",
      #         "orangered","sienna",brewer.pal(6, "PuBu")[6], "yellow" , "orange")
      #                  ecorisk    vs       lc       gi      eb          cs      cf     ws       wf       ns      nf
      angles <- matrix(c(90,270, 216,252,  180,216, 144,180, 108,144,  0,30,   -30,0, -60,-30, -90,-60,  60,90, 30,60 ),byrow = T,nrow = length(colz))
    }else if (ecorisk_dims == 10) {
      names <- c( ecorisk = "ecorisk", deltav = "vegetation\nstructure",
                  local = "local\nchange", global = "global\nimportance",
                  balance =  "ecosystem\nbalance", cstocks = "carbon stocks", 
                  cfluxes = "carbon fluxes", wfluxes = "water fluxes", 
                  nstocks = "nitrogen\nstocks", nfluxes = "nitrogen fluxes")
      set <- brewer.pal(12, "Set3") #c(blue-green, yellow,violet,red,blue,orange,green,pink,grey,purple,green-blue,yellow-orange)
      colz <- set[c(4,7,8,11,1,3,10,5,12,6)]
      #set <- brewer.pal(9, "Set1") #c(red,blue,green,purple,orange,yellow,brown,pink,grey)
      #colz <- c("limegreen", "darkgreen", "maroon","orchid4","bisque4",
      #         "orangered","sienna",brewer.pal(6, "PuBu")[6], "yellow" , "orange")
      #                  ecorisk    vs      lc       gi       eb         cs      cf       wf       ns      nf
      angles <- matrix(c(90,270, 216,252, 180,216, 144,180, 108,144,  -18,18, -54,-18, -90,-54,  54,90, 18,54 ),byrow = T,nrow = length(colz))
    }else if (ecorisk_dims == 8) {
      names <- c( ecorisk = "ecorisk", deltav = "vegetation\nstructure",
                  local = "local\nchange", global = "global\nimportance",
                  balance =  "ecosystem\nbalance", cstocks = "carbon\nstocks",
                  cfluxes = "carbon fluxes", wfluxes = "water fluxes")
      colz <- c("darkgoldenrod", brewer.pal(5,"Greens")[5], brewer.pal(6, "Set1")[seq(2, 6, by = 2)],
                rev(brewer.pal(6, "Oranges")[c(4,5)]), brewer.pal(6, "PuBu")[6])
      angles <- matrix(c(234,270, 198,234, 162,198, 126,162, 90,126,     18,54, -18,18, -54,-18),byrow = T,nrow = length(colz))
    }else{
      stop(paste("Unknown number of dimensions for ecorisk data:",ecorisk_dims))
    }
    par(oma = c(0,0,0,0), mar = c(0,0,0,0))
    plot(c(-zoom, zoom), c(-zoom, zoom), type = "n", axes = FALSE, ann = FALSE, asp = 1, main = "")
    title(main = title, line = titleline, cex.main = titleSize)
    if (type == "legend1") {
      draw.sector(0, 360, rou1 = 1)
      ro = c(1,1.1,0.8,1.1,0.8, 1,1,1,1, 1,1)
      for (i in 1:length(angles[,1])) {
         draw.sector(start.degree = angles[i,1] + 90, end.degree = angles[i,2] + 90, col = colz[i], clock.wise = F, rou1 = 0, rou2 = ro[i],border = "black")
      }
      if (ecorisk_dims == 11) {
        text(names,
        #       er    vs   lc   gi   eb   cs   cf   ws   wf   ns  nf
        x = c( 1.1,  1.0, 0.2,-0.8,-1.6, -0.6,0.1, 0.8,1.05,  -1.7,-1.4),
        y = c(-0.15,-0.9,-1.3,-1.3,-0.9,  1.2,1.2, 0.85,0.25,  0.3, 0.85), 
                   adj = 0)
      
      }else if (ecorisk_dims == 10) {
        text(names,x = c(1.1,1.0,0.2,-0.8,-1.6, -0.4,0.7,1.05,   -1.7,-1.5),
              y = c(-0.15,-0.9,-1.3,-1.3,-0.9, 1.2,1,0.25,  0.3,1), adj = 0)
      }else if (ecorisk_dims == 8) {
        text(names,x = c(1.1,0.6,-0.2,-1.2,-1.7, -1.5,-0.4,0.7),y = c(-0.3,-1.1,-1.3,-1,-0.5, 1,1.2,1),adj = 0)
      }else{
        stop(paste("Unknown number of dimensions for ecorisk data:",ecorisk_dims))
      }
      draw.sector(start.degree = (angles[3,1]+angles[3,2])/2+90, end.degree = (angles[3,1]+angles[3,2])/2+90, rou1 = 0.7, rou2 = 1.1)# line lc
      draw.sector(start.degree = -9, end.degree = -9, rou1 = 0.9, rou2 = 1.05)# line ecorisk
      draw.sector(start.degree = (angles[5,1]+angles[5,2])/2+90, end.degree = (angles[5,1]+angles[5,2])/2+90, rou1 = 0.7, rou2 = 1.1)# line eb
      draw.sector(start.degree = 180, end.degree = 180, clock.wise = F, rou1 = -1.2, rou2 = 1.2, border = "black",lwd = 2)
    }else if (type == "legend2") {
      text("+",x = 0, y = 0)
      draw.sector(0, 360, rou1 = 1)
      draw.sector(0, 360, rou1 = 0.65)
      draw.sector(0, 360, rou1 = 0.3)
      draw.sector(start.degree = -18 + 90, end.degree = 18 + 90, clock.wise = F, rou1 = 0.55, border = "black") #sector
      draw.sector(start.degree = 90, end.degree = 90, clock.wise = F, rou1 = 0.4, rou2 = 0.8, border = "black") #uncertainty arrow
      draw.sector(start.degree = -9 + 90, end.degree = 9 + 90, clock.wise = F, rou1 = 0.8, rou2 = 0.8, border = "black") #uncertainty lower
      draw.sector(start.degree = -9 + 90, end.degree = 9 + 90, clock.wise = F, rou1 = 0.4, rou2 = 0.4, border = "black") #uncertainty upper
      draw.sector(start.degree = 270 - 270, end.degree = 270 - 270, clock.wise = F, rou1 = 0.3, rou2 = 1.3, border = "black",lty = "dashed") #0.3
      draw.sector(start.degree = 280 - 270, end.degree = 280 - 270, clock.wise = F, rou1 = 0.65, rou2 = 1.3, border = "black",lty = "dashed") #0.65
      draw.sector(start.degree = 290 - 270, end.degree = 290 - 270, clock.wise = F, rou1 = 1, rou2 = 1.3, border = "black",lty = "dashed") #1.0
      text(c("0.3","0.65","1"),x = c(1.4,1.45,1.25), y = c(0,0.25,0.45))
      # plot how the whiskers are calculated
      if (quantile) text(c("Q90","Q50","Q10"),x = c(-0.3,-0.29,-0.26), y = c(0.8,0.48,0.35),cex=0.7) # quantile case
      else text(c("max","mean","min"),x = c(-0.3,-0.29,-0.26), y = c(0.8,0.48,0.35),cex=0.7) # minmeanmax case
   }else if (type == "regular") {
     draw.sector(180, 360, rou1 = 1,col = "gray80")
      for (i in 1:length(angles[,1])) {
         mangle <- mean(angles[i,])
         if (i==1) mangle <- -98
         dmin <- data[i,1]
         dmedian <- data[i,2]
         dmax <- data[i,3]
         draw.sector(start.degree = angles[i,1] + 90, end.degree = angles[i,2] + 90, col = colz[i], rou1 = dmedian,clock.wise = F, border = "black")
         draw.sector(start.degree = mangle + 90, end.degree = mangle + 90, clock.wise = F, rou1 = dmin, rou2 = dmax, border = "black") #uncertainty arrow
         draw.sector(start.degree = mangle - 9 + 90, end.degree = mangle + 9 + 90, clock.wise = F, rou1 = dmin, rou2 = dmin, border = "black") #uncertainty lower
         draw.sector(start.degree = mangle - 9 + 90, end.degree = mangle + 9 + 90, clock.wise = F, rou1 = dmax, rou2 = dmax, border = "black") #uncertainty upper
      }
      draw.sector(0, 360, rou1 = 1)
      draw.sector(0, 360, rou1 = 0.6)
      draw.sector(0, 360, rou1 = 0.3)
      draw.sector(start.degree = 180, end.degree = 180, clock.wise = F, rou1 = -1.2, rou2 = 1.2, border = "black",lwd = 2)

   }else {
      stop(paste0("Unknown type ",type,". Please use 'legend1' for variable and color legend,
                  'legend2' for value legend, or 'regular' (default setting) for the regular ecorisk plot."))
   }
}

#' Plot radial EcoRisk plot to file
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],3[min,mean,max])
#' @param file to write into
#' @param title character string title for plot, default empty
#' @param type plot type, 'legend1' for variable and color legend,
#'             'legend2' for value legend, or 'regular' (default setting)
#'             for the regular EcoRisk plot
#' @param eps write as eps or png
#' @param legYes logical. whether to plot legend or not. defaults to TRUE
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskradial <- function(data, file, title = "", legYes = T, eps = FALSE,quantile=T) {
   #param data EcoRisk data array c(8[ncomponents],3[min,median,max])
   #param title title for plot
   #param type plot type, 'legend1' for variable and color legend, 'legend2' for value legend, or 'regular' (default setting) for the regular EcoRisk plot
   outFol <- dirname(file)
   dir.create(file.path(outFol),showWarnings = F)
   if (length(which(data < 0 | data > 1)) > 0) print("Warning: there are values in data outside the expected EcoRisk range [0..1].")
   if (eps) {
      file <- strsplit(file,".",fixed = TRUE)[[1]]
      file <- paste(c(file[1:(length(file) - 1)],"eps"), collapse = ".")
      ps.options(family = c("Helvetica"), pointsize = 18)
      postscript(file, horizontal = FALSE, onefile = FALSE, width = 22, height = 8.5, paper = "special")
   }else {
      png(file, width = 7.25, height = 3.5, units = "in", res = 300, pointsize = 6,type = "cairo")
   }
   # adjust the margins, dependent on whether a legend should be plotted or not
   par(fig = c(0,0.7,0,1))#, oma=c(0,0,0,0),mar=c(0,0,0,0))

   #plot main EcoRisk radial
   plotEcoRiskradialToScreen(data = data, title = title,zoom=1.0, type = "regular")

   if (legYes) {
      par(fig = c(0.7,1,0,0.5), new = TRUE)#, oma=c(0,0,0,0),mar=c(0,0,0,0))
      plotEcoRiskradialToScreen(data = data, title = "", zoom = 1.5, type = "legend1")
      par(fig = c(0.7,1,0.5,1), new = TRUE)#, oma=c(0,0,0,0),mar=c(0,0,0,0))
      plotEcoRiskradialToScreen(data = data, title = "",zoom = 1.5, type = "legend2", quantile=quantile)
   }
   dev.off()
}

#' Plot timeline of EcoRisk variables to screen
#'
#' Function to plot timeline of EcoRisk variables to screen
#'
#' @param data EcoRisk data array 
#'        c(4/19[biomes],8/10[nEcoRiskcomponents],3[min,mean,max],timeslices)
#' @param timerange of the data input
#' @param yrange range for y axis default c(0,1)
#' @param legYes plot legend (default T)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotOvertimeToScreen <- function(data, timerange, yrange = c(0,1), legYes = T, legOnly = F, varnames = NULL) {
  ecorisk_dims <- dim(data)[1]
  if (is.null(varnames)){
    if (ecorisk_dims == 10) {
      names <- c( ecorisk = "ecorisk", deltav = "vegetation structure",
                  local = "local change", global = "global importance",
                  balance =  "ecosystem balance", cstocks = "carbon stocks",
                  cfluxes =  "carbon fluxes", wfluxes = "water fluxes", 
                  nstocks = "nitrogen stocks", nfluxes = "nitrogen fluxes")
      set <- brewer.pal(12, "Set3") #c(blue-green, yellow,violet,red,blue,orange,green,pink,grey,purple,green-blue,yellow-orange)
      colz <- set[c(4,7,8,11,1,3,10,5,12,6)]
    } else if (ecorisk_dims == 8) {
      names <- c( ecorisk = "ecorisk", deltav = "vegetation structure",
                  local = "local change", global = "global importance",
                  balance =  "ecosystem balance", cstocks = "carbon stocks",
                  cfluxes = "carbon fluxes", wfluxes = "water fluxes")
      colz <- c("darkgoldenrod", brewer.pal(5,"Greens")[5], 
                brewer.pal(6, "Set1")[seq(2, 6, by = 2)],
                rev(brewer.pal(6, "Oranges")[c(4,5)]), brewer.pal(6, "PuBu")[6])
    }else{
      stop(paste("Unknown number of dimensions for ecorisk data:",ecorisk_dims))
    }
  }else{
    names <- varnames
    colz <- brewer.pal(length(names), "Set2")
  }
  years <- timerange[1]:timerange[2]
  if (legOnly) {
    plot(NA, ylim = c(yrange[1],yrange[2]), cex.axis = 1,axes = F, xlab = "", ylab = "")

    legend("center",legend = names,fill = colz, border = colz)
  }else{
    plot(NA, xlim = timerange, ylim = c(yrange[1],yrange[2]), cex.axis = 1,xlab = "", ylab = "")
    for (i in 1:ecorisk_dims){
      if (i == 1) lines(x = years, y = data[i,2,], col = colz[i],lwd=4)
      else lines(x = years, y = data[i,2,], col = colz[i],lwd=2)
    }
    if (legYes) legend("topleft",legend = names,fill = colz)
  }
}

#' Plot timeline of EcoRisk variables as panel to file with 4/16 biomes
#'
#' Function to plot a panel of 4/16 timelines per biome aggregated EcoRisk values [0-1]
#' to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],3[min,mean,max])
#' @param biomeNames names of biomes
#' @param file to write into
#' @param yrange range for y axis (default c(0,1))
#' @param timerange of the data input
#' @param eps write as eps or png
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskovertimePanel <- function(data, 
                                biomeNames, 
                                file,
                                yrange = c(0,1),
                                timerange,
                                eps = FALSE,
                                varnames = NULL) {
  outFol <- dirname(file)
  dir.create(file.path(outFol),showWarnings = F)
  if (length(which(data < 0 | data > 1)) > 0) {
    print("Warning: values in data outside the expected EcoRisk range [0..1].")
  } 
  if (eps) {
    file <- strsplit(file,".",fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)],"eps"), collapse = ".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file, horizontal = FALSE, onefile = FALSE, width = 15, 
               height = 10, paper = "special")
  }else {
    png(file, width = 5.25, height = 3.5, units = "in", res = 300, 
        pointsize = 6,type = "cairo")
  }
  d <- length(data[,1,1,1])
  par(oma=c(0,0,0,0),mar=c(3,2,0.5,0))
  if (d==16 | d==4){
    k <- sqrt(d)
    xs <- seq(0,0.8,length.out = k+1)
    ys <- seq(0.98,0,length.out = k+1)
    for (x in 1:k){
      for (y in 1:k){
        if (x==1 & y==1) {
          par(fig = c(xs[x],xs[x+1],ys[y+1],ys[y]), 
              xpd = T)
        } else {
          par(fig = c(xs[x],xs[x+1],ys[y+1],ys[y]), xpd = T, new=T)
        }
        plotOvertimeToScreen(data = data[(x-1)*k+y,,,], 
                               timerange = timerange, yrange = yrange, 
                               legYes = F, varnames = varnames)
        mtext(text = biomeNames[(x-1)*k+y], side = 3, 
              line = 0, cex = 1,font = 2)
      }
    }
  }else{
    stop(paste("Unknown number of biomes: ",length(data[,1,1,1])))
  }
  #legend

  par(fig = c(0.8,1,0.5,1.0), new = TRUE, oma=c(0,0,0,0),mar=c(0,0,0,0))
  plot(NA,axes=F,ylim=c(0,1),xlim=c(0,1))
  if (d == 16){
    text(x=0.1,y=seq(0.95,0.05,length.out = length(get_biome_names(1))), labels = paste0(get_biome_names(1)," : ",get_biome_names(2)),cex=0.7,adj = 0)
  }
  par(fig = c(0.8,1,0.0,0.5), new = TRUE, oma=c(0,0,0,0),mar=c(0,0,0,0))
  if (is.null(varnames)){
    plotOvertimeToScreen(data = data[1,,,], timerange = timerange, legYes = F, legOnly = T )
  }else{
    plot(NA,axes=F,ylim=c(0,1),xlim=c(0,1))
    colz <- brewer.pal(length(varnames), "Set2")
    legend("center", legend = varnames, fill = colz,cex=1)
  }
  dev.off()
}

#' Plot radial EcoRisk panel to file with 4/16 biomes
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],3[min,mean,max])
#' @param biomeNames names of biomes
#' @param file to write into
#' @param quantile is it quantiles or minmeanmax data? - text for whiskers
#' @param eps write as eps or png
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskradialPanel <- function(data, 
                                biomeNames, 
                                file, 
                                quantile = T, 
                                eps = FALSE) {
  outFol <- dirname(file)
  dir.create(file.path(outFol),showWarnings = F)
  if (length(which(data < 0 | data > 1)) > 0) {
    print("Warning: values in data outside the expected EcoRisk range [0..1].")
  } 
  if (eps) {
    file <- strsplit(file,".",fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)],"eps"), collapse = ".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file, horizontal = FALSE, onefile = FALSE, width = 15, 
               height = 10, paper = "special")
  }else {
    png(file, width = 5.25, height = 3.5, units = "in", res = 300, 
        pointsize = 6,type = "cairo")
  }
  d <- length(data[,1,1])
  if (d==16 | d==4){
    k <- sqrt(d)
    xs <- seq(0,0.6,length.out = k+1)
    ys <- seq(0.98,0,length.out = k+1)
    for (x in 1:k){
      for (y in 1:k){
        if (x==1 & y==1) {
          par(fig = c(xs[x],xs[x+1],ys[y+1],ys[y]), 
              xpd = T, oma=c(0,0,0,0),mar=c(0,0,0,0))
        } else {
          par(fig = c(xs[x],xs[x+1],ys[y+1],ys[y]), xpd = T, new=T)
        }
        plotEcoRiskradialToScreen(data = data[(x-1)*k+y,,], 
                               title = "",zoom=1.0, type = "regular")
        mtext(text = biomeNames[(x-1)*k+y], side = 3, 
              line = -0.5, cex = 1,font = 2)
      }
    }
  }else{
    stop(paste("Unknown number of biomes: ",length(data[,1,1])))
  }
  #legend
  par(fig = c(0.6,1,0.1,0.6), new = TRUE)#, oma=c(0,0,0,0),mar=c(0,0,0,0))
  plotEcoRiskradialToScreen(data = data[1,,], title = "", 
                         zoom = 1.5, type = "legend1")
  par(fig = c(0.6,1,0.5,1.0), new = TRUE)#, oma=c(0,0,0,0),mar=c(0,0,0,0))
  plotEcoRiskradialToScreen(data = data[1,,], title = "legend",zoom = 1.5,
                          type = "legend2",titleSize = 1,quantile = quantile)
  dev.off()
}

#' Plot biomes
#'
#' Function to plot biome classification
#'
#' @param biome_ids biome id as given by classify_biomes
#' @param biomeNameLength length of biome names in legend: 1 - abbreviation, 
#'        2 - short name, 3 - full biome name
#' @param order legend order: either "plants" to first have forests, then 
#'        grasslands, then tundra ..., or "zones" to go from north to south
#'        (default: "plants")
#' @param title character string title for plot, default empty
#' @param titleSize size of title in cex units (defaukt: 2)
#' @param legYes whether to plot legend (default: True)
#' @param leg_scale size of legend in cex units (default 0.5)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomesToScreen <- function(biome_ids, biomeNameLength = 1, orderLegend = "plants", title = "", 
                               titleSize = 2, legYes = T, leg_scale = 0.5) {
   require(raster)
   require(RColorBrewer)
   #---- setting up colors and biome names ----------------------------------------------------------------#
   colz <- c(rev(brewer.pal(6, "YlOrBr")), # warm
                         rev(brewer.pal(9, "YlGn")[c(3,5,7,9)]),
                         rev(brewer.pal(9, "GnBu"))[c(2:4,6,8,9)], # cold below forest
                         "white", #"lightblue" # Water
                         "lightgrey", #Rocks & Ice
                         "pink3" #montane Tundra/Grassland
   )
   if (orderLegend == "plants") order_legend <- 1:19
   else if (orderLegend == "zones") order_legend <- c(1, 2, 9, 10, 11, 3, 4, 5, 6, 12, 13, 14, 7, 8, 15, 16, 17, 18, 19)
   else stop(paste0("Unknown value for parameter orderLegend (plants or zones) - was given as: ",orderLegend))
   biome_class_cols <-  colz[c(1,2,7,8,9,10,13,12,3,4,5,14,15,16,19,11,6,17,18)]
   biome_mapping <- read.csv(file = system.file("extdata", "biomes.csv", package = "biospheremetrics"),sep = ";")
   biome_class_names <- get_biome_names(biomeNameLength)

   if (!(length(biome_class_names) == length(biome_class_cols))) stop("Size of biome class names and colors do not match -- should be 18.")
   #---- plotting ----------------------------------------------------------------#
   brks <- seq(min(biome_ids,na.rm = T) - 0.5, max(biome_ids,na.rm = T) + 0.5, 1)
   ra <- raster::raster(ncols = 720, nrows = 360)
   range <- range(biome_ids)
   ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  biome_ids
   extent <- raster::extent(c(-180, 180, -60, 90))
   par(mar = c(0,0,0,0), oma = c(0,0,0,0),bty = "n")
   raster::plot(ra, ext = extent, breaks = brks, col = biome_class_cols, main = "", legend = FALSE, axes = FALSE)
   title(main = title, line = -2, cex.main = titleSize)
   if (legYes) {
      legend(x = -180, y = 27, legend = biome_class_names[order_legend], fill = biome_class_cols[order_legend], col = biome_class_cols[order_legend], cex = leg_scale, bg = "white", bty = "o")
   }
   maps::map('world', add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60,90))
}

#' Plot biomes to file
#'
#' Function to plot biome classification to file
#'
#' @param biome_ids biome id as given by classify_biomes
#' @param biomeNameLength length of biome names in legend: 1 - abbreviation, 
#'        2 - short name, 3 - full biome name
#' @param orderLegend legend order: either "plants" to first have forests, then 
#'        grasslands, then tundra ..., or "zones" to go from north to south
#'        (default: "plants")
#' @param file to write into
#' @param title character string title for plot, default empty
#' @param titleSize size of title in cex units (defaukt: 2)
#' @param legYes whether to plot legend (default: True)
#' @param leg_scale size of legend in cex units (default 0.5)
#' @param eps write as eps, replacing png in filename (default: True)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomes <- function(biome_ids, biomeNameLength = 1, orderLegend = "plants",file, title = "", titleSize = 2,
                       legYes = T, leg_scale = 1, eps = FALSE) {
   outFol <- dirname(file)
   dir.create(file.path(outFol),showWarnings = F)
   if (eps) {
      file <- strsplit(file,".",fixed = TRUE)[[1]]
      file <- paste(c(file[1:(length(file) - 1)],"eps"),collapse = ".")
      ps.options(family = c("Helvetica"), pointsize = 18)
      postscript(file, horizontal = FALSE, onefile = FALSE, width = 22, height = 8.5, paper = "special")
   }else{
      png(file, width = 7.25, height = 3.5, units = "in", res = 300, pointsize = 6,type = "cairo")
   }
   plotBiomesToScreen(biome_ids = biome_ids, biomeNameLength = biomeNameLength, orderLegend = orderLegend, title = title, titleSize = titleSize, legYes = legYes, leg_scale = leg_scale)
   dev.off()
}

#' Plot radial EcoRisk plot to file with 4/16 biomes
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data input data with dimension c(nbiome_classes,3) -- Q10,Q50,Q90 each
#' @param biome_class_names to write into
#' @param title character string title for plot, default empty
#' @param titleSize character string title for plot
#' @param leg_scale character string title for plot
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomeAveragesToScreen <- function(data, biome_class_names, title = "",
                               titleSize = 2, leg_scale = 0.5, palette = NULL) {
  require(raster)
  require(RColorBrewer)
  #---- setting up colors and biome names ----------------------------------------------------------------#
  brks <- seq(0,1,0.1)
  data[data < brks[1]] <- brks[1]
  data[data > brks[length(brks)]] <- brks[length(brks)]
  if (is.null(palette)) palette <- c("white","steelblue1","royalblue",RColorBrewer::brewer.pal(7,"YlOrRd"))
  colIndex <- floor(data[,2]*10) + 1
  if (!(length(biome_class_names) == dim(data)[1])) stop("Size of biome class names and data input do not match.")
  #---- plotting ----------------------------------------------------------------#
  plot(NA, xlim = c(0,1), ylim = c(0,1), main = title, axes = F,cex.main = titleSize,xlab = "", ylab = "")
  legend(x = 0, y = 1, legend = biome_class_names, fill = palette[colIndex], col = palette[colIndex],border = palette[colIndex], cex = leg_scale, bg = "white", bty = "o")
}

#' Plot radial EcoRisk plot to file with 4/16 biomes
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4[biomes],[nEcoRiskcomponents],3[min,median,max])
#' @param file to write into
#' @param biome_class_names to write into
#' @param title character string title for plot, default empty
#' @param titleSize character string title for plot
#' @param leg_scale character string title for plot
#' @param eps write as eps, replacing png in filename (default: True)
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotBiomeAverages <- function(data, file, biome_class_names, title = "", titleSize = 2,
                      leg_scale = 1, eps = FALSE, palette = NULL) {
  outFol <- dirname(file)
  dir.create(file.path(outFol),showWarnings = F)
  if (eps) {
    file <- strsplit(file,".",fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)],"eps"),collapse = ".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file, horizontal = FALSE, onefile = FALSE, width = 22, height = 8.5, paper = "special")
  }else{
    png(file, width = 4, height = 3, units = "in", res = 300, pointsize = 6,type = "cairo")
  }
  plotBiomeAveragesToScreen(data = data, biome_class_names = biome_class_names, 
                            title = title, titleSize = titleSize, 
                            leg_scale = leg_scale, palette = palette)
  dev.off()
}

#' Plot crosstable showing (dis-)similarity between average biome pixels
#'
#' Function to plot a crosstable showing (dis-)similarity between average
#' biome pixels based on EcoRisk (former gamma) metric from LPJmL simulations
#'
#' @param data crosstable data as array with [nbiomes,nbiomes] and row/colnames
#' @param lmar left margin for plot in lines (default: 3)
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskcrossTableToScreen <- function(data, lmar = 3, palette = NULL) {
  #data prep
  data <- round(data,digits = 2)
  x = 1:ncol(data)
  y = 1:nrow(data)
  centers <- expand.grid(y,x)
  #coloring
  if (is.null(palette)) palette <- c("white","steelblue1","royalblue",RColorBrewer::brewer.pal(7,"YlOrRd"))
  brks <- seq(0,1,0.1)
  #color.picker <- function(z){
  #  if(is.na(z)){return("black")}
  #  else {return(palette[floor(z*10)+1])}
  #}

  # plot margins
  par(mar = c(0,lmar,2,0)) #bltr

  image(x, y, t(data),
        col = palette,
        breaks = brks,
        xaxt = 'n',
        yaxt = 'n',
        xlab = '',
        ylab = '',
        ylim = c(max(y) + 0.5, min(y) - 0.5)
  )
  text(centers[,2], centers[,1], c(data), col = "black")

  #add margin text
  mtext(attributes(data)$dimnames[[2]], at=1:ncol(data), padj = -1)
  mtext(attributes(data)$dimnames[[1]], at=1:nrow(data), side = 2, las = 1, adj = 1,line = 1)

  #add black lines
  abline(h=y + 0.5)
  abline(v=x + 0.5)
}

#' Plot crosstable to file showing (dis-)similarity between average biome pixels
#'
#' Function to plot to file a crosstable showing (dis-)similarity between average
#' biome pixels based on EcoRisk (former Gamma) metric from LPJmL simulations
#'
#' @param data crosstable data as array with [nbiomes,nbiomes] and row/colnames
#' @param file to write into
#' @param lmar left margin for plot in lines (default: 3)
#' @param eps write as eps or png
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg 
#'        color scheme white-blue-yellow-red
#'
#' @return None
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
plotEcoRiskcrossTable <- function(data, file, lmar=3, eps = FALSE, palette = NULL) {
  outFol <- dirname(file)
  dir.create(file.path(outFol),showWarnings = F)
  if (eps) {
    file <- strsplit(file,".",fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)],"eps"),collapse = ".")
    ps.options(family = c("Helvetica"), pointsize = 18)
    postscript(file, horizontal = FALSE, onefile = FALSE, width = 22, height = 8.5, paper = "special")
  }else{
    png(file, width = 6, height = 3, units = "in", res = 300, pointsize = 6,type = "cairo")
  }
  plotEcoRiskcrossTableToScreen(data = data, lmar = lmar, palette = palette)
  dev.off()
}

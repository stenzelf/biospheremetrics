#' Classify biomes
#'
#' Classify biomes based on foliage protected cover (FPC) and temperature
#' LPJmL output plus either vegetation carbon or pft_lai depending on
#' the savanna_proxy option and elevation if montane_arctic_proxy requires this
#'
#' @param path_reference path to the reference LPJmL run. If not provided,
#'        the path is extracted from the file paths provided in files_reference.
#' @param files_reference list with variable names and corresponding file paths
#'        (character string) of the reference LPJmL run. All needed files are
#'        provided as key value pairs, e.g.:
#'        list(leaching = "/temp/leaching.bin.json"). If not needed for the
#'        applied method, set to NULL.
#' @param time_span_reference time span to be used for the scenario run, defined
#'        as an character string, e.g. `as.character(1901:1930)`.
#' @param savanna_proxy `list` with either pft_lai or vegc as
#'        key and value in m2/m2 for pft_lai (default = 6) and gC/m2 for
#'        vegc (default would be 7500), Set to `NULL` if no proxy should be
#'        used.
#' @param montane_arctic_proxy `list` with either "elevation" or "latitude" as
#'        name/key and value in m for elevation (default 1000) and degree for
#'        latitude (default would be 55), Set to `NULL` if no proxy is used.
#' @param tree_cover_thresholds list with minimum tree cover thresholds for
#'        definition of forest, woodland, savanna and grassland. Only changes to
#'        the default have to be included in the list, for the rest the default
#'        is used.
#'        Default values, based on the IGBP land cover classification system:
#'        "boreal forest" = 0.6
#'        "temperate forest" = 0.6
#'        "temperate woodland" = 0.3
#'        "temperate savanna" = 0.1
#'        "tropical forest" = 0.6
#'        "tropical woodland" = 0.3
#'        "tropical savanna" = 0.1
#'        In the boreal zone, there is no woodland, everything below the
#'        boreal forest threshold will be classified as boreal tundra.
#' @param avg_nyear_args list of arguments to be passed to
#'        \link[biospheremetrics]{average_nyear_window} (see for more info).
#'        To be used for time series analysis
#' @param input_files `list` with input file paths to be used instead of outputs
#'        default: empty list
#' @param diff_output_files `list` with output files, to be used from another
#' location than the output folders - default: empty list
#' @return list object containing biome_id (main biome per grid cell
#'         [dim=c(ncells)]), and list of respective biome_names[dim=c(nbiomes)]
#'
#' @examples
#' \dontrun{
#' classify_biomes(
#'   path_data = "/path/to/lpjml_run/to/classify",
#'   timespan = c(1982:2011)
#' )
#' }
#'
#' @export
classify_biomes <- function(path_reference = NULL,
                            files_reference = NULL,
                            time_span_reference,
                            savanna_proxy = list(pft_lai = 6),
                            montane_arctic_proxy = list(elevation = 1000),
                            tree_cover_thresholds = list(),
                            avg_nyear_args = list(), # currently a place holder
                            input_files = list(),
                            diff_output_files = list()) {
  if (is.null(files_reference) && is.null(path_reference)) {
    stop("files_reference or path_reference must be provided")
  } else if (!is.null(path_reference) && is.null(files_reference)) {
    # Get main file type (meta, clm)
    file_ext <- get_major_file_ext(path_reference)

    # List required output files for each boundary
    output_files <- list_outputs("biome",
      only_first_filename = FALSE
    )

    files_reference <- get_filenames(
      path = path_reference,
      output_files = output_files,
      diff_output_files = diff_output_files,
      input_files = input_files,
      file_ext = file_ext
    )
  }

  # test if provided proxies are valid
  savanna_proxy_name <- match.arg(
    names(savanna_proxy),
    c(NA, "vegc", "pft_lai")
  )
  montane_arctic_proxy_name <- match.arg(
    names(montane_arctic_proxy),
    c(NA, "elevation", "latitude")
  )

  # define default minimum tree cover for forest / woodland / savanna
  min_tree_cover <- list(
    "boreal forest" = 0.6,
    "temperate forest" = 0.6,
    "temperate woodland" = 0.3,
    "temperate savanna" = 0.1,
    "tropical forest" = 0.6,
    "tropical woodland" = 0.3,
    "tropical savanna" = 0.1
  )

  # replace default values by values defined in tree_cover_thresholds
  # parameter -> won't be applied if not specified
  replace_idx <- match(names(tree_cover_thresholds), names(min_tree_cover))
  if (any(is.na(replace_idx))) {
    stop(paste0(
      names(tree_cover_thresholds)[which(is.na(replace_idx))],
      " is not valid. Please use a name of: ",
      paste0(names(min_tree_cover), collapse = ", ")
    ))
  }
  min_tree_cover[replace_idx] <- tree_cover_thresholds

  # test if forest threshold is always > woodland threshold > savanna threshold
  if (min_tree_cover[["temperate forest"]] <=
    min_tree_cover[["temperate woodland"]] ||
    min_tree_cover[["temperate woodland"]] <=
      min_tree_cover[["temperate savanna"]] ||
    min_tree_cover[["tropical woodland"]] <=
      min_tree_cover[["tropical savanna"]] ||
    min_tree_cover[["tropical forest"]] <=
      min_tree_cover[["tropical woodland"]]) {
    stop(paste0(
      "Tree cover threshold for forest are not always higher than",
      "tree cover thresholds for woodland and savanna. Aborting."
    ))
  }
  # -------------------------------------------------------------------------- #
  # read in relevant data
  grid <- lpjmlkit::read_io(
    files_reference$grid,
    silent = TRUE
  )

  lat <- lpjmlkit::as_array(grid, subset = list(band = 2)) %>%
    drop()
  fpc <- lpjmlkit::read_io(
    files_reference$fpc,
    subset = list(year = time_span_reference),
    silent = TRUE
  ) %>%
    lpjmlkit::transform(to = c("year_month_day")) %>%
    lpjmlkit::as_array()

  temp <- lpjmlkit::read_io(
    files_reference$temp,
    subset = list(year = time_span_reference),
    silent = TRUE
  ) %>%
    lpjmlkit::transform(to = c("year_month_day")) %>%
    lpjmlkit::as_array(
      aggregate =
        list(month = sum, day = sum, band = sum)
    ) %>%
    suppressWarnings()

  if (!is.na(savanna_proxy_name)) {
    savanna_proxy_data <- lpjmlkit::read_io(
      files_reference[[savanna_proxy_name]],
      subset = list(year = time_span_reference),
      silent = TRUE
    ) %>%
      lpjmlkit::transform(to = c("year_month_day")) %>%
      lpjmlkit::as_array(aggregate = list(month = sum)) %>%
      suppressWarnings()
  }

  if (!is.na(montane_arctic_proxy_name)) {
    if (montane_arctic_proxy_name == "elevation") {
      elevation <- lpjmlkit::read_io(
        files_reference$elevation,
        silent = TRUE
      )$data %>%
        drop()
    }
  }

  fpc_nbands <- dim(fpc)[["band"]]
  npft <- fpc_nbands - 1

  # average fpc
  avg_fpc <- do.call(
    average_nyear_window,
    append(
      list(x = fpc),
      avg_nyear_args
    )
  )

  # average vegc or pft_lai
  if (!is.na(savanna_proxy_name)) {
    avg_savanna_proxy_data <- drop(
      do.call(
        average_nyear_window,
        append(
          list(x = savanna_proxy_data),
          avg_nyear_args
        )
      )
    )
  }

  # average temp
  # TODO understand why additional dimension is added here but not for fpc
  # (67420, 1)
  avg_temp <- do.call(
    average_nyear_window,
    append(
      list(x = temp), # fix_dimnames(temp, "temp", timespan, ncell, npft)),
      avg_nyear_args
    )
  )

  # biome_names after biome classification in Ostberg et al. 2013
  # (https://doi.org/10.5194/esd-4-347-2013), Ostberg et al 2015
  # (https://doi.org/10.1088/1748-9326/10/4/044011) and Gerten et al. 2020
  # (https://doi.org/10.1038/s41893-019-0465-1)

  # biome names
  biome_mapping <- system.file("extdata",
    "biomes.csv",
    package = "biospheremetrics"
  ) %>%
    readr::read_delim(col_types = readr::cols(), delim = ";")
  biome_names <- biome_mapping$id
  names(biome_names) <- biome_mapping$name

  # please R CMD check for use of dplyr syntax
  npft_proxy <- zone <- category <- type <- NULL

  pft_categories <- system.file("extdata",
    "pft_categories.csv",
    package = "biospheremetrics"
  ) %>%
    read_pft_categories() %>%
    dplyr::filter(., npft_proxy == npft)

  fpc_names <- dplyr::filter(pft_categories, category == "natural")$pft
  # TODO this is only required if header files without band names are read in
  # but maybe ok to use external data here?
  dimnames(avg_fpc)$band <- c("natural stand fraction", fpc_names)

  fpc_temperate_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & zone == "temperate" & category == "natural"
  )$pft

  fpc_tropical_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & zone == "tropical" & category == "natural"
  )$pft

  fpc_boreal_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & zone == "boreal" & category == "natural"
  )$pft

  fpc_needle_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & category == "needle"
  )$pft

  fpc_evergreen_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & category == "evergreen"
  )$pft

  fpc_grass <- dplyr::filter(
    pft_categories,
    type == "grass" & category == "natural"
  )$pft

  fpc_trees <- dplyr::filter(
    pft_categories,
    type == "tree" & category == "natural"
  )$pft

  third_dim <- names(dim(avg_fpc))[
    !names(dim(avg_fpc)) %in% c("cell", "band")
  ] %>%
    {
      if (rlang::is_empty(.)) NULL else .
    }

  fpc_tree_total <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_tree_tropical <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_tropical_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_tree_temperate <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_temperate_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_tree_boreal <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_boreal_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_tree_needle <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_needle_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_tree_evergreen <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_evergreen_trees),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_grass_total <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_grass),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  fpc_total <- apply(
    lpjmlkit::asub(avg_fpc, band = -1),
    c("cell", third_dim),
    sum,
    na.rm = TRUE
  )
  max_share_trees <- apply(
    lpjmlkit::asub(avg_fpc, band = fpc_trees),
    c("cell", third_dim),
    max,
    na.rm = TRUE
  )

  # use vegc 7500 gC/m2 or natLAI 6 as proxy threshold for forest/savanna
  #   "boundary
  if (!is.null(savanna_proxy)) {
    if (savanna_proxy_name == "pft_lai") {
      avg_savanna_proxy_data <- apply(
        lpjmlkit::asub(avg_savanna_proxy_data, band = seq_len(npft)) *
          lpjmlkit::asub(avg_fpc, band = 2:(npft + 1)) *
          lpjmlkit::asub(avg_fpc, band = 1),
        c("cell", third_dim),
        sum
      )
    } else {
      avg_savanna_proxy_data <- drop(avg_savanna_proxy_data)
    }
    is_tropical_proxy <- avg_savanna_proxy_data >=
      savanna_proxy[[savanna_proxy_name]]
    is_savanna_proxy <- avg_savanna_proxy_data <
      savanna_proxy[[savanna_proxy_name]]
  } else {
    is_tropical_proxy <- array(TRUE,
      dim = dim(avg_temp),
      dimnames = dimnames(avg_temp)
    )
    is_savanna_proxy <- array(FALSE,
      dim = dim(avg_temp),
      dimnames = dimnames(avg_temp)
    )
  }

  # Desert
  is_desert <- {
    fpc_total <= 0.05 &
      avg_temp >= 0 #-2
  }

  # montane (for classification of montane grassland)
  if (!is.na(montane_arctic_proxy_name)) {
    if (montane_arctic_proxy_name == "elevation") {
      is_montane_artic <- elevation > montane_arctic_proxy[[
        montane_arctic_proxy_name
      ]]
    } else if (montane_arctic_proxy_name == "latitude") {
      is_montane_artic <- !(abs(lat) > montane_arctic_proxy[[
        montane_arctic_proxy_name
      ]])
    }
  }

  # FORESTS ------------------------------------------------------------------ #
  is_boreal_forest <- {
    fpc_tree_total >= min_tree_cover[["boreal forest"]]
  }
  is_temperate_forest <- {
    fpc_tree_total >= min_tree_cover[["temperate forest"]]
  }
  is_tropical_forest <- {
    fpc_tree_total >= min_tree_cover[["tropical forest"]]
  }
  # Boreal Evergreen
  is_boreal_evergreen <- {
    is_boreal_forest &
      lpjmlkit::asub(
        avg_fpc,
        band = "boreal needleleaved evergreen tree"
      ) == max_share_trees
  }

  if (npft == 9) {
    # Boreal Broadleaved Deciduous
    # no simulation of boreal needleleaved summergreen trees
    is_boreal_broad_deciduous <- {
      is_boreal_forest &
        (
          lpjmlkit::asub(
            avg_fpc,
            band = "boreal broadleaved summergreen tree"
          ) == max_share_trees
        )
    }
  } else {
    # Boreal Deciduous
    is_boreal_broad_deciduous <- {
      is_boreal_forest &
        lpjmlkit::asub(
          avg_fpc,
          band = "boreal broadleaved summergreen tree"
        ) == max_share_trees
    }

    is_boreal_needle_deciduous <- {
      is_boreal_forest &
        lpjmlkit::asub(
          avg_fpc,
          band = "boreal needleleaved summergreen tree"
        ) == max_share_trees
    }
  }

  # Temperate Coniferous Forest
  is_temperate_coniferous <- {
    is_temperate_forest &
      lpjmlkit::asub(
        avg_fpc,
        band = "temperate needleleaved evergreen tree"
      ) == max_share_trees
  }
  # Temperate Broadleaved Evergreen Forest
  is_temperate_broadleaved_evergreen <- { # nolint
    is_temperate_forest &
      lpjmlkit::asub(
        avg_fpc,
        band = "temperate broadleaved evergreen tree"
      ) == max_share_trees
  }
  # Temperate Broadleaved Deciduous Forest
  is_temperate_broadleaved_deciduous <- { # nolint
    is_temperate_forest &
      lpjmlkit::asub(
        avg_fpc,
        band = "temperate broadleaved summergreen tree"
      ) == max_share_trees
  }

  # Tropical Rainforest
  is_tropical_evergreen <- {
    is_tropical_forest &
      lpjmlkit::asub(
        avg_fpc,
        band = "tropical broadleaved evergreen tree"
      ) == max_share_trees &
      is_tropical_proxy
  }

  # Tropical Seasonal & Deciduous Forest
  is_tropical_raingreen <- {
    is_tropical_forest &
      (lpjmlkit::asub(
        avg_fpc,
        band = "tropical broadleaved raingreen tree"
      ) == max_share_trees) &
      is_tropical_proxy
  }
  # Warm Woody Savanna, Woodland & Shrubland
  is_tropical_forest_savanna <- {
    is_tropical_forest &
      (
        lpjmlkit::asub(
          avg_fpc,
          band = "tropical broadleaved evergreen tree"
        ) == max_share_trees |
          lpjmlkit::asub(
            avg_fpc,
            band = "tropical broadleaved raingreen tree"
          ) == max_share_trees
      ) &
      is_savanna_proxy
  }

  # WOODY savanna ----------------------------------------------------------- #

  # Temperate Woody Savanna, Woodland & Shrubland
  is_temperate_woody_savanna <- {
    fpc_tree_total <= min_tree_cover[["temperate forest"]] &
      fpc_tree_total >= min_tree_cover[["temperate woodland"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") >
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass") &
      avg_temp >= 0 #-2 &
    # lat < 55
  }
  # Warm Woody Savanna, Woodland & Shrubland
  is_tropical_woody_savanna <- {
    fpc_tree_total <= min_tree_cover[["tropical forest"]] &
      fpc_tree_total >= min_tree_cover[["tropical woodland"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") <
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass")
  }

  # OPEN SHRUBLAND / SAVANNAS ----------------------------------------------- #

  # Temperate Savanna & Open Shrubland
  is_temperate_shrubland <- {
    fpc_tree_total <= min_tree_cover[["temperate woodland"]] &
      fpc_tree_total >= min_tree_cover[["temperate savanna"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") >
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass") &
      avg_temp >= 0 #-2 &
    # lat < 55
  }
  # Warm Savanna & Open Shrubland
  is_tropical_shrubland <- {
    fpc_tree_total <= min_tree_cover[["tropical woodland"]] &
      fpc_tree_total >= min_tree_cover[["tropical savanna"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") <
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass") &
      avg_temp >= 0 #-2
  }

  # GRASSLAND ---------------------------------------------------------------- #

  # Temperate grassland
  is_temperate_grassland <- {
    fpc_total > 0.05 &
      fpc_tree_total <= min_tree_cover[["temperate savanna"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") >
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass") &
      avg_temp >= 0 #-2 &
    # lat < 55
  }
  # Warm grassland
  is_tropical_grassland <- {
    fpc_total > 0.05 &
      fpc_tree_total <= min_tree_cover[["tropical savanna"]] &
      lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") <
        lpjmlkit::asub(avg_fpc, band = "tropical c4 grass") &
      avg_temp >= 0 #-2
  }

  # Arctic Tundra ------------------------------------------------------------ #
  is_arctic_tundra <- {
    (!is_boreal_forest &
      !is_temperate_forest &
      (
        avg_temp < 0 |
          lpjmlkit::asub(avg_fpc, band = "temperate c3 grass") ==
            lpjmlkit::asub(avg_fpc, band = "tropical c4 grass")) &
      fpc_total > 0.05
    ) |
      (avg_temp < 0 & fpc_total < 0.05)
  }

  # Rocks and Ice
  is_rocks_and_ice <- {
    fpc_total == 0 &
      avg_temp < 0 #-2
  }
  # Water body
  is_water <- {
    lpjmlkit::asub(avg_fpc, band = 1) == 0
  }

  # CLASSIFY BIOMES ---------------------------------------------------------- #

  # initiate biome_class array
  biome_class <- array(NA,
    dim = c(grid$meta$ncell),
    dimnames = dimnames(fpc_total)
  )

  biome_class[is_desert] <- biome_names["Desert"]

  # forests
  biome_class[is_boreal_evergreen] <- biome_names[
    "Boreal Needleleaved Evergreen Forest"
  ]
  biome_class[is_boreal_broad_deciduous] <- biome_names[
    "Boreal Broadleaved Deciduous Forest"
  ]
  biome_class[is_boreal_needle_deciduous] <- biome_names[
    "Boreal Needleleaved Deciduous Forest"
  ]
  biome_class[is_temperate_coniferous] <- biome_names[
    "Temperate Needleleaved Evergreen Forest"
  ]
  biome_class[is_temperate_broadleaved_evergreen] <- biome_names[
    "Temperate Broadleaved Evergreen Forest"
  ]
  biome_class[is_temperate_broadleaved_deciduous] <- biome_names[
    "Temperate Broadleaved Deciduous Forest"
  ]
  biome_class[is_tropical_evergreen] <- biome_names["Tropical Rainforest"]
  biome_class[is_tropical_raingreen] <- biome_names[
    "Tropical Seasonal & Deciduous Forest"
  ]
  biome_class[is_tropical_forest_savanna] <- biome_names[
    "Warm Woody Savanna, Woodland & Shrubland"
  ]

  # woody savanna
  biome_class[is_temperate_woody_savanna] <- biome_names[
    "Temperate Woody Savanna, Woodland & Shrubland"
  ]
  biome_class[is_tropical_woody_savanna] <- biome_names[
    "Warm Woody Savanna, Woodland & Shrubland"
  ]

  # open shrubland / savanna
  biome_class[is_temperate_shrubland] <- biome_names[
    "Temperate Savanna & Open Shrubland"
  ]
  biome_class[is_tropical_shrubland] <- biome_names[
    "Warm Savanna & Open Shrubland"
  ]

  # grassland
  biome_class[is_temperate_grassland] <- biome_names["Temperate Grassland"]
  biome_class[is_tropical_grassland] <- biome_names["Warm Grassland"]

  biome_class[is_arctic_tundra] <- biome_names["Arctic Tundra"]
  if (!is.na(montane_arctic_proxy_name)) {
    biome_class[
      biome_class == biome_names["Arctic Tundra"] & is_montane_artic
    ] <- biome_names["Montane Grassland"]
  }

  # other
  biome_class[is_rocks_and_ice] <- biome_names["Rocks and Ice"]
  biome_class[is_water] <- biome_names["Water"]

  return(list(biome_id = biome_class, biome_names = names(biome_names)))
}


read_pft_categories <- function(file_path) {
  # please R CMD check for use of dplyr syntax
  category_natural <- NULL
  # read_delim, col_types = readr::cols(), delim = ";")to suppress messages
  readr::read_delim(file_path, col_types = readr::cols(), delim = ";") %>%
    # change 1, 0.5, 0 values to TRUE and NAs (NA's can be dropped)
    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with(c("category_", "zone_"))),
      function(x) ifelse(as.logical(x), TRUE, NA)
    ) %>%
    # filter natural pfts
    dplyr::filter(category_natural) %>%
    # all binary zone columns (tropical, temperate, boreal) in one categorical
    #   zone column
    tidyr::pivot_longer(
      cols = dplyr::starts_with("zone_"),
      names_to = "zone",
      names_prefix = "zone_",
      values_to = "zone_value",
      values_drop_na = TRUE
    ) %>%
    # all binary category columns (natural, needle, evergreen) in one
    # category column
    tidyr::pivot_longer(
      cols = dplyr::starts_with("category_"),
      names_to = "category",
      names_prefix = "category_",
      values_to = "category_value",
      values_drop_na = TRUE
    ) %>%
    # delete side product - logical columns
    dplyr::select(-c("category_value", "zone_value")) %>%
    # values to lpjml_index, names to length of npft (convert to numeric)
    tidyr::pivot_longer(
      cols = dplyr::starts_with("lpjml_index_npft_"),
      values_to = "lpjml_index",
      names_to = "npft_proxy",
      names_transform = list(
        npft_proxy =
          function(x) suppressWarnings(as.numeric(x))
      ),
      names_prefix = "lpjml_index_npft_"
    ) %>%
    return()
}

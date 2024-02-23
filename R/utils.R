# function to get file extension
get_major_file_ext <- function(path) {
  # Get all files in path
  all_files <- list.files(
    path,
    full.names = TRUE
  )

  # Get file extensions
  all_file_types <- all_files %>%
    strsplit("/") %>%
    sapply(utils::tail, 1) %>%
    strsplit("\\.") %>%
    sapply(function(x) {
      y <- x[-1]
      if ("json" %in% y) {
        return(paste(tail(strsplit(y, "\\."), 2), collapse = "."))
      } else {
        return(tail(strsplit(y, "\\."), 1))
      }
    }) %>%
    unlist()


  # Get most frequent file types
  # TODO not yet working
  most_frequent <- all_file_types %>%
    factor() %>%
    table() %>%
    names() %>%
    .[seq_len(5)]

  # 5 exemplaric files to detect type
  files_to_check <- sapply(
    most_frequent,
    function(x, y, z) {
      y[which(z == x)[1]]
    },
    y = all_files,
    z = all_file_types
  ) %>%
    na.omit()
  browser()
  # Detect actual LPJmL data type
  types <- sapply(
    files_to_check,
    lpjmlkit:::detect_io_type
  ) %>%
    stats::setNames(names(.), .)

  # Assign file type after ranking which is available
  # first preferable: "meta", second: "clm", last: "raw"
  if ("meta" %in% names(types)) {
    file_type <- types["meta"]
  } else if ("clm" %in% names(types)) {
    file_type <- types["clm"]
  } else if ("raw" %in% names(types)) {
    file_type <- types["raw"]
  }
  return(file_type)
}


# function to get absolute file names
get_filenames <- function(path, # nolint
                          output_files,
                          diff_output_files,
                          input_files,
                          file_ext) {
  file_names <- list()
  # Iterate over required outputs
  for (ofile in names(output_files)) {
    # Get required max. temporal resolution and convert to nstep
    resolution <- output_files[[ofile]]$resolution
    nstep <- switch(resolution,
      annual = 1,
      monthly = 12,
      daily = 365,
      stop(paste0("Not supported time resolution: ", dQuote(nstep), "."))
    )

    # If input file supplied use it as first priority
    if (ofile %in% names(input_files)) {
      file_name <- input_files[[ofile]]
    } else if (ofile %in% names(diff_output_files)) {
      # If different output file should be used - as second priority
      file_name <- paste0(
        path, "/",
        diff_output_files[[ofile]], ".",
        file_ext
      )
    } else {
      file_name <- NULL
    }

    if (!is.null(file_name)) {
      # Check if data could be read in
      meta <- lpjmlkit::read_meta(file_name)

      # Then check if temporal resultion of file matches required nstep
      if (nstep != meta$nstep && nstep != meta$nbands) {
        stop(
          paste0(
            "Required temporal resolution (nstep = ", nstep, ") ",
            "not supported by file ", dQuote(file_name),
            " (", meta$nstep, ")"
          )
        )
      }

      # If nothing specified try to read required files from provided path
    } else {
      # Iterate over different used file name options (e.g. runoff, mrunoff, ...) # nolint
      for (cfile in seq_along(output_files[[ofile]]$file_name)) {
        file_name <- paste0(
          path, "/",
          output_files[[ofile]]$file_name[cfile], ".",
          file_ext
        )

        # Check if file exists and if so check required temporal resolution
        # else next
        if (file.exists(file_name)) {
          meta <- lpjmlkit::read_meta(file_name)
          if (nstep <= meta$nstep || nstep == meta$nbands) {
            # Matching file found, break and use current file_name
            break
          }
        }

        # At end of iteraton raise error that no matching file_name was found
        if (cfile == length(output_files[[ofile]]$file_name) &&
          !output_files[[ofile]]$optional) {
          stop(
            paste0(
              "No matching output for ", dQuote(ofile),
              " with required temporal resolution (nstep = ", nstep, ") ",
              "found at path ", dQuote(path), "."
            )
          )
        }
      }
    }
    file_names[[ofile]] <- file_name
  }
  file_names
}


 
#' List required output files for given metric
#'
#' List required output files for given metric based on parameter file
#' `inst/extfiles/metric_files.yml`
#'
#' @param metric character string/list of strings. metrics to list outputs for
#'        can be one of: 
#'        "all" - list all outputs for all metrics
#'        "ecorisk" - list outputs for ecorisk metric without nitrogen
#'        "ecorisk_nitrogen" - list outputs for ecorisk metric with nitrogen
#'        "biocol" - list outputs for biocol metric
#'        "biome" - list outputs for the biome classification
#' @param only_first_filename if several legal output names are listed, only 
#'        output the first of them (default: TRUE)
#' 
#' @return list object with required outputs, their required temporal resolution 
#'         and if it is optional
#' 
#' @examples
#' \dontrun{
#' list_outputs(metric = "ecorisk_nitrogen")
#' }
#'
#' @md
#' @export
list_outputs <- function(metric = "all",
                         only_first_filename = TRUE) {
  metric <- process_metric(metric = metric)

  system.file(
    "extdata",
    "metric_files.yml",
    package = "biospheremetrics"
  ) %>%
    yaml::read_yaml() %>%
    get_outputs(metric, only_first_filename)
}


# Translate metric options into internal metric names
process_metric <- function(metric = "all") {
  all_metrics <- c(
    "ecorisk", "ecorisk_nitrogen", "biocol", "biome"
  )

  if ("all" %in% metric) {
    metric <- all_metrics
  }

  metric <- match.arg(
    arg = metric,
    choices = all_metrics,
    several.ok = TRUE
  )

  metric
}


# for input list a, all duplicate keys are unified, taking the value with
#     highest temporal resolution (daily>monthly>annual)
get_outputs <- function(x, metric_name, only_first_filename) { # nolint

  outputs <- list()
  # Iterate over all metrics
  for (metric in x$metric[metric_name]) {
    # Iterate over all unique keys
    for (item in names(metric$output)) {
      # Check if output is already in list or if it has higher resolution
      if (!item %in% names(outputs) ||
        (item %in% names(outputs) &&
          higher_res(
            metric$output[[item]]$resolution,
            outputs[[item]]$resolution
          ))
      ) {
        # Assign output resolution from metric file
        outputs[[item]]$resolution <- metric$output[[item]]$resolution
        outputs[[item]]$optional <- metric$output[[item]]$optional
        # Assign output file name from metric file
        if (only_first_filename) {
          outputs[[item]]$file_name <- x$file_name[[item]][1]
        } else {
          outputs[[item]]$file_name <- x$file_name[[item]]
        }
      }
    }
  }
  outputs
}


# Check if resolution of x is higher than resolution of y
higher_res <- function(x, y) {
  levels <- c("annual", "monthly", "daily")
  resolution_x <- match(match.arg(x, levels), levels)
  resolution_y <- match(match.arg(y, levels), levels)

  if (resolution_x > resolution_y) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# Avoid note for "."...
utils::globalVariables(".") # nolint:undesirable_function_linter

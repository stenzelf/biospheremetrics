# written by Fabian Stenzel
# 2022-2023 - stenzel@pik-potsdam.de

################# BioCol plot functions  ###################

#' Plot absolute BioCol, overtime, maps, and npp into given folder
#'
#' Wrapper function to plot absolute biocol, overtime, maps, and npp into given
#' folder
#'
#' @param biocol_data biocol data list object (returned from calc_biocol)
#'        containing biocol_overtime, biocol_overtime_abs,
#'        biocol_overtime_abs_frac_piref, biocol_overtime_frac_piref,
#'        biocol_overtime_frac, biocol_overtime_abs_frac, npp_harv_overtime,
#'        npp_luc_overtime, npp_act_overtime, npp_pot_overtime,
#'        npp_eco_overtime, harvest_grasslands_overtime,
#'        harvest_bioenergy_overtime, harvest_cft_overtime,
#'        rharvest_cft_overtime, fire_overtime, timber_harvest_overtime,
#'        wood_harvest_overtime, biocol, biocol_frac, npp, biocol_frac_piref,
#'        npp_potential, npp_ref, harvest_cft, rharvest_cft, biocol_harvest,
#'        biocol_luc, lat, lon, cellarea
#' @param path_write folder to write outputs into
#' @param plotyears range of years to plot over time
#' @param min_val y-axis minimum value for plot over time
#' @param max_val y-axis maximum value for plot over time
#' @param legendpos position of legend
#' @param start_year first year of biocol_data object
#' @param details show all harvest components or not
#' @param mapyear year to plot biocol map for
#' @param mapyear_buffer +- years around mapyear to average biocol
#'        (make sure these years exist in biocol_data) - default: 5
#' @param highlightyear year(s) that should be highlighted in overtime plot
#' @param eps write plots as eps, instead of png (default = FALSE)
#'
#' @examples
#' \dontrun{
#' plot_biocol(
#'    biocol_data = biocol,
#'    path_write = "~/BioCol_plots/",
#'    plotyears = c(1980, 2014),
#'    min_val = 0,
#'    max_val = 90,
#'    legendpos = "left",
#'    start_year = 1980,
#'    mapyear = 2000,
#'    highlightyear = 2000,
#'    eps = FALSE
#' )
#' }
#'
#' @md
#' @export
plot_biocol <- function(
    biocol_data,
    path_write,
    plotyears,
    min_val,
    max_val,
    legendpos,
    details = FALSE,
    start_year,
    mapyear,
    mapyear_buffer = 5,
    highlightyear,
    eps = FALSE) {
  lon <- biocol_data$lon
  lat <- biocol_data$lat
  cellarea <- biocol_data$cellarea
  mapindex <- mapyear - start_year
  message("Plotting BioCol figures")
  dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)
  pick_years <- (mapindex - mapyear_buffer):(mapindex + mapyear_buffer)
  plot_global(
    data = rowMeans(
      biocol_data$biocol[, pick_years]
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
      biocol_data$biocol_luc[, pick_years]
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
      biocol_data$biocol_harvest[, pick_years]
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
      path_write, "BioCol_overtime_LPJmL_", plotyears[1], "-",
      plotyears[2], ".png"
    ),
    first_year = start_year,
    plot_years = plotyears,
    min_val = min_val,
    ref = "pi",
    legendpos = legendpos,
    details = details,
    max_val = max_val,
    eps = eps,
    highlight_years = highlightyear
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_frac[, pick_years]
    ),
    file = paste0(path_write, "BioCol_frac_LPJmL_", mapyear, ".png"),
    legendtitle = "frac of NPPpot",
    type = "lin",
    min = -1,
    max = 1,
    col_pos = "Reds",
    col_neg = "Blues",
    leg_yes = TRUE,
    eps = FALSE,
    n_legend_ticks = 11
  )

  plot_global(
    data = rowMeans(
      biocol_data$biocol_frac_piref[, pick_years]
    ),
    file = paste0(path_write, "BioCol_frac_piref_LPJmL_", mapyear, ".png"),
    title = "",
    legendtitle = "frac of NPPref",
    type = "lin",
    min = -1,
    max = 1,
    col_pos = "Reds",
    col_neg = "Blues",
    leg_yes = TRUE,
    eps = FALSE,
    n_legend_ticks = 11
  )

  plot_global(
    data = rowMeans(
      biocol_data$npp[, pick_years]
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
#' @param file to write into, if not supplied (default is NULL) write to screen
#' @param title character string title for plot (default: "")
#' @param legendtitle character string legend title (default: "")
#' @param zero_threshold smallest value to be distinguished from 0 in legend,
#'        both for negative and positive values (default: 0.001)
#' @param haberl_legend use color palette from Haberl et al.? (default: FALSE)
#' @param eps write eps file instead of PNG (boolean) - (default: FALSE)
#'
#' @examples
#' \dontrun{
#' plot_biocol_map(
#'    data = biocol$biocol_frac[, "2000"] * 100,
#'    file = "./BioCol_map_yr2000.png",
#' )
#' }
#'
#' @md
#' @export
plot_biocol_map <- function(
    data,
    file = NULL,
    title = "",
    legendtitle = "",
    zero_threshold = 0.001,
    haberl_legend = FALSE,
    eps = FALSE) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[1:(length(file) - 1)], "eps"), collapse = ".")
      grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
      grDevices::postscript(file,
                            horizontal = FALSE, onefile = FALSE, width = 22,
                            height = 8.5, paper = "special"
      )
    } else {
      grDevices::png(file,
                     width = 7.25, height = 3.5, units = "in", res = 300,
                     pointsize = 6, type = "cairo"
      )
    }
  }
  if (haberl_legend) {
    brks <- c(
      -400, -200, -100, -50, -zero_threshold,
      zero_threshold, 10, 20, 30, 40, 50, 60, 70, 80, 100
    )
    classes <- c(
      "<-200", "-200 - -100", "-100 - -50",
      paste0("-50 - -", zero_threshold),
      paste0("-", zero_threshold, " - ", zero_threshold),
      paste0(zero_threshold, " - 10"), "10 - 20", "20 - 30", "30 - 40",
      "40 - 50", "50 - 60", "60 - 70", "70 - 80", "80 - 100"
    )
    palette <- c(
      "navy", "royalblue3", "royalblue1", "skyblue1",
      "grey80", "yellowgreen", "greenyellow", "yellow",
      "gold", "orange", "orangered", "orangered4",
      "brown4", "black"
    )
  } else {
    brks <- c(
      -400, seq(-100, -10, 10), -zero_threshold,
      zero_threshold, seq(10, 100, 10), 400
    ) / 100
    classes <- c(
      "<-1", "-1 - -0.9", "-0.9 - -0.8", "-0.8 - -0.7",
      "-0.7 - -0.6", "-0.6 - -0.5", "-0.5 - -0.4", "-0.4 - -0.3",
      "-0.3 - -0.2", "-0.2 - -0.1", paste("-0.1 - -", zero_threshold),
      paste("-", zero_threshold, " - ", zero_threshold),
      paste(zero_threshold, " - 0.1"), "0.1 - 0.2", "0.2 - 0.3",
      "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7",
      "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1", ">1"
    )
    palette <- grDevices::colorRampPalette(rev(
      RColorBrewer::brewer.pal(11, "RdBu")
    ))(length(brks) - 1)
  }

  data[data < brks[1]] <- brks[1]
  data[data > brks[length(brks)]] <- brks[length(brks)]

  ra <- terra::rast(ncols = 720, nrows = 360)
  range <- range(data)
  ra[terra::cellFromXY(ra, cbind(lon, lat))] <- data
  extent <- terra::ext(-180, 180, -60, 90)
  graphics::par(bty = "n", oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), xpd = TRUE)
  terra::plot(ra,
    ext = extent, breaks = brks, col = palette, main = "",
    legend = FALSE, axes = FALSE
  )
  graphics::title(title, line = -2)
  maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))
  graphics::legend(
    x = -180, y = 50, fill = palette, border = palette,
    legend = classes, title = legendtitle
  )
  if (!is.null(file)) grDevices::dev.off()
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
#' @param max_val_right maximum value for the BioCol y-axis labs right
#'        (default: 0.45)
#' @param legendpos position of legend (default: "topleft")
#' @param highlight_years year(s) that should be highlighted in overtime plot
#' (default: 2000)
#' @param details show all harvest components or not, (default: FALSE)
#' @param ref reference period for biocol ("pi" or "act"), to either use
#'        biocol_data$biocol_overtime_perc_piref or biocol_data$biocol_overtime
#' @param eps write plots as eps, instead of png (default = FALSE)
#'
#' @examples
#' \dontrun{
#' plot_biocol_ts(
#'   biocol_data = biocol_data,
#'   file = "./BioCol_overtime_LPJmL_1550-2015.png",
#'   first_year = 1550,
#'   plot_years = c(1550, 2015),
#'   min_val = 0,
#'   max_val = 80,
#'   ref = "pi",
#'   legendpos = "topleft",
#'   details = TRUE,
#'   max_val = max_val,
#'   highlight_years = c(1900, 2000)
#'   )
#' }
#'
#' @md
#' @export
plot_biocol_ts <- function(
    biocol_data,
    file = NULL,
    first_year,
    plot_years,
    highlight_years = 2000,
    details = FALSE,
    min_val = 0,
    max_val = 100,
    max_val_right = 0.45,
    legendpos = "topleft",
    eps = FALSE,
    ref = "pi") {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[1:(length(file) - 1)], "eps"), collapse = ".")
      grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
      grDevices::postscript(file,
                            horizontal = FALSE, onefile = FALSE, width = 22,
                            height = 8.5, paper = "special"
      )
    } else {
      grDevices::png(file,
                     width = 3.5, height = 3, units = "in", res = 300,
                     pointsize = 6, type = "cairo"
      )
    }
  }

  last_year <- first_year + length(biocol_data$npp_act_overtime) - 1
  colz <- c(
    "slateblue", "gold", "green3", "grey60", "red3",
    "black", "grey40", "darkorange", "#ff8c009d", "blue",
    "yellow", "turquoise", "darkgreen"
  )



  withr::with_par(new = list(bty = "o", oma = c(0, 0, 0, 0),
                             mar = c(4, 5, 1, 3)),
    {graphics::plot(NA,
      ylab = "GtC/yr", xlab = "Year", xlim = plot_years,
      ylim = c(min_val, max_val), xaxs = "i", yaxs = "i"
    )
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
      col = colz[2]
    )
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
    graphics::polygon(
      x = c(seq(first_year, last_year, 1), seq(last_year, first_year, -1)),
      y = c(biocol_data$biocol_overtime_abs, rev(biocol_data$biocol_overtime)),
      border = NA,
      col = colz[7]
    )
    graphics::lines(
      x = seq(first_year, last_year, 1),
      y = biocol_data$biocol_overtime_pos,
      type = "l",
      col = colz[6]
    )
    graphics::lines(
      x = seq(first_year, last_year, 1),
      y = biocol_data$npp_harv_overtime,
      type = "l",
      col = colz[5]
    )
    if (details) {
      graphics::lines(
        x = seq(first_year, last_year, 1),
        y = biocol_data$rharvest_cft_overtime,
        type = "l",
        col = colz[10]
      )
      graphics::lines(
        x = seq(first_year, last_year, 1),
        y = biocol_data$fire_overtime,
        type = "l", col = colz[11]
      )
      graphics::lines(
        x = seq(first_year, last_year, 1),
        y = biocol_data$timber_harvest_overtime,
        type = "l",
        col = colz[12]
      )
      graphics::lines(
        x = seq(first_year, last_year, 1),
        y = biocol_data$wood_harvest_overtime,
        type = "l",
        col = colz[13]
      )
    }
    }
  )
  withr::with_par(new = list(bty = "n", oma = c(0, 0, 0, 0),
                             mar = c(4, 5, 1, 3), new = TRUE),
    {
    if (ref == "pi") {
      biocol_max <- biocol_data$biocol_overtime_abs_frac_piref
      biocol_min <- biocol_data$biocol_overtime_frac_piref
      biocol_mean <- biocol_data$biocol_overtime_pos_frac_piref
    } else if (ref == "act") {
      biocol_max <- biocol_data$biocol_overtime_abs_frac
      biocol_min <- biocol_data$biocol_overtime_frac
      biocol_mean <- biocol_data$biocol_overtime_pos_frac
    } else {
      stop(paste0("Unknown value for parameter ref: ", ref, " - Aborting."))
    }
    graphics::plot(
      NA,
      xlim = plot_years,
      ylab = "",
      xlab = "",
      ylim = c(0, max_val_right),
      xaxs = "i",
      yaxs = "i",
      axes = FALSE
    )
    graphics::polygon(
      x = c(seq(first_year, last_year, 1), seq(last_year, first_year, -1)),
      y = c(biocol_max, rev(biocol_min)),
      border = NA,
      col = colz[9],
    )
    graphics::lines(
      x = seq(first_year, last_year, 1),
      y = biocol_mean,
      type = "l",
      col = colz[8],
    )

    graphics::axis(side = 4, col = colz[8], col.axis = colz[8])
    graphics::mtext(
      text = "fraction of NPPref",
      col = colz[8],
      side = 4,
      line = 2
    )

    if (!is.null(highlight_years)) {
      for (y in highlight_years) {
        graphics::lines(x = c(y, y), y = c(min_val, max_val), col = "grey40")
      }
    }
    if (details) {
      graphics::legend(
        legendpos,
        legend = c(
          "NPPpot (PNV)", "NPPact (landuse)", "NPPeco", "NPPluc", "NPPharv",
          "HANPP sum", "BioCol sum [frac NPPref]",
          "rharvest", "firec", "timber_harvest", "wood_harvest"
        ), col = colz[c(1:6, 8, 10:13)], lty = 1, cex = 1
      )
    } else {
      graphics::legend(
        legendpos,
        legend = c(
          "NPPpot (PNV)", "NPPact (landuse)", "NPPeco", "NPPluc", "NPPharv",
          "HANPP sum", "BioCol sum [frac NPPref]"
        ), col = colz[c(1:6, 8)], lty = 1, cex = 1
      )
    }
    if (!is.null(file)) grDevices::dev.off()
  }
  )
}

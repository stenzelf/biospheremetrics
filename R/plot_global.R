#' Plot global LPJmL array
#'
#' Creates a PNG/eps with a plot of a global LPJmL array
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    colors for pos and neg values can be given, default is Blues for the
#'    positive and Reds for the negative numbers 0-range (from 2^-pow2min to
#'    2^pow2min) is white.
#'    The negatives can be omitted by setting only_pos=TRUE, in case there are
#'    only pos values.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max for exponential legend: upper (positive) end of data range to
#'                plot (2^pow2max)
#' @param pow2min for exponential legend: smallest positive number to be
#'                distinguished from 0 (2^-pow2min)
#' @param max for linear legend: upper end of data range to plot (0 is placed
#'            symmetrically between min and max, if onlypos = FALSE)
#' @param min for linear legend: lower end of data range to plot (0 is placed
#'            symmetrically between min and max, if onlypos = FALSE)
#' @param col_pos color palette for the positives
#' @param col_neg color palette for the negatives
#' @param type string indicating whether to plot exponential (exp) or
#'             linear (lin) legend (default: exp)
#' @param legendtitle character string legend title
#' @param leg_yes boolean whether to show legend (default: TRUE)
#' @param only_pos boolean to show only positive half of legend (default: FALSE)
#' @param eps boolean whether to write eps file instead of PNG (default: FALSE)
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' plot_global(
#'   data = irrigation2006,
#'   file = paste("~/", "mwateramount_2005_06.png", sep = ""),
#'   title = paste("irrigation amount 2006 in mm/yr", sep = ""),
#'   pow2max = 15,
#'   pow2min = 0,
#'   legendtitle = "legendtitle",
#'   leg_yes = TRUE,
#'   eps = FALSE
#' )
#' }
#'
#' @export
plot_global <- function(data,
                        file,
                        title = "",
                        pow2max = NULL,
                        pow2min = NULL,
                        min = NULL,
                        max = NULL,
                        col_pos = "GnBu",
                        type = "exp",
                        col_neg = "YlOrRd",
                        legendtitle = "",
                        leg_yes = TRUE,
                        only_pos = FALSE,
                        eps = FALSE) {
  if (eps) {
    file <- strsplit(file, ".", fixed = TRUE)[[1]]
    file <- paste(c(file[1:(length(file) - 1)], "eps"), collapse = ".")

    grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
    grDevices::postscript(file, horizontal = FALSE, onefile = FALSE, width = 22,
                          height = 8.5, paper = "special")

  } else {
    grDevices::png(file, width = 7.25, height = 3.5, units = "in", res = 300,
                   pointsize = 6, type = "cairo")
  }

  plot_global_to_screen(
    data = data, title = title, pow2max = pow2max, type = type,
    pow2min = pow2min, min = min, max = max, col_pos = col_pos,
    col_neg = col_neg, legendtitle = legendtitle, leg_yes = leg_yes,
    only_pos = only_pos
  )
  grDevices::dev.off()
}


#' Plot global LPJmL array
#'
#' Plot of a global LPJmL array inside RStudio
#'    Data is plotted in range: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'    where the positive values are colored green to blue,
#'    0-range is white,
#'    and the negative ones red to yellow
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to
#' @param title character string title for plot
#' @param pow2max for exponential legend: upper (positive) end of data range to
#'                plot (2^pow2max)
#' @param pow2min for exponential legend: smallest positive number to be
#'                distinguished from 0 (2^-pow2min)
#' @param max for linear legend: upper end of data range to plot (0 is placed
#'            symmetrically between min and max, if onlypos = FALSE)
#' @param min for linear legend: lower end of data range to plot (0 is placed
#'            symmetrically between min and max, if onlypos = FALSE)
#' @param col_pos color palette for the positives
#' @param col_neg color palette for the negatives
#' @param type string indicating whether to plot exponential (exp) or
#'             linear (lin) legend (default: exp)
#' @param legendtitle character string legend title
#' @param leg_yes boolean whether to show legend (default: TRUE)
#' @param only_pos boolean to show only positive half of legend (default: FALSE)
#' @param eps boolean whether to write eps file instead of PNG (default: FALSE)
#'
#' @return None
#
#' @examples
#' \dontrun{
#' plot_global_to_screen(
#'   data = irrigation2006,
#'   title = paste("irrigation amount 2006 in mm/yr", sep = ""),
#'   pow2max = 15,
#'   pow2min = 0,
#'   "legendtitle",
#'   leg_yes = TRUE
#' )
#' }
#'
#' @export
plot_global_to_screen <- function(data,
                                  title = "",
                                  pow2max = NULL,
                                  pow2min = NULL,
                                  min = NULL,
                                  max = NULL,
                                  col_pos = "GnBu",
                                  type = "exp",
                                  col_neg = "YlOrRd",
                                  legendtitle = "",
                                  leg_yes = TRUE,
                                  only_pos = FALSE) {
  if (only_pos) {
    if (type == "exp") {
      if (is.null(pow2max) | is.null(pow2min)) {
        stop("For exponental legend, pow2min and pow2max need to be specified.")
      }
      # actual brks and ticks
      legendticks <- c(0, 2^seq(pow2min, pow2max, 1))
      # just for displaying an equally sized legend
      brks <- c(seq(pow2min, pow2max, length.out = length(legendticks)))

    } else if (type == "lin") {
      if (is.null(max) | is.null(min)) {
        stop("For linear legend, min and max need to be specified.")
      }
      legendticks <- seq(min, max, length.out = 10)
      brks <- legendticks
    }
    palette <- c(
      "white",
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, col_pos))(length(legendticks) - 2) # nolint
    )

  } else {
    if (type == "exp") {
      if (is.null(pow2max) | is.null(pow2min)) {
        stop("For exponental legend, pow2min and pow2max need to be specified.")
      }
      legendticks <- c(
        -(2^seq(pow2max, pow2min, -1)), 2^seq(pow2min, pow2max, 1)
      )
      brks <- seq(-pow2max, pow2max, length.out = length(legendticks))
    } else if (type == "lin") {
      if (is.null(max) | is.null(min)) {
        stop("For linear legend, min and max need to be specified.")
      }
      legendticks <- seq(min, max, length.out = 20)
      brks <- legendticks
    }
    palette <- c(
      rev(
        grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, col_neg))(length(legendticks) / 2 - 1) # nolint
      ),
      "white",
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, col_pos))(length(legendticks) / 2 - 1) # nolint
    )
  }
  data[data < legendticks[1]] <- legendticks[1]
  data[data > legendticks[length(legendticks)]] <- (
    legendticks[length(legendticks)]
  )

  ra <- raster::raster(ncols = 720, nrows = 360)
  range <- range(data)
  ra[raster::cellFromXY(ra, cbind(lon, lat))] <- data
  extent <- raster::extent(c(-180, 180, -60, 90))

  if (leg_yes) {
    graphics::par(bty = "n", oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 3),
                  xpd = TRUE)
  } else {
    graphics::par(bty = "n", oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
  }
  raster::plot(ra, ext = extent, breaks = legendticks, col = palette, main = "",
               legend = FALSE, axes = FALSE)
  title(title, line = -1)
  if (leg_yes) {
    if (type == "exp") {
      fields::image.plot(
        legend.only = TRUE, zlim = c(-pow2max, pow2max), col = palette,
        useRaster = FALSE, breaks = brks, lab.breaks = round(legendticks, 2),
        legend.shrink = 0.8,
        legend.args = list(legendtitle, side = 3, font = 2, line = 1)
      )
    } else { # linear plotting
      fields::image.plot(
        legend.only = TRUE, zlim = c(min, max), col = palette,
        useRaster = FALSE, breaks = brks, lab.breaks = round(legendticks, 2),
        legend.shrink = 0.8,
        legend.args = list(legendtitle, side = 3, font = 2, line = 1)
      )
    }
  }
  maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))
}

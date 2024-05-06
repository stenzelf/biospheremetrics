#' Plot global LPJmL array
#'
#' Plot global LPJmL array to file (if file argument is given) or screen (else).
#'    type argument controls plot type: (exponential, linear, or manual legend).
#'    Depending on this more parameters are required.
#'    Data plot ranges:
#'      exp: c(-2^pow2max,-2^-pow2min,0,2^-pow2min,2^pow2max)
#'      lin: c(min,max)
#'      man: brks
#'    colors for pos and neg values can be given, default is Blues for the
#'    positive and Reds for the negative numbers 0-range (from 2^-pow2min to
#'    2^pow2min) is white.
#'    The negatives can be omitted by setting only_pos=TRUE, in case there are
#'    only pos values.
#'
#' @param data array with data to plot in LPJmL specific array c(67420)
#' @param file character string for location/file to save plot to, if not
#'             supplied, the plot is displayed to screen (default: NULL)
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
#' @param brks breaks for manual plotting type (type=man) default: NULL
#' @param palette palette for manual plotting type (type=man) default: NULL
#' @param type string indicating whether to plot manual (man),
#'             exponential (exp) or linear (lin) legend (default: exp).
#'             man requires: parameters brks and palette defined,
#'             exp requires: parameters pow2min and pow2max given defined,
#'             lin requires: parameters min and max defined
#' @param legendtitle character string legend title
#' @param leg_yes boolean whether to show legend (default: TRUE)
#' @param n_legend_ticks (default: 20)
#' @param min_0 (default: 0.01)
#' @param only_pos boolean to show only positive half of legend (default: FALSE)
#' @param eps boolean whether to write eps file instead of PNG (default: FALSE)
#'
#' @examples
#' \dontrun{
#' plot_global(
#'   data = biocol_data$biocol[,"2015"],
#'   file = "BioCol_absolute.png",
#'   type = "exp",
#'   pow2min = 0,
#'   pow2max = 12,
#'   legendtitle = "GtC",
#'   leg_yes = TRUE,
#'   only_pos = FALSE,
#'   eps = FALSE
#' )
#' }
#'
#' @md
#' @export
plot_global <- function(data,
                        file = NULL,
                        title = "",
                        pow2min = NULL,
                        pow2max = NULL,
                        min = NULL,
                        max = NULL,
                        brks = NULL,
                        palette = NULL,
                        col_pos = "GnBu",
                        type = "exp",
                        col_neg = "YlOrRd",
                        legendtitle = "",
                        leg_yes = TRUE,
                        only_pos = FALSE,
                        n_legend_ticks = 20,
                        min_0 = 0.01,
                        eps = FALSE) {
  if (!is.null(file)) {
    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[seq_len(length(file) - 1)], "eps"), collapse = ".")

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
  if (!is.null(min)){
    if (min == 0) only_pos <- TRUE
  }
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
      legendticks <- seq(min, max, length.out = n_legend_ticks)
      brks <- legendticks
    }
    palette <- c(
      "white",
      grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(9, col_pos))(length(legendticks) - 2)
    )
  } else {
    if (type == "exp" || type == "lin") {
      if (type == "exp") {
        if (is.null(pow2max) | is.null(pow2min)) {
          stop("For exponental legend, pow2min and pow2max are required.")
        }
        legendticks <- c(
          -(2^seq(pow2max, pow2min, -1)), 2^seq(pow2min, pow2max, 1)
        )
        brks <- seq(-pow2max, pow2max, length.out = length(legendticks))
      } else if (type == "lin") {
        if (is.null(max) | is.null(min)) {
          stop("For linear legend, min and max need to be specified.")
        }
        if (n_legend_ticks %% 2 == 0) {
          n_legend_ticks <- n_legend_ticks + 1
        }
        legendticks <- c(
          seq(min, 0, length.out = n_legend_ticks),
          seq(0, max, length.out = n_legend_ticks)
        )
        legendticks[c(n_legend_ticks, (n_legend_ticks + 1))] <- c(-min_0, min_0)
        brks <- legendticks
      }
      palette <- c(
        rev(
          grDevices::colorRampPalette(
            RColorBrewer::brewer.pal(9, col_neg))(length(legendticks) / 2 - 1)
        ),
        "white",
        grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(9, col_pos))(length(legendticks) / 2 - 1)
      )
    } else { # type == man
      if (is.null(palette)) {
        message("Manual breaks, but not palette given, using default.")
        palette <- grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(9, col_pos))(length(brks) - 1)
      }
      if (only_pos) stop("Manual breaks and palette, but conflicting parameter
              only_pos == TRUE defined. Aborting.")
      legendticks <- brks
    }
  }
  data[data < legendticks[1]] <- legendticks[1]
  data[data > legendticks[length(legendticks)]] <- (
    legendticks[length(legendticks)]
  )

  ra <- terra::rast(ncols = 720, nrows = 360)
  range <- range(data)
  ra[terra::cellFromXY(ra, cbind(lon, lat))] <- data
  extent <- terra::ext(c(-180, 180, -60, 90))

  if (leg_yes) {
    oma_p = c(0, 0, 0, 3)
  } else {
    oma_p = c(0, 0, 0, 0)
  }
  withr::with_par(new = list(
    bty = "n", oma = oma_p, mar = c(0, 0, 0, 0), xpd = TRUE),
    {terra::plot(ra,
      ext = extent, breaks = legendticks, col = palette, main = title,
      legend = FALSE, axes = FALSE
    )
    maps::map("world", add = TRUE, res = 0, lwd = 0.1, ylim = c(-60, 90))
    title(title, line = -1)
    if (leg_yes) {
      if (type == "exp") {
        fields::image.plot(
          legend.only = TRUE, zlim = c(-pow2max, pow2max), col = palette,
          useRaster = FALSE, breaks = brks, lab.breaks = round(legendticks, 2),
          legend.shrink = 0.7,
          legend.args = list(legendtitle, side = 3, font = 2, line = 1),
          smallplot = c(0.975, 0.99, 0.1, 0.9)
        )
      } else { # manual plotting
        fields::image.plot(
          legend.only = TRUE, zlim = range(brks), col = palette,
          useRaster = FALSE, breaks = brks, lab.breaks = round(legendticks, 2),
          legend.shrink = 0.7,
          legend.args = list(legendtitle, side = 3, font = 2, line = 1)
        )
      }
    }

    if (!is.null(file)) {
      grDevices::dev.off()
    }
    }
  )
}

# written by Fabian Stenzel, based on work by Sebastian Ostberg
# 2022-2023 - stenzel@pik-potsdam.de

################# EcoRisk plotting functions ##################

#' Plot distribution of similarity within biomes
#'
#' Function to plot the distribution of similarity within biomes
#'
#' @param data data object with distibution - as returned by
#'             calculateWithInBiomeDiffs for each subcategory of ecorisk.
#'             dim: c(biomes,bins)
#' @param file to write into, if not supplied (default is NULL) write to screen
#' @param biomes_abbrv character vector. abbreviated names of biomes
#'        (defaults to NULL -> extract dimension names from data)
#' @param scale scaling factor for distribution. defaults to 1
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title, default empty
#' @param eps write as eps or png (default: FALSE -> png)
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg
#'        color scheme white-blue-yellow-red
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_biome_internal_distribution <- function(
    data,
    file = NULL,
    biomes_abbrv = NULL,
    scale = 1,
    title = "",
    legendtitle = "",
    eps = FALSE,
    palette = NULL) {
  if (!is.null(file)) {
    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[seq_len((length(file) - 1))], "eps"), collapse = ".")
      grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
      grDevices::postscript(file,
        horizontal = FALSE, onefile = FALSE, width = 8,
        height = 16, paper = "special"
      )
    } else {
      grDevices::png(file,
        width = 3, height = 6, units = "in", res = 300,
        pointsize = 6, type = "cairo"
      )
    }
  }
  di <- dim(data)
  bins <- di["bin"]
  res <- 1 / bins
  biomes <- di["biome"]
  if (is.null(biomes_abbrv)) biomes_abbrv <- names(data)$biome

  if (is.null(palette)) {
    palette <- c("white", RColorBrewer::brewer.pal(9, "YlOrRd"))
  }
  col_index <- floor(seq(res / 2, 1 - res / 2, res) * 10) + 1

  withr::with_par(
    new = list(mar = c(2, 4, 0, 0), oma = c(0, 0, 0, 0)), # bltr
    {
      graphics::plot(NA,
        xlim = c(0, 1), ylim = c(0, 20), xlab = "EcoRisk",
        main = title, axes = FALSE, ylab = ""
      )
      graphics::axis(side = 2, labels = FALSE, at = seq_len(biomes))
      brks <- seq(0, 1, 0.1)
      fields::image.plot(
        legend.only = TRUE, col = palette,
        useRaster = FALSE, breaks = brks, horizontal = TRUE,
        lab.breaks = brks, legend.shrink = 0.925,
        legend.args = list("", side = 3, font = 2, line = 1.5)
      )
      graphics::mtext(biomes_abbrv,
        side = 2, line = 1,
        at = seq_len(biomes), las = 2
      )
      for (b in seq_len(biomes)) {
        graphics::rect(
          xleft = seq(0, 1 - res, res),
          xright = seq(res, 1, res),
          ybottom = b,
          ytop = b + data[b, ] * scale,
          col = palette[col_index]
        )
      }
      if (!is.null(file)) grDevices::dev.off()
    }
  )
}

#' Plot EcoRisk map to file
#'
#' Function to plot a global map of EcoRisk values [0-1] per grid cell to file
#'
#' @param ecorisk_object ecorisk object from which to plot
#' @param plot_dimension which dimension from ecorisk object to plot
#' @param file to write into, if not supplied (default is NULL) write to screen
#' @param focus_biome highlight the biome with this id and desaturate all other
#'                    (default NULL -- no highlight)
#' @param biome_classes to mask the focus_biome from
#' @param title character string title for plot, default empty
#' @param legendtitle character string legend title
#' @param eps write as eps or png
#' @param title_size size of the title (default: 1)
#' @param leg_yes logical. whether to plot legend or not. defaults to TRUE
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg
#'        color scheme white-blue-yellow-red
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_map <- function(
    ecorisk_object,
    plot_dimension,
    year = 1,
    file = NULL,
    focus_biome = NULL,
    biome_classes = NULL,
    title = "",
    legendtitle = "",
    eps = FALSE,
    title_size = 1,
    leg_yes = TRUE,
    palette = NULL) {
  data <- ecorisk_object[[plot_dimension]]
  di <- DIM(data)
  if (length(di) == 2) {
    data <- data[, year]
  } else if (length(di) == 1) {
    data <- data
  } else {
    stop(paste0("Unknown dimensions in ecorisk dimension ", plot_dimension, " :", di))
  }
  lat <- ecorisk_object$lat
  lon <- ecorisk_object$lon
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)
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
  brks <- seq(0, 1, 0.1)
  data[data < brks[1]] <- brks[1]
  data[data > brks[length(brks)]] <- brks[length(brks)]

  if (is.null(palette)) {
    palette <- c("white", RColorBrewer::brewer.pal(9, "YlOrRd"))
  }

  if (!is.null(focus_biome)) {
    focus <- data
    focus[!(biome_classes == focus_biome)] <- NA
    palette_low_sat <- grDevices::adjustcolor(palette, alpha.f = 0.25)
    ra_f <- terra::rast(ncols = 720, nrows = 360)
    ra_f[terra::cellFromXY(ra_f, cbind(lon, lat))] <- focus
  }

  ra <- terra::rast(ncols = 720, nrows = 360)
  ra[terra::cellFromXY(ra, cbind(lon, lat))] <- data
  range <- range(data)
  extent <- terra::ext(c(-180, 180, -60, 90))
  withr::with_par(new = list(mar = c(0, 0, 1, 3), oma = c(0, 0, 0, 0), bty = "n"), {
    if (is.null(focus_biome)) {
      terra::plot(ra,
        ext = extent, breaks = brks, col = palette, main = "",
        legend = FALSE, axes = FALSE
      )
    } else {
      terra::plot(ra,
        ext = extent, breaks = brks, col = palette_low_sat,
        main = "", legend = FALSE, axes = FALSE
      )
      terra::plot(ra_f,
        ext = extent, breaks = brks, col = palette, main = "",
        legend = FALSE, axes = FALSE, add = TRUE
      )
    }

    title(main = title, line = -2, cex.main = title_size)
    maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))

    if (leg_yes) {
      fields::image.plot(
        legend.only = TRUE, col = palette, breaks = brks, zlim = range,
        lab.breaks = brks, legend.shrink = 0.7,
        legend.args = list(legendtitle, side = 3, font = 2, line = 1)
      ) # removed zlim
    }
    if (!is.null(file)) grDevices::dev.off()
  })
}


#' Plot radial EcoRisk plot to screen
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to screen
#'
#' @param data EcoRisk data array c([nEcoRiskcomponents],
#'             3[min,mean,max])
#' @param title character string title for plot, default empty
#' @param zoom scaling factor for circle plot. defaults to 1
#' @param type plot type, 'legend1' for variable and color legend,
#'             'legend2' for value legend, or 'regular' (default setting)
#'             for the regular EcoRisk plot
#' @param title_size scaling factor for tile. defaults to 1
#' @param titleline line at which the title will be displayed. defaults to -2
#' @param use_quantile use quantiles or min,mean,max. defaults to TRUE
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_radial_to_screen <- function(data, # nolint
                                          title = "",
                                          zoom = 1.0,
                                          type = "regular",
                                          title_size = 2,
                                          titleline = -2,
                                          use_quantile = TRUE) {
  ecorisk_dims <- length(data[, 1])
  if (ecorisk_dims == 8) {
    # names <- c(
    #  ecorisk = "ecorisk", deltav = "vegetation\nstructure",
    #  local = "local\nchange", global = "global\nimportance",
    #  balance = "ecosystem\nbalance", cstocks = "carbon\nstocks",
    #  cfluxes = "carbon fluxes", wfluxes = "water fluxes"
    # )

    # take the names of the ecorisk list dimensions, removing "_total"
    names <- gsub("_", "\n", gsub("_total", "", dimnames(data)[[1]]))
    # c(blue-green, yellow, violet, red, blue, orange, green, pink, grey,
    #   purple, green-blue, yellow-orange)

    set <- RColorBrewer::brewer.pal(12, "Set3")
    colz <- set[c(4, 7, 8, 11, 1, 10, 5, 6)]
    #   ecorisk vs         lc        gi        eb        ct       wt         nt
    angles <- matrix(
      c(
        90, 270, 216, 252, 180, 216, 144, 180,
        108, 144, -18, 18, -54, -18, 18, 54
      ),
      byrow = TRUE,
      nrow = length(colz)
    )
  } else {
    stop("Unknown number of dimensions for ecorisk data:", ecorisk_dims)
  }

  withr::with_par(new = list(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)), {
    graphics::plot(c(-zoom, zoom), c(-zoom, zoom),
      type = "n", axes = FALSE,
      ann = FALSE, asp = 1, main = ""
    )
    graphics::title(main = title, line = titleline, cex.main = title_size)

    if (type == "legend1") {
      circlize::draw.sector(0, 360, rou1 = 1)
      ro <- c(1, 1.1, 0.8, 1.1, 0.8, 1, 1, 1, 1, 1, 1)

      for (i in seq_along(angles[, 1])) {
        circlize::draw.sector(
          start.degree = angles[i, 1] + 90,
          end.degree = angles[i, 2] + 90,
          col = colz[i],
          clock.wise = FALSE,
          rou1 = 0,
          rou2 = ro[i],
          border = "black"
        )
      }

      if (ecorisk_dims == 8) {
        graphics::text(names,
          x = c(1.1, 1.0, 0.2, -0.8, -1.6, -0.25, 0.7, -1.3),
          y = c(-0.15, -0.9, -1.3, -1.3, -0.9, 1.2, 1, 1),
          adj = 0
        )
      } else {
        stop("Unknown number of dimensions for ecorisk data:", ecorisk_dims)
      }

      # line lc
      circlize::draw.sector(
        start.degree = (angles[3, 1] + angles[3, 2]) / 2 + 90,
        end.degree = (angles[3, 1] + angles[3, 2]) / 2 + 90,
        rou1 = 0.7,
        rou2 = 1.1
      )
      # line ecorisk
      circlize::draw.sector(
        start.degree = -9,
        end.degree = -9,
        rou1 = 0.9,
        rou2 = 1.05
      )
      # line eb
      circlize::draw.sector(
        start.degree = (angles[5, 1] + angles[5, 2]) / 2 + 90,
        end.degree = (angles[5, 1] + angles[5, 2]) / 2 + 90, rou1 = 0.7,
        rou2 = 1.1
      )
      circlize::draw.sector(
        start.degree = 180,
        end.degree = 180,
        clock.wise = FALSE,
        rou1 = -1.2,
        rou2 = 1.2,
        border = "black",
        lwd = 2
      )
    } else if (type == "legend2") {
      graphics::text("+", x = 0, y = 0)
      circlize::draw.sector(0, 360, rou1 = 1)
      circlize::draw.sector(0, 360, rou1 = 0.65)
      circlize::draw.sector(0, 360, rou1 = 0.3)
      # sector
      circlize::draw.sector(
        start.degree = -18 + 90,
        end.degree = 18 + 90,
        clock.wise = FALSE,
        rou1 = 0.55,
        border = "black"
      )
      # uncertainty arrow
      circlize::draw.sector(
        start.degree = 90,
        end.degree = 90,
        clock.wise = FALSE,
        rou1 = 0.4,
        rou2 = 0.8,
        border = "black"
      )
      # uncertainty lower
      circlize::draw.sector(
        start.degree = -9 + 90,
        end.degree = 9 + 90,
        clock.wise = FALSE,
        rou1 = 0.8,
        rou2 = 0.8,
        border = "black"
      )
      # uncertainty upper
      circlize::draw.sector(
        start.degree = -9 + 90,
        end.degree = 9 + 90,
        clock.wise = FALSE,
        rou1 = 0.4,
        rou2 = 0.4,
        border = "black"
      )
      # 0.3
      circlize::draw.sector(
        start.degree = 270 - 270,
        end.degree = 270 - 270,
        clock.wise = FALSE,
        rou1 = 0.3,
        rou2 = 1.3,
        border = "black",
        lty = "dashed"
      )
      # 0.65
      circlize::draw.sector(
        start.degree = 280 - 270,
        end.degree = 280 - 270,
        clock.wise = FALSE,
        rou1 = 0.65,
        rou2 = 1.3,
        border = "black",
        lty = "dashed"
      )
      # 1.0
      circlize::draw.sector(
        start.degree = 290 - 270,
        end.degree = 290 - 270,
        clock.wise = FALSE,
        rou1 = 1,
        rou2 = 1.3,
        border = "black",
        lty = "dashed"
      )
      graphics::text(c("0.3", "0.65", "1"),
        x = c(1.4, 1.45, 1.25),
        y = c(0, 0.25, 0.45)
      )

      # plot how the whiskers are calculated
      #   quantile case
      if (use_quantile) {
        graphics::text(c("Q90", "Q50", "Q10"),
          x = c(-0.3, -0.29, -0.26),
          y = c(0.8, 0.48, 0.35),
          cex = 0.7
        )

        # minmeanmax case
      } else {
        graphics::text(c("max", "mean", "min"),
          x = c(-0.3, -0.29, -0.26),
          y = c(0.8, 0.48, 0.35),
          cex = 0.7
        )
      }
    } else if (type == "regular") {
      circlize::draw.sector(180, 360, rou1 = 1, col = "gray80")

      for (i in seq_along(angles[, 1])) {
        mangle <- mean(angles[i, ])
        if (i == 1) mangle <- -98
        dmin <- data[i, 1]
        dmedian <- data[i, 2]
        dmax <- data[i, 3]
        circlize::draw.sector(
          start.degree = angles[i, 1] + 90,
          end.degree = angles[i, 2] + 90, col = colz[i],
          rou1 = dmedian,
          clock.wise = FALSE,
          border = "black"
        )
        # uncertainty arrow
        circlize::draw.sector(
          start.degree = mangle + 90,
          end.degree = mangle + 90,
          clock.wise = FALSE,
          rou1 = dmin,
          rou2 = dmax,
          border = "black"
        )
        circlize::draw.sector(
          start.degree = mangle - 9 + 90,
          end.degree = mangle + 9 + 90,
          clock.wise = FALSE,
          rou1 = dmin,
          rou2 = dmin,
          border = "black"
        )
        # uncertainty upper
        circlize::draw.sector(
          start.degree = mangle - 9 + 90,
          end.degree = mangle + 9 + 90,
          clock.wise = FALSE,
          rou1 = dmax,
          rou2 = dmax,
          border = "black"
        )
      }
      circlize::draw.sector(0, 360, rou1 = 1)
      circlize::draw.sector(0, 360, rou1 = 0.6)
      circlize::draw.sector(0, 360, rou1 = 0.3)
      circlize::draw.sector(
        start.degree = 180,
        end.degree = 180,
        clock.wise = FALSE,
        rou1 = -1.2,
        rou2 = 1.2,
        border = "black",
        lwd = 2
      )
    } else {
      stop(
        "Unknown type ", type,
        ". Please use 'legend1' for variable and color legend,
           'legend2' for value legend, or 'regular' (default setting) ",
        "for the regular ecorisk plot."
      )
    }
  })
}


#' Plot radial EcoRisk plot to file
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],
#'             3[min,mean,max])
#' @param file to write into
#' @param title character string title for plot, default empty
#' @param use_quantile show quantiles or min,mean,max
#' @param eps write as eps or png
#' @param leg_yes logical. whether to plot legend or not. defaults to TRUE
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_radial <- function(data,
                                file,
                                title = "",
                                leg_yes = TRUE,
                                eps = FALSE,
                                use_quantile = TRUE) {
  path_write <- dirname(file)
  dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)
  if (length(which(data < 0 | data > 1)) > 0) {
    warning(
      "There are values in data outside the expected EcoRisk range [0..1]."
    )
  }

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

  # adjust the margins, dependent on whether a legend should be plotted or not
  withr::with_par(
    new = list(fig = c(0, 0.7, 0, 1)), # , oma=c(0,0,0,0),mar=c(0,0,0,0))
    # plot main EcoRisk radial
    plot_ecorisk_radial_to_screen(
      data = data, title = title, zoom = 1.0,
      type = "regular"
    )
  )
  if (leg_yes) {
    withr::with_par(
      new = list(fig = c(0.7, 1, 0, 0.5), new = TRUE),
      plot_ecorisk_radial_to_screen(
        data = data, title = "", zoom = 1.5,
        type = "legend1"
      )
    )
    withr::with_par(
      new = list(fig = c(0.7, 1, 0.5, 1), new = TRUE),
      plot_ecorisk_radial_to_screen(
        data = data, title = "", zoom = 1.5,
        type = "legend2", use_quantile = use_quantile
      )
    )
  }
  grDevices::dev.off()
}

#' Plot EcoRisk maps
#'
#' Function to plot with one command maps of all components of EcoRisk to files
#'
#' @param ecorisk EcoRisk object e.g. returned from calc_ecorisk
#' @param out_folder folder to plot the data into
#' @param year which year to plot, supply either as index, or character string
#'             of year (default = 1)
#'
#' @examples
#' \dontrun{
#' plot_ecorisk_maps(
#'   ecorisk = ecorisk,
#'   out_folder = "./plots/ecorisk/"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_maps <- function(ecorisk, out_folder, year = 1) {
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "ecorisk_total", year = year, file = paste0(out_folder, "/ecorisk_total.png"), title = "ecorisk")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "vegetation_structure_change", year = year, file = paste0(out_folder, "/ecorisk_vs.png"), title = "vegetation structure change")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "local_change", year = year, file = paste0(out_folder, "/ecorisk_lc.png"), title = "local change")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "global_importance", year = year, file = paste0(out_folder, "/ecorisk_gi.png"), title = "global importance")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "ecosystem_balance", year = year, file = paste0(out_folder, "/ecorisk_eb.png"), title = "ecosystem balance")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "carbon_stocks", year = year, file = paste0(out_folder, "/ecorisk_cs.png"), title = "carbon_stocks")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "carbon_fluxes", year = year, file = paste0(out_folder, "/ecorisk_cf.png"), title = "carbon_fluxes")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "carbon_total", year = year, file = paste0(out_folder, "/ecorisk_ct.png"), title = "carbon_total")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "water_total", year = year, file = paste0(out_folder, "/ecorisk_wt.png"), title = " water_total")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "water_fluxes", year = year, file = paste0(out_folder, "/ecorisk_wf.png"), title = " water_fluxes")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "nitrogen_stocks", year = year, file = paste0(out_folder, "/ecorisk_ns.png"), title = " nitrogen_stocks")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "nitrogen_fluxes", year = year, file = paste0(out_folder, "/ecorisk_nf.png"), title = " nitrogen_fluxes")
  biospheremetrics::plot_ecorisk_map(ecorisk, plot_dimension = "nitrogen_total", year = year, file = paste0(out_folder, "/ecorisk_nt.png"), title = " nitrogen_total")
}

#' Plot timeline of EcoRisk variables to screen
#'
#' Function to plot timeline of EcoRisk variables to screen
#'
#' @param data EcoRisk data array
#'        c(4/19[biomes],8/10[nEcoRiskcomponents],3[min,mean,max],timeslices)
#' @param timerange of the data input
#' @param yrange range for y axis default c(0,1)
#' @param leg_yes plot legend (default TRUE)
#' @param leg_only plot only the legend? default: FALSE
#' @param varnames manual override of ecorisk subvariable names - default: NULL
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_overtime_to_screen <- function(data,
                                    timerange,
                                    yrange = c(0, 1),
                                    leg_yes = TRUE,
                                    leg_only = FALSE,
                                    varnames = NULL) {
  ecorisk_dims <- dim(data)[1]

  if (is.null(varnames)) {
    if (ecorisk_dims == 10) {
      names <- c(
        ecorisk = "ecorisk", deltav = "vegetation structure",
        local = "local change", global = "global importance",
        balance = "ecosystem balance", cstocks = "carbon stocks",
        cfluxes = "carbon fluxes", wfluxes = "water fluxes",
        nstocks = "nitrogen stocks", nfluxes = "nitrogen fluxes"
      )
      # c(blue-green, yellow, violet, red, blue, orange, green, pink, grey,
      #   purple, green-blue, yellow-orange)
      set <- RColorBrewer::brewer.pal(12, "Set3")
      colz <- set[c(4, 7, 8, 11, 1, 3, 10, 5, 12, 6)]
    } else if (ecorisk_dims == 8) {
      names <- c(
        ecorisk = "ecorisk", deltav = "vegetation structure",
        local = "local change", global = "global importance",
        balance = "ecosystem balance", cstocks = "carbon stocks",
        cfluxes = "carbon fluxes", wfluxes = "water fluxes"
      )
      colz <- c(
        "darkgoldenrod", RColorBrewer::brewer.pal(5, "Greens")[5],
        RColorBrewer::brewer.pal(6, "Set1")[seq(2, 6, by = 2)],
        rev(RColorBrewer::brewer.pal(6, "Oranges")[c(4, 5)]),
        RColorBrewer::brewer.pal(6, "PuBu")[6]
      )
    } else {
      stop("Unknown number of dimensions for ecorisk data: ", ecorisk_dims)
    }
  } else {
    names <- varnames
    colz <- RColorBrewer::brewer.pal(length(names), "Set2")
  }
  years <- timerange[1]:timerange[2]
  if (leg_only) {
    graphics::plot(NA,
      ylim = c(yrange[1], yrange[2]), cex.axis = 1,
      axes = FALSE, xlab = "", ylab = ""
    )

    graphics::legend("center", legend = names, fill = colz, border = colz)
  } else {
    graphics::plot(NA,
      xlim = timerange, ylim = c(yrange[1], yrange[2]),
      cex.axis = 1, xlab = "", ylab = ""
    )
    for (i in seq_len(ecorisk_dims)) {
      if (i == 1) {
        graphics::lines(x = years, y = data[i, 2, ], col = colz[i], lwd = 4)
      } else {
        graphics::lines(x = years, y = data[i, 2, ], col = colz[i], lwd = 2)
      }
    }
    if (leg_yes) graphics::legend("topleft", legend = names, fill = colz)
  }
}

#' Plot timeline of EcoRisk variables as panel to file with 4/16 biomes
#'
#' Function to plot a panel of 4/16 timelines per biome aggregated EcoRisk
#' values [0-1]
#' to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],
#'             3[min,mean,max])
#' @param biome_names names of biomes
#' @param file to write into (if not supplied - default NULL - prints to screen)
#' @param yrange range for y axis (default c(0,1))
#' @param timerange of the data input
#' @param eps write as eps or png
#' @param varnames list vector with variable names
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_over_time_panel <- function(data,
                                         biome_names,
                                         file = NULL,
                                         yrange = c(0, 1),
                                         timerange,
                                         eps = FALSE,
                                         varnames = NULL) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

    if (length(which(data < 0 | data > 1)) > 0) {
      warning("Values in data outside the expected EcoRisk range [0..1].")
    }

    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[seq_len(length(file) - 1)], "eps"), collapse = ".")
      grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
      grDevices::postscript(file,
        horizontal = FALSE, onefile = FALSE, width = 15,
        height = 10, paper = "special"
      )
    } else {
      grDevices::png(file,
        width = 5.25, height = 3.5, units = "in", res = 300,
        pointsize = 6, type = "cairo"
      )
    }
  }
  d <- length(data[, 1, 1, 1])
  withr::with_par(new = list(oma = c(0, 0, 0, 0), mar = c(3, 2, 0.5, 0)), {
    if (d == 16 | d == 4) {
      k <- sqrt(d)
      xs <- seq(0, 0.8, length.out = k + 1)
      ys <- seq(0.98, 0, length.out = k + 1)

      for (x in seq_len(k)) {
        for (y in seq_len(k)) {
          if (x == 1 & y == 1) {
            graphics::par(
              fig = c(xs[x], xs[x + 1], ys[y + 1], ys[y]),
              xpd = TRUE
            )
          } else {
            graphics::par(
              fig = c(xs[x], xs[x + 1], ys[y + 1], ys[y]), xpd = TRUE,
              new = TRUE
            )
          }
          plot_overtime_to_screen(
            data = data[(x - 1) * k + y, , , ],
            timerange = timerange, yrange = yrange,
            leg_yes = FALSE, varnames = varnames
          )
          graphics::mtext(
            text = biome_names[(x - 1) * k + y], side = 3,
            line = 0, cex = 1, font = 2
          )
        }
      }
    } else {
      stop(paste("Unknown number of biomes: ", length(data[, 1, 1, 1])))
    }
  })
  # legend

  withr::with_par(new = list(
    fig = c(0.8, 1, 0.5, 1.0), new = TRUE, oma = c(0, 0, 0, 0),
    mar = c(0, 0, 0, 0)
  ), {
    graphics::plot(NA, axes = FALSE, ylim = c(0, 1), xlim = c(0, 1))
    if (d == 16) {
      graphics::text(
        x = 0.1,
        y = seq(0.95, 0.05, length.out = length(get_biome_names(1))),
        labels = paste0(get_biome_names(1), " : ", get_biome_names(2)),
        cex = 0.7, adj = 0
      )
    }
  })
  withr::with_par(new = list(
    fig = c(0.8, 1, 0.0, 0.5),
    new = TRUE, oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)
  ), {
    if (is.null(varnames)) {
      plot_overtime_to_screen(
        data = data[1, , , ], timerange = timerange,
        leg_yes = FALSE, leg_only = TRUE
      )
    } else {
      graphics::plot(NA, axes = FALSE, ylim = c(0, 1), xlim = c(0, 1))
      colz <- RColorBrewer::brewer.pal(length(varnames), "Set2")
      graphics::legend("center", legend = varnames, fill = colz, cex = 1)
    }
    if (!is.null(file)) grDevices::dev.off()
  })
}


#' Plot radial EcoRisk panel to file with 4/16 biomes
#'
#' Function to plot an aggregated radial status of EcoRisk values [0-1]
#' for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4/19[biomes],[nEcoRiskcomponents],
#'             3[min,mean,max])
#' @param biome_names names of biomes
#' @param file to write into (if not supplied - default NULL - prints to screen)
#' @param use_quantile is it quantiles or minmeanmax data? - text for whiskers
#' @param eps write as eps or png
#'
#' @examples
#' \dontrun{
#' plot_ecorisk_radial_panel(
#'   data = ecorisk_disaggregated_full[-c(3, 18, 19), c(1:5, 8, 9, 13), ],
#'   biome_names = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_radial_panel <- function(data,
                                      biome_names,
                                      file = NULL,
                                      use_quantile = TRUE,
                                      eps = FALSE) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

    if (length(which(data < 0 | data > 1)) > 0) {
      warning("Values in data outside the expected EcoRisk range [0..1].")
    }

    if (eps) {
      file <- strsplit(file, ".", fixed = TRUE)[[1]]
      file <- paste(c(file[seq_len(length(file) - 1)], "eps"), collapse = ".")
      grDevices::ps.options(family = c("Helvetica"), pointsize = 18)
      grDevices::postscript(file,
        horizontal = FALSE, onefile = FALSE, width = 15,
        height = 10, paper = "special"
      )
    } else {
      grDevices::png(file,
        width = 5.25, height = 3.5, units = "in", res = 300,
        pointsize = 6, type = "cairo"
      )
    }
  }
  d <- length(data[, 1, 1])
  if (d == 16 | d == 4) {
    k <- sqrt(d)
    xs <- seq(0, 0.6, length.out = k + 1)
    ys <- seq(0.98, 0, length.out = k + 1)
    for (x in seq_len(k)) {
      for (y in seq_len(k)) {
        if (x == 1 & y == 1) {
          graphics::par(
            fig = c(xs[x], xs[x + 1], ys[y + 1], ys[y]),
            xpd = TRUE, oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)
          )
        } else {
          graphics::par(
            fig = c(xs[x], xs[x + 1], ys[y + 1], ys[y]), xpd = TRUE,
            new = TRUE
          )
        }
        plot_ecorisk_radial_to_screen(
          data = data[(x - 1) * k + y, , ],
          title = "", zoom = 1.0, type = "regular"
        )
        graphics::mtext(
          text = biome_names[(x - 1) * k + y], side = 3,
          line = -0.5, cex = 1, font = 2
        )
      }
    }
  } else {
    stop(paste("Unknown number of biomes: ", length(data[, 1, 1])))
  }

  # legend
  withr::with_par(
    new = list(fig = c(0.6, 1, 0.1, 0.6), new = TRUE),
    plot_ecorisk_radial_to_screen(
      data = data[1, , ], title = "",
      zoom = 1.5, type = "legend1"
    )
  )
  withr::with_par(
    new = list(fig = c(0.6, 1, 0.5, 1.0), new = TRUE),
    plot_ecorisk_radial_to_screen(
      data = data[1, , ], title = "legend", zoom = 1.5,
      type = "legend2", title_size = 1, use_quantile = use_quantile
    )
  )
  if (!is.null(file)) grDevices::dev.off()
}


#' Plot biomes with mercator projection
#'
#' Function to plot biomes to file (or screen) using mercator projection
#'
#' @param biome_ids biome id as given by classify_biomes
#' @param biome_name_length length of biome names in legend: 1 - abbreviation,
#'        2 - short name, 3 - full biome name
#' @param order_legend legend order: either "plants" to first have forests, then
#'        grasslands, then tundra ..., or "zones" to go from north to south
#'        (default: "plants")
#' @param file to write into (if not supplied - default NULL - prints to screen)
#' @param title character string title for plot, default empty
#' @param title_size size of title in cex units (defaukt: 2)
#' @param leg_yes whether to plot legend (default: True)
#' @param leg_scale size of legend in cex units (default 0.5)
#' @param eps write as eps, replacing png in filename (default: True)
#'
#' @examples
#' \dontrun{
#' plot_biomes_mercator(
#'   biome_ids = biome_classification$biome_id,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_biomes_mercator <- function(biome_ids,
                                 biome_name_length = 1,
                                 order_legend = "plants",
                                 file = NULL,
                                 title = "",
                                 title_size = 2,
                                 leg_yes = TRUE,
                                 leg_scale = 1,
                                 eps = FALSE) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

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
  # setting up colors and biome names
  colz <- c(
    # warm
    rev(RColorBrewer::brewer.pal(6, "YlOrBr")),
    rev(RColorBrewer::brewer.pal(9, "YlGn")[c(3, 5, 7, 9)]),
    # cold below forest
    rev(RColorBrewer::brewer.pal(9, "GnBu"))[c(2:4, 6, 8, 9)],
    # "lightblue" # Water
    "white",
    # Rocks & Ice
    "lightgrey",
    # montane Tundra/Grassland
    "pink3"
  )

  if (order_legend == "plants") {
    order_legend <- seq_len(19)
  } else if (order_legend == "zones") {
    order_legend <- c(
      1, 2, 9, 10, 11, 3, 4, 5, 6, 12, 13, 14, 7, 8, 15, 16, 17, 18, 19
    )
  } else {
    stop(
      "Unknown value for parameter order_legend (plants or zones) - ",
      "was given as: ", order_legend
    )
  }
  biome_class_cols <- (
    colz[c(1, 2, 7, 8, 9, 10, 13, 12, 3, 4, 5, 14, 15, 16, 19, 11, 6, 17, 18)]
  )
  biome_class_names <- get_biome_names(biome_name_length)

  if (!(length(biome_class_names) == length(biome_class_cols))) {
    stop("Size of biome class names and colors do not match -- should be 18.")
  }

  # plotting
  brks <- seq(
    min(biome_ids, na.rm = TRUE) - 0.5,
    max(biome_ids, na.rm = TRUE) + 0.5,
    1
  )
  ra <- terra::rast(ncols = 720, nrows = 360)
  range <- range(biome_ids)
  ra[terra::cellFromXY(ra, cbind(lon, lat))] <- biome_ids
  extent <- terra::ext(c(-180, 180, -60, 90))
  withr::with_par(new = list(
    mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), bty = "n"
  ), {
    terra::plot(ra,
      ext = extent, breaks = brks, col = biome_class_cols,
      main = "", legend = FALSE, axes = FALSE
    )
    graphics::title(main = title, line = -2, cex.main = title_size)
    if (leg_yes) {
      graphics::legend(
        x = -180, y = 27, legend = biome_class_names[order_legend],
        fill = biome_class_cols[order_legend],
        col = biome_class_cols[order_legend],
        cex = leg_scale, bg = "white", bty = "o"
      )
    }
    maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))
    if (!is.null(file)) grDevices::dev.off()
  })
}


#' Plot radial EcoRisk with 4/16 biomes
#'
#' Function to plot to file (or screen) an aggregated radial status of EcoRisk
#' values [0-1] for the different sub-categories to file
#'
#' @param data EcoRisk data array c(4[biomes],[nEcoRiskcomponents],
#'             3[min,median,max])
#' @param file to write into (if not supplied - default NULL - prints to screen)
#' @param biome_class_names to write into
#' @param title character string title for plot, default empty
#' @param title_size character string title for plot
#' @param leg_scale character string title for plot
#' @param eps write as eps, replacing png in filename (default: True)
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg
#'        color scheme white-blue-yellow-red
#'
#' @examples
#' \dontrun{
#' plot_biome_internal_distribution(
#'   data = biomes,
#'   file = "./biomes.png"
#' )
#' }
#'
#' @md
#' @export
plot_biome_averages <- function(data,
                                file = NULL,
                                biome_class_names,
                                title = "",
                                title_size = 2,
                                leg_scale = 1,
                                eps = FALSE,
                                palette = NULL) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

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
        width = 4, height = 3, units = "in", res = 300,
        pointsize = 6, type = "cairo"
      )
    }
  }
  # setting up colors and biome names
  brks <- seq(0, 1, 0.1)
  data[data < brks[1]] <- brks[1]
  data[data > brks[length(brks)]] <- brks[length(brks)]

  if (is.null(palette)) {
    palette <- c("white", RColorBrewer::brewer.pal(9, "YlOrRd"))
  }
  col_index <- floor(data[, 2] * 10) + 1

  if (!(length(biome_class_names) == dim(data)[1])) {
    stop("Size of biome class names and data input do not match.")
  }

  # plotting
  graphics::plot(NA,
    xlim = c(0, 1), ylim = c(0, 1), main = title, axes = FALSE,
    cex.main = title_size, xlab = "", ylab = ""
  )
  graphics::legend(
    x = 0, y = 1, legend = biome_class_names,
    fill = palette[col_index], col = palette[col_index],
    border = palette[col_index], cex = leg_scale,
    bg = "white", bty = "o"
  )
  if (!is.null(file)) grDevices::dev.off()
}

#' Plot crosstable showing (dis-)similarity between average biome pixels
#'
#' Function to plot to file (or screen) a crosstable showing (dis-)similarity
#' between average biome pixels based on EcoRisk (former Gamma) metric from
#' LPJmL simulations
#'
#' @param data crosstable data as array with [nbiomes,nbiomes] and row/colnames
#' @param file to write into (if not supplied - default NULL - prints to screen)
#' @param lmar left margin for plot in lines (default: 3)
#' @param eps write as eps or png
#' @param palette color palette to plot EcoRisk with, defaults to the Ostberg
#'        color scheme white-blue-yellow-red
#'
#' @examples
#' \dontrun{
#' plot_ecorisk_cross_table(
#'   data = crosstable,
#'   file = "./ecorisk_crosstable.png"
#' )
#' }
#'
#' @md
#' @export
plot_ecorisk_cross_table <- function(data,
                                     file = NULL,
                                     lmar = 3,
                                     eps = FALSE,
                                     palette = NULL) {
  if (!is.null(file)) {
    path_write <- dirname(file)
    dir.create(file.path(path_write), showWarnings = FALSE, recursive = TRUE)

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
        width = 6, height = 3, units = "in", res = 300,
        pointsize = 6, type = "cairo"
      )
    }
  }
  # data prep
  data <- round(data, digits = 2)
  x <- seq_len(ncol(data))
  y <- seq_len(nrow(data))
  centers <- expand.grid(y, x)
  # coloring
  if (is.null(palette)) {
    palette <- c("white", RColorBrewer::brewer.pal(9, "YlOrRd"))
  }
  brks <- seq(0, 1, 0.1)

  # plot margins
  withr::with_par(
    new = list(mar = c(0, lmar, 2, 0)), # bltr

    {
      graphics::image(x, y, t(data),
        col = palette,
        breaks = brks,
        xaxt = "n",
        yaxt = "n",
        xlab = "",
        ylab = "",
        ylim = c(max(y) + 0.5, min(y) - 0.5)
      )
      graphics::text(centers[, 2], centers[, 1], c(data), col = "black")

      # add margin text
      graphics::mtext(attributes(data)$dimnames[[2]],
        at = seq_len(ncol(data)),
        padj = -1
      )
      graphics::mtext(attributes(data)$dimnames[[1]],
        at = seq_len(nrow(data)),
        side = 2,
        las = 1,
        adj = 1,
        line = 1
      )

      # add black lines
      graphics::abline(h = y + 0.5)
      graphics::abline(v = x + 0.5)

      if (!is.null(file)) grDevices::dev.off()
    }
  )
}

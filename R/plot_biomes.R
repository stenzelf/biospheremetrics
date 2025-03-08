#' Plot global distribution of lpjml simulated biomes
#'
#' Plots a map with the biome distribution as derived from a lpjml run based
#' on the "classify_biomes" function
#'
#' @param biome_data output (list) from classify_biomes()
#'
#' @param file_name directory for saving the plot (character string)
#' @param display_area boolean, adding occupied area per biome (default F)
#' @param to_robinson logical to define if robinson projection should be used
#'        for plotting
#' @param cellarea array with cellarea in m2 per gridcell (default NULL)
#' @param order_legend in which order the biomes should be displayed
#'        default: c(1,2,9,10,11,3,4,5,12,13,14,6,7,8,15,16,17,18,19)
#'
#' @param bg_col character, specify background possible (`NA` for transparent)
#'
#' @examples
#' \dontrun{
#' plot_biomes(
#'   biome_data = biomes,
#'   file_name = "/p/projects/open/Johanna/R/biomes.pfd"
#' )
#' }
#'
#' @md
#' @export

plot_biomes <- function(biome_data,
                        file_name = NULL,
                        display_area = FALSE,
                        to_robinson = TRUE,
                        cellarea = NULL,
                        order_legend = c(
                          1, 2, 9, 10, 11, 3, 4, 5, 12, 13, 14,
                          6, 7, 8, 15, 16, 17, 18, 19
                        ),
                        bg_col = "white",
                        marg = c(4, 0, 0, 0),
                        omar = c(0, 0, 0, 0)) {
  # load required data: bbox, countries
  lpjml_extent <- c(-180, 180, -60, 85)

  bounding_box <- system.file("extdata", "ne_110m_wgs84_bounding_box.shp",
    package = "biospheremetrics"
  ) %>%
    rgdal::readOGR(layer = "ne_110m_wgs84_bounding_box", verbose = FALSE) %>%
    {
      if (to_robinson) sp::spTransform(., sp::CRS("+proj=robin")) else .
    }

  countries <- system.file("extdata", "ne_110m_admin_0_countries.shp",
    package = "biospheremetrics"
  ) %>%
    rgdal::readOGR(layer = "ne_110m_admin_0_countries", verbose = FALSE) %>%
    raster::crop(., lpjml_extent) %>%
    {
      if (to_robinson) sp::spTransform(., sp::CRS("+proj=robin")) else .
    }

  biome_cols <- c(
    "#993404", "#D95F0E", "#004529", "#238443",
    "#D9F0A3", "#4EB3D3", "#2B8CBE", "#c4e2f4",
    "#FE9929", "#FEC44F", "#FEE391", "#A8DDB5",
    "#E0F3DB", "#F7FCF0", "#c79999", "#0868AC",
    "#FFFFD4", "white", "#dad4d4"
  )

  biome_mapping <- system.file("extdata", "biomes.csv",
    package = "biospheremetrics"
  ) %>%
    readr::read_delim(delim = ";", col_types = readr::cols())
  names(biome_cols) <- biome_mapping$short_name

  biome_cols_legend <- biome_cols[order_legend]

  biome_names_legend <- biome_mapping$short_name[order_legend]

  biomes_lpjml <- to_raster(
    lpjml_array = biome_data$biome_id,
    boundary_box = bounding_box,
    ext = lpjml_extent,
    to_robinson = to_robinson
  )

  if (!is.null(file_name)) {
    file_extension <- strsplit(file_name, split = "\\.")[[1]][-1]
    switch(file_extension,
      `png` = {
        png(file_name,
          width = 8 * 1.8,
          height = 4 * 2,
          units = "cm",
          res = 600,
          pointsize = 7
        )
      },
      `pdf` = {
        pdf(file_name,
          width = 8 * 1.8 / 2.54,
          height = (4 * 2) / 2.54,
          pointsize = 7
        )
      },
      {
        stop("File extension ", dQuote(file_extension), " not supported.")
      }
    )
  }
  brk <- seq(
    min(biome_mapping$id) - 0.5,
    max(biome_mapping$id, na.rm = TRUE) + 0.5, 1
  )
  par(oma = omar, mar = marg, xpd = TRUE, bg = bg_col)

  image(biomes_lpjml,
    asp = 1, xaxt = "n", yaxt = "n",
    xlab = "", ylab = "", col = biome_cols, breaks = brk, lwd = 0.1,
    bty = "n"
  )
  raster::plot(countries,
    add = TRUE, lwd = 0.3,
    border = "#5c565667", usePolypath = FALSE
  )
  if (to_robinson == TRUE) {
    ypoint <- (-6736039)
  } else {
    ypoint <- (-67)
  }
  legend_text <- biome_names_legend[1:19]
  if (display_area) {
    if (is.null(cellarea)) stop("Cellarea needs to be supplied for displaying.")
    biome_area <- rep(0, length(order_legend))
    names(biome_area) <- biome_mapping$short_name
    for (i in 1:length(order_legend)) {
      biome_area[i] <- sum(cellarea[which(biome_data$biome_id == i)])
    }
    biome_area <- round(biome_area / sum(cellarea) * 100, 3)
    legend_text <- paste(legend_text, paste0("(", biome_area, " %)"))
  }
  legend(0,
    y = ypoint, xjust = 0.45, yjust = 1, cex = 0.8,
    legend_text, fill = biome_cols_legend[1:19],
    horiz = FALSE, border = NULL, bty = "o", box.col = "white",
    bg = bg_col, ncol = 4
  )
  if (!is.null(file_name)) dev.off()
}

# convert lpjml vector to raster and change projection to robinson
to_raster <- function(lpjml_array, boundary_box, ext, to_robinson) {
  crs_init <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  lpj_ras <- raster::raster(res = 0.5, crs = crs_init)
  # TODO replace lpjmliotools
  lpj_ras[raster::cellFromXY(lpj_ras, cbind(lon, lat))] <-
    lpjml_array
  if (to_robinson) {
    ras_to <- raster(
      xmn = -18000000,
      xmx = 18000000,
      ymn = -9000000,
      ymx = 9000000,
      crs = "+proj=robin",
      nrows = 2 * 360,
      ncols = 2 * 720
    )

    out_ras <- raster::crop(lpj_ras, ext) %>%
      raster::projectRaster(
        to = ras_to, crs = "+proj=robin",
        na.rm = TRUE, method = "ngb"
      ) %>%
      raster::mask(boundary_box) %>%
      suppressWarnings()
  } else {
    out_ras <- raster::crop(lpj_ras, ext)
  }
  return(out_ras)
}

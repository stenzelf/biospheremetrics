#' Plot global distribution of lpjml simulated biomes
#'
#' Plots a map with the biome distribution as derived from a lpjml run based
#' on the "classify_biomes" function
#'
#' @param biome_data output (list) from classify_biomes()
#'
#' @param file_name directory for saving the plot (character string)
#' @param display_area boolean, adding occupied area per biome (default FALSE)
#' @param to_robinson logical to define if robinson projection should be used
#' for plotting
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
                        cellarea = NULL,
                        order_legend = c(
                          1, 2, 9, 10, 11, 3, 4, 5, 12, 13, 14,
                          6, 7, 8, 15, 16, 17, 18, 19
                        ),
                        bg_col = "white") {
  # load required data: bbox, countries
  lpjml_extent <- terra::ext(-180, 180, -60, 85)
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

  biomes_lpjml <- terra::rast(ncols = 720, nrows = 360)
  range <- range(biome_data$biome_id)
  biomes_lpjml[
    terra::cellFromXY(biomes_lpjml, cbind(lon, lat))
  ] <- as.integer(biome_data$biome_id)

  if (!is.null(file_name)) {
    file_extension <- strsplit(file_name, split = "\\.")[[1]][-1]
    switch(file_extension,
      `png` = {
        grDevices::png(file_name,
          width = 8 * 1.8,
          height = 4 * 2,
          units = "cm",
          res = 600,
          pointsize = 7
        )
      },
      `pdf` = {
        grDevices::pdf(file_name,
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
  graphics::par(
    fig = c(0, 1, 0.15, 1),
    mar = c(4, 0, 0, 0),
    oma = c(0, 0, 0, 0),
    xpd = FALSE,
    bg = bg_col
  )
  terra::plot(
    biomes_lpjml,
    ext = lpjml_extent,
    asp = 1,
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = "",
    col = biome_cols,
    breaks = brk,
    lwd = 0.1,
    bty = "n",
    legend = FALSE
  )
  maps::map("world", add = TRUE, res = 0.4, lwd = 0.25, ylim = c(-60, 90))
  legend_text <- biome_names_legend[seq_len(19)]

  if (display_area) {
    if (is.null(cellarea)) stop("Cellarea needs to be supplied for displaying.")
    biome_area <- rep(0, length(order_legend))
    names(biome_area) <- biome_mapping$short_name

    for (i in seq_along(order_legend)) {
      biome_area[i] <- sum(cellarea[which(biome_data$biome_id == i)])
    }

    biome_area <- round(biome_area / sum(cellarea) * 100, 3)
    legend_text <- paste(legend_text, paste0("(", biome_area, " %)"))
  }
  graphics::par(
    fig = c(0, 1, 0, 0.15),
    mar = c(0, 0, 0, 0),
    oma = c(0, 0, 0, 0),
    xpd = TRUE, bg = bg_col
  )
  graphics::legend(
    x = 0,
    y = 100,
    xjust = 0.45,
    yjust = 1,
    cex = 0.78,
    legend_text,
    ncol = 4,
    bg = bg_col,
    border = NULL,
    fill = biome_cols_legend[seq_len(19)],
    bty = "o",
    box.col = "white"
  )
  if (!is.null(file_name)) grDevices::dev.off()
}

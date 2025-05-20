#' Calculate ROC curve data
#'
#' Calculate data for ROC curve comparison between binary external and
#'    continuous internal indicator.
#'
#' @param external_binary array with binary transgression value: 0 - no, 1 - yes
#' @param internal_continuous array with continuous values
#' @param external_name name of external indicator
#' @param cellArea array with area for each cell of internal indicator
#' @param range_internal range of internal data, e.g. c(0,1)
#' @param sampling_res sampling rate for internal indicator. default: 0.01
#'
#' @md
#' @export
calc_roc_data <- function(external_binary, #binary transgression: 0 - no,1 - yes
                          internal_continuous,
                          external_name,
                          cellArea,
                          range_internal, # range of internal data, e.g. c(0,1)
                          sampling_res = 0.01 # together with range used to create potential thresholds
                          ) {

  # check dimensions
  internal_continuous <- drop(internal_continuous)
  external_binary <- drop(external_binary)
  if (length(internal_continuous) != length(external_binary) ||
      length(internal_continuous) != length(cellArea)) {
    stop("Dimensions of external_binary (", length(external_binary),
         "), internal_continuous (", length(internal_continuous),
         "), cellArea (", length(cellArea),
         ") need to match. Aborting.")
  }

  # controls sampling distance
  potential_thresholds <- seq(range_internal[1],range_internal[2],sampling_res)

  values <- array(NA, dim=c(length(potential_thresholds),1,3))
  dimnames(values) <- list(threshold = potential_thresholds,
                           indicator = external_name, value = c("TP","FP","slope"))
  for (i in seq_along(potential_thresholds)) {
    t <- potential_thresholds[i]
    internal_cells <- which(internal_continuous > t)
    TP <- sum(cellArea[which(external_binary == 1 & internal_continuous > t)]) # True Positive
    FP <- sum(cellArea[which(!(external_binary == 1) & internal_continuous > t)]) # False Positive
    values[i,1,1] <- TP
    values[i,1,2] <- FP
  }
  #browser()
  #plotGlobalManToScreen(data = external_binary, title = "change",
  #                      brks = seq(0,1,0.05),legYes = T,palette = palette_viridis,
  #                      legendtitle = "")
  values[,1,1] <- values[,1,1]/max(values[,1,1])
  values[,1,2] <- values[,1,2]/max(values[,1,2])
  # calculate tangent angle between each pair of values
  n <- length(potential_thresholds)
  xs <- values[c(1,1:n,n),1,1]
  ys <- values[c(1,1:n,n),1,2]
  n_e <- length(xs)
  slope <- (ys[1:(n_e-2)] - ys[3:n_e]) / (xs[1:(n_e-2)] - xs[3:n_e])
  values[,1,3] <- slope

  return(values)
}
calc_roc_data_3d <- function( external_continuous,
                              internal_continuous,
                              cellArea,
                              range_internal, # range of internal data, e.g. c(0,1)
                              range_external, # range of external data, e.g. c(0,1)
                              sampling = 101 # together with range used to create potential thresholds
                              ) {
  # check dimensions
  internal_continuous <- drop(internal_continuous)
  external_continuous <- drop(external_continuous)
  if (length(internal_continuous) != length(external_continuous) ||
      length(internal_continuous) != length(cellArea)) {
    stop("Dimensions of external_binary (", length(external_continuous),
         "), internal_continuous (", length(internal_continuous),
         "), cellArea (", length(cellArea),
         ") need to match. Aborting.")
  }

  # controls sampling distance
  potential_thresholds_internal <- seq(range_internal[1],range_internal[2], length.out = sampling)
  potential_thresholds_external <- seq(range_external[1],range_external[2], length.out = sampling)

  values <- array(NA, dim=c(length(potential_thresholds_internal),length(potential_thresholds_external),3))
  dimnames(values) <- list(threshold_internal = potential_thresholds_internal,
                           threshold_external = potential_thresholds_external,
                           value = c("TP","FP","diff"))
  for (e in seq_along(potential_thresholds_internal)) {
    for (i in seq_along(potential_thresholds_internal)) {
      ti <- potential_thresholds_internal[i]
      te <- potential_thresholds_external[e]
      TP <- sum(cellArea[which(external_continuous > te & internal_continuous > ti)]) # True Positive
      FP <- sum(cellArea[which(external_continuous <= te & internal_continuous > ti)]) # False Positive
      values[i,e,1] <- TP
      values[i,e,2] <- FP
    }
  }

  #values[,,1] <- values[,,1]/max(values[,,1])
  #values[,,2] <- values[,,2]/max(values[,,2])
  values[,,3] <- values[,,1] - values[,,2]

  return(values)
}
#' Plot ROC curve
#'
#' Plot ROC curve(s)
#'
#' @param filename path to plot roc curve to. default: NULL -> plot to screen
#' @param values roc data object as obtained from calc_roc_data(), can be an array
#'               with dimensions = c(thresholds, internal_metric(s),
#'                    external_indicator(s), 3`["TP","FP","slope"]`)
#'
#' @md
#' @export
roc_plot <- function(filename = NULL,
                     values # only one metric
) {
  if (!is.null(filename)) {
    grDevices::png(filename, res = 300, width = 6, height = 7, units = "in",
        pointsize = 6, type = "cairo")
  }
  graphics::par(oma = c(0,0,0,0), mar = c(6,10,5,0), bty = "o", xpd = F,xaxs="i",yaxs="i")
  indizes <- dimnames(values)$indicator
  auc <- array(NA,dim = c(length(indizes)))
  dimnames(auc) <- list(indicator = indizes)
  best_fit <- array(NA,dim=c(length(indizes),2))
  dimnames(best_fit) <- list(indicator = indizes, type = c("max_diff","slope"))

  # Set plot layout
  #graphics::layout(mat = matrix(c(1,2,3), nrow = 1,  ncol = 3, byrow = FALSE),
  #       heights = c(2,2),    # Heights of the rows
  #       widths = c(1,0.4,0.4))     # Widths of the columns
  #graphics::layout.show(3)
  cex_perc = 2.5
  colz <- RColorBrewer::brewer.pal(12,"Set3")[-c(2)]

  plot(NA, type = "n", xlab = "FP", ylab = "TP", main = "ROC curve", cex.main = cex_perc,
       xlim = c(0,1), ylim = c(0,1), cex.lab = cex_perc, cex.axis = cex_perc, mgp = c(3, 2, 0))
  graphics::abline(a = 0, b = 1, lty = "dashed", col = "black")
  for (ind in indizes) {
    indx <- match(ind,indizes)
    graphics::lines(x = values[,ind,2], y = values[,ind,1], col = colz[indx], lwd = cex_perc)
    values[is.na(values[,ind,3]),ind,3] <- 0
    best_max_diff <- sort(abs(values[,ind,2] - values[,ind,1]),decreasing = T)[1:5]
    best_slope <- sort(abs(values[,ind,3] - 1 ),decreasing = F)[1:5]
    closest <- function(x,y){abs((x - y)/sqrt(2))}
    best_max_diff_thr <- names(sort(closest(x = values[names(best_max_diff),ind,2],
                                            y = values[names(best_max_diff),ind,1]), decreasing = T)[1])
    best_slope_thr <- names(sort(closest(x = values[names(best_slope),ind,2],
                                         y = values[names(best_slope),ind,1]), decreasing = T)[1])
    best_fit[ind,] <- c(best_max_diff_thr, best_slope_thr)
    graphics::points(x = values[best_max_diff_thr,ind,2], y = values[best_max_diff_thr,ind,1],
           col = colz[indx], cex = 4, lwd = cex_perc)
    # AUC
    n <- length(values[,ind,2])
    auc[indx] <- sum(abs(values[2:n,ind,2] - values[1:(n - 1),ind,2]) *
                       values[1:(n - 1),ind,1])
  }
  graphics::legend("bottomright",legend = paste(indizes,round(auc,2)), col = colz,
         cex = cex_perc, lwd = cex_perc)
  if (!is.null(filename)) grDevices::dev.off()
  return(list(
    auc = auc,
    optimal_value = best_fit
  ))
}

#' Plot ROC curve
#'
#' Plot ROC curve(s) - paper version
#'
#' @param filename path to plot roc curve to. default: NULL -> plot to screen
#' @param values roc data object as obtained from calc_roc_data(), can be an array
#'               with dimensions = c(thresholds, internal_metric(s),
#'                    external_indicator(s), 3`["TP","FP","slope"]`)
#'
#' @md
#' @export
roc_plot_paper <- function(filename = NULL,
                     values # both metric
) {
  if (!is.null(filename)) {
    grDevices::png(filename, res = 300, width = 6, height = 7, units = "in",
        pointsize = 6, type = "cairo")
  }
  graphics::par(oma = c(0,0,0,0), mar = c(6,10,5,0), bty = "o", xpd = F,xaxs="i",yaxs="i")
  vars <- dimnames(values)$metric
  indicators <- dimnames(values)$indicator
  indizes <- seq_along(indicators)

  best_fit <- array(NA,dim=c(length(indicators),2,2))
  dimnames(best_fit) <- list(indicator = indicators,
                  metric = vars, type = c("max_diff","slope"))

  auc <- array(NA,dim = c(length(indicators),length(vars)))
  dimnames(auc) <- list(indicator = indicators, metric = vars)

  # Set plot layout
  graphics::layout(mat = matrix(c(1,2,3,4,5,6), nrow = 2,  ncol = 3, byrow = FALSE),
         heights = c(2,2),    # Heights of the rows
         widths = c(1,0.4,0.4))     # Widths of the columns
  graphics::layout.show(6)
  cex_perc = 2.5
  colz <- RColorBrewer::brewer.pal(12,"Set3")[-c(2)]

  for (var in vars){
    plot(NA, type = "n", xlab = "FP", ylab = "TP", main = "ROC curve", cex.main = cex_perc,
         xlim = c(0,1), ylim = c(0,1), cex.lab = cex_perc, cex.axis = cex_perc, mgp = c(3, 2, 0))
    if (var == "BioCol") {
      graphics::mtext(side = 3, at = -0.2, "A", xpd = NA, cex = 1.5, font = 2)
      graphics::mtext(side = 2, at = 0.5, line = 6, expression("HANPP"^Hol), xpd = NA, cex = 2, font = 2, srt = 90)
    }else{ #var = "EcoRisk"
      graphics::mtext(side = 3, at = -0.2, "B", xpd = NA, cex = 1.5, font = 2)
      graphics::mtext(side = 2, at = 0.5, line = 6, "EcoRisk", xpd = NA, cex = 2, font = 2, srt = 90)
    }
    graphics::abline(a = 0, b = 1, lty = "dashed", col = "black")
    for (ind in indizes) {
      graphics::lines(x = values[,var,ind,2], y = values[,var,ind,1], col = colz[ind], lwd = cex_perc)
      #graphics::points(x = values[,ind,2], y = values[,ind,1])
      values[is.na(values[,var,ind,3]),var,ind,3] <- 0
      best_max_diff <- sort(abs(values[,var,ind,2] - values[,var,ind,1]),decreasing = T)[1:5]
      best_slope <- sort(abs(values[,var,ind,3] - 1 ),decreasing = F)[1:5]
      closest <- function(x,y){abs((x - y)/sqrt(2))}
      best_max_diff_thr <- names(sort(closest(x = values[names(best_max_diff),var,ind,2],
                                              y = values[names(best_max_diff),var,ind,1]), decreasing = T)[1])
      best_slope_thr <- names(sort(closest(x = values[names(best_slope),var,ind,2],
                                           y = values[names(best_slope),var,ind,1]), decreasing = T)[1])
      best_fit[ind,match(var,vars),] <- c(best_max_diff_thr, best_slope_thr)
      graphics::points(x = values[best_max_diff_thr,var,ind,2], y = values[best_max_diff_thr,var,ind,1],
             col = colz[ind], cex = 4, lwd = cex_perc)

      # AUC
      n <- length(values[,var,ind,2])
      auc[ind,var] <- sum(abs(values[2:n,var,ind,2] - values[1:(n - 1),var,ind,2]) *
                            values[1:(n - 1),var,ind,1])
      graphics::legend("bottomright",legend = indicators, col = colz[indizes],
             cex = cex_perc, lwd = cex_perc)
    }
  }

  # optimal thresholds boxplots
  biocol_threshold <- graphics::boxplot(as.numeric(best_fit[indizes,"BioCol",1]), ylim = c(0,1), cex.main = cex_perc,
                              main = "optimal\nthreshold", cex.lab = cex_perc, cex.axis = cex_perc)
  graphics::points(x = rep(0.7, length(indizes)), y = as.numeric(best_fit[indizes,"BioCol",1]),
         col = scales::alpha(colz[indizes],1), lwd = 2)
  graphics::text(x = 1, y = max(biocol_threshold$stats)+0.2, paste0("median:\n",stats::median(biocol_threshold$stats)), cex = cex_perc)
  graphics::mtext(side = 3, at = 0, "C", xpd = NA, cex = 1.5, font = 2)

  ecorisk_threshold <- graphics::boxplot(as.numeric(best_fit[indizes,"EcoRisk",1]),
                               main = "optimal\nthreshold", cex.main = cex_perc,
                               ylim = c(0,1), cex.lab = cex_perc, cex.axis = cex_perc)
  graphics::points(x = rep(0.7, length(indizes)), y = as.numeric(best_fit[indizes,"EcoRisk",1]),
         col = scales::alpha(colz[indizes],1), lwd = 2)
  graphics::text(x = 1, y = max(ecorisk_threshold$stats)+0.2, paste0("median:\n", stats::median(ecorisk_threshold$stats)), cex = cex_perc)
  graphics::mtext(side = 3, at = 0, "D", xpd = NA, cex = 1.5, font = 2)

  # AUC boxplots
  biocol_auc <- graphics::boxplot(auc[indizes,"BioCol"], ylim = c(0,1), main = "AUC", cex.main = cex_perc,
                        cex.lab = cex_perc, cex.axis = cex_perc)
  graphics::points(x = rep(0.7, length(indizes)), y = auc[indizes,"BioCol"],
         col = scales::alpha(colz[indizes],1), lwd = 2)
  graphics::mtext(side = 3, at = 0, "E", xpd = NA, cex = 1.5, font = 2)

  ecorisk_auc <- graphics::boxplot(auc[indizes,"EcoRisk"], main = "AUC", cex.main = cex_perc,
                         ylim = c(0,1), cex.lab = cex_perc, cex.axis = cex_perc)
  graphics::points(x = rep(0.7, length(indizes)), y = auc[indizes,"EcoRisk"],
         col = scales::alpha(colz[indizes],1), lwd = 2)
  graphics::mtext(side = 3, at = 0, "F", xpd = NA, cex = 1.5, font = 2)

  if (!is.null(filename)) grDevices::dev.off()
  return(list(
    auc = auc,
    biocol_threshold = as.numeric(best_fit[indizes,"BioCol",1]),
    ecorisk_threshold = as.numeric(best_fit[indizes,"EcoRisk",1])
  ))
}

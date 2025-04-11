source(here::here("Scripts/Current-Scripts/datacleaningandsubsetting.R"))

# modify plotmag function to remove axes
plotmag.tts <- function(object, zlims = NULL, neat = TRUE,
                        colorfill = NULL, colorbar = TRUE,
                        title = NULL, filename = NA,
                        axes = FALSE, ...) {
  
  wav <- Mod(get_values(object))
  times <- get_times(object)
  timescales <- get_timescales(object)
  
  if (is.null(zlims)) {
    zlims <- range(wav, na.rm = TRUE)
  } else {
    rg <- range(wav, na.rm = TRUE)
    if (rg[1] < zlims[1] || rg[2] > zlims[2]) {
      stop("Error in plotmag.tts: zlims must encompass the z axis range of what is being plotted")
    }
  }
  
  if (neat) {
    inds <- which(!is.na(colMeans(wav, na.rm = TRUE)))
    wav <- wav[, inds]
    timescales <- timescales[inds]
  }
  
  if (is.null(colorfill)) {
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill <- grDevices::colorRampPalette(jetcolors)
  }
  
  ylocs <- pretty(timescales, n = 8)
  xlocs <- pretty(times, n = 8)
  
  if (!is.na(filename)) {
    grDevices::pdf(paste0(filename, ".pdf"))
  }
  
  # plot image without axes
  if (!colorbar) {
    graphics::image(x = times, y = log2(timescales), z = wav, xlab = "Time",
                    zlim = zlims, ylab = "Timescale", axes = FALSE,
                    col = colorfill(100), main = title, ...)
  } else {
    fields::image.plot(x = times, y = log2(timescales), z = wav, xlab = "Time",
                       zlim = zlims, ylab = "Timescale", axes = FALSE,
                       col = colorfill(100), main = title, ...)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
}


# calculate signficance thresholds 
temp_xx <- psync.by.chance(n)
upper<- temp_xx[2]
lower <- temp_xx[1]

# get wavelet data
wav <- Mod(get_values(res_growth_wpmf))
times <- get_times(res_growth_wpmf)
timescales <- get_timescales(res_growth_wpmf)

# plot wmf growth with adjusted axes
plotmag.tts(res_growth_wmf)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

# add contour lines (upper and lower) from wpmf
graphics::contour(x = times, y = log2(timescales), z = wav, levels = upper, 
                  drawlabels = FALSE, lwd = 2, xaxs = "i", xaxt = "n", 
                  xaxp = c(0, 1, 5), las = 1, frame = FALSE, lty = 1, 
                  yaxt = "n", add = TRUE)

# plot ppt wmf with adjusted axes
global_range <- range(c(wav1, wav2), na.rm = TRUE)
plotmag.tts(res_ppt_wmf, zlims = global_range)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)

# plot tmin wmf with adjusted axes
plotmag.tts(res_tmin_wmf, zlims = global_range)
axis(1, at = seq(1900, 2020, by = 20), labels = seq(1900, 2020, by = 20))
axis(2, at = log2(c(3, 10, 20, 30)), labels = c("3", "10", "20", "30"), las = 1)


#' Title
#'
#' @param df \code{data.frame} containing coordinates and variable values at those points.
#' @param variable single \code{character} indicating the name of the column in \code{df} to calculate variogram.
#' @param log_transform \code{logical}
#' @param min_dist 
#' @param boundaries 
#' @param nsimu 
#' @param crs 
#' @param probs 
#' @param verbose 
#'
#' @returns
#' @export
#'
#' @examples
#' library(sp)
#' data(meuse)
#' 
#' # First we plot a variogram with all points.
#' cutoff <- 4000
#' boundaries <- seq(0, 4000, by = 200)
#' meu <- sf::st_as_sf(meuse, coords = c("x", "y"), crs = NA)
#' vmeuse <- gstat::variogram(log(zinc) ~ 1, data = meu, boundaries = boundaries, cutoff = cutoff)
#' plot(vmeuse, type="l", lwd=2,xlim=c(0, 4000), ylim=c(0, 1.5))
#' 
#' # We now simulate 1000 filtered meuse data sets. Minimum distance will be 500.
#' nsimu <- 1000
#' min_dist <- 500
#' boundaries <- seq(min_dist, cutoff, by = 250)
#' r <- variogram_simu(meuse, variable = "zinc", log_transform = TRUE, min_dist = min_dist, boundaries = boundaries, nsimu = nsimu)
#' plot(r$mean_dist, r$variogram, type="l", lwd=2,xlim=c(0, 4000), ylim=c(0, 1.5), xlab = "Distance")
#' points(r$mean_dist, r$`median filtered variogram`, type = "l", lwd=2, lty = 1, col = "red")
#' points(r$mean_dist, r$`mean filtered variogram`, type = "l", lwd=2, lty = 2, col = "blue")
#' points(r$mean_dist, r$quantile_0.025, type = "l", lwd=2, lty = 3)
#' points(r$mean_dist, r$quantile_0.975, type = "l", lwd=2, lty = 3)
variogram_simu <- function(df, variable = variable, log_transform = TRUE, min_dist= min_dist,
                           boundaries = boundaries, nsimu = 100, crs = NA, probs = c(0.025, 0.975), verbose = TRUE) {
  
  # Checks.
  if (!is.data.frame(df)) cli::cli_abort("Input 'df' must be a data.frame")
  if (!all(c("x", "y") %in% colnames(df))) cli::cli_abort("Columns 'x' and 'y' in 'df' are missing")
  if (min_dist < 0) cli::cli_abort("'min_dist' must be positive")
  if (any(boundaries < 0)) cli::cli_abort("Intervals limits in 'boundaries' must be all positive")
  if (any(diff(boundaries) < 0)) cli::cli_abort("Interval limits in 'boundaries' must increasing monotonically")
  
  
  # data.frame to set lower and upper interval limits inside loop below.
  ref_bin <- data.frame(
    bin = 1:(length(boundaries) - 1),
    lower  = boundaries[-length(boundaries)],
    upper  = boundaries[-1]
  )
  
  
  # Formula for variogram.
  if (log_transform) variable <- paste0("log(", variable, ")")
  fo <- as.formula(paste0(variable, " ~ 1"))
  
  
  # First variogram with all data points.
  df_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = crs)
  v <- gstat::variogram(fo, data = df_sf, boundaries = boundaries)
  v$bin <- findInterval(v$dist, boundaries)
  v <- merge(ref_bin, v, by = "bin", all.x = TRUE)

  
  # Simulation.
  idx <- if (verbose) cli::cli_progress_along(1:nsimu, name = "variogram_simu: ") else 1:nsimu
  simu_vario <- sapply(idx, function(i) {
    
    # Random spatial filter.
    df_filtered <- distance_filter(df, min_dist = min_dist, columns = c("x", "y"), verbose = FALSE)
    
    # Convert to "sf" object for use below.
    df_filtered_sf <- sf::st_as_sf(df_filtered, coords = c("x", "y"), crs = crs)

    # Variogram
    va <- gstat::variogram(fo, data = df_filtered_sf, boundaries = boundaries)

    # Create intervals and return variogram.
    va$bin <- findInterval(va$dist, boundaries)
    va <- merge(ref_bin, va, by = "bin", all.x = TRUE)
    va$gamma
  })

  
  # Build data.frame with mean and quantile values.
  out <- data.frame((boundaries[-1] + boundaries[-length(boundaries)])/2, v$gamma,
                    t(apply(simu_vario, 1, function(x) c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE), quantile(x, probs = probs, na.rm = TRUE)))))
  colnames(out) <- c("mean_dist", "variogram", "mean filtered variogram", "median filtered variogram", paste0("quantile_", probs)) 
  
  
  return(out)

}

#' Simulations of spatially-filtered variograms
#' 
#' @description
#' \code{variogram_simu} spatially-filters (with \code{distance_filter} and then computes variograms
#' (with \code{variogram} from the 'gstat' package) for a number of simulations.
#' 
#'
#' @param df \code{data.frame} containing coordinates and variable values at those points.
#' @param variable single \code{character} indicating the name of the column in \code{data.frame} 'df' to calculate its variogram.
#' @param log_transform \code{logical} variable indicating whether 'variable' should be log-transformed.
#' @param min_dist \code{numeric} positive value specifying the minimum cutoff distance.
#' @param boundaries \code{numeric} vector with the limits of the distance intervals to calculate variogram.
#' @param nsimu \code{numeric} number of stochastic simulations (i.e. calls to 'distance_filter') to perform.
#' @param crs coordinate reference system; see help for \code{sf::st_as_sf}.
#' @param probs \code{numeric} vector of probabilities. See help for \code{quantile}.
#' @param verbose \code{logical} flag. If set to TRUE a progress bar will be shown on screen.
#'
#' @returns
#' A \code{data.frame} containing the average variogram at several distance intervals, as specified with
#' input parameter 'boundaries'. See example below.
#' 
#' @export
#' 
#' @details
#' Function 'distance_filter' is used to calculate 'nsimu' variogram values inside distance intervals.
#' Next, mean and quantiles are computed within each of those intervals.
#' 
#'
#' @examples
#' 
#' # install.packages(c("sf", "gstat", "sp))
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
#' # We now simulate 1000 filtered meuse data sets. Minimum distance will be 500. Notice that the "confidence
#' # band" will vary depending on the width of intervals.
#' nsimu <- 1000
#' min_dist <- 500
#' interval_width <- 500
#' boundaries <- seq(min_dist, cutoff, by = interval_width)
#' r <- variogram_simu(meuse, variable = "zinc", log_transform = TRUE, min_dist = min_dist, boundaries = boundaries, nsimu = nsimu)
#' plot(r$mean_dist, r$variogram, type="l", lwd=2,xlim=c(0, 4000), ylim=c(0, 1.5), xlab = "Distance")
#' points(r$mean_dist, r$quantile_0.5, type = "l", lwd=2, lty = 2, col = "red")
#' points(r$mean_dist, r$quantile_0.025, type = "l", lwd=2, lty = 3, col = "red")
#' points(r$mean_dist, r$quantile_0.975, type = "l", lwd=2, lty = 3, col = "red")
variogram_simu <- function(df, variable = variable, log_transform = TRUE, min_dist= min_dist,
                           boundaries = boundaries, nsimu = 100, crs = NA, probs = c(0.025, 0.5, 0.975), verbose = TRUE) {
  
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
                    t(apply(simu_vario, 1, function(x) c(mean(x, na.rm = TRUE), quantile(x, probs = probs, na.rm = TRUE)))))
  colnames(out) <- c("mean_dist", "variogram", "mean filtered variogram", paste0("quantile_", probs)) 
  
  
  return(out)

}

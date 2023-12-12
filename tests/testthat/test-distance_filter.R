test_that("Distance filter", {
  
  # Random set of 5-D coordinates.
  n <- 5
  df <- setNames(do.call("cbind", replicate(5, data.frame(runif(100)*10), simplify = F)), paste0("x", 1:n))

  # Test filtered dataset.
  min_dist <- 5
  df_filt <- distance_filter(df, min_dist, shuffle = F)
  expect_lte(nrow(df_filt), nrow(df))
  expect_lte(min_dist, min(dist(df_filt)))
  
  
  # Test shuffled+filtered dataset.
  df_shuf_filt <- distance_filter(df, min_dist, shuffle = T)
  expect_lte(nrow(df_shuf_filt), nrow(df))
  expect_lte(min_dist, min(dist(df_shuf_filt)))
  
  
  # Distance, in all cases (or, at least, a sample) is always > min_dist.
  nsimu <- 100
  expect_true(all(replicate(nsimu, min(dist(distance_filter(df, min_dist, shuffle = T)))) > min_dist))
  
  
  # When all distances are smaller than min_dist, output is NA.
  min_dist <- 1000
  z <- distance_filter(df, min_dist, shuffle = T)
  expect_true((is.na(z)))
  
  #########################################################
  # Same, but for some columns only.
  # Test filtered dataset.
  min_dist <- 5
  df_filt <- distance_filter(df, min_dist, columns = c("x4", "x2"), shuffle = F)
  expect_lte(nrow(df_filt), nrow(df))
  expect_lte(min_dist, min(dist(df_filt)))
  
  
  # Test shuffled+filtered dataset.
  df_shuf_filt <- distance_filter(df, min_dist, columns = c("x4", "x2"), shuffle = T)
  expect_lte(nrow(df_shuf_filt), nrow(df))
  expect_lte(min_dist, min(dist(df_shuf_filt)))
  
  
  # Distance, in all cases (or, at least, a sample) is always > min_dist.
  nsimu <- 100
  expect_true(all(replicate(nsimu, min(dist(distance_filter(df, min_dist, columns = c("x4", "x2"), shuffle = T)))) > min_dist))
  
  
  # When all distances are smaller than min_dist, output is NA.
  min_dist <- 1000
  z <- distance_filter(df, min_dist, columns = c("x4", "x2"), shuffle = T)
  expect_true((is.na(z)))
  
  
  # Columns can't be found.
  expect_error(distance_filter(df, min_dist, columns = c("AAA", "BBB"), shuffle = F))
  
  
  # Input is a 2D matrix.
  df <- array(runif(4*5*6), dim = c(4, 5, 6))
  expect_error(distance_filter(df, min_dist))
  
  
})

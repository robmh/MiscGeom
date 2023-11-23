test_that("Filtering by minimum distance", {
  
  # Random data.frame with preset values.
  xy <- data.frame(x = runif(10), y = runif(10))
  xy[1, ] <- c(1, 1)
  xy[2, ] <- c(10, 10)
  xy[9, ] <- c(22, 23)
  min_dist <- 10
  
  # In this case, the answer should be a data.frame containing xy[c(1, 2, 9), ]
  # in this order.
  z <- distance_filter(xy, min_dist, shuffle = F)
  
  expect_identical(xy[c(1, 2, 9), ], z)
  dxy <- as.matrix(dist(z, diag = T))
  diag(dxy) <- 1E64
  expect_true(all(dxy > min_dist))
  
  # In this other case, the answer may not be xy[c(1, 2, 9), ] exactly.
  # So we only check the distances.
  z <- distance_filter(xy, min_dist, shuffle = T)
  dxy <- as.matrix(dist(z, diag = T))
  diag(dxy) <- 1E64
  expect_true(all(dxy > min_dist))
  
  
})
test_that("Intersection of segments", {


  ## An example of intersection.
  s1 <- matrix(c(0, 1, 0, 1), 2, 2)
  s2 <- matrix(c(0, 1, 1, 0), 2, 2)
  expect_equal(segments_intersection(s1, s2), c(x = .5, y = .5))
  

  ## An example of segments that do not intersect.
  expect_false(segments_intersection(s1 + 3, s2))

  # An example of parallel lines.
  s1 <- matrix(c(1, 2, 1, 2), 2, 2)
  s2 <- matrix(c(0, 3, 1, 4), 2, 2)
  expect_warning(segments_intersection(s1, s2))
  expect_true(is.na(segments_intersection(s1, s2, verbose = F)))
  
  
})

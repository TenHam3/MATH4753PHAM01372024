test_that("mu returned", {
  expect_equal(myncurve(5,3,2)$mu, 5)
})

test_that("sigma returned", {
  expect_equal(myncurve(5,3,2)$sigma, 3)
})

test_that("area returned", {
  expect_equal(round(myncurve(5,3,2)$area,4), 0.1587)
})

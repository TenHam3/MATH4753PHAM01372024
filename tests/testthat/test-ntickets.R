test_that("nc returned", {
  expect_equal(ntickets(400,0.02,0.95)$nc, 412.0152)
})

test_that("nd returned", {
  expect_equal(ntickets(400,0.02,0.95)$nd, 412)
})

test_that("N returned", {
  expect_equal(ntickets(400,0.02,0.95)$N, 400)
})

test_that("gamma returned", {
  expect_equal(ntickets(400,0.02,0.95)$gamma, 0.02)
})

test_that("p returned", {
  expect_equal(ntickets(400,0.02,0.95)$p, 0.95)
})

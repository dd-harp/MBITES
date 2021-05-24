test_that("build_larvae does nothing with nothing", {
  l <- MBITES:::build_larvae(0.9, 3)

  for (i in 1:100) {
    initial_zero <- l$oneDay()
    expect_equal(initial_zero, 0)
  }
})


test_that("build_larvae lasts three days", {
  l <- MBITES:::build_larvae(0.9, 3)
  l$push(10001)
  expect_equal(l$count(), 10001)
  d1 <- l$oneDay()
  expect_equal(d1, 0L)
  d2 <- l$oneDay()
  expect_equal(d2, 0L)
  d3 <- l$oneDay()
  expect_gt(d3, 0L)
  d4 <- l$oneDay()
  expect_equal(d4, 0L)
})


test_that("build_larvae returns what you give it", {
  l <- MBITES:::build_larvae(1.0, 3)
  cnt <- 1000
  eggs <- runif(cnt + 10, 10, 300)
  total_in <- 0
  total_out <- 0
  for (i in 1:(cnt + 5)) {
    l$push(eggs[i])
    total_in <- total_in + eggs[i]
    r <- l$oneDay()
    total_out <- total_out + r
  }
  expect_true(total_out > 0.5 * total_in && total_out < 1.5 * total_in)
})

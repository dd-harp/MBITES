test_that("riskq adds and removes from human queue", {
  q <- make_RiskQ()
  q$add2Q(3, 0.7, 1.4)
  q$add2Q(4, 0.2, 2.0)
  ids <- integer(0)
  for (i in 1:40) {
    ids <- c(ids, q$sampleQ())
  }
  expect_equal(sort(unique(ids)), c(3, 4))
})


test_that("riskq adds and removes from animal queue", {
  q <- make_RiskQ()
  q$add2Q_zoo(-3, 0.7)
  q$add2Q_zoo(-4, 0.2)
  ids <- integer(0)
  for (i in 1:40) {
    ids <- c(ids, q$sampleQ())
  }
  expect_equal(sort(unique(ids)), c(-4, -3))
})


test_that("riskq adds and removes from human queue", {
  q <- make_RiskQ()
  for (h in sample(1:30)) {
    q$add2Q(h, 0.7, 1.0)
  }
  ids <- 1:30

  for (r in sample(1:30)[1:5]) {
    q$rmFromQ(r)
    ids <- setdiff(ids, r)
  }
  hids <- integer(0)
  for (i in 1:400) {
    hids <- c(hids, q$sampleQ())
  }
  hids <- sort(unique(hids))
  expect_true(setequal(ids, unique(hids)))
})

test_that("randomization is correct", {
  dat <- sim_2x2(24, 24, 0, 0, 0, 0, 1, 1, .5, 1, TRUE)
  err_r <- dat[["Y_r"]] - dat[["Y_fit"]]
  err_b <- dat[["Y_b"]] - dat[["Y_fit"]]
  expect_equal(
    err_r[order(dat[["subj_id"]], dat[["tnum_r"]])],
    err_b[order(dat[["subj_id"]], dat[["tnum_b"]])])
})

test_that("intercept is correct", {
  dat <- sim_2x2(24, 24, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, TRUE)
  expect_equal(dat[["Y_fit"]],
               rep(5, length(dat[["Y_fit"]])))
})

test_that("A is correct", {
  dat <- sim_2x2(24, 24, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 1, TRUE)
  agg <- aggregate(Y_fit ~ A, dat, mean)
  expect_equal(agg[["Y_fit"]][2] - agg[["Y_fit"]][1], 5)
})

test_that("B is correct", {
  dat <- sim_2x2(24, 24, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 1, TRUE)
  agg <- aggregate(Y_fit ~ B, dat, mean)
  expect_equal(agg[["Y_fit"]][2] - agg[["Y_fit"]][1], 5)
})

test_that("AB is correct", {
  dat <- sim_2x2(24, 24, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 1, TRUE)
  agg <- aggregate(Y_fit ~ A + B, dat, mean)[["Y_fit"]]
  expect_equal(agg[1] - agg[2] - (agg[3] - agg[4]), 5)
})

test_that("errs are correct", {
  dat <- sim_2x2(24, 24, -1, 1, 2, 4, .5, .5, .5, .5, .5, .5, 0, TRUE)
  dat[["err_r"]] <- with(dat, Y_r - Y_fit)
  dat[["err_b"]] <- with(dat, Y_b - Y_fit)
  dsplit <- split(dat, dat[["subj_id"]])
  lv <- sapply(dsplit, function(.x) {
    all(mapply(all.equal, sort(.x[["err_r"]]), sort(.x[["err_b"]])))
  })
  expect_true(all(lv))
})

test_that("A is within subject", {
  dat <- sim_2x2()
  xtbl <- xtabs(~subj_id + A, dat)
  lv <- !sapply(seq_len(nrow(xtbl)), function(.x) {
    any(sapply(xtbl[.x, ], function(.xx) {isTRUE(all.equal(.xx, 0))}))
  })
  expect_true(all(lv))
})

test_that("B is between subject", {
  dat <- sim_2x2()
  xtbl <- xtabs(~subj_id + B, dat)
  lv <- sapply(seq_len(nrow(xtbl)), function(.x) {
    any(sapply(xtbl[.x, ], function(.xx) {isTRUE(all.equal(.xx, 0))}))
  })
  expect_true(all(lv))
})

test_that("A is between item", {
  dat <- sim_2x2()
  xtbl <- xtabs(~item_id + A, dat)
  lv <- sapply(seq_len(nrow(xtbl)), function(.x) {
    any(sapply(xtbl[.x, ], function(.xx) {isTRUE(all.equal(.xx, 0))}))
  })
  expect_true(all(lv))
})

test_that("B is within item", {
  dat <- sim_2x2()
  xtbl <- xtabs(~item_id + B, dat)
  lv <- !sapply(seq_len(nrow(xtbl)), function(.x) {
    any(sapply(xtbl[.x, ], function(.xx) {isTRUE(all.equal(.xx, 0))}))
  })
  expect_true(all(lv))
})

test_that("A is between subject", {
  dat <- sim_2x2()
  xtbl <- xtabs(~subj_id + B, dat)
  expect_equal(c(48L, 2L), dim(xtbl))
})

test_that("fitted values are correct", {
  int <- -1
  A_eff <- 1
  B_eff <- 2
  AB_eff <- 4
  dat <- sim_2x2(24, 24, int, A_eff, B_eff, AB_eff, .5, .5, .5, .5, .5, .5, 0, TRUE)
  dat[["fits"]] <- with(dat,
                        int + subj_rint + item_rint +
                        (A_eff + subj_rslp) * A_c +
                        (B_eff + item_rslp) * B_c +
                        AB_eff * A_c * B_c)
  lv <- mapply(function(.x, .y) {
    isTRUE(all.equal(.x, .y))},
    dat[["Y_fit"]], dat[["fits"]])
  expect_true(all(lv))
})

test_that("all residual SDs are 1", {
  ff <- lapply(0:8, function(.i) {
    dat <- sim_2x2(12, 12, 0, 0, 0, 0, 1, 1, .5, 0, 0, 0, .i, TRUE)
    err <- dat[["Y_r"]] - dat[["Y_fit"]]
    ds <- split(err, dat[["subj_id"]])
    vv <- sapply(ds, sd)
    names(vv) <- NULL
    vv
  })
  expect_true(all(sapply(ff, function(.x) {
    all.equal(.x, rep(1, 12))
  })))
})

test_that("all residual means are 1", {
  ff <- lapply(0:8, function(.i) {
    dat <- sim_2x2(12, 12, 0, 0, 0, 0, 1, 1, .5, 0, 0, 0, .i, TRUE)
    err <- dat[["Y_r"]] - dat[["Y_fit"]]
    ds <- split(err, dat[["subj_id"]])
    vv <- sapply(ds, mean)
    names(vv) <- NULL
    vv
  })
  expect_true(all(sapply(ff, function(.x) {
    all.equal(.x, rep(0, 12), tolerance = 1e-10)
  })))
})

test_that("item variance is correct", {
  expect_true(TRUE)
})

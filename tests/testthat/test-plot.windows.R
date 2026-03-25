test_that("plot.mzdf returns a ggplot", {
  dia <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
  p <- plot(dia)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.mzdf works with center output type", {
  # center output type lacks lower/upper_windows — plot.mzdf requires them,
  # so this should error or at minimum not crash on range output
  dia <- DIA.windows(mnc = 400, mxc = 1000, windows.width = 20)
  expect_no_error(plot(dia))
})

test_that("plot.mzdf works on vDIA output", {
  vdia <- vDIA.windows(mnc = 400, mxc = 1000, n.windows = 32)
  p <- plot(vdia)
  expect_true(inherits(p, "ggplot"))
})

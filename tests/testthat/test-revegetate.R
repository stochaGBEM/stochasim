test_that("Revegetation works", {
  cs <- cross_section(11, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  revegetate(cs, width0 = 1) |>
    ch_width() |>
    expect_equal(10)
  revegetate(cs, width0 = 1, rate = 1) |>
    ch_width() |>
    expect_equal(1)
  revegetate(cs, width0 = 0, rate = 1) |>
    ch_width() |>
    expect_equal(0)
  revegetate(cs, width0 = 1, rate = 0) |>
    ch_width() |>
    expect_equal(11)
  expect_lt(
    cs |>
      revegetate(width0 = 1) |>
      revegetate(width0 = 1) |>
      ch_width(),
    cs |>
      revegetate(width0 = 1) |>
      ch_width()
  )
})

test_that("Revegetation handles unanticipated inputs.", {
  cs <- cross_section(11, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  expect_error(revegetate(cs, width0 = -1, rate = 0.1))
  expect_error(revegetate(cs, width0 = 1, rate = -0.1))
  expect_error(revegetate(cs, width0 = 1, rate = 1.1))
  na <- ch_width(revegetate(cs, NA))
  expect_true(is.na(na))
  na2 <- ch_width(revegetate(cs, width0 = 1, rate = NA))
  expect_true(is.na(na2))
})

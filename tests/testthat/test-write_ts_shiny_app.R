make_test_dt <- function() {
  data.table::data.table(
    country = rep(c("A", "B"), each = 4L),
    region = rep(c("North", "South"), each = 4L),
    time_period = rep(c("2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4"), 2L),
    value = seq_len(8L),
    rate = seq(1.5, 5, length.out = 8L)
  )
}

test_that("generator writes a self-contained app pair", {
  dt <- make_test_dt()
  out <- tempfile("tsShinyApp-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  info <- write_ts_shiny_app(dt, out_dir = out)

  expect_true(file.exists(file.path(out, "app.R")))
  expect_true(file.exists(file.path(out, "data.rds")))
  expect_identical(readRDS(file.path(out, "data.rds")), dt)
  expect_identical(info$time_column, "time_period")
  expect_identical(info$numeric_columns, c("value", "rate"))
  expect_identical(info$categorical_columns, c("country", "region"))
  expect_false(info$cascading)
})

test_that("generated app has the intended interaction defaults", {
  dt <- make_test_dt()
  out <- tempfile("tsShinyApp-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  write_ts_shiny_app(dt, out_dir = out, cascading = TRUE)
  app <- paste(readLines(file.path(out, "app.R"), warn = FALSE), collapse = "\n")

  expect_match(app, "CASCADING <- TRUE", fixed = TRUE)
  expect_match(app, 'checkboxInput("markers", "Show point markers", value = FALSE)', fixed = TRUE)
  expect_match(app, "scrollZoom = FALSE", fixed = TRUE)
  expect_match(app, '"hoverClosestCartesian", "hoverCompareCartesian"', fixed = TRUE)
  expect_match(app, ".metric-card .modebar-container", fixed = TRUE)
  expect_match(app, "filtered_rows <- reactive", fixed = TRUE)
  expect_match(app, "plot_rows <- reactive", fixed = TRUE)
})

test_that("duplicate series-time keys are rejected", {
  dt <- make_test_dt()
  dt <- data.table::rbindlist(list(dt, dt[1L, ]), use.names = TRUE)

  expect_error(
    write_ts_shiny_app(dt, out_dir = tempfile("tsShinyApp-")),
    "not uniquely identified"
  )
})

test_that("cascading must be a scalar non-missing logical", {
  dt <- make_test_dt()

  expect_error(
    write_ts_shiny_app(dt, out_dir = tempfile("tsShinyApp-"), cascading = NA),
    "exactly TRUE or FALSE"
  )
  expect_error(
    write_ts_shiny_app(dt, out_dir = tempfile("tsShinyApp-"), cascading = c(TRUE, FALSE)),
    "exactly TRUE or FALSE"
  )
})

test_that("numeric-coded dimensions remain measures unless converted", {
  dt <- data.table::data.table(
    time = c("2024-Q1", "2024-Q2"),
    region_code = c(1, 1),
    value = c(10, 11)
  )
  out <- tempfile("tsShinyApp-")
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  info <- write_ts_shiny_app(dt, out_dir = out)
  expect_true("region_code" %in% info$numeric_columns)
})

# tsShinyApp

`tsShinyApp` is a deliberately small R package with one exported function:

```r
write_ts_shiny_app()
```

The function takes a `data.table`, saves the data as an RDS file, and writes a ready-to-run single-file Shiny application (`app.R`) for interactively exploring one or more time series.

## What the generated app provides

- Multiple-selection, server-side Selectize filters for every categorical column.
- Optional left-to-right cascading filter choices.
- One interactive Plotly chart for every numeric measure column.
- A shared user-selectable time window.
- Optional rebasing of each displayed series to `100` at a selected base period.
- Optional point markers, **off by default**.
- Independently resizable chart cards: right edge changes width, bottom edge changes height, and the bottom-right corner changes both.
- Full-screen chart cards.
- A top-bar working spinner while Shiny or Plotly is busy.
- A maximum of 10 displayed series at a time. If broader filters match more than 10 series, the app shows a prominent warning and sends the same random sample of 10 series to every chart.
- Mouse-wheel/two-finger scrolling over charts scrolls the page instead of zooming the chart. Plotly drag and toolbar zoom remain available.
- A compact Plotly modebar positioned away from the resizable right edge; the redundant Cartesian hover-mode toggle buttons are removed while comparison hover remains the chart default.
- Automatic WebGL conversion for sufficiently large individual Plotly charts.

## Installation

### From a local clone

From the directory containing the package:

```r
install.packages(".", repos = NULL, type = "source")
```

For development:

```r
# install.packages("pak")
pak::local_install(".")
```

### From GitHub

After the repository has been pushed to GitHub:

```r
# install.packages("remotes")
remotes::install_github("<github-user>/tsShinyApp")
```

Replace `<github-user>` with the repository owner.

The package itself imports `data.table`. To **run the generated Shiny app**, install:

```r
install.packages(c("shiny", "bslib", "data.table", "plotly", "htmltools"))
```

## Basic usage

```r
library(data.table)
library(tsShinyApp)

DT <- CJ(
  COUNTRY = c("A", "B"),
  AGE = c("15-24", "25-64"),
  time = c("2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4")
)

set.seed(1)
DT[, `:=`(
  employment = runif(.N, 60, 80),
  unemployment = runif(.N, 3, 12)
)]

write_ts_shiny_app(
  DT,
  out_dir = "my-timeseries-app",
  title = "Economic indicators"
)

shiny::runApp("my-timeseries-app")
```

The output directory contains exactly the application file and its saved data by default:

```text
my-timeseries-app/
├── app.R
└── data.rds
```

Keep these files together.

## Function signature

```r
write_ts_shiny_app(
  dt,
  out_dir = "shiny-ts-app",
  app_file = "app.R",
  data_file = "data.rds",
  title = "Time-series explorer",
  cascading = FALSE,
  overwrite = FALSE,
  compress = "gzip"
)
```

## Function arguments

| Argument | Default | Requirements and meaning |
|---|---|---|
| `dt` | required | A non-empty `data.table` with unique column names. It must contain exactly one time column named `time` or `time_period`, matched case-insensitively. After excluding the time column, at least one numeric measure column must remain. The time column plus every categorical column must uniquely identify every row. |
| `out_dir` | `"shiny-ts-app"` | One non-empty character string naming the output directory. It is created recursively if needed. |
| `app_file` | `"app.R"` | One non-empty file name, not a path. Normally leave this as `"app.R"`. |
| `data_file` | `"data.rds"` | One non-empty file name, not a path. The generated `app.R` expects this file beside it. |
| `title` | `"Time-series explorer"` | One non-missing character string used as the application title. |
| `cascading` | `FALSE` | Exactly one non-missing logical value. With `TRUE`, categorical choices cascade according to the categorical columns' original left-to-right order in `dt`. |
| `overwrite` | `FALSE` | If `FALSE`, the function stops rather than replacing an existing generated app or data file. Set to `TRUE` to overwrite intentionally. |
| `compress` | `"gzip"` | Passed directly to `saveRDS(..., compress = compress)`. |

## Input data contract

### The input must be a `data.table`

A plain `data.frame` is not accepted directly:

```r
DT <- data.table::as.data.table(my_data_frame)
```

### Exactly one time column is required

The name must be `time` or `time_period`, case-insensitively. For example, `TIME`, `Time`, and `TIME_PERIOD` are accepted.

Do not include both `time` and `time_period` in the same table.

The time column may be numeric, character, factor, ordered factor, `Date`, or POSIX date-time. It must contain at least one usable non-missing value.

### Numeric columns are measures

Every numeric column other than the time column gets its own chart.

> [!IMPORTANT]
> If a dimension is represented by numeric codes, convert it to character or factor before calling `write_ts_shiny_app()`. Otherwise it is intentionally treated as a numeric measure.

```r
DT[, region_code := factor(region_code)]
```

### Non-numeric columns are categorical dimensions

Every non-numeric column other than the time column becomes a filter and forms part of the identity of a time series.

Missing categorical values are supported and appear as `(Missing)`. Empty strings appear as `(Blank)`.

List-like, matrix, complex, and raw categorical columns are rejected.

### Rows must be uniquely identified

The generator **never aggregates coincident rows**.

The key

```r
c(time_column, all_categorical_columns)
```

must uniquely identify each row.

For data with:

```text
COUNTRY
AGE
SEX
time
value
```

the unique key is:

```text
COUNTRY + AGE + SEX + time
```

You can check this yourself:

```r
key_cols <- c("time", "COUNTRY", "AGE", "SEX")
data.table::uniqueN(DT, by = key_cols) == nrow(DT)
```

If the uniqueness condition fails, the generator stops and prints example duplicate keys.

## Cascading filters

By default:

```r
cascading = FALSE
```

all categorical filters keep their complete choice sets. A user's selections affect which rows are plotted, but not the values listed in other filters.

With:

```r
cascading = TRUE
```

choices cascade from left to right according to the **original position of categorical columns in `dt`**.

Suppose the input columns are ordered as:

```text
time_period
country
region
industry
occupation_group
contract_type
value
```

The filter hierarchy is therefore:

```text
country
  -> region
    -> industry
      -> occupation_group
        -> contract_type
```

A selection in `country` restricts the available choices in every downstream filter. A subsequent `region` selection further restricts `industry`, `occupation_group`, and `contract_type`. A later filter never changes an earlier filter's choices.

If an upstream change makes a currently selected downstream value impossible, that invalid downstream selection is cleared automatically.

An empty upstream selector means all **currently available** values at that level.

## Time ordering

The generated app creates an internal ordered time index while retaining the original labels for display.

It directly handles numeric time values, ordered factors, `Date` values, POSIX date-times, and common textual formats such as:

```text
2024
FY2024
2024-Q1
2024Q1
Q1-2024
2024-H1
2024-M01
202401
2024-W01
2024-01-31
Jan 2024
January 2024
2024 Jan
```

Unrecognized character/factor labels fall back to stable lexical ordering. For unusual period systems, an ordered factor is a convenient way to define the intended order explicitly.

## Filtering and the 10-series limit

A distinct time series is the complete combination of all categorical dimensions.

If the current filters match at most 10 series, all matching series are plotted.

If more than 10 match:

1. the app displays a prominent warning in the top bar;
2. it randomly samples 10 matching series;
3. the same sample is used for every numeric chart;
4. the sample remains stable while only the time window, index settings, or marker setting changes;
5. changing the categorical filter selection can produce a new random sample.

The sidebar row count still reports all rows matching the categorical filters, not only the sampled rows sent to Plotly.

## Time window and index mode

`From` and `To` select the common time range displayed across all charts.

When `Show as index` is enabled, each displayed series is normalized independently to `100` at the selected base time:

```text
index(t) = 100 * value(t) / value(base_time)
```

A series is blank where its base-period value is missing or zero. The base period may lie outside the currently displayed time window.

## Chart interaction and scrolling

Mouse-wheel Plotly zoom is deliberately disabled:

```r
scrollZoom = FALSE
```

This means that when the pointer is over a chart, ordinary mouse-wheel or touchpad scrolling moves through the page and therefore makes it easy to move from one metric chart to another.

Zooming is still available through normal Plotly drag/toolbar controls.

Comparison hover remains the fixed default. The redundant `hoverClosestCartesian` and `hoverCompareCartesian` modebar buttons are removed, and the remaining modebar is inset from the card's resizable right edge to avoid scrollbar flicker.

`Show point markers` is unchecked by default.

## Resizing charts

The **outer chart card** is user-resizable:

- drag its shaded right edge to change width;
- drag its shaded bottom edge to change height;
- drag the shaded bottom-right corner to change both.

Plotly follows the new card dimensions through a browser `ResizeObserver`.

If a card is resized wider than the available chart pane, that card's wrapper can scroll horizontally without creating its own vertical scrolling region.

## Performance choices

The generated app is designed to avoid unnecessary repeated work:

- filter membership is encoded once at startup;
- server-side Selectize is used for categorical choices;
- filtering carries integer row indices instead of repeatedly copying the full filtered `data.table`;
- series labels are precomputed once;
- only the selected/sampled series are materialized for chart construction;
- index rebasing uses the precomputed integer series ID instead of repeatedly merging on all categorical columns;
- at most 10 series are sent to Plotly after categorical filtering;
- sufficiently large Plotly figures are converted to WebGL;
- a shared reactive series sample is reused by all metric charts.

## Included realistic test data

The package includes a synthetic, real-life-like quarterly European workforce dataset with a useful cascading hierarchy.

Find it with:

```r
csv <- system.file(
  "extdata",
  "synthetic_regional_workforce_quarterly.csv",
  package = "tsShinyApp"
)

DT <- data.table::fread(csv)
```

Its categorical order is:

```text
country -> region -> industry -> occupation_group -> contract_type
```

Test independent filters:

```r
write_ts_shiny_app(
  DT,
  out_dir = "test-independent",
  cascading = FALSE,
  overwrite = TRUE
)
```

Test cascading filters:

```r
write_ts_shiny_app(
  DT,
  out_dir = "test-cascading",
  cascading = TRUE,
  overwrite = TRUE
)
```

## Return value

The function invisibly returns:

```r
list(
  directory = ...,
  app = ...,
  data = ...,
  time_column = ...,
  numeric_columns = ...,
  categorical_columns = ...,
  cascading = ...
)
```

For example:

```r
info <- write_ts_shiny_app(DT, out_dir = "my-app")
shiny::runApp(info$directory)
```

## Deployment

The generated app is an ordinary Shiny app consisting of `app.R` plus the RDS data file. Deploy the whole generated directory.

The deployment environment needs:

```text
shiny
bslib
data.table
plotly
htmltools
```

The installed `tsShinyApp` package itself is **not** required at runtime by an already generated application.

## Development and release

The repository contains:

- testthat unit tests;
- an R CMD check GitHub Actions workflow;
- a tag-triggered GitHub release workflow that builds, checks, and uploads the source package;
- `NEWS.md`;
- MIT licensing files;
- dependency acknowledgements in `inst/NOTICE`.

A typical first release sequence is:

```bash
git tag v0.1.0
git push origin v0.1.0
```

The release workflow is configured for tags matching `v*`.

## License

`tsShinyApp` is released under the MIT License. Copyright 2026 Alek Rutkowski.

## Acknowledgements

`tsShinyApp` depends directly on [`data.table`](https://r-datatable.com/) and generates applications built with [`Shiny`](https://shiny.posit.co/), [`bslib`](https://rstudio.github.io/bslib/), [`plotly` for R](https://plotly-r.com/), and [`htmltools`](https://rstudio.github.io/htmltools/). Tests use [`testthat`](https://testthat.r-lib.org/).

These are independent open-source projects. Their own copyright notices and licenses apply. See `inst/NOTICE` for the dependency acknowledgement list.

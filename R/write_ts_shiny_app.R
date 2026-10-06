#' Generate a self-contained Shiny time-series explorer
#'
#' Creates a ready-to-run Shiny application (`app.R`) and an RDS copy of the
#' supplied `data.table`. The app provides categorical filters, interactive
#' Plotly time-series charts for numeric measures, time-window controls,
#' optional index rebasing, chart-card resizing, and a 10-series display cap.
#'
#' @param dt A non-empty [data.table::data.table()] with unique column names.
#'   It must contain exactly one time column named `time` or `time_period`
#'   (case-insensitive) and at least one numeric measure column other than the
#'   time column. Every non-numeric, non-time column is treated as a
#'   categorical dimension. The time column together with all categorical
#'   columns must uniquely identify every row.
#' @param out_dir A length-one non-empty character string naming the output
#'   directory. It is created recursively if needed.
#' @param app_file A length-one non-empty file name for the generated Shiny
#'   application. It must be a file name, not a path.
#' @param data_file A length-one non-empty file name for the generated RDS data
#'   file. It must be a file name, not a path.
#' @param title A length-one non-missing character string used as the app title.
#' @param cascading A single non-missing logical. If `TRUE`, categorical filter
#'   choices cascade in the categorical columns' original left-to-right order
#'   in `dt`: selections in earlier filters restrict choices in later filters,
#'   while later filters never alter earlier choices. Invalid downstream
#'   selections are cleared automatically. The default is `FALSE`.
#' @param overwrite A single logical. If `TRUE`, existing generated `app_file`
#'   or `data_file` files may be replaced. The default is `FALSE`.
#' @param compress Passed to [base::saveRDS()] as its `compress` argument.
#'
#' @details
#' Numeric columns other than the time column are treated as measures and get
#' one chart each. Numeric-coded dimensions should therefore be converted to
#' character or factor before calling this function.
#'
#' The generated app requires the packages `shiny`, `bslib`, `data.table`,
#' `plotly`, and `htmltools` at runtime. Mouse-wheel zoom is disabled so normal
#' page scrolling works while the pointer is over a chart; drag and modebar
#' zoom remain available. Point markers are off by default.
#'
#' If more than 10 distinct time series match the categorical filters, the app
#' displays a warning and sends a stable random sample of 10 matching series to
#' all charts until the categorical selection changes.
#'
#' @return Invisibly returns a list with `directory`, `app`, `data`,
#'   `time_column`, `numeric_columns`, `categorical_columns`, and `cascading`.
#'
#' @examples
#' dt <- data.table::data.table(
#'   country = rep(c("A", "B"), each = 4L),
#'   time = rep(c("2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4"), 2L),
#'   value = seq_len(8L)
#' )
#' out <- file.path(tempdir(), "ts-shiny-example")
#' write_ts_shiny_app(dt, out_dir = out, overwrite = TRUE)
#' unlink(out, recursive = TRUE)
#'
#' @export
write_ts_shiny_app <- function(
    dt,
    out_dir = "shiny-ts-app",
    app_file = "app.R",
    data_file = "data.rds",
    title = "Time-series explorer",
    cascading = FALSE,
    overwrite = FALSE,
    compress = "gzip"
) {
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.", call. = FALSE)

  if (!data.table::is.data.table(dt))
    stop("'dt' must be a data.table.", call. = FALSE)
  if (!nrow(dt))
    stop("'dt' has no rows.", call. = FALSE)
  if (anyDuplicated(names(dt)))
    stop("Column names must be unique.", call. = FALSE)
  if (!is.character(out_dir) || length(out_dir) != 1L || !nzchar(out_dir))
    stop("'out_dir' must be one non-empty character string.", call. = FALSE)
  if (!is.character(app_file) || length(app_file) != 1L || !nzchar(app_file))
    stop("'app_file' must be one non-empty character string.", call. = FALSE)
  if (!is.character(data_file) || length(data_file) != 1L || !nzchar(data_file))
    stop("'data_file' must be one non-empty character string.", call. = FALSE)
  if (dirname(app_file) != "." || dirname(data_file) != ".")
    stop("'app_file' and 'data_file' must be file names, not paths.", call. = FALSE)
  if (!is.character(title) || length(title) != 1L || is.na(title))
    stop("'title' must be one character string.", call. = FALSE)
  if (!is.logical(cascading) || length(cascading) != 1L || is.na(cascading))
    stop("'cascading' must be exactly TRUE or FALSE.", call. = FALSE)

  x <- data.table::copy(dt)
  nms_lower <- tolower(names(x))
  time_ix <- which(nms_lower %in% c("time", "time_period"))

  if (!length(time_ix))
    stop("No time column found. Expected 'time' or 'time_period' (case-insensitive).",
         call. = FALSE)
  if (length(time_ix) > 1L)
    stop(
      "More than one candidate time column was found: ",
      paste(names(x)[time_ix], collapse = ", "),
      ". Keep exactly one column named 'time' or 'time_period' (case-insensitive).",
      call. = FALSE
    )

  time_col <- names(x)[time_ix]
  if (!is.atomic(x[[time_col]]) || !is.null(dim(x[[time_col]])))
    stop("The time column must be an atomic vector.", call. = FALSE)

  numeric_cols <- setdiff(
    names(x)[vapply(x, is.numeric, logical(1L))],
    time_col
  )
  if (!length(numeric_cols))
    stop("No numeric measure columns were found after excluding the time column.",
         call. = FALSE)

  categorical_cols <- names(x)[
    !vapply(x, is.numeric, logical(1L)) & names(x) != time_col
  ]

  bad_cat <- categorical_cols[vapply(
    categorical_cols,
    function(nm) {
      z <- x[[nm]]
      !is.atomic(z) || !is.null(dim(z)) || is.complex(z) || is.raw(z)
    },
    logical(1L)
  )]
  if (length(bad_cat))
    stop(
      "These non-numeric columns cannot be used as categorical filters: ",
      paste(bad_cat, collapse = ", "),
      call. = FALSE
    )

  key_cols <- c(time_col, categorical_cols)
  if (data.table::uniqueN(x, by = key_cols) != nrow(x)) {
    dup_mask <-
      duplicated(x, by = key_cols) |
      duplicated(x, by = key_cols, fromLast = TRUE)
    dup_keys <- x[dup_mask, key_cols, with = FALSE]
    dup_keys <- dup_keys[!duplicated(dup_keys, by = key_cols)]
    example_keys <- utils::head(dup_keys, 8L)
    example_text <- paste(
      utils::capture.output(print(example_keys)),
      collapse = "\n"
    )
    stop(
      "The input data.table is not uniquely identified by the time column plus all categorical columns.\n",
      "Key columns: ", paste(key_cols, collapse = ", "), ".\n",
      "Found ", format(nrow(dup_keys), big.mark = ","),
      " duplicated key combination(s). Examples:\n", example_text,
      "\nResolve these duplicates before creating the app; the generated app does not aggregate coincident rows.",
      call. = FALSE
    )
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  app_path <- file.path(out_dir, app_file)
  data_path <- file.path(out_dir, data_file)

  existing <- c(app_path, data_path)[file.exists(c(app_path, data_path))]
  if (length(existing) && !isTRUE(overwrite))
    stop(
      "Refusing to overwrite existing file(s): ",
      paste(existing, collapse = ", "),
      ". Use overwrite = TRUE if intended.",
      call. = FALSE
    )

  saveRDS(x, data_path, compress = compress)

  r_literal <- function(z)
    paste(utils::capture.output(dput(z)), collapse = "")

  app_template <- r"---[
# Generated by write_ts_shiny_app().
# Keep this app.R beside the RDS data file.

DATA_FILE <- @@DATA_FILE@@
APP_TITLE <- @@APP_TITLE@@
CASCADING <- @@CASCADING@@

required_packages <- c("shiny", "bslib", "data.table", "plotly", "htmltools")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the missing package(s) before running this app: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(data.table)
library(plotly)

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

if (!file.exists(DATA_FILE)) {
  stop(
    "Cannot find '", DATA_FILE,
    "'. Run the app from the directory containing app.R and the data file.",
    call. = FALSE
  )
}

DT <- readRDS(DATA_FILE)
setDT(DT)

if (anyDuplicated(names(DT)))
  stop("The data contains duplicated column names.", call. = FALSE)

nms_lower <- tolower(names(DT))
time_ix <- which(nms_lower %in% c("time", "time_period"))
if (length(time_ix) != 1L)
  stop("Expected exactly one time column named 'time' or 'time_period' (case-insensitive).",
       call. = FALSE)

time_col <- names(DT)[time_ix]
numeric_cols <- setdiff(
  names(DT)[vapply(DT, is.numeric, logical(1L))],
  time_col
)
categorical_cols <- names(DT)[
  !vapply(DT, is.numeric, logical(1L)) & names(DT) != time_col
]

if (!length(numeric_cols))
  stop("No numeric measure columns were found.", call. = FALSE)

key_cols <- c(time_col, categorical_cols)
if (uniqueN(DT, by = key_cols) != nrow(DT))
  stop(
    "The data file is no longer uniquely identified by the time column plus all categorical columns. ",
    "Regenerate the app after resolving duplicate keys.",
    call. = FALSE
  )

make_internal_name <- function(base, existing) {
  out <- base
  while (out %chin% existing)
    out <- paste0(out, "_")
  out
}

time_id_col <- make_internal_name(".tsapp_time_id", names(DT))
series_id_col <- make_internal_name(".tsapp_series_id", c(names(DT), time_id_col))

order_time_levels <- function(x) {
  x_chr <- as.character(x)
  u <- unique(x_chr[!is.na(x_chr)])
  if (!length(u)) return(character())

  if (is.ordered(x)) {
    lev <- levels(x)
    return(c(lev[lev %chin% u], u[!u %chin% lev]))
  }

  if (inherits(x, "Date")) {
    key <- as.numeric(as.Date(u))
    return(u[order(key, method = "radix")])
  }

  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    src <- match(u, x_chr)
    key <- as.numeric(as.POSIXct(x[src]))
    return(u[order(key, method = "radix")])
  }

  if (is.numeric(x)) {
    src <- match(u, x_chr)
    key <- as.numeric(x[src])
    return(u[order(key, method = "radix")])
  }

  s <- trimws(u)
  numeric_key <- suppressWarnings(as.numeric(s))
  if (all(is.finite(numeric_key)))
    return(u[order(numeric_key, s, method = "radix")])

  iso_date <- grepl("^[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}$", s)
  if (all(iso_date)) {
    date_key <- suppressWarnings(as.Date(gsub("[/.]", "-", s)))
    if (!anyNA(date_key))
      return(u[order(date_key, method = "radix")])
  }

  z <- toupper(gsub("[[:space:]_./-]", "", s))
  key <- rep(NA_real_, length(z))

  annual <- grepl("^(FY)?[12][0-9]{3}$", z)
  key[annual] <- suppressWarnings(as.numeric(sub("^FY", "", z[annual])))

  yq <- grepl("^[12][0-9]{3}Q[1-4]$", z)
  if (any(yq)) {
    key[yq] <- as.numeric(substr(z[yq], 1L, 4L)) +
      (as.numeric(substr(z[yq], 6L, 6L)) - 1) / 4
  }

  qy <- grepl("^Q[1-4][12][0-9]{3}$", z)
  if (any(qy)) {
    key[qy] <- as.numeric(substr(z[qy], 3L, 6L)) +
      (as.numeric(substr(z[qy], 2L, 2L)) - 1) / 4
  }

  yh <- grepl("^[12][0-9]{3}H[12]$", z)
  if (any(yh)) {
    key[yh] <- as.numeric(substr(z[yh], 1L, 4L)) +
      (as.numeric(substr(z[yh], 6L, 6L)) - 1) / 2
  }

  hy <- grepl("^H[12][12][0-9]{3}$", z)
  if (any(hy)) {
    key[hy] <- as.numeric(substr(z[hy], 3L, 6L)) +
      (as.numeric(substr(z[hy], 2L, 2L)) - 1) / 2
  }

  ym <- grepl("^[12][0-9]{3}M(0?[1-9]|1[0-2])$", z)
  if (any(ym)) {
    yr <- as.numeric(substr(z[ym], 1L, 4L))
    mo <- as.numeric(sub("^[12][0-9]{3}M", "", z[ym]))
    key[ym] <- yr + (mo - 1) / 12
  }

  compact_ym <- grepl("^[12][0-9]{3}(0[1-9]|1[0-2])$", z)
  if (any(compact_ym)) {
    yr <- as.numeric(substr(z[compact_ym], 1L, 4L))
    mo <- as.numeric(substr(z[compact_ym], 5L, 6L))
    key[compact_ym] <- yr + (mo - 1) / 12
  }

  yw <- grepl("^[12][0-9]{3}W(0?[1-9]|[1-4][0-9]|5[0-3])$", z)
  if (any(yw)) {
    yr <- as.numeric(substr(z[yw], 1L, 4L))
    wk <- as.numeric(sub("^[12][0-9]{3}W", "", z[yw]))
    key[yw] <- yr + (wk - 1) / 53
  }

  month_keys <- c(
    JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
    JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12,
    JANUARY = 1, FEBRUARY = 2, MARCH = 3, APRIL = 4, JUNE = 6,
    JULY = 7, AUGUST = 8, SEPTEMBER = 9, OCTOBER = 10,
    NOVEMBER = 11, DECEMBER = 12
  )

  ymon <- grepl("^[12][0-9]{3}[A-Z]+$", z)
  if (any(ymon)) {
    yr <- as.numeric(substr(z[ymon], 1L, 4L))
    mn <- substr(z[ymon], 5L, nchar(z[ymon]))
    mo <- unname(month_keys[mn])
    ok <- !is.na(mo)
    idx <- which(ymon)[ok]
    key[idx] <- yr[ok] + (mo[ok] - 1) / 12
  }

  mony <- grepl("^[A-Z]+[12][0-9]{3}$", z)
  if (any(mony)) {
    yr <- as.numeric(substr(z[mony], nchar(z[mony]) - 3L, nchar(z[mony])))
    mn <- substr(z[mony], 1L, nchar(z[mony]) - 4L)
    mo <- unname(month_keys[mn])
    ok <- !is.na(mo)
    idx <- which(mony)[ok]
    key[idx] <- yr[ok] + (mo[ok] - 1) / 12
  }

  if (all(is.finite(key)))
    return(u[order(key, s, method = "radix")])

  u[order(tolower(u), u, method = "radix")]
}

time_levels <- order_time_levels(DT[[time_col]])
if (!length(time_levels))
  stop("The time column has no non-missing values.", call. = FALSE)

DT[, (time_id_col) := match(as.character(get(time_col)), time_levels)]
if (length(categorical_cols)) {
  DT[, (series_id_col) := .GRP, by = categorical_cols]
} else {
  DT[, (series_id_col) := 1L]
}
valid_time_mask <- !is.na(DT[[time_id_col]])
missing_time_rows <- sum(!valid_time_mask)

encode_category <- function(x) {
  x_chr <- as.character(x)
  fifelse(is.na(x_chr), "na:", paste0("v:", x_chr))
}

category_choice_vector <- function(x) {
  x_chr <- as.character(x)
  observed <- unique(x_chr[!is.na(x_chr)])

  if (is.factor(x)) {
    lev <- levels(droplevels(x))
    observed <- c(lev[lev %chin% observed], observed[!observed %chin% lev])
  } else if (is.logical(x)) {
    observed <- intersect(c("FALSE", "TRUE"), observed)
  } else {
    observed <- sort(observed, na.last = TRUE, method = "radix")
  }

  values <- paste0("v:", observed)
  labels <- observed
  labels[labels == ""] <- "(Blank)"

  if (anyNA(x)) {
    values <- c(values, "na:")
    labels <- c(labels, "(Missing)")
  }

  stats::setNames(values, labels)
}

category_keys <- setNames(
  lapply(categorical_cols, function(nm) encode_category(DT[[nm]])),
  categorical_cols
)
category_choices <- setNames(
  lapply(categorical_cols, function(nm) category_choice_vector(DT[[nm]])),
  categorical_cols
)

series_labels <- if (length(categorical_cols)) {
  meta <- unique(
    DT[, c(series_id_col, categorical_cols), with = FALSE],
    by = series_id_col
  )
  parts <- lapply(categorical_cols, function(nm) {
    z <- as.character(meta[[nm]])
    z[is.na(z)] <- "(Missing)"
    paste0(nm, " = ", z)
  })
  labels <- do.call(paste, c(parts, sep = " | "))
  stats::setNames(labels, as.character(meta[[series_id_col]]))
} else {
  stats::setNames("Series", "1")
}
series_labels_html <- htmltools::htmlEscape(series_labels)
time_levels_html <- htmltools::htmlEscape(time_levels)
time_col_html <- htmltools::htmlEscape(time_col)

filter_ids <- setNames(
  sprintf("filter_%03d", seq_along(categorical_cols)),
  categorical_cols
)
plot_ids <- setNames(
  sprintf("metric_%03d", seq_along(numeric_cols)),
  numeric_cols
)
time_choices <- stats::setNames(
  as.character(seq_along(time_levels)),
  time_levels
)

filter_controls <- if (length(categorical_cols)) {
  tagList(lapply(categorical_cols, function(nm) {
    selectizeInput(
      filter_ids[[nm]],
      nm,
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All values",
        closeAfterSelect = FALSE,
        plugins = list("remove_button")
      ),
      width = "100%"
    )
  }))
} else {
  tags$p(class = "sidebar-note", "No categorical columns were found.")
}

plot_cards <- tagList(lapply(numeric_cols, function(metric) {
  tags$div(
    class = "metric-card-scroll",
    bslib::card(
      class = "metric-card metric-card-resizable",
      full_screen = TRUE,
      height = "600px",
      bslib::card_header(
        tags$div(
          class = "metric-heading",
          tags$span(metric),
          tags$span(class = "metric-badge", "Drag shaded card edges or corner to resize")
        )
      ),
      bslib::card_body(
        class = "metric-plot-body",
        plotlyOutput(plot_ids[[metric]], width = "100%", height = "100%"),
        tags$div(
          class = "resize-handle resize-handle-right",
          `data-resize` = "x",
          title = "Drag the card edge to resize width",
          `aria-label` = "Resize chart card width"
        ),
        tags$div(
          class = "resize-handle resize-handle-bottom",
          `data-resize` = "y",
          title = "Drag the card edge to resize height",
          `aria-label` = "Resize chart card height"
        ),
        tags$div(
          class = "resize-handle resize-handle-corner",
          `data-resize` = "xy",
          title = "Drag the card corner to resize width and height",
          `aria-label` = "Resize chart card width and height"
        )
      )
    )
  )
}))

ui <- bslib::page_sidebar(
  title = tags$div(
    class = "app-topbar",
    tags$div(
      class = "app-heading",
      tags$div(class = "app-heading-main", APP_TITLE),
      tags$div(class = "app-heading-sub", "Filter, compare, index and zoom across time")
    ),
    uiOutput("series_warning_top", class = "app-top-warning-slot")
  ),
  sidebar = bslib::sidebar(
    width = 360,
    tags$div(class = "sidebar-section-title", "Filters"),
    tags$p(
      class = "sidebar-note",
      if (CASCADING) {
        "Filters cascade from top to bottom in data-table column order. Leave a selector empty to include all currently available values."
      } else {
        "Leave a selector empty to include all values. Choose several values to compare them."
      }
    ),
    filter_controls,
    tags$hr(),
    tags$div(class = "sidebar-section-title", "Time window"),
    tags$div(
      class = "two-col-controls",
      selectizeInput("time_from", "From", choices = NULL, width = "100%"),
      selectizeInput("time_to", "To", choices = NULL, width = "100%")
    ),
    tags$hr(),
    checkboxInput("indexed", "Show as index", value = FALSE),
    conditionalPanel(
      "input.indexed === true",
      selectizeInput("base_time", "Base time = 100", choices = NULL, width = "100%"),
      tags$p(
        class = "sidebar-note",
        "Each displayed series is divided by its value at the selected base time. A series is blank where that base value is missing or zero."
      )
    ),
    checkboxInput("markers", "Show point markers", value = FALSE),
    tags$hr(),
    uiOutput("selection_status")
  ),
  fillable = FALSE,
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2563eb",
    base_font = '"Segoe UI", system-ui, sans-serif'
  ),
  tags$div(
    id = "tsapp-busy-indicator",
    class = "tsapp-busy is-active",
    role = "status",
    `aria-live` = "polite",
    `aria-hidden` = "false",
    tags$div(
      class = "tsapp-busy-card",
      tags$div(class = "tsapp-spinner", `aria-hidden` = "true"),
      tags$span("Working...")
    )
  ),
  tags$style(HTML("\n    body { background: #f6f8fb; }\n    .bslib-sidebar-layout > .sidebar { background: #ffffff; border-right: 1px solid #e6eaf0; }\n    .app-topbar { width: 100%; min-width: 0; display: flex; align-items: center; gap: 18px; }\n    .app-heading { flex: 0 0 auto; padding: 2px 0; }\n    .app-heading-main { font-weight: 750; letter-spacing: -0.02em; }\n    .app-heading-sub { font-size: 0.83rem; opacity: 0.72; font-weight: 400; margin-top: 2px; }\n    .app-top-warning-slot { flex: 1 1 auto; min-width: 0; display: flex; justify-content: flex-end; align-items: center; }\n    .top-series-warning { display: inline-flex; align-items: center; max-width: 860px; padding: 8px 12px; border: 1px solid #f6d98b; border-radius: 9px; background: #fffbeb; color: #7c2d12; box-shadow: 0 2px 8px rgba(120,53,15,.10); font-size: .82rem; font-weight: 550; line-height: 1.3; }\n    .top-series-warning strong { font-weight: 800; margin-right: 4px; }\n    .sidebar-section-title { font-weight: 700; font-size: 0.84rem; text-transform: uppercase; letter-spacing: .055em; color: #475569; margin: 2px 0 8px; }\n    .sidebar-note { font-size: 0.79rem; line-height: 1.35; color: #64748b; margin-top: -3px; }\n    .two-col-controls { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; }\n    .metric-card-scroll { overflow-x: auto; overflow-y: hidden; padding: 0 14px 16px 0; margin-bottom: 18px; overscroll-behavior-x: contain; }\n    .metric-card { position: relative; width: 100%; min-width: 420px; min-height: 320px; max-width: none; box-sizing: border-box; border: 1px solid #e5eaf0; border-right: 10px solid #cbd5e1; border-bottom: 10px solid #cbd5e1; border-radius: 14px; box-shadow: 0 6px 22px rgba(15, 23, 42, .075); margin-bottom: 0; overflow: hidden; }\n    .metric-card.is-resizing { user-select: none; }\n    .metric-plot-body { position: static !important; min-width: 0; min-height: 0; padding: 0 !important; overflow: visible; }\n    .metric-plot-body > .shiny-plot-output, .metric-plot-body > .plotly { width: 100% !important; height: 100% !important; min-width: 0; min-height: 0; }\n    .resize-handle { position: absolute; z-index: 50; touch-action: none; }\n    .resize-handle-right { top: 0; right: 0; bottom: 18px; width: 12px; cursor: ew-resize; }\n    .resize-handle-bottom { left: 0; right: 18px; bottom: 0; height: 12px; cursor: ns-resize; }\n    .resize-handle-corner { right: 0; bottom: 0; width: 20px; height: 20px; cursor: nwse-resize; background: #94a3b8; border-top-left-radius: 6px; }\n    .resize-handle-right:hover, .resize-handle-bottom:hover { background: rgba(100, 116, 139, .22); }\n    .resize-handle-corner:hover { background: #64748b; }\n    .metric-card .card-header { background: #fff; border-bottom: 1px solid #eef1f5; padding: 12px 16px; }\n    .metric-card .modebar-container { top: 6px !important; right: 22px !important; }\n    .metric-heading { display: flex; align-items: center; justify-content: space-between; gap: 12px; font-weight: 700; }\n    .metric-badge { font-size: 0.72rem; font-weight: 650; padding: 4px 8px; border-radius: 999px; background: #eff6ff; color: #1d4ed8; }\n    .status-box { background: #f8fafc; border: 1px solid #e8edf3; border-radius: 10px; padding: 10px 11px; font-size: .79rem; color: #475569; line-height: 1.45; }\n    .status-box strong { color: #0f172a; }\n    .tsapp-busy { position: fixed; inset: 0; z-index: 2000; display: flex; align-items: center; justify-content: center; background: rgba(246,248,251,.46); backdrop-filter: blur(1px); -webkit-backdrop-filter: blur(1px); opacity: 0; visibility: hidden; pointer-events: none; transition: opacity .12s ease, visibility .12s ease; }\n    .tsapp-busy.is-active { opacity: 1; visibility: visible; pointer-events: auto; }\n    .tsapp-busy-card { min-width: 170px; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 14px; padding: 24px 28px; border: 1px solid #dbe3ec; border-radius: 18px; background: rgba(255,255,255,.97); box-shadow: 0 14px 38px rgba(15,23,42,.20); color: #334155; font-size: 1rem; font-weight: 700; }\n    .tsapp-spinner { width: 56px; height: 56px; border: 6px solid #cbd5e1; border-top-color: #2563eb; border-radius: 50%; animation: tsapp-spin .7s linear infinite; }\n    @keyframes tsapp-spin { to { transform: rotate(360deg); } }\n    @media (prefers-reduced-motion: reduce) { .tsapp-spinner { animation-duration: 1.5s; } }\n    .selectize-control.multi .selectize-input > div { border-radius: 6px; }\n    @media (max-width: 700px) {\n      .app-topbar { flex-wrap: wrap; gap: 8px 12px; }\n      .app-top-warning-slot { order: 3; flex: 1 0 100%; justify-content: flex-start; }\n      .top-series-warning { width: 100%; max-width: none; }\n      .two-col-controls { grid-template-columns: 1fr; }\n      .metric-card { border-radius: 10px; }\n    }\n  ")),
  plot_cards,
  tags$script(HTML("
    (function() {
      var MIN_WIDTH = 420;
      var MIN_HEIGHT = 320;
      var MAX_WIDTH = 4000;
      var MAX_HEIGHT = 2400;
      var shinyBusy = true;
      var plotlyBusyCount = 0;

      function updateBusyIndicator() {
        var el = document.getElementById('tsapp-busy-indicator');
        if (!el) return;
        var active = shinyBusy || plotlyBusyCount > 0;
        el.classList.toggle('is-active', active);
        el.setAttribute('aria-hidden', active ? 'false' : 'true');
      }

      function finishPlotlyWork() {
        window.requestAnimationFrame(function() {
          window.requestAnimationFrame(function() {
            plotlyBusyCount = Math.max(0, plotlyBusyCount - 1);
            updateBusyIndicator();
          });
        });
      }

      function wrapPlotlyMethod(name) {
        if (!window.Plotly || typeof Plotly[name] !== 'function') return;
        var original = Plotly[name];
        if (original._tsappBusyWrapped) return;

        var wrapped = function() {
          plotlyBusyCount += 1;
          updateBusyIndicator();
          try {
            var result = original.apply(this, arguments);
            if (result && typeof result.then === 'function') {
              return result.then(
                function(value) { finishPlotlyWork(); return value; },
                function(error) { finishPlotlyWork(); throw error; }
              );
            }
            finishPlotlyWork();
            return result;
          } catch (error) {
            finishPlotlyWork();
            throw error;
          }
        };
        wrapped._tsappBusyWrapped = true;
        wrapped._tsappBusyOriginal = original;
        Plotly[name] = wrapped;
      }

      function hookPlotlyBusyState() {
        if (!window.Plotly) {
          window.setTimeout(hookPlotlyBusyState, 100);
          return;
        }
        ['newPlot', 'react', 'redraw'].forEach(wrapPlotlyMethod);
      }

      if (window.jQuery) {
        window.jQuery(document)
          .on('shiny:busy.tsapp', function() {
            shinyBusy = true;
            updateBusyIndicator();
          })
          .on('shiny:idle.tsapp', function() {
            shinyBusy = false;
            updateBusyIndicator();
          });
      }
      hookPlotlyBusyState();
      updateBusyIndicator();

      function clamp(x, lo, hi) {
        return Math.max(lo, Math.min(hi, x));
      }

      function resizePlot(box) {
        var plot = box.querySelector('.js-plotly-plot');
        if (plot && window.Plotly && Plotly.Plots && Plotly.Plots.resize) {
          Plotly.Plots.resize(plot);
        }
      }

      function attachHandle(box, handle) {
        if (handle.dataset.tsappHandleReady === '1') return;
        handle.dataset.tsappHandleReady = '1';

        handle.addEventListener('pointerdown', function(ev) {
          if (ev.button !== 0 && ev.pointerType === 'mouse') return;
          ev.preventDefault();

          var axis = handle.dataset.resize || '';
          var rect = box.getBoundingClientRect();
          var startX = ev.clientX;
          var startY = ev.clientY;
          var startWidth = rect.width;
          var startHeight = rect.height;

          box.classList.add('is-resizing');
          handle.setPointerCapture(ev.pointerId);

          function move(moveEv) {
            if (axis.indexOf('x') !== -1) {
              var width = clamp(startWidth + moveEv.clientX - startX, MIN_WIDTH, MAX_WIDTH);
              box.style.width = width + 'px';
            }
            if (axis.indexOf('y') !== -1) {
              var height = clamp(startHeight + moveEv.clientY - startY, MIN_HEIGHT, MAX_HEIGHT);
              box.style.height = height + 'px';
            }
          }

          function stop(stopEv) {
            handle.removeEventListener('pointermove', move);
            handle.removeEventListener('pointerup', stop);
            handle.removeEventListener('pointercancel', stop);
            if (handle.hasPointerCapture && handle.hasPointerCapture(stopEv.pointerId)) {
              handle.releasePointerCapture(stopEv.pointerId);
            }
            box.classList.remove('is-resizing');
            window.requestAnimationFrame(function() { resizePlot(box); });
          }

          handle.addEventListener('pointermove', move);
          handle.addEventListener('pointerup', stop);
          handle.addEventListener('pointercancel', stop);
        });
      }

      function observeBox(box) {
        if (box.dataset.tsappResizeObserved === '1') return;
        box.dataset.tsappResizeObserved = '1';

        box.querySelectorAll('.resize-handle').forEach(function(handle) {
          attachHandle(box, handle);
        });

        if ('ResizeObserver' in window) {
          var ro = new ResizeObserver(function() {
            window.requestAnimationFrame(function() { resizePlot(box); });
          });
          ro.observe(box);
          box._tsappResizeObserver = ro;
        }

        var mo = new MutationObserver(function() {
          if (box.querySelector('.js-plotly-plot')) {
            mo.disconnect();
            window.requestAnimationFrame(function() { resizePlot(box); });
          }
        });
        mo.observe(box, { childList: true, subtree: true });
        box._tsappMutationObserver = mo;
        resizePlot(box);
      }

      function initResizablePlots() {
        document.querySelectorAll('.metric-card-resizable').forEach(observeBox);
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', initResizablePlots, { once: true });
      } else {
        initResizablePlots();
      }
      document.addEventListener('shiny:connected', initResizablePlots);
    })();
  "))
)

server <- function(input, output, session) {
  session$onFlushed(function() {
    if (length(categorical_cols)) {
      invisible(lapply(categorical_cols, function(nm) {
        updateSelectizeInput(
          session,
          filter_ids[[nm]],
          choices = category_choices[[nm]],
          selected = character(),
          server = TRUE
        )
      }))
    }

    updateSelectizeInput(
      session, "time_from",
      choices = time_choices,
      selected = "1",
      server = TRUE
    )
    updateSelectizeInput(
      session, "time_to",
      choices = time_choices,
      selected = as.character(length(time_levels)),
      server = TRUE
    )
    updateSelectizeInput(
      session, "base_time",
      choices = time_choices,
      selected = "1",
      server = TRUE
    )
  }, once = TRUE)

  if (CASCADING && length(categorical_cols) > 1L) {
    cascade_choice_cache <- reactiveValues()

    observe({
      selected <- setNames(
        lapply(
          categorical_cols,
          function(nm) input[[filter_ids[[nm]]]] %||% character()
        ),
        categorical_cols
      )

      # Build each downstream choice set only from selections in earlier
      # categorical columns. Invalid downstream selections are dropped.
      allowed_rows <- rep(TRUE, nrow(DT))
      effective_selected <- selected

      for (j in seq_along(categorical_cols)) {
        nm <- categorical_cols[[j]]

        if (j > 1L) {
          available_values <- unique(category_keys[[nm]][allowed_rows])
          choices_j <- category_choices[[nm]][
            category_choices[[nm]] %chin% available_values
          ]

          selected_j <- effective_selected[[nm]]
          selected_j <- selected_j[selected_j %chin% unname(choices_j)]

          old_choices <- isolate(cascade_choice_cache[[nm]])
          new_choices <- unname(choices_j)

          if (
            is.null(old_choices) ||
            !identical(old_choices, new_choices) ||
            !identical(effective_selected[[nm]], selected_j)
          ) {
            freezeReactiveValue(input, filter_ids[[nm]])
            updateSelectizeInput(
              session,
              filter_ids[[nm]],
              choices = choices_j,
              selected = selected_j,
              server = TRUE
            )
          }

          cascade_choice_cache[[nm]] <- new_choices
          effective_selected[[nm]] <- selected_j
        }

        if (length(effective_selected[[nm]])) {
          allowed_rows <- allowed_rows &
            category_keys[[nm]] %chin% effective_selected[[nm]]
        }
      }
    }, priority = 1000)
  }

  filter_mask <- reactive({
    if (!length(categorical_cols)) return(valid_time_mask)

    selected <- setNames(
      lapply(categorical_cols, function(nm) input[[filter_ids[[nm]]]]),
      categorical_cols
    )
    active <- categorical_cols[lengths(selected) > 0L]
    if (!length(active)) return(valid_time_mask)

    conditions <- lapply(
      active,
      function(nm) category_keys[[nm]] %chin% selected[[nm]]
    )
    Reduce(`&`, conditions, init = valid_time_mask)
  })

  filtered_rows <- reactive({
    which(filter_mask())
  })

  series_selection <- reactive({
    rows <- filtered_rows()
    ids <- unique(DT[[series_id_col]][rows])
    ids <- ids[!is.na(ids)]
    n_series <- length(ids)
    shown_ids <- if (n_series > 10L) sample(ids, 10L) else ids
    list(total = n_series, shown_ids = shown_ids)
  })

  plot_rows <- reactive({
    rows <- filtered_rows()
    sel <- series_selection()
    if (!length(rows) || sel$total <= 10L)
      return(rows)

    rows[DT[[series_id_col]][rows] %in% sel$shown_ids]
  })

  safe_time_id <- function(x, default) {
    z <- suppressWarnings(as.integer(x %||% ""))
    if (length(z) != 1L || is.na(z)) z <- default
    max(1L, min(length(time_levels), z))
  }

  time_bounds <- reactive({
    from <- safe_time_id(input$time_from, 1L)
    to <- safe_time_id(input$time_to, length(time_levels))
    sort(c(from, to))
  })

  metric_data <- function(metric) {
    rows <- plot_rows()
    validate(need(length(rows), "No rows match the current filters."))

    time_id <- DT[[time_id_col]][rows]
    series_id <- DT[[series_id_col]][rows]
    values <- DT[[metric]][rows]

    indexed <- isTRUE(input$indexed)
    base_id <- if (indexed) safe_time_id(input$base_time, 1L) else NA_integer_

    if (indexed) {
      base_pos <- which(time_id == base_id)
      base_lookup <- stats::setNames(
        values[base_pos],
        as.character(series_id[base_pos])
      )
      base_values <- unname(base_lookup[as.character(series_id)])
      values <- fifelse(
        is.na(base_values) | base_values == 0,
        NA_real_,
        100 * values / base_values
      )
    }

    bounds <- time_bounds()
    keep <-
      time_id >= bounds[[1L]] &
      time_id <= bounds[[2L]] &
      !is.na(time_id)

    time_id <- time_id[keep]
    series_id <- series_id[keep]
    values <- values[keep]

    series_key <- as.character(series_id)
    out <- data.table(
      .ts_x = time_id,
      .ts_value = values,
      .ts_series = unname(series_labels[series_key]),
      .ts_series_html = unname(series_labels_html[series_key])
    )
    out[, .ts_time := time_levels[.ts_x]]
    setorder(out, .ts_series, .ts_x, na.last = TRUE)

    y_title <- if (indexed) {
      paste0("Index (", time_levels[base_id], " = 100)")
    } else {
      metric
    }
    y_title_html <- htmltools::htmlEscape(y_title)

    shown_value <- ifelse(
      is.na(out$.ts_value),
      "NA",
      formatC(out$.ts_value, digits = 7, format = "fg", flag = "#")
    )
    out[, .ts_hover := paste0(
      "<b>", .ts_series_html, "</b><br>",
      time_col_html, ": ", time_levels_html[.ts_x], "<br>",
      y_title_html, ": ", shown_value
    )]
    out[, .ts_series_html := NULL]

    list(data = out, y_title = y_title)
  }

  make_plot <- function(metric) {
    md <- metric_data(metric)
    d <- md$data
    validate(need(nrow(d), "No observations fall inside the selected time window."))

    bounds <- time_bounds()
    tick_count <- min(12L, bounds[[2L]] - bounds[[1L]] + 1L)
    tick_vals <- unique(as.integer(round(seq(
      bounds[[1L]], bounds[[2L]], length.out = tick_count
    ))))
    tick_text <- time_levels[tick_vals]
    show_legend <- uniqueN(d$.ts_series) > 1L
    markers_on <- isTRUE(input$markers)
    trace_mode <- if (markers_on) "lines+markers" else "lines"

    p <- plotly::plot_ly(
      d,
      x = ~.ts_x,
      y = ~.ts_value,
      split = ~.ts_series,
      type = "scatter",
      mode = trace_mode,
      text = ~.ts_hover,
      hoverinfo = "text",
      marker = list(
        size = if (markers_on) 5 else 0,
        opacity = if (markers_on) 1 else 0
      ),
      line = list(width = 2)
    )

    if (nrow(d) > 5000L)
      p <- plotly::toWebGL(p)

    p <- plotly::layout(
      p,
      showlegend = show_legend,
      hovermode = "x",
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = tick_vals,
        ticktext = tick_text,
        range = c(bounds[[1L]] - 0.2, bounds[[2L]] + 0.2),
        zeroline = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = md$y_title,
        zeroline = FALSE,
        gridcolor = "#eef2f7",
        automargin = TRUE
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.22,
        xanchor = "left",
        yanchor = "top"
      ),
      margin = list(l = 70, r = 42, t = 20, b = if (show_legend) 100 else 55),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
    p <- plotly::config(
      p,
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = FALSE,
      modeBarButtonsToRemove = c(
        "lasso2d", "select2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )

    p
  }

  invisible(lapply(numeric_cols, function(metric) {
    local({
      metric_local <- metric
      output_id <- plot_ids[[metric_local]]
      output[[output_id]] <- renderPlotly(make_plot(metric_local))
    })
  }))

  output$series_warning_top <- renderUI({
    sel <- series_selection()
    if (sel$total <= 10L) return(NULL)

    tags$div(
      class = "top-series-warning",
      role = "status",
      `aria-live` = "polite",
      tags$strong("Warning:"),
      format(sel$total, big.mark = ","),
      " time series match the current filters. To keep the charts responsive, Plotly is showing a random sample of 10 of them."
    )
  })

  output$selection_status <- renderUI({
    n_filtered <- length(filtered_rows())
    bounds <- time_bounds()
    tags$div(
      class = "status-box",
      tags$div(tags$strong(format(n_filtered, big.mark = ",")), " filtered rows"),
      tags$div(
        tags$strong(time_levels[bounds[[1L]]]),
        " to ",
        tags$strong(time_levels[bounds[[2L]]])
      ),
      if (missing_time_rows)
        tags$div(
          style = "margin-top:5px;",
          format(missing_time_rows, big.mark = ","),
          " row(s) with missing/unusable time are omitted."
        )
    )
  })
}

shinyApp(ui, server)
]---"

  app_text <- gsub(
    "@@DATA_FILE@@",
    r_literal(data_file),
    app_template,
    fixed = TRUE
  )
  app_text <- gsub(
    "@@APP_TITLE@@",
    r_literal(title),
    app_text,
    fixed = TRUE
  )
  app_text <- gsub(
    "@@CASCADING@@",
    if (isTRUE(cascading)) "TRUE" else "FALSE",
    app_text,
    fixed = TRUE
  )

  writeLines(app_text, app_path, useBytes = TRUE)

  out_dir_abs <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  message(
    "Created:\n  ", normalizePath(app_path, winslash = "/", mustWork = TRUE),
    "\n  ", normalizePath(data_path, winslash = "/", mustWork = TRUE),
    "\nRun with:\n  shiny::runApp(", r_literal(out_dir_abs), ")"
  )

  invisible(list(
    directory = out_dir_abs,
    app = normalizePath(app_path, winslash = "/", mustWork = TRUE),
    data = normalizePath(data_path, winslash = "/", mustWork = TRUE),
    time_column = time_col,
    numeric_columns = numeric_cols,
    categorical_columns = categorical_cols,
    cascading = cascading
  ))
}

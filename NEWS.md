# tsShinyApp 0.1.0

* Fixed R CMD check diagnostics for duplicate-key validation and made the duplicate-key regression test use explicit row subsetting.

* Initial GitHub-ready release of `write_ts_shiny_app()`.
* Generates one Plotly chart per numeric measure and server-side Selectize filters for categorical dimensions.
* Supports optional left-to-right cascading filters.
* Validates that time plus all categorical dimensions uniquely identify observations; generated apps never aggregate coincident rows.
* Supports time windows and optional base-period indexing to 100.
* Limits displays to a random sample of 10 series when broader filters select more than 10, with a prominent top-bar warning.
* Adds independently resizable chart cards and a busy indicator.
* Disables mouse-wheel Plotly zoom so normal vertical page scrolling works over charts; drag and toolbar zoom remain available.
* Keeps point markers off by default and moves/removes modebar controls that could cause edge-scrollbar flicker.
* Reduces repeated server-side copying by using filtered row indices, precomputed series labels, and series-ID based rebasing.

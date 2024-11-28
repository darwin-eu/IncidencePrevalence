
#' Plot incidence results
#'
#' @param result Incidence results
#' @param x Variable to plot on x axis
#' @param y Variable to plot on y axis. Options are: "incidence_100000_pys",
#' "outcome_count", "denominator_count", "person_days"
#' @param ylim Limits for the Y axis
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param options A list of optional plot options. See optionsPlot() for the
#' default parameters.
#'
#' @return A ggplot with the incidence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#' )
#' inc <- estimateIncidence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotIncidence(inc)
#' }
plotIncidence <- function(result,
                          x = "incidence_start_date",
                          y = "incidence_100000_pys",
                          ylim = c(0, NA),
                          ribbon = FALSE,
                          facet = NULL,
                          colour = NULL,
                          options = list()) {

  rlang::check_installed("visOmopResults", version = "0.5.0")

  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyPlot())
  }

  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(
      .data$result_type == "incidence"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("No incidence results available to plot")
    return(emptyPlot())
  }

  omopgenerics::assertChoice(y, c("incidence_100000_pys", "outcome_count", "denominator_count", "person_days"))

  resultTidy <- result |>
    tidyIncidence() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_date"),
        .fns = ~ toDate(.x)
      ),
      dplyr::across(
        .cols = dplyr::contains("time|washout|days"),
        .fns = ~ as.numeric(.x)
      )
    )

  plotEstimates(
    result = resultTidy,
    x = x,
    y = y,
    ylim = ylim,
    ribbon = ribbon,
    facet = facet,
    colour = colour,
    options = options
  )
}

#' Plot prevalence results
#'
#' @param result Prevalence results
#' @param x  Variable to plot on x axis
#' @param y Variable to plot on y axis. Options are: "prevalence",
#' "denominator_count", "outcome_count"
#' @param ylim Limits for the Y axis
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param options A list of optional plot options. See optionsPlot() for the
#' default parameters.
#'
#' @return A ggplot with the prevalence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalence(sampleSize = 1000)
#' cdm <- generateDenominatorCohortSet(
#'   cdm = cdm, name = "denominator",
#'   cohortDateRange = c(as.Date("2014-01-01"), as.Date("2018-01-01"))
#' )
#' prev <- estimatePointPrevalence(
#'   cdm = cdm,
#'   denominatorTable = "denominator",
#'   outcomeTable = "outcome"
#' )
#' plotPrevalence(prev)
#' }
plotPrevalence <- function(result,
                           x = "prevalence_start_date",
                           y = "prevalence",
                           ylim = c(0, NA),
                           ribbon = FALSE,
                           facet = NULL,
                           colour = NULL,
                           options = list()) {

  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyPlot())
  }

  rlang::check_installed("visOmopResults", version = "0.5.0")

  result <- omopgenerics::validateResultArgument(result) |>
    omopgenerics::filterSettings(
      .data$result_type == "prevalence"
    )

  if (nrow(result) == 0) {
    cli::cli_warn("No incidence results available to plot")
    return(emptyPlot())
  }
  omopgenerics::assertChoice(y, c("prevalence", "denominator_count", "outcome_count"))

  resultTidy <- result |>
    tidyPrevalence() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::ends_with("_date"),
        .fns = ~ toDate(.x)
      ),
      dplyr::across(
        .cols = dplyr::contains("time|washout|days"),
        .fns = ~ as.numeric(.x)
      )
    )

  plotEstimates(
    result = resultTidy,
    x = x,
    y = y,
    ylim = ylim,
    ribbon = ribbon,
    facet = facet,
    colour = colour,
    options = options
  )
}



# helper functions

plotEstimates <- function(result,
                          x,
                          y,
                          ylim,
                          ribbon,
                          facet,
                          colour,
                          options) {

  rlang::check_installed("ggplot2", reason = "for plot functions")
  rlang::check_installed("scales", reason = "for plot functions")

  if (y %in% c("prevalence")) {
    ytype = "percentage"
  } else {
    ytype = "count"
  }

  options <- getOptions(options)

  if (!options$hideConfidenceInterval & (y %in% c("incidence_100000_pys", "prevalence"))) {
    ymin <- paste0(y, "_95CI_lower")
    ymax <- paste0(y, "_95CI_upper")
  } else {
    ymin <- NULL
    ymax <- NULL
  }

  labels = c("denominator_cohort_name", "outcome_cohort_name", "denominator_count", "outcome_count", "person_days", y, ymin, ymax,
             "incidence_start_date", "incidence_end_date", "prevalence_start_date", "prevalence_end_date")
  labels <- labels[labels %in% colnames(result)]
  plot <- visOmopResults::scatterPlot(
    result = result,
    x = x,
    y = y,
    line = options$line,
    point = options$point,
    ribbon = ribbon,
    ymin = ymin,
    ymax = ymax,
    facet = facet,
    colour = colour,
    group = colour,
    label = labels
  )

  if (is.null(ylim)) {
    if (ytype == "count") {
      plot <- plot +
        ggplot2::scale_y_continuous(labels = scales::comma)
    }
    if (ytype == "percentage") {
      plot <- plot +
        ggplot2::scale_y_continuous(
          labels =
            scales::percent_format(accuracy = 0.1)
        )
    }
  } else {
    plot <- addYLimits(plot = plot, ylim = ylim, ytype = ytype)
  }

  if (!is.null(facet) & (!is.null(options$facetNcols) | is.null(options$facetScales))) {
    facetOpt <- list("facets" = facet, "ncol" = options$facetNcols, "scales" = options$facetScales)
    plot <- plot + do.call(ggplot2::facet_wrap, facetOpt)
  }

  if (plot$labels$y == "Incidence 100000 pys") {
    plot <- plot + ggplot2::labs(y = "Incidence (100,000 person-years)")
  }

  return(plot + visOmopResults::themeVisOmop())
}

addYLimits <- function(plot, ylim, ytype) {
  if (ytype == "count") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        limits = ylim
      )
  }
  if (ytype == "percentage") {
    plot <- plot +
      ggplot2::scale_y_continuous(
        labels =
          scales::percent_format(accuracy = 0.1),
        limits = ylim
      )
  }
  return(plot)
}

emptyPlot <- function(title = "No result to plot",
                      subtitle = "") {
  ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    )
}

getOptions <- function(options) {
  default <- list(
    "hideConfidenceInterval" = FALSE,
    "line" = FALSE,
    "point" = TRUE,
    "facetNcols" = NULL,
    "facetScales" = "fixed"
  )
  if (is.null(options)) {return(default)}
  for (opt in names(options)) {
    default[[opt]] <- options[[opt]]
  }
  return(default)
}

#' List of parameter options for IncidencePrevalence plot functions.
#'
#' @description
#' Options are:
#'
#'
#' **hideConfidenceInterval:** Logical. Whether to show confidence intervals
#'
#'
#' **line:** Logical. Whether to plot a line using `geom_line`
#'
#'
#' **point:** Logical. Whether to plot points using `geom_point`
#'
#'
#' **facetNcols** -> Numeric. Number of facet columns
#'
#'
#' **facetScales** -> Character. Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#'
#'
#' @return List of available parameters and their default values.
#' @export
#'
#' @examples
#' \donttest{
#' optionsPlot()
#' }
optionsPlot <- function() {
  getOptions(NULL)
}

tidyIncidence <- function(result) {
  result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    dplyr::select(!c("variable_name", "variable_level")) |>
    omopgenerics::pivotEstimates()
}

tidyPrevalence <- function(result) {

result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
  dplyr::select(!c("variable_name", "variable_level")) |>
  omopgenerics::pivotEstimates()

}

toDate <- function(x) {
  tryCatch(
    expr = {
      x <- as.Date(x)
    },
    error = function(e) e,
    finally = return(x)
  )
}


#' Plot incidence results
#'
#' @param result Incidence results
#' @param x Variable to plot on x axis
#' @param ylim Limits for the Y axis
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param colour_name Colour legend name
#' @param options a list of optional plot options
#'
#' @return A ggplot with the incidence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
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
                          ylim = c(0, NA),
                          ribbon = FALSE,
                          facet = NULL,
                          colour = NULL,
                          colour_name = NULL,
                          options = list()) {
  plotEstimates(
    result = result,
    x = x,
    y = "incidence_100000_pys",
    ylim = ylim,
    ytype = "count",
    ribbon = ribbon,
    facet = facet,
    colour = colour,
    colour_name = colour_name,
    options = options
  )
}

#' Plot prevalence results
#'
#' @param result Prevalence results
#' @param x  Variable to plot on x axis
#' @param ylim Limits for the Y axis
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param colour_name Colour legend name
#' @param options a list of optional plot options
#'
#' @return A ggplot with the prevalence results plotted
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
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
                           ylim = c(0, NA),
                           ribbon = FALSE,
                           facet = NULL,
                           colour = NULL,
                           colour_name = NULL,
                           options = list()) {
  plotEstimates(
    result = result,
    x = x,
    y = "prevalence",
    ylim = ylim,
    ytype = "percentage",
    ribbon = ribbon,
    facet = facet,
    colour = colour,
    colour_name = colour_name,
    options = options
  )
}



# helper functions

plotEstimates <- function(result,
                          x,
                          y,
                          ylim,
                          ytype,
                          ribbon,
                          facet,
                          colour,
                          colour_name,
                          options) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(result, "IncidencePrevalenceResult"))
  checkmate::assertTRUE(all(c(x, y) %in% colnames(result)))
  checkmate::assertList(options, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  plot_data <- getPlotData(
    estimates = result,
    facetVars = facet,
    colourVars = colour
  )

  if (is.null(colour)) {
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = !!rlang::sym(x),
          y = !!rlang::sym(y)
        )
      )
  } else {
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = !!rlang::sym(x),
          y = !!rlang::sym(y),
          group = .data$colour_vars,
          colour = .data$colour_vars,
          fill = .data$colour_vars
        )
      ) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(
        fill = colour_name,
        colour = colour_name
      )
  }

  hideConfidenceInterval <- "hideConfidenceInterval" %in% names(options) &&
    options[["hideConfidenceInterval"]]
  yLower <- ifelse(hideConfidenceInterval, y, paste0(y, "_95CI_lower"))
  yUpper <- ifelse(hideConfidenceInterval, y, paste0(y, "_95CI_upper"))

  plot <- plot +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = !!rlang::sym(yLower),
        ymax = !!rlang::sym(yUpper)
      ),
      width = 0
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

  if (!is.null(facet)) {
    facetNcols <- NULL
    if ("facetNcols" %in% names(options)) {
      facetNcols <- options[["facetNcols"]]
    }
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var), ncol = facetNcols) +
      ggplot2::theme_bw()
  } else {
    plot <- plot +
      ggplot2::theme_minimal()
  }

  if (isTRUE(ribbon)) {
    plot <- addRibbon(plot = plot, yLower = yLower, yUpper = yUpper)
  }



  plot <- plot

  return(plot)
}


getPlotData <- function(estimates, facetVars, colourVars) {
  plotData <- estimates
  if (!is.null(facetVars)) {
    plotData <- plotData %>%
      tidyr::unite("facet_var",
        c(tidyselect::all_of(.env$facetVars)),
        remove = FALSE, sep = "; "
      )
  }
  if (!is.null(colourVars)) {
    plotData <- plotData %>%
      tidyr::unite("colour_vars",
        c(tidyselect::all_of(.env$colourVars)),
        remove = FALSE, sep = "; "
      )
  }

  return(plotData)
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

addRibbon <- function(plot, yLower, yUpper) {
  plot <- plot +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = !!rlang::sym(yLower),
        ymax = !!rlang::sym(yUpper)
      ),
      alpha = .3, color = NA, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.25)
}

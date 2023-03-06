#' Title
#'
#' @param error
#' @param frequency
#'
#' @return
get_risk_category <- function(error, frequency) {
    risk_grouping <- cut(
        abs(error),
        breaks = c(0, 0.02, 0.04, 0.06, Inf),
        include.lowest = TRUE,
        right = FALSE
    ) |>
        as_factor() |>
        as.numeric()

    freq_grouping <- cut(
        frequency,
        breaks = c(0, 4.4, 8.5, 15, Inf),
        include.lowest = TRUE,
        right = FALSE
    ) |>
        as_factor() |>
        as.numeric()

    score <- risk_grouping + freq_grouping

    score - 2
}

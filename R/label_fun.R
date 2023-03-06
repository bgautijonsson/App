#' Title
#'
#' @param data
#'
#' @return
#' @export
spatial_labels <- function(data, input) {

    result <- ifelse(
        round(data$err, 3) == 0,
        "<b>as expected</b>",
        ifelse(
            data$err < 0,
            str_c("<b>", percent(abs(data$err), accuracy = 0.1), " below</b>"),
            str_c("<b>", percent(abs(data$err), accuracy = 0.1)," above</b>")
        )
    )

    timing <- ifelse(
        data$n_err == 0,
        "has <b>never</b> happened since 2019-01-01",
        str_c(
            "has happened <b>", data$n_err, "</b> times since 2019-01-01",
            "<br>",
            "(appriximately every <b>", data$freq_err, "</b> weeks)"
        )
    )

    str_c(
        "<strong>", data$region, "</strong>",
        "<br>",
        "<em>Spending is ", result, " expected this week", "</em>",
        "<br>",
        "A deviation of this size ", timing,
        "<br>",
        "Based on numbers from <b>", data$n_user, "</b> individuals"
    ) |>
        lapply(HTML)
}

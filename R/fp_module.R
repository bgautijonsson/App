# UI ----------------------------------------------------------------------
fp_ui <- function(id) {
    sidebarLayout(

        # SIDEBAR PANEL -----------------------------------------------------------------
        sidebarPanel(
            dateInput(
                inputId = NS(id, "date"),
                label = "Date",
                value = "2022-11-20",
                min = "2019-01-06",
                max = "2023-02-19",
                weekstart = 0,
                daysofweekdisabled = 1:6
            ),
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Calculate",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info),
            img(src = "hagstofa.png", width = "100%"),
            img(src = "ec_logo.svg", width = "100%"),
            img(src = "fable_logo.svg", width = "100%"),
            img(src = "hi.png", width = "100%")
        ),

        # MAIN PANEL --------------------------------------------------------------
        mainPanel(
            plotOutput(NS(id, "fp_plot"), width = "100%", height = "800px") |> withSpinner()
        )
    )
}

# SERVER ------------------------------------------------------------------
fp_server <- function(id) {
    moduleServer(id, function(input, output, session) {


        output$fp_plot <- renderPlot({

                crossing(
                    risk_grouping = 1:4,
                    freq_grouping = 1:4
                ) |>
                mutate(
                    risk_category = risk_grouping + freq_grouping - 2
                ) |>
                ggplot(aes(risk_grouping, freq_grouping)) +
                geom_tile(aes(fill = risk_category)) +
                geom_text(
                    data = front_page |>
                        filter(
                            txn_date == input$date
                        ),
                    aes(x = risk_grouping, y = freq_grouping, label = "X"),
                    colour = "black", size = 30
                ) +
                scale_fill_distiller(palette = "RdBu") +
                coord_cartesian(expand = FALSE) +
                theme(
                    legend.position = "none",
                    text = element_text(size = 22)
                ) +
                labs(
                    x = "Severity",
                    y = "Likelihood"
                )



        }) |>
            bindEvent(
                input$goButton,
                ignoreNULL = FALSE
            )

    })
}

# UI ----------------------------------------------------------------------
country_ui <- function(id) {
    sidebarLayout(

        # SIDEBAR PANEL -----------------------------------------------------------------
        sidebarPanel(
            dateInput(
                inputId = NS(id, "date"),
                label = "Date",
                value = "2022-11-20",
                min = "2019-01-06",
                max = "2023-02-19",
                weekstart = 0
            ),
            selectInput(
                inputId = NS(id, "group"),
                label = "Aggregation",
                choices = unique(country$group),
                selected = "By Income",
                multiple = FALSE,
                selectize = FALSE
            ),
            selectInput(
                inputId = NS(id, "coicop"),
                label = "COICOP",
                choices = unique(country$coicop),
                selected = "01 Food and non-alcoholic beverages",
                multiple = F, selectize = F
            ),
            selectInput(
                inputId = NS(id, "type"),
                label = "Show",
                choices = unique(country$type),
                selected = "Weekly changes",
                multiple = F,
                selectize = F
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
            img(src = "ec_logo.svg", width = "100%"),
            img(src = "fable_logo.svg", width = "100%")
        ),

        # MAIN PANEL --------------------------------------------------------------
        mainPanel(
            h4("Difference between actual and expected spending"),
            plotlyOutput(NS(id, "ts_plot"), width = "100%", height = "1000px") |> withSpinner()
        )
    )
}

# SERVER ------------------------------------------------------------------
country_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        prepare_plot_data <- reactive({

            plot_dat <- country |>
                filter(
                    coicop == input$coicop,
                    type == input$type,
                    group == input$group,
                    txn_date >= input$date - weeks(8),
                    txn_date <= input$date
                )
        }) |>
            bindEvent(
                input$goButton,
                ignoreNULL = FALSE
            )

        output$ts_plot <- renderPlotly({

            plot_dat <- prepare_plot_data()

            (
                plot_dat |>
                    ggplot(aes(txn_date, 1 + err)) +
                    geom_hline(yintercept = 1, lty = 2, linewidth = 0.5, alpha = 0.5) +
                    geom_line(
                        data = plot_dat |>
                            rename(gr = group_val),
                        aes(group = gr),
                        col = "grey60",
                        linewidth = 0.5,
                        alpha = 0.5
                    ) +
                    geom_line(aes(group = group_val, col = group_val), linewidth = 1.5) +
                    scale_x_date(
                        labels = label_date_short()
                    ) +
                    scale_y_continuous(
                        labels = function(x) percent(x - 1),
                        trans = "log10",
                        breaks = breaks_log(7),
                        expand = expansion()
                    ) +
                    scale_colour_brewer(
                        palette = "Set1"
                    ) +
                    facet_wrap("group_val", ncol = 1, scales = "free_y") +
                    theme(legend.position = "none") +
                    labs(
                        x = NULL,
                        y = NULL
                    )
            ) |>
                ggplotly(
                ) |>
                layout(
                    xaxis = list(
                        rangeslider = list(
                            type = "date",
                            autorange = TRUE,
                            relayout = TRUE
                        )
                    )
                )

        }) |>
            bindEvent(
                input$goButton,
                ignoreNULL = FALSE
            )

    })
}

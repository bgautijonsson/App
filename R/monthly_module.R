# UI ----------------------------------------------------------------------
monthly_ui <- function(id) {
    sidebarLayout(

        # SIDEBAR PANEL -----------------------------------------------------------------
        sidebarPanel(
            dateInput(
                inputId = NS(id, "date"),
                label = "Date",
                value = "2022-11-01",
                min = "2019-01-01",
                max = "2023-02-01",
                weekstart = 0
            ),
            selectInput(
                inputId = NS(id, "coicop"),
                label = "COICOP",
                choices = unique(d$coicop),
                selected = "01 Food and non-alcoholic beverages",
                multiple = F, selectize = F
            ),
            div(
                actionButton(
                    inputId = NS(id, "goButton"),
                    label = "Calculate",
                    width = "120px"
                ),
                class = "center", align = "middle"
            ),
            HTML(sidebar_info)
        ),

        # MAIN PANEL --------------------------------------------------------------
        mainPanel(
            leafletOutput(NS(id, "spatial_plot"), width = "100%", height = "800px") |> withSpinner()
        )
    )
}

# SERVER ------------------------------------------------------------------
monthly_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        prepare_plot_data <- reactive({

            d <- monthly

            plot_dates <- floor_date(input$date, "month") - months(c(0, 1))

            plot_dat <- d |>
                filter(
                    coicop == input$coicop,
                    txn_date %in% plot_dates
                ) |>
                collect() |>
                inner_join(
                    sf_centroid_coords,
                    by = join_by(nuts2 == nuts)
                )  |>
                group_by(txn_date) |>
                mutate(
                    # time = as.factor(txn_date),
                    pred = gam(median_spend_per_user ~ te(x, y),
                               link = "log",
                               weight = n_user,
                               method = "REML") |> predict() |> as.numeric()
                ) |>
                ungroup() |>
                group_by(nuts2) |>
                summarise(
                    diff = exp(diff(log(pred))) - 1,
                    n_user = min(n_user)
                )

            map_dat <- de_sf |>
                inner_join(
                    plot_dat,
                    by = join_by(nuts == nuts2)
                ) |>
                st_make_valid()

            map_dat
        })

        output$spatial_plot <- renderLeaflet({

            dat <- prepare_plot_data()

            min_diff <- min(dat$diff)
            max_diff <- max(dat$diff)
            max_abs <- max(abs(c(min_diff, max_diff)))
            range <- c(-1, 1) * max_abs

            # Color palette for Leaflet
            pal <- colorNumeric(
                "RdBu",
                domain = range,
                reverse = TRUE
            )

            req(dat)
            labels <- spatial_labels(dat)


            dat |>
                mutate(
                    legend_numbers = seq(range[1], range[2], length.out = n())
                ) |>
                leaflet() |>
                addProviderTiles(providers$MapBox) |>
                addPolygons(
                    stroke = FALSE,
                    fillOpacity = 0.7,
                    opacity = 1,
                    weight = 6,
                    color = "white",
                    dashArray = "3",
                    fillColor = ~ pal(diff),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"
                    ),
                    highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "1",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    )
                ) |>
                addLegendNumeric(
                    title = "% change",
                    pal = pal,
                    values = ~ legend_numbers,
                    fillOpacity = 1,
                    height = 250,
                    width = 30,
                    bins = 7,
                    numberFormat = label_percent(accuracy = 0.1),
                    decreasing = TRUE
                )

        }) |>
            bindEvent(
                input$goButton,
                ignoreNULL = TRUE
            )

    })
}

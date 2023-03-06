# UI ----------------------------------------------------------------------
nuts2_ui <- function(id) {
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
            selectInput(
                inputId = NS(id, "coicop"),
                label = "COICOP",
                choices = unique(nuts2$coicop),
                selected = "01 Food and non-alcoholic beverages",
                multiple = F, selectize = F
            ),
            selectInput(
                inputId = NS(id, "type"),
                label = "Show",
                choices = unique(nuts2$type),
                selected = "Weekly discrepancy",
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
            leafletOutput(NS(id, "spatial_plot"), width = "100%", height = "800px") |> withSpinner()
        )
    )
}

# SERVER ------------------------------------------------------------------
nuts2_server <- function(id) {
    moduleServer(id, function(input, output, session) {

        prepare_plot_data <- reactive({

            plot_dat <- nuts2 |>
                filter(
                    coicop == input$coicop,
                    txn_date == input$date,
                    type == input$type
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

            min_err <- min(dat$err)
            max_err <- max(dat$err)
            max_abs <- max(abs(c(min_err, max_err)))
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
                    fillOpacity = 0.7,
                    opacity = 1,
                    stroke = 0.1,
                    weight = 0.1,
                    color = "white",
                    dashArray = "3",
                    fillColor = ~ pal(err),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"
                    ),
                    highlightOptions = highlightOptions(
                        stroke = 1,
                        weight = 4,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 1,
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
                ignoreNULL = FALSE
            )

    })
}

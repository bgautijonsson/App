ui <- shinyUI(
    navbarPage(
        title = "Consumer Spending and Economic Monitoring Engine",
        theme = bs_global_get(),


        tabPanel(
            title = "Risk Assessment",
            fp_ui("fp")
        ),

        tabPanel(
            title = "Country",
            country_ui("country")
        ),

        tabPanel(
            title = "NUTS2",
            nuts2_ui("nuts2")
        ),


    )
)

# PACKAGES ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(mgcv)
library(shinycssloaders)
library(eurostat)
library(sf)
library(leaflet)
library(leaflegend)
library(scales)
library(plotly)
library(arrow)
library(metill)

shinyOptions(plot.autocolor = TRUE)

# DATA --------------------------------------------------------------------
nuts2 <- read_parquet("Data/nuts2.parquet")
country <- read_parquet("Data/country.parquet")
front_page <- read_parquet("Data/front_page.parquet")



de_sf <- get_eurostat_geospatial(
    cache_dir = here::here("Data", "Eurostat_Spatial"),
    update_cache = TRUE,
    year = "2016",
    nuts_level = "3"
) |>
    janitor::clean_names() |>
    filter(
        cntr_code == "DE"
    ) |>
    st_make_valid() |>
    select(nuts_name, nuts_id) |>
    mutate(
        nuts = str_sub(nuts_id, 1, 4)
    ) |>
    group_by(nuts) |>
    summarise(n_nuts = n()) |>
    ungroup() |>
    st_make_valid()

sf_centroid_coords <- de_sf |>
    st_centroid() |>
    st_coordinates() |>
    as_tibble() |>
    rename(x = X, y = Y) |>
    mutate(
        nuts = de_sf$nuts
    )



# THEME -------------------------------------------------------------------
theme_set(theme_metill())



bs_global_theme(
    bootswatch = "flatly"
)

bs_global_add_variables(
    primary = "#484D6D",
    secondary = "#969696",
    success = "#969696",
    # danger = "#FF8CC6",
    # info = "#FF8CC6",
    light = "#faf9f9",
    dark = "#484D6D",
    bg = "#faf9f9",
    fg = "#737373",
    "body-bg" = "#faf9f9",
    base_font = "Lato",
    heading_font = "Segoe UI",
    "navbar-brand-font-family" = "Playfair Display",
    code_font = "SFMono-Regular"
)

# SIDEBAR INFO ------------------------------------------------------------

sidebar_info <- paste0(
)

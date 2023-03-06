nuts2 <- tbl(con, "NUTS2_GAMS_ERRORS") |>
    collect() |>
    arrange(txn_date, nuts2, coicop) |>
    mutate(
        nuts2 = as_factor(nuts2),
        coicop_num = parse_number(coicop)
    ) |>
    arrange(txn_date, nuts2, coicop_num) |>
    mutate(
        risk_category = get_risk_category(err, freq_err),
        type = ifelse(type == "Weekly changes", "Weekly discrepancy", type)
    )


nuts2 |>
    write_parquet(
        here::here("App", "Data", "nuts2.parquet")
    )



country <- bind_rows(
    tbl(con, "GAM_SPENDING_USER_ERROR") |>
        collect() |>
        mutate(
            group = "Whole Country",
            group_val = "Whole Country"
        ),
    tbl(con, "GAM_SPENDING_USER_ERROR_BY_AGE") |>
        collect() |>
        mutate(
            group = "By Age"
        ) |>
        rename(group_val = age_group),
    tbl(con, "GAM_SPENDING_USER_ERROR_BY_INCOME") |>
        collect() |>
        mutate(
            group = "By Income"
        ) |>
        rename(group_val = income_band)
) |>
    mutate(
        group_val = as_factor(group_val) |>
            fct_relevel(
                "Whole Country",
                "LOW",
                "MID",
                "HIGH",
                "20-29",
                "30-39",
                "40-49",
                "50-59",
                "60-69"
            )
    ) |>
    filter(
        group != "Whole Country"
    )

country |>
    write_parquet(
        here::here("App", "Data", "country.parquet")
    )



front_page <- nuts2 |>
    filter(
        parse_number(coicop) %in% c(
            1, 3, 4, 6, 7, 9, 11
        ),
        txn_date < date_build(2023, 02, 01),
        txn_date > date_build(2019, 2, 1),
        type == "Weekly discrepancy"
    ) |>
    mutate(
        err = log(1 + err)
    ) |>
    summarise(
        freq = mean(freq_err, na.rm = T),
        risk = mean(err),
        .by = txn_date
    ) |>
    mutate(
        risk_grouping = cut(
            abs(risk),
            breaks = c(0, 0.02, 0.04, 0.06, Inf),
            include.lowest = TRUE,
            right = FALSE
        ) |>
            as_factor() |>
            as.numeric(),
        freq_grouping = cut(
            freq,
            breaks = c(0, 4.4, 8.5, 15, Inf),
            include.lowest = TRUE,
            right = FALSE
        ) |>
            as_factor() |>
            as.numeric(),
        risk_category = risk_grouping + freq_grouping - 2
    ) |>
    select(txn_date, risk_grouping, freq_grouping, risk_category)


front_page |>
    write_parquet(
        here::here("App", "Data", "front_page.parquet")
    )

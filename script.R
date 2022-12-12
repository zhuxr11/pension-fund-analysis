rm(list = ls())
gc()

library(tidyverse)
library(abind)
library(tseries)
source("functions.R")

fund_metadata <- readRDS("fund_metadata.rds")

fund_data <- get_fund_val(token = rownames(fund_metadata),
                          start_date = "2001-01-01",
                          end_date = Sys.Date(),
                          batch_size = 49L,
                          id_colname = "token_ref")
saveRDS(fund_data, "fund_data.rds")

# Compute daily Sharpe ratio
ref_daily_rate <- 2.31/250
ref_date <- lubridate::as_date("2022-12-03")
fund_res <- purrr::map(
  list("3 months" = lubridate::period(3, units = "month"),
       "6 months" = lubridate::period(6, units = "month"),
       "1 year" = lubridate::period(1, units = "year"),
       "2 years" = lubridate::period(2, units = "year"),
       "3 years" = lubridate::period(3, units = "year"),
       # Additional time periods go here
       "All time" = lubridate::as.period(difftime(ref_date, "0-1-1"))),
  function(time_period) {
    fund_data %>%
      tidyr::drop_na(`净值日期`, `单位净值`, `累计净值`, `日增长率`) %>%
      dplyr::filter(nchar(`日增长率`) > 0L) %>%
      dplyr::mutate(value_date = lubridate::as_date(`净值日期`),
                    daily_rate = as.numeric(stringr::str_replace(`日增长率`, "%$", ""))) %>%
      dplyr::group_by(token_ref) %>%
      dplyr::mutate(full_cover = any(value_date <= ref_date - time_period) |
                      time_period >= lubridate::as.period(difftime(ref_date, "0-1-1"))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(value_date > ref_date - time_period, value_date <= ref_date) %>%
      dplyr::group_by(token_ref) %>%
      dplyr::arrange(value_date, .by_group = TRUE) %>%
      dplyr::summarize(
        full_cover = unique(full_cover),
        `Record count` = ifelse(
          full_cover == TRUE,
          dplyr::n(),
          NA_integer_
        ),
        `Annual return` = ifelse(
          full_cover == TRUE,
          (dplyr::last(`累计净值`) - dplyr::first(`累计净值`)) /
            as.numeric(difftime(dplyr::last(value_date), dplyr::first(value_date), units = "days")) * 250,
          NA_real_
        ),
        `Max drawdown` = ifelse(
          full_cover == TRUE,
          tseries::maxdrawdown(`累计净值`)[["maxdrawdown"]],
          NA_real_
        ),
        `Sharpe ratio` = ifelse(
          full_cover == TRUE,
          tseries::sharpe(`累计净值`, r = ref_daily_rate/100, scale = sqrt(250)),
          NA_real_
        ),
        # Additional metrics go here
        .groups = "drop"
      ) %>%
      dplyr::select(-full_cover) %>%
      tibble::column_to_rownames("token_ref") %>%
      as.matrix()
  }
) %>%
  abind::abind(along = 1.5, force.array = TRUE)
names(dimnames(fund_res)) <- c("Fund", "Period", "Metric")
saveRDS(fund_res, "fund_res.rds")

rm(list = ls())
gc()

library(tidyverse)
library(unglue)

# Load all fund metadata
all_fund_metadata <- readLines("http://fund.eastmoney.com/js/fundcode_search.js",
                               encoding = "UTF-8", warn = FALSE) %>%
  stringr::str_extract("\\[{2}.*\\]{2}") %>%
  jsonlite::parse_json() %>%
  # Set column names
  purrr::map(purrr::set_names, c("Token", "Abbrev.", "Name", "Type", "English name")) %>%
  dplyr::bind_rows()

# Load input file of pension funds (this may vary according to input formats)
fund_metadata_raw <- readr::read_lines(file.path("lookup", "pension_funds_20221203.txt"),
                                       locale = readr::locale(encoding = "GBK")) %>%
  # Remove comment lines
  stringr::str_subset(pattern = "^# ", negate = TRUE)
# Split lines into funds
fund_start_row_idx <- which(stringr::str_detect(fund_metadata_raw, "^产品状态") == TRUE) + 1L
# Define a helper function to match fund names across different shares
.fund_names_for_match <- function(x) {
  x %>%
    stringr::str_replace("[A-Z]$", "") %>%
    stringr::str_replace(".?年持有混合(发起式)?", "")
}
# Parse fund metadata
fund_metadata <- split(
  fund_metadata_raw,
  rep(seq_along(fund_start_row_idx),
      fund_start_row_idx - dplyr::lag(fund_start_row_idx, default = 1L))
) %>%
  purrr::map(paste, collapse = "") %>%
  unglue::unglue_data(
    patterns = c("{Name}产品代码：{Token}产品类型：{Type}产品状态：{Status}",
                 "{Name}产品代码：{Token}产品状态：{Status}")
  ) %>%
  # Match each fund to its first established share
  dplyr::rowwise() %>%
  dplyr::mutate(
    ref_idx = {
      ref_name <- all_fund_metadata %>%
        dplyr::filter(Token %in% .env[["Token"]] == TRUE) %>%
        dplyr::pull("Name")
      stopifnot(length(ref_name) == 1L)
      list(
        which(.fund_names_for_match(all_fund_metadata[["Name"]]) %in%
                .fund_names_for_match(ref_name) == TRUE)
      )
    },
    token_ref = all_fund_metadata[["Token"]][dplyr::first(ref_idx)]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-ref_idx) %>%
  tibble::column_to_rownames("token_ref")

saveRDS(fund_metadata, "fund_metadata.rds")

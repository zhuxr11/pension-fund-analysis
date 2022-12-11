get_fund_val <- function(token,
                         start_date = Sys.Date() - 6L,
                         end_date = Sys.Date(),
                         batch_size = 20L,
                         id_colname = if (length(token) > 1) "token" else NULL,
                         threads = 1L,
                         verbose = TRUE) {
  # Validate inputs
  stopifnot(is.character(token) == TRUE, all(is.na(token) == FALSE))
  stopifnot(is.numeric(batch_size) == TRUE, length(batch_size) == 1L,
            is.na(batch_size) == FALSE, batch_size == round(batch_size),
            batch_size > 0L)
  if (is.null(id_colname) == FALSE) {
    stopifnot(is.character(id_colname) == TRUE, length(id_colname) == 1L,
              is.na(id_colname) == FALSE)
  }
  stopifnot(is.numeric(threads) == TRUE, length(threads) == 1L,
            is.na(threads) == FALSE, threads == round(threads),
            threads > 0L)
  stopifnot(is.logical(verbose) == TRUE, length(verbose) == 1L,
            is.na(verbose) == FALSE)
  # Vectorize core function
  fun_args <- list(token = token,
                   start_date = start_date,
                   end_date = end_date,
                   MoreArgs = list(batch_size = batch_size,
                                   verbose = verbose),
                   SIMPLIFY = FALSE,
                   USE.NAMES = FALSE)
  if (threads == 1L) {
    res <- rlang::inject(
      mapply(FUN = .get_fund_val,
             !!!fun_args)
    )
  } else {
    if (.Platform[["OS.type"]] %in% "unix" == TRUE) {
      res <- rlang::inject(
        parallel::mcmapply(FUN = .get_fund_val,
                           mc.cores = threads,
                           !!!fun_args)
      )
    } else {
      cl <- parallel::makePSOCKcluster(threads)
      parallel::clusterExport(cl = cl,
                              varlist = c(".get_fund_val", ".get_val_content"))
      parallel::clusterEvalQ(cl = cl,
                             expr = library(tidyverse))
      res <- rlang::inject(
        parallel::clusterMap(cl = cl,
                             fun = .get_fund_val,
                             !!!fun_args)
      )
    }
  }
  dplyr::bind_rows(purrr::set_names(res, token), .id = id_colname)
}

.get_fund_val <- function(token,
                          start_date = Sys.Date() - 6L,
                          end_date = Sys.Date(),
                          batch_size = 20L,
                          verbose = TRUE) {
  if (verbose == TRUE) {
    message("* [", Sys.time(), "] Processing token: ", token, "...")
  }
  fund_page <- .get_val_content(
    data_type = "lsjz",
    token = token,
    page_idx = 1L,
    start_date = start_date,
    end_date = end_date,
    batch_size = batch_size
  )
  page_count <- stringr::str_extract(fund_page, "(?<=pages:)[0-9]+(?=,)") %>%
    as.integer()
  stopifnot(is.na(page_count) == FALSE)
  row_count <- stringr::str_extract(fund_page, "(?<=records:)[0-9]+(?=,)") %>%
    as.integer()
  stopifnot(is.na(row_count) == FALSE)
  if (verbose == TRUE) {
    message("** [", Sys.time(), "] Total records: ", row_count)
  }
  if (page_count > 1L) {
    if (verbose == TRUE) {
      pb <- progress::progress_bar$new(
        total = page_count,
        format = "Loading content from page :current/:total [:bar] :percent  ~ :eta remaining",
        show_after = 0
      )$tick()
    } else {
      pb <- NULL
    }
    fund_page <- c(
      fund_page,
      .get_val_content(
        data_type = "lsjz",
        token = token,
        page_idx = seq(2L, page_count, by = 1L),
        start_date = start_date,
        end_date = end_date,
        batch_size = batch_size,
        pb = pb
      )
    )
  }
  table_content <- fund_page %>%
    stringr::str_extract("\\<table .*\\</table\\>") %>%
    purrr::map(~ {
      .x %>%
        xml2::read_html() %>%
        rvest::html_table()
    }) %>%
    dplyr::bind_rows()
  stopifnot(nrow(table_content) == row_count)
  table_content
}

.get_val_content <- function(data_type, token, page_idx, start_date, end_date, batch_size, retry = 20L, pb = NULL) {
  res <- glue::glue(
    "http://fund.eastmoney.com/f10/F10DataApi.aspx?type={data_type}&code={token}&page={page_idx}&sdate={start_date}&edate={end_date}&per={batch_size}",
    data_type = data_type,
    token = token,
    page_idx = page_idx,
    start_date = start_date,
    end_date = end_date,
    batch_size = batch_size
  ) %>%
    purrr::imap_chr(~ {
      cur_retry <- 0L
      while (TRUE) {
        res <- tryCatch(
          readLines(.x, encoding = "UTF-8", warn = FALSE),
          error = function(err) err
        )
        if (is(res, "error") == FALSE) break
        cur_retry <- cur_retry + 1L
        if (cur_retry > retry) {
          stop("Error after ", retry, " ", if (retry == 1L) "retry" else "retries",
               ": ", conditionMessage(res))
        }
      }
      if (is.null(pb) == FALSE) pb$tick()
      res
    })
}

get_key <- function() {
  out <- Sys.getenv("RESCUETIME_KEY", unset = NA)
  if (is.na(out))
    stop("Please ensure the RESCUETIME_KEY environment variable is configured in .Renviron")
  out
}

url_api <- "https://www.rescuetime.com/anapi"
url_oauth <- "https://www.rescuetime.com/api/oauth"
urls <- c(summary = "daily_summary_feed",
  analytic = "data",
  alerts = "alerts_feed",
  highlights = "highlights_feed",
  highlights_post = "highlights_post",
  start_focustime = "start_focustime",
  end_focustime = "end_focustime",
  focustime_started = "focustime_started_feed",
  focustime_ended = "focustime_ended_feed",
  offline = "offline_time_post") |>
  purrr::map( ~ {
    .url <- httr::parse_url(url_api)
    .url <- purrr::list_modify(.url, path = c(.url$path, .x))
  })


analytic <-
  function (format = c("csv", "json"),
            perspective = c("rank",
                            "interval"),
            resolution_time = c("minute", "hour", "day", "week",  "month"),
            restrict_begin = lubridate::floor_date(Sys.Date(), "week"),
            restrict_end = Sys.Date(),
            restrict_kind = c("overview",
                              "category",
                              "activity",
                              "productivity",
                              "document",
                              "efficiency"),
            restrict_thing = NULL,
            restrict_thingy = NULL,
            restrict_source_type = c("computers",
                                     "mobile", "offline"),
            restrict_schedule_id = NULL,
            key = get_key())  {

    .e <- environment()
    .fmls <- rlang::fn_fmls()
    params <- rlang::env_get_list(.e, nms = rlang::fn_fmls_names()) |>
      purrr::imap(~{

        if (is.character(.x)) rlang::exec(UU::match_letters,.x[[1]], !!!eval(.fmls[[.y]], envir = .e), n = 2) else .x
        })
    .url <- urls[[match.call()[[1]]]]
    .url$query <- params
    req <- httr::GET(httr::build_url(.url))
    pretty_content(req)
  }

pretty_content <- function(request) {
  if (request$status_code != 200)
    rlang::warn(glue::glue("{request$url}:\nStatus code:{request$status_code}\nMessage:{httr::content(request)}"))

  httr::content(request)
}

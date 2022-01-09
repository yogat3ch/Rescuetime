get_key <- function() {
  out <- Sys.getenv("RESCUETIME_KEY", unset = NA)
  if (is.na(out))
    stop("Please ensure the RESCUETIME_KEY environment variable is configured in .Renviron")
  out
}

url_api <- "https://www.rescuetime.com/anapi"
url_oauth <- "https://www.rescuetime.com/api/oauth"
urls <- c(daily_summary = "daily_summary_feed",
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
    .url <- httr::modify_url(url_api, path = c(.url$path, .x))
  })

q_fn <- function(x) switch(tail(as.character(call_type), 1), daily_summary = ,
               analytic = ,
               alerts = ,
               highlights = ,
               focustime_started = ,
               focustime_ended = httr::GET,
               highlights_post = ,
               start_focustime = ,
               end_focustime = ,
               offline = httr::POST)

arg_match <- function(.x, .y, .fmls, env = rlang::caller_env()) {
  if (is.character(.x) && .y != "key")
    rlang::exec(UU::match_letters,.x[[1]], !!!eval(.fmls[[.y]], envir = env), n = 2)
  else
    .x
}



redact_url <- function(url) stringr::str_replace(url, "(?<=key\\=)\\w+", "[REDACTED]")

pretty_content <- function(request) {
  if (request$status_code != 200)
    rlang::warn(glue::glue("{redact_url(request$url)}:\nStatus code:{request$status_code}\nMessage:{httr::content(request)}"))

  httr::content(request)
}

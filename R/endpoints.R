#' @title Analytic API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#analytic-api-reference}{Analytic API}
#' @param format  'csv' | 'json'
#' @param perspective Consider this the X - axis of the returned data. It is what determines how your data is crunched serverside for ordered return.
#' \itemize{
#'   \item{\code{rank}}{: **(default)** Organized around a calculated value, usually a sum like time spent.}
#'   \item{\code{interval}}{: Organized around calendar time.}
#' }
#' @param resolution_time Default is "hour". In an interval report, the X axis unit. In other words, data is summarizd into chunks of this size. "minute" will return data grouped into five-minute buckets, which is the most granular view available.
#' @param restrict_begin Sets the start day for data batch, inclusive (always at time 00:00, start hour/minute not supported) Format ISO 8601 "YYYY-MM-DD"
#' @param restrict_end Sets the end day for data batch, inclusive (always at time 00:00, end hour/minute not supported) Format ISO 8601 "YYYY-MM-DD"
#' @param restrict_kind Allows you to preprocess data through different statistical engines. The perspective dictates the main grouping of the data, this provides different aspects of that main grouping.
#' \itemize{
#'   \item{\code{overview}}{: sums statistics for all activities into their top level category}
#'   \item{\code{category}}{: sums statistics for all activities into their sub category}
#'   \item{\code{activity}}{: sums statistics for individual applications / web sites / activities}
#'   \item{\code{productivity}}{: productivity calculation}
#'   \item{\code{efficiency}}{: efficiency calculation (not applicable in "rank" perspective)}
#'   \item{\code{document}}{: sums statistics for individual documents and web pages}
#' }
#' @param restrict_thing The name of a specific overview, category, application or website. For websites, use the domain component only if it starts with "www", eg. "www.nytimes.com" would be "nytimes.com".
# The easiest way to see what name you should be using is to retrieve a list that contains the name you want, and inspect it for the exact names.
#' @param restrict_thingy Refers to the specific "document" or "activity" we record for the currently active application, if supported. For example, the document name active when using Microsoft Word. Available for most major applications and web sites. Let us know if yours is not.
#' @param restrict_source_type Allows for querying by source device type.
#' @param restrict_schedule_id Allows for filtering results by schedule.
#' @param key Your API key (retrieved automatically if stored in *.Renviron*)  OR `access_token` - the access token from the Oauth2 Connection
#'
#' @return \code{(data.frame/list)}
#' @export

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
    query <-rlang::env_get_list(.e, nms = rlang::fn_fmls_names()) |>
      purrr::imap(~{
        if (is.character(.x)) rlang::exec(UU::match_letters,.x[[1]], !!!eval(.fmls[[.y]], envir = .e), n = 2) else .x
      })

    call_type <- tail(as.character(match.call()[[1]]), 1)
    req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
    pretty_content(req)
  }

#' @title Daily Summary Feed API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#daily-summary-feed-reference}{Daily Summary Feed API}
#' @inheritParams analytic
#' @return \code{(list)}
#' @export

daily_summary <- function (key = get_key()) {
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls, env = .e)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
  pretty_content(req)
}

#' @title Alerts Feed API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#alerts-feed-reference}{Alerts Feed API}
#' @inherit daily_summary params return
#' @export

alerts <-
  function (op = c("status", "list"), alert_id = NULL, key = get_key())
  {
    .e <- environment()
    .fmls <- rlang::fn_fmls()
    query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                          arg_match, .fmls = .fmls)
    call_type <- tail(as.character(match.call()[[1]]), 1)
    req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
    pretty_content(req)
  }

#' @title Daily Highlights Feed API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#highlights-feed-reference}{Daily Highlights Feed API}
#' @inherit daily_summary params return
#' @export

highlights <-
  function (key = get_key())
  {
    .e <- environment()
    .fmls <- rlang::fn_fmls()
    query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                          arg_match, .fmls = .fmls)
    call_type <- tail(as.character(match.call()[[1]]), 1)
    req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
    pretty_content(req)
  }

#' @title Daily Highlights Post API
#' @description POST data to the \href{https://www.rescuetime.com/anapi/setup/documentation#highlights-post-reference}{Daily Highlights Post API}
#' @inheritParams daily_summary
#' @return No warning if successful
#' @export
highlights_post <-
  function (key = get_key())
  {
    .e <- environment()
    .fmls <- rlang::fn_fmls()
    query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                          arg_match, .fmls = .fmls)
    call_type <- tail(as.character(match.call()[[1]]), 1)
    req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
    pretty_content(req)
  }

#' @title FocusTime Trigger API
#' @description POST data to the \href{https://www.rescuetime.com/anapi/setup/documentation#focustime-trigger-reference}{FocusTime Trigger API}
#' @inherit highlights_post params return
#' @export
start_focustime <- function (key = get_key())
{
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
  pretty_content(req)
}
#' @title FocusTime Trigger API
#' @description POST data to the \href{https://www.rescuetime.com/anapi/setup/documentation#focustime-trigger-reference}{FocusTime Trigger API}
#' @inherit highlights_post params return
#' @export
end_focustime <- function (key = get_key())
{
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
  pretty_content(req)
}

#' @title FocusTime Feed API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#focustime-feed-reference}{FocusTime Feed API}
#' @inherit daily_summary params return
#' @export
focustime_started <- function (key = get_key())
{
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
  pretty_content(req)
}

#' @title FocusTime Feed API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#focustime-feed-reference}{FocusTime Feed API}
#' @inherit daily_summary params return
#' @export
focustime_ended <- function (key = get_key())
{
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = query))
  pretty_content(req)
}


#' @title Offline Time POST API
#' @description GET data from the \href{https://www.rescuetime.com/anapi/setup/documentation#offline-time-reference}{Offline Time POST API}
#' @param start_time - A string representing the date/time the for the start of the offline time block. This should be in the format of ‘YYYY-MM-DD HH:MM:SS’, but a unix timestamp is also acceptable.
#' @param end_time - (or duration) Either a string representing the date/time the for the end of the offline time block, OR an integer representing the duration of the offline time block in minutes.
#' @param activity_name - A 255 character or shorter string containing the text that will be entered as the name of the activity (e.g. "Meeting", "Driving", "Sleeping", etc).
#' @param activity_details - *Optional* A 255 character or shorter string containing the text that will be entered as the details of the named activity.
#' @inherit highlights_post return
#' @export

offline <- function (key = get_key(),
                     start_time = Sys.time() - lubridate::dminutes(5),
                     end_time = 5,
                     activity_name = c("Break"),
                     activity_details = NULL)
{
  .e <- environment()
  .fmls <- rlang::fn_fmls()
  query <-purrr::imap(rlang::env_get_list(.e, nms = rlang::fn_fmls_names()),
                        arg_match, .fmls = .fmls)
  call_type <- tail(as.character(match.call()[[1]]), 1)
  body_params <- list(start_time = start_time, end_time = end_time, activity_name = activity_name, activity_details = activity_details)
  req <- q_fn(call_type)(httr::modify_url(urls[[call_type]], query = list(key = key)), body = body_params, encode = "json")
  pretty_content(req)
}

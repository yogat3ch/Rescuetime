

htm <- xml2::read_html("https://www.rescuetime.com/apidoc")
sections <- rvest::html_elements(htm, xpath = "//section[@class='api-doc']")

headings <- sections |>
  purrr::map_chr(~xml2::xml_child(.x, "h2") |> rvest::html_text())

ul2list <- function(x) {
  out <- rvest::html_children(x) |>
    purrr::map_chr(rvest::html_text) |>
    stringr::str_split("\\s\\-\\s", n = 2)
  if (all(purrr::map_dbl(out, length) == 2))
    out <- rlang::set_names(purrr::map_chr(out, 2), purrr::map_chr(out, 1))
}

params <- purrr::map2(sections, headings, ~{
  .content <- xml2::xml_contents(.x)
  .content <- purrr::keep(.content, ~rvest::html_name(.x) != "text")
  .text <- purrr::map_chr(.content, rvest::html_text)
  .tag <- purrr::map_chr(.content, rvest::html_name)
  .h3 <- which(.tag == "h3" & .text %in% paste(c("Required", "Optional", "Query"), "parameters"))
  out <- list()
  out$key_params <- .content[.h3 + 1] |>
    purrr::map(ul2list) |>
    rlang::set_names(purrr::map_chr(.content[.h3], rvest::html_text))

  if (any(.tag %in% "div"))
    out$query_parameters <- rvest::html_table(rvest::html_children(.content[.tag == "div"]))[[1]]
  out
}) |>
  rlang::set_names(headings)
params <- params[stringr::str_detect(names(params), "^Doc")]

split_args <- function(x) {
  out <- list()
  if (any(stringr::str_detect(x, "^\\[")))
    out <- stringr::str_extract_all(x, "(?<=\\')\\w+(?=\\')") |> rlang::set_names(names(x))

  out[purrr::map_lgl(out, rlang::is_empty)] <- stringr::str_extract(x, "\\w+")
  out
}
do_params <- function(x) split_args(subset(x, names(x) != "key"))
purrr::imap(params, ~{


  r_params <- do_params(.x$key_params$`Required parameters`)

  q_params <- list()
  if (!is.null(.x$query_parameters))
    q_params <- rlang::set_names(split_args(.x$query_parameters$Values), .x$query_parameters$`Principle name`)
  o_params <- list()
  if (!is.null(.x$key_params$`Optional parameters`))
    o_params <- do_params(.x$key_params$`Optional parameters`)
  rlang::expr()
  rlang::new_function(rlang::pairlist2(!!!r_params,
                                       !!!q_params,
                                       !!!o_params,
                                       key = rlang::expr(key())), body = rlang::expr())
})

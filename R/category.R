#' Disqus Category API
#' @export
category <- function(...) {
  raw <- fromJSON(listTemplate(resource = "categories", ...))

  class(raw) <- c("dq_category")

  return(raw$response)
}

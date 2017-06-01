#' Disqus Category API
#' @param ... ...
#' @export
category <- function(...) {
  raw <- fromJSON(listTemplate(ressource = "categories", ...))

  class(raw) <- c("dq_category")

  return(raw$response)
}

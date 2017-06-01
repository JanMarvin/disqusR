#' Disqus Forums API
#' @param ... ...
#' @export
forums <- function(...) {
  raw <- fromJSON(listTemplate(ressource = "forums", ...))

  class(raw) <- c("dq_forums")

  return(raw$response)
}


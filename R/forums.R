#' Disqus Forums API
#' @export
forums <- function(...) {
  raw <- fromJSON(listTemplate(resource = "forums", ...))

  class(raw) <- c("dq_forums")

  return(raw$response)
}


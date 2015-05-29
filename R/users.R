#' Disqus Users API
#' @export
users <- function(...) {
  raw <- fromJSON(listTemplate(resource = "users", ...))

  class(raw) <- c("dq_users")

  return(raw$response)
}


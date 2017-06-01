#' Disqus Users API
#' @param ... ...
#' @export
users <- function(...) {
  raw <- fromJSON(listTemplate(ressource = "users", ...))

  class(raw) <- c("dq_users")

  return(raw$response)
}


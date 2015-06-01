#' Disqus Threads API
#' @export
threads <- function(...) {
  raw <- fromJSON(listTemplate(resource = "threads", ...))

  class(raw) <- c("dq_thread")

  return(raw$response)
}

#' getthreadid
#'
#' Wrapper for threads() to fetch the id of a given forum.
#'
#' @param thrd numeric or string if is url this wrapper tries to fetch the id of
#' the disqus thread in the link.
#' @param ... additional listTemplate Options.
#' @export
getthreadid <- function(thrd=NULL, ...) {
  if (!is.null(thrd))
    raw <- threads(option="list", thread=thrd, ...)
  else
    stop("No thread choosen.")

  return(raw$id)
}

#' @export
threadsS4 <- function(...) {
  raw <- fromJSON(listTemplate(resource = "threads", ...))
  # out <- dqThread()

  out <- apply(raw$response, 1, function(x) {
    out <- dqThread()
    assignValues(out, x)
  }
  )

  return(out)
}

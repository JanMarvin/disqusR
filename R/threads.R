
#' @export
threads <- function(...) {
  raw <- fromJSON(listTemplate(resource = "threads", ...))
  out <- dqThread()

  out <- apply(raw$response, 1, function(x) {
    out <- dqThread()
    assignValues(out, x)
  }
  )

  return(out)
}


#' Disqus Threads API
#'
#' \code{threads()} is a function to access the disqus threads api. It is built
#' as a comfortable wrapper for \code{listTemplate()} a disqusR function
#' underlying most of the retrieving (GET) functions of the disqus API. It will
#' return an object of class dq_thread, containing the content retrieved.
#' By default \code{threads()} will return a json object.
#'
#' threads() requires at least one option.
#'
#' For a full documentation and all currently possible options please see
#' \url{https://disqus.com/api/docs/threads/}.
#'
#' As of June 2015 following options are available.
#' @param type json or rss
#' @param ... options: close, create, details, list, listHot, listPopular,
#' listPosts, open, remove, restore, set, subscribe, unsubscribe, update, vote
#'
#' GET functions:
#' details
#' list
#' listHot
#' listPopular
#' listPosts
#' set
#' subscribe
#' update
#'
#' POST functions: close,
#' create
#' open
#' remove
#' restore
#' unsubscribe
#' vote -1, 0, 1
#'
#' @examples
#' \dontrun{
#' # lists threads of forum=politico in RSS-type
#' threads("list", forum="politico", type="rss")
#'
#' # list a few disqus threads
#' threads("list")
#' }
#' @export
threads <- function(option, type = "json", ...) {

  lT <- listTemplate(option, ressource = "threads", type = type, ...)

  if (type == "json") {
    raw <- fromJSON(lT)$response
  } else{
    raw <- lT
  }

  class(raw) <- c("dq_thread")

  return(raw)
}

#' getthreadid
#'
#' Wrapper for threads() to fetch the id of a given forum.
#'
#' @param thrd numeric or string if is url this wrapper tries to fetch the id of
#' the disqus thread in the link.
#' @param ... additional listTemplate Options.
#' @example
#' \donotrun{
#' trds <- threads("list", forum="politico")
#'
#' getthreadid(trds$link[1])
#' }
#' @export
getthreadid <- function(thrd=NULL, ...) {
  if (!is.null(thrd))
    raw <- threads(option="list", thread=thrd, ...)
  else
    stop("No thread choosen.")

  return(raw$id)
}

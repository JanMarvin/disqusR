#' Disqus Posts API
#' @param ... listTemplate option.
#' @details Original API Documentation for threads
#' \code{https://disqus.com/api/docs/threads/}
#' @examples
#' \dontrun{
#' # lists threads of forum=politico in RSS-format
#' threads("list", forum="politico", format="rss")
#'
#' # use a specific link ending with ".html" (may work if link was not changed)
#' art <- "http://www.11freunde.de/liveticker/das-rueckspiel-der-relegation-im-liveticker"
#' thrd <- threads("list", thread=art)
#' pst <- posts("list", forum="11freunde", thread=thrd$id)
#'
#' # use a timestamp
#' unixtime <- as.numeric(as.POSIXct("2015-05-26", format="%Y-%m-%d"))
#' threads("list", forum="politico", since=unixtime)
#' }
#' @export
posts <- function(...) {

  # author is a nested data.frame in posts. If not flattend, apply does not work
  raw <- fromJSON(listTemplate(ressource = "posts", ...),
                  flatten = TRUE)

  class(raw) <- c("dq_post")
  return(raw)
}

#' getposts
#'
#' Wrapper for posts to extract and clean the returned posts.
#' @param forum string or numeric
#' @param thread string presumably collected by getthreadid.
#' @param ... addition listTemplate options.
#' @export
getposts <- function(forum=forum, thread=thread, ...) {

  raw <- posts(option="list", forum=forum, thread=thread, ...)

  pst <- raw$response

  pst <- lapply(pst$raw_message, FUN=gsub, pattern="\n", replacement=" ")

  return(unlist(pst))
}

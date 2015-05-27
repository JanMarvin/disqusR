
# for our own testing requires remove befor release
pubkey <- readChar(".pubkey", file.info(".pubkey")$size)

disqus_api_url <- "https://disqus.com/api/3.0/"

threads <- "threads/"

# "https://disqus.com/api/3.0/threads/list.json"

#' Disqus Threads: list API
#' @param category integer
#' @param forum integer/string unique identifier of a forum
#' @param thread integer or string if integer must be unique if string must
#' have link: or ident:
#' @param author
#' @param since
#' @param related
#' @param cursor
#' @param attach
#' @param limit integer between 25 (default) and 100 (maximum)
#' @param include
#' @param order string "desc" (descending) or "asc" (ascending)
#' @param format string json, jsonp or rss
#' @details Original API Documentation for threads:list
#' \code{https://disqus.com/api/docs/threads/list/}
#' @examples
#' # lists threads of forum=politico in RSS-format
#' threads_lists(forum="politico", format="rss")
#'
#' # use a specific link ending with ".html" (may work if link was not changed)
#' art <- "http://www.politico.com/magazine/story/2015/05/fox-news-liberals-118235.html"
#' #art <- URLencode(art, reserved = TRUE)
#' threads_lists(forum="politico", thread=paste0("link:", art))
#'

#' @export
threads_lists <- function(category = NULL, forum = NULL, thread = NULL,
                          author = NULL, since = NULL, related = NULL,
                          cursor = NULL, attach = NULL, limit = 25,
                          include = NULL, order="desc", format="json") {

  # create minimal required link.
  list <- paste0("list.", format)
  url <- paste0(disqus_api_url, threads, list)
  auth <- paste0("?api_key=", pubkey)

  # Add apikey to link
  url <- paste0(url, auth)

  if (!is.null(category)) {
    ctgry <- paste0("&category=", category)
    url <- paste0(url, ctgry)
  }

  if (!is.null(forum)) {
    frm <- paste0("&forum=", forum)
    url <- paste0(url, frm)
  }

  if (!is.null(thread)) {
    thrd <- paste0("&thread=", thread)
    url <- paste0(url, thrd)
  }

  if (!is.null(author)) {
    athr <- paste0("&author=", author)
    url <- paste0(url, athr)
  }

  if (!is.null(since)) {
    snc <- paste0("&since=", since)
    url <- paste0(url, snc)
  }

  # []
  if (!is.null(related)) {
    rltd <- paste0("&related=", related)
    url <- paste0(url, rltd)
  }

  if (!is.null(cursor)) {
    crsr <- paste0("&cursor=", cursor)
    url <- paste0(url, crsr)
  }

  # []
  if (!is.null(attach)) {
    attch <- paste0("&attach=", attach)
    url <- paste0(url, attch)
  }

  # [ "open", "closed", "killed" ]
  if (!is.null(include)) {
    attch <- paste0("&include=", include)
    url <- paste0(url, attch)
  }

  # asc, desc
  if (order!="desc") {
    ordr <- paste0("&order=", order)
    url <- paste0(url, ordr)
  }


  # GET results
  url <- httr::GET(url)

  # convert url to readable output of format
  erg <- httr::content(url, as = "text")

  return(erg)

#   forums <- fromJSON(erg, simplifyDataFrame = TRUE, flatten = TRUE)
#   forums <- forums$response

}

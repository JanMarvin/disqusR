
# for our own testing requires remove befor release
pubkey <- readChar(".pubkey", file.info(".pubkey")$size)


#' Disqus Threads API
#' @param option string close, create, details, list, listHot, listPopular,
#' listPosts, open, remove, restore, set, subscribe, unsubscribe, update, vote
#' Options may differ in their set of Arguments.
#' @param category integer
#' @param forum integer/string unique identifier of a forum
#' @param thread integer or string if integer must be unique if string must
#' have link: or ident:
#' @param author
#' @param since integer or string timestamp iso or unixtime.
#' @param related
#' @param cursor
#' @param attach
#' @param limit integer between 25 (default) and 100 (maximum)
#' @param include
#' @param order string "desc" (descending) or "asc" (ascending)
#' @param format string json, jsonp or rss
#' @details Original API Documentation for threads
#' \code{https://disqus.com/api/docs/threads/}
#' @examples
#' # lists threads of forum=politico in RSS-format
#' threads("list", forum="politico", format="rss")
#'
#' # use a specific link ending with ".html" (may work if link was not changed)
#' art <- "http://www.politico.com/magazine/story/2015/05/fox-news-liberals-118235.html"
#' threads("list" , forum="politico", thread=paste0("link:", art))
#'
#' # use a timestamp
#' unixtime <- as.numeric(as.POSIXct("2015-05-26", format="%Y-%m-%d"))
#' threads("list", forum="politico", since=unixtime)
#'
#' @export
threads <- function(option = NULL,
                    category = NULL, forum = NULL, thread = NULL,
                    author = NULL, since = NULL, related = NULL,
                    cursor = NULL, attach = NULL, limit = 25,
                    include = NULL, order = NULL, format = "json") {

  if (is.null(option))
    stop("No option was called.")


  disqus_api_url <- "https://disqus.com/api/3.0/"
  threads_url <- "threads/"

  # create minimal required link.
  option <- paste0(option, ".", format)
  url <- paste0(disqus_api_url, threads_url, option)
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
  if (option == "list") {
    if (order!="desc") {
      ordr <- paste0("&order=", order)
      url <- paste0(url, ordr)
    }
  }


  # GET results
  url <- httr::GET(url)

  # convert url to readable output of format
  erg <- httr::content(url, as = "text")

  return(erg)

  #   forums <- fromJSON(erg, simplifyDataFrame = TRUE, flatten = TRUE)
  #   forums <- forums$response

}

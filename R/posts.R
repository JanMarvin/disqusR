#' Disqus Posts API
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
#' arts <- threads("list" , forum="politico", thread=paste0("link:", art))
#' postslist <- posts("list", thread=arts[1,"id"])
#' # use a timestamp
#' unixtime <- as.numeric(as.POSIXct("2015-05-26", format="%Y-%m-%d"))
#' threads("list", forum="politico", since=unixtime)
#'
#' @export
posts <- function(...) {
  listTemplate(resource = "posts", ...)
}

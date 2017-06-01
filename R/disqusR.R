#' Access to the version 3.0 Disqus API.
#'
#' Check applications(), blacklist(), category(), exports(), forums(),
#' imports(), threads(), topics(), trends(), users() and whitelist().
#'
#' @author Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#'
#' @name disqusR
#' @docType package
#' @import httr jsonlite
#' @note If you catch a bug, please do not sue us, we do not have any money.
#' @examples
#' \dontrun{
#'
#' # use a specific link ending with ".html" (may work if link was not changed)
#' art <- "http://www.politico.com/magazine/story/2015/05/fox-news-liberals-118235.html"
#' arts <- threads("list" , forum="politico", thread=paste0("link:", art))
#' postslist <- posts("list", thread=arts[1,"id"])
#' # use a timestamp
#' unixtime <- as.numeric(as.POSIXct("2015-05-26", type="%Y-%m-%d"))
#' threads("list", forum="politico", since=unixtime)
#' }
NULL

#' Disqus Threads API
#' @param option string close, create, details, list, listHot, listPopular,
#' listPosts, open, remove, restore, set, subscribe, unsubscribe, update, vote
#' Options may differ in their set of Arguments.
#' @param category integer
#' @param forum integer/string unique identifier of a forum
#' @param thread integer or string if integer must be unique if string must
#' have link: or ident:
#' @param author post author
#' @param since integer or string: unixtime or iso timestamp
#' \code{type(Sys.time(), "\%Y-\%m-\%dT\%H:\%M:\%S")}.
#' @param related related
#' @param cursor cursor
#' @param attach attach
#' @param limit integer between 25 (default) and 100 (maximum)
#' @param include include
#' @param order string "desc" (descending) or "asc" (ascending)
#' @param type string json, jsonp or rss
#' @param ressource ressource
#' @param pubkey string containing disqus pubkey
#' @param start start timestamp
#' @details Original API Documentation for threads
#' \code{https://disqus.com/api/docs/threads/}
#' @importFrom utils URLencode
#' @export
listTemplate <- function(option = NULL,
                         category = NULL, forum = NULL, thread = NULL,
                         author = NULL, since = NULL, related = NULL,
                         cursor = NULL, attach = NULL, limit = 25,
                         include = NULL, order = "desc", type = "json",
                         ressource = c("threads","posts"), pubkey,
                         start = NULL, end = NULL) {

  if (missing(pubkey)) {
    pubkey <- get0("pubkey", envir = globalenv())
    if (is.null(pubkey)) stop("Abort. No pubkey provided or found.")
  }

  if (is.null(option))
    stop("No option was called.")

  disqus_api_url <- "https://disqus.com/api/3.0/"
  ressource_url <- paste0(ressource,"/")

  # create minimal required link.
  option <- paste0(option, ".", type)
  url <- paste0(disqus_api_url, ressource_url, option)
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

    # test if thread is a link.
    if (length(grep("^(http|https)", thread))) {

      # disqus requires this
      thread <- URLencode(thread, reserved = TRUE)

      thrd <- paste0("&thread=link:", thread)
    } else {
      thrd <- paste0("&thread=", thread)
    }

    url <- paste0(url, thrd)
  }

  # asc, desc
  if (order!="desc") {
    ordr <- paste0("&order=", order)
    url <- paste0(url, ordr)
  }

  if (!is.null(author)) {
    athr <- paste0("&author=", author)
    url <- paste0(url, athr)
  }

  if (!is.null(since)) {
    snc <- paste0("&since=", since)
    url <- paste0(url, snc)
  }

  if (!is.null(start)) {
    stt <- paste0("&start=", start)
    url <- paste0(url, stt)
  }


  if (!is.null(end)) {
    end <- paste0("&end=", end)
    url <- paste0(url, end)
  }

  # []
  n_rel <- length(related)

  rltd <- NULL
  if (!is.null(related)) {
    for (i in 1:n_rel) {
      rltd_i <- paste0("&related=", related)
      rltd <- paste0(rltd, rltd_i)
    }
    url <- paste0(url, rltd)
  }

  if (!is.null(cursor)) {
    crsr <- paste0("&cursor=", cursor)
    url <- paste0(url, crsr)
  }

  # []
  n_att <- length(attach)

  attch <- NULL
  if (!is.null(attach)) {
    for (i in 1:n_att) {
      attch_i <- paste0("&attach=", attach)
      attch <- paste0(attch, attch_i)
    }
    url <- paste0(url, attch)
  }

  # the bracket variant does not seem to work.
  # e.g. attach <- paste(shQuote(attach, type="cmd"), collapse=", ")
  # attach <- paste0("[ ", attach, " ]")

  # [ "open", "closed", "killed" ]
  n_incl <- length(include)

  incl <- NULL
  if (!is.null(include)) {
    for (i in 1:n_incl) {
      incl_i <- paste0("&include=", include[i])
      incl <- paste0(incl, incl_i)
    }
    url <- paste0(url, include)
  }

  # limit

  if (limit != 25) {
    lmt <- paste0("&limit=", limit)
    url <- paste0(url, lmt)
  }

  cat("URL: ", url, "\n")

  # GET results
  url <- httr::GET(url)

  # convert url to readable output of type
  erg <- httr::content(url, as = "text")

  return(erg)

  #   forums <- fromJSON(erg, simplifyDataFrame = TRUE, flatten = TRUE)
  #   forums <- forums$response

}

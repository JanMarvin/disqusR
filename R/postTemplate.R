#' postTemplate
#' @param option option
#' @param title title
#' @param message message
#' @param category category
#' @param forum forum
#' @param thread thread
#' @param author author
#' @param since since
#' @param related related
#' @param cursor cursor
#' @param attach attach
#' @param limit limit
#' @param include include
#' @param order order
#' @param format string default json
#' @param ressource ressource
#' @param atoken atorken
#' @param pubkey pubkey
#' @param seckey seckey
#' @export
postTemplate <- function(option = NULL, title = NULL, message = NULL,
                         category = NULL, forum = NULL, thread = NULL,
                         author = NULL, since = NULL, related = NULL,
                         cursor = NULL, attach = NULL, limit = 25,
                         include = NULL, order = NULL, format = "json",
                         ressource = c("threads","posts"),
                         atoken = NULL, pubkey, seckey ) {


  if (missing(pubkey)) {
    pubkey <- get0("pubkey")
    if (is.null(pubkey)) stop("Abort. No pubkey provided or found.")
  }


  if (missing(seckey)) {
    seckey <- get0("seckey")
    if (is.null(seckey)) stop("Abort. No seckey provided or found.")
  }

  if (is.null(option))
    stop("No option was called.")

  disqus_api_url <- "https://disqus.com/api/3.0/"
  ressource_url <- paste0(ressource,"/")

  # create minimal required link.
  option <- paste0(option, ".", format)
  url <- paste0(disqus_api_url, ressource_url, option)

  # Die Api doku sagt, man braucht einen access_token, den gibt es nur ueber
  # Umwege. Man muss sich dafuer erst authentifizieren, dann kommt eine web-
  # seite, dann muss man da ok druecken.
  auth <- paste0("?access_token=", atoken,
                 "&api_key=", pubkey,
                 "&api_secret=", seckey)

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

  if (!is.null(title)) {
    ttl <- paste("&title=", title)
    url <- paste0(url, ttl)
  }

  if (!is.null(message)) {
    mssg <- paste("&message=", message)
    url <- paste0(url, mssg)
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

  if (!is.null(author)) {
    athr <- paste0("&author=", author)
    url <- paste0(url, athr)
  }

  if (!is.null(since)) {
    snc <- paste0("&since=", since)
    url <- paste0(url, snc)
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

  # asc, desc
  if (option == "list") {
    if (order!="desc") {
      ordr <- paste0("&order=", order)
      url <- paste0(url, ordr)
    }
  }

  cat("\n debug::URL: ", url, "\n")

  # GET results
  url <- httr::POST(url)

  # convert url to readable output of format
  # erg <- httr::content(url, as = "text")

  return(url)

  #   forums <- fromJSON(erg, simplifyDataFrame = TRUE, flatten = TRUE)
  #   forums <- forums$response

}

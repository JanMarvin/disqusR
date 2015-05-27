
# for our own testing requires remove befor release
pubkey <- readChar(".pubkey", file.info(".pubkey")$size)

disqus_api_url <- "https://disqus.com/api/3.0/"

threads <- "threads/"

# "https://disqus.com/api/3.0/threads/list.json"

#' Disqus Threads: list API
#' @details https://disqus.com/api/docs/threads/list/
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

  # GET results
  url <- httr::GET(url)

  # convert url to readable output of format
  erg <- httr::content(url, as = "text")

  return(erg)

#   forums <- fromJSON(erg, simplifyDataFrame = TRUE, flatten = TRUE)
#   forums <- forums$response

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listTemplate.R
\name{listTemplate}
\alias{listTemplate}
\title{Disqus Threads API}
\usage{
listTemplate(option = NULL, category = NULL, forum = NULL,
  thread = NULL, author = NULL, since = NULL, related = NULL,
  cursor = NULL, attach = NULL, limit = 25, include = NULL,
  order = NULL, type = "json", ressource = c("threads", "posts"), pubkey,
  start = NULL)
}
\arguments{
\item{option}{string close, create, details, list, listHot, listPopular,
listPosts, open, remove, restore, set, subscribe, unsubscribe, update, vote
Options may differ in their set of Arguments.}

\item{category}{integer}

\item{forum}{integer/string unique identifier of a forum}

\item{thread}{integer or string if integer must be unique if string must
have link: or ident:}

\item{author}{post author}

\item{since}{integer or string: unixtime or iso timestamp
\code{type(Sys.time(), "\%Y-\%m-\%dT\%H:\%M:\%S")}.}

\item{related}{related}

\item{cursor}{cursor}

\item{attach}{attach}

\item{limit}{integer between 25 (default) and 100 (maximum)}

\item{include}{include}

\item{order}{string "desc" (descending) or "asc" (ascending)}

\item{type}{string json, jsonp or rss}

\item{ressource}{ressource}

\item{pubkey}{string containing disqus pubkey}

\item{start}{start timestamp}
}
\description{
Disqus Threads API
}
\details{
Original API Documentation for threads
\code{https://disqus.com/api/docs/threads/}
}

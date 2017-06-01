# disqusR


`disqusR` is a wrapper around the public disqus API version 3 and allows reading
threads from disqus forums into R.

Among the many companies that use or used disqus are for instance news sites 
politico.com^x, wired.com, cnn.com^x and welt.de^x (^x marks companies that 
appear to no longer use disqus). Lately it looks like many of these
have dropped disqus as of today only wired.com looks like it is still using
disqus.

We developed `disqusR`some time ago as a fun project to play around with json
and playing with word clouds in R. Development was ceased after some initial
functions were working. Since then it was rotting in a non public git of ours.

To be able to use `disqusR` one has to register at disqus.com. After 
registration one has to create an app in disqus so that you will retrieve a
public and a secret key. Both are required to use `disqusR`. If `pubkey` and
`seckey` are set, one can start parsing comments. Be aware that there is an 
upper limit to the number of requests one can make to `disqus.com`.

A few examples are given in the help files.

```{R}
library(disqusR)

# insert pubkey and seckey (or parse from file)
pubkey <- "..."
seckey <- "..."

# use the websites link
art <- "http://www.rollingstone.com/music/lists/50-greatest-live-albums-of-all-time-20150429"
arts <- threads("list" , forum="rollingstone", thread=paste0("link:", art))

# fetch 5 comments
postslist <- posts("list", thread=arts$id, limit = 5)

# select 100 items after a certain timestamp
unixtime <- as.numeric(as.POSIXct("2017-05-31", type="%Y-%m-%d"))
trds <- threads("list", forum="rollingstone", since=unixtime, limit = 100)

str(trds)

```

Since you can read a maximum of 100 entries, lets have a look how to read more
than 100 comments of a post.

```{R}
# use the websites link (may work if link was not changed)
art <- "http://www.rollingstone.com/music/lists/50-greatest-live-albums-of-all-time-20150429"
arts <- threads("list" , forum="rollingstone", thread=paste0("link:", art))

postslist <- posts("list", thread=arts$id, limit = 5)

# ascending
postslist <- posts("list", thread=arts$id, order = "asc", limit = 5)

postslist$author.name

pl1 <- postslist$raw_message


# last timestamp in iso form
time <- postslist$createdAt[5]


postslist <- posts("list", thread=arts$id, order = "asc", limit = 5,
                   start = time)

# should start with the last name
postslist$author.name

pl2 <- postslist$raw_message[2:5]

p <- c(pl1, pl2)
p
```

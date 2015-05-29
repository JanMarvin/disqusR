##
##  Filename:      object.R
##  Creation Date: 28-05-2015
##  Last Modified: Do 28 Mai 2015 12:00:28 CEST
##
##  Description:
##
##
##

#cat(paste0(names(fromJSON(arts)$response), ' = "',
#                 sapply(fromJSON(arts)$response, typeof), '",\n'))

dqThread <- setClass(
  "disqus thread",
  slots = c(
    feed = "character",
    identifiers = "character",
    dislikes = "integer",
    likes = "integer",
    clean_title_unescaped = "character",
    message = "character",
    id = "character",
    createdAt = "character",
    category = "character",
    author = "character",
    userScore = "integer",
    signedLink = "character",
    isDeleted = "logical",
    raw_message = "character",
    isClosed = "logical",
    link = "character",
    slug = "character",
    forum = "character",
    clean_title = "character",
    posts = "integer",
    userSubscription = "logical",
    title = "character",
    highlightedPost = "logical"
  )
)

# dqAvatar <- setClass(
#   "disqus post author avatar",
#   slots = c(
#     small_permalink = "character",
#     small_cache = "character",
#     isCustom = "logical",
#     permalink = "character",
#     cache = "character",
#     large_permalink = "character",
#     large_cache = "character"
#   )
# )
# dqAuthor <- setClass(
#   "disqus post author",
#   slots = c(
#     usename = "character",
#     about = "character",
#     name = "character",
#     disable3rdPartyTrackers = "logical",
#     url = "character",
#     isAnonymous = "logical",
#     rep = "numeric",
#     profileUrl= "character",
#     reputation = "numeric",
#     location = "character",
#     isPrivate = "logical",
#     isPrimary = "logical",
#     joinedAt = "character", # Date (Y-M-DTH:M:S)
#     id = "integer",
#     avatar = "disqus post author avatar" # Da kaeme wieder viel
#   )
# )

dqPost <- setClass(
  "disqus post",
  slots = c(
    forum = "character",
    parent = "integer",
    isApproved = "logical",
    # author = "disqus post author",
    # This is a nested "data.frame". Since the original data.frame needs to be
    # flattend it is uncomplicated to include them here.
    author_usename = "character",
    author_about = "character",
    author_name = "character",
    author_disable3rdPartyTrackers = "logical",
    author_url = "character",
    author_isAnonymous = "logical",
    author_rep = "numeric",
    author_profileUrl= "character",
    author_reputation = "numeric",
    author_location = "character",
    author_isPrivate = "logical",
    author_isPrimary = "logical",
    author_joinedAt = "character", # Date (Y-M-DTH:M:S)
    author_id = "character",
    # avatar
    small_permalink = "character",
    small_cache = "character",
    isCustom = "logical",
    permalink = "character",
    cache = "character",
    large_permalink = "character",
    large_cache = "character",
    # media = "", # ein weiterer Data.Frame
    isDeleted = "logical",
    isFlagged = "logical",
    dislikes = "integer",
    raw_message = "character",
    createdAt = "character", #"Date",
    id = "character",
    thread = "character",
    numReports = "integer",
    isDeletedByAuthor = "logical",
    likes = "integer",
    isEdited = "logical",
    message = "character",
    isSpam = "logical",
    isHighlighted = "logical",
    points = "integer"
  )
)

# create a method to assign the valuess
setGeneric(name="assignValues",
           def=function(theObject, theData)
           {
             standardGeneric("assignValues")
           }
)

setMethod(f="assignValues",
          signature="disqus thread",
          {
            definition=function(theObject,theData)
            {
              theObject@feed                  <- theData$feed
              theObject@identifiers           <- theData$identifiers
              theObject@dislikes              <- theData$dislikes
              theObject@likes                 <- theData$likes
              theObject@clean_title_unescaped <- theData$clean_title_unescaped
              theObject@message               <- theData$message
              theObject@id                    <- theData$id
              theObject@createdAt             <- theData$createdAt
              theObject@category              <- theData$category
              theObject@author                <- theData$author
              theObject@userScore             <- theData$userScore
              theObject@signedLink            <- theData$signedLink
              theObject@isDeleted             <- theData$isDeleted
              theObject@raw_message           <- theData$raw_message
              theObject@isClosed              <- theData$isClosed
              theObject@link                  <- theData$link
              theObject@slug                  <- theData$slug
              theObject@forum                 <- theData$forum
              theObject@clean_title           <- theData$clean_title
              theObject@posts                 <- theData$posts
              theObject@userSubscription      <- theData$userSubscription
              theObject@title                 <- theData$title
              theObject@highlightedPost       <- theData$highlightedPost

              return(theObject)
            }
          }
)

setMethod(f="assignValues",
          signature="disqus post",
          {
            definition=function(theObject,theData)
            {
              theObject@forum                 <- theData$forum
              theObject@parent                <- theData$parent
              theObject@isApproved            <- theData$isApproved
              # theObject@author               <- theData$author
              theObject@author_usename        <- theData$author.username
              theObject@author_about          <- theData$author.about
              theObject@author_name           <- theData$author.name
              theObject@author_disable3rdPartyTrackers <-
                theData$author.disable3rdPartyTrackers
              theObject@author_url            <- theData$author.url
              theObject@author_isAnonymous    <- theData$author.isAnonymous
              theObject@author_rep            <- theData$author.rep
              theObject@author_profileUrl     <- theData$author.profileUrl
              theObject@author_reputation     <- theData$author.reputation
              theObject@author_location       <- theData$author.location
              theObject@author_isPrivate      <- theData$author.isPrivate
              theObject@author_isPrimary      <- theData$author.isPrimary
              theObject@author_joinedAt       <- theData$author.joinedAt
              theObject@author_id             <- theData$author.id
              theObject@small_permalink       <- theData$author.avatar.small.permalink
              theObject@small_cache           <- theData$author.avatar.small.cache
              theObject@isCustom              <- theData$author.avatar.isCustom
              theObject@permalink             <- theData$author.avatar.permalink
              theObject@cache                 <- theData$author.avatar.cache
              theObject@large_permalink       <- theData$author.avatar.large.permalink
              theObject@large_cache           <- theData$author.avatar.large.cache
              # theObject@media                 <- theData$media
              theObject@isDeleted             <- theData$isDeleted
              theObject@isFlagged             <- theData$isFlagged
              theObject@dislikes              <- theData$dislikes
              theObject@raw_message           <- theData$raw_message
              theObject@createdAt             <- theData$createdAt
              theObject@id                    <- theData$id
              theObject@thread                <- theData$thread
              theObject@numReports            <- theData$numReports
              theObject@isDeletedByAuthor     <- theData$isDeletedByAuthor
              theObject@likes                 <- theData$likes
              theObject@isEdited              <- theData$isEdited
              theObject@message               <- theData$message
              theObject@isSpam                <- theData$isSpam
              theObject@isHighlighted         <- theData$isHighlighted
              theObject@points                <- theData$points

              return(theObject)
            }
          }
)


setGeneric(name="getList",
           def=function(theObject)
           {
             standardGeneric("getList")
           }
)

setMethod(f="getList",
          signature="disqus thread",
          definition=function(theObject)
          {
            thePosts <- jsonlite::fromJSON(
              listTemplate("list", thread=theObject@id, resource = "posts")
            )$response
            return(thePosts)
          }
)


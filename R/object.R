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

# create a method to assign the valuess
setGeneric(name="assignValues",
           def=function(theObject, theData)
           {
             standardGeneric("assignValues")
           }
           )

setMethod(f="assignValues",
          signature="disqus thread",
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
            thePosts <- fromJSON(listTemplate("list", thread=theObject@id, resource = "posts"))$response
            return(thePosts)
          }
          )


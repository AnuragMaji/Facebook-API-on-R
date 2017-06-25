install.packages("Rfacebook")
library(devtools)
library(Rfacebook)
token <- 'EAACEdEose0cBAKEsKbWZCkoTZAGM9IN0AFnlJOfyNu7a9K4OfxOZBZAm5p00YiDm2xsG8P1gzPlZB1YiydIvum0QRDpaKqzSxMU76tgqxEOYNdjgcZAptf2IyJpesEgaGxuIyKJCoQvJ2jzBNamiZBWEKTrT9bHURCidzSYsUsqRYregPlOS81DfL3svM5DWgcZD'
me <- getUsers("me", token, private_info=TRUE)
me$name
me$hometown
x<- getUsers(c("barackobama", "donaldtrump"), token)
my_friends <- getFriends(token, simplify = TRUE)
my_friends_info <- getUsers(my_friends$id, token, private_info = TRUE)
mat <- getNetwork(token, format = "adj.matrix")
page <- getPage("humansofnewyork", token, n = 5000)

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", 
                                                                                  breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())
page2 <- getPage("humansofnewyork", token, n = 5000, since='2015/01/01', until='2015/12/31')


post_id <- head(page$id, n = 1)  ## ID of most recent post
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = FALSE)

group <- getGroup("150048245063649", token, n=50)

getLikes('me', token = token)[1,]

getNewsfeed(token = token, n=1)

callAPI("https://graph.facebook.com/v2.0/barackobama?fields=id", token)

#scraper
library(tidyverse)
library(purrr)
library(wordcloud)
library(stringi)
library(string)
library(dplyr)
library(ytcol)
library(tuber)

#ytcol documentation: https://github.com/Vintonm49/ytcol/tree/master/man
#tuber documentation: https://cran.r-project.org/web/packages/tuber/tuber.pdf

# Google Authoraization and channel ID ---------------------------------------------------
api_key <-
#Google Atuh 
client <- ""
secret <- ""
yt_oauth(client, secret)

#Get channel ID
url <- "https://www.youtube.com/channel/UCh6HDKcLwJioBBRSprqfezA"
channel_id <- yt.GetChannelID('url')


# Data Collection ---------------------------------------------------------

#collect general channel statistics
chstat = get_channel_stats(channel_id)
chstat$snippet$publishedAt <- as.Date(chstat$snippet$publishedAt)
#collect all video stats
all_stats <- get_all_channel_video_stats('UC-Vc3KOCQAwn4dck3O8EFBw')

all_stats$viewCount <- as.numeric(all_stats$viewCount)
all_stats$likeCount <- as.numeric(all_stats$likeCount)
all_stats$dislikeCount <- as.numeric(all_stats$dislikeCount)
all_stats$commentCount <- as.numeric(all_stats$commentCount)
all_stats$publication_date = as.Date(all_stats$publication_date)

#collect top "b" comments 

b <- 20

#arrange video by most commented
top_comments <- all_stats %>% 
  arrange(desc(commentCount)) %>% 
  head(b) %>%
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,commentCount)) %>% top_n(b)

#collect comments from the list created above
topi_comments <-  lapply(as.character(top_comments$id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})

#collect related videos for top "b" videos (competitors)

top_viewed_videos <- all_stats %>% 
  arrange(desc(viewCount)) %>% 
  head(b) %>% 
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,viewCount)) %>% top_n(b)
  
related_videos <- 
  lapply(as.character(top_viewed_videos$id), function(x){
  get_related_videos(c(video_id = x))
})
  

# data processing (related videos & general channel stats)  ---------------------------------------------------------

#after collecting all the related video channels, channel names for competitors should be organized in one place

#create related video table (competitors)
c <- c(1:20)

com_list = lapply(c, function(x){
  related_videos[[x]][["channelTitle"]]
})

competitors%>% arrange(desc(viewCount)) %>% head(20)

competitors <- (unlist(com_list))
competitors <- table(competitors)
competitors <- sort(competitors, method = "radix")
tail(competitors)
  
#create a general information table 
name <- chstat[["snippet"]][["title"]]
video_count <- chstat$statistics$videoCount
views <- chstat$statistics$viewCount
started <- chstat$snippet$publishedAt
country <- chstat$snippet$country
chstat$snippet$publishedAt <- as.Date(chstat$snippet$publishedAt)
subscribers <- chstat$statistics$subscriberCount
likes <-  sum(all_stats$likeCount)
dislikes = sum(all_stats$dislikeCount)
Comments_counti = sum(all_stats$commentCount)

genstat = data.frame(name, started, video_count, views, country, subscribers,likes, dislikes, Comments_counti )
genstat


# save data in excel sheets ---------------------------------------------

#save all comments 
save(topi_comments, file="top_comments.RData")

#save all statistics

write.table(all_stats , file = "all_stats.csv")

#save save channel statistcs
write.table(chstat , file = "chstat.csv")






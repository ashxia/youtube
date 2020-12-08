library(tidyverse)
library(tidytext)
library(rlist)
library(rlist)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corpus)
library(tm)
library(tuber)
library(ggthemes)


#number of results
b = 10

#upload all video stats, all stats, commentstext, ts and all comments 
Videostats <- read.csv("C:/Projects/yoututbe project/videostats.csv",stringsAsFactors = FALSE, sep ="")

all_stats <- read.csv("C:/Projects/yoututbe project/all_stats.csv",stringsAsFactors = FALSE, sep ="")

chstat <- read.csv("C:/Projects/yoututbe project/chstat.csv",stringsAsFactors = FALSE, sep ="")

comments_text <- read.csv("C:/Projects/yoututbe project/comments_text.csv", ,stringsAsFactors = FALSE, sep ="")

comments_ts <- read.csv("C:/Projects/yoututbe project/comments_ts.csv", ,stringsAsFactors = FALSE, sep ="")

load("C:/Projects/yoututbe project/fname.RData")

#convert the date to date
all_stats$publication_date = as.Date(all_stats$publication_date)

# creating new variables 'year' and 'month'
all_stats <-  all_stats %>% mutate(month = month(publication_date)) %>% mutate(year = year(publication_date))

# = General Stats Table = #

video_count <- chstat$statistics.videoCount
views <- chstat$statistics.viewCount
comments <- chstat$statistics.commentCount
country <- chstat$snippet.country
Start <- date(chstat$snippet.publishedAt)
subscribers <- chstat$statistics.subscriberCount



genstat = data.frame(Channel="lewishowes", Subcriptions=chstat$statistics.subscriberCount,
                     Views = chstat$statistics.viewCount,
                     Videos = chstat$statistics.videoCount, Likes = sum(all_stats$likeCount),
                     Dislikes = sum(all_stats$dislikeCount), Comments = sum(all_stats$commentCount))

genstat

# cleaning video titles
### find better solution for this in tidyvers
all_stats$title= gsub("<.*?>","", all_stats$title) #removing html tags
all_stats$title= gsub("[[:punct:]]", " ", all_stats$title) #removing html tags
all_stats$title = gsub("[ |\t]{2,}", " ", all_stats$title)  # Remove tabs
all_stats$title = gsub("^ ", "", all_stats$title)  # Leading blanks
all_stats$title = gsub(" $", "", all_stats$title)  # Lagging blanks
all_stats$title = gsub(" +", " ", all_stats$title) # General spaces 
all_stats$title = tolower(all_stats$title) # lowering all letters
head(all_stats$title,5) 

# creating like, dislike, comment ratio
### my idea was to find like an X factor where we give points to each activity 
all_stats = all_stats %>%
  mutate(like_ratio = likeCount/commentCount) %>% 
  mutate(dislike_ratio = dislikeCount/commentCount) %>%
  mutate(comment_ratio = commentCount/commentCount) %>% 
  mutate(ratio_to_total = commentCount/sum(commentCount, na.rm = TRUE))

#month wise video posting (data = all_stats)*
### this code only shows number of views per year
all_stats %>% group_by(year, month) %>% 
  summarise(comment_count = sum(commentCount)) %>% 
              ggplot(aes(year, comment_count))+
              geom_point()

all_stats %>% group_by(year) %>% 
  summarise(avg = mean(commentCount)) %>% 
  select(year, avg
         )

#engagement rate
all_stats %>% 
  group_by(year, month) %>% 
  summarise(engagment_rate = (commentCount+favoriteCount+likeCount+dislikeCount)/viewCount) %>% ggplot(aes(year, engagment_rate)) + geom_point()

#most viewed videos

all_stats %>% arrange(desc(viewCount)) %>% head(20) %>%
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,viewCount)) %>% top_n(20) %>% 
  ggplot(aes(as.factor(title), viewCount/1000000, fill = factor(year))) + 
  geom_col()+ 
  scale_x_discrete() +
  coord_flip()+
  ggtitle('Top 20 most viewd videos') + 
  xlab('Video title') + 
  ylab('Number of views (in millions)') +
  labs(fill = 'year', caption = '* Video titles have been trimmed') + 
  theme_economist(base_size = 16)

# most liked videos
all_stats %>% arrange(desc(likeCount)) %>% head(20) %>%
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,likeCount)) %>% top_n(20) %>% 
  ggplot(aes(as.factor(title), likeCount/1000, fill = factor(year))) + 
  geom_col()  + 
  scale_x_discrete() +
  coord_flip() +
  ggtitle('Top 20 most liked videos') + 
  xlab('Video title') + 
  ylab('Number of likes (in thousands)') +
  labs(fill = 'year', caption = '* Video titles have been trimmed')

b = 10

# most disliked videos
all_stats %>% arrange(desc(dislikeCount)) %>% head(20) %>%
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,dislikeCount)) %>% top_n(20) %>% 
  ggplot(aes(as.factor(title), dislikeCount/1000, fill = factor(year))) + 
  geom_col()  + 
  scale_x_discrete() +
  coord_flip() +
  ggtitle('Top 20 most disliked videos') + 
  xlab('Video title') + 
  ylab('Number of dislikes (in thousands)') +
  labs(fill = 'year', caption = '* Video titles have been trimmed')

b = 10

#monst videos that got comments  
all_stats %>% arrange(desc(commentCount)) %>% head(b) %>%
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,commentCount)) %>% top_n(b) %>% 
  ggplot(aes(as.factor(title), (commentCount/1000), fill = factor(year))) + 
  geom_col()  + 
  scale_x_discrete() +
  coord_flip() +
  ggtitle('Top 20 most videos with comments') + 
  xlab('Video title') + 
  ylab('Number of comments (in thousands)') +
  labs(fill = 'year', caption = '* Video titles have been trimmed')

#top most engagmed 
all_stats %>% 
  mutate(engagment  = commentCount+likeCount+dislikeCount) %>%
  arrange(desc(engagment)) %>% 
  head(b) %>% 
  mutate(title = strtrim(title, 50)) %>%
  mutate(title = reorder(title,engagment)) %>% top_n(b) %>%
  ggplot(aes(as.factor(title), (engagment/1000), fill = factor(year))) + 
  geom_col()  + 
  scale_x_discrete() +
  coord_flip() +
  ggtitle('Top most engaaged videso') + 
  xlab('Video title') + 
  ylab('Number of engagments (in thousands)') +
  labs(fill = 'year', caption = '* Video titles have been trimmed')

#calculate the most used words in the title
all_stats$title = removeWords(as.character(all_stats$title), stopwords('english'))
corpus = VCorpus(VectorSource(all_stats$title)) #remove common english words

dtm = DocumentTermMatrix(corpus) #construct a document term matrix

doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]

freq = colSums(as.matrix(dtm))
length(freq)

ord = order(freq, decreasing = TRUE)
freq[head(ord, n = 50)]

###################################

#likes vs dislikes of all records
p1 <- all_stats %>%
  ggplot(aes(likeCount,dislikeCount)) +
  geom_jitter(alpha = 0.4, shape = 1) + 
  labs(subtitle = 'All records') +  
  xlab('Count of likes') + ylab('Count of dislikes')
p1

#we want only to represent the data between 25% and 75% quanta
quantile(all_stats$likeCount, na.rm = TRUE)
quantile(all_stats$dislikeCount, na.rm = TRUE)

p2 <- all_stats %>% 
  filter(likeCount >= 112 & likeCount<1117 & dislikeCount>= 3 & dislikeCount<= 35) %>% 
  ggplot(aes(likeCount, dislikeCount)) +
  geom_jitter(alpha = 0.4, shape = 1) +
  labs(subtitle = "both lowest and highest quantile values removed")+
  xlab('count of likes') +
  ylab('count of dislikes')
p2 
#Top 10 most commented videos with like dislike ratio
all_stats <- all_stats %>% 
  mutate(like_dislike = round(likeCount/dislikeCount), 2)

all_stats %>%  arrange(desc(commentCount)) %>% 
  head(10) %>% 
  mutate(title = strtrim(title, 30)) %>%
  mutate(title = reorder(title, commentCount)) %>%
  ggplot(aes(title, commentCount, fill = like_dislike)) + geom_col()+ xlab("")+ coord_flip() + ggtitle('Top 10 most commented videos with likes/dislike ratio') + 
  scale_fill_continuous("Likes to Dislikes") +
  labs(caption = '* Video titles have been truncated')




name <- function(variables) {
  
}
#correlation between likes and comments

cor(all_stats$likeCount, all_stats$commentCount, use = 'complete')

#correlation betwween dislike and comments
cor(all_stats$dislikeCount, all_stats$commentCount, use = 'complete')
################################################################






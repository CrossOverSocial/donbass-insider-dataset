setwd("~/donbass-insider")
#Load the readr library to bring in the dataset
library(readr)
library(openssl)
library(ggplot2)
library("stringr") 
#Download the data set
df= read_csv('fb_crowdtangle.csv', col_names = TRUE)
dim(df)
#Displays the type and a preview of all columns as a row so that it's very easy to take in.
library(dplyr)
glimpse(df)

# clean column headers 
names(df) = gsub(pattern = "_ - ", replacement = "-", x = names(df))
names(df) = gsub(pattern = " - ", replacement = "-", x = names(df))
names(df) = gsub(pattern = "-", replacement = "", x = names(df))

# create a  ranking ow twitter users by tweets count
df1 <- df %>% group_by(userusername) %>% summarise(count = n_distinct(outlinksoutlinks)) %>% arrange(desc(count))
# select the first 50 twitter users
df2 <- df1 %>%  top_n(50)
df2$count<- as.numeric(as.character(df2$count))
#df2$userusername <- md5(df2$userusername) 
ggplot(df2,aes(x = reorder(`userusername`,-`count`), y = `count`)) + geom_bar(stat='identity')  + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Utilisateurs") + 
  ylab("Nombre total de tweets") + guides(fill=guide_legend(title="Les 50 plus gros posteurs de liens qactus sur Twitter"))

# see if the bubble is efficient by getting the number of likes and retweets

df4 <- df %>% select(userusername, date, outlinksoutlinks, retweetCount, likeCount ) %>% group_by(userusername) %>% summarise(count = sum(retweetCount)) %>% arrange(desc(count))
df5 <- df4  %>%  top_n(50)
df5$count<- as.numeric(as.character(df5$count))
#df5$userusername <- md5(df5$userusername) 

ggplot(df5,aes(x = reorder(`userusername`,-`count`), y = `count`)) + geom_bar(stat='identity')  + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Utilisateurs") + 
  ylab("Nombre total de retweets") + guides(fill=guide_legend(title="Les 50 plus gros retweetés sur des liens qactus sur Twitter"))

df6 <- df %>% select(userusername, date, outlinksoutlinks, retweetCount, likeCount ) %>% group_by(userusername) %>% summarise(count = sum(likeCount)) %>% arrange(desc(count))
df7 <- df6  %>%  top_n(50)
df7$count<- as.numeric(as.character(df7$count))
#df7$userusername <- md5(df7$userusername)

ggplot(df7,aes(x = reorder(`userusername`,-`count`), y = `count`)) + geom_bar(stat='identity')  + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Utilisateurs") + 
  ylab("Nombre total de likes") + guides(fill=guide_legend(title="Les 50 plus gros likés sur des liens qactus sur Twitter"))

# Synthesis  anonymous

df8 <- df2 %>% inner_join(df5, by ='userusername') %>% inner_join(df7, by = 'userusername')
df8 <- setNames(df8, c('User', 'Nb of tweets', 'Nb of Retweets', 'Nb of likes' ))

write.csv2(df8, file = "compil.csv")
install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite",
                   "devtools"), type = "binary")

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)

useNIADic()

txt <- readLines("hiphop.txt")
head(txt)

install.packages("stringr")
library(stringr)
txt <- str_replace_all(txt, "\\W", " ")

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

system.file(package = "KoNLP")

nouns <- extractNoun(txt)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
library(dplyr)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)

top_20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top_20

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

pal <- brewer.pal(8, "Dark2")

set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

pal <- brewer.pal(9, "Blues")[5:9]
set.seed(1234)

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

twitter <- read.csv("twitter.csv",
                    header = T,
                    fileEncoding = "UTF-8")
twitter <- rename(twitter,
                  no = 번호,
                  id = 계정이름,
                  date = 작성일,
                  tw = 내용)
library(stringr)
twitter$tw <- str_replace_all(twitter$tw, "\\W", " ")
head(twitter$tw)

nouns <- extractNoun(twitter$tw)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)

df_word <- filter(df_word, nchar(word) >= 2)
top20 <- df_word %>%
  arrange(desc(freq)) %>%
  head(20)
top20

library(ggplot2)
order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limit = order) +
  geom_text(aes(label = freq), hjust = 0.1)

pal <- brewer.pal(9, "Blues")[5:9]
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 150,
          random.order = F,
          rot.per = 0,
          scale = c(6, 0.5),
          colors = pal)

install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

library(tibble)
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)
str(crime)

install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)

install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

str(korpop1)
library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")

str(changeCode(kormap1))
str(changeCode(korpop1))
str(korpop1)
str(kormap1)

library(dplyr)
korpop1 <- rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "CP949", "UTF-8")

ggChoropleth(data = korpop1,
             aes(fill = pop,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

ggChoropleth(data = tbc,
             aes(fill = NewPts,
                 map_id = code,
                 tooltip = name),
             map = kormap1,
             interactive = T)

install.packages("plotly")
library(plotly)

library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) + geom_point()

ggplotly(p)

p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) + geom_bar(position = "dodge")
ggplotly(p)

install.packages("dygraphs")
library(dygraphs)
economics <- ggplot2::economics
head(economics)

library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)

dygraph(eco)

dygraph(eco) %>% dyRangeSelector()

eco_a <- xts(economics$psavert, order.by = economics$date)
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)
eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert", "unemploy")
head(eco2)

dygraph(eco2) %>% dyRangeSelector()

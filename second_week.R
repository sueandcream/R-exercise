english <- c(90, 80, 60, 70)
english

math <- c(50, 60, 100, 20)
math

df_midterm <- data.frame(english, math)
df_midterm

class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english)

mean(df_midterm$math)

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm

install.packages("readxl")
library(readxl)

df_exam <- read_excel("excel_exam.xlsx")
df_exam

mean(df_exam$english)
mean(df_exam$math)

df_exam_novar <- read_excel("excel_exam_novar.xlsx")
df_exam_novar

df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar

df_exam_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet

df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam

df_midterm

write.csv(df_midterm, file = "df_midterm.csv")

saveRDS(df_midterm, file = "df_midterm.rds")

rm(df_midterm)

df_midterm
df_midterm <- readRDS("df_midterm.rds")

df_midterm

exam <- read.csv("csv_exam.csv")

head(exam)

head(exam, 10)

tail(exam)
tail(exam, 10)

View(exam)

dim(exam)

str(exam)

summary(exam)

install.packages("ggplot2")
mpg <- as.data.frame(ggplot2::mpg)

head(mpg)

tail(mpg)

dim(mpg)

str(mpg)

summary(mpg)

df_raw <- data.frame(var1 = c(1, 2, 1),
                     var2 = c(2, 3, 2))
df_raw

install.packages("dplyr")
library(dplyr)

df_new <- df_raw
df_new
df_new <- rename(df_new, v2 =  var2)
df_new

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df

df$var_sum <- df$var1 + df$var2
df

df$var_mean <- (df$var1 + df$var2)/2
df

mpg$total <- (mpg$cty + mpg$hwy)/2
head(mpg)
mean(mpg$total)

summary(mpg$total)

hist(mpg$total)

mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")
head(mpg, 20)

table(mpg$total)
table(mpg$test)

library(ggplot2)
qplot(mpg$test)

mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "c"))

head(mpg)

table(mpg$grade)
qplot(mpg$grade)

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

exam %>% filter(class == 1)

exam %>% filter(class == 2)

exam %>% filter(class != 1)

exam %>% filter(math > 50)

exam %>% filter(math < 50)

exam %>% filter(english >= 80)

exam %>% filter(english <= 80)

exam %>% filter(class == 1 & math >= 50)

exam %>% filter(math >= 90 | english >= 90)

exam %>% filter(class == 1 | class == 2 | class == 3)

exam %>% filter(class %in% c(1, 2, 3))

class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)

exam %>% select(math)

exam %>% select(english)

exam %>% select(class, math, english)

exam %>% select(-math)

exam %>% select(-math, -english)

exam %>% filter(class ==1) %>% select(english)

exam %>% 
  filter(class ==1) %>% 
  select(english)

exam %>%
  select(id, math) %>%
  head

exam %>% arrange(math)

exam %>% arrange(desc(math))

exam %>% arrange(class, math)

exam %>%
  mutate(total = math + english + science) %>%
  head
tail(exam$total)

exam %>%
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>%
  head

exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head

exam %>%
  mutate(total = math + english + science) %>%
  arrange(total) %>%
  head

exam %>% summarise(mean_math = mean(math))

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math))

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

mpg %>%
  group_by(manufacturer) %>%
  filter(class == "suv") %>%
  mutate(tot = (cty+hwy)/2) %>%
  summarise(mean_tot = mean(tot)) %>%
  arrange(desc(mean_tot)) %>%
  head(5)

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

test2 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(70, 83, 65, 95, 80))

total <- left_join(test1, test2, by = "id")
total

exam

group_a <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85))

group_b <- data.frame(id = c(6, 7, 8, 9, 10),
                    midterm = c(70, 83, 65, 95, 80))

group_all <- bind_rows(group_a, group_b)
group_all

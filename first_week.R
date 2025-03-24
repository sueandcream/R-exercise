a <- 1
a

b <- 2
b

a+b

var1 <- c(1, 2, 5, 7, 8)
var1

var2 <- c(1:5)
var2

var3 <- seq(1,5)
var3

var4 <- seq(1, 10, by = 2)
var4


var1+2

var1+var2

str1 <- "a"
str1

str2 <- "text"
str2

str3 <- "Hello world!"
str3

str4 <- c("a", "b", "c")
str4

str5 <- c("Hello", "World", "is", "good!")
str5

x <- c(1, 2, 3)
x
mean(x)
max(x)
min(x)

paste(str5, collapse = ",")
paste(str5, collapse = " ")

x_mean <- mean(x)
x_mean

str5_paste <- paste(str5, collapse = " ")
str5_paste

install.packages("ggplot2")
library(ggplot2)

x <- c("a", "a", "b", "c")
x
qplot(x)

qplot(data = mpg, x = hwy)

qplot(data = mpg, x = cty)

qplot(data = mpg, x = drv, y = hwy)

qplot(data = mpg, x = drv, y = hwy, geom = "line")

qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")

qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", colour = drv)

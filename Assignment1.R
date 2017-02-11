dat = read.csv("C:\\Users\\Vytas\\Downloads\\iris_with_missing_data.csv", header = TRUE)

names(dat)[1:2]
names(dat)
summary(dat)

library("psych")
describe(dat)

describe(dat)[1:4,c(1,3)]

describeBy(dat[-5], dat$Species)

#reg1 <- lm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = dat )
reg1 <- lm(Species ~ 1, data = dat)
reg1

regf <- step(reg1,
             direction = "forward",
             scope = (~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width),
             data = dat,
             trace = 0)


summary(regf)

View(dat)
?plot
pairs.panels(dat[c(1:4)], gap = 0)
?pairs.panels

iris.features <- dat[1:4]
iris.features
summary(iris.features)

?kmeans
result <- kmeans(iris.features, 3)

library("rgl")

install.packages("ggvis")
library("ggvis")


?ggvis
dat %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

table(dat$Species)

library(class)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris_norm <- as.data.frame(lapply(dat[1:4], normalize))

summary(iris_norm)



library(ggplot2)
library(GGally)
library(usdm)

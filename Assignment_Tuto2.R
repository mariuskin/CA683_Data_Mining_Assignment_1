dat = read.csv("iris_with_missing_data.csv", header = TRUE)

install.packages("ggplot2")
install.packages("GGally")
install.packages("usdm")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("MASS")
install.packages("Amelia")

library(ggplot2)
library(GGally)
library(usdm)
library(dplyr)
library(gridExtra)
library(MASS)
library(Amelia)

summary(dat)

# specify columns and run amelia

?amelia
amelia_fit <- amelia(dat, m=5, parallel = "multicore", noms = "Species")

iris.imputation1 <- amelia_fit$imputations[[1]]
iris.imputation1[1:4] <- round(iris.imputation1[1:4], digits = 1)

dat <- iris.imputation1

dat.X <- dat[1:4]
dat.y <- dat[5]

dat.X
dat.y


summary(dat)

# 75% train
sample.size <- floor(0.75 * nrow(dat))

# set seed
?set.seed
set.seed(321)

# create index for data partition
index.dat <- sample(seq_len(nrow(dat)), size=sample.size)

# train & test dframes
train.dat <- dat[index.dat,]
test.dat <- dat[-index.dat,]

# test & train vars
train.X <- dat[index.dat, 1:4]
train.y <- dat[index.dat, 5]
test.X <- dat[-index.dat, 1:4]
test.y <- dat[-index.dat, 5]

summary(train.X)

# analyze relationships between variables
ggpairs(train.dat, aes(colour=Species), title="Iris Feature  Relationships") + theme(plot.title = element_text(size=30, face='bold'))



plt1<- ggplot(data=train.dat, aes(x=Petal.Length, y=Petal.Width, colour=Species)) + 
  scale_color_manual(values=c('#9933CC','#00CC00','#FFCC00')) +
  geom_point(size=3) + geom_smooth() +
  
  ggtitle("Petal Width vs Petal Length") +
  xlab("Petal Length") + ylab("Petal Width") +
  annotate('text', x=2,y=2, label="Correlation : 0.958", size=6) +
  
  theme(axis.title.x = element_text(size=16,face='bold'),
        axis.text.x = element_text(size=12, face='bold'),
        axis.title.y = element_text(size=16,face='bold'),
        axis.text.y = element_text(size=12, face='bold'),
        plot.title = element_text(size=28,face='bold'))

plt2<- ggplot(data=train.dat, aes(x=Petal.Length, y=Sepal.Length, colour=Species)) + 
  scale_color_manual(values=c('#9933CC','#00CC00','#FFCC00')) +
  geom_point(size=3) + geom_smooth() + 
  
  ggtitle("Sepal Length vs Petal Length") +
  xlab("Petal Length") + ylab("Sepal Length") +
  annotate('text', x=5.5,y=4.5, label="Correlation : 0.865", size=6) +
  
  theme(axis.title.x = element_text(size=16,face='bold'),
        axis.text.x = element_text(size=12, face='bold'),
        axis.title.y = element_text(size=16,face='bold'),
        axis.text.y = element_text(size=12, face='bold'),
        plot.title = element_text(size=28,face='bold'))

grid.arrange(plt1, plt2)

# Linear Discriminant Analysis

# build model - set prior probability
model.lda <- lda(Species ~ ., data=train.dat, prior=c(1,1,1)/3)

# summarize model
model.lda

# prediction model
pred.model.lda <- predict(model.lda, newdata=test.dat, type='class')

pred.model.lda$class
test.y
pred.model.lda

# prediction table
table(pred.model.lda$class, test.y)

# accuracy of predictions
lda.accuracy <- sum(pred.model.lda$class == test.y)/length(pred.model.lda$class)
lda.accuracy

# dataset to be used for plotting lda
dataset.lda <- data.frame(Species=test.y, lda=pred.model.lda)

# proportion lda
prop.model.lda <- model.lda$svd^2/sum(model.lda$svd^2)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
# visualize lda
ggplot(data=dataset.lda, aes(x=lda.x.LD1, y=lda.x.LD2, colour=Species)) + 
  geom_point(size=3) +
  
  xlab(paste("LD1 (", percent(prop.model.lda[1]), ")", sep="")) +
  ylab(paste("LD2 (", percent(prop.model.lda[2]), ")", sep="")) +
  ggtitle("Linear Discriminant Analysis") +
  scale_color_manual(values=c('#9933CC','#00CC00','#FFCC00')) +
  
  theme(plot.title = element_text(size=32, face='bold'),
        axis.title.x = element_text(size=16, face='bold'),
        axis.title.y = element_text(size=16, face='bold'))

# K-Mean

results.kmean1 <- kmeans(dat.X, 3)
results.kmean2 <- kmeans(dat.X, 3)
results.kmean3 <- kmeans(dat.X, 3)
?kmeans()
results.kmean
results.kmean.cluster1 <- results.kmean1$cluster
results.kmean.cluster2 <- results.kmean2$cluster
results.kmean.cluster3 <- results.kmean3$cluster

# prediction table
table(dat.y$Species, results.kmean.cluster1)
table(dat.y$Species, results.kmean.cluster2)
table(dat.y$Species, results.kmean.cluster3)

# accuracy of predictions

(50+48+36)/(150)
(48+36+50)/150

plot(dat, col = results.kmean.cluster1)

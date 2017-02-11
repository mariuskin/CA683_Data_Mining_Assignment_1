dat = read.csv("C:\\Users\\Vytas\\Downloads\\dat_with_missing_data.csv", header = TRUE)

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

# specify columns and run amelia
?amelia
amelia_fit <- amelia(dat, m=5, parallel = "multicore", noms = "Species")

iris.imputation1 <- amelia_fit$imputations[[1]]
iris.imputation1[1:4] <- round(iris.imputation1[1:4], digits = 1)

dat <- iris.imputation1



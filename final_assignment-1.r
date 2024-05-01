#Using Pearson correlation coefficient
dataset <- read.csv("C:/Users/tarik/Desktop/10 semester/data science/World Happiness Report.csv",
                header=TRUE, sep=",")
dataset



cor.test(dataset$Perceptions.of.corruption,dataset$Overall.rank)

library(ggplot2)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Overall.rank))
Graph1+
  geom_point()

cor.test(dataset$Perceptions.of.corruption,dataset$Country.or.region)

cor.test(dataset$Perceptions.of.corruption,dataset$Score)

Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Score))
Graph1+
  geom_point()

cor.test(dataset$Perceptions.of.corruption,dataset$GDP.per.capita)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=GDP.per.capita))
Graph1+
  geom_point()


cor.test(dataset$Perceptions.of.corruption,dataset$Social.support)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Social.support))
Graph1+
  geom_point()


cor.test(dataset$Perceptions.of.corruption,dataset$Healthy.life.expectancy)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Healthy.life.expectancy))
Graph1+
  geom_point()


cor.test(dataset$Perceptions.of.corruption,dataset$Freedom.to.make.life.choices)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Freedom.to.make.life.choices))
Graph1+
  geom_point()


cor.test(dataset$Perceptions.of.corruption,dataset$Generosity)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Generosity))
Graph1+
  geom_point()


cor.test(dataset$Perceptions.of.corruption,dataset$Perceptions.of.corruption)
Graph1 =ggplot(dataset,aes(x=Perceptions.of.corruption,y=Perceptions.of.corruption))
Graph1+
  geom_point()




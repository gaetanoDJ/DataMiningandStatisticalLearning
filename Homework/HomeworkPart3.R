library(ggplot2)
library(tidyverse)

Cancelledflight = ABIA %>%
  group_by(UniqueCarrier)%>%
  summarize(mean_cancel=mean(Cancelled, na.rm=TRUE))
Cancelledflight

test <- ggplot(data = Cancelledflight, aes(x=UniqueCarrier, y=mean_cancel, fill=UniqueCarrier))+
  geom_bar(stat = "identity")
test

Cancelledflight2 = ABIA %>%
  group_by(Origin)%>%
  summarize(mean_cancel=mean(Cancelled, na.rm=TRUE))
Cancelledflight2

test2 <- ggplot(data = Cancelledflight2, aes(x=Origin, y=mean_cancel, fill=Origin))+
  geom_bar(stat = "identity")
test2

Cancelledflight3 = ABIA %>%
  group_by(Dest)%>%
  summarize(mean_cancel=mean(Cancelled, na.rm=TRUE))
Cancelledflight3

test3 <- ggplot(data = Cancelledflight3[which(Cancelledflight3$mean_cancel>'0.1')], aes(x=Dest, y=mean_cancel, fill=Dest))+
  geom_bar(stat = "identity")
test3




Delayflight = ABIA %>%
  group_by(Origin)%>%
  summarize(mean_Delay=mean(ArrDelay, na.rm=TRUE))
Delayflight
test3 <- ggplot(data = Delayflight, aes(x=Origin, y=mean_Delay, fill=Origin))+
  geom_bar(stat = "identity")
test3

Delayflight1 = ABIA %>%
  group_by(Origin)%>%
  summarize(mean_Delay=mean(DepDelay , na.rm=TRUE))
Delayflight1
test4 <- ggplot(data = Delayflight1, aes(x=Origin , y=mean_Delay, fill=Origin ))+
  geom_bar(stat = "identity")
test4

Delayflight2 = ABIA %>%
  group_by(UniqueCarrier)%>%
  summarize(mean_Delay=mean(ArrDelay, na.rm=TRUE))
Delayflight2
test5 <- ggplot(data = Delayflight2, aes(x=UniqueCarrier , y=mean_Delay, fill=UniqueCarrier))+
  geom_bar(stat = "identity")
test5

Delayflight3 = ABIA %>%
  group_by(UniqueCarrier)%>%
  summarize(mean_Delay=mean(DepDelay, na.rm=TRUE))
Delayflight3
test6 <- ggplot(data = Delayflight3, aes(x=UniqueCarrier, y=mean_Delay, fill=UniqueCarrier))+
  geom_bar(stat = "identity")
test6




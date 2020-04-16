library(tidyverse)
library(ggplot2)

a=read.csv(file = "distritos_dummy.csv")

expand.grid(x =as.factor(1:3),y= as.factor(1:3), id = 1:9) ->df

df$tempo<-round(a$Tempo.de.viagem,2)

df %>% 
  ggplot(aes(x,y))+
  geom_tile(col = "white")+
  facet_wrap(~id,ncol= 3,scales = "free")+
  geom_text(aes(label = tempo))

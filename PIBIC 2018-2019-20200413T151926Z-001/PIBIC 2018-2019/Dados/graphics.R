

#pacotes ----------------------------------------------

library(ggplot2)
library(tidyverse)
library(naniar)
library(car)
library(graphics)

#descricoes de coleta


#funcao de intervalo de confianca

int.conf.t <- function(x, conf = 0.975){
  a <- mean(x)
  s <- sd(x)
   n <- length(x)
 error <- qt(conf,df=n-1)*s/sqrt(n)
return (c(a-error, a+error))
}
#Tratamento dos dados --------------------------------------
dados = read.csv(file = "D:/Pessoal/tcc/PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/ Dados/Bancos/Cloud_SQL_Export_2019-06-10 (00_06_00)alt", header = FALSE, col.names = c("ID","Data","Hour","Day", "OrCoords","OrAdress","DestCoords", "DestAdress", "duration","distance", "fare", "mode"))

dados = dados[dados$ID>400,]

#separacao dos pares latitudelongitude
dados %>%
  separate(col=OrCoords,into = c("OrLat", "OrLong"), sep = ",", remove=TRUE,fill = "right", convert = TRUE) %>%
  separate(col=DestCoords,into = c("DestLat", "DestLong"), sep = ",", remove=TRUE,fill = "right", convert = TRUE) ->dados

#Acertando o fuso, o horario de verão e horarios que passam do registro da meia noite
dados %>%
  mutate(Hour_cut=case_when(as.Date(dados$Data, format = "%d-%m-%Y") < as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 2,
                            as.Date(dados$Data, format = "%d-%m-%Y") > as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 3)) %>%
  mutate(minutecut=as.numeric(substring(as.character(Hour),4,5))) %>%
  mutate(Hour_cut = ifelse(Hour_cut<0, Hour_cut+24,Hour_cut)) %>%
  mutate(Day = ifelse(substring(as.character(Hour),1,2) == "00", 
                      case_when(Day == "Tuesday" ~ "Monday",
                                Day == "Wednesday" ~ "Tuesday",
                                Day == "Thursday" ~ "Wednesday",
                                Day == "Friday" ~ "Thursday",
                                Day == "Saturday" ~ "Friday"), 
                      as.character(Day)))->dados




#Espacializacao dos dados do banco - criacao de um spatialdataframe para os enderecos ---------------------


#de factor para numeric 
dados$OrLat = as.numeric(dados$OrLat) 
dados$OrLong = as.numeric(dados$OrLong)
dados$DestLong = as.numeric(dados$DestLong)
dados$DestLat = as.numeric(dados$DestLat) 

dados %>%
  group_by(Data) %>%
  summarize(count = n()) -> vetor_data

vetor_data$Data <-as.Date(vetor_data$Data, format = "%d-%m-%Y")

datas = seq(as.Date("11-02-2019", format = "%d-%m-%Y"), as.Date("08-06-2019",  format = "%d-%m-%Y"), by="days")
datas <- as.data.frame(as.Date(datas, format = "%d-%m-%Y"))
names(datas) <- "Data"

datas = merge(datas,vetor_data, by.x="Data", by.y="Data", all = TRUE)


for (i in 1:length(datas$Data)){
  datas$count[i] = ifelse (is.na(datas$count[i]) == TRUE, 0,datas$count[i])  
}

#jpeg("Histograma_Registro_Chamadas.jpg", width = 800, height = 400)
plot(datas$count/200 ~ datas$Data, type = "l", xlab = "", ylab = "Nº de chamadas", ylim = c(0,70))
title("Registro de chamadas")
dev.off()
#carregamaento de dados e tratamento de dados------------



dados = read.csv2(file = "D:/Pessoal/tcc/PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Bancos/banco_comparacao_medias.csv", dec = ",")

dados2 <- read.csv2(file = "D:/Pessoal/tcc/PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Bancos/banco_viagens_pareadas.csv", dec = ",")


dados %>%
  mutate(Hour_cut=case_when(as.Date(dados$Data, format = "%d-%m-%Y") < as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 2,
                            as.Date(dados$Data, format = "%d-%m-%Y") > as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 3)) %>%
  mutate(minutecut=as.numeric(substring(as.character(Hour),4,5))) %>%
  mutate(Hour_cut = ifelse(Hour_cut<0, Hour_cut+24,Hour_cut)) %>%
  mutate(Day = ifelse(substring(as.character(Hour),1,2) == "00", 
                      case_when(Day == "Tuesday" ~ "Monday",
                                Day == "Wednesday" ~ "Tuesday",
                                Day == "Thursday" ~ "Wednesday",
                                Day == "Friday" ~ "Thursday",
                                Day == "Saturday" ~ "Friday"), 
                      as.character(Day)))->dados

dados %>%
  mutate(Hourclass = ifelse(minutecut<=30, Hour_cut, Hour_cut+1)) ->dados




dados2 %>%
  mutate(Hour_cut=case_when(as.Date(dados2$Data, format = "%d-%m-%Y") < as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 2,
                            as.Date(dados2$Data, format = "%d-%m-%Y") > as.Date("18-02-2019",format = "%d-%m-%Y") ~ as.numeric(substring(as.character(Hour),1,2)) - 3)) %>%
  mutate(minutecut=as.numeric(substring(as.character(Hour),4,5))) %>%
  mutate(Hour_cut = ifelse(Hour_cut<0, Hour_cut+24,Hour_cut)) %>%
  mutate(Day = ifelse(substring(as.character(Hour),1,2) == "00", 
                      case_when(Day == "Tuesday" ~ "Monday",
                                Day == "Wednesday" ~ "Tuesday",
                                Day == "Thursday" ~ "Wednesday",
                                Day == "Friday" ~ "Thursday",
                                Day == "Saturday" ~ "Friday"), 
                      as.character(Day)))->dados2

dados2 %>%
  mutate(Hourclass = ifelse(minutecut<=30, Hour_cut, Hour_cut+1)) ->dados2


#peqeno experimento da media dos dados de tempo relativo
mean(dados2[dados2$distpublico>1000,]$temprelativo)
k = 100
p = matrix(nrow = k,ncol =5)
for (i in 1:k){
  p[i,] = c((i-1)*1000, 
            mean(dados2[dados2$distpublico>1000*(i-1),]$temprelativo),
            mean(dados2[dados2$distpublico>1000*(i-1) & dados2$distpublico<1000*(i),]$temprelativo),
            mean(dados[dados2$distpublico>1000*(i-1),]$tempodif),
            mean(dados[dados2$distpublico>1000*(i-1) & dados2$distpublico<1000*(i),]$tempodif))

}

#jpeg("Mean_Ratios_through_distance..jpg", width = 800, height = 600)
plot(y=p[,3],x=p[,1]/1000,
            xlab = "Intervals of 1km",
            ylab = "Mean of travel Times Ratios",
            pch = 20,
     xlim = c(0,100),
     ylim = c(0,3.5))
title("Mean of Travel Times Ratios for 1km intervals")
dev.off()

#jpeg("Mean_Diferences_through_distance..jpg", width = 800, height = 600)
plot(y=p[,5],x=p[,1]/1000,
     xlab = "Intervals of 1km",
     ylab = "Mean of Travel Time Diferences",
     pch = 20,
     xlim = c(0,100),
     ylim = c(0,16000))
title("Mean of Travel Time Diferences for 1km intervals")
#dev.off()

#analises dos dados-------------

media_dist=(dados2$distpriv+dados2$distpublico)/2

jpeg(paste("PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/Scatterplot", "Diferencas.jpg", sep = ""), width = 480, height = 480)

scatterplot(y=dados$tempodif,x=((dados2$distpriv+dados2$distpublico)/2),
     xlab = "Média de distâncias entre a viagem pública e a privada",
     ylab = "Diferenças de tempo",
     pch = 20,
     #span = FALSE,
     smooth = F,col = "black",cex.lab = 1.5)

dev.off()
jpeg(paste("PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/graficos/Scatterplot", "TempoRel.jpg", sep = ""), width = 480, height = 480)

scatterplot(y=dados2$temprelativo,x=(dados2$distpriv+dados2$distpublico)/2,
     xlab = "Média de distâncias entre a viagem pública e a privada",
     ylab = "Tempo relativo",
     pch = 20,
     #span = FALSE,
     smooth = F,
     col = "black",
     cex.lab = 1.5)

dev.off()

cor(y=dados$tempodif,x=(dados2$distpriv+dados2$distpublico)/2)

plot(y=dados$tempodif,x=((dados2$distpriv+dados2$distpublico)/2),
     xlab = "Média de distâncias entre a viagem pública e a privada",
     ylab = "Diferenças de tempo",
     pch = 20)

modelo <- lm(I(dados$tempodif-0)~ 0 + media_dist)
abline(0, coef(modelo))

modelo.res <- resid(modelo)
plot(media_dist, modelo.res, 
     xlab="Resíduos", ylab="Diferença", 
     main="Residuos do modelo linear - Diferenças de tempo em relação à distância das viagens") 
abline(0, 0)          

cor(y=dados2$temprelativo,x=(dados2$distpriv+dados2$distpublico)/2)

plot(y=dados2$temprelativo,x=(dados2$distpriv+dados2$distpublico)/2,
            xlab = "Média de distâncias entre a viagem pública e a privada",
            ylab = "Tempo relativo",
            pch = 20)
title("Tempo relativo em relação à distância das viagens")
modelo <- lm(I(dados2$temprelativo-2.3)~ 0 + media_dist)
abline(2.3, coef(modelo))


modelo.res <- resid(modelo)
plot(media_dist, modelo.res, 
     xlab="Resíduos", ylab="Diferença", 
     main="Residuos do modelo linear - Diferenças de tempo em relação à distância das viagens") 
abline(0, 0)   

#jpeg(paste("Scatter", "_Tempo_Tempo_relativo_entreeles.jpg", sep = ""), width = 800, height = 600)
scatterplot(y=dados2$temprelativo,x=dados$tempodif, z = dados2$distpublico,
      xlab = "Diferenças de tempo",
      ylab = "Tempo relativo",
      pch = 20,
      boxplots = "xy",
      smooth = FALSE,
      span = FALSE,
      reg.line = FALSE)
title("Tempo relativo em relação à diferença de tempo")

dev.off()

#histogramas
setwd(dir ="graficos/")

#jpeg(paste("Histograma_Diferenca", "_Tempo.jpg", sep = ""), width = 800, height = 400)
m = mean(dados$tempodif)
std = sqrt(var(dados$tempodif))
grafico1 = hist(scale(dados$tempodif), breaks=50, freq = FALSE,
                xlim = range(-5,5),
                main = expression("Histograma de Diferenças de tempo de viagem (T"["público"]*"-T"[privado]*")"),
                xlab = "Diferenças", ylab = "Freqüencia",
                axes = TRUE, plot = TRUE, labels = FALSE)
curve(dt(x, df = nrow(dados)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off()

int.conf.t(dados$tempodif)

#jpeg(paste("Histograma_Diferenca","_Distancia.jpg", sep = ""), width = 800, height = 400)
m = mean(dados$distdif)
std = sqrt(var(dados$distdif))
grafico1 = hist(scale(dados$distdif), breaks=200, freq = FALSE,
                xlim = range(-5,5),
                main = expression("Histograma de Diferenças de distâncias (D"["público"]*"-D"[privado]*")"),
                xlab = "Diferenças", ylab = "Freqüencia",
                axes = TRUE, plot = TRUE, labels = FALSE)
curve(dt(x, df = nrow(dados)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off()

#jpeg(paste("Histograma_Diferenca","_Velocidade.jpg", sep =""), width = 800, height = 400)
m = mean(dados$veldif)
std = sqrt(var(dados$veldif))
grafico1 = hist(scale(dados$veldif), breaks=500, freq = FALSE,
                xlim = range(-5,5),
                main = expression("Histograma de Diferenças de Velocidades (V"["público"]*"-V"[privado]*")"),
                xlab = "Diferenças", ylab = "Freqüencia",
                axes = TRUE, plot = TRUE, labels = FALSE)
curve(dt(x, df = nrow(dados)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off()

#jpeg(paste("Histograma_Tempo_Relativo.jpg", sep =""), width = 800, height = 400)
m = mean(dados2$temprelativo)
std = sqrt(var(dados2$temprelativo))
grafico1 = hist(scale(dados2$temprelativo), breaks=50, freq = FALSE,
                xlim = range(-5,5),
                main = expression("Histograma de tempos relativos (T"["público"]*"/T"[privado]*")"),
                xlab = "Tempos relativos", ylab = "Freqüencia",
                axes = TRUE, plot = TRUE, labels = FALSE)
curve(dt(x, df = nrow(dados)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
dev.off()
int.conf.t(dados2$temprelativo)

for (i in (5:21)){
  aux = dados[dados$Hourclass==i,]
  aux = aux[is.na(aux$ID) != TRUE,]  
  
  aux2 = dados2[dados$Hourclass==i,]
  aux2 = aux2[is.na(aux2$ID) != TRUE,]
  
  #jpeg(paste("Histograma_Diferenca_Tempo_", as.character(i),"h.jpg", sep = ""), width = 800, height = 400)
  m = mean(aux$tempodif)
  std = sqrt(var(aux$tempodif))
  grafico1 = hist(scale(aux$tempodif), breaks=100, freq = FALSE,
                  xlim = range(-5,5),
                  main = expression("Histograma de Diferenças de tempo de viagem (T"["público"]*"-T"[privado]*")"),
                  sub = paste(as.character(i), "horas", sep = " "),
                  xlab = "Diferenças", ylab = "Freqüencia",
                  axes = TRUE, plot = TRUE, labels = FALSE)
  curve(dt(x, df = nrow(dados)), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  #dev.off()
  
  #jpeg(paste("Histograma_Diferenca_Distancia_", as.character(i),"h.jpg", sep = ""), width = 800, height = 400)
  m = mean(aux$distdif)
  std = sqrt(var(aux$distdif))
  grafico1 = hist(scale(aux$distdif), breaks=100, freq = FALSE,
                  xlim = range(-5,5),
                  main = expression("Histograma de Diferenças de distâncias (D"["público"]*"-D"[privado]*")"),
                  sub = paste(as.character(i), "horas", sep = " "),
                   xlab = "Diferenças", ylab = "Freqüencia",
                  axes = TRUE, plot = TRUE, labels = FALSE)
  curve(dt(x, df = nrow(dados)), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  #dev.off()
  
  #jpeg(paste("Histograma_Diferenca_Velocidade_", as.character(i),"h.jpg", sep = ""), width = 800, height = 400)
  m = mean(aux$veldif)
  std = sqrt(var(aux$veldif))
  grafico1 = hist(scale(aux$veldif), breaks=100, freq = FALSE,
                  xlim = range(-5,5),
                  main = expression("Histograma de Diferenças de velocidades  (V"["público"]*"-V"[privado]*")"),
                  sub = paste(as.character(i), "horas", sep = " "),
                  xlab = "Diferenças", ylab = "Freqüencia",
                  axes = TRUE, plot = TRUE, labels = FALSE)
  curve(dt(x, df = nrow(dados)), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  dev.off()
  
  #jpeg(paste("Histograma_Tempo_Relativo_", as.character(i), "h.jpg", sep =""), width = 800, height = 400)
  m = mean(aux2$temprelativo)
  std = sqrt(var(aux2$temprelativo))
  grafico1 = hist(scale(aux2$temprelativo), breaks=50, freq = FALSE,
                  xlim = range(-5,5),
                  main = expression("Histograma de tempos relativos (T"["público"]*"/T"[privado]*")"),
                  sub = paste(as.character(i), "horas", sep = " "),
                  xlab = "Tempos relativos", ylab = "Freqüencia",
                  axes = TRUE, plot = TRUE, labels = FALSE)
  curve(dt(x, df = nrow(dados)), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
  #dev.off()
}

#boxplot diferenca de temp com os dias

boxpl_dias = list()
count.dias = unique(dados[is.na(dados$Day) == F  & 
                            dados$Day != "NORESULTS" &
                            dados$Day != "Saturday"&
                            dados$Day != "Sunday",]$Day)

for (i in 1:length(count.dias)){
  
  boxpl_dias[[i]] = dados[dados$Data != "NORESULTS"  & dados$Day == count.dias[i],]$tempodif
}

jpeg(paste("Boxplot_Diferenca_Tempo_Dias",".jpg", sep = ""), width = 800, height = 400)
boxplot(boxpl_dias, names = c("Segunda", "Terça", "Quarta", "Quinta", "Sexta"))
title(main = expression("Diferença de tempos de viagem (T"["público"]*"-T"[privado]*") por dia"),
      ylab = "Diferenças")
dev.off()

#boxplot diferenca de temprelativo com os dias

boxpl_dias = list()
count.dias = unique(dados2[is.na(dados2$Day) == F  & 
                            dados2$Day != "NORESULTS" &
                            dados2$Day != "Saturday"&
                            dados2$Day != "Sunday",]$Day)

for (i in 1:length(count.dias)){
  
  boxpl_dias[[i]] = dados2[dados2$Data != "NORESULTS"  & dados2$Day == count.dias[i],]$temprelativo
}

jpeg(paste("Boxplot_Diferenca_Temprelativo_Dias",".jpg", sep = ""), width = 800, height = 400)
boxplot(boxpl_dias, names = c("Segunda", "Terça", "Quarta", "Quinta", "Sexta"))
title(main = expression("Tempos relativos(T"["público"]*"/T"[privado]*") por dia"),
      ylab = "Tempos relativos")
dev.off()

#boxplot diferenca do tempo com as horas

boxpl_horas = list()
count.horas = unique(dados[is.na(dados$Hourclass) == F  & 
                            dados$Hourclass != "NORESULTS" &
                            dados$Hourclass != "NORESULT",]$Hourclass)

count.horas = sort(count.horas)

for (i in 1:length(count.horas)){
  
  boxpl_horas[[i]] = dados[is.na(dados$Hourclass) == F & dados$Hourclass == count.horas[i],]$tempodif
}

for (i in 1:length(count.horas)){
  count.horas[i] = paste(count.horas[i], "h", sep = "")
}

jpeg(paste("Boxplot_Diferenca_Tempo_Tempo_Relativo_Horas",".jpg", sep = ""), width = 800, height = 600)
layout(mat = matrix(c(1,1,2,2), byrow = TRUE, nrow=2 ))
boxplot(boxpl_horas, names = count.horas)
title(main = expression("Diferenças de tempo de viagem (T"["público"]*"-T"[privado]*") por hora"),
      ylab = "Diferenças")



#boxplot do tempo relativo com as horas

boxpl_horas = list()
count.horas = unique(dados2[is.na(dados2$Hourclass) == F  & 
                             dados2$Hourclass != "NORESULTS" &
                             dados2$Hourclass != "NORESULT",]$Hourclass)

count.horas = sort(count.horas)

for (i in 1:length(count.horas)){
  
  boxpl_horas[[i]] = dados2[is.na(dados2$Hourclass) == F & dados2$Hourclass == count.horas[i],]$temprelativo
}

for (i in 1:length(count.horas)){
  count.horas[i] = paste(count.horas[i], "h", sep = "")
}


boxplot(boxpl_horas, names = count.horas)
title(main = expression("Tempos relativos (T"["público"]*"/T"[privado]*") por hora"),
      ylab = "Tempos relativos")
dev.off()

#boxplot diferenca da distancia com os dias

boxpl_dias = list()
count.dias = unique(dados[is.na(dados$Day) == F  & 
                            dados$Day != "NORESULTS" &
                            dados$Day != "Saturday"&
                            dados$Day != "Sunday",]$Day)

for (i in 1:length(count.dias)){
  
  boxpl_dias[[i]] = dados[dados$Data != "NORESULTS"  & dados$Day == count.dias[i] & dados$distdif<10000,]$distdif
}

jpeg(paste("Boxplot_Diferenca_Distancia_Dias",".jpg", sep = ""), width = 800, height = 400)
boxplot(boxpl_dias, names = count.dias)
title(main = expression("Diferenças de distâncias (D"["público"]*"-D"[privado]*") por dia"),
      ylab = "Diferenças")
dev.off()

#boxplot diferenca da distancia com as horas

boxpl_horas = list()
count.horas = unique(dados[is.na(dados$Hourclass) == F  & 
                             dados$Hourclass != "NORESULTS" &
                             dados$Hourclass != "NORESULT",]$Hourclass)

count.horas = sort(count.horas)

for (i in 1:length(count.horas)){
  
  boxpl_horas[[i]] = dados[dados$Data != "NORESULTS" & dados$Hourclass == count.horas[i] & dados$distdif<10000,]$distdif
}

for (i in 1:length(count.horas)){
  count.horas[i] = paste(count.horas[i], "h", sep = "")
}

jpeg(paste("Boxplot_Diferenca_Distancia_Horas",".jpg", sep = ""), width = 800, height = 400)
boxplot(boxpl_horas, names = count.horas)
title(main = expression("Diferenças de distância (T"["público"]*"-T"[privado]*") por hora"),
      ylab = "Diferenças")
dev.off()


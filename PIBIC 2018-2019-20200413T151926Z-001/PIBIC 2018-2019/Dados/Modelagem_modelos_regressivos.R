setwd(dir ="~/Documentos/Programação/PIBIC 2018-2019/Dados/")

#pacotes ----------------------------------------------

library(tidyverse)
library(car)
library(rgdal)
library(caret)
library(leaps)
library(MASS)


#carregamento do banco de dados - shapefile concatenado

dados <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/mapa_Origem_tempo_relativo_distritos.shp",layer = "mapa_Origem_tempo_relativo_distritos")

#modelagens tempo relativo
#modelagem regressao simples ----------------

modelo_inteiro <- lm(dados$V4 ~dados$QTLbus2018+dados$KMLIN_2018+dados$QPbus_2018+
                       dados$Qmetr_2018+dados$RENDP2010+dados$ARE1+dados$POP2018+
                       dados$DENPOP2018+dados$DOMP2018+dados$ESTAB2016+dados$EMP2016+
                       dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010+dados$PDOMM2010, data =dados)
plot(modelo_inteiro)
summary(modelo_inteiro)
#retirada a partir do StepAIC de variaveis excedentes
modelo_novo <- stepAIC(modelo_inteiro,direction = "both")

plot(modelo_novo)
summary(modelo_novo)

#retirada de variáveis por ignificancia e depois multicolinearidade
modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+dados$ARE1+
                              dados$DENPOP2018+dados$ESTAB2016+dados$EMP2016+
                              dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional
modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+dados$ARE1+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area

modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus

modelo_novo_alterado <- lm(V4~dados$RENDP2010+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus, emprego

modelo_novo_alterado <- lm(V4~dados$RENDP2010+
                             dados$ESTAB2016+
                             dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus, emprego, renda

modelo_novo_alterado <- lm(V4~dados$ESTAB2016+
                             dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus, emprego, renda, pop nao branca

modelo_novo_alterado <- lm(V4~dados$ESTAB2016+
                             dados$temp_medio+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#modelagem regressao simples sem variaveis explicativas de tempo --------- 

modelo_inteiro <- lm(dados$V4 ~dados$QTLbus2018+dados$KMLIN_2018+dados$QPbus_2018+
                       dados$Qmetr_2018+dados$RENDP2010+dados$ARE1+dados$POP2018+
                       dados$DENPOP2018+dados$DOMP2018+dados$ESTAB2016+dados$EMP2016+
                       dados$PNBRAN2010+dados$PDOMC2010+dados$PDOMM2010, data =dados)
summary(modelo_inteiro)
#retirada a partir do StepAIC de variaveis excedentes
modelo_novo <- stepAIC(modelo_inteiro,direction = "both")

summary(modelo_novo)

#retirada de variáveis por ignificancia e depois multicolinearidade
modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+dados$ARE1+
                             dados$DENPOP2018+dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional
modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+dados$ARE1+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area

modelo_novo_alterado <- lm(V4~dados$QTLbus2018+dados$RENDP2010+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus

modelo_novo_alterado <- lm(V4~dados$RENDP2010+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus, emprego

modelo_novo_alterado <- lm(V4~dados$RENDP2010+
                             dados$ESTAB2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#tirando densidade populacional, area, quantidade linhas de onibus, emprego, renda

modelo_novo_alterado <- lm(V4~dados$ESTAB2016+
                             dados$PNBRAN2010+dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)




#carregamento do banco de dados - shapefile concatenado

dados2 <- readOGR(dsn = "/home/rodolfo/Documentos/Programação/PIBIC 2018-2019/Dados/Geo/mapa_Origem_diferenca_tempo_distritos.shp",layer = "mapa_Origem_diferenca_tempo_distritos")


dados@data<-merge(dados@data,dados2@data, by = "ds_nome")


#modelagens diferenca tempo
#modelagem regressao simples ----------------

modelo_inteiro <- lm(dados$V4.y ~dados$QTLbus2018+dados$KMLIN_2018+dados$QPbus_2018+
                       dados$Qmetr_2018+dados$RENDP2010+dados$ARE1+dados$POP2018+
                       dados$DENPOP2018+dados$DOMP2018+dados$ESTAB2016+dados$EMP2016+
                       dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010+dados$PDOMM2010, data =dados)
plot(modelo_inteiro)
summary(modelo_inteiro)
#retirada a partir do StepAIC de variaveis excedentes
modelo_novo <- stepAIC(modelo_inteiro,direction = "both")

plot(modelo_novo)
summary(modelo_novo)

#retirada de variáveis por significancia e depois multicolinearidade
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$RENDP2010+dados$ARE1+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010+dados$PDOMM2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de renda
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$ARE1+
                             dados$ESTAB2016+dados$EMP2016+
                             dados$PNBRAN2010+dados$PDOMC2010+dados$PDOMM2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de renda, emprego
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$ARE1+
                             dados$ESTAB2016+
                             dados$PNBRAN2010+dados$PDOMC2010+dados$PDOMM2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de renda, emprego, estabelecimentos
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$ARE1+
                             dados$PNBRAN2010+dados$PDOMC2010+dados$PDOMM2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de renda, emprego, estabelecimentos, domicilios carro
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$ARE1+
                             dados$PNBRAN2010+dados$PDOMM2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#modelagem regressao simples - sem area ----------------

modelo_inteiro <- lm(dados$V4.y ~dados$QTLbus2018+dados$KMLIN_2018+dados$QPbus_2018+
                       dados$Qmetr_2018+dados$RENDP2010+dados$POP2018+
                       dados$DENPOP2018+dados$DOMP2018+dados$ESTAB2016+dados$EMP2016+
                       dados$PNBRAN2010+dados$temp_medio+dados$PDOMC2010+dados$PDOMM2010, data =dados)
plot(modelo_inteiro)
summary(modelo_inteiro)
#retirada a partir do StepAIC de variaveis excedentes
modelo_novo <- stepAIC(modelo_inteiro,direction = "both")

plot(modelo_novo)
summary(modelo_novo)

#retirada de variáveis por significancia e depois multicolinearidade
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$POP2018+dados$DENPOP2018+
                             dados$DOMP2018+dados$ESTAB2016+dados$PNBRAN2010+
                             dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de estabelecimentos
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$POP2018+dados$DENPOP2018+
                             dados$DOMP2018+dados$PNBRAN2010+
                             dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de estabelecimentos, domicilios
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$POP2018+dados$DENPOP2018+
                             dados$PNBRAN2010+
                             dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

#retirada de estabelecimentos, domicilios, populacao
modelo_novo_alterado <- lm(V4.y~dados$QTLbus2018+dados$DENPOP2018+
                             dados$PNBRAN2010+
                             dados$PDOMC2010, data = dados)

summary(modelo_novo_alterado)
vif(modelo_novo_alterado)

simulation_data_to_sp <- function(origem = T){
  require(dplyr)
  require(car)
  require(tidyr)
  source('502_simulation_processing_functions.R')
  
  #Tratamento dos dados --------------------------------------
  dados = read.csv(file = "PIBIC 2018-2019-20200413T151926Z-001/PIBIC 2018-2019/Dados/Bancos/Cloud_SQL_Export_2019-06-10 (00_06_00)alt", header = FALSE, col.names = c("ID","Data","Hour","Day", "OrCoords","OrAdress","DestCoords", "DestAdress", "duration","distance", "fare", "mode"), encoding = 'UTF-8')
  
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
  
  
  #Criando banco para comparacao de medias
  
  dados_pub = dados[dados$mode == "traffic" | dados$mode == "traffic2",]
  dados_priv = dados[dados$mode == "private" | dados$mode == "private2",]
  
  dados_priv$ID = dados_priv$ID-100
  
  dados_novo = merge(dados_pub,dados_priv, by = c("ID","ID"))
  
  dados_novo = dados_novo[c(1,2,3,4,5,6,7,8,9,10,11,12,14,26,27,29,30,31)]
  names(dados_novo) <- c("ID", "Data", "Hour", "Day", "OrLat", "OrLong", "OrAdress",  
                               "DestLat", "DestLong", "DestAdress", "temppublico", "distpublico", "modepub", "temppriv",
                               "distpriv", "modepriv", "Hour_cut", "minutecut" )

  
  return(convert_spatial(dados_novo, origem = origem))}
  
  
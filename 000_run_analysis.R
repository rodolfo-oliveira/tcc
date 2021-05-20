#abertura e filtragem do banco de dados da pesquisa OD
source('001_opening_filtering_OD.R', encoding = 'UTF-8')

#espacialização do banco OD - transformação em sp
source('002_spatialize_OD.R', encoding = 'UTF-8')

#espacialização do baco de dados simulados - transformação em sp
source('003_spatialize_simulated_data.R', encoding = 'UTF-8')

#criação de banco de dados para analise - calculo de diferença e razão entre os tempos de viagens simuladas e viagens OD
source('004_creating_analysis_databases.R', encoding = 'UTF-8')

#agrupamento dos dados para as zonas OD
source('005_grouping_data_OD_zones.R', encoding = 'UTF-8')

#função que imprime os mapas e diagramas de autocorrelação espacial
source('006_plotting_LISA_Moran.R', encoding = 'UTF-8')

#script para a análise quantitativa dos dodos - regressões lineares e espaciais
source('007_modelling.R', encoding = 'UTF-8')

##filtering time of high traffic - TODO


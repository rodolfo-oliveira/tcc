library(dplyr)


#lendo base da OD
database_base <- foreign::read.dbf('OD 2017/Banco de dados/OD_2017.dbf')

#filtando variaveis de interesse
#c(CONDMORA, CRITERIOBR, GRAU_INS, VL_REN_I, TOT_VIAG, CO_O_X, CO_O_Y, CO_D_X, CO_D_Y, MOTIVO_O, MOTIVO_D, MODOPRIN)
database <- database_base[,c('CONDMORA', 'CRITERIOBR', 'GRAU_INS',
                        'VL_REN_I', 'TOT_VIAG', 'CO_O_X',
                        'CO_O_Y', 'CO_D_X', 'CO_D_Y',
                        'MOTIVO_O', 'MOTIVO_D', 'MODOPRIN','DURACAO',
                        'ANDA_O', 'ANDA_D', "MUNI_O", "MUNI_D")]

#viagens de transporte motorizado (carro, proprio ou 'alugado')
database_privado <-database[database$MODOPRIN %in% 9:12,]

#viagens de transporte motorizado (moto, dirigindo ou passageiro)
database_privado_moto <-database[database$MODOPRIN %in% 13:14,]

#viagens de transporte público - todos os modais (sem fretados privados e sem trasnporte escolar)
database_publico <- database[database$MODOPRIN %in% 1:6,]

#viagens de transporte fretados privados
database_fretados <- database[database$MODOPRIN %in% 7,]

#viagens de transporte escolar
database_escolar <- database[database$MODOPRIN %in% 8,]


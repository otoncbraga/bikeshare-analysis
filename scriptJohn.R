###############################
######## INFORMAÇÕES ##########
###############################

#Q1: 2017-01-01 a 2017-03-31
#Q2: 2017-04-01 a 2017-06-30
#Q3: 2017-07-01 a 2017-09-30
#Q4: 2017-10-01 a 2017-12-31

#DataSet com as estações
#name <- c("Spring", "Summer", "Fall", "Winter", "Winter")
#start <- c("2017-03-21","2017-06-21","2017-09-21","2017-12-21","2017-01-01")
#end <- c("2017-06-20","2017-09-20","2017-12-20","2017-12-31","2017-03-20")
#season <- data.frame(name,start,end)

#Monday, January 2, 2017  New Year's Day**
#Monday, January 16, 2017  Martin Luther King Jr. Day
#Friday, January 20, 2017 Inauguration Day***
#Monday, February 20, 2017 Washington's Birthday
#Monday, April 17, 2017 DC Emancipation Day**
#Monday, May 29, 2017 Memorial Day
#Tuesday, July 4, 2017  Independence Day
#Monday, September 4, 2017  Labor Day
#Monday, October 9, 2017  Columbus Day
#Friday, November 10, 2017  Veterans Day*
#Thursday, November 23, 2017  Thanksgiving Day
#Monday, December 25, 2017  Christmas Day

###############################
########## COMANDOS ###########
###############################

#para formar dataFrame apartir de outro
#(dados = data.frame(x,y,outraTabela))

#mostra o tipo de objeto
#class(nomeDoObjeto)

#para renomear colunas
#names(dados) = c("Variável 1", "Variável 2")

#rbind() para juntar linhas / cbind() para juntar colunas / merge()

#Formatar data para dia da semana
#format.Date(as.Date(datas), "%a") # abreviado
#format.Date(as.Date(datas), "%A") # nome do dia completo

#duas funções acima são equivalentes às seguintes:
#weekdays(as.Date(datas), abbreviate = TRUE) ou weekdays(as.Date(datas))

###############################
######## PARA APRENDER ########
###############################
#[X] Juntar arquivos em um só
#[X] Exportar para csv
#[X] Adicionar coluna com dia da semana
#[ ] Adicionar coluna com estação do ano
#[ ] Adicionar coluna com informação de dia útil

###############################
##### PRÉ-PROCESSAMENTO #######
###############################

#Instalar bibliotecas
install.packages("ggplot2"); #gerar gráficos
install.packages("sqldf"); #reconhecer comandos SQL
install.packages("dplyr"); #Transformação/manipulação de dados
install.packages("lubridate");

#Carregar bibliotecas
library("ggplot2");
library("sqldf");
library("dplyr");
library("lubridate");

Q1 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q1-capitalbikeshare-tripdata.csv");
Q2 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q2-capitalbikeshare-tripdata.csv");
Q3 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q3-capitalbikeshare-tripdata.csv");
Q4 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q4-capitalbikeshare-tripdata.csv");

#Juntar as 4 partes em um único lugar
QTOTAL <- rbind(rbind(Q1, Q2), rbind(Q3, Q4));

#Adicionar coluna com dia do mês, mês, ano, dia da semana (1 a 7), dia da semana (nome)
QTOTAL <- QTOTAL %>% mutate(dia = mday(Start.date), mes = month(Start.date), ano = year(Start.date), weekday = wday(Start.date), weekday_name = wday(Start.date, label=TRUE, abbr = FALSE))

#Adicionar estações do ano [[[AINDA NÃO ESTÁ FUNCIONANDO]]]
#Filtra Estações
#________________________
# Primavera: 21 mar - 20 jun
Primavera <- QTOTAL %>% filter(Start.date>=as.Date("2017-03-21") & Start.date<=as.Date("2017-06-20"))
Primavera <- Primavera %>% mutate(season = "spring")
#________________________
# Verão: 21 jun - 20 set
Verão <- QTOTAL %>% filter(Start.date>=as.Date("2017-06-21") & Start.date<=as.Date("2017-09-20"))
Verão <- Verão %>% mutate(season = "summer")
#________________________
# Outono: 21 set - 20 dez
Outono <- QTOTAL %>% filter(Start.date>=as.Date("2017-09-21") & Start.date<=as.Date("2017-12-20"))
Outono <- Outono %>% mutate(season = "fall")
#________________________
# Inverno: 21 dez - 20 mar
Inverno <- filter(QTOTAL, (Start.date>=as.Date("2017-12-21") & Start.date<=as.Date("2017-12-31")) & (Start.date>=as.Date("2017-01-01") & Start.date<=as.Date("2017-03-20")))
Inverno <- Inverno %>% mutate(season = "winter")
#Junta arquivos
QTOTAL <- rbind(rbind(Primavera, Verão), rbind(Outono, Inverno));

#exportar tabela para arquivo csv
write.csv(QTOTAL, "tabela.csv")

###############################
######### RESULTADOS ##########
###############################

#TOTAL DE ALUGUEIS POR DIA DA SEMANA
#!![Quarta-feira é o dia com mais aluguéis]!!
TotalAlugueisPorDiaSemana <- QTOTAL %>% group_by(weekday_name) %>% summarize(count_all = n());
ggplot(data=TotalAlugueisPorDiaSemana) + geom_bar(mapping = aes(x=weekday_name,y=count,group=1), stat="identity")

#!![Usuários casuais utilizam mais nos sábados e domingos]!!
AlUgueisCasuais <- filter(QTOTAL, Member.type=="Casual")
TotalAlugueisPorDiaSemanaCasuais <- AlUgueisCasuais %>% group_by(weekday_name) %>% summarize(count_casual = n())
ggplot(data=TotalAlugueisPorDiaSemanaCasuais) + geom_bar(mapping = aes(x=weekday_name,y=count,group=1), stat="identity")

#!![Usuários registrados utilizam mais de segunda a sexta]!!
AlugueisMembros <- filter(QTOTAL, Member.type=="Member")
TotalAlugueisPorDiaSemanaMembros <- AlugueisMembros %>% group_by(weekday_name) %>% summarize(count_membro = n())
ggplot(data=TotalAlugueisPorDiaSemanaMembros) + geom_bar(mapping = aes(x=weekday_name,y=count,group=1), stat="identity")

#Fazer o merge em TotalAlugueisPorDiaSemanaDetalhado, com o total dos dois tipos de usuario, de casual e de membro, pelo dia da semana
TotalAlugueisPorDiaSemanaDetalhado = merge(TotalAlugueisPorDiaSemana, (merge(TotalAlugueisPorDiaSemanaCasuais, TotalAlugueisPorDiaSemanaMembros, by="weekday_name")),by="weekday_name")
#Mostrar dduas linhas msotrando diferença. [[Ainda não está funcionando]]
#ggplot(data=TotalAlugueisPorDiaSemanaDetalhado) + geom_bar(mapping = aes(x=weekday_name, y=count_casual)) + geom_bar(mapping = aes(x=weekday_name, y=count_membro))
#ggplot(data=TotalAlugueisPorDiaSemanaDetalhado) + geom_smooth(mapping = aes(x=weekday_name, y=count_casual)) + geom_smooth(mapping = aes(x=weekday_name, y=count_membro, color="blue"))

#=======================================#
#MÉDIA DE ALUGUÉIS POR DIA DA SEMANA


#=======================================#
#MÉDIA DE ALUGUÉIS POR MÊS



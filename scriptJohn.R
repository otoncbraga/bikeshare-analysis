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


#remover labels do grafico
#theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

###############################
##### PRÉ-PROCESSAMENTO #######
###############################

#Instalar bibliotecas
install.packages("ggplot2"); #gerar gráficos
install.packages("sqldf"); #reconhecer comandos SQL
install.packages("dplyr"); #Transformação/manipulação de dados
install.packages("lubridate");
install.packages('rpart',dependencies = T)

#Carregar bibliotecas
library("ggplot2");
library("sqldf");
library("dplyr");
library("lubridate");
library("rpart");

#Remover notação científica dos gráficos
options(scipen = 999)


#Q1 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q1-capitalbikeshare-tripdata.csv");
#Q2 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q2-capitalbikeshare-tripdata.csv");
#Q3 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q3-capitalbikeshare-tripdata.csv");
#Q4 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q4-capitalbikeshare-tripdata.csv");

trip2017 <- read.csv(file.choose(), sep=',');
agt_hour <- read.csv(file.choose(), sep=',');

#Detalha valores da tabela
summary(agt_hour, digits=3)
summary(trip2017)

#Duração média dos aluguéis
summary(trip2017$duration)


sqldf("select distinct season from trip2017 group by season")


#Juntar as 4 partes em um único lugar
#QTOTAL <- rbind(rbind(Q1, Q2), rbind(Q3, Q4));

#Adicionar coluna com dia do mês, mês, ano, dia da semana (1 a 7), dia da semana (nome)
#QTOTAL <- QTOTAL %>% mutate(dia = mday(Start.date), mes = month(Start.date), ano = year(Start.date), weekday = wday(Start.date), weekday_name = wday(Start.date, label=TRUE, abbr = FALSE))

#Adicionar estações do ano [[[AINDA NÃO ESTÁ FUNCIONANDO]]]
#Filtra Estações
#________________________
# Primavera: 21 mar - 20 jun
#Primavera <- QTOTAL %>% filter(Start.date>=as.Date("2017-03-21") & Start.date<=as.Date("2017-06-20"))
#Primavera <- Primavera %>% mutate(season = "spring")
#________________________
# Verão: 21 jun - 20 set
#Verão <- QTOTAL %>% filter(Start.date>=as.Date("2017-06-21") & Start.date<=as.Date("2017-09-20"))
#Verão <- Verão %>% mutate(season = "summer")
#________________________
# Outono: 21 set - 20 dez
#Outono <- QTOTAL %>% filter(Start.date>=as.Date("2017-09-21") & Start.date<=as.Date("2017-12-20"))
#Outono <- Outono %>% mutate(season = "fall")
#________________________
# Inverno: 21 dez - 20 mar
#Inverno <- filter(QTOTAL, (Start.date>=as.Date("2017-12-21") & Start.date<=as.Date("2017-12-31")) & (Start.date>=as.Date("2017-01-01") & Start.date<=as.Date("2017-03-20")))
#Inverno <- Inverno %>% mutate(season = "winter")
#Junta arquivos
#QTOTAL <- rbind(rbind(Primavera, Verão), rbind(Outono, Inverno));

#exportar tabela para arquivo csv
#write.csv(QTOTAL, "tabela.csv")

###############################
######### RESULTADOS ##########
###############################

#TOTAL DE ALUGUEIS POR DIA DA SEMANA
#!![Quarta-feira é o dia com mais aluguéis]!!
TotalAlugueisPorDiaSemana <- QTOTAL %>% group_by(weekday_name) %>% summarize(count_all = n());
ggplot(data=TotalAlugueisPorDiaSemana) + geom_bar(mapping = aes(x=weekday_name,y=count_all,group=1, fill=weekday_name), stat="identity")

TotalAlugueisPorEstacao <- trip2017 %>% group_by(season) %>% summarize(count_all = n());
ggplot(data=TotalAlugueisPorEstacao) + geom_bar(mapping = aes(x=season,y=count_all,group=1, fill=season), stat="identity")+ xlab("Estação do ano") + ylab("Total de aluguéis") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())


ggplot(data=TotalAlugueisPorDiaSemana) + geom_point(mapping = aes(x=weekday_name,y=count_all))

#!![Usuários casuais utilizam mais nos sábados e domingos]!!
AlUgueisCasuais <- filter(QTOTAL, Member.type=="Casual")
TotalAlugueisPorDiaSemanaCasuais <- AlUgueisCasuais %>% group_by(weekday_name) %>% summarize(count_casual = n())
ggplot(data=TotalAlugueisPorDiaSemanaCasuais) + geom_bar(mapping = aes(x=weekday_name,y=count_casual,group=1), stat="identity")

#!![Usuários registrados utilizam mais de segunda a sexta]!!
AlugueisMembros <- filter(QTOTAL, Member.type=="Member")
TotalAlugueisPorDiaSemanaMembros <- AlugueisMembros %>% group_by(weekday_name) %>% summarize(count_membro = n())
ggplot(data=TotalAlugueisPorDiaSemanaMembros) + geom_bar(mapping = aes(x=weekday_name,y=count_membro,group=1), stat="identity")

#Gráfico unificado
count_usuarios_dia <- sqldf("select type, day, count (*) as total from trip2017 group by type, day")
ggplot(data=count_usuarios_dia) + geom_bar(mapping = aes(x=as.factor(day),y=total,group=1, fill=type), stat="identity") + xlab("day") + ylab("total") + theme(legend.position = "none")

ggplot(count_usuarios_dia, fill=type) + geom_bar(mapping = aes(x=day, fill=type), position = "dodge")
ggplot(data=count_usuarios_dia) + geom_bar(mapping = aes(x=day,y=total,group=1, fill=type), position="dodgedentity")


count_usuarios_mes <- sqldf("select type, month, count (*) as total from trip2017 group by type, month")
ggplot(data=count_usuarios_mes) + geom_bar(mapping = aes(x=as.factor(month),y=total,group=1, fill=type), stat="identity") + xlab("month") + ylab("total")


#Fazer o merge em TotalAlugueisPorDiaSemanaDetalhado, com o total dos dois tipos de usuario, de casual e de membro, pelo dia da semana
TotalAlugueisPorDiaSemanaDetalhado = merge(TotalAlugueisPorDiaSemana, (merge(TotalAlugueisPorDiaSemanaCasuais, TotalAlugueisPorDiaSemanaMembros, by="weekday_name")),by="weekday_name")

#=======================================#
#MÉDIA DE ALUGUÉIS POR DIA DA SEMANA


#=======================================#
#TOTAL DE ALUGUÉIS POR MÊS
#!![Inverno é a estação com menos alugueis. Principalmente em Dezembro e Janeiro]!!
TotalAlugueisPorMes <- trip2017 %>% group_by(month) %>% summarize(count = n());
ggplot(data=TotalAlugueisPorMes) + geom_bar(mapping = aes(x=month,y=count_all,group=1, class=season), stat="identity")


TotalAlugueisPorMes <- sqldf("select type, start_station, count(*) qtd from trip2017 group by type, start_station order by qtd desc limit 100")

count_total <- sqldf("select type, count(start_station) as total from trip2017 group by type limit 10")
ggplot(data=count_total) + geom_bar(mapping = aes(x=type,y=total,group=1, fill=type), stat="identity") + xlab("Tipo de aluguel") + ylab("Quantidade") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
#No total, o aluguel feito por membros corresponde a quase 3 vezes os aluguéis casuais: 2775979/981798 = 2.82
((981798*100)/2775979) - 100



# sqldf("select start_station_name from trip2017 where start_station='31247' limit 1") #pra saber o nome da estação 31247
rota_mais_usada <- sqldf("select start_station, end_station, count(*) qtd from trip2017 group by start_station, end_station order by qtd desc limit 10") # rota mais usada
ggplot(data=rota_mais_usada) + geom_point(mapping = aes(x=start_station,y=end_station, size = qtd))
ggplot(data=trip2017) + geom_point(mapping = aes(x=m_duration, y= m_distance, colour=type, fill=type))

# se soubessemos os historico das estacoes, poderiamos descobrir quais estacoes precisam de mais bikes baseado no numero de vezes que ela fica vazia
# com essa informacao é possível sugerir uma ciclovia caso nao haja ainda
# fazer isso automaticamente podemos saber a infuencia que uma ciclovia tem no uso do sistema


#relação entre distancia e duração
#identificamos dois perfis de corridas, voltados para o propósito da corrida.
#Uma corrida para o trabalho, por exemplo, não é desejável perder tempo.
#Então vamos pela menor rota, pecorrendo grande distância, no menor tempo possível.
#E existe ainda outro perfil oposto, aqueles que fazem um trajeto menor, mas que levam mais tempo para completá-lo. Podendo, inclusive, devolver a bike no mesmo ponto de onde alugaram.

#ggplot(trip2017, aes(x = m_duration, y = m_distance)) + geom_point() + facet_grid(workday~.)+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(trip2017, aes(x = m_duration, y = m_distance) + geom_point() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()))

#relação duration x m_duration
ggplot(trip2017, aes(x = duration, y = m_duration)) + geom_point() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


#AGRUPAMENTO POR HORA
#quantidade de alguel por hora em cada estação
ggplot(data = agt_hour) + geom_point(mapping = aes(x = qtd, y = hour, color=temperature)) + facet_wrap(~ season, nrow=1) + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
#melhor de ver assim
ggplot(data = agt_hour) + geom_point(mapping = aes(x = hour, y = qtd, color=temperature)) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ season, nrow=2) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())


#quantidade de alguel por hora em cada dia da semana
ggplot(data = agt_hour) + geom_point(mapping = aes(x = qtd, y = hour, color=as.character(workday))) + facet_wrap(~ weekday, nrow=1) + labs(color = "dia útil\n") + xlab("quantidade") + ylab("hora") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
#melhor de ver assim
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=4) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=3) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=2) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

#quantidade de alguel por hora de acordo com a temperature e r_temperature
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = qtd, y = temperature)) + xlab("quantity") + ylab("temperature") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = qtd, y = r_temperature)) + xlab("quantity") + ylab("r_temperature") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#quantidade de aluguel por dia de cada mes
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = day, y = qtd)) + facet_wrap(~ month, ncol=4) + xlab("day") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())


sqldf("select type, start_station, count(*) qtd from trip2017 group by type, start_station order by qtd desc limit 100") # estações mais alugam para cadastrados ou membros

bikes_pec <- sqldf("select bike, sum(m_distance) dist from trip2017 group by bike order by dist desc") # bikes que percorreram maior distancia
# baseado nessa informacao eh possivel criar um sistema de reparos - importante (pode ficar melhor se usar o tempo)

#select start_station, end_station, season, count(*) qtd  from trip2017 group by start_station, end_station, season order by qtd desc limit 10 # rota mais usada por estacao do ano 
# o rota com mais alugueis acontece na primavera e corresponde a um passseio, pois a estacao de saida e chegada é a mesma
# 31247 fica proximo ao monumento hisorico, onde ha um otimo espaco para passeio, assim como a maioria dessas estacoes (31258, 31248, 31249)

#select TERMINAL_NUMBER, NUMBER_OF_EMPTY_DOCKS from station order by NUMBER_OF_EMPTY_DOCKS desc
# por incrivel que pareca, as estacoes com mais docks nao sao as mais utilizadas


#TESTE ARVORE
amostra = sample(2,1000,replace=T, prob=c(0.7,0.3))
treino = trip2017[amostra==1,] # para treinamento
teste = trip2017[amostra==1,] # para teste
arvore = rpart(class ~ ., data=treino)

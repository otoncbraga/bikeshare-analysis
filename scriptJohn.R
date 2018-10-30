###############################
######## INFORMAÇÕES ##########
###############################

#Q1: 2017-01-01 a 2017-03-31
#Q2: 2017-04-01 a 2017-06-30
#Q3: 2017-07-01 a 2017-09-30
#Q4: 2017-10-01 a 2017-12-31

# Primavera: 21 mar - 20 jun
# Verão: 21 jun - 20 set
# Outono: 21 set - 20 dez
# Inverno: 21 dez - 20 mar

###############################
########## COMANDOS ###########
###############################

#para formar dataFrame apartir de outro
#(dados = data.frame(x,y,outraTabela))

#para renomear colunas
#names(dados) = c("Variável 1", "Variável 2")

#rbind() para juntar linhas / cbind() para juntar colunas / merge()

###############################
######### PENDÊNCIAS ##########
###############################
#[ ] Juntar arquivos em um só
#[ ] Exportar para csv
#[ ] Adicionar coluna com dia da semana
#[ ] Adicionar coluna com estação do ano
#[ ] Adicionar coluna com informação de dia útil

###############################
########### SCRIPT ############
###############################

#Instalar bibliotecas
install.packages("ggplot2") #gerar gráficos
install.packages("sqldf") #reconhecer comandos SQL
install.packages("dplyr") #Transformação/manipulação de dados

#Carregar bibliotecas
library("ggplot2")
library("sqldf")
library("dplyr")

Q1 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q1-capitalbikeshare-tripdata.csv")
Q2 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q2-capitalbikeshare-tripdata.csv")
Q3 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q3-capitalbikeshare-tripdata.csv")
Q4 <- read.csv("C:/Users/johnattan.douglas/Desktop/capitalbikeshare/2017Q4-capitalbikeshare-tripdata.csv")


#Juntar as 4 partes em um único lugar
QTOTAL <- rbind(rbind(Q1, Q2), rbind(Q3, Q4))

#exportarTabela  #!!!Ainda não está dando certo!!!!
tabela <- data.frame(QTOTAL)
write.csv(tabela, "teste.csv")

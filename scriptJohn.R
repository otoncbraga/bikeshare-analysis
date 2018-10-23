#Instalar bibliotecas
install.packages("ggplot2") #gerar gráficos
install.packages("sqldf") #reconhecer comandos SQL
install.packages("dplyr") #Transformação/manipulação de dados

#Carregar bibliotecas
library("ggplot2","sqldf", "dplyr")

#para formar dataFrame apartir de outro
#(dados = data.frame(x,y,outraTabela))

#para renomear colunas
#names(dados) = c("Variável 1", "Variável 2")
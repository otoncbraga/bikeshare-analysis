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

trip2017 <- read.csv(file.choose(), sep=',');
agt_hour <- read.csv(file.choose(), sep=',');

#=======================================#
count_total <- sqldf("select type, count(start_station) as total from trip2017 group by type limit 10")
ggplot(data=count_total) + geom_bar(mapping = aes(x=type,y=total,group=1, fill=type), stat="identity") + xlab("Tipo de aluguel") + ylab("Quantidade") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
#No total, o aluguel feito por membros corresponde a quase 3 vezes os aluguéis casuais: 2775979/981798 = 2.82

#relação entre distancia e duração
ggplot(trip2017, aes(x = m_duration, y = m_distance)) + geom_point() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#relação duration x m_duration
ggplot(trip2017, aes(x = duration, y = m_duration)) + geom_point() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#quantidade de alguel por hora em cada dia da semana
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=4) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=3) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ weekday, ncol=2) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

#quantidade de aluguel por dia de cada mes
ggplot(data = agt_hour) + geom_smooth(mapping = aes(x = day, y = qtd)) + facet_wrap(~ month, ncol=4) + xlab("day") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

#quantidade de alguel por hora em cada estação do ano
ggplot(data = agt_hour) + geom_point(mapping = aes(x = hour, y = qtd, color=temperature)) + geom_smooth(mapping = aes(x = hour, y = qtd)) + facet_wrap(~ season, nrow=2) + xlab("hour") + ylab("flow") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
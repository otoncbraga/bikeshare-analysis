library(ggplot2) # importa a lib
library(sqldf)

grafico1 <- sqldf("select weekday, sum(casual) alugueis from day group by weekday") 

# trabalhando com dataset day - importar antes
#ggplot(data = day) + geom_bar(mapping = aes(x = weekday, y = casual, group = weekday))

ggplot(data = day) + geom_bar(mapping = aes(x = workingday)) # ha mais alugeis em dias uteis

ggplot(data = day) + geom_point( mapping = aes(x = casual, y = weekday))



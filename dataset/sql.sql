select weekday, sum(casual) from day group by weekday # numero de alugueis agrupados por casuais

select * from day where casual > registered # casos onde os casuais superarm os registrados - nunca em dias de trabalho

select hr, cnt from hour order by cnt desc # quais os horarios de pico durante a semana e no final de semana?
# os horario de pico sao a tarde, pelas 18 e 17 
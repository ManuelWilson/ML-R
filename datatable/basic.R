#install.packages('data.table')
library(data.table)

DT = data.table(
  ID = c("b","b","b","a","a","c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)

#filtrado por indice 
DT[1:2, 2]


#filtrar por condicion
DT[ID == "a" & b >= 11]

#Obtener arreglo de las columnas 
DT[,c(a,b,c)]

#obtener columnas por nombre
DT[,list(a,b,c)]
DT[, .(a, b, c)]

#renombrar columnas
DT[, .(col1 = a, col2 = b)]


#sumar columnas
DT[, sum((a + b))]


#filtrar columna y relizar agregacion en columnas
DT[ID == "b",  .(restul1 = mean(a), restul2 = mean(b))]

#.N conteo de elementos en el grupo
DT[ID == "b", .N]


#agregacion
DT[, .(.N), by = .(ID)]

DT[c >= 15 , .N, by = ID]



#JOIN
dt1 <- data.table(ID = c("b","b","b","a","a","c"), X1 = 1:6, X2 = 7:12, X3 = 13:18, key = "ID")
dt2 <- data.table(ID = c("a", "b", "e"), Y = 1:3, key = "ID")

#INNER
merge(dt1, dt2, by = 'ID')

#FULL
merge(dt1, dt2, all = TRUE)

#LEFT
merge(dt1, dt2, all.x = TRUE)



#PIVOT 
dt1 <- data.table(
  ID = c("b","b","b","a","a","c"), 
  month = c('month1','month1', 'month2', 'month2', 'month2', 'month2'),  
  X1 = 1:6, 
  X2 = 7:12, 
  X3 = 13:18, 
  key = "ID")



dt1_pivot <- dcast( dt1[,list(ID,month,X1)] , ID ~ month, fun=sum)



#unpivot
melt(dt1_pivot, id.vars=c("ID"))




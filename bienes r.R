
#Install packages

install.packages ("readxl") 
library(readxl) #cargar el paquete 
bienes <- read_excel("Desktop/BIENES.xlsx")

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

#Let's check the dataset

View(bienes)
summary(bienes)

#Property Types: Let's count how many properties are listed for: HOUSE | APARTMENT | STORE |INDUSTRIAL | OFFICE


resultados_casa <- bienes %>% count(Tipo == "Casa")
print(resultados_casa)

resultados_apartamento <- bienes %>% count(Tipo == "Piso")
print(resultados_apartamento)

resultados_local <- bienes %>% count(Tipo == "Local")
print(resultados_local)

resultados_industrial <- bienes %>% count(Tipo == "Industrial")
print(resultados_industrial)

resultados_oficina  <- bienes %>% count(Tipo == "Oficina")
print(resultados_oficina)



#mas caro
most_expensive_property <- 
  max(bienes_ventas$`Precio Venta`)
print(most_expensive_property)

#mas barato 
cheapest_property <- 
  min(bienes_ventas$`Precio Venta`)
print(cheapest_property)

#promedio ventas general 
average_sale <- filter (bienes, Operación == "Venta")
print (average_sale)

average_general <- 
  mean(average_sale$`Precio Venta`)
print(average_general)

#ventascasa
filas_categoria_B <- filter(bienes, Tipo == "Casa", Operación == "Venta")
print(filas_categoria_B)
View(filas_categoria_B)
casa_tabla <- select(filas_categoria_B,3,4,7)
print(casa_tabla)

promedio_ventas_casa <- 
  mean(casa_tabla$`Precio Venta`)
print(promedio_ventas_casa)


filas_categoria_A <- filter(bienes,Tipo =="Piso", Operación == "Venta")
print(filas_categoria_A)+
View(filas_categoria_A)

apartamento_tabla <- select(filas_categoria_A,3,4,7)
print(apartamento_tabla)

promedio_ventas_apartamento <- 
  mean(filas_categoria_A$`Precio Venta`)
print(promedio_ventas_apartamento)

filas_categoria_C <- filter(bienes, Tipo == "Local", Operación == "Venta")
print(filas_categoria_C)

local_tabla <- select(filas_categoria_C,3,4,7)
print(local_tabla)

promedio_ventas_local <- 
  mean(filas_categoria_C$`Precio Venta`)
print(promedio_ventas_local)

filas_categoria_D <- filter(bienes, Tipo == "Oficina", Operación == "Venta")
print(filas_categoria_D)

Oficina_tabla <- select(filas_categoria_D,3,4,7)
print(Oficina_tabla)

promedio_ventas_oficina <- 
  mean(filas_categoria_D$`Precio Venta`)
print(promedio_ventas_oficina)


filas_categoria_E <- filter(bienes, Tipo == "Industrial", Operación == "Venta")
print(filas_categoria_E)

industrial_tabla <- select(filas_categoria_E,3,4,7)
print(industrial_tabla)

promedio_ventas_industrial <- 
  mean(filas_categoria_E$`Precio Venta`)
print(promedio_ventas_industrial)



bienes_ventas<- filter(bienes, Operación == "Venta")
View(bienes_ventas)

#_____________
 
 #Promedio Alquiler 
 
 promedio_alquiler_bienes <-  filter (bienes, Operación == "Alquiler")
 print (promedio_alquiler_bienes)
 
 average_general_alquiler <- 
   mean(promedio_alquiler_bienes$`Precio Venta`)
 print( average_general_alquiler)
   
 #alquiler casa
 categoria_alquiler_A <- filter(bienes, Tipo == "Casa", Operación == "Alquiler")
 print(categoria_alquiler_A)
 View( categoria_alquiler_A)
 
 casa_tabla_alquiler <- select( categoria_alquiler_A,3,4,7)
 print( casa_tabla_alquiler)
 
 promedio_alquiler_casa <- 
   mean(casa_tabla_alquiler$`Precio Venta`)
 print(promedio_alquiler_casa)
 
 #alquiler apartamento
 apartamento_categoria_B <- filter(bienes,Tipo =="Piso", Operación == "Alquiler")
 print(apartamento_categoria_B)

 apartamento_tabla_alquiler <- select(apartamento_categoria_B,3,4,7)
 print(apartamento_tabla_alquiler)
 
 promedio_alquiler_apartamento <- 
   mean(apartamento_tabla_alquiler$`Precio Venta`)
 print(promedio_alquiler_apartamento)
 
 #alquiler local 
 alquiler_categoria_C <- filter(bienes, Tipo == "Local", Operación == "Alquiler")
 print(alquiler_categoria_C)
 
 local_alquiler <- select(alquiler_categoria_C,3,4,7)
 print(local_alquiler)
 
 promedio_alquiler_local <- 
   mean(local_alquiler$`Precio Venta`)
 print(promedio_alquiler_local)
 
 #alquiler oficina
 alquiler_categoria_D <- filter(bienes, Tipo == "Oficina", Operación == "Alquiler")
 print(alquiler_categoria_D)
 
 Oficina_alquiler <- select(alquiler_categoria_D,3,4,7)
 print(Oficina_alquiler)
 
 promedio_alquiler_oficina <- 
   mean( alquiler_categoria_D$`Precio Venta`)
 print( promedio_alquiler_oficina)
 
 
 alquiler_categoria_E <- filter(bienes, Tipo == "Industrial", Operación == "Alquiler")
 print(alquiler_categoria_E)
 
 industrial_alquiler <- select(alquiler_categoria_E,3,4,7)
 print(industrial_alquiler)
 
 promedio_alquiler_industrial <- 
   mean(alquiler_categoria_E$`Precio Venta`)
 print( promedio_alquiler_industrial)
 
 
 
 #promedio precio by region
promedio_precio_A <- filter(bienes, Provincia == "Barcelona", Operación == "Venta")
 print(promedio_precio_A)
 View(promedio_precio_A)
 
 promedio_venta_BCN <- 
   mean(promedio_precio_A$`Precio Venta`)
 print(promedio_venta_BCN)
 
 promedio_precio_B <- filter(bienes, Provincia == "Girona", Operación == "Venta")
 print(promedio_precio_B)
 View(promedio_precio_B)
 
 promedio_venta_GRN <- 
   mean(promedio_precio_B$`Precio Venta`)
 print(promedio_venta_GRN)
 
 promedio_precio_C <- filter(bienes, Provincia == "Tarragona", Operación == "Venta")
 print(promedio_precio_C)
 View(promedio_precio_C)
 
 promedio_venta_TRG <- 
   mean(promedio_precio_C$`Precio Venta`)
 print(promedio_venta_TRG)
 
 promedio_precio_D <- filter(bienes, Provincia == "Lleida", Operación == "Venta")
 print(promedio_precio_D)
 View(promedio_precio_D)
 
 promedio_venta_LLD <- 
   mean(promedio_precio_D$`Precio Venta`)
 print(promedio_venta_LLD)

 
properties_ventas <- filter(bienes, Operación == "Venta")
 print(properties_ventas)
 
 cantidad_propiedades_venta <- 
   count(properties_ventas)
 
 print( cantidad_propiedades_venta)
 
 
 properties_alquiler <- filter(bienes, Operación == "Alquiler")
 print(properties_alquiler)
 
 cantidad_propiedades_alquiler <- 
   count(properties_alquiler)
 
 print(cantidad_propiedades_alquiler)
 
 properties_ejer <- filter(bienes, Tipo == "Apartamento", Tipo == " Oficina ", Tipo == "Piso", Tipo ==  "Casa", Tipo == "Industrial", Operación == "Alquiler")
 print(properties_ejer)
 
ejemplo <- filter(bienes, Tipo == "Apartamento", Operación == "Alquiler")
 print(ejemplo)
 view(subset_datos)
 tabla_unida <- bind_rows(casa_tabla,
                          industrial_tabla,
                          apartamento_tabla,
                          Oficina_tabla,
                          local_tabla)
 


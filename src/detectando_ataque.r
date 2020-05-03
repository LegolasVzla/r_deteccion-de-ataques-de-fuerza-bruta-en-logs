# Jose Prato 23185710 - Manuel Carrero 19821361

# Primeramente se cargan las librerias necesarias
library(ProjectTemplate)
library(tm)  # Para el tratamiento de limpieza la data
library(arules)  # Para el uso de reglas de asociación
library(arulesViz)  # Para graficar las reglas

datos <- read.csv("~/tarea2/data/data.csv")  # Se procede a leer la data suministrada
probabilidad_ataque <- 0  # Se declara la variable de la probabilidad

tiempo <- c(as.POSIXct(datos$start_time, origin ="1970-01-01"))  # Se cambia el formato del tiempo a uno mas entendible
datos[, "start_time"] <- tiempo  # Se actualiza la columna del tiempo con el nuevo formato
View(datos)  # Se muestra el conjunto de datos con el nuevo formato de la la columna start_time

# c_source_ip <- length(unique(datos$source_ip))  # Se obtiene la cantidad de ips de origen distintas
# c_port_ip <- length(unique(datos$source_port))  # Se obtiene la cantidad de puertos de origen distintos

aux_datos <- subset(datos, select= c(source_ip,destination_ip,start_time,source_port,destination_port,asn,num_packets,num_bytes))  # Se toman las columnas que se consideran necesarias para el análisis del ataque
# subconjunto <- head(aux_datos)  # Para pruebas

# Para poder aplicar las reglas de asociacion a partir de nuestro subconjunto de datos y que el algoritmo apriori pueda funcionar correctamente, algunas columnas deben ser discretizadas primero, por lo que sera necesario pasar dichas columnas de sus valores numericos (o en el caso de start_time de tipo desconocido), a tipos de datos factor

aux_datos$start_time <- factor(aux_datos$start_time)
aux_datos$source_port<- factor(aux_datos$source_port)
aux_datos$destination_port <- factor(aux_datos$destination_port)
aux_datos$num_packets <- factor(aux_datos$num_packets)
aux_datos$asn <- factor(aux_datos$asn)
aux_datos$num_bytes <- factor(aux_datos$num_bytes)

# Se discretiza mediante el campo de num_packets

min(datos$num_packets)  # Mínimo valor: 1
mean(datos$num_packets)  # Promedio: 1016.098~1016
max(datos$num_packets)  # Máximo valor: 2592165

aux_datos$num_packets[aux_datos$num_packets >= 1 & aux_datos$num_packets <= 1016] <- 'Bajo'
aux_datos$num_packets[aux_datos$num_packets == 1016] <- 'Promedio'
aux_datos$num_packets[aux_datos$num_packets > 1016 & aux_datos$num_packets <= 2592165] <- 'Alto'

rules <- apriori(aux_datos, parameter= list(supp=0.1, conf=0.6))  # Una vez discretizada nuestra matriz, se aplica el algoritmo apriori pasandole un Soporte (es decir, la proporción de transacciones en aux_datos que contiene el conjunto de columnas) de 0,1 y una Confianza de 0,6
rules  # Se muestra la cantidad de reglas de asociación obtenidas
summary(rules)  # Se muestran los valores obtenidos para el soporte, confianza y lift a partir de las reglas de asociación

plot(rules)  # Es posible observar las proporciones entre los diferentes registros
plot(rules, measure=c("support","lift"), shading="confidence")  # Un lift > 1 indica que ese conjunto aparece una cantidad de veces superior a lo esperado bajo condiciones de independencia

subrules = rules[quality(rules)$conf > 0.8];  # Se obtiene un subconjunto de las reglas más importantes
subrules  # Dependiendo de la regla aplicada, la confianza máxima obtenida será tomada como la probabilidad de ataque
plot(subrules)  # Se muestra el grafico notando los cambios con respecto al de rules

plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=50))  # Se observan grupos de acuerdo a las reglas que cumplen
# sel = plot(rules, method="grouped", interactive=TRUE)

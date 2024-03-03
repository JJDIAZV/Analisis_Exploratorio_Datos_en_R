
#######################################################################################
#                        Análisis exploratorio de datos con R

#######################################################################################




install.packages("dplyr")# si no ha instalado estos paquetes debe correr primero el comando:
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("gmodels")
library(gmodels)
install.packages("Hmisc")
library(Hmisc)
install.packages("ggthemes")
library(ggthemes)





# si no ha instalado estos paquetes debe correr primero el comando:
library(dplyr)
library(ggplot2)
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)


#Importar archivos

library(readxl)
encuesta <- read_excel("C:/Users/Andres/Desktop/JHON_J_DIAZ_DRIVE/CODE/R/3.ANALISIS_EXPLORATORIO/BD_AEXPLORATORIO/encuesta.xlsx")
View(encuesta)


library(readxl)
cumbres <- read_excel("C:/Users/Andres/Desktop/JHON_J_DIAZ_DRIVE/CODE/R/3.ANALISIS_EXPLORATORIO/BD_AEXPLORATORIO/cumbres.xls")
View(cumbres)

attach(encuesta) #Esto significa que R busca en la base de datos al evaluar una variable,
#por lo que se puede acceder a los objetos de la base de datos simplemente dando sus nombres.


glimpse(encuesta)



table(encuesta$REGION)
#Variables categóricas

#Frecuencias simples

table(encuesta$ZONA)


#Y podemos ver que 321 personas viven en Zona Rural y 314 en Urbana.

#Tablas de contingencia
table(encuesta$Tipo_vivienda, encuesta$ZONA)

addmargins(table(encuesta$Tipo_vivienda, encuesta$ZONA))


# Proporciones

prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA))


#En este caso nos muestra los datos como proporciones totales,
#pero ¿cómo hacemos si queremos ver porcentajes por fila o columna?.

#Esto lo hacemos poniendo una coma y luego 1 (filas) o 2 (columnas).

#Filas
prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA),1)

#Columnas
prop.table(table(encuesta$Tipo_vivienda, encuesta$ZONA),2)

#CrossTable()

#Un comando muy útil para simplificar los pasos es el comando CrossTable()
#del paquete gmodels(). El comando nos permite presentar en una misma tabla
#los porcentajes por fila o columna y el total de la tabla.

CrossTable(encuesta$Tipo_vivienda, encuesta$ZONA)

CrossTable(encuesta$Tipo_vivienda, encuesta$ZONA, prop.t=F, prop.chisq = F)

CrossTable(encuesta$ZONA)


# Datos numéricos
summary(encuesta)

describe(encuesta)

hist(encuesta$ingreso)

hist(encuesta$ingreso)
mean(encuesta$ingreso)

library("ggplot2")
histograma<-ggplot(encuesta,aes(x=Mensualidad)) +
  ggtitle("Mensualidad neto de hogares unipersonales")+
  theme_fivethirtyeight()+ geom_histogram(color="#28324a", fill="#3c78d8")
histograma

histograma <- ggplot(encuesta, aes(x=ingreso)) +
  ggtitle("Ingreso neto de hogares unipersonales") +
  theme_fivethirtyeight() +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma

# Group_by
encuesta <- encuesta %>%
  group_by(REGION) %>%
  mutate(ingresoprom=mean(ingreso))
encuesta %>%
  group_by(REGION) %>%
  summarise(ingresoprom=mean(ingreso),
            edadprom=mean(Edad))


ggplot(data = encuesta, aes(x = Edad, y = Escolari)) +
  geom_point() +
  xlab('Puntuación Audiencia') +
  ylab('Puntuación de la Crítica') +
  ggtitle('Relación entre Edad e Ingreso') +
  theme_minimal()


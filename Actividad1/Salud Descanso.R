# Primero: Importar los datos desde el archivo "Sleep_health.csv" ubicado en "C:\R"

# Segundo: Establecer el directorio de trabajo (donde está almacenado el archivo descargado)
# Para facilitar el proceso, en RStudio, se puede hacer lo siguiente:
# Ir al menú: Session > Set Working Directory > Choose Directory

# Tercero: Leer el archivo de datos "Sleep_health.csv" usando read.table().
# Especificamos que las columnas están separadas por tabulaciones (sep=",") y que la primera fila contiene los nombres de las columnas (header=TRUE).
library(ggplot2)
library(mxmaps)
library(scales)
library(data.table)
library(dplyr)

Data <- read.table("Data/Sleep_health.csv",sep=",",header=TRUE)

#Lectura de los primeros 10 registros
head(Data,10)

#Tamaño de Dataset
dim(Data)

#Columnas de Ds
colnames(Data)

#Estructura DS
str(Data) 

N <- nrow(Data)
N

#Porcentaje de genero dentro del ds
T_frec.gender <- data.frame(transform(table(Data$Gender)))
names(T_frec.gender) <- c("Genero", "Frecuencia") 
T_frec.gender

F.relativa <- (T_frec.gender$Frecuencia / N)*100
F.relativa

T_frec.gender <- cbind(T_frec.gender, F.relativa)

T_frec.gender

#G1:Porcentaje de genero
ggplot(T_frec.gender,aes(x=2,y=F.relativa, fill=Genero))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(F.relativa/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue"))+
  theme_void()+
  labs(title="Porcentaje por Género")+
  xlim(0.5,2.5)

# Edad menor
min(Data$Age)

#  Edad mayor
max(Data$Age)

#Duración Promedio de Sueño por Género
SPG = data.table(Data$Gender,Data$Sleep.Duration)
names(SPG) <- c("Genero", "Sleep") 
SPG

SProm <- SPG[ , mean(Sleep), by = Genero ]
names(SProm) <- c("Genero", "SProm") 
SProm

#G2:Duración Promedio de Sueño por Género
ggplot(SProm, aes(x = Genero, y = SProm)) +
  geom_bar(stat = "identity", width=0.4, fill = "steelblue")


#Promedio de calidad de sueño por genero
QSG = data.table(Data$Gender,Data$Quality.of.Sleep)
names(QSG) <- c("Genero", "Quality") 
QSG

SPromQ <- QSG[ , mean(Quality), by = Genero ]
names(SPromQ) <- c("Genero", "SQuality") 
SPromQ

#G3:Calidad Promedio de Sueño por Género
ggplot(SPromQ, aes(x = "", y = SQuality, fill = Genero)) +
  geom_col() +
  geom_text(aes(label = SQuality),
            position = position_stack(vjust = 0.5)) +
  labs(y="Calidad Promedio de Sueño por Género")+
  coord_polar(theta = "y")


#Cantidad de desordenes de sueño
T_Des <- data.frame(transform(table(Data$Sleep.Disorder)))
names(T_Des) <- c("Desorden", "Frecuencia") 
T_Des


#Cantidad de desordenes de sueño por genero
T_Des_Gen <- data.frame(transform(table(Data$Gender,Data$Sleep.Disorder)))
names(T_Des_Gen) <- c("Genero", "Desorden", "Frecuencia") 
T_Des_Gen


#Distribución por Ocupación
T_Ocupa <- data.frame(transform(table(Data$Occupation)))
names(T_Ocupa) <- c("Ocupacion", "Frecuencia") 
T_Ocupa

#G4:Distribución por Ocupación
ggplot(T_Ocupa, aes(x = Ocupacion, y = Frecuencia, fill= Ocupacion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frecuencia), vjust = 2, colour = "white")


#G5:Grafico para verificar el nivel de estrés por las horas de sueño
plot(x = Data$Sleep.Duration, y = Data$Stress.Level,
     pch = 16, frame = TRUE,
     xlab = "Horas de Sueño", ylab = "Nivel de Estrés", col = "royalblue")


#G6:BoxPlot para verificación del nivel de estrés por la ocupación
ggplot(Data, aes(x = Data$Occupation, y = Data$Stress.Level, fill = Data$Occupation)) + 
  geom_boxplot()+
  labs(x='Ocupaciones', y='Nivel de Estres')


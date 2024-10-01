#Instalación de paquetes
#install.packages("reticulate")
library(reticulate)

#reticulate::py_install("pandas")
# reticulate::py_install("numpy")
# reticulate::py_install("plotly.express")

#Ingreso a modo Python
library(reticulate)
library(ggplot2)

pd <- import("pandas") 
np <- import("numpy") 
px <- import("plotly.express") 

#Lectura de dataset
Datos <- pd$read_csv("Data/Sleep_health.csv", sep = ',')

#Información de columnas
str(Datos)

#G1 Grafico ocupaciones vrs nivel estrés
Df_R <- py_to_r(Datos)
Df_R

# Edad menor
Age_Min <- np$min(Df_R$Age)
Age_Min

#  Edad mayor
Age_Max <- np$max(Df_R$Age)
Age_Max

#Promedio de horas de sueño
PromSle <- np$mean(Df_R$`Sleep Duration`)
PromSle

Ocupa = data.frame(transform(table(Df_R$Occupation)))
names(Ocupa) <- c("Ocupacion","Cant") 
Ocupa

#G1:Ocupaciones y nivel de estres
ggplot(Ocupa, aes(x = Ocupacion, y = Cant)) +
  geom_segment(aes(x = Ocupacion, xend = Ocupacion, y = 0, yend = Cant)) +
  geom_point(size = 6, pch = 21, bg = "4", col = 1) +
  geom_text(aes(label = Cant), color = "white", size = 3) +
  coord_flip()


#Duración Promedio de Sueño por Género
SPG = data.table(Df_R$Gender,Df_R$`Sleep Duration`)
names(SPG) <- c("Genero", "Sleep") 
SPG

SProm <- SPG[ , mean(Sleep), by = Genero ]
names(SProm) <- c("Genero", "SProm") 
SProm

#G2:Duración Promedio de Sueño por Género
it <- px$bar(SProm, x='Genero', y='SProm', title= "Duración Promedio de Sueño por Género")
it

#Porcentaje de genero dentro del ds
T_frec.gender <- data.frame(transform(table(Df_R$Gender)))
names(T_frec.gender) <- c("Genero", "Frecuencia") 
T_frec.gender

F.relativa <- (T_frec.gender$Frecuencia / N)*100
F.relativa

T_frec.gender <- cbind(T_frec.gender, F.relativa)

T_frec.gender

#G2:Porcentaje de genero
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


#Promedio de calidad de sueño por genero
QSG = data.table(Df_R$Gender,Df_R$`Quality of Sleep`)
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
Dsueño <- pd$value_counts(Df_R$`Sleep Disorder`)
Dsueño

#G4:Grafico para verificar el nivel de estrés por las horas de sueño
plot(x = Df_R$`Sleep Duration`, y = Df_R$`Stress Level`,
     pch = 16, frame = TRUE,
     xlab = "Horas de Sueño", ylab = "Nivel de Estrés", col = "royalblue")


#G5:BoxPlot para verificación del nivel de estrés por la ocupación
ggplot(Data, aes(x = Df_R$Occupation, y = Df_R$`Stress Level`, fill = Df_R$Occupation)) + 
  geom_boxplot()+
  labs(x='Ocupaciones', y='Nivel de Estres')




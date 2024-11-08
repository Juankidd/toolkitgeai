################################################################################
#Script de análisis de validación de expertos
#Cocoshapes
#16 diciembre de 2023
################################################################################

#Library
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Work's directory
setwd("~/Library/CloudStorage/Dropbox/ACTIVIDADES/ACTIVIDADES_2023-01/Proyecto_CocoShapes/Analisis_Validación_Expertos")#Mac 

setwd("D:/Users/jaospina.UAO/Dropbox/ACTIVIDADES/ACTIVIDADES_2023-01/Proyecto_CocoShapes/Analisis_Validación_Expertos")

#Datos
df = read.xlsx("Datos_Validacion_Expertos.xlsx", sheet = 1)

#Function to build the Indicator
Indicador = function(variable, n){
  E = n
  C = E * 6
  
  Di = round((sum(variable) - E)/(C - E)*100,2)
  return(Di)
}

n = dim(df)[1]
Indicador(df$LAD1_Utilidad, n)

#Data.frame of analysis
df_analisis = df[,-c(1,2,3,4,19,34,41,72,81)]
names(df_analisis)

#Apply the function over the data.frame
Di = apply(df_analisis, 2, Indicador, n = dim(df)[1])

Di = as.data.frame(Di)
Di$Item =  rownames(Di)  
rownames(Di) <- seq(1:dim(Di)[1])  

Lista = strsplit(Di$Item, split = "_")
Lista = do.call(rbind.data.frame, Lista )
colnames(Lista) = c("Item", "Concepto")

Indicador = cbind.data.frame(Lista$Item, Lista$Concepto, Di$Di)
colnames(Indicador) = c("Item", "Concepto", "Percepción")

Etiquetas = Indicador$Item

#Plot in spanish
Indicador_re =  reshape(Indicador, idvar = "Item", timevar = "Concepto", direction = "wide")
colnames(Indicador_re) = c("Item", "Utilidad", "Claridad")

Etiquetas0 = unique(Etiquetas)

pdf("Figures/Fig1_Spanish.pdf")
ggplot(Indicador_re) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Utilidad, color = "Utilidad")) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Claridad, color = "Claridad")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Utilidad, color = "Utilidad")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Claridad, color = "Claridad")) +
  scale_color_manual(values = c("Utilidad" = "red", "Claridad" = "blue")) +
  scale_x_continuous(breaks = 1:dim(Indicador_re)[1], labels = Etiquetas0) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "top") +  # Puedes cambiar la posición si es necesario
  labs(x = "Lineamientos para el análisis del aprendizaje",
       y = "Indicador de percepción (%)",
       color = "") +  # Título de la leyenda
  coord_flip() +
  geom_hline(yintercept = 70, linetype = "dotted", 
             color = "black", size = 1.5) +
  ylim(64, 78)

dev.off()  





pdf("Figures/Fig1_English.pdf")
ggplot(Indicador_re) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Utilidad, color = "Usefulness")) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Claridad, color = "Clarity")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Utilidad, color = "Usefulness")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Claridad, color = "Clarity")) +
  scale_color_manual(values = c("Usefulness" = "red", "Clarity" = "blue")) +
  scale_x_continuous(breaks = 1:dim(Indicador_re)[1], labels = Etiquetas0) +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "top") +  # Puedes cambiar la posición si es necesario
  labs(x = "Guidelines for Learning Analytics",
       y = "Perception indicator (%)",
       color = "") +  # Título de la leyenda
  coord_flip() +
  geom_hline(yintercept = 70, linetype = "dotted", 
             color = "black", size = 1.5) +
  ylim(64, 78)

dev.off()  


#Comparison
 
df_LAD = Indicador_re[1:7,] 
df_LAS = Indicador_re[8:14,] 
df_LAT = Indicador_re[15:17,] 
df_LAA = Indicador_re[18:32,] 
df_LAP = Indicador_re[33:36,] 

#Vander warden
VanWaerdenTest(df_LAD$Utilidad, df_LAD$Claridad)
VanWaerdenTest(df_LAS$Utilidad, df_LAS$Claridad)
VanWaerdenTest(df_LAT$Utilidad, df_LAT$Claridad)
VanWaerdenTest(df_LAA$Utilidad, df_LAA$Claridad)
VanWaerdenTest(df_LAP$Utilidad, df_LAP$Claridad)

#U-Mann-Whitney
wilcox.test(df_LAD$Utilidad, df_LAD$Claridad)
wilcox.test(df_LAS$Utilidad, df_LAS$Claridad)
wilcox.test(df_LAT$Utilidad, df_LAT$Claridad)
wilcox.test(df_LAA$Utilidad, df_LAA$Claridad)
wilcox.test(df_LAP$Utilidad, df_LAP$Claridad)

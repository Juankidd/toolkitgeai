################################################################################
#Script de análisis de lineamientos
#Cocoshapes
#14 octubre de 2023
################################################################################

#Library
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Work's directory
setwd("D:/Users/jaospina.UAO/Dropbox/ACTIVIDADES/ACTIVIDADES_2024-03/Artículo_Experiencia_Usuario")


df_lineamientos = read.xlsx("df_lineamientos.xlsx", sheet = 1) 
etiquetas_ingles = read.xlsx("df_lineamientos.xlsx", sheet = 2) 

colnames(df_lineamientos) <- etiquetas_ingles[,4]


#Function to build the Indicator
Indicador = function(variable, n){
  E = n
  C = E * 6
  
  Di = round((sum(variable) - E)/(C - E)*100,2)
  return(Di)
}

n = dim(df_lineamientos)[1]


#Data.frame of analysis
df_analisis = df_lineamientos[,-c(1, 2, 3, 4, 23, 38, 49, 60, 77, 84)]


#Apply the function over the data.frame
Di = apply(df_analisis, 2, Indicador, n = dim(df_analisis)[1])

Di = as.data.frame(Di)
Di$Item =  rownames(Di)  
rownames(Di) <- seq(1:dim(Di)[1])  

Lista = strsplit(Di$Item, split = "_")
Lista = do.call(rbind.data.frame, Lista )
colnames(Lista) = c("Item", "Concepto", "Grupo")

Indicador_analisis = cbind.data.frame(Lista$Item, Lista$Concepto, Lista$Grupo , Di$Di)
colnames(Indicador_analisis) = c("Item", "Concept", "Group",  "Perception")


Indicador = cbind.data.frame(Lista$Item, Lista$Concepto, Di$Di)
colnames(Indicador) = c("Item", "Concept", "Perception")

Etiquetas = Indicador$Item

Indicador_re =  reshape(Indicador, idvar = "Item", timevar = "Concept", direction = "wide")
colnames(Indicador_re) = c("Item", "Utility", "Clarity")

Etiquetas0 = unique(Etiquetas)

pdf("Figures_guidelines/Fig_1.pdf")
ggplot(Indicador_re) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Utility, color = "Utility")) +
  geom_line(aes(x = 1:dim(Indicador_re)[1], y = Clarity, color = "Clarity")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Utility, color = "Utility")) +
  geom_point(aes(x = 1:dim(Indicador_re)[1], y = Clarity, color = "Clarity")) +
  scale_color_manual(values = c("Utility" = "red", "Clarity" = "blue")) +
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
             color = "black", size = 1.5)+
  ylim(50, 80)
dev.off()

#Statistcs
write.xlsx(Indicador_re, "Indicador_re.xlsx")


Indicador_analisis %>%
  group_by(Group, Concept) %>%
  summarise(Prom = mean(Perception,na.rm = T))


df_L1R = Indicador_re[1:9,]
df_L2R = Indicador_re[10:16,]
df_L3R = Indicador_re[17:21,]
df_L4R = Indicador_re[22:26,]
df_L5R = Indicador_re[27:34,]
df_L6R = Indicador_re[35:37,]


install.packages("DescTools")
library(DescTools)


wilcox.test(df_L1R$Utility , df_L1R$Clarity)
wilcox.test(df_L2R$Utility , df_L2R$Clarity)
wilcox.test(df_L3R$Utility , df_L3R$Clarity)
wilcox.test(df_L4R$Utility , df_L4R$Clarity)
wilcox.test(df_L5R$Utility , df_L5R$Clarity)
wilcox.test(df_L6R$Utility , df_L6R$Clarity)


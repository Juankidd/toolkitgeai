################################################################################
#Análisis de datos validación Toolkit
################################################################################

#Referencias
#http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization

install.packages("psych")
install.packages("corrplot")
install.packages("FactoMineR")
install.packages("GGally")

# Cargar librerías necesarias
library(psych)        # Para alfa de Cronbach y análisis factorial
library(corrplot)     # Para visualización de correlaciones
library(ggplot2)      # Para visualización
library(FactoMineR)   # Para Análisis de Componentes Principales
library(GGally)       # Para matriz de correlación visual
library(openxlsx)
library(factoextra)

#Directorio de trabajo
setwd("D:/Users/jaospina.UAO/Dropbox/ACTIVIDADES/ACTIVIDADES_2024-03/Artículo_Experiencia_Usuario")

#Cargar datos
df = read.xlsx("dataset_v10SEP2024.xlsx", sheet = "datos_2")

df$Rol <- as.factor(df$Rol)
df$Experiencia <- as.factor(df$Experiencia)
df$Frecuencia_uso <- as.factor(df$Frecuencia_uso)


# Seleccionar columnas de Usabilidad y Experiencia del Jugador
usabilidad  <- df[, c(1:7)]  
experiencia <- df[, c(8:17)]
utilidad <- df[, c(18:23)]

# Alfa de Cronbach para la sección de Usabilidad
psych::alpha(usabilidad)

# Alfa de Cronbach para la sección de Experiencia del Jugador
psych::alpha(experiencia)


# Alfa de Cronbach para la sección de Utilidad del Jugador
psych::alpha(utilidad)


###############################################################################
# Análisis de Componentes Principales
###############################################################################
usabilidad <- cbind.data.frame(usabilidad, df[,24:26])
experiencia <- cbind.data.frame(experiencia, df[,24:26])
utilidad <- cbind.data.frame(utilidad, df[,24:26])

usabilidad$Rol <- as.factor(usabilidad$Rol)
usabilidad$Experiencia <- as.factor(usabilidad$Experiencia)
usabilidad$Frecuencia_uso <- as.factor(usabilidad$Frecuencia_uso)

utilidad$Rol <- as.factor(utilidad$Rol)
utilidad$Experiencia <- as.factor(utilidad$Experiencia)
utilidad$Frecuencia_uso <- as.factor(utilidad$Frecuencia_uso)


acp_usabilidad  <- PCA(usabilidad, quali.sup = c(8, 9, 10))
acp_experiencia <- PCA(experiencia, quali.sup = c(11, 12, 13))
acp_utilidad    <- PCA(utilidad, quali.sup = c(7, 8, 9))

acp_usabilidad$eig
acp_experiencia$eig
acp_utilidad$eig

#Variabilidad explicada por cada componente
fviz_eig(acp_usabilidad,addlabels=TRUE)
fviz_eig(acp_experiencia, addlabels=TRUE)
fviz_eig(acp_utilidad, addlabels=TRUE)


# Visualización de los resultados del ACP

png("Figures/Fig_1a.png")
plot(acp_usabilidad, choix = "var")   # Gráfico de variables
dev.off()

png("Figures/Fig_1b.png")
plot(acp_experiencia, choix = "var")   # Gráfico de variables
dev.off()

png("Figures/Fig_1c.png")
plot(acp_utilidad, choix = "var")   # Gráfico de variables
dev.off()

# Contribuciones de las características

png("Figures/Fig_2a.png")
fviz_contrib(acp_usabilidad, choice = "var", axes = 1:2, top = 10)
dev.off()

png("Figures/Fig_2b.png")
fviz_contrib(acp_experiencia, choice = "var", axes = 1:2, top = 10)
dev.off()

png("Figures/Fig_2c.png")
fviz_contrib(acp_utilidad, choice = "var", axes = 1:2, top = 10)
dev.off()


fviz_pca_var(acp_usabilidad, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)




fviz_pca_var(acp_experiencia, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


fviz_pca_var(acp_utilidad, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


#Variables suplementarias
fviz_pca_var(acp_usabilidad , 
              col.var =  "black", # Variables activas 
              col.qua ="red"  # Supl. Variables cuantitativas
)



fviz_pca_var(acp_usabilidad,
             col.var = "black",     # Active variables
             col.quali.sup = "red" # Sup cualitativas variables
)


#Variables suplementarias
fviz_pca_var(acp_experiencia,
             col.var = "black",     # Active variables
             col.quali.sup = "red" # Sup cualitativas variables
)

#Representación simultanea
fviz_pca_biplot(acp_usabilidad, repel = TRUE)
fviz_pca_biplot(acp_experiencia, repel = TRUE)


png("Figures/Fig_3a.png")
fviz_pca_biplot(acp_usabilidad,
                col.ind = usabilidad$Rol, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_3b.png")
fviz_pca_biplot(acp_usabilidad,
                col.ind = usabilidad$Experiencia, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_3c.png")
fviz_pca_biplot(acp_usabilidad,
                col.ind = usabilidad$Frecuencia_uso, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()




png("Figures/Fig_4a.png")
fviz_pca_biplot(acp_experiencia,
                col.ind = experiencia$Rol, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_4b.png")
fviz_pca_biplot(acp_experiencia,
                col.ind = experiencia$Experiencia, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_4c.png")
fviz_pca_biplot(acp_experiencia,
                col.ind = experiencia$Frecuencia_uso, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()




png("Figures/Fig_5a.png")
fviz_pca_biplot(acp_utilidad,
                col.ind = utilidad$Rol, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_5b.png")
fviz_pca_biplot(acp_utilidad,
                col.ind = utilidad$Experiencia, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()

png("Figures/Fig_5c.png")
fviz_pca_biplot(acp_utilidad,
                col.ind = utilidad$Frecuencia_uso, palette = "jco",
                addEllipses = FALSE, label = "var",
                col.var = "black", repel = TRUE)
dev.off()







###############################################################################
#Análisis de sentimientos
###############################################################################
install.packages("syuzhet")

df_sentimiento = read.xlsx("df_analisis_sentimientos.xlsx")
df_sentimiento = as.data.frame(df_sentimiento$observación)
df_sentimiento = na.omit(df_sentimiento)
colnames(df_sentimiento) <- "observación"
# Cargar librerías necesarias
library(syuzhet)
library(tidyverse)

# Extraer las observaciones
observaciones <- df_sentimiento$observación

# Obtener las emociones utilizando el análisis de sentimientos NRC
sentimientos <- get_nrc_sentiment(observaciones, language = "spanish")

# Ver los resultados del análisis de sentimientos
head(sentimientos)

# Sumar los sentimientos por cada categoría emocional
sentimiento_total <- colSums(sentimientos)

# Convertir a data frame para visualizar
sentimiento_df <- data.frame(emocion = names(sentimiento_total), valor = sentimiento_total)

# Graficar los resultados de los sentimientos
png("Figures/Fig_6.png")
ggplot(sentimiento_df, aes(x = emocion, y = valor)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "", x = "Emotion", y = "Value")
dev.off()

#Sentiment Analysis of Observations


install.packages("wordcloud")
install.packages("tm")

library(wordcloud)
library(tm)


# Crear un corpus de texto
corpus <- Corpus(VectorSource(observaciones))

# Preprocesamiento del texto
corpus <- tm_map(corpus, content_transformer(tolower)) # Pasar a minúsculas
corpus <- tm_map(corpus, removePunctuation)            # Eliminar puntuaciones
corpus <- tm_map(corpus, removeNumbers)                # Eliminar números
corpus <- tm_map(corpus, removeWords, stopwords("spanish")) # Eliminar stopwords en español
corpus <- tm_map(corpus, stripWhitespace)              # Eliminar espacios en blanco adicionales


# Crear la matriz de términos
dtm <- TermDocumentMatrix(corpus)
matriz_palabras <- as.matrix(dtm)
frecuencia_palabras <- sort(rowSums(matriz_palabras), decreasing = TRUE)

# Crear la nube de palabras
png("Figures/Fig_7.png")
wordcloud(names(frecuencia_palabras), frecuencia_palabras, max.words = 100, colors = brewer.pal(8, "Dark2"))
dev.off()


#Validación de expertos

df_lineamientos = read.csv("Encuesta_sobre_lineamientos_UX_GenAI.csv", sep = ",")
df_lineamientos = df_lineamientos[,-1]

write.xlsx(df_lineamientos, "df_lineamientos.xlsx")

colnames(df_lineamientos) <- c("Institución", "País_institución", "Años_Experiencia_UX",
                               "Años_Experiencia_IA", "L1R1", "L2R2", "L1R3", "L1R4",
                               "L1R5", "L1R6", "L1R7", "L1R8", "L1R9", "Comentarios_1",
                               "L2R1", "L2R2", "L2R3", "L2R4", "L2R5", "L2R6", "L2R7",
                               "Comentarios_2", "L3R1", "L3R2", "L3R3", "L3R4", "L3R5",
                               "Comentarios_3", "L4R1", "L4R2", "L4R3", "L4R4", "L4R5",
                               "Comentarios_4", "L5R1", "L5R2", "L5R3", "L5R4", "L5R5",
                               "L5R6", "L5R7", "L5R8", "Comentarios_5", "L6R1", "L6R2",
                               "L6R3", "Comentarios_6") 


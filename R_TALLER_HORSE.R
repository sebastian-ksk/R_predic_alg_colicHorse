#library
library(PASWR)
library(ggplot2)
library(glue)
library(tidyverse)
library(GGally)
library(hrbrthemes)
library(naivebayes)

chance_med<-function(atri){
  clear_dat<-na.omit(atri)
  med <- round(mean(clear_dat),digits = 2)
  med
}

rand.impute <- function(x){
  missing <- is.na(x)
  n.missing <- sum(missing)
  x.obs <- x[!missing]
  imputed <- x
  imputed[missing] <- sample(x.obs, n.missing,replace = TRUE)
  
  
  imputed
  }


data <- read.csv("D:/SEMESTRES/semestre-2020_1/ELECTIVA_II_2020/R_/COLIC_HORSE_CSV.csv",header=T,sep=';',na.strings = '?')


#change data for media
data$X4..temp_rectal<- ifelse(is.na(data$X4..temp_rectal),chance_med(data$X4..temp_rectal),data$X4..temp_rectal)
data$X5..pulso <- ifelse(is.na(data$X5..pulso),chance_med(data$X5..pulso),data$X5..pulso)
data$X6..tasa_respiracion <-  ifelse(is.na(data$X6..tasa_respiracion),chance_med(data$X6..tasa_respiracion),data$X6..tasa_respiracion)

data$X16..ph_reflujo_nasogastrico<-ifelse(is.na(data$X16..ph_reflujo_nasogastrico),chance_med(data$X16..ph_reflujo_nasogastrico),data$X16..ph_reflujo_nasogastrico)

data$X19..volumen_celular<- ifelse(is.na(data$X19..volumen_celular),chance_med(data$X19..volumen_celular),data$X19..volumen_celular)
data$X20..proteina_total<- ifelse(is.na(data$X20..proteina_total),chance_med(data$X20..proteina_total),data$X20..proteina_total)

data$X22..proteina_toal_abdominal <- ifelse(is.na(data$X22..proteina_toal_abdominal),chance_med(data$X22..proteina_toal_abdominal),data$X22..proteina_toal_abdominal)


#change var categoricas
data$X1..cirug�a <- rand.impute(data$X1..cirug�a)

data$X7..temp_extremidades <- rand.impute(data$X7..temp_extremidades)
data$X8..pulso_periferico <- rand.impute(data$X8..pulso_periferico)
data$X9..membrana_mucosa <- rand.impute(data$X9..membrana_mucosa)
data$X10..tiempo_recuperacion_capilar<- rand.impute(data$X10..tiempo_recuperacion_capilar)
data$X11..dolor <- rand.impute(data$X11..dolor)
data$X12..movimiento_peristaltico<- rand.impute(data$X12..movimiento_peristaltico)
data$X13..distension_abdominal<-rand.impute(data$X13..distension_abdominal)
data$X14..tubo_nasogastrico <- rand.impute(data$X14..tubo_nasogastrico)
data$X15..reflujo_nasogastrico<- rand.impute(data$X15..reflujo_nasogastrico)

data$X17..examen_rectal_heces <- rand.impute(data$X17..examen_rectal_heces)
data$X18..abdomen <- rand.impute(data$X18..abdomen)

data$X21..aspecto_fluido_abdominal <- rand.impute(data$X21..aspecto_fluido_abdominal)

data$X24..lesion_quirurgica <- rand.impute(data$X24..lesion_quirurgica)
data$X25..presencia_patologia <- rand.impute(data$X25..presencia_patologia)


#remplazo de etiquetas
data$X23..resultado <- rand.impute(data$X23..resultado)
data$X23..resultado <- ifelse(data$X23..resultado==1,"live","die")



#data <- data[,-3] #elimar columna de hospitales
#organiza los datos los primeros son categoricos y los ultimos continuos
datao<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]

## ANALISIS EXPLORATORIO
a=1
for(i in 1:23) {
  #ggplot(data=datao)+geom_histogram(mapping = aes(x=datao[,i]),binwidth = 0.5)
  print( EDA(datao[,i]))
  #  a=a+1
   # Sys.sleep(0.5)
}

ggparcoord(datao,columns = c(18,19,20,21,22,23),
           groupColumn = 23,
           title = "Parallel Coordinate Plot HORSE COLIC",alphaLines = 1)
+ scale_color_manual("#69b3a2", "#E8E8E8") +
  theme(
    plot.title = element_text(size=0.1)
  )

EDA(datao[,23])

scale_color

EDA(data$X4..temp_rectal)
EDA(data$X5..pulso)
hist(data$X2..edad)
hist(data$X1..cirug�a)
hist(data$X6..tasa_respiracion) #histograma

a1<- data[,c(1,2,3,4,5,6,7,22)]


ggplot(data) +
  aes(x = X23..resultado, y = data[,7], colour = X23..resultado) +
  geom_point(size = 1.12) +
  scale_color_gradient() +
  theme_minimal()

pairs(~data$X2..edad+data$X5..pulso+data$X23..resultado,data = data,
      lower.panel=panel.smooth)

ggpairs(a1,6:8)


pm <- ggpairs(
  data[, c(1, 3, 4, 22)],
  upper = list(continuous = "barDiag", combo = "box"),
  lower = list(continuous = "points", combo = "dot")
)
pm
#####
library(naivebayes)

nrow(datao)
num_reg_entrenamiento<-as.integer(0.75*nrow(datao))
num_reg_entrenamiento
data_train<- sample(nrow(datao), num_reg_entrenamiento)
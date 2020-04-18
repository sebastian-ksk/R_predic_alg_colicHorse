#library
library(PASWR)
library(ggplot2)
library(glue)
library(tidyverse)
library(GGally)
library(hrbrthemes)
library(naivebayes)
library(naivebayes)
library(e1071)
library(caret)
library(C50)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

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
data$X1..cirugía <- rand.impute(data$X1..cirugía)
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

data$X23..resultado <- ifelse(data$X23..resultado==1,1,2)


#data <- data[,-3] #elimar columna de hospitales
#organiza los datos los primeros son categoricos y los ultimos continuos
datao<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]

## ANALISIS EXPLORATORIO

G<-as.character()

# for(i in 1:22) {
#  # print(i)
#   ggplot(data=datao,mapping = aes(x = datao$X23..resultado,y = datao[,i])) +
#     geom_point(na.rm = TRUE)
#     ggsave(path = "figs",
#        "imagen.png"
#       # plot = last_plot(),
#       # device = NULL,
#       # path = NULL,
#       # scale = 1,
#       # width = NA,
#       # height = NA,
#       # units = c("in", "cm", "mm"),
#       # dpi = 300,
#       # limitsize = TRUE,
#   )
#  # Est<-EDA(datao[,i])
#     dev.off()
# }

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
hist(data$X1..cirugía)
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


#########################################################
#######bayes##########################

data$X23..resultado <- ifelse(data$X23..resultado==1,"live","die")
datao<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]


datosCat<- data[,c(1,7:15,17:18,21,24:25,23)]
#convertir datos numericos en caracteres
for (x in 1:16) {
  datosCat[,x] <- as.factor(datosCat[,x])
}
  set.seed(101)
  t.ids <- createDataPartition(datosCat$X23..resultado,p=0.80,list=F)
  mod <- naiveBayes(X23..resultado~.,data = datosCat[t.ids,])
  
  pred<-predict(mod,datosCat[-t.ids,])
  tab<- table(datosCat[-t.ids,]$X23..resultado,pred, dnn = c("ACTUAL","PREDICHA"))
  confusionMatrix(tab)



##########con todos los datos##########################
#######################################################
all_data<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]
#convertir datos numericos en caracteres
for (x in 1:23) {
  all_data[,x] <- as.factor(all_data[,x])
}

set.seed(101)
t.ids <- createDataPartition(all_data$X23..resultado,p=0.8,list=F)
mod <- naiveBayes(X23..resultado~.,data = all_data[t.ids,])

pred<-predict(mod,all_data[-t.ids,])
tab<- table(all_data[-t.ids,]$X23..resultado,pred, dnn = c("ACTUAL","PREDICHA"))
confusionMatrix(tab)

####################################################
#####arbol de clasificacion#########################

set.seed(101)
dat_Tree<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]
tr.id<-createDataPartition(dat_Tree$X23..resultado,p=0.80,list = F)

Tremod<-rpart(X23..resultado~., method = "class",data = dat_Tree[tr.id,])

#print(Tremod)
rpart.plot(Tremod,extra = 100)
#printcp(Tremod)
plotcp(Tremod)

predT<-predict(Tremod,newdata = dat_Tree[-tr.id,],type = "class")
tab<- table(dat_Tree[-tr.id,]$X23..resultado,predT, dnn = c("ACTUAL","PREDICHA"))
confusionMatrix(tab)



###otro arbol

set.seed(100)
dat_Tree_C<- data[,c(1,7:15,17:18,21,24:25,4:6,16,19:20,22,23)]
for (x in 1:23) {
  dat_Tree_C[,x] <- as.factor(dat_Tree_C[,x])
}

tr.id<-createDataPartition(dat_Tree_C$X23..resultado,p=0.80,list = F)
Tremod<-C5.0(X23..resultado~.,data = dat_Tree_C[tr.id,])

summary(Tremod)
plot(Tremod)

predT<-predict(Tremod,newdata = dat_Tree_C[-tr.id,])
tab<- table(dat_Tree_C[-tr.id,]$X23..resultado,predT, dnn = c("ACTUAL","PREDICHA"))
confusionMatrix(tab)
###

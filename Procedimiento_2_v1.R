library(foreign)
library(plyr)
rm(list=ls())
setwd("C:/Users/sburone/Documents/Base Enero")

#Cargo base de actividades y formulario
actividades<-read.csv("base_paso_1_con_fracasos.csv", sep=",")
formulario<-read.csv("C:/Users/sburone/Documents/Base Enero/Formularios/base_con_formulario_y_fracasos.csv", sep=",")
#Me quedo unicamente con los estudiantes que son PLAN 2012 PUROS
actividades2<-actividades[which(actividades$ESTCI%in%formulario$ESTCI),]
formulario$dups<-duplicated(formulario$ESTCI)
formulario<-formulario[which(formulario$dups==FALSE),]
basefinal<-merge(actividades2, formulario, by="ESTCI")

rm("actividades", "actividades2", "formulario")

write.csv(basefinal, "basefinal_con_fracasos.csv")

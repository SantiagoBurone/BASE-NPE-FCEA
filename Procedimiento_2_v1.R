#######################################################
###########################################################
############     PROCEDIMIENTO 2.V1
############        BASE NPE

#Este programa MERGEA la base de actividades con la base de formulario estadistico 
#####################################

library(foreign)
library(plyr)
rm(list=ls())
setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases/auxiliares")

#Cargo base de actividades y formulario
actividades<-read.csv("base_paso_1_con_fracasos2.csv", sep=",")
formulario<-read.dta("base_formulario_montevideo.dta")
formulario$ESTCI<-formulario$CI
#Me quedo unicamente con los estudiantes que son PLAN 2012 PUROS

actividades2<-actividades[which(actividades$ESTCI%in%formulario$ESTCI),]
formulario$dups<-duplicated(formulario$ESTCI)
formulario<-formulario[which(formulario$dups==FALSE),]
formulario$dups<-NULL
basefinal<-merge(actividades2, formulario, by="ESTCI")

rm("actividades", "actividades2", "formulario")

write.dta(basefinal, "basefinal_con_fracasos_formulario.dta")

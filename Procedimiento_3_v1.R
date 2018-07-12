#######################################################
###########################################################
############     PROCEDIMIENTO 3.V1
############        BASE NPE
#####################################

library(foreign)
library(plyr)
#Cargo base de datos
rm(list=ls())
setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases/auxiliares")
base<-read.csv("base_paso_1_con_fracasos2.csv", sep=",")

#Genero variables para áreas de conocimiento (Tomo únicamente áreas de conocimiento del NPE)

base$MAT<-as.character(base$MAT)
base$MAT<- lapply(base$MAT, trimws)
#Materias Métodos Cuantitativos

base$Met_Cuant<-ifelse((base$MAT=="Q114" | base$MAT=="213A" | base$MAT=="227A" | base$MAT=="MC40B" | base$MAT=="MC10" | base$MAT=="114A" | base$MAT=="MC20" | base$MAT=="128A" | base$MAT=="CM128" | base$MAT=="MC30" | base$MAT=="MC40" | base$MAT=="MC72" | base$MAT=="MC45" | base$MAT=="MC31" | base$MAT=="MC42" | base$MAT=="MC51" | base$MAT=="MC31" | base$MAT=="MC44" | base$MAT=="MC43" | base$MAT=="MC52" | base$MAT=="MC61" | base$MAT=="MC70" | base$MAT=="MC71" | base$MAT=="462"), 1, 0)

#Materias Economía
base$Econ<-ifelse((base$MAT=="E10" | base$MAT=="E20" | base$MAT=="E30" | base$MAT=="E33" | base$MAT=="E31" | base$MAT=="E40" | base$MAT=="E41" | base$MAT=="E51" | base$MAT=="E50" | base$MAT=="E60" | base$MAT=="E61" | base$MAT=="E72" | base$MAT=="E73" | base$MAT=="E74" | base$MAT=="E70" | base$MAT=="E71" | base$MAT=="E75" | base$MAT=="E76" | base$MAT=="E77" | base$MAT=="E78" | base$MAT=="E79" | base$MAT=="E90" | base$MAT=="E170" | base$MAT=="E91" | base$MAT=="E80" | base$MAT=="E81" |base$MAT=="E82" |base$MAT=="E83" |base$MAT=="E86" | base$MAT=="E94" | base$MAT=="E87" | base$MAT=="E88" | base$MAT=="E81" | base$MAT=="E53" | base$MAT=="E52" | base$MAT=="E81"), 1, 0)

#Materias Contabilidad
base$Cont<-ifelse((base$MAT=="TC40" | base$MAT=="C10" | base$MAT=="C20" | base$MAT=="C30" | base$MAT=="C31" | base$MAT=="C40" | base$MAT=="C41"| base$MAT=="C50" | base$MAT=="C51" | base$MAT=="C60" | base$MAT=="C61" | base$MAT=="C62" |  base$MAT=="C63" | base$MAT=="C70" | base$MAT=="C72" | base$MAT=="C73" | base$MAT=="C74" | base$MAT=="C81" | base$MAT=="C82" | base$MAT=="C80" | base$MAT=="C83"), 1, 0)

#Materias Administración
base$Admin<-ifelse((base$MAT=="A10" | base$MAT=="A20" | base$MAT=="A30" | base$MAT=="A40" | base$MAT=="A41" | base$MAT=="A42" | base$MAT=="A50" | base$MAT=="A52" | base$MAT=="A54" |  base$MAT=="A61" | base$MAT=="A62" | base$MAT=="A63" | base$MAT=="A67" |  base$MAT=="A65" | base$MAT=="A66" | base$MAT=="A70" | base$MAT=="A71" | base$MAT=="A72" | base$MAT=="A73" | base$MAT=="A76" | base$MAT=="A80" |  base$MAT=="A81" | base$MAT=="A82" | base$MAT=="A83" | base$MAT=="A84" | base$MAT=="A85" | base$MAT=="A86" | base$MAT=="A87"), 1 , 0)

#Materias Jurídica
base$Juridica<-ifelse((base$MAT=="J20TP" |  base$MAT=="J21" | base$MAT=="J30" | base$MAT=="J40TP" |  base$MAT=="J50" |  base$MAT=="J60TP"), 1, 0)

#Materias Social
base$Social<-ifelse((base$MAT=="S20" | base$MAT=="S21" | base$MAT=="S30" | base$MAT=="141" | base$MAT=="S40" | base$MAT=="S41" | base$MAT=="S44" | base$MAT=="S45" | base$MAT=="S46" | base$MAT=="S60" | base$MAT=="S80" | base$MAT=="S90" | base$MAT=="D78" ), 1, 0)

#Materias Actividades Integradoras
base$Act_Int<-ifelse((base$MAT=="I52" | base$MAT=="I97" | base$MAT=="I111" | base$MAT=="I112" | base$MAT=="I113" | base$MAT=="I50" | base$MAT=="I40" |  base$MAT=="I97" | base$MAT=="I114D" | base$MAT=="I60" | base$MAT=="I113" | base$MAT=="I132" | base$MAT=="I80" | base$MAT=="I83" | base$MAT=="I84" | base$MAT=="I51" | base$MAT=="I116" | base$MAT=="I140" | base$MAT=="I132" | base$MAT=="I30A" | base$MAT=="I73" | base$MAT=="ITF12" | base$MAT=="I117" | base$MAT=="I140"), 1, 0)


#base<-subset(base, select=c("ESTCI", "MAT", "NOMMAT", "FECHA", "T", "NOTAMATERIA", "CRCURR", "CARR", "CARR", "NOMCAR", "NOMBRE", "CELULAR", "ANIOFINSEC", "INST", "NOMINST", "TIPOINST", "FECEG", "FECHAING", "resultado_actividad", "CREDITOS_APROBADOS", "anio1", "anio2", "anio3", "anio4", "anio5", "barrio", "mujer", "residen", "lug_nac", "ed_padre", "ed_madre", "hora_tra", "Met_Cuant", "Econ", "Cont", "Admin", "Juridica", "Social", "Act_Int"))
base$FECHAING<-as.integer(as.character(base$FECHAING))
base$ANIOFINSEC<-as.integer(as.character(base$ANIOFINSEC))
base$brecha_sec_fac<-(base$FECHAING)-(base$ANIOFINSEC)

#Calculo escolaridad
data<-base
data$x<-ifelse(data$resultado_actividad == "curso exonerado" | data$resultado_actividad == "examen aprobado" | data$resultado_actividad == "examen perdido", 1, 0)
#data$x<-ifelse((data$x==1 & data$CRCURR==5), 2, ifelse((data$x==1 & data$CRCURR==15), 3, 1))
aux<-aggregate(data$x, by=list(Category=data$ESTCI), FUN=sum)
data22<-data[which(data$resultado_actividad=="curso exonerado" | data$resultado_actividad=="examen aprobado"),]
aux2<-aggregate(data22$NOTAMATERIA, by=list(Category=data22$ESTCI), FUN=sum)
data12<-merge(x=data, y=aux, by.x ="ESTCI", by.y ="Category" )
data12$denominador<-data12$x.y
data12<-merge(x=data12, y=aux2, by.x="ESTCI", by.y ="Category")
data12$nominador<-data12$x
data12$nominador<-as.numeric(data12$nominador)
data12$denominador<-ifelse(data12$denominador==0, NA, data12$denominador)
data12$denominador<-as.numeric(data12$denominador)
data12$escolaridad<-(data12$nominador/data12$denominador)
data12$x.x<-NULL
data12$x.y<-NULL
data12$x<-NULL
data12$denominador<-NULL
data12$nominador<-NULL
base<-data12
rm(aux, aux2, data, data12, data22)

#Guardo hasta acá, ya que lo siguiente demora mucho en correr

base$MAT<-as.character(base$MAT)
write.table(base, "base_con_dummy_area_conocimiento_sin_fe.csv", sep=";", quote=FALSE, row.names = FALSE)

############################################################
##########GENERO CREDITOS POR AREA DE CONOCIMIENTO##########
############################################################
#Genero creditos Metodos cuantitativos
base$cred_obt<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Met_Cuant==1, base$CRCURR, 0 )
#Creditos aprobados totales Metodos Cuantitativos
base$acts_aprob_MC <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Met_Cuant==1,1,0)
base$creditos_aprob_MC <- base$acts_aprob_MC*as.integer(as.character(base$CRCURR))
creditos_estudiantes_MC <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_estudiantes_MC) <- c("ESTCI","CREDITOS_APROBADOS_MC")
base <- join(base,creditos_estudiantes_MC)
#Creditos aprobados por anio (primeros 5 anios solamente) METODOS CUANTITATIVOS
#Creditos antes de entrar a FCEA
base$anio0 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)<floor(as.integer(as.character(base$MINFECHAING))),1,0)
creditos_anio0_MC <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio0_MC) <- c("ESTCI","creditos_anio0_MC")
base <- join(base,creditos_anio0_MC)
#Creditos anio1 METODOS CUANTITATIVOS
creditos_anio1_MC <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio1_MC) <- c("ESTCI","creditos_anio1_MC")
base <- join(base,creditos_anio1_MC)
#Creditos anio2 METODOS CUANTITATIVOS
creditos_anio2_MC <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio2_MC) <- c("ESTCI","creditos_anio2_MC")
base <- join(base,creditos_anio2_MC)
#Creditos anio3 METODOS CUANTITATIVOS
creditos_anio3_MC <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio3_MC) <- c("ESTCI","creditos_anio3_MC")
base <- join(base,creditos_anio3_MC)
#Creditos anio4 METODOS CUANTITATIVOS
creditos_anio4_MC <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio4_MC) <- c("ESTCI","creditos_anio4_MC")
base <- join(base,creditos_anio4_MC)
#Creditos anio5 METODOS CUANTITATIVOS
creditos_anio5_MC <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_MC)})
colnames(creditos_anio5_MC) <- c("ESTCI","creditos_anio5_MC")
base <- join(base,creditos_anio5_MC)

#GENERO CREDITOS ECONOMIA
base$cred_obt_EC<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Econ==1, base$CRCURR, 0 )
#Creditos aprobados totales Economía
base$acts_aprob_EC <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Econ==1,1,0)
base$creditos_aprob_EC <- base$acts_aprob_EC*as.integer(as.character(base$CRCURR))
creditos_estudiantes_EC <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_estudiantes_EC) <- c("ESTCI","CREDITOS_APROBADOS_EC")
base <- join(base,creditos_estudiantes_EC)
creditos_anio0_EC <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio0_EC) <- c("ESTCI","creditos_anio0_EC")
base <- join(base,creditos_anio0_EC)
#Creditos anio 1 Economía
creditos_anio1_EC <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio1_EC) <- c("ESTCI","creditos_anio1_EC")
base <- join(base,creditos_anio1_EC)
#Creditos anio2 ECONOMIA
creditos_anio2_EC <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio2_EC) <- c("ESTCI","creditos_anio2_EC")
base <- join(base,creditos_anio2_EC)
#Creditos anio3 ECONOMIA
creditos_anio3_EC <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio3_EC) <- c("ESTCI","creditos_anio3_EC")
base <- join(base,creditos_anio3_EC)
#Creditos anio4 ECONOMIA
creditos_anio4_EC <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio4_EC) <- c("ESTCI","creditos_anio4_EC")
base <- join(base,creditos_anio4_EC)
#Creditos anio5 ECONOMIA
creditos_anio5_EC <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_EC)})
colnames(creditos_anio5_EC) <- c("ESTCI","creditos_anio5_EC")
base <- join(base,creditos_anio5_EC)
#GENERO CREDITOS ADMINISTRACION
base$cred_obt_ADM<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Admin==1, base$CRCURR, 0 )
#Creditos aprobados totales ADMINISTRACION
base$acts_aprob_ADM <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Admin==1,1,0)
base$creditos_aprob_ADM <- base$acts_aprob_ADM*as.integer(as.character(base$CRCURR))
creditos_estudiantes_ADM <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_estudiantes_ADM) <- c("ESTCI","CREDITOS_APROBADOS_ADM")
base <- join(base,creditos_estudiantes_ADM)
creditos_anio0_ADM <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio0_ADM) <- c("ESTCI","creditos_anio0_ADM")
base <- join(base,creditos_anio0_ADM)
#Creditos anio 1 ADMINISTRACION
creditos_anio1_ADM <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio1_ADM) <- c("ESTCI","creditos_anio1_ADM")
base <- join(base,creditos_anio1_ADM)
#Creditos anio2 ADMINISTRACION
creditos_anio2_ADM <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio2_ADM) <- c("ESTCI","creditos_anio2_ADM")
base <- join(base,creditos_anio2_ADM)
#Creditos anio3 ADMINISTRACION
creditos_anio3_ADM <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio3_ADM) <- c("ESTCI","creditos_anio3_ADM")
base <- join(base,creditos_anio3_ADM)
#Creditos anio4 ADMINISTRACION
creditos_anio4_ADM <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio4_ADM) <- c("ESTCI","creditos_anio4_ADM")
base <- join(base,creditos_anio4_ADM)
#Creditos anio5 ADMINISTRACION
creditos_anio5_ADM <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_ADM)})
colnames(creditos_anio5_ADM) <- c("ESTCI","creditos_anio5_ADM")
base <- join(base,creditos_anio5_ADM)
#GENERO CREDITOS SOCIAL
base$cred_obt_Social<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Social==1, base$CRCURR, 0 )
#Creditos aprobados totales SOCIAL
base$acts_aprob_Social <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Social==1,1,0)
base$creditos_aprob_Social <- base$acts_aprob_Social*as.integer(as.character(base$CRCURR))
creditos_estudiantes_Social <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_estudiantes_Social) <- c("ESTCI","CREDITOS_APROBADOS_Social")
base <- join(base,creditos_estudiantes_Social)
creditos_anio0_Social <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio0_Social) <- c("ESTCI","creditos_anio0_Social")
base <- join(base,creditos_anio0_Social)
#Creditos anio 1 SOCIAL
creditos_anio1_Social <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio1_Social) <- c("ESTCI","creditos_anio1_Social")
base <- join(base,creditos_anio1_Social)
#Creditos anio2 SOCIAL
creditos_anio2_Social <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio2_Social) <- c("ESTCI","creditos_anio2_Social")
base <- join(base,creditos_anio2_Social)
#Creditos anio3 SOCIAL
creditos_anio3_Social <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio3_Social) <- c("ESTCI","creditos_anio3_Social")
base <- join(base,creditos_anio3_Social)
#Creditos anio4 SOCIAL
creditos_anio4_Social <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio4_Social) <- c("ESTCI","creditos_anio4_Social")
base <- join(base,creditos_anio4_Social)
#Creditos anio5 SOCIAL
creditos_anio5_Social <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Social)})
colnames(creditos_anio5_Social) <- c("ESTCI","creditos_anio5_Social")
base <- join(base,creditos_anio5_Social)
#GENERO CREDITOS Juridica
base$cred_obt_Juridica<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Juridica==1, base$CRCURR, 0 )
#Creditos aprobados totales Juridica
base$acts_aprob_Juridica <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Juridica==1,1,0)
base$creditos_aprob_Juridica <- base$acts_aprob_Juridica*as.integer(as.character(base$CRCURR))
creditos_estudiantes_Juridica <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_estudiantes_Juridica) <- c("ESTCI","CREDITOS_APROBADOS_Juridica")
base <- join(base,creditos_estudiantes_Juridica)
creditos_anio0_Juridica <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio0_Juridica) <- c("ESTCI","creditos_anio0_Juridica")
base <- join(base,creditos_anio0_Juridica)
#Creditos anio 1 Juridica
creditos_anio1_Juridica <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio1_Juridica) <- c("ESTCI","creditos_anio1_Juridica")
base <- join(base,creditos_anio1_Juridica)
#Creditos anio2 Juridica
creditos_anio2_Juridica <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio2_Juridica) <- c("ESTCI","creditos_anio2_Juridica")
base <- join(base,creditos_anio2_Juridica)
#Creditos anio3 Juridica
creditos_anio3_Juridica <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio3_Juridica) <- c("ESTCI","creditos_anio3_Juridica")
base <- join(base,creditos_anio3_Juridica)
#Creditos anio4 Juridica
creditos_anio4_Juridica <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio4_Juridica) <- c("ESTCI","creditos_anio4_Juridica")
base <- join(base,creditos_anio4_Juridica)
#Creditos anio5 Juridica
creditos_anio5_Juridica <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Juridica)})
colnames(creditos_anio5_Juridica) <- c("ESTCI","creditos_anio5_Juridica")
base <- join(base,creditos_anio5_Juridica)
#GENERO CREDITOS Act_Int
base$cred_obt_Act_Int<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Act_Int==1, base$CRCURR, 0 )
#Creditos aprobados totales Act_Int
base$acts_aprob_Act_Int <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Act_Int==1,1,0)
base$creditos_aprob_Act_Int <- base$acts_aprob_Act_Int*as.integer(as.character(base$CRCURR))
creditos_estudiantes_Act_Int <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_estudiantes_Act_Int) <- c("ESTCI","CREDITOS_APROBADOS_Act_Int")
base <- join(base,creditos_estudiantes_Act_Int)
creditos_anio0_Act_Int <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio0_Act_Int) <- c("ESTCI","creditos_anio0_Act_Int")
base <- join(base,creditos_anio0_Act_Int)
#Creditos anio 1 Act_Int
creditos_anio1_Act_Int <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio1_Act_Int) <- c("ESTCI","creditos_anio1_Act_Int")
base <- join(base,creditos_anio1_Act_Int)
#Creditos anio2 Act_Int
creditos_anio2_Act_Int <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio2_Act_Int) <- c("ESTCI","creditos_anio2_Act_Int")
base <- join(base,creditos_anio2_Act_Int)
#Creditos anio3 Act_Int
creditos_anio3_Act_Int <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio3_Act_Int) <- c("ESTCI","creditos_anio3_Act_Int")
base <- join(base,creditos_anio3_Act_Int)
#Creditos anio4 Act_Int
creditos_anio4_Act_Int <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio4_Act_Int) <- c("ESTCI","creditos_anio4_Act_Int")
base <- join(base,creditos_anio4_Act_Int)
#Creditos anio5 Act_Int
creditos_anio5_Act_Int <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Act_Int)})
colnames(creditos_anio5_Act_Int) <- c("ESTCI","creditos_anio5_Act_Int")
base <- join(base,creditos_anio5_Act_Int)
#GENERO CREDITOS Contabilidad
base$cred_obt_Cont<-ifelse((base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota") & base$Cont==1, base$CRCURR, 0 )
#Creditos aprobados totales Cont
base$acts_aprob_Cont <- ifelse((base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota")) & base$Cont==1,1,0)
base$creditos_aprob_Cont <- base$acts_aprob_Cont*as.integer(as.character(base$CRCURR))
creditos_estudiantes_Cont <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_estudiantes_Cont) <- c("ESTCI","CREDITOS_APROBADOS_Cont")
base <- join(base,creditos_estudiantes_Cont)
creditos_anio0_Cont <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio0_Cont) <- c("ESTCI","creditos_anio0_Cont")
base <- join(base,creditos_anio0_Cont)
#Creditos anio 1 Cont
creditos_anio1_Cont <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio1_Cont) <- c("ESTCI","creditos_anio1_Cont")
base <- join(base,creditos_anio1_Cont)
#Creditos anio2 Cont
creditos_anio2_Cont <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio2_Cont) <- c("ESTCI","creditos_anio2_Cont")
base <- join(base,creditos_anio2_Cont)
#Creditos anio3 Cont
creditos_anio3_Cont <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio3_Cont) <- c("ESTCI","creditos_anio3_Cont")
base <- join(base,creditos_anio3_Cont)
#Creditos anio4 Cont
creditos_anio4_Cont <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio4_Cont) <- c("ESTCI","creditos_anio4_Cont")
base <- join(base,creditos_anio4_Cont)
#Creditos anio5 Cont
creditos_anio5_Cont <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob_Cont)})
colnames(creditos_anio5_Cont) <- c("ESTCI","creditos_anio5_Cont")
base <- join(base,creditos_anio5_Cont)

#Guardo la base


#Guardo base de datos
setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases")
base$MAT<-as.character(base$MAT)
write.table(base, "base_NPE_v_1_2018_0_0.csv", quote=FALSE, sep=";", row.names = FALSE)

write.dta(base, "base_NPE_v_1_2018_0_0.dta")

########################################################################################
##############    NO CORRER LA SIGUIENTE PARTE A MENOS QUE SE REQUIERA LA VERSION 2 DE 
#############               LA BASE DE DATOS
########################################################################################

#####Las siguientes lineas generan un vector por area de conocimiento con las materias
####aprobadas en esa área de conocimiento

##No se incluye en la version 0 de la base porque solo en algunos casos puntuales se ha 
##requerido de esta información y demora mucho tiempo en correr (MUCHO)

########################################################################################
########################################################################################
########################################################################################

################################################################
#######Genero materias aprobadas por area de conocimiento#######
################################################################

#Metodos Cuantitativos
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Met_Cuant")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Met_Cuant")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Met_Cuant<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Economía
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Econ")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Econ")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Econ<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Contabilidad
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Cont")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Cont")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Cont<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Jurídica
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Juridica")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Juridica")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Juridica<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Administración
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Admin")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Admin")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Admin<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Social
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Social")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Social")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Social<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Actividades integradoras
base <- base[order(base$ESTCI),]
s<-ncol(base)
base[, c(s+1)]<-" "
base[c(1), c(s+1)]<-as.character(base[1, c(s+1)])

for (i in nrow(base):2){
  
  base[c(i), c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Act_Int")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))
  
}

base[1, c(s+1)]<-ifelse(  ((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]) & (base[c(i),c("Act_Int")]==1) & ((base[c(i),c("resultado_actividad")]=="Aprobado/Revalida (sin nota)") | (base[c(i),c("resultado_actividad")]=="curso exonerado") | (base[c(i),c("resultado_actividad")]=="examen aprobado") )), (paste(base[c(i),c("NOMMAT")], (base[c(i+1),c(s+1)]), sep="&")), ifelse((base[c(i),c("ESTCI")]==base[c(i+1), c("ESTCI")]),    (base[c(i+1),c(s+1)]), NA))

base$Aprobaciones_Act_Int<-base[, c(s+1)]
base[, c(s+1)]<-NULL

#Guardo base de datos
setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases")
base$MAT<-as.character(base$MAT)
write.table(base, "base_NPE_v_2018_1_1_0.csv", quote=FALSE, sep=";", row.names = FALSE)
getwd()
write.dta(base, "base_NPE_v_2018_1_1_0.dta")


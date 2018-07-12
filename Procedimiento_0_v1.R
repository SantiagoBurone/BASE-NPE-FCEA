#######################################################
###########################################################
############     PROCEDIMIENTO 0.V1
############        BASE NPE
#####################################

#Seteo el directorio en el que estan los datos y cargo paquetes
rm(list=ls())
require(foreign)
require(plyr)

setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/SGB")



#NOTA: descargar del SGB las bases de datos con la consulta "consulta_por_fecha_actividades.sql".
#Para actualizar la base, fijarse el último período incluido en la versión anterior,
#descargar el siguiente período y nombarlo consecutivamente en las siguientes líneas

#Leo las sub bases de datos
bo<-read.csv("actividades_2018_ene_mar.csv", sep=";", quote= "")
b1<-read.csv("actividades_2017.csv", sep=";", quote = "")
b2_1<-read.csv("fcea_por_carreraprueba1.csv", sep=";", quote = "")
b2_2<-read.csv("fcea_por_carreraprueba2.csv", sep=";", quote = "")
b2<-read.csv("fcea_por_carrera2.csv", sep=";", quote = "")
b3<-read.csv("fcea_por_carrera3.csv", sep=";", quote = "")
b4<-read.csv("fcea_por_carrera4.csv", sep=";", quote = "")
b5<-read.csv("fcea_por_carrera5.csv", sep=";", quote = "")
b6<-read.csv("fcea_por_carrera6.csv", sep=";", quote = "")

#Pego las bases de datos y elimino auxiliares
base<-b1
base<-rbind(base, b2_1)
base<-rbind(base, b2_2)
base<-rbind(base, b2)
base<-rbind(base, b3)
base<-rbind(base, b4)
base<-rbind(base, b5)
base<-rbind(base, b6)
base<-rbind(base, bo)
rm(list=c("b1","b2","b3","b4","b5", "b6", "b2_1", "b2_2", "bo"))

#Elimino espacios en blanco 

base<-base[!(base$T=="-"),]
base<-base[!(base$T==""),]
base<-base[!(base$T==""),]
base$ESTCI<-as.integer(as.character(base$ESTCI))
base$borrar<-ifelse(is.na(base$ESTCI),1,0)
base<-base[!(base$borrar==1),]
base$borrar<-NULL



#####Genero creditos aprobados#####

#Recreo variables
base$FECHAING<-floor(as.integer(as.character(base$FECHAING))/10000)
base$FECHAEG<-floor(as.integer(as.character(base$FECEG))/10000)
base$FECHAMATERIA<-floor(as.integer(as.character(base$FECHA))/10000)
base$NOTAMATERIA<-as.integer(as.character(base$NOTAMATERIA))
base$NOTAMATERIA<-as.integer(as.character(base$NOTAMATERIA))

######################
####GENERO PRIMERA FECHA DE INGRESO
######################

#Genero primera fecha de ingreso
#Genero primer ingreso a FCEA
#Cargo consulta de inscriptos historicos
#Para correr esta parte se debe descargar los inscriptos a FCEA del SGB utilizando la consulta "g_inscriptos.sql"


i<-read.csv("g_inscriptos_consulta1_conv.csv", header = T)
i$X<-NULL

#Genero fecha de ingreso MINFECHAING (primer fecha de ingreso y la pego a la base por ci (ESTCI))
i$FECHAING<-floor(as.integer(as.character(i$FECHA))/10000)
d = aggregate(i$FECHAING,by=list(i$ESTCI), min)
names(d)[1]<-paste("ESTCI")
t2 = merge(i, d, by = ("ESTCI"))
t2$MINFECHAING<-t2$x
t2<-t2[!(duplicated(t2$ESTCI)),]
t2<-subset(t2, select =c("ESTCI", "NOMBRE", "MINFECHAING"))
base2<-merge(base, t2, by=("ESTCI"))
base2$NOMBRE.y<-NULL
base<-base2
remove(list=c("base2", "d", "i", "t2"))


###############################################
#####Genero resultado actividad
##
########################################

base <- base[order(base$ESTCI),]
base$T<-as.character(base$T)
base$T<-ifelse(base$T=="O", "E", base$T)
base$T<-ifelse(base$T==" ", "C", base$T)
base$result_act<-ifelse(base$T=="E" & as.integer(as.character(base$NOTAMATERIA))>=3,"examen aprobado",0)
base$result_act[which(as.integer(as.character(base$NOTAMATERIA))==20)] <- "Aprobado/Revalida (sin nota)"
base$result_act2<-ifelse(base$T=="E" & as.integer(as.character(base$NOTAMATERIA))<3,"examen perdido",0)
base$result_act3 <- ifelse(base$T=="C" & as.integer(as.character(base$NOTAMATERIA))>=3,"curso exonerado",0)
base$result_act4 <- ifelse(base$T=="C" & as.integer(as.character(base$NOTAMATERIA))<3,"curso no exonerado",0)
base$result_act5 <- ifelse(base$T!="C" & base$T!="E", "revalida sin nota",0)
base$resultado_actividad <- ifelse(base$result_act!=0, base$result_act, ifelse(base$result_act2!=0, base$result_act2, ifelse(base$result_act3!=0, base$result_act3, ifelse(base$result_act4!=0, base$result_act4, ifelse(base$result_act5!=0, base$result_act5, 0)))))
base$result_act <- NULL
base$result_act2<- NULL
base$result_act3<- NULL
base$result_act4<- NULL
base$result_act5<- NULL

#################################
#################################
######Genero creditos aprobados
##################################################################
#################################


base$CRCURR<-as.integer(as.character(base$CRCURR))
base$cred_obt<-ifelse(base$resultado_actividad=="curso exonerado" | base$resultado_actividad=="examen aprobado" | base$resultado_actividad=="Aprobado/Revalida (sin nota)" | base$resultado_actividad=="revalida sin nota", base$CRCURR, 0 )
#Elimino los duplicados
base$dups <- duplicated(base[,c("ESTCI","MAT","FECHA","resultado_actividad")])
base <- base[which(base$dups==F),which(colnames(base)%in%c("result_act2","dups")==F)]
base <- base[order(base$ESTCI,base$FECHA,base$NOMMAT),]
base$notaspos <- ifelse(base$NOTAMATERIA>0,1,0)
base$dupsmat <- NA
base[which(base$notaspos==1),"dupsmat"] <- duplicated(base[which(base$notaspos==1),c("ESTCI","MAT")],fromLast=F)
base$dupsmat2 <- NA
base[which(base$notaspos==1),"dupsmat2"] <- duplicated(base[which(base$notaspos==1),c("ESTCI","MAT","NOTAMATERIA")],fromLast=F)
base$dupsnommat <- NA
base[which(base$notaspos==1),"dupsnommat"] <- duplicated(base[which(base$notaspos==1),c("ESTCI","NOMMAT")],fromLast=F)
base$dupsnommat2 <- NA

base[which(base$notaspos==1),"dupsnommat2"] <- duplicated(base[which(base$notaspos==1),c("ESTCI","NOMMAT","NOTAMATERIA")],fromLast=F)
base$nota20 <- ifelse(as.integer(as.character(base$NOTAMATERIA))==20,1,0)
#Correr estas líneas si se desea eliminar los actividades rendidas y no aprobadas (Cursos no exonerados)
#base <- base[which(base$dupsmat+base$dupsmat2+base$dupsnommat+base$dupsnommat2!=4),]
#base <- base[which(base$dupsmat+base$dupsmat2+base$dupsnommat+base$dupsnommat2==0 | (base$dupsmat+base$dupsmat2+base$dupsnommat+base$dupsnommat2!=0 & base$nota20==0)),]
base$acts_aprob <- ifelse(base$resultado_actividad%in%c("examen aprobado","curso exonerado","Aprobado/Revalida (sin nota)","revalida sin nota"),1,0)

#Creditos aprobados totales

base$creditos_aprob <- base$acts_aprob*as.integer(as.character(base$CRCURR))
creditos_estudiantes <- ddply(.data=base,.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_estudiantes) <- c("ESTCI","CREDITOS_APROBADOS")
base <- join(base,creditos_estudiantes)

#Identifica creditos que son revalida

base$aux0 <- ifelse(base$resultado_actividad=="Aprobado/Revalida (sin nota)", 1, 0)

#Genero dummy que es revalida si tiene (R) en el nombre de la materia

base$NOMMAT<-as.character(base$NOMMAT)
base$aux1<-ifelse((grepl(" (R)", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux2<-ifelse((grepl("Revalida", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux3<-ifelse((grepl("Reválida", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux4<-ifelse((grepl("REVALIDA", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux5<-ifelse((grepl("REVÁLIDA", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux6<-ifelse((grepl("RECONOCIDOS", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)
base$aux7<-ifelse((grepl("Reconocidos", base$NOMMAT, fixed=TRUE)==TRUE) & base$NOTAMATERIA!=20,1,0)

base$revalida<-base$aux0+base$aux1+base$aux2+base$aux3+base$aux4+base$aux5+base$aux6+base$aux7

base$aux0<-NULL
base$aux1<-NULL
base$aux2<-NULL
base$aux3<-NULL
base$aux4<-NULL
base$aux5<-NULL
base$aux6<-NULL
base$aux7<-NULL

###############################################################################
#################ESTO ES PARA VER Y MEJORAR###############################
##############

#COMENTARIO PARA MEJORAR: Con estas lineas se identifican bastante bien las revalidas pero siguen quedando casos (464) que tienen revalidas que no estan identificadas

#Un criterio adicional podria ser tomar a todos los que tienen acumuladas mas de 3 aprobaciones en EXACTAMENTE la misma fecha, considerar esas observaciones como revalidas

#####################################################################################


#Creditos antes de entrar a FCEA
base$anio0 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)<floor(as.integer(as.character(base$MINFECHAING))),1,0)
creditos_anio0 <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio0) <- c("ESTCI","creditos_anio0")
base <- join(base,creditos_anio0)

#Creditos anio1
base$anio1 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==floor(as.integer(as.character(base$MINFECHAING))),1,0)
creditos_anio1 <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio1) <- c("ESTCI","creditos_anio1")
base <- join(base,creditos_anio1)

#Creditos revalidas anio 1

base$revalida_anio1<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==floor(as.integer(as.character(base$MINFECHAING))) & base$revalida==1 ,1,0)

creditos_anio1r <- ddply(.data=base[which(base$anio1==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio1r) <- c("ESTCI","creditos_anio1r")
base<-join(base, creditos_anio1r)

#Creditos anio1sr
base$creditos_anio1sr<-ifelse(is.na(base$creditos_anio1r)==FALSE, base$creditos_anio1-base$creditos_anio1r, base$creditos_anio1) 

#Creditos anio2
base$anio2 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(1+floor(as.integer(as.character(base$MINFECHAING)))),1,0)
creditos_anio2 <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio2) <- c("ESTCI","creditos_anio2")
base <- join(base,creditos_anio2)

#Creditos revalidas anio 2 
base$revalida_anio2<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(1+floor(as.integer(as.character(base$MINFECHAING)))) & base$revalida==1 ,1,0)
creditos_anio2r<-ddply(.data=base[which(base$anio2==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio2r) <- c("ESTCI","creditos_anio2r")
base<-join(base, creditos_anio2r)

#Creditos anio2sr
base$creditos_anio2sr<-ifelse(is.na(base$creditos_anio2r)==FALSE, base$creditos_anio2-base$creditos_anio2r, base$creditos_anio2) 

#Creditos anio3
base$anio3 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(2+floor(as.integer(as.character(base$MINFECHAING)))),1,0)
creditos_anio3 <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio3) <- c("ESTCI","creditos_anio3")
base <- join(base,creditos_anio3)

#Creditos revalidas anio 3 
base$revalida_anio3<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(2+floor(as.integer(as.character(base$MINFECHAING)))) & base$revalida==1 ,1,0)
creditos_anio3r<-ddply(.data=base[which(base$anio3==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio3r) <- c("ESTCI","creditos_anio3r")
base<-join(base, creditos_anio3r)

#Creditos anio3sr
base$creditos_anio3sr<-ifelse(is.na(base$creditos_anio3r)==FALSE, base$creditos_anio3-base$creditos_anio3r, base$creditos_anio3) 

#Creditos anio4
base$anio4 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(3+floor(as.integer(as.character(base$MINFECHAING)))),1,0)
creditos_anio4 <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio4) <- c("ESTCI","creditos_anio4")
base <- join(base,creditos_anio4)

#Creditos revalidas anio 4 
base$revalida_anio4<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(3+floor(as.integer(as.character(base$MINFECHAING)))) & base$revalida==1 ,1,0)
creditos_anio4r<-ddply(.data=base[which(base$anio4==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio4r) <- c("ESTCI","creditos_anio4r")
base<-join(base, creditos_anio4r)

#Creditos anio4sr
base$creditos_anio4sr<-ifelse(is.na(base$creditos_anio4r)==FALSE, base$creditos_anio4-base$creditos_anio4r, base$creditos_anio4) 

#Creditos anio5
base$anio5 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(4+floor(as.integer(as.character(base$MINFECHAING)))),1,0)
creditos_anio5 <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio5) <- c("ESTCI","creditos_anio5")
base <- join(base,creditos_anio5)

#Creditos revalidas anio 5 
base$revalida_anio5<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(4+floor(as.integer(as.character(base$MINFECHAING)))) & base$revalida==1 ,1,0)
creditos_anio5r<-ddply(.data=base[which(base$anio5==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio5r) <- c("ESTCI","creditos_anio5r")
base<-join(base, creditos_anio5r)

#Creditos anio5sr
base$creditos_anio5sr<-ifelse(is.na(base$creditos_anio5r)==FALSE, base$creditos_anio5-base$creditos_anio5r, base$creditos_anio5) 

#Creditos anio6
base$anio6 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(5+floor(as.integer(as.character(base$MINFECHAING)))),1,0)
creditos_anio6 <- ddply(.data=base[which(base$anio6==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio6) <- c("ESTCI","creditos_anio6")
base <- join(base,creditos_anio6)

#Creditos revalidas anio 6 
base$revalida_anio6<-ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(5+floor(as.integer(as.character(base$MINFECHAING)))) & base$revalida==1 ,1,0)
creditos_anio6r<-ddply(.data=base[which(base$anio6==1 & base$revalida==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio6r) <- c("ESTCI","creditos_anio6r")
base<-join(base, creditos_anio6r)

#Creditos anio6sr
base$creditos_anio6sr<-ifelse(is.na(base$creditos_anio6r)==FALSE, base$creditos_anio6-base$creditos_anio6r, base$creditos_anio6) 

#Limpio la base
base <- base[,which(colnames(base)%in%c("acts_aprob","creditos_aprob","dupsmat","dupsnommat","dupsmat2","dupsnommat2","nota20")==F)]
rm(aux, aux2, creditos_aprob, cred_obt, creditos_anio0, creditos_anio1, creditos_anio2, creditos_anio3, creditos_anio4, creditos_estudiantes, creditos_anio5)

#Guardo la base (esta base tiene todas las actividades de estudiantes que hicieron algo por el plan nuevo en el periodo analizado)
#Tomar en cuenta que esto incluye estudiantes plan 90 que revalidaron
setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases/auxiliares")
write.csv(base, "base_paso_1_con_fracasos2.csv",  row.names = FALSE)

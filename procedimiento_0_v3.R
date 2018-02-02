setwd("C:/Users/sburone/Documents/Base Enero")
rm(list=ls())
library(foreign)
library(plyr)

#NOTA: descargar del SGB las bases de datos con la consulta "consulta_por_fecha_actividades.sql".
#Para actualizar la base, fijarse el último período incluido en la versión anterior,
#descargar la el siguiente período y nombarlo consecutivamente en las siguientes líneas

#Leo las sub bases de datos
b1<-read.csv("fcea_por_carrera.csv", sep=";")
b2_1<-read.csv("fcea_por_carreraprueba1.csv", sep=";")
b2_2<-read.csv("fcea_por_carreraprueba2.csv", sep=";")
b2<-read.csv("fcea_por_carrera2.csv", sep=";")
b3<-read.csv("fcea_por_carrera3.csv", sep=";")
b4<-read.csv("fcea_por_carrera4.csv", sep=";")
b5<-read.csv("fcea_por_carrera5.csv", sep=";")
b6<-read.csv("fcea_por_carrera6.csv", sep=";")

#Pego las bases de datos y elimino auxiliares
base<-b1
base<-rbind(base, b2_1)
base<-rbind(base, b2_2)
base<-rbind(base, b2)
base<-rbind(base, b3)
base<-rbind(base, b4)
base<-rbind(base, b5)
base<-rbind(base, b6)

rm(list=c("b1","b2","b3","b4","b5", "b6", "b2_1", "b2_2"))

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

#Genero resultado actividad
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

#Genero creditos
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


#Creditos aprobados por anio (primeros 5 anios solamente)
#Creditos antes de entrar a FCEA
base$anio0 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)<floor(as.integer(as.character(base$FECHAING))),1,0)
creditos_anio0 <- ddply(.data=base[which(base$anio0==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio0) <- c("ESTCI","creditos_anio0")
base <- join(base,creditos_anio0)
#Creditos anio1
base$anio1 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==floor(as.integer(as.character(base$FECHAING))),1,0)
creditos_anio1 <- ddply(.data=base[which(base$anio1==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio1) <- c("ESTCI","creditos_anio1")
base <- join(base,creditos_anio1)
#Creditos anio2
base$anio2 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(1+floor(as.integer(as.character(base$FECHAING)))),1,0)
creditos_anio2 <- ddply(.data=base[which(base$anio2==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio2) <- c("ESTCI","creditos_anio2")
base <- join(base,creditos_anio2)
#Creditos anio3
base$anio3 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(2+floor(as.integer(as.character(base$FECHAING)))),1,0)
creditos_anio3 <- ddply(.data=base[which(base$anio3==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio3) <- c("ESTCI","creditos_anio3")
base <- join(base,creditos_anio3)
#Creditos anio4
base$anio4 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(3+floor(as.integer(as.character(base$FECHAING)))),1,0)
creditos_anio4 <- ddply(.data=base[which(base$anio4==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio4) <- c("ESTCI","creditos_anio4")
base <- join(base,creditos_anio4)
#Creditos anio5
base$anio5 <- ifelse(floor(as.integer(as.character(base$FECHA))/10000)==(4+floor(as.integer(as.character(base$FECHAING)))),1,0)
creditos_anio5 <- ddply(.data=base[which(base$anio5==1),],.variables=c("ESTCI"),.fun=function(x){sum(x$creditos_aprob)})
colnames(creditos_anio5) <- c("ESTCI","creditos_anio5")
base <- join(base,creditos_anio5)


#Limpio la base
base <- base[,which(colnames(base)%in%c("acts_aprob","creditos_aprob","dupsmat","dupsnommat","dupsmat2","dupsnommat2","nota20")==F)]
rm(aux, aux2, creditos_aprob, cred_obt, creditos_anio0, creditos_anio1, creditos_anio2, creditos_anio3, creditos_anio4, creditos_estudiantes, creditos_anio5)

#Guardo la base

write.csv(base, "base_paso_1_con_fracasos.csv")

##########################################################################
##############Pegado de la base con formularios estadísticos##############
##########################################################################

setwd("C:/Users/sburone/Documents/Base Enero/Formularios")

#Genero una base por generación sin duplicados

base2<-base[which(!duplicated(base$ESTCI)),]

#Genero primer ingreso a FCEA

#Cargo consulta de inscriptos historicos
#Para correr esta parte se debe descargar los inscriptos a FCEA del SGB utilizando la consulta "g_inscriptos.sql"
i<-read.csv("g_inscriptos_consulta1_conv.csv", header = T)
i$X<-NULL
i$FECHAING<-floor(as.integer(as.character(i$FECHA))/10000)
d = aggregate(i$FECHAING,by=list(i$ESTCI), min)
names(d)[1]<-paste("ESTCI")
t2 = merge(i, d, by = ("ESTCI"))
t2$FECHAING2<-t2$x
t2<-t2[!(duplicated(t2$ESTCI)),]

base3<-merge(base2, t2, by=("ESTCI"))

gen2012<-base3[which(base3$FECHAING2==2012),]
gen2013<-base3[which(base3$FECHAING2==2013),]
gen2014<-base3[which(base3$FECHAING2==2014),]
gen2015<-base3[which(base3$FECHAING2==2015),]
gen2016<-base3[which(base3$FECHAING2==2016),]
gen2017<-base3[which(base3$FECHAING2==2017),]

#Cargo formularios estadísticos
#Para correr esta parte se debe contar con los formularios estadísticos de ingreso
#de las generaciones.

ing2010<-read.spss("2010.sav", to.data.frame = TRUE)
ing2010$ESTCI<-as.integer(as.character(ing2010$C.I))
ing2011<-read.spss("2011.sav", to.data.frame = TRUE)
ing2011$ESTCI<-as.integer(as.character(ing2011$cedula))
ing2012<-read.spss("2012.sav", to.data.frame = TRUE)
ing2012$ESTCI<-as.integer(as.character(ing2012$C_I))
ing2013<-read.spss("2013.sav", to.data.frame = TRUE)
ing2013$ESTCI<-as.integer(as.character(ing2013$CI))
ing2014<-read.spss("2014.sav", to.data.frame = TRUE)
ing2014$ESTCI<-as.integer(as.character(ing2014$CI))
ing2015<-read.spss("2015.sav", to.data.frame = TRUE)
ing2015$ESTCI<-as.integer(as.character(ing2015$CI))
ing2016<-read.spss("2016.sav",  to.data.frame = TRUE)
ing2016$ESTCI<-as.integer(as.character(ing2016$CI))


#Combpatibilizo 

colnames(ing2012)[which(colnames(ing2012)=="c_i")]<-"ci"
total12<-join(gen2012,ing2012,by="ESTCI")
write.csv(total12,"gen2012_sindup_confest2.csv")

colnames(ing2013)[which(colnames(ing2013)=="c_i")]<-"ci"
total13<-join(gen2013,ing2013,by="ESTCI")
write.csv(total13,"gen2013_sindup_confest2.csv")

colnames(ing2014)[which(colnames(ing2014)=="c_i")]<-"ci"
total14<-join(gen2014,ing2014,by="ESTCI")
write.csv(total14,"gen2014_sindup_confest2.csv")

colnames(ing2015)[which(colnames(ing2015)=="CI")]<-"ci"
total15<-join(gen2015,ing2015,by="ESTCI")
write.csv(total15,"gen2015_sindup_confest2.csv")

colnames(ing2016)[which(colnames(ing2016)=="CI")]<-"ci"
total16<-join(gen2016,ing2016,by="ESTCI")
write.csv(total16,"gen2016_sindup_confest2.csv")


base4<-base3[which(base3$FECHAING2==2012 | base3$FECHAING2==2013 | base3$FECHAING2==2014 | base3$FECHAING2==2015 | base3$FECHAING2==2016),]

base_final<-base[which(base$ESTCI%in%base4$ESTCI),]
rm(base, base2, base3, base4, d, gen2012, gen2013, gen2014, gen2015, gen2016, gen2017, i, t2)
setwd("C:/Users/sburone/Documents/Base Enero")
write.csv(base_final, "base_paso_1.csv")


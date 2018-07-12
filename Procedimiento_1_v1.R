#######################################################
###########################################################
############     PROCEDIMIENTO 1.V1
############        BASE NPE
#####################################

#Cargo datos formularios estadísticos Montevideo
library(foreign)
library(plyr)
rm(list=ls())

setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/SGB")

#Lo primero que hay que hacer es cargar los formularioes estadisticos y no la base con 
#las actividades del formulario estadistico


load("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/precargadas.Rdata")
#gen20122<-read.dta("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/2012.dta")
#gen20132<-read.dta("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/2013.dta")
#gen20142<-read.dta("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/2014.dta")
#gen20152<-read.dta("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/2015.dta")
#gen20162<-read.dta("C:/Users/sburone/Documents/BASE NPE FINAL/Datos/DGplan/2016.dta")



###################################
######Compatibilizado de variables#
###################################

#############Sexo (mujer)#############
gen2012$mujer<-ifelse(gen2012$Sexo=="Mujer",1,0)
gen2013$mujer<-ifelse(gen2013$sexo=="F",1,0)
gen2014$mujer<-ifelse(gen2014$sexo=="F",1,0)
gen2015$mujer<-ifelse(gen2015$sexo=="Mujer",1,0)
gen2016$mujer<-ifelse(gen2016$sexo=="Mujeres",1,0)

#############lugar de nacimiento (lug_nac)#############

gen2012$lug_nac<-gen2012$nac_lug
gen2013$lug_nac<-gen2013$lug_nac
gen2014$lug_nac<-gen2014$lug_nac
gen2015$lug_nac<-gen2015$nac_lug
gen2016$lug_nac<-gen2016$lug_nac

gen2012$lug_nac <- ifelse(as.numeric(gen2012$lug_nac) > 1 & as.numeric(gen2012$lug_nac) < 20, 1,
                          ifelse(as.numeric(gen2012$lug_nac) > 19,3,2))

gen2013$lug_nac <- ifelse(as.numeric(gen2013$lug_nac) > 1 & as.numeric(gen2013$lug_nac) < 20, 1,
                          ifelse(as.numeric(gen2013$lug_nac) > 19,3,2))

gen2014$lug_nac <- ifelse(as.numeric(gen2014$lug_nac) > 1 & as.numeric(gen2014$lug_nac) < 20, 1,
                          ifelse(as.numeric(gen2014$lug_nac) > 19,3,2))

gen2015$lug_nac <- ifelse(as.numeric(gen2015$lug_nac) > 1 & as.numeric(gen2015$lug_nac) < 20, 1,
                          ifelse(as.numeric(gen2015$lug_nac) > 19,3,2))

gen2016$lug_nac <- ifelse(as.numeric(gen2016$lug_nac) > 1 & as.numeric(gen2016$lug_nac) < 20, 1,
                          ifelse(as.numeric(gen2016$lug_nac) > 19,3,2))

#############lugar de residencia en marzo del año previo al ingreso (res_marzo)#############

#gen2012$res_marzo <- ifelse(as.numeric(gen2012$res_marzo) > 1 & as.numeric(gen2012$res_marzo) < 20, 1,
 #                           ifelse(as.numeric(gen2012$res_marzo) > 19,3,2))

#gen2013$res_marzo <- ifelse(as.numeric(gen2013$res_marzo) > 1 & as.numeric(gen2013$res_marzo) < 20, 1,
 #                           ifelse(as.numeric(gen2013$res_marzo) > 19,3,2))

#gen2014$res_marzo <- ifelse(as.numeric(gen2014$res_marzo) > 1 & as.numeric(gen2014$res_marzo) < 20, 1,
 #                           ifelse(as.numeric(gen2014$res_marzo) > 19,3,2))

#gen2015$res_marzo <- ifelse(as.numeric(gen2015$res_marzo) > 1 & as.numeric(gen2015$res_marzo) < 20, 1,
 #                           ifelse(as.numeric(gen2015$res_marzo) > 19,3,2))

#gen2016$res_marzo <- ifelse(as.numeric(gen2016$res_marzo) > 1 & as.numeric(gen2016$res_marzo) < 20, 1,
 #                           ifelse(as.numeric(gen2016$res_marzo) > 19,3,2))


gen2012$res_marzo<-ifelse(gen2012$res_marzo=="Montevideo", as.character("Montevideo"),
                          ifelse(gen2012$res_marzo=="Canelones", as.character("Canelones"),
                                 ifelse(gen2012$res_marzo=="Flores", as.character("Flores"),
                                        ifelse(gen2012$res_marzo=="Florida", as.character("Florida"),
                                               ifelse(gen2012$res_marzo=="Lavalleja", as.character("Lavalleja"),
                                                      ifelse(gen2012$res_marzo=="Rocha", as.character("Rocha"),
                                                             ifelse(gen2012$res_marzo=="Treinta y Tres", as.character("Treinta y Tres"),
                                                                    ifelse(gen2012$res_marzo=="Durazno", as.character("Durazno"),
                                                                           ifelse(gen2012$res_marzo=="Maldonado", as.character("Maldonado"),
                                                                                  ifelse(gen2012$res_marzo=="Soriano", as.character("Soriano"),
                                                                                         ifelse(gen2012$res_marzo=="Artigas", as.character("Artigas"), 
                                                                                                ifelse(gen2012$res_marzo=="Rivera", as.character("Rivera"),
                                                                                                       ifelse(gen2012$res_marzo=="Salto", as.character("Salto"),
                                                                                                              ifelse(gen2012$res_marzo=="Colonia", as.character("Colonia"),
                                                                                                                     ifelse(gen2012$res_marzo=="Cerro Largo", as.character("Cerro Largo"),
                                                                                                                            ifelse(gen2012$res_marzo%in%c("Paysandu", "Paysandú"), as.character("Paysandú"),
                                                                                                                                   ifelse(gen2012$res_marzo%in%c("Tacuarembo", "Tacuarembó"), as.character("Tacuarembó"),
                                                                                                                                          ifelse(gen2012$res_marzo%in%c("Río Negro", "Rio Negro"), as.character("Río Negro"),
                                                                                                                                                 ifelse(gen2012$res_marzo%in%c("San Jose", "San José"), as.character("San José"),
                                                                                                                                                        "Otro")))))))))))))))))))

gen2013$res_marzo<-ifelse(gen2013$res_marzo=="Montevideo", as.character("Montevideo"),
                          ifelse(gen2013$res_marzo=="Canelones", as.character("Canelones"),
                                 ifelse(gen2013$res_marzo=="Flores", as.character("Flores"),
                                        ifelse(gen2013$res_marzo=="Florida", as.character("Florida"),
                                               ifelse(gen2013$res_marzo=="Lavalleja", as.character("Lavalleja"),
                                                      ifelse(gen2013$res_marzo=="Rocha", as.character("Rocha"),
                                                             ifelse(gen2013$res_marzo=="Treinta y Tres", as.character("Treinta y Tres"),
                                                                    ifelse(gen2013$res_marzo=="Durazno", as.character("Durazno"),
                                                                           ifelse(gen2013$res_marzo=="Maldonado", as.character("Maldonado"),
                                                                                  ifelse(gen2013$res_marzo=="Soriano", as.character("Soriano"),
                                                                                         ifelse(gen2013$res_marzo=="Artigas", as.character("Artigas"), 
                                                                                                ifelse(gen2013$res_marzo=="Rivera", as.character("Rivera"),
                                                                                                       ifelse(gen2013$res_marzo=="Salto", as.character("Salto"),
                                                                                                              ifelse(gen2013$res_marzo=="Colonia", as.character("Colonia"),
                                                                                                                     ifelse(gen2013$res_marzo=="Cerro Largo", as.character("Cerro Largo"),
                                                                                                                            ifelse(gen2013$res_marzo%in%c("Paysandu", "Paysandú"), as.character("Paysandú"),
                                                                                                                                   ifelse(gen2013$res_marzo%in%c("Tacuarembo", "Tacuarembó"), as.character("Tacuarembó"),
                                                                                                                                          ifelse(gen2013$res_marzo%in%c("Río Negro", "Rio Negro"), as.character("Río Negro"),
                                                                                                                                                 ifelse(gen2013$res_marzo%in%c("San Jose", "San José"), as.character("San José"),
                                                                                                                                                        "Otro")))))))))))))))))))


gen2014$res_marzo<-ifelse(gen2014$res_marzo=="Montevideo", as.character("Montevideo"),
                          ifelse(gen2014$res_marzo=="Canelones", as.character("Canelones"),
                                 ifelse(gen2014$res_marzo=="Flores", as.character("Flores"),
                                        ifelse(gen2014$res_marzo=="Florida", as.character("Florida"),
                                               ifelse(gen2014$res_marzo=="Lavalleja", as.character("Lavalleja"),
                                                      ifelse(gen2014$res_marzo=="Rocha", as.character("Rocha"),
                                                             ifelse(gen2014$res_marzo=="Treinta y Tres", as.character("Treinta y Tres"),
                                                                    ifelse(gen2014$res_marzo=="Durazno", as.character("Durazno"),
                                                                           ifelse(gen2014$res_marzo=="Maldonado", as.character("Maldonado"),
                                                                                  ifelse(gen2014$res_marzo=="Soriano", as.character("Soriano"),
                                                                                         ifelse(gen2014$res_marzo=="Artigas", as.character("Artigas"), 
                                                                                                ifelse(gen2014$res_marzo=="Rivera", as.character("Rivera"),
                                                                                                       ifelse(gen2014$res_marzo=="Salto", as.character("Salto"),
                                                                                                              ifelse(gen2014$res_marzo=="Colonia", as.character("Colonia"),
                                                                                                                     ifelse(gen2014$res_marzo=="Cerro Largo", as.character("Cerro Largo"),
                                                                                                                            ifelse(gen2014$res_marzo%in%c("Paysandu", "Paysandú"), as.character("Paysandú"),
                                                                                                                                   ifelse(gen2014$res_marzo%in%c("Tacuarembo", "Tacuarembó"), as.character("Tacuarembó"),
                                                                                                                                          ifelse(gen2014$res_marzo%in%c("Río Negro", "Rio Negro"), as.character("Río Negro"),
                                                                                                                                                 ifelse(gen2014$res_marzo%in%c("San Jose", "San José"), as.character("San José"),
                                                                                                                                                        "Otro")))))))))))))))))))



gen2015$res_marzo<-ifelse(gen2015$res_marzo=="Montevideo", as.character("Montevideo"),
                          ifelse(gen2015$res_marzo=="Canelones", as.character("Canelones"),
                                 ifelse(gen2015$res_marzo=="Flores", as.character("Flores"),
                                        ifelse(gen2015$res_marzo=="Florida", as.character("Florida"),
                                               ifelse(gen2015$res_marzo=="Lavalleja", as.character("Lavalleja"),
                                                      ifelse(gen2015$res_marzo=="Rocha", as.character("Rocha"),
                                                             ifelse(gen2015$res_marzo=="Treinta y Tres", as.character("Treinta y Tres"),
                                                                    ifelse(gen2015$res_marzo=="Durazno", as.character("Durazno"),
                                                                           ifelse(gen2015$res_marzo=="Maldonado", as.character("Maldonado"),
                                                                                  ifelse(gen2015$res_marzo=="Soriano", as.character("Soriano"),
                                                                                         ifelse(gen2015$res_marzo=="Artigas", as.character("Artigas"), 
                                                                                                ifelse(gen2015$res_marzo=="Rivera", as.character("Rivera"),
                                                                                                       ifelse(gen2015$res_marzo=="Salto", as.character("Salto"),
                                                                                                              ifelse(gen2015$res_marzo=="Colonia", as.character("Colonia"),
                                                                                                                     ifelse(gen2015$res_marzo=="Cerro Largo", as.character("Cerro Largo"),
                                                                                                                            ifelse(gen2015$res_marzo%in%c("Paysandu", "Paysandú"), as.character("Paysandú"),
                                                                                                                                   ifelse(gen2015$res_marzo%in%c("Tacuarembo", "Tacuarembó"), as.character("Tacuarembó"),
                                                                                                                                          ifelse(gen2015$res_marzo%in%c("Río Negro", "Rio Negro"), as.character("Río Negro"),
                                                                                                                                                 ifelse(gen2015$res_marzo%in%c("San Jose", "San José"), as.character("San José"),
                                                                                                                                                        "Otro")))))))))))))))))))


gen2016$res_marzo<-ifelse(gen2016$res_marzo=="Montevideo", as.character("Montevideo"),
                          ifelse(gen2016$res_marzo=="Canelones", as.character("Canelones"),
                                 ifelse(gen2016$res_marzo=="Flores", as.character("Flores"),
                                        ifelse(gen2016$res_marzo=="Florida", as.character("Florida"),
                                               ifelse(gen2016$res_marzo=="Lavalleja", as.character("Lavalleja"),
                                                      ifelse(gen2016$res_marzo=="Rocha", as.character("Rocha"),
                                                             ifelse(gen2016$res_marzo=="Treinta y Tres", as.character("Treinta y Tres"),
                                                                    ifelse(gen2016$res_marzo=="Durazno", as.character("Durazno"),
                                                                           ifelse(gen2016$res_marzo=="Maldonado", as.character("Maldonado"),
                                                                                  ifelse(gen2016$res_marzo=="Soriano", as.character("Soriano"),
                                                                                         ifelse(gen2016$res_marzo=="Artigas", as.character("Artigas"), 
                                                                                                ifelse(gen2016$res_marzo=="Rivera", as.character("Rivera"),
                                                                                                       ifelse(gen2016$res_marzo=="Salto", as.character("Salto"),
                                                                                                              ifelse(gen2016$res_marzo=="Colonia", as.character("Colonia"),
                                                                                                                     ifelse(gen2016$res_marzo=="Cerro Largo", as.character("Cerro Largo"),
                                                                                                                            ifelse(gen2016$res_marzo%in%c("Paysandu", "Paysandú"), as.character("Paysandú"),
                                                                                                                                   ifelse(gen2016$res_marzo%in%c("Tacuarembo", "Tacuarembó"), as.character("Tacuarembó"),
                                                                                                                                          ifelse(gen2016$res_marzo%in%c("Río Negro", "Rio Negro"), as.character("Río Negro"),
                                                                                                                                                 ifelse(gen2016$res_marzo%in%c("San Jose", "San José"), as.character("San José"),
                                                                                                                                                        "Otro")))))))))))))))))))






###################################
######Ensamblado de bases##########
###################################

#############tipo de vivienda actual (tip_viv)#############
levels(gen2012$vive_en)<-c("Casa", "Pensión/Hotel", "Hogar estudiantil", "Otros")
gen2012$tip_viv<-gen2012$vive_en
levels(gen2013$tip_viv)<-c("Casa", "Pensión/Hotel", "Hogar estudiantil", "Otros")
levels(gen2014$tip_viv)<-c("Casa", "Pensión/Hotel", "Hogar estudiantil", "Otros")
levels(gen2015$tip_viv)<-levels(gen2016$tip_viv)

#############numero de hijos (hij_num)#############
gen2012$hij_num<- gen2012$hijos 
gen2013$hij_num<-gen2013$hijos
gen2014$hij_num<-gen2014$hijos
gen2015$hij_num<-gen2015$hijos
gen2016$hij_num<-gen2016$hijos

#############vive solo (viv_solo)#############
gen2012$viv_solo <- ifelse(as.integer(gen2012$viv_solo) == 2, 1, 0)
gen2013$viv_solo <- ifelse(as.integer(gen2013$viv_solo) == 2, 1, 0)
gen2016$viv_solo <- ifelse(as.integer(gen2016$viv_solo) == 2, 1, 0)

#############cantidad de personas que viven con ud (ocup)#############
gen2012$ocup<- gen2012$pers_num

#############cantidad de personas que viven con ud y perciben ingresos(ocup_ing)#############
gen2012$ocup_ing<-gen2012$pers_Y

#############estado conyugal (est_cony)#############
levels(gen2012$est_cony)<-c(levels(gen2012$est_cony), "Sin Dato")
levels(gen2016$est_cony)<-c(levels(gen2012$est_cony), "Sin Dato")

#############cantidad de padres que viven con ud (padres)#############
#Este esta bien

#############cantidad de padres que viven con ud y perciben ingresos (pad_ing)#############
gen2012$pad_ing<-gen2012$Y_padres
gen2013$pad_ing<-gen2013$Pad_ing
gen2014$pad_ing<-gen2014$Pad_ing
gen2015$pad_ing<-gen2015$Pad_ing
gen2016$pad_ing<-gen2016$Pad_ing

#############cantidad de conyuges que viven con ud (conyug)#############
gen2016$conyug <- ifelse(gen2016$conyug=="Sí",1,0)

#############cantidad de conyuges que viven con ud  y perciben ingresos (cony_ing)#############
gen2012$cony_ing<-gen2012$Y_conyug
gen2013$cony_ing<-gen2013$Cony_ing
gen2014$cony_ing<-gen2014$Cony_ing
gen2015$cony_ing<-gen2015$Cony_ing
gen2016$cony_ing<-gen2016$Cony_ing

#############cantidad de conyuges de padres que viven con ud (cony_pad)#############
gen2012$cony_pad<-gen2012$cony_p_m
gen2016$cony_pad<-gen2016$cony_pad

#############cantidad de conyuges de padres que viven con ud  y perciben ingresos (cony_pad_ing)#############
gen2012$cony_pad_ing <- gen2012$Y_cony_p_m
gen2013$cony_pad_ing <- gen2013$Cony_pad_ing
gen2014$cony_pad_ing <- gen2014$Cony_pad_ing
gen2015$cony_pad_ing <- gen2015$Cony_pad_ing
gen2016$cony_pad_ing <- gen2016$Cony_pad_ing
   
#############cantidad de hijos que viven con ud (hijo_viv)#############
gen2012$hijo_viv<-gen2012$hijos

#############cantidad de hijos que viven con ud  y perciben ingresos (hijo_ing)#############
gen2012$hijo_ing<-gen2012$Y_hijos
gen2013$hijo_ing<-gen2013$Hijo_ing
gen2014$hijo_ing<-gen2014$Hijo_ing
gen2015$hijo_ing<-gen2015$Hijo_ing
gen2016$hijo_ing<-gen2016$Hijo_ing

#############cantidad de hermanos que viven con ud (herman)#############
#Este esta bien

#############cantidad de hermanos que viven con ud  y perciben ingresos (herm_ing)#############
gen2012$herm_ing<-gen2012$Y_herman
gen2013$herm_ing<-gen2013$Herm_ing
gen2014$herm_ing<-gen2014$Herm_ing
gen2015$herm_ing<-gen2015$Herm_ing
gen2016$herm_ing<-gen2016$Herm_ing

#############cantidad de abuelos que viven con ud (abuelo)#############
#Este esta bien
#############cantidad de abuelos que viven con ud  y perciben ingresos (abu_ing)#############
gen2012$abu_ing<-gen2012$Y_abuelo
gen2013$abu_ing<-gen2013$Abu_ing
gen2014$abu_ing<-gen2014$Abu_ing
gen2015$abu_ing<-gen2015$Abu_ing
gen2016$abu_ing<-gen2016$Abu_ing

#############cantidad de suegros que viven con ud (suegro)#############
gen2012$suegro<-gen2012$suegros

#############cantidad de suegros que viven con ud  y perciben ingresos (sueg_ing)#############
gen2012$sueg_ing<-gen2012$Y_suegros
gen2013$sueg_ing<-gen2013$Sueg_ing
gen2014$sueg_ing<-gen2014$Sueg_ing
gen2015$sueg_ing<-gen2015$Sueg_ing
gen2016$sueg_ing<-gen2016$Sueg_ing

#############cantidad de otros familiares que viven con ud (fam_otro)#############
#Este esta bien

#############cantidad de otros familiares que viven con ud  y perciben ingresos (fam_ing)#############
gen2012$fam_ing<-gen2012$Y_fam_otro
gen2013$fam_ing<-gen2013$Fam_ing
gen2014$fam_ing<-gen2014$Fam_ing
gen2015$fam_ing<-gen2015$Fam_ing
gen2016$fam_ing<-gen2016$Fam_ing

#############cantidad de otros estudiantes que viven con ud (est_otro)#############
gen2012$est_otro<-gen2012$estud

#############cantidad de otros estudiantes que viven con ud  y perciben ingresos (est_ing)#############
gen2012$est_ing<-gen2012$Y_estud
gen2013$est_ing<-gen2013$Est_ing
gen2014$est_ing<-gen2014$Est_ing
gen2015$est_ing<-gen2015$Est_ing
gen2016$est_ing<-gen2016$Est_ing

#############cantidad de otras personas que viven con ud (per_otro)#############
gen2012$per_otro<-gen2012$otros

#############cantidad de otras personas que viven con ud  y perciben ingresos (per_ing)#############
gen2012$per_ing<-gen2012$Y_otros
gen2013$per_ing<-gen2013$Per_ing
gen2014$per_ing<-gen2014$Per_ing
gen2015$per_ing<-gen2015$Per_ing
gen2016$per_ing<-gen2016$Per_ing

#############region donde curso educacion primaria (edu_prim)#############
gen2012$edu_prim<-gen2012$primaria_region

gen2012$edu_prim <- ifelse(as.numeric(gen2012$edu_prim) > 1 & as.numeric(gen2012$edu_prim) < 20, 1,
                          ifelse(as.numeric(gen2012$edu_prim) > 19, 3, 2))

gen2013$edu_prim <- ifelse(as.numeric(gen2013$edu_prim) > 2 & as.numeric(gen2013$edu_prim) < 21, 1,
                          ifelse(as.numeric(gen2013$edu_prim) > 20, 3,
                                 ifelse(as.numeric(gen2013$edu_prim) == 1, NA, 2)))
                          
gen2014$edu_prim <- ifelse(as.numeric(gen2014$edu_prim) > 1 & as.numeric(gen2014$edu_prim) < 20, 1,
                          ifelse(as.numeric(gen2014$edu_prim) > 19, 3,
                                 ifelse(as.numeric(gen2014$edu_prim) == 32, NA, 2)))

gen2015$edu_prim <- ifelse(as.numeric(gen2015$edu_prim) > 2 & as.numeric(gen2015$edu_prim) < 21, 1,
                          ifelse(as.numeric(gen2015$edu_prim) > 20, 3,
                                 ifelse(as.numeric(gen2015$edu_prim) == 1, NA, 2)))

gen2016$edu_prim <- ifelse(as.numeric(gen2016$edu_prim) > 1 & as.numeric(gen2016$edu_prim) < 20, 1,
                          ifelse(as.numeric(gen2016$edu_prim) > 19, 3, 2))

#############sector en que curso educacion primaria (tip_inst_pri)#############
gen2012$tip_inst_pri <- gen2012$primaria_sector

gen2014$tip_inst_pri <- ifelse(gen2014$tip_inst_pri == "Sin dato", NA, gen2014$tip_inst_pri)
gen2014$tip_inst_pri <- as.factor(gen2014$tip_inst_pri)
levels(gen2014$tip_inst_pri) <- levels(gen2012$tip_inst_pri)

gen2016$tip_inst_pri <- ifelse(gen2016$tip_inst_pri == "Sin datos", NA, gen2016$tip_inst_pri)
gen2016$tip_inst_pri <- as.factor(gen2016$tip_inst_pri)
levels(gen2016$tip_inst_pri) <- levels(gen2012$tip_inst_pri)

#############region donde curso primeros 5 anos de secundaria (sec_1_5)#############
gen2012$sec_1_5<-gen2012$secund_region

gen2012$sec_1_5 <- ifelse(as.numeric(gen2012$sec_1_5) > 1 & as.numeric(gen2012$sec_1_5) < 20, 1,
                           ifelse(as.numeric(gen2012$sec_1_5) > 19, 3, 2))

gen2013$sec_1_5 <- ifelse(as.numeric(gen2013$sec_1_5) > 2 & as.numeric(gen2013$sec_1_5) < 21, 1,
                          ifelse(as.numeric(gen2013$sec_1_5) > 20, 3,
                                 ifelse(as.numeric(gen2013$sec_1_5) == 1, NA, 2)))

gen2014$sec_1_5 <- ifelse(as.numeric(gen2014$sec_1_5) > 1 & as.numeric(gen2014$sec_1_5) < 20, 1,
                          ifelse(as.numeric(gen2014$sec_1_5) > 19, 3,
                                 ifelse(as.numeric(gen2014$sec_1_5) == 32, NA, 2)))

gen2015$sec_1_5 <- ifelse(as.numeric(gen2015$sec_1_5) > 2 & as.numeric(gen2015$sec_1_5) < 21, 1,
                          ifelse(as.numeric(gen2015$sec_1_5) > 20, 3,
                                 ifelse(as.numeric(gen2013$sec_1_5) == 1 , NA, 2)))

gen2016$sec_1_5 <- ifelse(as.numeric(gen2016$sec_1_5) > 2 & as.numeric(gen2016$sec_1_5) < 21, 1,
                          ifelse(as.numeric(gen2016$sec_1_5) > 20, 3, 
                                 ifelse(as.numeric(gen2013$sec_1_5) == 1 | as.numeric(gen2013$sec_1_5) == 33, NA, 2)))

#############sector en que curso primeros 5 anos de secundaria (tip_inst_sec)#############
gen2012$tip_inst_sec<-gen2012$secund_sector

gen2014$tip_inst_sec <- ifelse(gen2014$tip_inst_sec == "Sin dato", NA, gen2014$tip_inst_sec)
gen2014$tip_inst_sec <- as.factor(gen2014$tip_inst_sec)
levels(gen2014$tip_inst_sec) <- levels(gen2012$tip_inst_sec)

gen2016$tip_inst_sec <- ifelse(gen2016$tip_inst_sec == "Sin datos", NA, gen2016$tip_inst_sec)
gen2016$tip_inst_sec <- as.factor(gen2016$tip_inst_sec)
levels(gen2016$tip_inst_sec) <- levels(gen2012$tip_inst_sec)

#############region donde curso 6 de secundaria (sec_6)#############
gen2012$sec_6<-gen2012$sexto_region

gen2012$sec_6 <- ifelse(as.numeric(gen2012$sec_6) > 2 & as.numeric(gen2012$sec_6) < 21, 1,
                        ifelse(as.numeric(gen2012$sec_6) > 20, 3,
                               ifelse(as.numeric(gen2012$sec_6) == 1, NA, 2)))

## HAY UN PROBLEMA CON ESTA VARIABLE EN 2013: 363 (salto respecto a otros años) RESPONDEN NO HABER CURSADO SECUNDARIA (creo que porque hicieron UTU y en vez de tener NA como otros años se puso no cursó)

gen2013$sec_6 <- ifelse(as.numeric(gen2013$sec_6) > 2 & as.numeric(gen2013$sec_6) < 21, 1,
                        ifelse(as.numeric(gen2013$sec_6) > 20, 3,
                               ifelse(as.numeric(gen2013$sec_6) == 1, NA, 2)))

gen2014$sec_6 <- ifelse(as.numeric(gen2014$sec_6) > 1 & as.numeric(gen2014$sec_6) < 20, 1,
                        ifelse(as.numeric(gen2014$sec_6) > 19, 3,
                               ifelse(as.numeric(gen2014$sec_6) == 32, NA, 2)))

gen2015$sec_6 <- ifelse(as.numeric(gen2015$sec_6) > 2 & as.numeric(gen2015$sec_6) < 21, 1,
                        ifelse(as.numeric(gen2015$sec_6) > 20, 3,
                               ifelse(as.numeric(gen2013$sec_6) == 1, NA, 2)))

gen2016$sec_6 <- ifelse(as.numeric(gen2016$sec_6) > 2 & as.numeric(gen2016$sec_6) < 21, 1,
                        ifelse(as.numeric(gen2016$sec_6) > 20, 3, 
                               ifelse(as.numeric(gen2013$sec_6) == 1 | as.numeric(gen2013$sec_6) == 33, NA, 2)))

## Si hizo UTU

gen2012$utu <- ifelse(as.numeric(gen2012$utu) > 2 & as.numeric(gen2012$utu) < 21, 1,
                      ifelse(as.numeric(gen2012$utu) > 20, 3,
                             ifelse(as.numeric(gen2012$utu) == 1, NA, 2)))

gen2013$utu <- ifelse(as.numeric(gen2013$utu) > 2 & as.numeric(gen2013$utu) < 21, 1,
                      ifelse(as.numeric(gen2013$utu) > 20, 3,
                             ifelse(as.numeric(gen2013$utu) == 1, NA, 2)))

## Vuelvo a recodificar sec_6 incluyendo resultados de UTU

gen2012$sec_6 <- ifelse(is.na(gen2012$sec_6), gen2012$utu, gen2012$sec_6)
gen2013$sec_6 <- ifelse(is.na(gen2013$sec_6), gen2013$utu, gen2013$sec_6)

#############sector en que curso 6 de secundaria (tip_inst_sexto)#############
gen2012$tip_inst_sexto<-gen2012$sexto_sector

gen2012$tip_inst_sexto <- ifelse(gen2012$tip_inst_sexto == "No cursó", NA, gen2012$tip_inst_sexto)
gen2012$tip_inst_sexto <- as.factor(gen2012$tip_inst_sexto)
levels(gen2012$tip_inst_sexto) <- levels(gen2013$tip_inst_sexto)

gen2014$tip_inst_sexto <- ifelse(gen2014$tip_inst_sexto == "Sin dato", NA, gen2014$tip_inst_sexto)
gen2014$tip_inst_sexto <- as.factor(gen2014$tip_inst_sexto)
levels(gen2014$tip_inst_sexto) <- levels(gen2013$tip_inst_sexto)

gen2015$tip_inst_sexto <- ifelse(gen2015$tip_inst_sexto == "Sin dato", NA, gen2015$tip_inst_sexto)
gen2015$tip_inst_sexto <- as.factor(gen2015$tip_inst_sexto)
levels(gen2015$tip_inst_sexto) <- levels(gen2013$tip_inst_sexto)

gen2016$tip_inst_sexto <- ifelse(gen2016$tip_inst_sexto == "Sin datos" | gen2016$tip_inst_sexto == "No corresponde", NA, gen2016$tip_inst_sexto)
gen2016$tip_inst_sexto <- as.factor(gen2016$tip_inst_sexto)
levels(gen2016$tip_inst_sexto) <- levels(gen2013$tip_inst_sexto)

#############ano de egreso de educacion media (egre_sec)#############
gen2012$egre_sec<-gen2012$egre_sexto

##############################
###########################
##   EDUCACION DE LOS PADRES
####################

##############################
###########################
##   PADRE
####################

#Educacion del padre
gen2012$pad_edu_seciu<-as.character(gen2012$pad_edu_seciu)
gen2012$pad_edu_papel<-as.character(gen2012$pad_edu_papel)
gen2012$pad_edu_2014<-as.character(gen2012$pad_edu_2014)

gen2012$p_edu <- ifelse(is.na(gen2012$pad_edu_seciu) & is.na(gen2012$pad_edu_papel),gen2012$pad_edu_2014,
                        ifelse(is.na(gen2012$pad_edu_seciu) & !is.na(gen2012$pad_edu_papel),gen2012$pad_edu_papel,gen2012$pad_edu_seciu))

gen2013$p_edu <- as.character(gen2013$ed_padre)
gen2014$p_edu <- gen2014$ed_padre
gen2015$p_edu <- as.character(gen2015$ed_padre)
gen2016$p_edu <- as.character(gen2016$ed_padre)

gen2012$edu_p <- ifelse(gen2012$p_edu == "Primaria completa" | gen2012$p_edu == "Primaria Completa" |  gen2012$p_edu == "Primaria incompleta", 1,
                        ifelse(gen2012$p_edu == "Ens. Militar/ Policial completa" | gen2012$p_edu == "Ens. Militar/ Policial incompleta" | gen2012$p_edu ==                           "Enseñanza Militar/ Policial Completa" | gen2012$p_edu == "Enseñanza Militar/ Policial Incompleta" | 
                        gen2012$p_edu == "Educación Media completa" | gen2012$p_edu == "Educación Media incompleta" | gen2012$p_edu == "Secundaria completa" |
                        gen2012$p_edu == "Secundaria Completa" | gen2012$p_edu == "Secundaria incompleta" | gen2012$p_edu == "Secundaria Incompleta", 2, 
                               ifelse(gen2012$p_edu == "Educ Terciaria No Universitaria incompleto" | gen2012$p_edu == "Educ. Téc. (UTU) completa" | 
                               gen2012$p_edu == "Educ. Téc. (UTU) incompleta" | gen2012$p_edu == "Educación Técnica (UTU) Completa" | gen2012$p_edu ==                                       "Educación Técnica (UTU) Incompleta" | gen2012$p_edu == "IPA/ CERP/ Magisterio/ INET/ Ed Física Completo" | 
                               gen2012$p_edu == "IPA/ CERP/Magisterio/ INET/ Ed Física Incompleto" | 
                               gen2012$p_edu == "IPA/ CERP/Magisterio/ INET/ Ed. Física completa" | gen2012$p_edu == "Terciario no Universitario", 3,
                                      ifelse(gen2012$p_edu == "Universidad completa" | gen2012$p_edu == "Universidad Completa" | 
                                      gen2012$p_edu == "Universidad incompleta" | gen2012$p_edu == "Universidad Incompleto", 4,
                                             ifelse(gen2012$p_edu == "No sabe", NA, 0)))))

gen2013$edu_p <- ifelse(gen2013$p_edu == "Primaria completa" | gen2013$p_edu == "Primaria incompleta", 1,
                        ifelse(gen2013$p_edu == "Secundaria completa" | gen2013$p_edu == "Secundaria incompleta" | 
                        gen2013$p_edu == "Enseñanza militar/policial completa" | gen2013$p_edu == "Enseñanza militar/policial incompleta", 2,
                               ifelse(gen2013$p_edu == "Educ Terciaria No Universitaria completo" | 
                               gen2013$p_edu == "Educ Terciaria No Universitaria incompleto" | gen2013$p_edu == "Educación técnica (UTU) completa" | 
                               gen2013$p_edu == "Educación técnica (UTU) incompleta", 3,
                                      ifelse(gen2013$p_edu == "Universidad Completa" | gen2013$p_edu == "Universidad incompleta", 4, 
                                            ifelse(gen2013$p_edu == "Sin Dato", NA, 0)))))
                               
gen2014$edu_p <- ifelse(gen2014$p_edu == 2 | gen2014$p_edu == 3, 1,
                        ifelse(gen2014$p_edu == 4 | gen2014$p_edu == 5, 2,
                               ifelse(gen2014$p_edu == 10 | gen2014$p_edu == 11, 3,
                                      ifelse(gen2014$p_edu == 12 | gen2014$p_edu == 13, 4, 
                                             ifelse(gen2014$p_edu == 1, 0, NA)))))

gen2015$edu_p <- ifelse(gen2015$p_edu == "Primaria completa" | gen2015$p_edu == "Primaria incompleta", 1,
                        ifelse(gen2015$p_edu == "Secundaria completa" | gen2015$p_edu == "Secundaria incompleta", 2,
                               ifelse(gen2015$p_edu == "Educ Terciaria No Universitaria completo" | 
                                      gen2015$p_edu == "Educ Terciaria No Universitaria incompleto", 3,
                                      ifelse(gen2015$p_edu == "Universidad Completa" | gen2015$p_edu == "Universidad incompleta", 4,
                                             ifelse(gen2015$p_edu == "Sin Dato", NA, 0)))))

gen2016$edu_p <- ifelse(gen2016$p_edu == "Primaria completa" | gen2016$p_edu == "Primaria incompleta", 1,
                        ifelse(gen2016$p_edu == "Secundaria completa" | gen2016$p_edu == "Secundaria incompleta", 2,
                               ifelse(gen2016$p_edu == "Educ Terciaria No Universitaria completo" | 
                                      gen2016$p_edu == "Educ Terciaria No Universitaria incompleto", 3,
                                      ifelse(gen2016$p_edu == "Universidad Completa" | gen2016$p_edu == "Universidad incompleta", 4,
                                             ifelse(gen2016$p_edu == "Sin Dato", NA, 0)))))

table(gen2012$edu_p)
table(gen2013$edu_p)
table(gen2014$edu_p)
table(gen2015$edu_p)
table(gen2016$edu_p)

##############################
###########################
##   MADRE
####################

#Educacion de la madre
gen2012$mad_edu_seciu<-as.character(gen2012$mad_edu_seciu)
gen2012$mad_edu_mapel<-as.character(gen2012$mad_edu_papel)
gen2012$mad_edu_2014<-as.character(gen2012$mad_edu_2014)

gen2012$m_edu <- ifelse(is.na(gen2012$mad_edu_seciu) & is.na(gen2012$mad_edu_mapel),gen2012$mad_edu_2014,
                        ifelse(is.na(gen2012$mad_edu_seciu) & !is.na(gen2012$mad_edu_mapel),gen2012$mad_edu_mapel,gen2012$mad_edu_seciu))

gen2013$m_edu <- as.character(gen2013$ed_madre)
gen2014$m_edu <- gen2014$ed_madre
gen2015$m_edu <- as.character(gen2015$ed_madre)
gen2016$m_edu <- as.character(gen2016$ed_madre)

gen2012$edu_m <- ifelse(gen2012$m_edu == "Primaria completa" | gen2012$m_edu == "Primaria Completa" |  gen2012$m_edu == "Primaria incompleta", 1,
                        ifelse(gen2012$m_edu == "Ens. Militar/ Policial completa" | gen2012$m_edu == "Ens. Militar/ Policial incompleta" | gen2012$m_edu ==                           "Enseñanza Militar/ Policial Completa" | gen2012$m_edu == "Enseñanza Militar/ Policial Incompleta" | 
                               gen2012$m_edu == "Educación Media completa" | gen2012$m_edu == "Educación Media incompleta" | 
                               gen2012$m_edu == "Secundaria completa" | gen2012$m_edu == "Secundaria Completa" | gen2012$m_edu == "Secundaria incompleta" |                                  gen2012$m_edu == "Secundaria Incompleta", 2, 
                               ifelse(gen2012$m_edu == "Educ Terciaria No Universitaria incompleto" | gen2012$m_edu == "Educ. Téc. (UTU) completa" | 
                                      gen2012$m_edu == "Educ. Téc. (UTU) incompleta" | gen2012$m_edu == "Educación Técnica (UTU) Completa" | gen2012$m_edu==                                        "Educación Técnica (UTU) Incompleta" | gen2012$m_edu == "IPA/ CERP/ Magisterio/ INET/ Ed Física Completo" | 
                                      gen2012$m_edu == "IPA/ CERP/Magisterio/ INET/ Ed Física Incompleto" | 
                                      gen2012$m_edu == "IPA/ CERP/Magisterio/ INET/ Ed. Física completa" | gen2012$m_edu == "Terciario no Universitario", 3,
                                      ifelse(gen2012$m_edu == "Universidad completa" | gen2012$m_edu == "Universidad Completa" | 
                                             gen2012$m_edu == "Universidad incompleta" | gen2012$m_edu == "Universidad Incompleto", 4,
                                             ifelse(gen2012$m_edu == "No sabe", NA, 0)))))

gen2013$edu_m <- ifelse(gen2013$m_edu == "Primaria completa" | gen2013$m_edu == "Primaria incompleta", 1,
                        ifelse(gen2013$m_edu == "Secundaria completa" | gen2013$m_edu == "Secundaria incompleta" | 
                               gen2013$m_edu == "Enseñanza militar/policial completa" | gen2013$m_edu == "Enseñanza militar/policial incompleta", 2,
                               ifelse(gen2013$m_edu == "Educ Terciaria No Universitaria completo" | 
                                      gen2013$m_edu == "Educ Terciaria No Universitaria incompleto" | gen2013$m_edu == "Educación técnica (UTU) completa" | 
                                      gen2013$m_edu == "Educación técnica (UTU) incompleta", 3,
                                      ifelse(gen2013$m_edu == "Universidad Completa" | gen2013$m_edu == "Universidad incompleta", 4, 
                                             ifelse(gen2013$m_edu == "Sin Dato", NA, 0)))))

gen2014$edu_m <- ifelse(gen2014$m_edu == 2 | gen2014$m_edu == 3, 1,
                        ifelse(gen2014$m_edu == 4 | gen2014$m_edu == 5, 2,
                               ifelse(gen2014$m_edu == 10 | gen2014$m_edu == 11, 3,
                                      ifelse(gen2014$m_edu == 12 | gen2014$m_edu == 13, 4,
                                             ifelse(gen2014$p_edu == 1, 0, NA)))))

gen2015$edu_m <- ifelse(gen2015$m_edu == "Primaria completa" | gen2015$m_edu == "Primaria incompleta", 1,
                        ifelse(gen2015$m_edu == "Secundaria completa" | gen2015$m_edu == "Secundaria incompleta", 2,
                               ifelse(gen2015$m_edu == "Educ Terciaria No Universitaria completo" | 
                                      gen2015$m_edu == "Educ Terciaria No Universitaria incompleto", 3,
                                      ifelse(gen2015$m_edu == "Universidad Completa" | gen2015$m_edu == "Universidad incompleta", 4,
                                             ifelse(gen2015$m_edu == "Sin Dato", NA, 0)))))

gen2016$edu_m <- ifelse(gen2016$m_edu == "Primaria completa" | gen2016$m_edu == "Primaria incompleta", 1,
                        ifelse(gen2016$m_edu == "Secundaria completa" | gen2016$m_edu == "Secundaria incompleta", 2,
                               ifelse(gen2016$m_edu == "Educ Terciaria No Universitaria completo" | 
                                      gen2016$m_edu == "Educ Terciaria No Universitaria incompleto", 3,
                                      ifelse(gen2016$m_edu == "Universidad Completa" | gen2016$m_edu == "Universidad incompleta", 4,
                                             ifelse(gen2016$m_edu == "Sin Dato", NA, 0)))))

table(gen2012$edu_m)
table(gen2013$edu_m)
table(gen2014$edu_m)
table(gen2015$edu_m)
table(gen2016$edu_m)

#############horas promedio de trabajo semanal (hora_tra)#############
gen2012$hora_tra <- gen2012$hor_trab
gen2015$hora_tra <- ifelse(gen2015$hora_tra == "No corresponde", NA, gen2015$hora_tra)
gen2015$hora_tra <- as.factor(gen2015$hora_tra)
levels(gen2015$hora_tra) <- levels(gen2012$hora_tra)
gen2016$hora_tra <- ifelse(gen2016$hora_tra == "No corresponde", NA, gen2016$hora_tra)
gen2016$hora_tra <- as.factor(gen2016$hora_tra)
levels(gen2016$hora_tra) <- levels(gen2012$hora_tra)


#############anio en que comenzo su actividad laboral (inic_tra)#############
gen2012$inic_tra<-gen2012$an_com
gen2015$inic_tra<-ifelse(is.na(gen2015$inic_tra) | gen2015$inic_tra == 0, NA, gen2015$inic_tra)
gen2016$inic_tra<-ifelse(is.na(gen2016$inic_tra) | gen2016$inic_tra == 0, NA, gen2016$inic_tra)


#############relacion de la ocupacion con la carrera (rel_trab)#############

gen2012$rel_trab<-ifelse(gen2012$rel_trab=="Está bastante relacionada" | gen2012$rel_trab=="Está muy relacionada", 1,
                         ifelse(gen2012$rel_trab=="Está relacionada" | gen2012$rel_trab=="Está poco relacionada", 2, 3))

gen2013$rel_trab<-ifelse(gen2013$rel_trab=="Está muy relacionada", 1,
                         ifelse(gen2013$rel_trab=="Está relacionada" | gen2013$rel_trab=="Está poco relacionada" | 
                                gen2013$rel_trab=="Está parcialmente relacionada", 2,
                                ifelse(gen2013$rel_trab=="No hay dato", NA, 3)))

gen2014$rel_trab<-ifelse(gen2014$rel_trab=="Está bastante relacionada" | gen2014$rel_trab=="Está muy relacionada", 1,
                         ifelse(gen2014$rel_trab=="Está relacionada" | gen2014$rel_trab=="Está poco relacionada", 2, 3))

gen2015$rel_trab<-ifelse(gen2015$rel_trab=="Está muy relacionada", 1,
                         ifelse(gen2015$rel_trab=="Está relacionada" | gen2015$rel_trab=="Está poco relacionada" | 
                                  gen2015$rel_trab=="Está parcialmente relacionada", 2,
                                ifelse(gen2015$rel_trab=="No corresponde", NA, 3)))

gen2016$rel_trab<-ifelse(gen2016$rel_trab=="Está muy relacionada", 1,
                         ifelse(gen2016$rel_trab=="Está relacionada" | gen2016$rel_trab=="Está poco relacionada" | 
                                  gen2016$rel_trab=="Está parcialmente relacionada", 2,
                                ifelse(gen2016$rel_trab=="No corresponde" | gen2016$rel_trab=="No hay dato", NA, 3)))

#HASTA ACA ESTA CHEQUEADO##

#HAY QUE ARREGLAR LAS CATEGORIAS OCUPACIONALES

#############ocupacion del estudiante (ocup_est)#############
levels(gen2012$cat_oc_est)
levels(gen2013$cat_oc_est)
levels(gen2014$cat_oc_est)
levels(gen2015$cat_oc_est)
levels(gen2016$cat_oc_est)

gen2012$catoc_est <- as.numeric(gen2012$cat_oc_est)
gen2012$catoc_est <- ifelse(is.na(gen2012$catoc_est), NA, gen2012$catoc_est)

gen2013$catoc_est <- as.numeric(gen2013$cat_oc_est)
gen2013$catoc_est <- ifelse(is.na(gen2013$catoc_est), NA, gen2013$catoc_est)

gen2014$catoc_est <- as.numeric(gen2014$cat_oc_est)
gen2014$catoc_est <- ifelse(is.na(gen2014$catoc_est), NA, gen2014$catoc_est)

gen2015$catoc_est <- as.numeric(gen2015$cat_oc_est)
gen2015$catoc_est <- ifelse((gen2015$catoc_est==1 | gen2015$catoc_est==9), NA, 
                            ifelse(gen2015$catoc_est==2, 1, 
                                   ifelse(gen2015$catoc_est==3, 2,
                                          ifelse(gen2015$catoc_est==4, 3,
                                                 ifelse(gen2015$catoc_est==5, 4,
                                                        ifelse(gen2015$catoc_est==6, 5,
                                                               ifelse(gen2015$catoc_est==7, 6,7)))))))

gen2016$catoc_est <- as.numeric(gen2016$cat_oc_est)
gen2016$catoc_est <- ifelse((gen2016$catoc_est==1 | gen2016$catoc_est==9), NA, 
                            ifelse(gen2016$catoc_est==2, 1, 
                                   ifelse(gen2016$catoc_est==3, 2,
                                          ifelse(gen2016$catoc_est==4, 3,
                                                 ifelse(gen2016$catoc_est==5, 4,
                                                        ifelse(gen2016$catoc_est==6, 5,
                                                               ifelse(gen2016$catoc_est==7, 6,7)))))))

############# ocupacion del estudiante (ocup_est)#############

levels(gen2012$enc_trab)
levels(gen2013$ocup_est)
levels(gen2014$ocup_est)
levels(gen2015$ocup_est)
levels(gen2016$ocup_est)

gen2012$enc_trab2 <- as.numeric(gen2012$enc_trab)
gen2013$ocup_est2  <- as.numeric(gen2013$ocup_est)
gen2014$ocup_est2  <- as.numeric(gen2014$ocup_est)
gen2015$ocup_est2  <- as.numeric(gen2015$ocup_est)
gen2016$ocup_est2  <- as.numeric(gen2016$ocup_est)

## SUMO EMPLEADOS/VENDEDORES A CALIFICADOS
gen2012$ocu_est <- ifelse((gen2012$enc_trab2==2 | gen2012$enc_trab2==3 | gen2012$enc_trab2==6 | gen2012$enc_trab2==7 | gen2012$enc_trab2==10 | (gen2012$enc_trab2> 12 & gen2012$enc_trab2<17) | gen2012$enc_trab2==19 | gen2012$enc_trab2==21), 1, 
                          ifelse((gen2012$enc_trab2==4 | gen2012$enc_trab2==11 | gen2012$enc_trab2==12 | gen2012$enc_trab2==17 | gen2012$enc_trab2==18), 2,
                                 ifelse(gen2012$enc_trab2==1, NA,3)))

## EN ESTE AÑO HAY UN PROBLEMA CON LA CATEGORÍA 8 "SOCIOS DE COMERCIOS": TA REPETIDA Y SALTA EN FRECUENCIA. HAY QUE VER QUÉ ES
gen2013$ocu_est <- ifelse((gen2013$ocup_est2==15 | (gen2013$ocup_est2>17 & gen2013$ocup_est2<24) | gen2013$ocup_est2==25 | gen2013$ocup_est2==28 | gen2013$ocup_est2==27 | (gen2013$ocup_est2> 8 & gen2013$ocup_est2<13)), 1, 
                          ifelse((gen2013$ocup_est2==7 | gen2013$ocup_est2==13 | gen2013$ocup_est2==14 | gen2013$ocup_est2==25 | gen2013$ocup_est2==26), 2, 
                                 ifelse(gen2013$ocup_est2==24, NA,3)))

gen2014$ocu_est <- ifelse((gen2014$ocup_est2==6 | (gen2014$ocup_est2>0 & gen2014$ocup_est2<5)), 1, 
                          ifelse((gen2014$ocup_est2==5 | gen2014$ocup_est2==7 | gen2014$ocup_est2==8), 2, 3))

gen2015$ocu_est <- ifelse((gen2015$ocup_est2==6 | (gen2015$ocup_est2>0 & gen2015$ocup_est2<5)), 1, 
                          ifelse((gen2015$ocup_est2==5 | gen2015$ocup_est2==7 | gen2015$ocup_est2==8), 2,
                                 ifelse(gen2015$ocup_est2==10, NA, 3)))

gen2016$ocu_est <- ifelse((gen2016$ocup_est2==7 | (gen2016$ocup_est2>1 & gen2016$ocup_est2<6)), 1, 
                          ifelse((gen2016$ocup_est2==6 | gen2016$ocup_est2==8 | gen2016$ocup_est2==9), 2,
                                 ifelse((gen2016$ocup_est2==11 | gen2016$ocup_est2==1), NA, 3)))

gen2012$enc_trab2 <- NULL
gen2013$ocup_est2  <- NULL
gen2014$ocup_est2  <- NULL
gen2015$ocup_est2  <- NULL
gen2016$ocup_est2  <- NULL

table(gen2012$ocu_est)
table(gen2013$ocu_est)
table(gen2014$ocu_est)
table(gen2015$ocu_est)
table(gen2016$ocu_est)

###### OCUPACIÓN DEL PADRE ########### 

levels(gen2012$pad_trab)
levels(gen2013$ocup_pad)
levels(gen2014$ocup_pad)
levels(gen2015$ocup_pad)
levels(gen2016$ocup_pad)

gen2012$pad_trab2 <- as.numeric(gen2012$pad_trab)
gen2013$ocup_pad2  <- as.numeric(gen2013$ocup_pad)
gen2014$ocup_pad2  <- as.numeric(gen2014$ocup_pad)
gen2015$ocup_pad2  <- as.numeric(gen2015$ocup_pad)
gen2016$ocup_pad2  <- as.numeric(gen2016$ocup_pad)

## SUMO EMPLEADOS/VENDEDORES A CALIFICADOS
gen2012$ocu_pad <- ifelse((gen2012$pad_trab2==2 | gen2012$pad_trab2==3 | gen2012$pad_trab2==6 | gen2012$pad_trab2==7 | gen2012$pad_trab2==10 | (gen2012$pad_trab2> 12 & gen2012$pad_trab2<17) | gen2012$pad_trab2==19 | gen2012$pad_trab2==21), 1, 
                          ifelse((gen2012$pad_trab2==4 | gen2012$pad_trab2==11 | gen2012$pad_trab2==12 | gen2012$pad_trab2==17 | gen2012$pad_trab2==18), 2,
                                 ifelse(gen2012$pad_trab2==1, NA,3)))

## EN ESTE AÑO HAY UN PROBLEMA CON LA CATEGORÍA 8 "SOCIOS DE COMERCIOS": TA REPETIDA Y SALTA EN FRECUENCIA. HAY QUE VER QUÉ ES
gen2013$ocu_pad <- ifelse((gen2013$ocup_pad2==15 | (gen2013$ocup_pad2>17 & gen2013$ocup_pad2<24) | gen2013$ocup_pad2==25 | gen2013$ocup_pad2==28 | gen2013$ocup_pad2==27 | (gen2013$ocup_pad2> 8 & gen2013$ocup_pad2<13)), 1, 
                          ifelse((gen2013$ocup_pad2==7 | gen2013$ocup_pad2==13 | gen2013$ocup_pad2==14 | gen2013$ocup_pad2==25 | gen2013$ocup_pad2==26), 2, 
                                 ifelse(gen2013$ocup_pad2==24, NA,3)))

gen2014$ocu_pad <- ifelse((gen2014$ocup_pad2==6 | (gen2014$ocup_pad2>0 & gen2014$ocup_pad2<5)), 1, 
                          ifelse((gen2014$ocup_pad2==5 | gen2014$ocup_pad2==7 | gen2014$ocup_pad2==8), 2, 3))

gen2015$ocu_pad <- ifelse((gen2015$ocup_pad2==6 | (gen2015$ocup_pad2>0 & gen2015$ocup_pad2<5)), 1, 
                          ifelse((gen2015$ocup_pad2==5 | gen2015$ocup_pad2==7 | gen2015$ocup_pad2==8), 2,
                                 ifelse(gen2015$ocup_pad2==10, NA, 3)))

gen2016$ocu_pad <- ifelse((gen2016$ocup_pad2==7 | (gen2016$ocup_pad2>1 & gen2016$ocup_pad2<6)), 1, 
                          ifelse((gen2016$ocup_pad2==6 | gen2016$ocup_pad2==8 | gen2016$ocup_pad2==9), 2,
                                 ifelse((gen2016$ocup_pad2==11 | gen2016$ocup_pad2==1), NA, 3)))

gen2012$pad_trab2 <- NULL
gen2013$ocup_pad2  <- NULL
gen2014$ocup_pad2  <- NULL
gen2015$ocup_pad2  <- NULL
gen2016$ocup_pad2  <- NULL

table(gen2012$ocu_pad)
table(gen2013$ocu_pad)
table(gen2014$ocu_pad)
table(gen2015$ocu_pad)
table(gen2016$ocu_pad)

####### OCUPACIÓN DE LA MADRE ###########

levels(gen2012$mad_trab)
levels(gen2013$ocup_mad)
levels(gen2014$ocup_mad)
levels(gen2015$ocup_mad)
levels(gen2016$ocup_mad)

gen2012$mad_trab2 <- as.numeric(gen2012$mad_trab)
gen2013$ocup_mad2  <- as.numeric(gen2013$ocup_mad)
gen2014$ocup_mad2  <- as.numeric(gen2014$ocup_mad)
gen2015$ocup_mad2  <- as.numeric(gen2015$ocup_mad)
gen2016$ocup_mad2  <- as.numeric(gen2016$ocup_mad)

## SUMO EMPLEADOS/VENDEDORES A CALIFICADOS
gen2012$ocu_mad <- ifelse((gen2012$mad_trab2==2 | gen2012$mad_trab2==3 | gen2012$mad_trab2==6 | gen2012$mad_trab2==7 | gen2012$mad_trab2==10 | (gen2012$mad_trab2> 12 & gen2012$mad_trab2<17) | gen2012$mad_trab2==19 | gen2012$mad_trab2==21), 1, 
                          ifelse((gen2012$mad_trab2==4 | gen2012$mad_trab2==11 | gen2012$mad_trab2==12 | gen2012$mad_trab2==17 | gen2012$mad_trab2==18), 2,
                                 ifelse(gen2012$mad_trab2==1, NA,3)))

## EN ESTE AÑO HAY UN PROBLEMA CON LA CATEGORÍA 8 "SOCIOS DE COMERCIOS": TA REPETIDA Y SALTA EN FRECUENCIA. HAY QUE VER QUÉ ES
gen2013$ocu_mad <- ifelse((gen2013$ocup_mad2==15 | (gen2013$ocup_mad2>17 & gen2013$ocup_mad2<24) | gen2013$ocup_mad2==25 | gen2013$ocup_mad2==28 | gen2013$ocup_mad2==27 | (gen2013$ocup_mad2> 8 & gen2013$ocup_mad2<13)), 1, 
                          ifelse((gen2013$ocup_mad2==7 | gen2013$ocup_mad2==13 | gen2013$ocup_mad2==14 | gen2013$ocup_mad2==25 | gen2013$ocup_mad2==26), 2, 
                                 ifelse(gen2013$ocup_mad2==24, NA,3)))

gen2014$ocu_mad <- ifelse((gen2014$ocup_mad2==6 | (gen2014$ocup_mad2>0 & gen2014$ocup_mad2<5)), 1, 
                          ifelse((gen2014$ocup_mad2==5 | gen2014$ocup_mad2==7 | gen2014$ocup_mad2==8), 2, 3))

gen2015$ocu_mad <- ifelse((gen2015$ocup_mad2==6 | (gen2015$ocup_mad2>0 & gen2015$ocup_mad2<5)), 1, 
                          ifelse((gen2015$ocup_mad2==5 | gen2015$ocup_mad2==7 | gen2015$ocup_mad2==8), 2,
                                 ifelse(gen2015$ocup_mad2==10, NA, 3)))

gen2016$ocu_mad <- ifelse((gen2016$ocup_mad2==7 | (gen2016$ocup_mad2>1 & gen2016$ocup_mad2<6)), 1, 
                          ifelse((gen2016$ocup_mad2==6 | gen2016$ocup_mad2==8 | gen2016$ocup_mad2==9), 2,
                                 ifelse((gen2016$ocup_mad2==11 | gen2016$ocup_mad2==1), NA, 3)))

gen2012$mad_trab2 <- NULL
gen2013$ocup_mad2  <- NULL
gen2014$ocup_mad2  <- NULL
gen2015$ocup_mad2  <- NULL
gen2016$ocup_mad2  <- NULL

table(gen2012$ocu_mad)
table(gen2013$ocu_mad)
table(gen2014$ocu_mad)
table(gen2015$ocu_mad)
table(gen2016$ocu_mad)

#############inscribio a algun programa de becas para iniciar sus estudios (beca)#############
gen2012$beca<-gen2012$becas
levels(gen2016$beca) <- levels(gen2012$beca)
#############solicito la beca del fondo de solidaridad (beca_fondo)#############
gen2012$beca_fondo<-gen2012$aplica_fs
levels(gen2016$beca_fondo) <- levels(gen2012$beca)

#############solicito la beca de bienestar universitario (beca_scbu)#############
gen2012$beca_scbu<-gen2012$aplica_bu
levels(gen2016$beca_scbu) <- gen2012$beca_scbu

#############solicito otras becas (beca_otra)#############
gen2012$beca_otra<-gen2012$aplica_otro
levels(gen2016$beca_otra) <- levels(gen2012$beca)

#CI
gen2012$CI<-gen2012$C_I


# a2012<-c( "ESTCI", "T", "MAT", "NOMMAT", "FECHA", "PER", "NOTAMATERIA", "CRCURR", "CARR", "CICLO", "NOMCAR", "LUGARINSC", "NOMBRE", "CELULAR", "ANIOFINSEC", "INST", "NOMINST", "TIPOINST", "LUGARINST", "FECEG", "FECHAING", "OBS", "FECHAEG", "barrio", "fecha_nac", "residen", "mujer", "lug_nac", "res_marzo", "tip_viv", "hij_num", "viv_solo", "hij_num", "viv_solo", "ocup", "ocup_ing",  "est_cony", "Cony_ing", "padres", "conyug", "cony_ing", "ocup_ing", "Cony_pad_ing", "hijo_viv", "Hijo_ing", "Herm_ing", "Abu_ing", "suegro", "Sueg_ing", "fam_otro",  "est_otro", "Est_ing", "per_otro", "Per_ing", "edu_prim", "tip_inst_pri", "sec_1_5", "tip_inst_sec", "sec_6", "tip_inst_sexto", "egre_sec", "ed_padre", "ed_madre", "hora_tra", "rel_trab", "cat_oc_est", "ocup_est", "ocup_pad", "cat_oc_pad", "ocup_mad", "cat_oc_mad", "beca", "beca_fondo", "beca_scbu", "beca_otra")

# var<-c( "CI", "fecha_nac", "barrio",  "mujer", "lug_nac", "res_marzo", "tip_viv", "hij_num", "viv_solo", "ocup", "ocup_ing", "est_cony", "padres", "Pad_ing", "conyug", "cony_ing", "cony_pad", "Cony_pad_ing", "hijo_viv", "Hijo_ing", "herman", "Herm_ing", "abuelo", "Abu_ing", "suegro", "Sueg_ing", "fam_otro", "Fam_ing", "est_otro", "Est_ing", "per_otro", "Per_ing", "edu_prim", "abuelo", "Abu_ing", "suegro", "Sueg_ing", "fam_otro", "Fam_ing", "est_otro", "Est_ing", "per_otro", "Per_ing", "edu_prim", "tip_inst_pri", "sec_1_5", "tip_inst_sec", "sec_6", "tip_inst_sexto", "egre_sec", "ed_padre", "ed_madre", "hora_tra", "inic_tra", "rel_trab", "cat_oc_est", "ocup_est", "cat_oc_pad", "ocup_pad", "cat_oc_mad", "ocup_mad", "beca", "beca_fondo", "beca_scbu", "beca_otra")

var<-c( "CI", "fecha_nac",  "mujer", "lug_nac", "res_marzo", "tip_viv", "hij_num", "viv_solo", "ocup", "ocup_ing", "est_cony", "padres", "pad_ing", "conyug", "cony_ing", "cony_pad", "cony_pad_ing", "hijo_viv", "hijo_ing", "herman", "herm_ing", "abuelo", "abu_ing", "suegro", "sueg_ing", "fam_otro", "fam_ing", "est_otro", "est_ing", "per_otro", "per_ing", "edu_prim", "tip_inst_pri", "sec_1_5", "tip_inst_sec", "sec_6", "tip_inst_sexto", "egre_sec", "edu_p", "edu_m", "hora_tra", "inic_tra", "rel_trab", "ocu_est", "ocu_pad", "ocu_mad", "beca", "beca_fondo", "beca_scbu", "beca_otra")

base2012<-subset(gen2012, select=var)
base2013<-subset(gen2013, select=var)
base2014<-subset(gen2014, select=var)
base2015<-subset(gen2015, select=var)
base2016<-subset(gen2016, select=var)

#Pegar las bases
baseconform<- base2012
baseconform<- rbind(baseconform, base2013)
baseconform<- rbind(baseconform, base2014)
baseconform<- rbind(baseconform, base2015)
baseconform<- rbind(baseconform, base2016)

#Para saber cuales son las que no tienen en comun uno y el otro 
#
#var[!var %in% names(gen2012)]

###Limpio y exporto la base de datos

rm(list=c("base2012", "base2013", "base2014", "base2015", "base2016", "gen2012", "gen2013", "gen2014", "gen2015", "gen2016"))

setwd("C:/Users/sburone/Documents/BASE NPE FINAL/Bases/auxiliares")

write.csv(baseconform, "base_formulario_montevideo.csv")
write.dta(baseconform, "base_formulario_montevideo.dta")




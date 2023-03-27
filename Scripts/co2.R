rm(list=ls()
## C:\Program Files\R\R-4.1.0\bin\x64
   options(digits=6)

#install.packages("readxl")
library(readxl)
library(data.table)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)
library(purrr)
##############################################################################
##############################################################################
endereco_arquivo<-
#flor2<-read.csv ("/home/roberval/Documentos/Lotofacil/180a200R.csv", header=TRUE, sep="\t") # linux
                                        #flor2<-read.xlsx ("/home/roberval/Documentos/Lotofacil/origin1.xlsx", sheet=3) # linux
#endereco_arquivo<- "C:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\dados\\boca_jun_2021_flor2anel_todos.xlsx"
getwd()

list(ls())
rm("flor1")

setwd("C:\\Users\\rober\\OneDrive\\Disco-Universal\\Minhas-publicacoes-2021-2022\\Serie-Doc-RAD-taxi\\R-analise-co2-Boca\\dados")
       
getwd()

flor2 <- read.xlsx("flor2anel_todos.xlsx", sheet=1)
flor1 <- read.xlsx("flor_anel_todos.xlsx", sheet=1)

##################
#Uso do split
base_list<-split(baseinv, baseinv$anel)
tail(base_list)
co2_list<-split(baseinv,list(baseinv$anel,baseinv$rep))
co2_list[1:4] #anel 1,2,3,4 com rep1
co2_list[1:11] #anel 1,2,3 (3 reps) e anel 4(2 reps)

str(co2_list)

map(co2_list,~lm(inside_CO2_ratio~time, data=.x))
map(base_list,~lm(inside_CO2_ratio~time, data=.x)) #deucerto anel geral

map(co2_list[1:1],~lm(inside_CO2_ratio~time, data=.x)) #deucerto anel1 Rep1
map(co2_list[1:11],~lm(inside_CO2_ratio~time, data=.x)) #deucerto anel1,2,3 
co2_out<-map(co2_list[1:11],~lm(inside_CO2_ratio~time, data=.x)) #deucerto anel1,2,3 


co2_df[1] #anel1 rep1
co2_df[2] #anel2 rep1

#REP 1 anel 1,2,3
co2_df[[1]][[2]] #beta 1 de anel rep1
co2_df[[2]][[2]] #beta 1 de ane2 rep1
co2_df[[3]][[2]] #beta 1 de ane3 rep1
co2_df[[4]][[2]] #beta 1 de ane4 rep1

#REP 2 anel 1,2,3
co2_df[[5]][[2]] #beta 1 de anel rep2
co2_df[[6]][[2]] #beta 1 de ane2 rep2
co2_df[[7]][[2]] #beta 1 de ane3 rep2
co2_df[[8]][[2]] #beta 1 de ane4 rep2

#REP 3 anel 1,2,3
co2_df[[9]][[2]] #beta 1 de anel rep3
co2_df[[10]][[2]] #beta 1 de ane2 rep3
co2_df[[11]][[2]] #beta 1 de ane3 rep3
###############################################
#media
map(co2_list[1:11],~mean(inside_CO2_ratio, data=.x)) #deucerto anel1,2,3 

# media de outside_temperature e inside_pressure
base2<-select(
  group_by(flor2, anel, rep),
  outside_temperature, inside_pressure)
attach(base2)
head(base2)
edit(base2)
base2_list<-split(base2,list(base2$anel,base2$rep))
lapply(base2_list[1:3], function(x){meanx=mean(outside_temperature)})
lapply(base2_list, function(x){meanx=mean(outside_temperature)})
temp_out<-lapply(base2_list, function(x){meanx=mean(x$outside_temperature)})
pres_out<-lapply(base2_list, function(x){meanx=mean(x$inside_pressure)})

temp_df<-do.call(rbind,temp_out)
pres_df<-do.call(rbind,pres_out)

#temperature_out REP 1 anel 1,2,3,4
temp_df[[1]] # media de anel1 rep1
temp_df[[2]] # media de anel2 rep1
temp_df[[3]] # media de anel3 rep1
temp_df[[4]] # media de anel4 rep1

#temperature_out REP 2 anel 1,2,3,4
temp_df[[5]] # media de anel1 rep2
temp_df[[6]] # media de anel2 rep2
temp_df[[7]] # media de anel3 rep2
temp_df[[8]] # media de anel4 rep2

#temperature_out REP 3 anel 1,2,3,4
temp_df[[9]] # media de anel1 rep3
temp_df[[10]] # media de anel2 rep3
temp_df[[11]] # media de anel3 rep3

#pressure_out REP 1 anel 1,2,3,4
pres_df[[1]] # media de anel1 rep1
pres_df[[2]] # media de anel2 rep1
pres_df[[3]] # media de anel3 rep1
pres_df[[4]] # media de anel4 rep1

#pressure_out REP 2 anel 1,2,3,4
pres_df[[5]] # media de anel1 rep2
pres_df[[6]] # media de anel2 rep2
pres_df[[7]] # media de anel3 rep2
pres_df[[8]] # media de anel4 rep2

#pressureout REP 3 anel 1,2,3,4
pres_df[[9]] # media de anel1 rep3
pres_df[[10]] # media de anel2 rep3
pres_df[[11]] # media de anel3 rep3

##Funcoes
f<-function(x){
    ajuste<-lm(inside_CO2_ratio~time, data=x)
    a<-summary(ajuste)$coeff[2] # Beta 1
    b<-summary(ajuste)$adj.r.squared
    c<-summary(ajuste)$sigma #QMR
    print(a) # Beta1
   # print(b) # r.squared
   # print(c) # sigma
}
########################################
baseinv<-select(
  group_by(flor2, anel, rep),
  inside_CO2_ratio, time)
edit(baseinv)
attach(baseinv)
names(baseinv)
#########################################

volume <- 0
f2<-function(x,y,z){
    a<- x*60
    b<- y*100*12
    c<- z+273.15
    #microg_Co2 <- (((a)/1000000)*(0.015955+volume)*b)/(8.314*(c))*(1/0.0415475628437)
    microg_Co2 <- (((a)/1000000)*(0.01587+volume)*b)/(8.314*(c))*(1/0.0491)
    milig_Co2<- microg_Co2*1000
    micromol_Co2<- milig_Co2/12/60/60*1000
   # print(microg_Co2.flor2a1r1)
   # print(milig_Co2.flor2a1r1)
    print( micromol_Co2)
}

##Exemplo
##CO2 Floresta 2 Anel, 1 Repeticao 1 (LINHA 1),rep2 (LINHA 2), rep3 (LINHA 3)
co2f2a1r1<-f2(betaflor2a1r1,flor2anel.media[[1,4]],flor2anel.media[[1,3]] )


######################################FLOR 2 ANEL TODOS ##############################
##flor 2 select por ANEL
flor2anel1<-flor2 %>% filter(anel=="anel1")
flor2anel2<-flor2 %>% filter(anel=="anel2")
flor2anel3<-flor2 %>% filter(anel=="anel3")
flor2anel4<-flor2 %>% filter(anel=="anel4")

##Flor 2 Anel 1 select por REP
flor2anel1_r1<-flor2 %>% filter(anel=="anel1", rep=="rep1")
flor2anel1_r2<-flor2 %>% filter (anel=="anel1", rep=="rep2")
flor2anel1_r3<-flor2 %>% filter (anel=="anel1", rep=="rep3")


##Flor 2 Anel 2 select por REP
flor2anel2_r1<- flor2 %>% filter(anel=="anel2", rep=="rep1")
flor2anel2_r2<- flor2 %>% filter(anel=="anel2", rep=="rep2")
flor2anel2_r3<- flor2 %>% filter(anel=="anel2", rep=="rep3")

##Flor 2 Anel 3 select por REP
flor2anel3_r1<- flor2 %>% filter(anel=="anel3", rep=="rep1")
flor2anel3_r2<- flor2 %>% filter(anel=="anel3", rep=="rep2")
flor2anel3_r3<- flor2 %>% filter(anel=="anel3", rep=="rep3")

##Flor 2 Anel 4 select por REP
flor2anel4_r1<- flor2 %>% filter(anel=="anel4", rep=="rep1")
flor2anel4_r2<- flor2 %>% filter(anel=="anel4", rep=="rep2")
#flor2anel4_r3<- flor2 %>% filter(anel=="anel4", rep=="rep3")


## Médias de temp e pressao
flor2anel.media<- summarise(
        select(
        group_by(flor2, anel, rep),
        outside_temperature, inside_pressure, inside_CO2_ratio, soil_moisture, outside_humidity
    ),
    outside_temperature = mean(outside_temperature,na.rm=TRUE),
    inside_pressure = mean(inside_pressure, na.rm=TRUE),
    inside_CO2_ratio = mean(inside_CO2_ratio,na.rm=TRUE),
    soil_moisture = mean(soil_moisture, na.rm=TRUE),
    outside_humidity = mean(outside_humidity, na.rm=TRUE)

)

flor2anel.media
#edit(flor2anel.media)
ls(flor2anel.media)
flor2anel.media[[1,1]]
flor2anel.media[[1,2]]

##OUTSIDE TEMPERATURE
flor2anel.media[[1,3]] #anel1/rep1/outside_temperature
flor2anel.media[[2,3]] #anel1/rep2/outside_temperature
flor2anel.media[[3,3]] #anel1/rep3/outside_temperature

flor2anel.media[[4,3]] #anel2/rep1/outside_temperature
flor2anel.media[[5,3]] #anel2/rep2/outside_temperature
flor2anel.media[[6,3]] #anel2/rep3/outside_temperature

flor2anel.media[[7,3]] #anel3/rep1/outside_temperature
flor2anel.media[[8,3]] #anel3/rep2/outside_temperature
flor2anel.media[[9,3]] #anel3/rep3/outside_temperature

flor2anel.media[[10,3]] #anel4/rep1/outside_temperature
flor2anel.media[[11,3]] #anel4/rep2/outside_temperature

##INSIDE PRESSURE
flor2anel.media[[1,4]] #anel1/rep1/inside_pressure
flor2anel.media[[2,4]] #anel1/rep2/inside_pressure
flor2anel.media[[3,4]] #anel1/rep3/inside_pressure

flor2anel.media[[4,4]] #anel2/rep1/inside_pressure
flor2anel.media[[5,4]] #anel2/rep2/inside_pressure
flor2anel.media[[6,4]] #anel2/rep3/inside_pressure

flor2anel.media[[7,4]] #anel3/rep1/inside_pressure
flor2anel.media[[8,4]] #anel3/rep2/inside_pressure
flor2anel.media[[9,4]] #anel3/rep3/inside_pressure

flor2anel.media[[10,4]] #anel4/rep1/inside_pressure
flor2anel.media[[11,4]] #anel4/rep2/inside_pressure

write.xlsx(flor2anel.media,
          file="c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\flor2anel.media2.xlsx")


## Regressao


## Calculo dos Betas FLORESTA 2 EL 1, 2 E 3
##Floresta 2, Anel 1, Rep 1, rep2 e rep3
betaflor2a1r1<-f(flor2anel1_r1)
betaflor2a1r2<-f(flor2anel1_r2)
betaflor2a1r3<-f(flor2anel1_r3)

##Floresta 2, Anel 2, Rep 1, Rep2 e Rep3
betaflor2a2r1 <- f(flor2anel2_r1)
betaflor2a2r2 <- f(flor2anel2_r2)
betaflor2a2r3 <-  f(flor2anel2_r3)

##Floresta 2, Anel 3, Rep 1, Rep2 e Rep3
betaflor2a3r1<- f(flor2anel3_r1)
betaflor2a3r2<- f(flor2anel3_r2)
betaflor2a3r3<- f(flor2anel3_r3)

##Floresta 2, Anel 4, Rep 1, Rep2 e Rep3
betaflor2a4r1 <- f(flor2anel4_r1)
betaflor2a4r2 <- f(flor2anel4_r2)

##############################


## Calculo do CO2 Floresta 2 Anel 1
volume<-0
#microg_Co2.flor2a1r1 <- (((betaflor2a1r1*60)/1000000)*(0.015955+volume)*flor2anel.media[[1,4]]*100*12)/(8.314*(flor2anel.media[[1,3]]
#    +273.15))*(1/0.0415475628437)  # fiz uma correção em vez de 1/0.0415 coloquei 0.0415
microg_Co2.flor2a1r1 <- (((betaflor2a1r1*60)/1000000)*(0.01587+volume)*flor2anel.media[[1,4]]*100*12)/(8.314*(flor2anel.media[[1,3]]
                                                                                                               +273.15))*(0.0491)


microg_Co2.flor2a1r1
[1] 0.1072097


##CO2 Floresta 2 Anel, 1 Repeticao 1 (LINHA 1),rep2 (LINHA 2), rep3 (LINHA 3)
co2f2a1r1<-f2(betaflor2a1r1,flor2anel.media[[1,4]],flor2anel.media[[1,3]] )
##CO2 Floresta 2 Anel 1 Repeticao 2
co2f2a1r2<-f2(betaflor2a1r2,flor2anel.media[[2,4]],flor2anel.media[[2,3]] )
##CO2 Floresta 2 Anel 1 Repeticao 3
co2f2a1r3<-f2(betaflor2a1r2,flor2anel.media[[3,4]],flor2anel.media[[3,3]] )

##CO2 Floresta 2 Anel 2, repeticao 1(LINHA 4),rep2(LINHA 5), rep3 (LINHA 6)
co2f2a2r1<-f2(betaflor2a2r1,flor2anel.media[[4,4]],flor2anel.media[[4,3]] ) # REP 1 DEU MUITO ALTO, EXCLUIR
##CO2 Floresta 2 Anel 2 Repeticao 2
co2f2a2r2<-f2(betaflor2a2r2,flor2anel.media[[5,4]],flor2anel.media[[5,3]] )
##CO2 Floresta 2 Anel 2 Repeticao 3
co2f2a2r3<-f2(betaflor2a2r2,flor2anel.media[[6,4]],flor2anel.media[[6,3]] )

##CO2 Floresta 2 Anel 3, repeticao 1(LINHA 5),rep2(LINHA 6), rep3 (LINHA 7)
co2f2a3r1<-f2(betaflor2a3r1,flor2anel.media[[5,4]],flor2anel.media[[5,3]] ) # REP 1 DEU MUITO ALTO, EXCLUIR
##CO2 Floresta 2 Anel 3 Repeticao 2
co2f2a3r2<-f2(betaflor2a3r2,flor2anel.media[[6,4]],flor2anel.media[[6,3]] )
##CO2 Floresta 2 Anel 3 Repeticao 3
co2f2a3r3<-f2(betaflor2a3r3,flor2anel.media[[7,4]],flor2anel.media[[7,3]] )

##CO2 Floresta 2 Anel 4, repeticao 1(LINHA 8),rep2(LINHA 9)
co2f2a4r1<-f2(betaflor2a4r1,flor2anel.media[[8,4]],flor2anel.media[[8,3]] ) # REP 1 DEU MUITO ALTO, EXCLUIR
##CO2 Floresta 2 Anel 4 Repeticao 2
co2f2a4r2<-f2(betaflor2a4r2,flor2anel.media[[9,4]],flor2anel.media[[9,3]] )

#############################################################
##Tabela Final de resultados Flor2
dataflor2.co2<- data.table(amb = c("Flor2", "Flor2", "Flor2",
                            "Flor2", "Flor2", "Flor2",
                            "Flor2", "Flor2", "Flor2","Flor2", "Flor2"),
                   anel = c("anel1", "anel1", "anel1",
                            "anel2", "anel2", "anel2",
                            "anel3", "anel3", "anel3", "anel4","anel4"),
                   rep = c("rep1", "rep2", "rep3",
                       "rep1", "rep2", "rep3",
                        "rep1", "rep2", "rep3","rep1", "rep2"),
                   CO2 = c(co2f2a1r1, co2f2a1r2, co2f2a1r3,
                           co2f2a2r1, co2f2a2r2, co2f2a2r3,
                           co2f2a3r1, co2f2a3r2, co2f2a3r3,
                           co2f2a4r1, co2f2a4r2))

dataflor2.co2

write.xlsx(dataflor2.co2,
          file="c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\dataflor2CO2.xlsx")


###########################################################
a<-betaflor2a1r1*60
b<-flor2anel.media[[1,4]]*100*12
c<-flor2anel.media[[1,3]] + 273.15
#microg_Co2.flor2a1r1 <- (((a)/1000000)*(0.015955+volume)*b)/(8.314*(c))*(1/0.0415475628437) # Parece que esta errado divisao 1/0.041547 em 15/03/2022
microg_Co2.flor2a1r1 <- (((a)/1000000)*(0.015955+volume)*b)/(8.314*(c))*(0.0415475628437) # em 15/03/2022

milig_Co2.flor2a1r1<- microg_Co2.flor2a1r1*1000
milig_Co2.flor2a1r1

micromol_Co2.flor2a1r1<- milig_Co2.flor2a1r1/12/60/60*1000
micromol_Co2.flor2a1r1


################################################
#library(dplyr)
longtudo2<-arrange(longtudo, conc)
head(longtudo2)
longtudo3<-arrange(longtudo2,conc,valor)
head(longtudo3)
write.table(longtudo3, file="/home/roberval/Documentos/Lotofacil/longtudo.txt")
write.table(longtudo3, file=""c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\
            boca_jun_2021_flor2anel_todos.xlsx",/ongtudo.csv")
###############################################################
##############################################################
######################################FLOR 1 ANEL TODOS ##############################
##flor 1 select por ANEL
flor1anel1<-flor1 %>% filter(anel=="anel1")
flor1anel2<-flor1 %>% filter(anel=="anel2")
flor1anel3<-flor1 %>% filter(anel=="anel3")
flor1anel4<-flor1 %>% filter(anel=="anel4")

##Flor 1 Anel 1 select por REP
flor1anel1_r1<-flor1 %>% filter(anel=="anel1", rep=="rep1")
flor1anel1_r1b<-flor1 %>% filter(anel=="anel1", rep=="rep1b")
flor1anel1_r2<-flor1 %>% filter (anel=="anel1", rep=="rep2")
flor1anel1_r3<-flor1 %>% filter (anel=="anel1", rep=="rep3")

##Flor 1 Anel 2 select por REP
flor1anel2_r1<- flor1 %>% filter(anel=="anel2", rep=="rep1")
flor1anel2_r2<- flor1 %>% filter(anel=="anel2", rep=="rep2")
flor1anel2_r3<- flor1 %>% filter(anel=="anel2", rep=="rep3")

##Flor 1 Anel 3 select por REP
flor1anel3_r1<- flor1 %>% filter(anel=="anel3", rep=="rep1")
flor1anel3_r2<- flor1 %>% filter(anel=="anel3", rep=="rep2")
flor1anel3_r3<- flor1 %>% filter(anel=="anel3", rep=="rep3")

##Flor 1 Anel 4 select por REP
flor1anel4_r1<- flor1 %>% filter(anel=="anel4", rep=="rep1")
flor1anel4_r2<- flor1 %>% filter(anel=="anel4", rep=="rep2")
flor1anel4_r3<- flor1 %>% filter(anel=="anel4", rep=="rep3")


## Médias de temp e pressao  FLOR1
flor1anel.media<- summarise(
        select(
        group_by(flor1, anel, rep),
        outside_temperature, inside_pressure, inside_CO2_ratio, soil_moisture, outside_humidity
    ),
    outside_temperature = mean(outside_temperature,na.rm=TRUE),
    inside_pressure = mean(inside_pressure, na.rm=TRUE),
    inside_CO2_ratio = mean(inside_CO2_ratio,na.rm=TRUE),
    soil_moisture = mean(soil_moisture, na.rm=TRUE),
    outside_humidity = mean(outside_humidity, na.rm=TRUE)

)

write.xlsx(flor1anel.media,
         file="c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\flor1anel.media.xlsx")



##OUTSIDE TEMPERATURE
flor1anel.media[[1,3]] #anel1/rep1/outside_temperature
flor1anel.media[[2,3]] #anel1/rep1b/utside_temperature
flor1anel.media[[3,3]] #anel1/rep2/outside_temperature
flor1anel.media[[4,3]] #anel1/rep3/outside_temperature

flor1anel.media[[5,3]] #anel2/rep1/outside_temperature
flor1anel.media[[6,3]] #anel2/rep2/outside_temperature
flor1anel.media[[7,3]] #anel2/rep3/outside_temperature

flor1anel.media[[8,3]] #anel3/rep1/outside_temperature
flor1anel.media[[9,3]] #anel3/rep2/outside_temperature
flor1anel.media[[10,3]] #anel3/rep3/outside_temperature

flor1anel.media[[11,3]] #anel4/rep1/outside_temperature
flor1anel.media[[12,3]] #anel4/rep2/outside_temperature
flor1anel.media[[13,3]] #anel4/rep3/outside_temperature

##INSIDE PRESSURE
flor1anel.media[[1,4]] #anel1/rep1/inside_pressure
flor1anel.media[[2,4]] #anel1b/rep1b/inside_pressure
flor1anel.media[[3,4]] #anel1/rep2/inside_pressure
flor1anel.media[[4,4]] #anel1/rep3/inside_pressure

flor1anel.media[[5,4]] #anel2/rep1/inside_pressure
flor1anel.media[[6,4]] #anel2/rep2/inside_pressure
flor1anel.media[[7,4]] #anel2/rep3/inside_pressure

flor1anel.media[[8,4]] #anel3/rep1/inside_pressure
flor1anel.media[[9,4]] #anel3/rep2/inside_pressure
flor1anel.media[[10,4]] #anel3/rep3/inside_pressure

flor1anel.media[[11,4]] #anel4/rep1/inside_pressure
flor1anel.media[[12,4]] #anel4/rep2/inside_pressure
flor1anel.media[[13,4]] #anel4/rep3/inside_pressure


#write.xlsx(flor1anel.media,
 #         file="c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\flor1anel.media2.xlsx")


## Regressao


## Calculo dos Betas FLORESTA 1 ANEL 1, 2 E 3
##Floresta 1, Anel 1, Rep 1, rep2 e rep3
betaflor1a1r1<-f(flor1anel1_r1)
betaflor1a1r1b<-f(flor1anel1_r1b)
betaflor1a1r2<-f(flor1anel1_r2)
betaflor1a1r3<-f(flor1anel1_r3)

##Floresta 2, Anel 2, Rep 1, Rep2 e Rep3
betaflor1a2r1 <- f(flor1anel1_r1)
betaflor1a2r2 <- f(flor1anel1_r2)
betaflor1a2r3 <-  f(flor1anel2_r3)

##Floresta 2, Anel 3, Rep 1, Rep2 e Rep3
betaflor1a3r1<- f(flor1anel3_r1)
betaflor1a3r2<- f(flor1anel3_r2)
betaflor1a3r3<- f(flor1anel3_r3)

##Floresta 2, Anel 4, Rep 1, Rep2 e Rep3
betaflor1a4r1 <- f(flor1anel4_r1)
betaflor1a4r2 <- f(flor1anel4_r2)
betaflor1a4r3 <- f(flor1anel4_r3)
##############################

#do.call(rbind, lapply(ajustes, function(a){ summary(a)$coeff})) # junta das tabelas
}
###################################################################
## Calculo do CO2 Floresta 2 Anel 1
#olume<-0
#icrog_Co2.flor2a1r1 <- (((betaflor2a1r1*60)/1000000)*(0.015955+volume)*flor2anel.media[[1,4]]*100*12)/(8.314*(flor2anel.media[[1,3]]
#   +273.15))*(1/0.0415475628437)
#icrog_Co2.flor2a1r1


##CO2 Floresta 1 Anel, 1 Repeticao 1 (LINHA 1),rep1b (LINHA2), rep2 (LINHA 3), rep3 (LINHA 4)
co2f1a1r1<-f2(betaflor1a1r1,flor1anel.media[[1,4]],flor1anel.media[[1,3]] )
co2f1a1r1b<-f2(betaflor1a1r1b,flor1anel.media[[2,4]],flor1anel.media[[2,3]] )
##CO2 Floresta 1 Anel 1 Repeticao 2
co2f1a1r2<-f2(betaflor1a1r2,flor1anel.media[[3,4]],flor1anel.media[[3,3]] )
##CO2 Floresta 1 Anel 1 Repeticao 3
co2f1a1r3<-f2(betaflor1a1r2,flor1anel.media[[4,4]],flor1anel.media[[4,3]] )

##CO2 Floresta 1 Anel 2, repeticao 1(LINHA 5),rep2(LINHA 6), rep3 (LINHA 7)
co2f1a2r1<-f2(betaflor1a2r1,flor1anel.media[[5,4]],flor1anel.media[[5,3]] ) # REP 1 DEU MUITO ALTO, EXCLUIR
##CO2 Floresta 1 Anel 2 Repeticao 2
co2f1a2r2<-f2(betaflor1a2r2,flor1anel.media[[6,4]],flor1anel.media[[6,3]] )
##CO2 Floresta 1 Anel 2 Repeticao 3
co2f1a2r3<-f2(betaflor1a2r3,flor1anel.media[[7,4]],flor1anel.media[[7,3]] )

##CO2 Floresta 1 Anel 3, repeticao 1(LINHA 8),rep2(LINHA 9), rep3 (LINHA 10
co2f1a3r1<-f2(betaflor1a3r1,flor1anel.media[[8,4]],flor1anel.media[[8,3]] )
##CO2 Floresta 1 Anel 3 Repeticao 2
co2f1a3r2<-f2(betaflor1a3r2,flor1anel.media[[9,4]],flor1anel.media[[9,3]] )
##CO2 Floresta 1 Anel 3 Repeticao 3
co2f1a3r3<-f2(betaflor1a3r3,flor1anel.media[[10,4]],flor1anel.media[[10,3]] )

##CO2 Floresta 1 Anel 4, repeticao 1(LINHA 11),rep2(LINHA 12), rep13 (LINHA 13)
co2f1a4r1<-f2(betaflor1a4r1,flor1anel.media[[11,4]],flor1anel.media[[11,3]] )
##CO2 Floresta 1 Anel 4 Repeticao 2
co2f1a4r2<-f2(betaflor1a4r2,flor1anel.media[[12,4]],flor1anel.media[[12,3]] )
##CO2 Floresta 1 Anel 4 Repeticao 3
co2f1a4r3<-f2(betaflor1a4r3,flor1anel.media[[13,4]],flor1anel.media[[13,3]] )


#############################################################
##Tabela Final de resultados Flor1
dataflor1.co2<- data.table(amb = c( "Flor1", "Flor1","Flor1","Flor1",
                            "Flor1", "Flor1", "Flor1",
                            "Flor1", "Flor1", "Flor1","Flor1", "Flor1", "Flor1"),
                   anel = c("anel1", "anel1", "anel1","anel1",
                            "anel2", "anel2", "anel2",
                            "anel3", "anel3", "anel3", "anel4","anel4", "anel4"),
                   rep = c("rep1","rep1b", "rep2", "rep3",
                       "rep1", "rep2", "rep3",
                        "rep1", "rep2", "rep3","rep1", "rep2", "rep3"),
                   CO2 = c(co2f1a1r1, co2f1a1r1b, co2f1a1r2, co2f1a1r3,
                           co2f1a2r1, co2f1a2r2, co2f1a2r3,
                           co2f1a3r1, co2f1a3r2, co2f1a3r3,
                           co2f1a4r1, co2f1a4r2, co2f1a4r3))

dataflor1.co2
write.xlsx(dataflor1.co2,
      file="c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\dataflor1Co2.xlsx")

###########################################################
a<-betaflor2a1r1*60
b<-flor2anel.media[[1,4]]*100*12
c<-flor2anel.media[[1,3]] + 273.15
microg_Co2.flor2a1r1 <- (((a)/1000000)*(0.015955+volume)*b)/(8.314*(c))*(1/0.0415475628437)

milig_Co2.flor2a1r1<- microg_Co2.flor2a1r1*1000
milig_Co2.flor2a1r1

micromol_Co2.flor2a1r1<- milig_Co2.flor2a1r1/12/60/60*1000
micromol_Co2.flor2a1r1


################################################
#library(dplyr)
longtudo2<-arrange(longtudo, conc)
head(longtudo2)
longtudo3<-arrange(longtudo2,conc,valor)
head(longtudo3)
write.table(longtudo3, file="/home/roberval/Documentos/Lotofacil/longtudo.txt")
write.table(longtudo3, file=""c:\\Users\\rober\\OneDrive\\Disco-2021\\Minhas_publicacoes_2021\\Serie-Doc-RAD-taxi\\R-analise-co2\\resultados\\
            boca_jun_2021_flor2anel_todos.xlsx",/ongtudo.csv")








################################################################
###############################################################
data <- data.table(name = c("Adam", "Betty", "Carl",
                            "Adam", "Betty", "Carl",
                            "Adam", "Betty", "Carl"),
                   exam = c("exam1", "exam1", "exam1",
                            "exam2", "exam2", "exam2",
                            "exam3", "exam3", "exam3"),
                   grade = c(10.0, 8.4, 6.5,
                              9.8, 8.0, 5.6,
                             9.7, 8.8, 4.5))
data
# muda o formato da tabela
olddata <- reshape(data = newdata, direction = "long")

# muda o nome das colunas para ficar como antes
colnames(olddata) <- c("name", "exam", "grade")

# muda os nome das linhas para ficar como antes
rownames(olddata) <- 1:9

##combinando tabelas
resultado <- merge(x = measurements, y = beams) # a variavel em coamum é particules; x e y é o nome das tabelas

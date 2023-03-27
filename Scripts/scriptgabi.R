# Analise de dados Gabi - 28.nov.2019
## Analise Setembro-2019
rm(list=ls())
dataset<-read.csv("/run/media/roberval/EMBRAPAGABI/Dados emissão de gases TCC/Dados Gabi/nov.2019/set.csv", sep=",", header=TRUE)
dataset<-read.csv("/run/media/roberval/D862-1263/DadosGabiTCC/nov.2019/set.csv", sep=",", header=TRUE)

head(dataset)
attach(dataset)
names(dataset)
edit(dataset)
# Separando os arquivos
install.packages("dplyr")
library(dplyr)

cast<-filter(dataset,ecosystem=="castanha")
edit(cast)
length(cast)
head(cast)
tail(cast) #ultimas linhas

###############################################
# castanha anel1
castanel1<-filter(cast,ring=="anel1")
edit(castanel1)
tail(castanel1)
castanel1b<-filter(castanel1,tempo>1.0)
edit(castanel1b)
tail(castanel1b)
castanel1brep1<-filter(castanel1b, rep=="r1") 
tail(castanel1brep1)
edit(castanel1brep1)
attach(castanel1brep1)
sapply(castanel1brep1, is.numeric)
#### castanha anel2 REP 1
castanel2<-filter(cast,ring=="anel2")
castanel2b<-filter(castanel2,tempo>1.0)
castanel2brep1<-filter(castanel2b, rep=="r1") 
tail(castanel2brep1)
attach(castanel2brep1)
##### #### castanha anel2 REP 2
castanel2brep2<-filter(castanel2b, rep=="r2") 
tail(castanel2brep2)
attach(castanel2brep2)
######### #### castanha anel2 REP 3
castanel2brep3<-filter(castanel2b, rep=="r3") 
tail(castanel2brep3)
attach(castanel2brep3)
##########
####castanha anel3 REP 1
castanel3<-filter(cast,ring=="anel3")
castanel3b<-filter(castanel3,tempo>1.0)
castanel3brep1<-filter(castanel3b, rep=="r1") 
tail(castanel3brep1)
attach(castanel3brep1)
##### #### castanha anel3 REP 2
castanel3brep2<-filter(castanel3b, rep=="r2") 
tail(castanel3brep2)
attach(castanel3brep2)
######### #### castanha anel3 REP 3
castanel3brep3<-filter(castanel3b, rep=="r3") 
tail(castanel3brep3)
attach(castanel3brep3)
#############
####castanha anel4 REP 1
castanel4<-filter(cast,ring=="anel4")
castanel4b<-filter(castanel4,tempo>1.0)
castanel4brep1<-filter(castanel4b, rep=="r1") 
tail(castanel4brep1)
attach(castanel4brep1)
##### #### castanha anel4 REP 2
castanel4brep2<-filter(castanel4b, rep=="r2") 
tail(castanel4brep2)
attach(castanel4brep2)
######### #### castanha anel4 REP 3
castanel4brep3<-filter(castanel4b, rep=="r3") 
tail(castanel4brep3)
attach(castanel4brep3)

##################################################################
# Analise regressao: castanha, anel1, rep 1, setembro 
regcast1<-lm(co2ppm~tempo, data=castanel1brep1)
summary(regcast1)
names(regcast1)
regcast1$coeff[1]
regcast1$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  
plot(castanel1brep1$tempo,castanel1brep1$co2ppm)
plot(tempo,co2ppm)
#############################################

#SETEMBRO - ANEL 1
## Analise: castanha, anel1, rep 1, setembro 
castanel1brep1<-filter(castanel1b, rep=="r1") 
## Analise: castanha, anel1, rep 2, setembro 
castanel1brep2<-filter(castanel1b, rep=="r2") 
## Analise: castanha, anel1, rep 3, setembro 
castanel1brep3<-filter(castanel1b, rep=="r3") 
####SETEMBRO - ANEL 2
## Analise: castanha, anel2, rep 1 
castanel2brep1 
## Analise: castanha, anel2, rep 2 
castanel1brep2<-filter(castanel1b, rep=="r2") 
## Analise: castanha, anel1, rep 3, setembro 
castanel1brep3<-filter(castanel1b, rep=="r3") 

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# SET Anel 1 
reg(castanel1brep1)
reg(castanel1brep2)
reg(castanel1brep3)
# SET Anel 2 
reg(castanel2brep1)
reg(castanel2brep2)
reg(castanel2brep3)
# SET Anel 3 
reg(castanel3brep1)
reg(castanel3brep2)
reg(castanel3brep3)
# SET Anel 4 
reg(castanel4brep1)
reg(castanel4brep2)
reg(castanel4brep3)
####################################FLORESTA NOVEMBRO##########################
dataset<-read.csv("/run/media/roberval/D862-1263/DadosGabiTCC/nov.2019/set.csv", sep=",", header=TRUE)

head(dataset)
tail(dataset)

# Separando os arquivos
#install.packages("dplyr")
library(dplyr)

flor<-filter(dataset,ecosystem=="floresta")
head(flor)
tail(flor) #ultimas linhas

###############################################
# floresta anel1
flor1<-filter(flor,ring=="anel1")
flor1b<-filter(flor1,tempo>1.0)
### floresta anel1 REP 1
flor1brep1<-filter(flor1b, rep=="r1") 
tail(flor1brep1)
head(flor1brep1)
#attach(flor1brep1)
#sapply(flor1brep1, is.numeric)

### floresta anel1 REP 2
flor1brep2<-filter(flor1b, rep=="r2") 
head(flor1brep2)
### floresta anel1 REP 3
flor1brep3<-filter(flor1b, rep=="r3") 
head(flor1brep3)
tail(flor1brep3)
### floresta anel2 REP 1
flor2<-filter(flor,ring=="anel2")
flor2b<-filter(flor2,tempo>1.0)
flor2brep1<-filter(flor2b, rep=="r1") 

##### #### floresta anel2 REP 2
flor2brep2<-filter(flor2b, rep=="r2") 

######### #### castanha anel2 REP 3
flor2brep3<-filter(flor2b, rep=="r3") 

##########
####floresta anel3 REP 1
flor3<-filter(flor,ring=="anel3")
flor3b<-filter(flor3,tempo>1.0)
flor3brep1<-filter(flor3b, rep=="r1") 
head(flor3brep1)
##### #### floresta anel3 REP 2
flor3brep2<-filter(flor3b, rep=="r2") 
head(flor3brep2)
tail(flor3brep2)
######### #### floresta anel3 REP 3
flor3brep3<-filter(flor3b, rep=="r3") 
tail(flor3brep3)
#############
####floresta anel4 REP 1
flor4<-filter(flor,ring=="anel4")
flor4b<-filter(flor4,tempo>1.0)
flor4brep1<-filter(flor4b, rep=="r1") 
tail(flor4brep1)
##### #### floresta anel4 REP 2
flor4brep2<-filter(flor4b, rep=="r2") 

######### #### floresta anel4 REP 3
flor4brep3<-filter(flor4b, rep=="r3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# flor SET Anel 1 
reg(flor1brep1)
reg(flor1brep2)
reg(flor1brep3)
# flor SET Anel 2 
reg(flor2brep1)
reg(flor2brep2)
reg(flor2brep3)
# flor SET Anel 3 
reg(flor3brep1)
reg(flor3brep2)
reg(flor3brep3)
# flor SET Anel 4 
reg(flor4brep1)
reg(flor4brep2)
reg(flor4brep3)



#####
## Analise JUlho-2019
rm(list=ls())
datasetjulho<-read.csv("/run/media/roberval/D862-1263/DadosGabiTCC/solicitados/julho.csv", sep=",", header=TRUE)
head(dataset)
tail(dataset)
attach(datasetjulho)

# Separando os arquivos
#install.packages("dplyr")
library(dplyr)
# Ecosystem CASTANHA
cast<-filter(datasetjulho,ecosystem=="castanha")
edit(cast)
length(cast)
head(cast)
tail(cast) #ultimas linhas

###############################################
# castanha anel1
cast1<-filter(cast,ring=="anel1")

cast1b<-filter(cast1,tempo>1.0)
edit(cast1b)
length(cast1b)
#######Castanha anel1 rep1
cast1brep1<-filter(cast1b, rep=="r1") 
#######Castanha anel1 rep2
cast1brep2<-filter(cast1b, rep=="r2") 
#######Castanha anel1 rep3
cast1brep3<-filter(cast1b, rep=="r3") 

#### castanha anel2 REP 1
cast2<-filter(cast,ring=="anel2")
cast2b<-filter(cast2,tempo>1.0)
cast2brep1<-filter(cast2b, rep=="r1") 
##### #### castanha anel2 REP 2
cast2brep2<-filter(cast2b, rep=="r2") 
######### #### castanha anel2 REP 3
cast2brep3<-filter(cast2b, rep=="r3") 

##########
####castanha anel3 REP 1
cast3<-filter(cast,ring=="anel3")
cast3b<-filter(cast3,tempo>1.0)
cast3brep1<-filter(cast3b, rep=="r1") 
##### #### castanha anel3 REP 2
cast3brep2<-filter(cast3b, rep=="r2") 
######### #### castanha anel3 REP 3
cast3brep3<-filter(cast3b, rep=="r3") 
#############
####castanha anel4 REP 1
cast4<-filter(cast,ring=="anel4")
cast4b<-filter(cast4,tempo>1.0)
cast4brep1<-filter(cast4b, rep=="r1") 
##### #### castanha anel4 REP 2
cast4brep2<-filter(cast4b, rep=="r2") 
######### #### castanha anel4 REP 3
cast4brep3<-filter(cast4b, rep=="r3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# JUL CAST Anel 1 
reg(cast1brep1)
reg(cast1brep2)
reg(cast1brep3)
# JUL CAST Anel 2 
reg(cast2brep1)
reg(cast2brep2)
reg(cast2brep3)
# JUL CAST Anel 3 
reg(cast3brep1)
reg(cast3brep2)
reg(cast3brep3)
# JUL CAST Anel 4 
reg(cast4brep1)
reg(cast4brep2)
reg(cast4brep3)

#regcast2$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  

########## JULHO Floresta

flor<-filter(datasetjulho,ecosystem=="floresta")
head(flor)
tail(flor) #ultimas linhas

###############################################
# floresta anel1
flor1<-filter(flor,ring=="anel1")
flor1b<-filter(flor1,tempo>1.0)
### floresta anel1 REP 1
flor1brep1<-filter(flor1b, rep=="r1") 
tail(flor1brep1)
head(flor1brep1)
#attach(flor1brep1)
#sapply(flor1brep1, is.numeric)

### floresta anel1 REP 2
flor1brep2<-filter(flor1b, rep=="r2") 
head(flor1brep2)
### floresta anel1 REP 3
flor1brep3<-filter(flor1b, rep=="r3") 
head(flor1brep3)
tail(flor1brep3)
### floresta anel2 REP 1
flor2<-filter(flor,ring=="anel2")
flor2b<-filter(flor2,tempo>1.0)
flor2brep1<-filter(flor2b, rep=="r1") 

##### #### floresta anel2 REP 2
flor2brep2<-filter(flor2b, rep=="r2") 

######### #### castanha anel2 REP 3
flor2brep3<-filter(flor2b, rep=="r3") 

##########
####floresta anel3 REP 1
flor3<-filter(flor,ring=="anel3")
flor3b<-filter(flor3,tempo>1.0)
flor3brep1<-filter(flor3b, rep=="r1") 
head(flor3brep1)
##### #### floresta anel3 REP 2
flor3brep2<-filter(flor3b, rep=="r2") 
head(flor3brep2)
tail(flor3brep2)
######### #### floresta anel3 REP 3
flor3brep3<-filter(flor3b, rep=="r3") 
tail(flor3brep3)
#############
####floresta anel4 REP 1
flor4<-filter(flor,ring=="anel4")
flor4b<-filter(flor4,tempo>1.0)
flor4brep1<-filter(flor4b, rep=="r1") 
tail(flor4brep1)
##### #### floresta anel4 REP 2
flor4brep2<-filter(flor4b, rep=="r2") 

######### #### floresta anel4 REP 3
flor4brep3<-filter(flor4b, rep=="r3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# flor JUL Anel 1 
reg(flor1brep1)
reg(flor1brep2)
reg(flor1brep3)
# flor JUL Anel 2 
reg(flor2brep1)
reg(flor2brep2)
reg(flor2brep3)
# flor JUL Anel 3 
reg(flor3brep1)
reg(flor3brep2)
reg(flor3brep3)
# flor JUL Anel 4 
reg(flor4brep1)
reg(flor4brep2)
reg(flor4brep3)
###################################
#####
## Analise Junho-2019
rm(list=ls())
datasetjunho<-read.csv("/run/media/roberval/D862-1263/DadosGabiTCC/solicitados/jun.csv", sep=",", header=TRUE)
attach(datasetjunho)

# Separando os arquivos
#install.packages("dplyr")
library(dplyr)
# Ecosystem CASTANHA
cast<-filter(datasetjunho,ecosystem=="castanha")
edit(cast)
length(cast)
head(cast)
tail(cast) #ultimas linhas

###############################################
# castanha anel1
cast1<-filter(cast,ring=="anel1")

cast1b<-filter(cast1,tempo>1.0)
names(cast1b)
head(cast1b)
edit(cast1b)
length(cast1b)
#######Castanha anel1 rep1
cast1brep1<-filter(cast1b, nome=="casta131.1") 
#######Castanha anel1 rep2
cast1brep2<-filter(cast1b, nome=="casta131.2") 
#######Castanha anel1 rep3
cast1brep3<-filter(cast1b, nome=="casta131.3") 

#### castanha anel2 REP 1
cast2<-filter(cast,ring=="anel2")
cast2b<-filter(cast2,tempo>1.0)
edit(cast2b)
cast2brep1<-filter(cast2b, nome=="casta132.1") 
##### #### castanha anel2 REP 2
cast2brep2<-filter(cast2b, nome=="casta132.2") 
######### #### castanha anel2 REP 3
cast2brep3<-filter(cast2b, nome=="casta132.3") 

##########
####castanha anel3 REP 1
cast3<-filter(cast,ring=="anel3")
cast3b<-filter(cast3,tempo>1.0)
cast3brep1<-filter(cast3b, nome=="casta133.1") 
##### #### castanha anel3 REP 2
cast3brep2<-filter(cast3b, nome=="casta133.2") 
######### #### castanha anel3 REP 3
cast3brep3<-filter(cast3b, nome=="casta133.3") 
#############
####castanha anel4 REP 1
cast4<-filter(cast,ring=="anel4")
cast4b<-filter(cast4,tempo>1.0)
cast4brep1<-filter(cast4b, nome=="casta134.1") 
##### #### castanha anel4 REP 2
cast4brep2<-filter(cast4b, nome=="casta134.2") 
######### #### castanha anel4 REP 3
cast4brep3<-filter(cast4b, nome=="casta134.3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# JUNHO CAST Anel 1 
reg(cast1brep1)
reg(cast1brep2)
reg(cast1brep3)
# JUNHO CAST Anel 2 
reg(cast2brep1)
reg(cast2brep2)
reg(cast2brep3)
# JUNHO CAST Anel 3 
reg(cast3brep1)
reg(cast3brep2)
reg(cast3brep3)
# JUNHO CAST Anel 4 
reg(cast4brep1)
reg(cast4brep2)
reg(cast4brep3)

#regcast2$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  

########## JUnho Floresta

flor<-filter(datasetjunho,ecosystem=="floresta")
head(flor)
tail(flor) #ultimas linhas

###############################################
# floresta anel1
flor1<-filter(flor,ring=="anel1")
flor1b<-filter(flor1,tempo>1.0)
edit(flor1b)
### floresta anel1 REP 1
flor1brep1<-filter(flor1b, nome=="flor121.1") 
tail(flor1brep1)
head(flor1brep1)
#attach(flor1brep1)
#sapply(flor1brep1, is.numeric)

### floresta anel1 REP 2
flor1brep2<-filter(flor1b, nome=="flor121.2") 
head(flor1brep2)
### floresta anel1 REP 3
flor1brep3<-filter(flor1b, nome=="flor121.3") 
head(flor1brep3)
tail(flor1brep3)
### floresta anel2 REP 1
flor2<-filter(flor,ring=="anel2")
flor2b<-filter(flor2,tempo>1.0)
flor2brep1<-filter(flor2b, nome=="flor122.1") 

##### #### floresta anel2 REP 2
flor2brep2<-filter(flor2b, nome=="flor122.2") 

######### #### castanha anel2 REP 3
flor2brep3<-filter(flor2b, nome=="flor122.3") 

##########
####floresta anel3 REP 1
flor3<-filter(flor,ring=="anel3")
flor3b<-filter(flor3,tempo>1.0)
flor3brep1<-filter(flor3b, nome=="flor123.1") 
edit(flor3brep1)
head(flor3brep1)
##### #### floresta anel3 REP 2
flor3brep2<-filter(flor3b, nome=="flor123.2") 
edit(flor3brep2)
head(flor3brep2)
tail(flor3brep2)
######### #### floresta anel3 REP 3
flor3brep3<-filter(flor3b, nome=="flor123.3") 
tail(flor3brep3)
#############
####floresta anel4 REP 1
flor4<-filter(flor,ring=="anel4")
flor4b<-filter(flor4,tempo>1.0)
flor4brep1<-filter(flor4b, nome=="flor124.1") 
tail(flor4brep1)
##### #### floresta anel4 REP 2
flor4brep2<-filter(flor4b, nome=="flor124.2") 

######### #### floresta anel4 REP 3
flor4brep3<-filter(flor4b, nome=="flor124.3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# flor JUNHO Anel 1 
reg(flor1brep1)
reg(flor1brep2)
reg(flor1brep3)
# flor JUNHO Anel 2 
reg(flor2brep1)
reg(flor2brep2)
reg(flor2brep3)
# flor JUNHO Anel 3 
reg(flor3brep1)
reg(flor3brep2)
reg(flor3brep3)
# flor JUNHO Anel 4 
reg(flor4brep1)
reg(flor4brep2)
reg(flor4brep3)
###################################
## Analise Março-2019
rm(list=ls())
datasetmar<-read.csv("/run/media/roberval/D862-1263/DadosGabiTCC/solicitados/marco.csv", sep=",", header=TRUE)
attach(datasetmar)


  # Separando os arquivos
#install.packages("dplyr")
library(dplyr)
# Ecosystem CASTANHA
cast<-filter(datasetmar,ecosystem=="castanha")
#edit(cast)
#length(cast)
#head(cast)
#tail(cast) #ultimas linhas

###############################################
# castanha anel1
cast1<-filter(cast,ring=="anel1")
#length(cast1)
cast1b<-filter(cast1,tempo>1.0)
#length(cast1b)
#names(cast1b)
#head(cast1b)
#edit(cast1b)
#length(cast1b)
#######Castanha anel1 rep1
cast1brep1<-filter(cast1b, nome=="casta131.1") 
#edit(cast1brep1)
#######Castanha anel1 rep2
cast1brep2<-filter(cast1b, nome=="casta131.2") 
#edit(cast1brep2)
#######Castanha anel1 rep3
cast1brep3<-filter(cast1b, nome=="casta131.3") 

#### castanha anel2 REP 1
cast2<-filter(cast,ring=="anel2")
#length(cast2)
cast2b<-filter(cast2,tempo>1.0)
#edit(cast2b)
cast2brep1<-filter(cast2b, nome=="casta132.1") 
##### #### castanha anel2 REP 2
cast2brep2<-filter(cast2b, nome=="casta132.2") 
######### #### castanha anel2 REP 3
cast2brep3<-filter(cast2b, nome=="casta132.3") 

##########
####castanha anel3 REP 1
cast3<-filter(cast,ring=="anel3")
#edit(cast3)
#length(cast3)
cast3b<-filter(cast3,tempo>1.0)
#edit(cast3b)
cast3brep1<-filter(cast3b, nome=="casta133.1") 
##### #### castanha anel3 REP 2
cast3brep2<-filter(cast3b, nome=="casta133.2") 
#edit(cast3brep2)
######### #### castanha anel3 REP 3
cast3brep3<-filter(cast3b, nome=="casta133.3") 
#edit(cast3brep3)
#############
####castanha anel4 REP 1
cast4<-filter(cast,ring=="anel4")
cast4b<-filter(cast4,tempo>1.0)
cast4brep1<-filter(cast4b, nome=="casta134.1") 
#edit(cast4brep1)
##### #### castanha anel4 REP 2
cast4brep2<-filter(cast4b, nome=="casta134.2") 
######### #### castanha anel4 REP 3
cast4brep3<-filter(cast4b, nome=="casta134.3") 


##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}


media<-function(x){
    print(mean(x$inside_temp), data=x)
  print(mean(x$inside_pres), data=x)
  print(mean(x$inside_humid),data=x)
  print(mean(x$soil_moist), data=x)
  print(mean(x$soil_temp),data=x)
  print(mean(x$co2ppm), data=x)
  print(mean(x$outside_humid))
  print(mean(x$outside_par))
  print(mean(x$outside_pres))
  print(mean(x$outside_temp))
  print(mean(x$power_box_charge), data=x)
}


mediana<-function(x){
  print(median(x$inside_temp), data=x)
  print(median(x$inside_pres), data=x)
  print(median(x$inside_humid),data=x)
  print(median(x$soil_moist), data=x)
  print(median(x$soil_temp),data=x)
  print(median(x$co2ppm), data=x)
  print(median(x$outside_humid))
  print(median(x$outside_par))
  print(median(x$outside_pres))
  print(median(x$outside_temp))
  print(median(x$power_box_charge), data=x)
}

desvio<-function(x){
  print(sd(x$inside_temp), data=x)
  print(sd(x$inside_pres), data=x)
  print(sd(x$inside_humid),data=x)
  print(sd(x$soil_moist), data=x)
  print(sd(x$soil_temp),data=x)
  print(sd(x$co2ppm), data=x)
  print(sd(x$outside_humid))
  print(sd(x$outside_par))
  print(sd(x$outside_pres))
  print(sd(x$outside_temp))
  print(sd(x$power_box_charge), data=x)
}

##Castanha anel1
media(cast1brep1)
mediana(cast1brep1)
desvio(cast1brep1)

media(cast1brep2)
mediana(cast1brep2)
desvio(cast1brep2)

media(cast1brep3)
mediana(cast1brep3)
desvio(cast1brep3)

#Castanha anel2
media(cast2brep1)
mediana(cast2brep1)
desvio(cast2brep1)

media(cast2brep2)
mediana(cast2brep2)
desvio(cast2brep2)

media(cast2brep3)
mediana(cast2brep3)
desvio(cast2brep3)

#Castanha anel3
media(cast3brep1)
mediana(cast3brep1)
desvio(cast3brep1)

media(cast3brep2)
mediana(cast3brep2)
desvio(cast3brep2)

media(cast3brep3)
mediana(cast3brep3)
desvio(cast3brep3)

#Castanha anel 4
media(cast4brep1)
mediana(cast4brep1)
desvio(cast4brep1)

media(cast4brep2)
mediana(cast4brep2)
desvio(cast4brep2)

media(cast4brep3)
mediana(cast4brep3)
desvio(cast4brep3)

#valor.central<-function(x,estat="mean"){
 # if(estat=="mean")
#return(mean(x))
 # else if (estat="median")
  #  return(median(x))
  #else return(NULL) }
 


# MARÇO CAST Anel 1 
reg(cast1brep1)
reg(cast1brep2)
reg(cast1brep3)
# MARÇO CAST Anel 2 
reg(cast2brep1)
reg(cast2brep2)
reg(cast2brep3)
# MARÇO CAST Anel 3 
reg(cast3brep1)
reg(cast3brep2)
reg(cast3brep3)
# MARÇO CAST Anel 4 
reg(cast4brep1)
reg(cast4brep2)
reg(cast4brep3)

#regcast2$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  

########## Março Floresta

flor<-filter(datasetmar,ecosystem=="floresta")
head(flor)
tail(flor) #ultimas linhas

###############################################
# floresta anel1
flor1<-filter(flor,ring=="anel1")
flor1b<-filter(flor1,tempo>1.0)
#edit(flor1b)
### floresta anel1 REP 1
flor1brep1<-filter(flor1b, nome=="flor121.1") 
#tail(flor1brep1)
#head(flor1brep1)
#attach(flor1brep1)
#sapply(flor1brep1, is.numeric)

### floresta anel1 REP 2
flor1brep2<-filter(flor1b, nome=="flor121.2") 
head(flor1brep2)
### floresta anel1 REP 3
flor1brep3<-filter(flor1b, nome=="flor121.3") 
#head(flor1brep3)
#tail(flor1brep3)
### floresta anel2 REP 1
flor2<-filter(flor,ring=="anel2")
flor2b<-filter(flor2,tempo>1.0)
flor2brep1<-filter(flor2b, nome=="flor122.1") 

##### #### floresta anel2 REP 2
flor2brep2<-filter(flor2b, nome=="flor122.2") 

######### #### castanha anel2 REP 3
flor2brep3<-filter(flor2b, nome=="flor122.3") 

##########
####floresta anel3 REP 1
flor3<-filter(flor,ring=="anel3")
flor3b<-filter(flor3,tempo>1.0)
flor3brep1<-filter(flor3b, nome=="flor123.1") 
#head(flor3brep1)
##### #### floresta anel3 REP 2
flor3brep2<-filter(flor3b, nome=="flor123.2") 
#head(flor3brep2)
#tail(flor3brep2)
######### #### floresta anel3 REP 3
flor3brep3<-filter(flor3b, nome=="flor123.3") 
#tail(flor3brep3)
#############
####floresta anel4 REP 1
flor4<-filter(flor,ring=="anel4")
flor4b<-filter(flor4,tempo>1.0)
flor4brep1<-filter(flor4b, nome=="flor124.1") 
#tail(flor4brep1)
##### #### floresta anel4 REP 2
flor4brep2<-filter(flor4b, nome=="flor124.2") 

######### #### floresta anel4 REP 3
flor4brep3<-filter(flor4b, nome=="flor124.3") 

#################################
#Calculo das medias
#Floresta anel1
media(flor1brep1)
mediana(flor1brep1)
desvio(flor1brep1)

media(flor1brep2)
mediana(flor1brep2)
desvio(flor1brep2)

media(flor1brep3)
mediana(flor1brep3)
desvio(flor1brep3)

#Floresta anel2
media(flor2brep1)
mediana(flor2brep1)
desvio(flor2brep1)

media(flor2brep2)
mediana(flor2brep2)
desvio(flor2brep2)

media(flor2brep3)
mediana(flor2brep3)
desvio(flor2brep3)

#Floresta anel3
media(flor3brep1)
mediana(flor3brep1)
desvio(flor3brep1)

media(flor3brep2)
mediana(flor3brep2)
desvio(flor3brep2)

media(flor3brep3)
mediana(flor3brep3)
desvio(flor3brep3)

#Floresta anel 4
media(flor4brep1)
mediana(flor4brep1)
desvio(flor4brep1)

media(flor4brep2)
mediana(flor4brep2)
desvio(flor4brep2)

media(flor4brep3)
mediana(flor4brep3)
desvio(flor4brep3)



##################################################################

reg<-function(x){
  reg<-lm(co2ppm~tempo, data=x)
  #print(summary(reg))
  print(reg$coeff)
}
# flor Março Anel 1 
reg(flor1brep1)
reg(flor1brep2)
reg(flor1brep3)
# flor março Anel 2 
reg(flor2brep1)
reg(flor2brep2)
reg(flor2brep3)
# flor março Anel 3 
reg(flor3brep1)
reg(flor3brep2)
reg(flor3brep3)
# flor Março Anel 4 
reg(flor4brep1)
reg(flor4brep2)
reg(flor4brep3)
###################################





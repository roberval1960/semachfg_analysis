data121r1 <- read.csv("/run/media/roberval/0F29B50370115127/Backup/disco-2017/IPAAM-projeto-Alemanha-Agosto/Pos-doutorado/Dados-Dropox-Jorg/SEMACH-Ecorespira-fase3/0120/0120/rober-regresao-julh-2019/co2-ring121-1.csv", sep=",",head=TRUE) 
data121r1 <- read.csv("/run/media/roberval/0F29B50370115127/Disco-2019/Pibic-Abner/0121-1.csv",sep="\t", head=TRUE) 
head(data121r1)
edit(data121r1)
attach(data121r1)
names(data121r1)
rega<-lm(co2ppm~tempo)
summary(rega)
names(rega)
rega$coeff[1]
rega$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  # não bateu os valores
###############
data121r2 <- read.csv("/run/media/roberval/0F29B50370115127/Disco-2019/Pibic-Abner/0121/121-2b.csv",sep="\t", head=TRUE) 
head(data121r2)
edit(data121r2)
attach(data121r2)
names(data121r2)
reg2<-lm(co2ppm~tempo)
summary(reg2)
names(reg2)
reg2$coeff[1]
reg2$coeff[2]  #Anstieg(incremento) lin Reg  [ppm/min]  # não bateu os valores






############################## 
# Em 21/09/2019 retirando o 1 minuto inicial
data223r1<- read.csv("/home/roberval/Documents/2019-revisitando-nopa/Arquivos_Dropbox/EcoRespira-Amazon/EcoRespira-Amazon/phase03/semachecorespiraf3/0220/0223-1a.csv", head=TRUE)
head(data223r1)
edit(data223r1)
attach(data223r1)
names(data223r1)
reg223_1<-lm(CO2ppm~Zeit)
summary(reg223_1)
names(reg223_1)
reg223_1$coeff[1]
reg223_1$coeff[2] 
#Zeit 
#37.49017 


################################
# Em 21/09/2019 sem retirar o 1 minuto inicial
data223r1<-read.csv("/home/roberval/Documents/2019-revisitando-nopa/Arquivos_Dropbox/EcoRespira-Amazon/EcoRespira-Amazon/phase03/semachecorespiraf3/0220/0223-1.csv", head=TRUE)
head(data223r1)
edit(data223r1)
attach(data223r1)
names(data223r1)
reg223_1<-lm(CO2ppm~Zeit)
summary(reg223_1)
names(reg223_1)
reg223_1$coeff[1]
reg223_1$coeff[2] 


#> reg223_1$coeff[2]
#Zeit 
#41.10248 

##################
data121r2 <- read.csv("/run/media/roberval/0F29B50370115127/Backup/disco-2017/IPAAM-projeto-Alemanha-Agosto/Pos-doutorado/Dados-Dropox-Jorg/SEMACH-Ecorespira-fase3/0120/0120/rober-regresao-julh-2019/co2-ring121-2.csv", sep=",",head=TRUE) 
head(data121r2)
attach(data121r2)
names(data121r2)
regb<-lm(co2ppm~tempo)
summary(regb)
names(regb)
regb$coeff[1]
regb$coeff[2]
###################
data121r3 <- read.csv("/run/media/roberval/0F29B50370115127/Backup/disco-2017/IPAAM-projeto-Alemanha-Agosto/Pos-doutorado/Dados-Dropox-Jorg/SEMACH-Ecorespira-fase3/0120/0120/rober-regresao-julh-2019/co2-ring121-3.csv", sep=",",head=TRUE) 
data121r3<-read.csv("H:/Disco-2019/Pibic-Abner/co2-ring121-3.csv", sep=",",head=TRUE)
head(data121r3)
attach(data121r3)
names(data121r3)
regc<-lm(co2ppm~tempo)
summary(regc)
names(regc)
regc$coeff[1]
regc$coeff[2]

#Calculando a média:
(reg_media= (rega$coeff[2]+regb$coeff[2]+regc$coeff[2])/3)



#############################
daten <- read.csv("/run/media/roberval/0F29B50370115127/Backup/disco-2017/IPAAM-projeto-Alemanha-Agosto/Pos-doutorado/Dados-Dropox-Jorg/SEMACH-Ecorespira-fase3/0120/0120/rober-regresao-julh-2019/daten.csv", sep=",",head=TRUE) 
#arquivo daten.csv é a partir de 0 minuto
head(daten)
attach(daten)
names(daten)
plot(tempo,co2)
regd<-lm(co2~tempo)
abline(regd,col=2)
summary(regd)
names(regd)
regd$coeff[1]
regd$coeff[2]
####################################
daten2 <- read.csv("/run/media/roberval/0F29B50370115127/Disco-2019/Pibic-Abner/daten2.csv", sep=",",head=TRUE) 
# arquivo daten2 é a partir de 1 minuto
head(daten2)
edit(daten2)
attach(daten2)
names(daten2)
plot(tempo,co2)
rege<-lm(co2~tempo)
abline(rege,col=2)
summary(rege)
names(rege)
        rege$coeff[1]
        rege$coeff[2]
        

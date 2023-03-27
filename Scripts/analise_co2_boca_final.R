## Analise Co2 boca Final 
ls()
rm(dados)
# Carregamento dos pacotes
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr, data.table, openxlsx, scales, ggpubr, latex2exp, tikzDevice, readx)

library(ggplot2)
library(dplyr)
library(scales)
#install.packages("Rtools")
#install.packages("ggpubr")
library(ggpubr)
library(latex2exp)
library(tikzDevice)
library(readxl)
library(data.table)
library(openxlsx)

# Selecao do diretÃ³rio de trabalho (working directory)
getwd()
setwd("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_semach/R-analise-co2-Boca/dados")


# Carregamento do banco de dados
getwd()
dados <- read.csv("summary-todos.csv", stringsAsFactors = T)
glimpse(dados)
head(dados)

## Modificando DataLancamento para o formato data
dados$data <- as.Date(dados$data, format = "%m/%d/%Y")
attach(dados)
### Salvando o mÃªs e o ano em colunas separadas
dados <- dados %>% mutate(ano = format(data, "%Y"))
dados <- dados %>% mutate(mes = format(data, "%m"))


write.xlsx(dados,
           file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_semach/R-analise-co2-Boca/dados/dados2.xlsx"))


### Salvando o anel e a rep em colunas separadas  ## Aprender
#dados <- dados %>% mutate(ponto_med = format.(ponto_med, "anel"))
#dados <- dados %>% mutate(rep = format(ponto_med, "%m"))

# media de outside_temperature e inside_pressure
base2<-select(
  group_by(dados, anel, rep),
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

#Agrupado  as medias de CO2  por ano e ambiente
media.co2<-group_by(dados,ambiente, ano) %>%
  summarize(co2.medias=(mean(F_CO2)),
            alt.sd=(sd(F_CO2)))
media.co2
is.data.frame(media.co2)
write.xlsx(media.co2,
           file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_semach/R-analise-co2-Boca/dados/tab_media_co2.xlsx"))

## Tentando fazer todas a medias
media.co2<-group_by(dados,ambiente, ano) %>%
  summarize(co2.medias=(mean(F_CO2)),
            alt.sd=(sd(F_CO2)),
            CO2_ppm=(mean(CO2_ppm)),
            CO2_ppm.sd=(sd(CO2_ppm)))

a1<-group_by(dados, ambiente, ano, mes)
a2<-select(a1, F_CO2, SM_perc, ST_graus, rH_o_perc, T_o_graus, PAR)
write.xlsx(a2,
           file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_semach/R-analise-co2-Boca/dados/tab_ano_mes.xlsx"))

a3<-summarise(a2,
              F_CO2.m = mean(F_CO2, na.rm=TRUE),
              F_CO2.sd = sd(F_CO2, na.rm=TRUE),
              SM_perc.m = mean(SM_perc, na.rm=TRUE),
              SM_perc.sd=sd(SM_perc, na.rm=TRUE),
              ST_graus.m=mean(ST_graus, na.rm=TRUE),
              ST_graus.sd=sd(ST_graus, na.rm=TRUE),
              rH_o_perc.m=mean(rH_o_perc, na.rm=TRUE),
              rH_o_perc.sd=sd(rH_o_perc, na.rm=TRUE),
              T_o_graus.m=mean(T_o_graus, na.rm=TRUE),
              T_o_graus.sd=sd(T_o_graus, na.rm=TRUE),
              PAR.m=mean(PAR, na.rm=TRUE),
              PAR.sd=sd(PAR, na.rm=TRUE))

write.xlsx(a3,
           file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_semach/R-analise-co2-Boca/dados/tab_ano_mes_variav.xlsx"))



#### Ficou bom - Ponto medicao vs co2
graf_ponto_med_fco2 <- dados %>% filter(ambiente %in% c("ARL", "Floresta")) %>%
  ggplot(aes(x=ponto_med, y = F_CO2, color = ambiente)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs( x = "Tempo (Julho 2019 a Julho 2021)", color = "ambiente")+
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
graf_ponto_med_fco2 + scale_x_discrete(breaks = seq(min(dados$mes),
                                     max(dados$mes),
                                     by = 1))

#gl + scale_x_continuous(breaks = seq(1935, 2020, by = 5))

graf_ponto_med_fco2  
##Salva o ultimo grafico
ggsave("graph_ponto_med_co2.png", height = 4.5, width = 8, units = "in", dpi = 600)


#### Ficou bom - estacao vc co2
graf_estacao_fco2 <- dados %>% filter(estacao %in% c("seca", "umida")) %>%
  ggplot(aes(x=ponto_med, y = F_CO2, color = estacao)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs(x = "Tempo (JUlho 2019 a Julho 2021)", color = "ambiente")+
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)")) 

    graf_estacao_fco2 + scale_x_discrete(breaks = seq(min(dados$mes),
                                                        max(dados$mes),
                                                        by = 1))
    
graf_estacao_fco2  

##Salva o ultimo grafico
ggsave("graf_estacao_fco2.png", height = 4.5, width = 8, units = "in", dpi = 600)





####################
#################

## Ponto medicao vs PAR vs ambiente
graf_ponto_med_PAR <- dados %>% filter(ambiente %in% c("ARL", "Floresta")) %>%
  ggplot(aes(x=ponto_med, y = PAR, color = ambiente)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs(x = "Tempo (JUlho 2019 a Julho 2021)", color = "ambiente")+
  ylab(TeX("Radiação PAR ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
graf_ponto_med_PAR + scale_x_discrete(breaks = seq(min(dados$mes),
                                                  max(dados$mes),
                                                  by = 1))
ggsave("graf_ponto_med_PAR.png", height = 4.5, width = 8, units = "in", dpi = 600)

## Ponto medicao vs PAR vs estacao
graf_ponto_med_PAR_estacao <- dados %>% filter(estacao %in% c("seca", "umida")) %>%
  ggplot(aes(x=ponto_med, y = PAR, color = estacao)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs(x = "Tempo (JUlho 2019 a Julho 2021)", color = "estacao")+
  ylab(TeX("Radiação PAR ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
graf_ponto_med_PAR_estacao
graf_ponto_med_PAR_estacao + scale_x_discrete(breaks = seq(min(dados$mes),
                                                   max(dados$mes),
                                                   by = 1))
ggsave("graf_ponto_med_PAR_estacao.png", height = 4.5, width = 8, units = "in", dpi = 600)



## Ponto medicao vs Temperatura do solo vc ambiente
graf_ponto_med_TempSolo <- dados %>% filter(ambiente %in% c("ARL", "Floresta")) %>%
  ggplot(aes(x=ponto_med, y = T_o_graus, color = ambiente)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs(x="Tempo (Julho 2019 a Julho 2021)", y = "Temperatura do solo (graus C)", color = "ambiente")
  
graf_ponto_med_TempSolo  
graf_ponto_med_TempSolo + scale_x_discrete(breaks = seq(min(dados$mes),
                                                   max(dados$mes),
                                                   by = 1))

ggsave("graf_ponto_med_TempSolo.png", height = 4.5, width = 8, units = "in", dpi = 600)

## Ponto medicao vs Temperatura do solo vc estacao
graf_ponto_med_TempSolo_estacao <- dados %>% filter(estacao %in% c("seca", "umida")) %>%
  ggplot(aes(x=ponto_med, y = T_o_graus, color = estacao)) +
  geom_point(stat = "summary", fun = "mean",size=3, position = position_dodge(.4)) +
  labs(x="Tempo (Julho 2019 a Julho 2021)", y = "Temperatura do solo (graus C)", color = "estacao")

graf_ponto_med_TempSolo_estacao  
graf_ponto_med_TempSolo_estacao + scale_x_discrete(breaks = seq(min(dados$mes),
                                                        max(dados$mes),
                                                        by = 1))

ggsave("graf_ponto_med_TempSolo_estacao.png", height = 4.5, width = 8, units = "in", dpi = 600)






## Modificando a ordem das camadas (a ordem dos geoms importa!)

##Ficou bom - Umidade do solo-SM_percent vs co2 vc ambiente
graf_umid_co2_ambiente<-ggplot(data = dados, aes(x = SM_perc, y = F_CO2)) +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = SM_perc, y = F_CO2, colour = as.factor(ambiente)))+
  geom_smooth(method = "lm", se = F, color = "black", size = 1) +
  labs(x="Umidade do solo (%)", color = "ambiente")+
   ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)"))

graf_umid_co2_ambiente
ggsave("graph_umid_co2_ambiente.png", height = 4.5, width = 8, units = "in", dpi = 600)

##Ficou bom - Umidade do solo-SM_percent vs co2 vs estacao
graf_umid_co2_estacao<-ggplot(data = dados, aes(x = SM_perc, y = F_CO2)) +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = SM_perc, y = F_CO2, colour = as.factor(estacao)))+
  geom_smooth(method = "lm", se = F, color = "black", size = 1) +
  labs(x="Umidade do solo (%)", color = "estacao")+
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)"))

graf_umid_co2_estacao
ggsave("graph_umid_co2_estacao.png", height = 4.5, width = 8, units = "in", dpi = 600)




#Ficou bom - Umidade do ar rH_o vs co2
graf_umid_rh_o_co2<-ggplot(data = dados, aes(x = rH_o_perc, y = F_CO2)) +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = rH_o_perc, y = F_CO2, colour = as.factor(ambiente)))+
  geom_smooth(method = "lm", se = F, color = "black", size= 1 ) + 
  labs(x="Umidade do ar (%)", color = "estacao")+
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
  
graf_umid_rh_o_co2

##Salva o ultimo grafico
ggsave("graph_umid_rh_o__co2_tendencia.png", height = 4.5, width = 8, units = "in", dpi = 600)

#Ficou bom - temperatura do solo ST vs co2 vs ambiente
graf_temp_solo_o_co2<-ggplot(data = dados, aes(x = ST_graus, y = F_CO2)) +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = ST_graus, y = F_CO2, colour = as.factor(ambiente)))+
  geom_smooth(method = "lm", se = F, color = "black", size= 1 ) + 
  labs(x="Temperatura do solo (graus C)", color = "estacao")+
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
 
graf_temp_solo_o_co2

ggsave("graph_temp_solo_o__co2_tendencia.png", height = 4.5, width = 8, units = "in", dpi = 600)



#Ficou bom - radiacao PAR vs co2
graf_radiacao_co2<-ggplot(data = dados, aes(x = PAR, y = F_CO2)) +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = PAR, y = F_CO2, colour = as.factor(ambiente)))+
  geom_smooth(method = "lm", se = F, color = "black", size= 0.7 ) + 
  #geom_point(color = "#61988E", shape = 16, size = 2)+
  #labs(y = "Fluxo de CO2 (TeX(sprintf(r'($\mu)) mol) ", x = "Umidade do solo (%)", color="ambiente")
  #ylab(TeX("Fluxo de CO2 ($\\frac{2hc^2}{\\mu mol\\beta}$"))
  ylab(TeX("Fluxo de CO2 ($\\mu mol {m^{-2}}{s^{-1}}$)")) +
  xlab(TeX("Radiação PAR ($\\mu mol {m^{-2}}{s^{-1}}$)")) 
  
graf_radiacao_co2

ggsave("graph_temp_solo_o__co2_tendencia.png", height = 4.5, width = 8, units = "in", dpi = 600)

##############
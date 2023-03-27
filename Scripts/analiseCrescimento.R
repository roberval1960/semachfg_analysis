rm(list=ls()
   ## C:\Program Files\R\R-4.1.0\bin\x64
   options(digits=2)

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

##setwd("C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_M_Transm\\Dados-analise")
getwd() 
setwd("C:\\Users\\rober\\OneDrive\\Disco-Universal\\Banco-de-dados\\Banco_dados_Taxi_Boca")
  
ls()
rm(list=ls())
###############################################################################
################ ANALISE  #######################################################################################################################
dados <- read.xlsx("taxi-boca.xlsx", sheet=2)
attach(dados)
head(dados)
tail(dados)
is.numeric(dados$alt_m)
is.numeric(dados$dap)

########### Complete Case
complete.cases(dados) # 
dim(dados) #
dados_complete<-dados[complete.cases(dados),]
dim(dados_complete)
is.data.frame(dados_complete)
head(dados_complete)
complete.cases(dados$alt_m)
h_complete<-dados$alt_m[complete.cases(dados$alt_m),] #Errado
length(h_complete)
#####################################################

##########################################

# media de altura e dap 

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados, idade_mes) %>%
  summarize(alt.med=(mean(alt_m,na.rm=TRUE)),
            alt.sd=(sd(alt_m,na.rm=TRUE)))
media.alt
write.xlsx(media.alt, file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_Taxi_Boca/media.alt.xlsx"))

# Criação da variavel IMAalt
dados <- mutate(dados, IMAalt = (alt_m/idade_ano))
head(dados)

# Criação da variavel IMAdap
dados <- mutate(dados, IMAdap = (dap/idade_ano))
head(dados)


## Agrupando imaALT  por ano
media.IMAalt<-group_by(dados, idade_ano) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados, idade_ano) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
                      dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

## fazer todas a medias

a1<-group_by(dados, idade_ano)
a2<-select(a1, alt_m,dap, IMAalt, IMAdap)
write.xlsx(a2, file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_Taxi_Boca/dados,idade.xlsx"))

a3<-summarise(a2,
              alt_media = mean(alt_m, na.rm=TRUE),
              alt.sd = sd(alt_m, na.rm=TRUE),
              dap_media = mean(dap, na.rm=TRUE),
              dap_sd=sd(dap, na.rm=TRUE),
              IMA_alt.media=mean(IMAalt, na.rm=TRUE),
              IMA_alt.sd=sd(IMAalt, na.rm=TRUE),
              IMA_dap.media=mean(IMAdap, na.rm=TRUE),
              IMA_dap.sd=sd(IMAdap, na.rm=TRUE))
              
write.xlsx(a3, file=("C:/Users/rober/OneDrive/Disco-Universal/Banco-de-dados/Banco_dados_Taxi_Boca/dados.media.todas.xlsx"))


#Grarifcos de dispersao dap x alt 
grafdapalt = ggplot() +
  geom_point(data = dados,  size=3 ,shape = 16,aes(x = dap, y = alt_m, colour = as.factor(idade_mes)))+
  geom_smooth(method = "lm", se = F, color = "black", size = 1) +
  labs(y="Altura total (m)", x="Dap (cm)", color = "idade_mes")+
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))
grafdapalt
grafdapalt + scale_color_discrete(labels=c("12", "18", "30", "38"))

grafdapalt
ggsave("grafdapalt.png", height = 4.5, width = 8, units = "in", dpi = 600)



# Este ta bom tambem
gs <- ggplot(data = dados, aes(x = dap, y = alt_m)) +
  geom_point(color = "#61988E", shape = 16, size = 3) +
  geom_smooth(method = "lm", se = F, color = "red", size = 0.5) +
  labs(y = "Altura (m)", x = "Dap (cm)") +
  scale_y_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                   accuracy=0.1,
                                                    scale = 1e-0)) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    accuracy=0.1,
                                                    scale = 1e-0)) +
  labs(y = "Altura total (m) ", x = "Dap (cm)")
gs

gs + theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))+
  labs(y = "Altura (m)", x = "dap (cm)", color = "idade (mes)")
##Salva o ultimo grafico
ggsave("graph_dap_alt_tendencia.png", height = 4.5, width = 6, units = "in", dpi = 600)


##Ficou bom
graphdapalt + theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))+
labs(y = "Altura (m)", x = "dap (cm)", color = "idade (mes)")

##Salva o ultimo grafico
ggsave("graph_dap_alt.png", height = 4.5, width = 8, units = "in", dpi = 600)


 
 
##########################
ggplot(data = dados, aes(x = dap, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean")

## Incluindo barras de erros (usando também o summary) - altura

graf_alt_barra_error<-ggplot(data = dados, aes(x = idade_ano, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)+
  labs(y = "Altura (m)", x = "Idade (anos)")
graf_alt_barra_error

graf_alt_barra_error + theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))

ggsave("graph_alt_barra_error.png", height = 4.5, width = 8, units = "in", dpi = 600)
#############
## Incluindo barras de erros (usando também o summary) - dap

graf_dap_barra_error<-ggplot(data = dados, aes(x = idade_ano, y = dap)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)+
  labs(y = "Dap (cm)", x = "Idade (anos)")
graf_dap_barra_error

graf_dap_barra_error + theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))

ggsave("graph_dap_barra_error.png", height = 4.5, width = 8, units = "in", dpi = 600)



ggplot(data = dados, aes(x = idade_mes, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.min = "min", fun.max = "max")


ggplot(data = dados, aes(x = idade_ano, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)

## Usando IC 95% ao invés de erro-padrão (pacote ggpubr)

pacman::p_load(ggpubr)

ggplot(data = dados, aes(x = idade_mes, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_ci", width = 0.3)

#grafico altura idade
graf_idade_alt<-ggplot(data = dados, aes(x = idade_mes, y = alt_m)) +
  geom_point(stat = "summary", fun = "mean",position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.3)+
  labs(y = "Altura total (m)", x = "Idade (Meses)")
graf_idade_alt
graf_idade_alt +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))

ggsave("graph_idade_alt.png", height = 4.5, width = 8, units = "in", dpi = 600)

#grafico idade dap
graf_idade_dap<-ggplot(data = dados, aes(x = idade_mes, y = dap)) +
  geom_point(stat = "summary", fun = "mean",position = position_dodge(0.4)) +
  geom_errorbar(stat = "summary", fun.data = "mean_sd", width = 0.3)+
  labs(y = "Diâmetro ã altura do peito (cm)", x = "Idade (Meses)")
graf_idade_dap
graf_idade_dap +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))

ggsave("graph_idade_dap.png", height = 4.5, width = 8, units = "in", dpi = 600)

#retirando fundo e grades
graph_idade_alt + theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
## Remover grades + mudar fonte e fundo do facet

graph_idade_alt + facet_wrap(~ idade_mes) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "lavenderblush3"),
        strip.text = element_text(size = 10))+
  scale_y_continuous(expand = expansion(add = c(0,10)),
                     labels = scales::number_format(accuracy = 0.1,
                                                    decimal.mark = ",",
                                                    big.mark = "."))+
  scale_x_continuous(expand = expansion(add = c(0,40)),
                     labels = scales::number_format(accuracy = 0,
                                                    decimal.mark = ",",
                                                    big.mark = "."))+
  
  
  scale_y_continuous(labels = scales::label_number(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-0))+
                                                  
  scale_x_continuous(labels = scales::label_number(big.mark = ".",
                                                    decimal.mark = ",",
                                                   scale=1e-0))
  
                                                    
# Opcoes: strip.text.x, strip.text.y
# Opcoes: panel.grid.major, panel.grid.minor

### Usando uma fonte personalizada

pacman::p_load(extrafont)
font_import()
loadfonts()

graph_idade_alt + theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold", family = "Montserrat"),
        axis.text = element_text(size = 11, family = "Nunito"))


## Salvar os graficos em alta resolucao

### ggsave - por padrao, salva o ultimo grafico rodado

ggsave("graph_idade_alt.png", height = 4.5, width = 6, units = "in", dpi = 600)
# Formatos aceitos: tiff, png, pdf, jpeg, eps, svg...
# Unidades aceitas: in, cm, mm, px


ggsave(plot = gs, "Lucro_local_mundial.tiff",  height = 4.5, width = 6,
       units = "in", dpi = 600)



############################### deu muito certo
is.list(media.alt2)
media.alt2[[1]]
media.alt2[[2]]
media.alt2[[3]]
media.alt2.df<-as.data.frame(media.alt2)
is.data.frame(media.alt2)
#Concatenando alt e dap
list.alt.dap<-c(media.alt2,media.dap[3],media.dap[4])
alt.dap.47a.df<-as.data.frame(list.alt.dap)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47A")
write.xlsx(alt.dap.47a.df, file=c("alt.dap.47a.xlsx"))

#################################################
####NAO DEU MUITO CERTO
base47a_list<-split(base47a,list(base47a$idade_anos,base47a$perim_parc))
edit(base47a_list)
alt_out<-lapply(base47a_list, function(x){meanx=mean(x$alt,na.rm=TRUE)})
alt_out
dap_out<-lapply(base47a_list, function(x){meanx=mean(x$dap)})
head(alt_out)

dados47a_list<-split(dados47a,list(dados47a$perim_parc,dados47a$idade_anos))

edit(base47a_list)

alt_out<-lapply(dados47a_list,
                function(x){data.frame(meanx=mean(x$alt,na.rm=TRUE))})
alt_out
dap_out<-lapply(base47a_list, function(x){meanx=mean(x$dap,na.rm=true)})
head(alt_out)

alt_out_df<-do.call(rbind,alt_out)
is.data.frame(alt_out)
rownames(alt_out)<-NULL; print(alt_out_df)
edit(alt_out_df)
head(alt_out_df)
################################################################

# concatenate rows
#df.out <- do.call(rbind,lt.out)
#rownames(df.out) <- NULL; print(df.out)

####### GRAFICOS  ##################
   GGplot
   ggplot(data=media.alt, aes(x=idade_anos, y=alt.med))+
     geom_point()+
     geom_line()
   ggplot(data=media.alt, aes(x=perim_parc, y=alt.med))+
     geom_point()+
     geom_line()

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47A")
jpeg("graf.alt.47a.jpeg")
graf.alt<-   ggplot(media.alt2, aes(idade_anos, alt2.med)) +
   geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #   geom_point()                             # theme_classic() +
   #scale_shape_manual(values=c(2,3,5,7,9,12,14,15 16,17))+
    xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_y_continuous(breaks=seq(0,4,0.5), expand=c(0,0),limits=c(0,4)) +
   scale_x_continuous(breaks=seq(1,5,0.5), expand=c(0,0), limits=c(1,5))+                  
   geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   ggtitle("Altura Média - Perímetro 47A")+  theme(plot.title = element_text(hjust = 0.5))+
                     geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
dev.off()


   ggplot(media.alt, aes(idade_anos, alt.med)) +
                     geom_point() +                                          # theme_classic() +
                     xlab("Idade (anos)") + ylab("Altura total (m)") +
                    #scale_x_continuous(breaks=seq(1,5,0.2), expand=c(0,0), limits=c(1,5))+
                   scale_y_continuous(breaks=seq(0,4,0.5), expand=c(0,0),limits=c(0,4))  +
   geom_smooth(method = "loess", se = FALSE)


## Grafico por Parcela
jpeg("graf.alt.parcela.47a.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .) ### FICOU Muito  BOM PODE MELHORAR
plot(graf.alt.parc)
dev.off

##Grafico por parcela colorido
jpeg("graf.alt.parcela.color.47a.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
dev.off

##Grafico por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   geom_point(aes(colour=factor(parcela)))+
   
plot(graf.alt.idade)

##Boxplot parcela vs altura total
alt.parc.box <- ggplot(dados47a, aes(parcela, alt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   xlab("Parcela (40 árvores, 360 m²)") + ylab("Altura total (m)")  
   plot(alt.parc.box)

##Boxplot idade vs altura total

alt.idade.box <- ggplot(dados47a, aes(parcela,idade_anos, alt)) +
   geom_boxplot(aes(fill = parcela))    # geom_boxplot() # nao ficou muito bom
plot(alt.idade.box)


## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total - Perímetro 47A")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

IMAalt.parc.box <- ggplot(dados47a, aes(parcela, IMAalt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   ggtitle("DAP Médio - Perímetro 47A")+  theme(plot.title = element_text(hjust = 0.5))+
   scale_x_continuous(breaks=seq(3.5,5,0.3), limits=c(3.5,5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
jpeg("graf.dap.parcela.color.47a.jpeg")
graf.dap.parc.47a.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.47a.color)
dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados47a, aes(parcela, dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 47 B CASTANHA#################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()


ls()
rm(list=ls())
options(digits = 2)
dados47b<- read.xlsx("47B.xlsx", sheet=2)

attach(dados47b)
head(dados47b)
tail(dados47b)
is.numeric(dados47b$alt)
dados47b$alt<-as.numeric(dados47b$alt)
#dados47b$IMAalt<-as.numeric(dados47b$IMAalt)
is.numeric(dados47b$dap)
dados47b$dap<-as.numeric(dados47b$dap)
#dados47b$IMAdap<-as.numeric(dados47b$IMAdap)

############################################
#Criando variavel IMAalt
is.numeric(dados47b$idade_anos)
dados47b <- mutate(dados47b,
                         IMAalt = (dados47b$alt/dados47b$idade_anos))
edit(dados47b)
                   
# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
media.alt
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.47b.df<-as.data.frame(list.alt.dap)
edit(alt.dap.47b.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47B")
write.xlsx(alt.dap.47b.df, file=c("alt.dap.47b.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47B")
#jpeg("graf.alt.47b.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
    xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(1.5,2.3,0.2), limits=c(1.5,2.5))+
   ggtitle("Altura total média - Per. 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
    geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   #geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
   geom_point() +   theme_bw() +  
    xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)


#ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point() +                                          # theme_classic() +
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_x_continuous(breaks=seq(1,5,0.2), expand=c(0,0), limits=c(1,5))+
   scale_y_continuous(breaks=seq(0,4,0.5), expand=c(0,0),limits=c(0,4))  +
   geom_smooth(method = "loess", se = FALSE)


## Grafico por Parcela
#jpeg("graf.alt.parcela.47b.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .) ### FICOU Muito  BOM PODE MELHORAR
plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.47b.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))+
geom_jitter(position = position_jitter(width = 0.05, height = 0.05))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
  geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_point(aes(colour=factor(parcela)))
   plot(graf.alt.idade)

##Boxplot parcela vs altura total
alt.parc.box <- ggplot(dados47b, aes(parcela, alt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   xlab("Parcela (40 árvores, 360 m²)") + ylab("Altura total (m)")  
plot(alt.parc.box)

##Boxplot idade vs altura total
#Não ficou bom
alt.idade.box <- ggplot(dados47b, aes(parcela,idade_anos, alt)) + geom_boxplot() # nao ficou muito bom
plot(alt.idade.box)


## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
IMAalt.parc.box <- ggplot(dados47b, aes(parcela, IMAalt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)
## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   ggtitle("DAP Médio - Perímetro 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   scale_x_continuous(breaks=seq(3.5,5,0.3), limits=c(3.5,5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
jpeg("graf.dap.parcela.color.47b.jpeg")
graf.dap.parc.47b.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.47b.color)
dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados47b, aes(parcela, dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 47 B TAXI#################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()


ls()
rm(list=ls())
options(digits = 2)
dados47b<- read.xlsx("47B.xlsx", sheet=3)
edit(dados47b)
attach(dados47b)
head(dados47b)
tail(dados47b)
is.numeric(dados47b$alt)
dados47b$alt<-as.numeric(dados47b$alt)
#dados47b$IMAalt<-as.numeric(dados47b$IMAalt)
is.numeric(dados47b$dap)
dados47b$dap<-as.numeric(dados47b$dap)
#dados47b$IMAdap<-as.numeric(dados47b$IMAdap)

############################################
#Criando variavel IMAalt
is.numeric(dados47b$idade_anos)
dados47b <- mutate(dados47b,
                   IMAalt = (dados47b$alt/dados47b$idade_anos))
edit(dados47b)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
media.alt
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados47b, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.47b.df<-as.data.frame(list.alt.dap)
edit(alt.dap.47b.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47B-taxi")
write.xlsx(alt.dap.47b.df, file=c("alt.dap.47btaxi.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47B-taxi")
#jpeg("graf.alt.47b.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(1.5,2.3,0.2), limits=c(1.5,2.5))+
   ggtitle("Altura total média - Per. 47B-taxi")+  theme(plot.title = element_text(hjust = 0.5))+
  # geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)


#ggplot(media.alt, aes(idade_anos, alt.med)) +
geom_point() +                                          # theme_classic() +
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_x_continuous(breaks=seq(1,5,0.2), expand=c(0,0), limits=c(1,5))+
   scale_y_continuous(breaks=seq(0,4,0.5), expand=c(0,0),limits=c(0,4))  +
   geom_smooth(method = "loess", se = FALSE)


## Grafico por Parcela
#jpeg("graf.alt.parcela.47b.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.47b.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
  # geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
shade <- if_else(dados47b$idade_anos >= 2, "blue", "gray")
alt.parc.box <- ggplot(dados47b, aes(parcela,alt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("Altura total (m)")  
plot(alt.parc.box)

##Boxplot idade vs altura total
#Não ficou bom
alt.idade.box <- ggplot(dados47b, aes(parcela,idade_anos, alt)) + geom_boxplot() # nao ficou muito bom
plot(alt.idade.box)


## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
  # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
shade <- if_else(dados47b$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados47b, aes(parcela, IMAalt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)
## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   ggtitle("DAP Médio - Perímetro 47B")+  theme(plot.title = element_text(hjust = 0.5))+
   scale_x_continuous(breaks=seq(3.5,5,0.3), limits=c(3.5,5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
jpeg("graf.dap.parcela.color.47b.jpeg")
graf.dap.parc.47b.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.47b.color)
dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados47b, aes(parcela, dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 47 C #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados47c<- read.xlsx("47C.xlsx", sheet=2)
edit(dados47c)
attach(dados47c)
head(dados47c)
tail(dados47c)
is.numeric(dados47c$alt)
dados47c$alt<-as.numeric(dados47c$alt)
#dados47b$IMAalt<-as.numeric(dados47b$IMAalt)
is.numeric(dados47c$dap)
dados47c$dap<-as.numeric(dados47c$dap)
#dados47b$IMAdap<-as.numeric(dados47b$IMAdap)

############################################
#Criando variavel IMAalt
is.numeric(dados47c$idade_anos)
dados47c <- mutate(dados47c,
                   IMAalt = (dados47c$alt/dados47c$idade_anos))
edit(dados47c)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados47c, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
media.alt
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados47c, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados47c, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.47c.df<-as.data.frame(list.alt.dap)
edit(alt.dap.47c.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47C")
write.xlsx(alt.dap.47c.df, file=c("alt.dap.47c.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47B-taxi")
#jpeg("graf.alt.47b.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+
   ggtitle("Altura total média - Per. 47C")+  theme(plot.title = element_text(hjust = 0.5))+
   # geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 47C")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.47b.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.47b.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#shade <- if_else(dados47c$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados47c, aes(parcela,alt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(alt.med = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
    geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("Altura total (m)")  
plot(alt.parc.box)


##Boxplot idade vs altura total
#Não ficou bom
alt.idade.box <- ggplot(dados47c, aes(parcela,idade_anos, alt)) + geom_boxplot() # nao ficou muito bom
plot(alt.idade.box)


## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 47C")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados47c$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados47c, aes(parcela, IMAalt)) + #geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(IMAalt.med = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   geom_boxplot(aes(fill = parcela)) +
  # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+
    ggtitle("DAP Médio - Perímetro 47C")+  theme(plot.title = element_text(hjust = 0.5))+
   scale_x_continuous(breaks=seq(2.5,3.5,0.3), limits=c(2.5,3.5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.47c.jpeg")
graf.dap.parc.47c.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.47c.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados47c, aes(parcela, dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

##Boxplot parcela vs dap
#shade <- if_edap.parc.box <- ggplot(dados47c, aes(parcela,dap)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   stat_summary(media.dap[3] = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab ("Parcela (40 árvores, 360 m²)") + ylab ("DAP (cm)") 

plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 47 D #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados47d<- read.xlsx("47d.xlsx", sheet=2)
edit(dados47d)
attach(dados47d)
head(dados47d)
tail(dados47d)
is.numeric(dados47d$alt)
dados47d$alt<-as.numeric(dados47d$alt)
dados47d$IMAalt<-as.numeric(dados47d$IMAalt)
is.numeric(dados47d$dap)
dados47d$dap<-as.numeric(dados47d$dap)
#dados47d$IMAdap<-as.numeric(dados47d$IMAdap)

############################################
#Criando variavel IMAalt
is.numeric(dados47d$idade_anos)
dados47d <- mutate(dados47d,
                   IMAalt = (alt/idade_anos))
edit(dados47d)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados47d, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
media.alt
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados47d, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados47d, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.47d.df<-as.data.frame(list.alt.dap)
edit(alt.dap.47d.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47D")
write.xlsx(alt.dap.47d.df, file=c("alt.dap.47d.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\47D")
#jpeg("graf.alt.47b.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+
   ggtitle("Altura total média - Per. 47d")+  theme(plot.title = element_text(hjust = 0.5))+
   # geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 47d")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.47d.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.47d.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#shade <- if_else(dados47d$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados47d, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
   # fun.y esta deprecated, use "fun". 
######
##Boxplot idade vs altura total
#Não ficou bom
alt.idade.box <- ggplot(dados47d, aes(idade_anos,parcela,alt)) + geom_boxplot() # nao ficou muito bom
plot(alt.idade.box)


## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 47d")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados47d$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados47d, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+
   ggtitle("DAP Médio - Perímetro 47d")+  theme(plot.title = element_text(hjust = 0.5))+
   scale_x_continuous(breaks=seq(2.5,3.5,0.3), limits=c(2.5,3.5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.47d.jpeg")
graf.dap.parc.47d.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.47d.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados47d, aes(parcela, dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

##Boxplot parcela vs altura total
#shade <- if_else(dados47d$idade_anos >= 3, "blue", "gray")
dap.parc.box <- ggplot(dados47d, aes(parcela,dap)) + #geom_boxplot(colour="blue")+ ## ficoubom
   geom_boxplot(aes(fill = parcela)) +
   stat_summary(dap.med = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 48 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados48<- read.xlsx("48.xlsx", sheet=2)
edit(dados48)
attach(dados48)
head(dados48)
tail(dados48)
is.numeric(dados48$alt)
dados48$alt<-as.numeric(dados48$alt)
dados48$IMAalt<-as.numeric(dados48$IMAalt)
is.numeric(dados48$dap)
dados48$dap<-as.numeric(dados48$dap)
#dados48$IMAdap<-as.numeric(dados48$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)
dados48<-arrange(dados48,idade_anos)
head(select(dados48, idade_anos,alt), 20)
tail(select(dados48, idade_anos,alt), 20)

############################################
#Criando variavel IMAalt
is.numeric(dados48$idade_anos)
dados48 <- mutate(dados48,
                   IMAalt = (alt/idade_anos))
edit(dados48)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados48, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados48<-mutate(dados48, alt.med=(mean(alt, na.rm=TRUE)),
                alt.sd=(sd(alt, na.rm=TRUE)))
media.alt
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados48, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


## Agrupando dap por ano e parcela

media.dap<-group_by(dados48, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.48.df<-as.data.frame(list.alt.dap)
edit(alt.dap.48.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48")
write.xlsx(alt.dap.48.df, file=c("alt.dap.48.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48")
#jpeg("graf.alt.48.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+
   ggtitle("Altura total média - Per. 48")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 48")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.48.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.48.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#shade <- if_else(dados48$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados48, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(1.5,3.5,0.2), limits=c(1.5,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 48")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados47d$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados48, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   scale_x_continuous(breaks=seq(2.5,3.5,0.2), limits=c(2.5,3.5))+
   ggtitle("DAP Médio - Perímetro 48")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.47d.jpeg")
graf.dap.parc.48.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.48.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados48, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
    xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)


###############################################################################
################ ANALISE PARCELA 48A #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados48a<- read.xlsx("48A.xlsx", sheet=1)
edit(dados48a)
attach(dados48a)
head(dados48a)
tail(dados48a)
is.numeric(dados48a$alt)
dados48a$alt<-as.numeric(dados48a$alt)
dados48a$IMAalt<-as.numeric(dados48a$IMAalt)
is.numeric(dados48a$dap)
dados48a$dap<-as.numeric(dados48a$dap)
#dados48a$IMAdap<-as.numeric(dados48a$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)
dados48a<-arrange(dados48a,idade_anos)
head(select(dados48a, idade_anos,alt), 20)
tail(select(dados48a, idade_anos,alt), 20)

############################################
#Criando variavel IMAalt
is.numeric(dados48a$idade_anos)
dados48a$idade_anos<-as.numeric(dados48a$idade_anos)

dados48a <- mutate(dados48a,
                  IMAalt = (alt/idade_anos))
edit(dados48a)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados48a, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados48<-mutate(dados48, alt.med=(mean(alt, na.rm=TRUE)),
                alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados48a, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados48a, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap

############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.48a.df<-as.data.frame(list.alt.dap)
edit(alt.dap.48a.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48a")
write.xlsx(alt.dap.48a.df, file=c("alt.dap.48a.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48a")
#jpeg("graf.alt.48a.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 48a")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 48a")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.48a.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.48a.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#shade <- if_else(dados48a$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados48a, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 48a")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados48a$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados48a, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 48a")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.48a.jpeg")
graf.dap.parc.48a.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.48a.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados48a, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

###############################################################################
################ ANALISE PARCELA 48B #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados48b<- read.xlsx("48B.xlsx", sheet=2)
edit(dados48b)
attach(dados48b)
head(dados48b)
tail(dados48b)
is.numeric(dados48b$alt)
dados48b$alt<-as.numeric(dados48b$alt)
dados48b$IMAalt<-as.numeric(dados48b$IMAalt)
is.numeric(dados48b$dap)
dados48b$dap<-as.numeric(dados48b$dap)
#dados48b$IMAdap<-as.numeric(dados48b$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados48b<-arrange(dados48b,idade_anos)
head(select(dados48b, idade_anos,alt), 40)
tail(select(dados48b, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados48b$alt, na.rm=TRUE);min(dados48b$alt, na.rm=TRUE)
max(dados48b$dap, na.rm=TRUE);min(dados48b$dap, na.rm=TRUE)
max(dados48b$idade_anos, na.rm=TRUE);min(dados48b$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados48b$idade_anos)
#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados48b <- mutate(dados48b,
                   IMAalt = (alt/idade_anos))
#Criando variavel IMAdap
is.numeric(dados48b$idade_anos)
#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados48b <- mutate(dados48b,
                   IMAdap = (dap/idade_anos))


edit(dados48b)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados48b, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados48b<-mutate(dados48b, alt.med=(mean(alt, na.rm=TRUE)),
                alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados48b, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt
# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados48b, idade_anos,parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados48b, idade_anos,parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])
alt.dap.48b.df<-as.data.frame(list.alt.dap)
edit(alt.dap.48b.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48B")
write.xlsx(alt.dap.48b.df, file=c("alt.dap.48b.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48b")
#jpeg("graf.alt.48a.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 48b")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 48b")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.48b.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.48b.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados48b$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados48b, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 48b")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados48a$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados48b, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(idade_anos, dap.med)) +
   geom_point() +   theme_bw()+                                       # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 48b")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = FALSE) #se=banda de confiança
plot(graf.dap)

##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.48a.jpeg")
graf.dap.parc.48b.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.48b.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados48b, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(idade_anos, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,3,0.3), limits=c(1,3))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 48b")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados48b, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade

#### Analisando Replantio
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")

rm(dados47a.alt)
ls()
rm(list=ls())
options(digits = 2)
mudas <- read.xlsx("replanta.xlsx", sheet=7)

names(mudas)
is.numeric(mudas$total_mudas)
min(mudas$total_mudas)
max(mudas$total_mudas)

sum.fase<-group_by(mudas, fase,ano, perim) %>%
   summarize(total.fase=(sum(mudas$mudas_total)))
             
#######################
q_data <- airquality %>%
   arrange(Month) %>%
   mutate(month_abb = fct_inorder(month.abb[Month]))

ggplot(data = aq_data, aes(month_abb, Temp, fill = month_abb)) +
   geom_bar(stat = "summary", fun.y = "mean") +
   labs(title = "Mean Temp by Month",
        x = "",
        y = "Temp (deg. F)") +
   scale_fill_brewer(palette = "Paired")
###########################33
##Fase
ggplot(data = mudas, aes(factor(fase), y=total_mudas, fill = fase)) +
   geom_bar(stat = "summary", fun = "sum") +
   #geom_bar(stat = "identity") +
   scale_y_continuous(breaks=seq(0,60000,5000), limits=c(0,60000)) +
   labs(title = "Replantio de mudas por Fase",
        x = "",
        y = "Total de mudas" ) #+ scale_fill_brewer(palette = "Paired"))
   
   
ggplot(data = mudas, aes(factor(ano), y=total_mudas, fill = fase)) +
   geom_bar(stat = "summary", fun = "sum") +
   #stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   #geom_bar(stat = "identity") +
   #scale_y_continuous(breaks=seq(0,3,0.3), limits=c(100,3)) +
   labs(title = "Replantio de mudas por ano e fase",
        x = "Ano",
        y = "Total de mudas" + scale_fill_brewer(palette = "Paired"))

#nao deu muito certo
pl <- ggplot(mudas, aes(x=total_mudas))
pl + geom_histogram()
             
pl2 <- pl + geom_histogram(binwidth = 0.1, col='black', fill='green', alpha=0.4)
pl2

###############################################################################
################ ANALISE PARCELA 39 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados39<- read.xlsx("per_antigos.xlsx", sheet=2)
is.numeric(dados39$alt)
dados39$alt<-as.numeric(dados39$alt)
dados39$IMAalt<-as.numeric(dados39$IMAalt)
is.numeric(dados39$dap)
dados39$dap<-as.numeric(dados39$dap)
#dados48b$IMAdap<-as.numeric(dados48b$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados39<-arrange(dados39,idade_anos)
head(select(dados39, idade_anos,alt), 40)
tail(select(dados38, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados39$alt, na.rm=TRUE);min(dados39$alt, na.rm=TRUE)
max(dados39$dap, na.rm=TRUE)
min(dados39$dap, na.rm=TRUE)
max(dados39$idade_anos, na.rm=TRUE);min(dados39$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados39$idade_anos)
#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados39 <- mutate(dados39,
                   IMAalt = (alt/idade_anos))
#Criando variavel IMAdap
is.numeric(dados39$idade_anos)
#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados39 <- mutate(dados39,
                   IMAdap = (dap/idade_anos))


edit(dados39)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados39, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados39<-mutate(dados39, alt.med=(mean(alt, na.rm=TRUE)),
                 alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados39, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt
# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados39, parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados39, parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)
##DAP
is.list(media.dap)
media.dap[[1]]
media.dap[[2]]
media.dap[[3]]
media.dap.df<-as.data.frame(media.dap)
names(media.dap.df)
is.data.frame(media.dap.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results")
write.xlsx(media.dap.df, file=c("media.dap.39.xlsx"))


#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])

alt.dap.39.df<-as.data.frame(list.alt.dap)
edit(alt.dap.39.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\39")
write.xlsx(alt.dap.39.df, file=c("alt.dap.39.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\48b")
#jpeg("graf.alt.39.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 39")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 39")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.39.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.39.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados39$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados39, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 39")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados39$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados39, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(x=parcela,y=dap.med)) +
   geom_point() +   theme_bw()+    # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   #scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 39")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "lm", se = TRUE) #se=banda de confiança
plot(graf.dap)

##Bar plot #  
ggplot(data = dados39, aes(parcela, dap, fill = parcela)) +
   #geom_bar(stat = "summary", fun = "mean") +
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   labs(title = "Media do DAP por parcela",
        x = "",
        y = "DAP (cm)" + scale_fill_brewer(palette = "Paired"))
###############
ggplot(dados39, aes(parcela, dap, fill=parcela)) +
   #geom_bar(stat = "identity",size=3) +
   geom_bar(stat = "summary", fun = "mean",size=2) + # , fill="white"
   #stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   stat_summary(fun.data = mean_se, geom = "errorbar") +
   #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
labs(title = "Média do DAP por parcela",
     x = "", y = "DAP (cm)")+
   scale_fill_brewer(palette = "Paired")

dados39<-dados39[-161,] # excluir a linha 161


##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.39.jpeg")
graf.dap.parc.39.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.39.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados39, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(idade_anos, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,3,0.3), limits=c(1,3))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 39")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados48b, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade

###############################################################################
################ ANALISE PARCELA 43 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados43<- read.xlsx("per_antigos.xlsx", sheet=3)
is.numeric(dados43$alt)
dados43$alt<-as.numeric(dados43$alt)
dados43$IMAalt<-as.numeric(dados43$IMAalt)
is.numeric(dados43$dap)
dados43$dap<-as.numeric(dados43$dap)
#dados43$IMAdap<-as.numeric(dados43$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados43<-arrange(dados43,idade_anos)
head(select(dados43, idade_anos,alt), 40)
tail(select(dados43, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados43$alt, na.rm=TRUE);min(dados43$alt, na.rm=TRUE)
max(dados43$dap, na.rm=TRUE)
min(dados43$dap, na.rm=TRUE)
max(dados43$idade_anos, na.rm=TRUE);min(dados43$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados43$idade_anos)
#dados43b$idade_anos<-as.numeric(dados43b$idade_anos)
dados43 <- mutate(dados43,
                  IMAalt = (alt/idade_anos))

#Criando variavel IMAdap
is.numeric(dados43$idade_anos)

#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados43 <- mutate(dados43,
                 IMAdap = (dap/idade_anos))


edit(dados43)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados43, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados43<-mutate(dados43, alt.med=(mean(alt, na.rm=TRUE)),
                alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados43, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt

# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados43, parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados43, parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)

##DAP
is.list(media.dap)
media.dap[[1]]
media.dap[[2]]
media.dap[[3]]
media.dap.df<-as.data.frame(media.dap)
names(media.dap.df)
is.data.frame(media.dap.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\43")
write.xlsx(media.dap.df, file=c("media.dap.43.xlsx"))


#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])

alt.dap.43.df<-as.data.frame(list.alt.dap)
edit(alt.dap.43.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\43")
write.xlsx(alt.dap.43.df, file=c("alt.dap.43.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\43")
#jpeg("graf.alt.43.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 43")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 43")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.43.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.43.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados43$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados43, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 43")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados43$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados43, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(x=parcela,y=dap.med)) +
   geom_point() +   theme_bw()+    # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   #scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 43")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "lm", se = TRUE) #se=banda de confiança
plot(graf.dap)

##Bar plot #  
ggplot(data = dados43, aes(parcela, dap, fill = parcela)) +
   #geom_bar(stat = "summary", fun = "mean") +
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   labs(title = "Media do DAP por parcela",
        x = "",
        y = "DAP (cm)" + scale_fill_brewer(palette = "Paired"))
###############
ggplot(dados43, aes(parcela, dap, fill=parcela)) +
   #geom_bar(stat = "identity",size=3) +
   geom_bar(stat = "summary", fun = "mean",size=2) + # , fill="white"
   #stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   stat_summary(fun.data = mean_se, geom = "errorbar") +
   #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
   labs(title = "Média do DAP por parcela",
        x = "", y = "DAP (cm)")+
   scale_fill_brewer(palette = "Paired")

dados43<-dados43[-161,] # excluir a linha 161


##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.43.jpeg")
graf.dap.parc.43.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.43.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados43, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(parcela, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 43")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados43, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade

###############################################################################
################ ANALISE PARCELA 9 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados9<- read.xlsx("per_antigos.xlsx", sheet=4)
is.numeric(dados9$alt)
dados9$alt<-as.numeric(dados9$alt)
dados9$IMAalt<-as.numeric(dados9$IMAalt)
is.numeric(dados9$dap)
dados9$dap<-as.numeric(dados9$dap)
#dados9$IMAdap<-as.numeric(dados9$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados9<-arrange(dados9,idade_anos)
head(select(dados9, idade_anos,alt), 40)
tail(select(dados9, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados9$alt, na.rm=TRUE);min(dados9$alt, na.rm=TRUE)
max(dados9$dap, na.rm=TRUE)
min(dados9$dap, na.rm=TRUE)
max(dados9$idade_anos, na.rm=TRUE);min(dados9$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados9$idade_anos)
#dados9b$idade_anos<-as.numeric(dados9b$idade_anos)
dados9 <- mutate(dados9,
                  IMAalt = (alt/idade_anos))

#Criando variavel IMAdap
is.numeric(dados9$idade_anos)

#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados9 <- mutate(dados9,
                  IMAdap = (dap/idade_anos))


edit(dados9)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados9, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados9<-mutate(dados9, alt.med=(mean(alt, na.rm=TRUE)),
                alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados9, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt

# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados9, parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados9, parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)

##DAP
is.list(media.dap)
names(media.dap)
media.dap[[1]]
media.dap[[2]]
min(media.dap[[2]])
max(media.dap[[2]])
media.dap[[3]]
media.dap.df<-as.data.frame(media.dap)
names(media.dap.df)
is.data.frame(media.dap.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\9")
write.xlsx(media.dap.df, file=c("media.dap.9.xlsx"))


#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])

alt.dap.9.df<-as.data.frame(list.alt.dap)
edit(alt.dap.9.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\9")
write.xlsx(alt.dap.9.df, file=c("alt.dap.9.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\9")
#jpeg("graf.alt.9.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 9")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 9")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.9.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.9.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados9$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados9, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 9")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados9$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados9, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(x=parcela,y=dap.med)) +
   geom_point() +   theme_bw()+    # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   #scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 9")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "lm", se = TRUE) #se=banda de confiança
plot(graf.dap)

##Bar plot #  
ggplot(data = dados9, aes(parcela, dap, fill = parcela)) +
   #geom_bar(stat = "summary", fun = "mean") +
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
    labs(title = "Media do DAP por parcela",
        x = "",
        y = "DAP (cm)" + scale_fill_brewer(palette = "Paired"))
###############
ggplot(dados9, aes(parcela, dap, fill=parcela)) +
   #geom_bar(stat = "identity",size=3) +
   geom_bar(stat = "summary", fun = "mean",size=2) + # , fill="white"
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
   stat_summary(fun.data = mean_se, geom = "errorbar") +
   #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
   labs(title = "Média do DAP por parcela",
        x = "", y = "DAP (cm)")+
   scale_fill_brewer(palette = "Paired")

dados9<-dados9[-161,] # excluir a linha 161


##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.9.jpeg")
graf.dap.parc.9.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.9.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados9, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(parcela, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 9")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados9, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade


###############################################################################
################ ANALISE PARCELA 6 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados6<- read.xlsx("per_antigos.xlsx", sheet=5)
is.numeric(dados6$alt)
dados6$alt<-as.numeric(dados6$alt)
dados6$IMAalt<-as.numeric(dados6$IMAalt)
is.numeric(dados6$dap)
dados6$dap<-as.numeric(dados6$dap)
#dados6$IMAdap<-as.numeric(dados6$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados6<-arrange(dados6,idade_anos)
head(select(dados6, idade_anos,alt), 40)
tail(select(dados6, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados6$alt, na.rm=TRUE);min(dados6$alt, na.rm=TRUE)
max(dados6$dap, na.rm=TRUE)
min(dados6$dap, na.rm=TRUE)
max(dados6$idade_anos, na.rm=TRUE);min(dados6$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados6$idade_anos)
#dados6b$idade_anos<-as.numeric(dados6b$idade_anos)
dados6 <- mutate(dados6,
                 IMAalt = (alt/idade_anos))

#Criando variavel IMAdap
is.numeric(dados6$idade_anos)

#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados6 <- mutate(dados6,
                 IMAdap = (dap/idade_anos))


edit(dados6)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados6, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados6<-mutate(dados6, alt.med=(mean(alt, na.rm=TRUE)),
               alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados6, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt

# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados6, parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados6, parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)

##DAP
is.list(media.dap)
names(media.dap)
media.dap[[1]]
media.dap[[2]]
min(media.dap[[2]])
max(media.dap[[2]])
media.dap[[3]]
media.dap.df<-as.data.frame(media.dap)
names(media.dap.df)
is.data.frame(media.dap.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\6")
write.xlsx(media.dap.df, file=c("media.dap.6.xlsx"))


#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])

alt.dap.6.df<-as.data.frame(list.alt.dap)
edit(alt.dap.6.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\6")
write.xlsx(alt.dap.6.df, file=c("alt.dap.6.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\6")
#jpeg("graf.alt.6.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 6")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 6")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.6.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.6.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados6$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados6, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 6")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados6$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados6, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(x=parcela,y=dap.med)) +
   geom_point() +   theme_bw()+    # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   #scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 6")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "lm", se = TRUE) #se=banda de confiança
plot(graf.dap)

##Bar plot #  
ggplot(data = dados6, aes(parcela, dap, fill = parcela)) +
   #geom_bar(stat = "summary", fun = "mean") +
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
   labs(title = "Media do DAP por parcela",
        x = "",
        y = "DAP (cm)" + scale_fill_brewer(palette = "Paired"))
###############
ggplot(dados6, aes(parcela, dap, fill=parcela)) +
   #geom_bar(stat = "identity",size=3) +
   geom_bar(stat = "summary", fun = "mean",size=2) + # , fill="white"
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
   stat_summary(fun.data = mean_se, geom = "errorbar") +
   #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
   labs(title = "Média do DAP por parcela",
        x = "", y = "DAP (cm)")+
   scale_fill_brewer(palette = "Paired")

dados6<-dados6[-161,] # excluir a linha 161


##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.6.jpeg")
graf.dap.parc.6.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.6.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados6, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(parcela, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 6")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados6, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 360 m²)") + ylab("IMA DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade


###############################################################################
################ ANALISE PARCELA 7 #################################
###############################################################################
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise")
getwd()

ls()
rm(list=ls())
options(digits = 2)
dados7<- read.xlsx("per_antigos.xlsx", sheet=6)
is.numeric(dados7$alt)
dados7$alt<-as.numeric(dados7$alt)
dados7$IMAalt<-as.numeric(dados7$IMAalt)
is.numeric(dados7$dap)
dados7$dap<-as.numeric(dados7$dap)
#dados7$IMAdap<-as.numeric(dados7$IMAdap)

#Ordenando pelo idade_anos
#chicago <- arrange(chicago, date)
#head(select(chicago, date, pm25tmean2), 3)

## Ordenando pela Idade
dados7<-arrange(dados7,idade_anos)
head(select(dados7, idade_anos,alt), 40)
tail(select(dados7, idade_anos,alt), 20)
############################################
#Checando os valores
max(dados7$alt, na.rm=TRUE);min(dados7$alt, na.rm=TRUE)
max(dados7$dap, na.rm=TRUE)
min(dados7$dap, na.rm=TRUE)
max(dados7$idade_anos, na.rm=TRUE);min(dados7$idade_anos, na.rm=TRUE)
############################################
#Criando variavel IMAalt
is.numeric(dados7$idade_anos)
#dados7b$idade_anos<-as.numeric(dados7b$idade_anos)
dados7 <- mutate(dados7,
                 IMAalt = (alt/idade_anos))

#Criando variavel IMAdap
is.numeric(dados7$idade_anos)

#dados48b$idade_anos<-as.numeric(dados48b$idade_anos)
dados7 <- mutate(dados7,
                 IMAdap = (dap/idade_anos))


edit(dados7)

# media de altura e dap ##TA DANDO CERTO

#Agrupado  ALTURA  por ano e parcela
media.alt<-group_by(dados7, idade_anos,parcela) %>%
   summarize(alt.med=(mean(alt,na.rm=TRUE)),
             alt.sd=(sd(alt,na.rm=TRUE)))
###Outra opção, criando a variavel no data frame
#chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
dados7<-mutate(dados7, alt.med=(mean(alt, na.rm=TRUE)),
               alt.sd=(sd(alt, na.rm=TRUE)))
################
media.alt  # é uma lista
is.list(media.alt)
ls(media.alt)
dim(media.alt)

## Agrupando imaALT  por ano e parcela
media.IMAalt<-group_by(dados7, idade_anos,parcela) %>%
   summarize(IMAalt.med=(mean(IMAalt, na.rm=TRUE)))
media.IMAalt

# Agrupando imadap  por ano e parcela
media.IMAdap<-group_by(dados7, parcela) %>%
   summarize(IMAdap.med=(mean(IMAdap, na.rm=TRUE)))
media.IMAdap


##################

## Agrupando dap por ano e parcela

media.dap<-group_by(dados7, parcela) %>%
   summarize(dap.med=(mean(dap, na.rm=TRUE)),
             dap.sd=(sd(dap,na.rm=TRUE)))
media.dap
max(media.dap[3], na.rm=TRUE)
min(media.dap[3],na.rm=TRUE)
############################### deu muito certo
is.list(media.alt)
media.alt[[1]]
media.alt[[2]]
media.alt[[3]]
media.alt.df<-as.data.frame(media.alt)
is.data.frame(media.alt)

##DAP
is.list(media.dap)
names(media.dap)
media.dap[[1]]
media.dap[[2]]
min(media.dap[[2]])
max(media.dap[[2]])
media.dap[[3]]
media.dap.df<-as.data.frame(media.dap)
names(media.dap.df)
is.data.frame(media.dap.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\7")
write.xlsx(media.dap.df, file=c("media.dap.7.xlsx"))


#Concatenando alt e dap
list.alt.dap<-c(media.alt,media.dap[3],media.dap[4])

alt.dap.7.df<-as.data.frame(list.alt.dap)
edit(alt.dap.7.df)
setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\7")
write.xlsx(alt.dap.7.df, file=c("alt.dap.7.xlsx"))


####### GRAFICOS  ##################
GGplot

##Grafico Altura média #Esse ja esta bom


setwd("C:\\Users\\rober\\OneDrive\\Disco-2021\\Banco_dados_M_Transm\\Dados-analise\\results\\7")
#jpeg("graf.alt.7.jpeg")
graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
   geom_point()  +   theme_bw()+         # theme_classic() + 
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   scale_x_continuous(breaks=seq(0.5,3.0,0.2), limits=c(0.5,3.0))+
   scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   ggtitle("Altura total média - Per. 7")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)
#dev.off()

##Outro Altura com as parcelas coloridos
#graf.alt<-   ggplot(media.alt, aes(idade_anos, alt.med)) +
#geom_point(aes(shape = parcela, color = parcela))   +   theme_bw()+        #                              # theme_classic() +
geom_point() +   theme_bw() +  
   xlab("Idade (anos)") + ylab("Altura total (m)") +
   #scale_y_continuous(breaks=seq(0,3,0.5), expand=c(0,0),limits=c(0,3)) +
   #scale_x_continuous(breaks=seq(1.5,2.5,0.2), limits=c(1.5,2.5))+                  
   ggtitle("Altura Média - Per. 7")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.alt)

## Grafico por Parcela
#jpeg("graf.alt.parcela.7.jpeg")
graf.alt.parc <- graf.alt + facet_wrap(parcela ~ .)+ ### FICOU Muito  BOM PODE MELHORAR
   geom_jitter(position = position_jitter(width = 0.1, height = 0.1))+
   geom_smooth(method = "loess", se = FALSE)

plot(graf.alt.parc)
#dev.off

##Grafico por parcela colorido
#jpeg("graf.alt.parcela.color.7.jpeg")
graf.alt + geom_point(aes(colour = factor(parcela)))
# geom_jitter(position = position_jitter(width = 0.02, height = 0.02))
#dev.off

##Grafico Altura por idade
graf.alt.idade <- graf.alt + facet_wrap(idade_anos ~ .)+
   #geom_jitter(position = position_jitter(width = 0.5, height = 0.5))+
   geom_point(aes(colour=factor(parcela)))
plot(graf.alt.idade)

##Boxplot parcela vs altura total
#Ver que o boxplot pega todos os dados nao a media das parcelas
#shade <- if_else(dados7$idade_anos >= 3, "blue", "gray")
alt.parc.box <- ggplot(dados7, aes(x=factor(parcela),y=alt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #geom_boxplot(aes(fill = parcela)) +
   #geom_jitter(position = position_jitter(width = 0.02, height = 0.02))+
   #geom_point(color =shade)+
   xlab("Parcela (40 árvores, 370 m²)") + ylab("IMA Altura (m)")  
plot(alt.parc.box)
######
ggplot(birthwt, aes(x = factor(race), y = bwt)) + geom_boxplot() +
   stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")
# fun.y esta deprecated, use "fun". 
######

## Grafico IMA alt
graf.IMAalt<- ggplot(media.IMAalt, aes(idade_anos, IMAalt.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,1,0.2), expand=c(0,0),limits=c(0,1)) +
   scale_x_continuous(breaks=seq(0,3.5,0.2), limits=c(0,3.5))+ 
   xlab("Idade (anos)") + ylab("IMA - Altura total (m)") +
   ggtitle("IMA Altura total média- Per. 7")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAalt)

##Boxplot parcela vs IMA altura total
#shade <- if_else(dados7$idade_anos >= 2, "blue", "black")
IMAalt.parc.box <- ggplot(dados7, aes(x=factor(parcela), y=IMAalt)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
   #geom_boxplot(aes(fill = parcela)) +
   # geom_point(color =shade)+
   xlab("Parcela (40 árvores, 370 m²)") + ylab("IMA Altura (m)")  
plot(IMAalt.parc.box)

## Grafico DAP
graf.dap<-   ggplot(media.dap, aes(x=parcela,y=dap.med)) +
   geom_point() +   theme_bw()+    # theme_classic() +
   xlab("Idade (anos)") + ylab("DAP (cm)") +
   #scale_x_continuous(breaks=seq(1.5,3.0,0.2), limits=c(1.5,3.0))+
   ggtitle("Média do DAP - Perímetro 7")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "lm", se = TRUE) #se=banda de confiança
plot(graf.dap)

##Bar plot #  
ggplot(data = dados7, aes(parcela, dap, fill = parcela)) +
   #geom_bar(stat = "summary", fun = "mean") +
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", na.rm=TRUE)+
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
   labs(title = "Media do DAP por parcela",
        x = "",
        y = "DAP (cm)" + scale_fill_brewer(palette = "Paired"))
###############
ggplot(dados7, aes(parcela, dap, fill=parcela)) +
   #geom_bar(stat = "identity",size=3) +
   geom_bar(stat = "summary", fun = "mean",size=2) + # , fill="white"
   #scale_y_continuous(breaks=seq(0,12,1), expand=c(0,0),limits=c(0,12)) +
   stat_summary(fun.data = mean_se, geom = "errorbar") +
   #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
   labs(title = "Média do DAP por parcela",
        x = "", y = "DAP (cm)")+
   scale_fill_brewer(palette = "Paired")

dados7<-dados7[-171,] # excluir a linha 171


##Grafico por parcela colorido
#jpeg("graf.dap.parcela.color.7.jpeg")
graf.dap.parc.7.color<-graf.dap + geom_point(aes(colour = factor(parcela)))
plot(graf.dap.parc.7.color)
#dev.off

##Boxplot parcela vs dap
dap.parc.box <- ggplot(dados7, aes(factor(parcela), y=dap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 370 m²)") + ylab("DAP (cm)")  
plot(dap.parc.box)

## Grafico IMA dap
graf.IMAdap<- ggplot(media.IMAdap, aes(parcela, IMAdap.med)) +
   geom_point() +   theme_bw()+       # theme_classic() +
   #geom_point(aes(shape = parcela, color = parcela)) +   theme_bw()+ 
   geom_jitter(position = position_jitter(width = 0.05, height = 0.05))+
   # scale_y_continuous(breaks=seq(0,3,0.3), limits=c(0,3)) +
   scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12))+ 
   xlab("Idade (anos)") + ylab("IMA - DAP (cm)") +
   ggtitle("IMA DAP - Per. 7")+  theme(plot.title = element_text(hjust = 0.5))+
   geom_smooth(method = "loess", se = TRUE) #se=banda de confiança
plot(graf.IMAdap)

##Boxplot parcela vs IMAdap
IMAdap.parc.box <- ggplot(dados7, aes(factor(parcela), y=IMAdap)) + geom_boxplot(colour="blue")+ ## ficoubom
   stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+ 
   xlab("Parcela (40 árvores, 370 m²)") + ylab("IMA DAP (cm)")  
plot(IMAdap.parc.box)
#OBS: Precisa melhorar o boxplot por idade


















































































































































































































































































































































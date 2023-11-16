## ----
## Estudo descritivos 
## ----

## Dados: Taxa de beneficiários do bolsa família

## Bibliotecas

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(ggplot2,dplyr,geobr, ggplot2, sf,rio,readr)

## Leitura do banco de dados

setwd("C:/Users/dioni/OneDrive - University of São Paulo/Doutorado em Estatística/2023.2/2 - Aprendizado de Máquina Estatístico/Codigos_Pratica/3_Trabalho_Final/dados")

bf = read.csv("bolsafamilia_tx.csv")
head(bf)
dim(bf)

## ---
## Criação de um mapa 
## ---


## analise pelos municipios

brasil_muni <- read_municipality(code_muni = "all",year=2010) %>% 
  select(-c("code_state"))

codigos_abrev <- substr(brasil_muni$code_muni, 1, nchar(brasil_muni$code_muni) - 1)

codigos_full = cbind(as.numeric(brasil_muni$code_muni), as.numeric(codigos_abrev))
colnames(codigos_full) = c("cod_ibge_muni", "shortibge")
codigos_full=as.data.frame(codigos_full)

bf = left_join(bf,codigos_full, by = c("codigo_ibge"="shortibge"))

## adicionando ao objeto geom
dfmapa = left_join(brasil_muni,bf[,c("cod_ibge_muni", "Município","tx_benbf")], by = c("code_muni"="cod_ibge_muni"))

#st_as_sf(dfmapa, coords = "geometry")

ggplot() +
  geom_sf(data=dfmapa , aes(fill=tx_benbf), color= NA, size=.15)+
  labs(title="",
       size=8)+
  scale_fill_distiller(palette = "RdPu", limits=c(0.5, 0.8),
                       name="Rate of beneficiaries",direction = 1)+
  theme_minimal() 

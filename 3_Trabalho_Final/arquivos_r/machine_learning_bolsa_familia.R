## ----
## Estudo sobre métodos 
## ensemble sobre a Regressão Beta
## ----

## Dados: Taxa de beneficiários do bolsa família, em 2015.

## Bibliotecas

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(ggplot2,dplyr, tidyverse,
       caret,   # for general data preparation and model fitting
       e1071) # for fitting the xgboost model

## Leitura do banco de dados

setwd("C:/Users/dioni/OneDrive - University of São Paulo/Doutorado em Estatística/2023.2/2 - Aprendizado de Máquina Estatístico/Codigos_Pratica/3_Trabalho_Final/dados")

bf = read.csv("bolsafamilia_tx.csv")
head(bf)

## retirar a coluna indice
bf = bf %>% subset(select=-c(X))

## ---
## Separação das covariaveis e da resposta
## ---

head(bf)

y = bf %>% subset(select=c(tx_benbf))
X = bf %>% subset(select=c(FECTOT, RAZDEP,
                           E_ANOSESTUDO, T_ANALF18M,
                           T_FBBAS, T_FBFUND,
                           T_FBMED, T_FBSUPER,
                           GINI, PIND, PPOB, RDPCT,
                           THEIL, P_FORMAL, T_BANAGUA, T_DENS,
                           T_LIXO, T_LUZ,AGUA_ESGOTO,
                           PAREDE, T_M10A14CF, T_M15A17CF,
                           I_ESCOLARIDADE,  IDHM, IDHM_L, IDHM_R,
                           tx_ocupacao_urbana, capital))

databf = cbind(X,y)
dim(databf)

## Separação entre treino e teste

set.seed(10)
# Proporção treino
prop_treino = 0.8
n_treino = round(nrow(databf) * prop_treino)

ind_treino = sample(1:nrow(databf), n_treino)

# Criar conjuntos de treino e teste 
treinobf = databf[ind_treino, ]
testebf = databf[-ind_treino, ]

dim(treinobf);dim(testebf)


## -------
## Machine Learning Regression Models
## -------

## ----
## Máquinas de Vetores de Suporte (SVM)
## ----


## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, tx_benbf~.,
                 data = treinobf,
                 ranges=list(epsilon=seq(0,1,0.1),
                             cost=1:10, kernel=c("linear", "polynomial",
                                                  "radial", "sigmoid"),
                             gamma=log(seq(1,10,0.9))),
                 tunecontrol = tune.control(cross = 10))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

## ---
## Random Forest Regressor (RFR)
## ---

#Tune the Random Forest model
install.packages("randomForest")

OptModelRF=tune(randomForest, tx_benbf~.,
                 data = treinobf,
                 ranges=list(ntree = seq(100,1000,100),
                             mtry = seq(5,25,5),
                             maxnodes = seq(5,25,5)),
                 tunecontrol = tune.control(cross = 10))

#Print optimum value of parameters
print(OptModelRF)

#Plot the perfrormance of SVM Regression model
plot(OptModelRF)





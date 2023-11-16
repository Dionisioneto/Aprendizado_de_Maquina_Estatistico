## ----
## Estudo sobre métodos 
## ensemble sobre a Regressão Beta
## ----

## Dados: Taxa de beneficiários do bolsa família, em 2015.

## Bibliotecas

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(ggplot2,dplyr, tidyverse,
       caret,   # for general data preparation and model fitting
       xgboost) # for fitting the xgboost model

## Leitura do banco de dados

setwd("C:/Users/dioni/OneDrive - University of São Paulo/Doutorado em Estatística/2023.2/2 - Aprendizado de Máquina Estatístico/3_Trabalho_Final")

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

## ---
## XGBoost
## ---

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation

## Especificando a tecnica de validação cruzada que será passada no treino
## futuramente e o paramtro k para o k-fold CV.

train_control = trainControl(method = "cv", number = 10,
                             search = "grid")

set.seed(50)

# Customização do grid

gbmGrid =  expand.grid(max_depth = c(3, 5, 7), 
                        nrounds = (1:10)*50,    # number of trees
                        # default values below
                        eta = 0.3,
                        gamma = 0,
                        subsample = 1,
                        min_child_weight = 1,
                        colsample_bytree = 0.6)

# training a XGboost Regression tree model while tuning parameters
model = train(tx_benbf~., data = treinobf, method = "xgbTree",
              trControl = train_control, tuneGrid = gbmGrid)

# summarising the results
print(model)

## ---
## Usar a melhor configuração xgboost nos dados de teste
## ---

#use model to make predictions on test data
predxgb = predict(model, testebf)

# performance metrics on the test data
test_y = testebf['tx_benbf']

mse = sum((test_y - predxgb)^2)/length(predxgb) #mse - Mean Squared Error
mse

sqrt(mse) #rmse - Root Mean Squared Error


## ---
## Support Vector Machines (SVM)
## ---

# Setup for cross validation
ctrl <- trainControl(method="cv",   # 10 fold cross validation
                     number = 10,
                     search = "grid")

tuneGridsvm = expand.grid(
  C = c(0.25, .5, 1),
  sigma = 0.1
)

svm.tune = train(tx_benbf~., data = treinobf,
                  method = "svmRadial",   # Linear kernel, tentar svmRadial
                  tuneLength = 5,         # 5 values of the cost function
                  metric="RMSE",
                 trControl=ctrl,
                 tuneGrid = tuneGridsvm)

svm.tune

plot(svm.tune)


## Uma outra abortagem para o tunning do svm
#install.packages("e1071")
library(e1071)

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, tx_benbf~.,
                 data = treinobf,
                 ranges=list(elsilon=seq(0,1,0.1),
                             cost=1:10, kernel=c("linear", "polynomial",
                                                  "radial", "sigmoid")))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

## ---
## Random Forest Regressor (RFR)
## ---







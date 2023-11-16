## ----
## Estudo sobre métodos 
## ensemble sobre a Regressão Beta
## ----

## Dados: Taxa de beneficiários do bolsa família, em 2015.

## Bibliotecas

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(ggplot2,dplyr, betareg, betaboost,e1071,caret,compiler)


## Leitura do banco de dados

#setwd("C:/Users/dioni/OneDrive - University of São Paulo/Doutorado em Estatística/2023.2/2 - Aprendizado de Máquina Estatístico/3_Trabalho_Final")

bf = read.csv("https://raw.githubusercontent.com/Dionisioneto/Aprendizado_de_Maquina_Estatistico/main/3_Trabalho_Final/dados/bolsafamilia_tx.csv")
head(bf)

## retirar a coluna indice
bf = bf %>% subset(select=-c(X))

## ---
## Separação das covariaveis e da resposta
## ---

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

## Realizando um pequena mudança na resposta
## para que o velopr não seja 1
## 1 munícipios teve todos os seus candidatos escolhidos

y[y>0.99999] = 0.9999

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
## Modelo de Regressão Beta
## -------
#library(gamlss)

breg = betareg(tx_benbf ~.,
                data=treinobf)

#summary(breg)

ypredbreg = predict(breg,testebf)

MSE = function(ypred,ytrue){sum((ypred - ytrue)^2)/length(ytrue)}

msebeta = MSE(ypred=ypredbreg,ytrue=testebf$tx_benbf)
msebeta

sqrt(msebeta)


## Validação Cruzada (k-fold cross validation)
## para a regressão beta

## O elemento de variação é a função de ligação

linksfun = c("logit", "probit", "cloglog", "cauchit", "log", "loglog")

# for (link in linksfun) {
#   
#   breg = betareg(tx_benbf ~.,
#                  data=treinobf,
#                  link=link)
#   
#   ypredbreg = predict(breg,testebf)
#   
#   msebeta = MSE(ypred=ypredbreg,ytrue=testebf$tx_benbf)
#   msebeta
#   
# }

set.seed(10)
kfolds = 10

tcv_data = treinobf %>% 
  mutate(fold = sample(1:kfolds, size=dim(treinobf)[1], replace=T))

cv_err = rep(0, kfolds)
matrix_cv = matrix(data=0,nrow=length(linksfun),
                   ncol = kfolds,
                   dimnames = list(linksfun))

enableJIT(3)
for(link in 1:length(linksfun)){
  for(i in 1:kfolds){
    t_data = filter(tcv_data, fold!=i)
    v_data = filter(tcv_data, fold==i)
    
    fitbeta = betareg(tx_benbf ~.,
                   data=t_data,
                   link = linksfun[link])
    
    predsbeta = predict(fitbeta, newdata=v_data)
    
    err = v_data$tx_benbf - predsbeta 
    mse = mean(err^2)
    
    # Record the RMSE
    matrix_cv[link,i] <- sqrt(mse)
  }
}

matrix_cv

## as medias da validação cruzada

colMeans(matrix_cv)
linkmin=which(colMeans(matrix_cv)==min(colMeans(matrix_cv)))

matrix_cv ## ligação log-log é a que retorno o menor RMSE

## ----
## Iremos treinar o modelo final com o loglog
## ----

modbeta = betareg(tx_benbf ~.,
            data=treinobf,
              link = "loglog")

ypbreg = predict(modbeta,testebf)

msebetareg = mean((testebf$tx_benbf - ypbreg)^2)
msebetareg ## EQM

## RMSE
sqrt(msebetareg)

## MAE
mean(abs(testebf$tx_benbf - ypbreg))

R2 = function(y,ypred){
  num = sum((y-ypred)^2)
  dem =  sum((y-mean(y))^2)
  r = 1 - (num/dem)
  return(r)
}

## R2: Coeficiente de Determinação
R2(y=testebf$tx_benbf,ypred=ypbreg) 
  
## ----
## Algoritmo de Estimação Bagging
## ----

## ----
## Algoritmo de Bagging para a Regressão Beta
## ----

bagging_betareg = function(xtreino,ytreino,xteste,n_estimadores,n_amostra,linkfun){
  
  ## predicoes do bagging em uma matriz
  matriz_bag = matrix(data=0,nrow=dim(xteste)[1],ncol=n_estimadores)
  
  enableJIT(3)
  for(preditor in 1:n_estimadores){
    random_id = sample(1:dim(xtreino)[1],replace=T)
    
    xtreino_bag = xtreino[random_id,]
    ytreino_bag = ytreino[random_id]
    
    dados_bag = cbind(ytreino_bag,xtreino_bag)
    colnames(dados_bag)[1] = "y"
    
    reg_bag = betareg(y ~ .,
                      data=dados_bag,
                      link=linkfun)
    
    y_pred_bag = predict(reg_bag, newdata=xteste)
    matriz_bag[,preditor] = y_pred_bag
  }
  
  predicoes_bag = rowSums(matriz_bag)/n_estimadores
  return(predicoes_bag)
}

y_pred_bagging= bagging_betareg(xtreino=treinobf[, -which(names(treinobf) == "tx_benbf")],
                                ytreino=treinobf[, "tx_benbf"],
                                xteste=testebf[, -which(names(testebf) == "tx_benbf")],
                                n_estimadores=100,
                                n_amostra=dim(treinobf)[1],
                                linkfun = "logit")


## ---
## Realizando o processo de Grid Search
## ---

set.seed(12)

b_values = c(100,200)
linksfun

kfolds = 10

tcv_bag = treinobf %>% 
  mutate(fold = sample(1:kfolds, size=dim(treinobf)[1], replace=T))


array_cv_bag = array(0, dim = c(length(linksfun), kfolds,length(b_values)))

enableJIT(3)
for (b in 1:length(b_values)){
  for(link in 1:length(linksfun)){
    for(i in 1:kfolds){
      t_data = filter(tcv_data, fold!=i)
      v_data = filter(tcv_data, fold==i)
      
      predsbetabag = bagging_betareg(xtreino=t_data[, -which(names(t_data) == "tx_benbf")],
                                   ytreino=t_data[, "tx_benbf"],
                                   xteste=v_data[, -which(names(v_data) == "tx_benbf")],
                                   n_estimadores=b_values[b],
                                   n_amostra=dim(t_data)[1],
                                   linkfun = linksfun[link])
      
      errbag = v_data$tx_benbf - predsbetabag 
      msebag = mean(errbag^2)
      
      # Record the RMSE
      array_cv_bag[link,i,b] <- sqrt(mse)
    }
  }
}





## Calculando o EQM

msebetabag1 = MSE(ypred=y_pred_bagging,ytrue=testebf$tx_benbf)
msebetabag1

sqrt(msebetabag1)



## ----
## Algoritmo de Estimação Random Forest
## ----


rf_betareg = function(xtreino,ytreino,xteste,
                           n_estimadores,n_amostra,n_features){
  
  ## predicoes do Random Forest em uma matriz
  matriz_rf = matrix(data=0,nrow=dim(xteste)[1],ncol=n_estimadores)
  
  enableJIT(3)
  for(preditor in 1:n_estimadores){
    random_id = sample(1:dim(xtreino)[1],replace=T)
    random_idf = sample(1:dim(xtreino)[2],replace=F)[1:n_features]
    
    xtreino_rf = xtreino[random_id,random_idf]
    ytreino_rf = ytreino[random_id]
    
    dados_rf = cbind(ytreino_rf,xtreino_rf)
    colnames(dados_rf)[1] = "y"
    
    reg_rf = betareg(y ~ .,
                      data=dados_rf,
                      link='logit')
    
    y_pred_rf = predict(reg_rf, newdata=xteste)
    matriz_rf[,preditor] = y_pred_rf
  }
  
  predicoes_rf = rowSums(matriz_rf)/n_estimadores
  return(predicoes_rf)
}


y_pred_rf1 = rf_betareg(xtreino=treinobf[, -which(names(treinobf) == "tx_benbf")],
                        ytreino=treinobf[, "tx_benbf"],
                        xteste=testebf[, -which(names(testebf) == "tx_benbf")],
                        n_estimadores=100,
                        n_amostra=dim(treinobf)[1],
                        n_features=10)


msebetarf1 = MSE(ypred=y_pred_rf,ytrue=testebf$tx_benbf)
msebetarf1

sqrt(msebetarf1)


## ----
## Algoritmo de Boosing para a Regressão Beta
## Gradiente Boosting
## ----

## biblioteca betaboost implementa
## tem que puxar do github

library("devtools")

#install_github("boost-R/betaboost")
library("betaboost")


betaboosting = betaboost(tx_benbf ~.,
                data=treinobf)

summary(betaboosting)

y_predbetaboost1 = predict(betaboosting,testebf)

msebetaboost1 = MSE(ypred=y_predbetaboost1,ytrue=testebf$tx_benbf)
msebetaboost1

sqrt(msebetaboost1)


## ---
## Implementação do algoritmo de Adaboosting
## ---












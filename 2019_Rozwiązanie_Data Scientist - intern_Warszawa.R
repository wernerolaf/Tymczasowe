set.seed(777)
library(mlr)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(DALEX)
library(dataMaid)
library(vtreat)

mozliwosci<-mlr::listLearners(check.packages = TRUE)
mozliwosci<-mozliwosci[mozliwosci$type=="regr",]

xd<-readxl::read_excel("Data Scientist - Intern - zadanie.xlsx",sheet = 1)
dane<-readxl::read_excel("Data Scientist - Intern - zadanie.xlsx",sheet = 2)

#dataMaid::makeDataReport(dane)

#czyszczenie

#usuniecie bezuzytecznych kolumn
todelete<-apply(dane, 2, function(x){all(x==x[1])})
dane <- dane[!todelete ]

#usuniecie unikalnego Modelu (3D White były różnych marek więc są różnymi produktami)
dane<-dplyr::select(dane,-Model)

#zamiana na numeryczne
dane[20:29]<-apply(dane[20:29],2,function(x){ as.numeric(x)})


#normalizacja daty
dane$Data_wystawienia_aukcji<-(as.numeric(dane$Data_wystawienia_aukcji)-min(as.numeric(dane$Data_wystawienia_aukcji)))/(
  max(as.numeric(dane$Data_wystawienia_aukcji))-min(as.numeric(dane$Data_wystawienia_aukcji))
)

#zamiana na factor
dane[sapply(dane, is.character)] <- lapply(dane[sapply(dane, is.character)],as.factor) 

#permutujemy
dane<-sample_frac(dane)
#usuwamy unikalny indeks
dane<-dane[-1]
#zamiana na numeryczne
dane$top3_sales_30m<-as.numeric(as.character(dane$top3_sales_30m))

#one hot encode
dane_org<-dane
dane<-mlr::createDummyFeatures(dane)
#poprawiamy nazwy kolumn
colnames(dane)<-make.names(colnames(dane),allow_ = FALSE)
names(dane) <- iconv(names(dane), to='ASCII', sub='')

#nowe kolumny
dane<-dplyr::mutate(dane,stosunekCeny=cena.wysylka30A/cena.z.wysylka30m)


#podzielenie zbioru na brakujace
dane %>% filter_all(any_vars(is.na(.))) ->braki

dane<-tidyr::drop_na(dane)

#ocena waznosci kolumn

treat<-vtreat::designTreatmentsN(dane,varlist = colnames(dane)[-1],outcomename = "Sztuk.sprzedanych.pierwsze.30.dni")
dane<-vtreat::prepare(treat,dane,pruneSig=1/10)


#detekcja outlajerow
OutVals = boxplot(dane$Sztuk.sprzedanych.pierwsze.30.dni,plot = FALSE)$out
odstajace<-which(dane$Sztuk.sprzedanych.pierwsze.30.dni %in% OutVals)
dane_odstajace<-dane[odstajace,]
dane_nieodstajace<-dane[-odstajace,]

#DataExplorer::plot_histogram(dane )
#DataExplorer::plot_density(dane)
#DataExplorer::create_report(dane)
#DataExplorer::plot_qq(dane)


#Tworzenie modelu dla danych nieodstajacych

regr_task = makeRegrTask(id = "task", data = dane_nieodstajace, target = "Sztuk.sprzedanych.pierwsze.30.dni")
regr_lrn = makeLearner("regr.ranger")

#:# audit
cv <- makeResampleDesc("CV", iters = 5)
r1 <- resample(regr_lrn, regr_task, cv, measures = list(mse, rmse, mae, rsq))
r1$aggr


custom_predict <- function(object, newdata) {
  pred <- predict(object, newdata=newdata)
  response <- pred$data$response
  return(response)
}

#podzial na testowy i sprawdzajacy
train_index <- sample(1:nrow(dane_nieodstajace), 0.8 * nrow(dane_nieodstajace))
train <- dane_nieodstajace[train_index,]
test <- dane_nieodstajace[-train_index,]

regr_task = makeRegrTask(id = "task", data = train, target = "Sztuk.sprzedanych.pierwsze.30.dni")

#wyjasnianie działania modelu 
learner<-train(regr_lrn,task = regr_task)
explainer<-DALEX::explain(learner,data = test,y=test$Sztuk.sprzedanych.pierwsze.30.dni,predict_function = custom_predict)
plot(DALEX::model_performance(explainer))
plot(DALEX::model_performance(explainer),geom = "boxplot")
plot(DALEX::variable_importance(explainer))

#moja własna miara jakosci modelu, polega na tym ze za sukces uznajemy predykcja ktora przewidziala sprzedaz +-20%
wzglednie<-(custom_predict(learner,test)-test$Sztuk.sprzedanych.pierwsze.30.dni)/test$Sztuk.sprzedanych.pierwsze.30.dni
sum(abs(wzglednie)<=0.2)/length(wzglednie)

#powody problemow z predykcja 
#powodem problemów z predykcja jest to że sprzedawca dla wiele podobnych produktów dominuje rozna czesc rynku 

dane_org<-dplyr::mutate(dane_org,udzialRynku=dane_org$Sztuk_sprzedanych_pierwsze_30_dni/dane_org$sum_tr_quantity30m)
levels(dane_org$CA_NAME_5)<-iconv(levels(dane_org$CA_NAME_5), to='ASCII', sub='')

ggplot(dane_org,aes(y=udzialRynku,group=CA_NAME_5))+geom_boxplot()+facet_grid(~CA_NAME_5)+theme(axis.text.x.bottom = element_blank())



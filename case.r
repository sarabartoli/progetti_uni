#carico i pacchetti e importo il dataset
library(kknn)
library(rpart)
library(randomForest)
library(Ecdat)
data(Housing)
attach(Housing)

#analisi grafica
x=colnames(Housing)[-1]
par(mfrow=n2mfrow(length(x)))
for(v in x){
	plot(Housing[,v],Housing$price,xlab=v,ylab='price')
    if(is.numeric(Housing[,v])) abline(lm(Housing$price~Housing[,v]))
}
par(mfrow=c(1,1)) #grafici da cambiare

#funzione calcolo mse, rmse, mae, mape
calcolo_errori=function(y_oss,y_prev){
    mse=mean((y_oss-y_prev)^2)
    rmse=sqrt(mse)
    mae=mean(abs(y_oss-y_prev))
    mape=100*mean(abs((y_oss-y_prev)/y_oss))
	return(c(mse,rmse,mae,mape))
}

#funzione per la validazione 10 fold e leave one out
formula=price~lotsize+bedrooms+bathrms+stories+garagepl
formula_log=log(price)~lotsize+bedrooms+bathrms+stories+garagepl
validazione=function(dati,k,m,f=formula,f_log=formula_log){
    arr_MSE=array(dim=c(m,6,4))
    dimnames(arr_MSE)=list(NULL,c('Regressione lineare','Regressione log-lineare media','Regressione log-lineare mediana','Knn','Alberi','Foreste'),c('MSE','RMSE','MAE','MAPE'))
    for (w in 1:m){
        indici_fold=cut(sample(1:nrow(dati),nrow(dati)),breaks=k,labels=F)
        mat_y_prev=matrix(nrow=nrow(dati),ncol=6)
        for(i in 1:k){
            #identifico train set e test set
            indici_train_set=which(indici_fold!=i)
            indici_test_set=which(indici_fold==i)
            train_set=dati[indici_train_set,]
            test_set=dati[indici_test_set,]
            reg_lineare=lm(f,data=train_set) #stima regressione lineare sul train set
            mat_y_prev[indici_test_set,1]=predict(reg_lineare,test_set) #previsione sul test set (regressione lineare)           
            reg_log_lineare=lm(f_log,data=train_set) #stima regressione log-lineare sul train set
            pred_rlog=predict(reg_log_lineare,test_set)
            s2=summary(reg_log_lineare)$sigma^2
            mat_y_prev[indici_test_set,2]=exp(pred_rlog+s2/2) #previsione sul test set (regressione log-lineare media)
            mat_y_prev[indici_test_set,3]=exp(pred_rlog) #previsione sul test set (regressione log-lineare mediana)
            mat_y_prev[indici_test_set,4]=kknn(f,train=train_set,test=test_set,k=round(sqrt(nrow(train_set))))$fitted #algoritmo knn
            alberi=rpart(f,data=train_set) #stima regola predittiva alberi sul train set
            mat_y_prev[indici_test_set,5]=predict(alberi,test_set) #previsione sul test set (alberi)
            foreste=randomForest(f,data=train_set) #stima regola predittiva foreste sul train set
            mat_y_prev[indici_test_set,6]=predict(foreste,test_set) #previsione sul test set (foreste)
        }
        for(j in 1:ncol(mat_y_prev)) arr_MSE[w,j,]=calcolo_errori(dati$price,mat_y_prev[,j])
    }
    return(list((apply(arr_MSE,c(2,3),mean)),mat_y_prev))
}

validazione(Housing,k=10,m=30)[[1]] #validazione del modello con 10-fold
l=validazione(Housing,k=nrow(Housing),m=1) #validazione del modello con leave one out
print(l[[1]])
print(paste('Varianza prezzi case:',var(price)))
print(paste('SD prezzi case:',sd(price)))

#grafico osservati contro previsti
plot(Housing$price,l[[2]][,1],col='black',xlab='Valori osservati',ylab='Valori previsti')
abline(0,1)
points(Housing$price,l[[2]][,2],col='navy')
points(Housing$price,l[[2]][,3],col='red')
points(Housing$price,l[[2]][,4],col='green')
points(Housing$price,l[[2]][,5],col='pink')
points(Housing$price,l[[2]][,6],col='yellow')
legend('topleft',c('Regressione lineare','Regressione log-lineare media','Regressione log-lineare mediana','Knn','Alberi','Foreste'),lty=1,col=c('black','navy','red','green','pink','yellow'),bty='n')

#stimo regressione lineare con funzione score esponenziale (media) su tutto il campione
reg_lineare=lm(formula,data=Housing)

#Previsione su nuove case
case=data.frame(t(apply(Housing[,c(2,3,4,5,11)],2,median)))
case=case[rep(1,7),]
row.names(case)=c('SuperficieMed PianiMed','SuperficieBasso PianiMed','SuperficieMed PianiBasso','SuperficieAlto PianiMed','SuperficieMed PianiAlto','SuperficieBasso PianiBasso','SuperficieAlto PianiAlto')
case$lotsize[c(2,6)]=quantile(Housing$lotsize,prob=0.05)
case$lotsize[c(4,7)]=quantile(Housing$lotsize,prob=0.95)
case$stories[c(3,6)]=quantile(Housing$stories,prob=0.05)
case$stories[c(5,7)]=quantile(Housing$stories,prob=0.95)
sort(predict(reg_lineare,case))

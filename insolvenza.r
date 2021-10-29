#carico i pacchetti e importo il dataset
library(MASS)
library(kknn)
library(rpart)
library(randomForest)
library(pROC)
dati=read.csv('~/Statistica per l\'economia e l\'impresa/Progetto/defaultProgetto.csv',sep=';')

#creo il campione
set.seed(7004524)
nInsolventi=sum(dati$default) #numero aziende insolventi
idSane=sample(which(dati$default==0),nInsolventi*2) #id aziende sane nel campione
campione=rbind(dati[dati$default==1,],dati[idSane,]) #data frame del campione
attach(campione)

#calcolo gli indici
campione$rigid=IMMOBILIZZAZIONI/TotaleAttivo
campione$cr=(SCORTE+CREDITI_AF+LIQUIDITA)/PASSIVO_CORRENTE
campione$qr=(CREDITI_AF+LIQUIDITA)/PASSIVO_CORRENTE
campione$beta=(PASSIVO_CORRENTE+PASSIVO_CONSOLIDATO)/CAPITALE_NETTO
campione$incidenzaDebiti=(PASSIVO_CORRENTE+PASSIVO_CONSOLIDATO)/TotalePassivo
campione$coperturaOf=(UTILE+ONERI_FINANZIARI+ONERI_TRIBUTARI)/(ONERI_FINANZIARI+ONERI_TRIBUTARI)
campione$gamma=(ONERI_FINANZIARI+ONERI_TRIBUTARI)/(PASSIVO_CORRENTE+PASSIVO_CONSOLIDATO)
campione$roe=UTILE/CAPITALE_NETTO
campione$roa=REDDITO_OPERATIVO/TotaleAttivo
campione$roi=REDDITO_OPERATIVO/CAPITALE_NETTO
campione$ros=REDDITO_OPERATIVO/FATTURATO
campione$rotazioneScorte=FATTURATO/SCORTE
campione$rotazioneAttivoCorrente=FATTURATO/(SCORTE+CREDITI_AF+LIQUIDITA)
campione$rotazioneCapitale=FATTURATO/TotaleAttivo

#analisi grafica indici
par(mfrow=n2mfrow(32-19))
for(i in 19:32){
	boxplot(campione[,i]~default, ylab=names(campione[i]))
}
par(mfrow=c(1,1))

MSE_FNR_FPR=function(y_oss,prob,ottimo){
	curva_roc=suppressMessages(roc(response=y_oss,predictor=prob))
	if(ottimo){
        valori=coords(curva_roc,x='best',best.method='youden',ret=c('accuracy','sensitivity','specificity'),transpose=T)
    }
	else{
        valori=coords(curva_roc,x=0.5,ret=c('accuracy','sensitivity','specificity'),transpose=T)
    }
	if(is.matrix(valori)) return(1-valori[,1])
	return(1-valori)
}

#funzione per la validazione 10 fold e leave one out
formula=default~cr+qr+beta+incidenzaDebiti+coperturaOf+roe+rotazioneAttivoCorrente
formulaF=factor(default)~cr+qr+beta+incidenzaDebiti+coperturaOf+roe+rotazioneAttivoCorrente
validazione=function(dati,k,m,youden,f=formula,ff=formulaF){
    arr_MSE=array(dim=c(m,6,3))
    dimnames(arr_MSE)=list(NULL,c('Regressione logistica','Regressione lineare','Discriminante lineare','Knn','Alberi','Foreste'),c('MSE','Tasso Falsi Negativi','Tasso Falsi Positivi'))
    for (w in 1:m){
        indici_fold=cut(sample(1:nrow(dati),nrow(dati)),breaks=k,labels=F)
        mat_prob=matrix(nrow=nInsolventi*3,ncol=6)
        for(i in 1:k){
            #identifico train set e test set
            indici_train_set=which(indici_fold!=i)
            indici_test_set=which(indici_fold==i)
            train_set=dati[indici_train_set,]
            test_set=dati[indici_test_set,]
            reg_logistica=glm(f,data=train_set,family='binomial') #stima regressione logistica sul train set
            mat_prob[indici_test_set,1]=predict(reg_logistica,test_set,type='response') #previsione sul test set (regressione logistica)
            reg_lineare=lm(f,data=train_set) #stima regressione lineare sul train set
            mat_prob[indici_test_set,2]=predict(reg_lineare,test_set) #previsione sul test set (regressione lineare)
            analisi_discr_lineare=lda(f,data=train_set) #stima discriminante lineare sul train set
            mat_prob[indici_test_set,3]=predict(analisi_discr_lineare,test_set)$posterior[,2] #previsione sul test set (discriminante lineare)
            mat_prob[indici_test_set,4]=kknn(ff,train=train_set,test=test_set,k=round(sqrt(nrow(train_set))))$prob[,2] #algoritmo knn
            alberi=rpart(ff,data=train_set) #stima regola predittiva alberi sul train set            
            mat_prob[indici_test_set,5]=predict(alberi,test_set)[,2] #previsione sul test set (alberi)
            foreste=randomForest(ff,data=train_set) #stima regola predittiva foreste sul train set    
            mat_prob[indici_test_set,6]=predict(foreste,test_set,type='prob')[,2] #previsione sul test set (foreste)
        }
        for(j in 1:ncol(mat_prob)) arr_MSE[w,j,]=MSE_FNR_FPR(dati$default,mat_prob[,j],youden)
    }
    return(list(apply(arr_MSE,c(2,3),mean),mat_prob))
}

validazione(campione,k=10,m=30,youden=F)[[1]] #validazione del modello con 10-fold e massima probabilità di appartenenza
validazione(campione,k=nInsolventi*3,m=1,youden=F)[[1]] #validazione del modello con leave one out e massima probabilità di appartenenza
validazione(campione,k=10,m=30,youden=T)[[1]] #validazione del modello con 10-fold e indice di Youden
l=validazione(campione,k=nInsolventi*3,m=1,youden=T) #validazione del modello con leave one out e indice di Youden
l[[1]]

#curve roc della validazione del modello con leave one out e indice di Youden
roc_reglogistica=roc(response=default,predictor=l[[2]][,1])
roc_reglineare=roc(response=default,predictor=l[[2]][,2])
roc_discrlineare=roc(response=default,predictor=l[[2]][,3])
roc_knn=roc(response=default,predictor=l[[2]][,4])
roc_alberi=roc(response=default,predictor=l[[2]][,5])
roc_foreste=roc(response=default,predictor=l[[2]][,6])
plot.roc(roc_reglogistica,col='navy',print.thres='best',print.thres.best.method='youden',main='Curve roc della validazione del modello con leave one out e indice di Youden')
plot.roc(roc_reglineare,col='red',add=T,print.thres='best',print.thres.best.method='youden')
plot.roc(roc_discrlineare,col='green',add=T,print.thres='best',print.thres.best.method='youden')
plot.roc(roc_knn,col='orange',add=T,print.thres='best',print.thres.best.method='youden')
plot.roc(roc_alberi,col='pink',add=T,print.thres='best',print.thres.best.method='youden')
plot.roc(roc_foreste,col='black',add=T,print.thres='best',print.thres.best.method='youden')
legend('bottomright',c('Regressione logistica','Regressione lineare','Discriminante lineare','Knn','Alberi','Foreste'),lty=1,col=c('navy','red','green','orange','pink','black'))

print(roc_reglogistica$auc)
print(roc_reglineare$auc)
print(roc_discrlineare$auc)
print(roc_knn$auc)
print(roc_alberi$auc)
print(roc_foreste$auc)

#stimo regressione lineare su tutto il campione
reg_logistica=glm(formula,data=campione,family='binomial')

#Previsione su nuove aziende
nuove=data.frame(t(apply(campione[,c(20:24,26,31)],2,median)))
nuove=nuove[rep(1,7),]
row.names(nuove)=c('IncDebMed roeMed','IncDebBasso roeMed','IncDebMed roeBasso','IncDebAlto roeMed','IncDebMed roeAlto','IncDebBasso roeBasso','IncDebAlto roeAlto')
nuove$incidenzaDebiti[c(2,6)]=quantile(campione$incidenzaDebiti,prob=0.05)
nuove$incidenzaDebiti[c(4,7)]=quantile(campione$incidenzaDebiti,prob=0.95)
nuove$roe[c(3,6)]=quantile(campione$roe,prob=0.05)
nuove$roe[c(5,7)]=quantile(campione$roe,prob=0.95)
sort(predict(reg_logistica,nuove,type='response')) #probabilità di insolvenza prevista

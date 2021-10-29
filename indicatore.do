cd "C:\Users\barto\Documents\Statistica sociale\Progetto"

*CREO DATASET
**per creare il dataset in formato stata
import excel "indicatori_gruppo_20.xlsx", sheet("Foglio1") firstrow clear
save "indicatori_gruppo_20.dta",replace

**dataset con dati delle settimane 0-24
use "indicatori_gruppo_1.dta",clear
**
foreach j of numlist 2/14 {
merge 1:1 regioni using "indicatori_gruppo_`j'.dta",nogen
}
destring *, replace
save "indicatori_monitoraggio_settimane 0-24.dta",replace


**dataset con dati delle settimane 25-42
use "indicatori_gruppo_15.dta",clear
**
foreach j of numlist 16/26 {
merge 1:1 regioni using "indicatori_gruppo_`j'.dta",nogen
}
destring *, replace
rename Trend_3_1_28 Trend3_1_28
rename Trend3_4_42 Indic3_4_42
label variable Indic3_4_42 "Indic3_4_42"
rename Trend3_4_43 Indic3_4_43
label variable Indic3_4_43 "Indic3_4_43"
save "indicatori_monitoraggio_settimane 25-42.dta",replace

** 

use "indicatori_monitoraggio_settimane 25-42.dta",clear

*IMPUTAZIONE

misstable summarize
misstable patterns
*in una regione manca Indic3_9_29, in una Indic1_3_34, Indic1_3_35, Indic3_8_26, Indic3_9_26 ->potrebbe essere un problema il fatto che 1.3 manca per due settimane consecutive

tab Indic1_3_34
egen media_indic1_3_34=mean(Indic1_3_34)
replace Indic1_3_34=media_indic1_3_34 if Indic1_3_34==.
*i missing sono stati sostituiti con la media dell'indice nella settimana delle altre regioni perchè i valori sono simili

tab Indic1_3_35
egen media_indic1_3_35=mean(Indic1_3_35)
replace Indic1_3_35=media_indic1_3_35 if Indic1_3_35==.
*stessa cosa di sopra per la settimana successiva

correlate Indic3_1_26 Indic3_2_26 Indic3_4_26 Indic3_5_26 Indic3_6_26	Indic3_8_26 Indic3_9_26
tab Indic3_8_26
reg Indic3_8_26 Indic3_1_26 Indic3_2_26
estimate store tre_otto
predict pred_tre_otto, xb
replace Indic3_8_26=pred_tre_otto if Indic3_8_26==.
*imputo il 3.8_26 con la regressione perchè i valori sono molto diversi da una setttimana all'altra, si usano come variabili esplicative le variabili del gruppo 3 che sono le più simili/correlate, non si usa il trend ma solo l'indicatore 3.1, tolto 3.9 che è missing, lasciati solo quelli concettualmente più simili

tab Indic3_9_26
reg Indic3_9_26 Indic3_1_26	Indic3_2_26 Indic3_6_26
estimate store tre_nove_26
predict pred_tre_nove_26, xb
*replace Indic3_9_26=pred_tre_nove_26 if Indic3_9_26==.
*stessa cosa con il 3.9_26, sempre tolgiendo il 3.8 che è imputato e si propagherebbe l'errore, valore non coerente.
tab Indic3_9_26
gen media_Indic3_9_26=(Indic3_9_25+Indic3_9_27)/2
replace Indic3_9_26=media_Indic3_9_26 if Indic3_9_26==.
*lo imputo con la media

tab Indic3_9_29
reg Indic3_9_29 Indic3_2_29 Indic3_8_29
estimate store tre_nove_29
predict pred_tre_nove_29, xb
replace Indic3_9_29=pred_tre_nove_29 if Indic3_9_29==.
*stessa cosa 3.9_29

* STANDARDIZZAZIONE INDICATORI

merge 1:1 regioni using "popolazione Italia 2020.dta"
drop _merge

encode regioni,gen(regioni_num)
order regioni regioni_num
tab regioni_num, nol
tab regioni_num
*per creare variabil regione numerica con label nome regione

drop *_25
*perchè ci sono indicatori in decimale invece che in % perchè è così nei report

save,replace

*standardizzazione indicatori (riportati per 100 000 abitanti, altrimenti la lombaridia è penalizzata rispetto al molise):
* 3.1 (numero casi riportati negli ultimi 14 gg), 
* 3.5 (numero nuovi focolai),
* 3.6 (Numero di nuovi casi non associati a catene di trasmissione note)
foreach j of numlist 26/43 {
gen Std3_1_`j'=(Indic3_1_`j'/Totale)*100000
gen Std3_5_`j'=(Indic3_5_`j'/Totale)*100000
gen Std3_6_`j'=(Indic3_6_`j'/Totale)*100000
}

summ Std3_*
list regioni Indic3_1_35 Indic3_5_35 Indic3_6_35 Std3_?_35

save,replace

* ANALISI MULTIVARIATA

***ALPHA DI CRONBACH***
cd "C:\Users\barto\Documents\Statistica sociale\Progetto"
use "indicatori_monitoraggio_settimane 25-42.dta", clear
set more off, permanently
set logtype text
log using alpha_cronbach_log, replace

*SETTIMANA 33
*GRUPPO 1
alpha Indic1_*_33, c std i
*se tolgo 1.1 passa da 0.4690 a 0.5993
alpha Indic1_2_33 Indic1_3_33 Indic1_4_33 , c std i
*se tolgo 1.3 passa da 0.5993 a 0.9175
*GRUPPO 2
alpha Indic2_*_33, c std i
*senza 2.4 va da 0.5041 a 0.5981
alpha Indic2_1_33 Indic2_2_33 Indic2_5_33 Indic2_6_33, c std i
*senza 2.6 viene 0.6529 e poi non migliora più
alpha Indic2_1_33 Indic2_2_33 Indic2_5_33, c std i
*GRUPPO 3
alpha Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 Trend3_1_33, c std i

*SETTIMANA 34
*GRUPPO 1
alpha Indic1_*_34, c std i
*da 0.4370 a 0.6255 togliendo 1.1 e 1.4
alpha Indic1_2_34 Indic1_3_34 , c std i
*GRUPPO 2
alpha Indic2_*_34, c std i
*senza 2.4 va da 0.5031 a 0.6161
alpha Indic2_1_33 Indic2_2_33 Indic2_5_33 Indic2_6_33, c std i
*senza 2.6 viene 0.6529 poi non migliora più
*GRUPPO 3
alpha Indic3_*_34 Trend3_1_34, c std i
*viene 0.72 e va bene, migliora se tolgo 3.8 e 3.9 e peggiora se tolgo il trend

log close

***	ANALISI DEI GRUPPI (cluster anaysis) GERARCHICA	***
**	Ward's linkage	(distanza euclidea al quadrato)**
*GRUPPO 1
cluster wardslinkage Indic1_2_33 Indic1_4_33, name(ward)
cluster dendrogram ward
cluster generate gruppi_w=groups(2/6), name(ward)
*quasi tutti nell'1
tabstat Indic1_2_33 Indic1_4_33, by(gruppi_w2) stat(n mean sd median)
list regioni Indic1_2_33 Indic1_4_33 if gruppi_w2==1
list regioni Indic1_2_33 Indic1_4_33 if gruppi_w2==2

*GRUPPO 2
cluster wardslinkage Indic2_1_33 Indic2_2_33 Indic2_5_33, name(ward2)
cluster dendrogram ward2
cluster generate gruppi2_w=groups(2/6), name(ward2)
tabstat Indic2_1_33 Indic2_2_33 Indic2_5_33, by(gruppi2_w4) stat(n mean sd median)
list regioni Indic2_1_33 Indic2_2_33 Indic2_5_33 if gruppi2_w4==1
list regioni Indic2_1_33 Indic2_2_33 Indic2_5_33 if gruppi2_w4==2
list regioni Indic2_1_33 Indic2_2_33 Indic2_5_33 if gruppi2_w4==3
list regioni Indic2_1_33 Indic2_2_33 Indic2_5_33 if gruppi2_w4==4

*GRUPPO 3
cluster wardslinkage Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 Trend3_1_33, name(ward3)
cluster dendrogram ward3
cluster generate gruppi3_w=groups(2/6), name(ward3)
tabstat Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 Trend3_1_33, by(gruppi3_w2) stat(n mean sd median)
list regioni Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 Trend3_1_33 if gruppi3_w2==1
list regioni Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 Trend3_1_33 if gruppi3_w2==2

*(gruppo 3 che non va bene perchè non c'è il trend)
cluster wardslinkage Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33, name(ward3)
cluster dendrogram ward3_
cluster generate gruppi3__w=groups(2/6), name(ward3_)
tabstat Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33, by(gruppi3__w2) stat(n mean sd median)
list regioni Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 if gruppi3__w2==1
list regioni Std3_1_33 Indic3_2_33 Indic3_4_33 Std3_5_33 Std3_6_33 Indic3_8_33 Indic3_9_33 if gruppi3__w2==2

* NORMALIZZAZIONE

set logtype text
log using "normalizzazione.txt",replace

*INDICATORI INVERSI
foreach i of numlist 26/43{
	foreach j of numlist 1/4{
		gen Indic1_`j'_`i'_C=(-1)*Indic1_`j'_`i'
	}
}

foreach i of numlist 26/43{
	foreach j of numlist 4/6{
		gen Indic2_`j'_`i'_C=(-1)*Indic2_`j'_`i'
	}
}

*standardizzazione con media e dev che variano nel tempo
*gruppo 1
foreach i of numlist 26/43{
	foreach j in 2 4{
		egen media1_`j'_`i'=mean(Indic1_`j'_`i'_C)
		egen stdev1_`j'_`i'=sd(Indic1_`j'_`i'_C)
		gen Snorm1_`j'_`i'=(Indic1_`j'_`i'_C-media1_`j'_`i')/stdev1_`j'_`i'
	}
}
*gruppo 2
foreach i of numlist 26/43{
	foreach j of numlist 1/2{
		egen media2_`j'_`i'=mean(Indic2_`j'_`i')
		egen stdev2_`j'_`i'=sd(Indic2_`j'_`i')
		gen Snorm2_`j'_`i'=(Indic2_`j'_`i'-media2_`j'_`i')/stdev2_`j'_`i'
	}
}
foreach i of numlist 26/43{
		egen media2_5_`i'=mean(Indic2_5_`i')
		egen stdev2_5_`i'=sd(Indic2_5_`i')
		gen Snorm2_5_`i'=(Indic2_5_`i'-media2_5_`i')/stdev2_5_`i'
}
*gruppo 3
foreach i of numlist 26/43{
	foreach j in 2 4 8 9{
		egen media3_`j'_`i'=mean(Indic3_`j'_`i')
		egen stdev3_`j'_`i'=sd(Indic3_`j'_`i')
		gen Snorm3_`j'_`i'=(Indic3_`j'_`i'-media3_`j'_`i')/stdev3_`j'_`i'
	}
}
foreach i of numlist 26/43{
	foreach j in 1 5 6{
		egen media3_`j'_`i'=mean(Indic3_`j'_`i')
		egen stdev3_`j'_`i'=sd(Indic3_`j'_`i')
		gen Snorm3_`j'_`i'=(Indic3_`j'_`i'-media3_`j'_`i')/stdev3_`j'_`i'
	}
}
foreach i of numlist 26/43{
		egen Tmedia3_1_`i'=mean(Trend3_1_`i')
		egen Tstdev3_1_`i'=sd(Trend3_1_`i')
		gen Tnorm3_1_`i'=(Trend3_1_`i'-Tmedia3_1_`i')/Tstdev3_1_`i'
}

*min-max che variano nel tempo
*foreach i of numlist 26/43{
*	foreach j of numlist 1/4{
*		egen min1_`j'_`i'=min(Indic1_`j'_`i'_C)
*		egen max1_`j'_`i'=max(Indic1_`j'_`i'_C)
*		gen Mnorm1_`j'_`i'=(Indic1_`j'_`i'_C-min1_`j'_`i')/(max1_`j'_`i'-min1_`j'_`i')
*	}
*}

log close

* PONDERAZIONE E AGGREGAZIONE

**** i tre gruppi hanno lo stesso peso, annullo la ponderazione implicita
foreach i of numlist 26/43{
	gen nop_indice_ponderato_`i'= (3*Snorm1_2_`i'+3*Snorm1_4_`i'+2*Snorm2_1_`i'+2*Snorm2_2_`i'+2*Snorm2_5_`i'+0.75*Snorm3_2_`i'+0.75*Snorm3_8_`i'+0.75*Snorm3_1_`i'+0.75*Snorm3_4_`i'+0.75*Snorm3_9_`i'+0.75*Snorm3_5_`i'+0.75*Snorm3_6_`i'+0.75*Tnorm3_1_`i')/18
̇}	

sort nop_indice_ponderato_33
list regioni nop_indice_ponderato_33

sort nop_indice_ponderato_42
list regioni nop_indice_ponderato_42

sort nop_indice_ponderato_43
list regioni nop_indice_ponderato_43

*

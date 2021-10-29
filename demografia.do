use "C:\Users\barto\Documents\Demografia sociale\Elaborato\Bartoli - Toccafondi\ESS5e03_4.dta" 

fre hwwkhs phwwkhs
tab icptn maritalb

drop if icptn != 1
*tolgo chi non vive con partner o marito/moglie

drop if hwwkhs == .b | hwwkhs == .c | hwwkhs == .d
drop if phwwkhs == .b | phwwkhs == .c | phwwkhs == .d
*tolgo chi non ha risposto alle ore di lavoro

fre hwwkhs phwwkhs

drop if hwwkhs > 100 | phwwkhs > 100
*tolgo risposte false, massimo 100 ore di lavoro domestico

fre hwwkhs phwwkhs
tab hwwkhs phwwkhs

fre rshipa2 rshipa3 rshipa4 rshipa5 rshipa6 rshipa7 rshipa8

drop if gndr != 1 & gndr !=2
drop if rshipa2 == 1 & (gndr == gndr2 | (gndr2 != 1 & gndr2 !=2))
drop if rshipa3 == 1 & (gndr == gndr3 | (gndr3 != 1 & gndr3 !=2))
drop if rshipa4 == 1 & (gndr == gndr4 | (gndr4 != 1 & gndr4 !=2))
drop if rshipa5 == 1 & (gndr == gndr5 | (gndr5 != 1 & gndr5 !=2))
drop if rshipa6 == 1 & (gndr == gndr6 | (gndr6 != 1 & gndr6 !=2))
drop if rshipa7 == 1 & (gndr == gndr7 | (gndr7 != 1 & gndr7 !=2))
*elimino se sono marito/moglie/partner e hanno lo stesso sesso o non sono M e F

tab gndr gndr2 if rshipa2 == 1
tab gndr gndr3 if rshipa3 == 1
tab gndr gndr4 if rshipa4 == 1
tab gndr gndr5 if rshipa5 == 1
tab gndr gndr6 if rshipa6 == 1
tab gndr gndr7 if rshipa7 == 1

drop if maritalb ==. | maritalb ==.b | maritalb ==.c | maritalb ==.d
*tolgo quelli che sono missing/non sanno/non rispondono allo stato civile

recode maritalb (2/6 = 0 "Non sposati") (1 = 1 "Sposati"), gen(sposati)
*creo variabile 0 = non sposato, 1 = legalmente sposato

drop if chldhm == .d
*tolgo missing a vivere con bambini

recode chldhm (2 = 0 "Does not") (1 = 1 "Respondent lives with children at household grid"), gen(children)
la var children "Children living at home or not"
*creo variabile 0 = vive senza bambini, 1 = vive con bambini

gen istr = 0
replace istr = eisced if eisced != 55 & eisced != .b & eisced != .c & eisced != .d 
replace istr = eiscedp if eiscedp > eisced & eiscedp != 55 & eiscedp != .b & eiscedp != .c & eiscedp != .d 
drop if istr == 0
*considero il livello di istruzione più alta tra i partner, tolgo i missing e other

recode istr (1 = 0 "Licenza elementare") (2 = 1 "Licenza media") (3/4 = 2 "Diploma") (5/7 = 3 "Laurea o superiore"), gen(istruzione)
la var istruzione "Istruzione più alta dei partner"
*ricodifico eisced con 0 = licenza elementare, 1 = licenza media, 2 = diploma, 3 = laurea o superiore

recode rlgdgr (0/3 = 0 "Non religioso") (4/6 = 1 "Religioso") (7/10 = 2 "Molto religioso"), gen(religione)
la var religione "Quanto sei religioso"
drop if religione == .b | religione == .c | religione == .d
*religione

recode wmcpwrk (4/5 = 0 "Le donne dovrebbero lavorare") (3 = 1 "Nè d'accordo nè in disaccordo") (1/2 = 2 "Le donne dovrebbero stare a casa"), gen(donne_lavorano)
la var donne_lavorano "Le donne dovrebbero smettere di lavorare per il bene della famiglia"
drop if donne_lavorano == .b | donne_lavorano == .c | donne_lavorano == .d
*donne non devono lavorare per la famiglia

gen anni_uomo = 2010 - yrbrn if gndr == 1
replace anni_uomo = 2010 - yrbrn2 if rshipa2 == 1 & anni_uomo == .
replace anni_uomo = 2010 - yrbrn3 if rshipa3 == 1 & anni_uomo == .
replace anni_uomo = 2010 - yrbrn4 if rshipa4 == 1 & anni_uomo == .
replace anni_uomo = 2010 - yrbrn5 if rshipa5 == 1 & anni_uomo == .
replace anni_uomo = 2010 - yrbrn6 if rshipa6 == 1 & anni_uomo == .
replace anni_uomo = 2010 - yrbrn7 if rshipa7 == 1 & anni_uomo == .
drop if anni_uomo == . | anni_uomo < 25
gen anni_donna = 2010 - yrbrn if gndr == 2
replace anni_donna = 2010 - yrbrn2 if rshipa2 == 1 & anni_donna == .
replace anni_donna = 2010 - yrbrn3 if rshipa3 == 1 & anni_donna == .
replace anni_donna = 2010 - yrbrn4 if rshipa4 == 1 & anni_donna == .
replace anni_donna = 2010 - yrbrn5 if rshipa5 == 1 & anni_donna == .
replace anni_donna = 2010 - yrbrn6 if rshipa6 == 1 & anni_donna == .
replace anni_donna = 2010 - yrbrn7 if rshipa7 == 1 & anni_donna == .
drop if anni_donna == . | anni_donna < 25
*età donna e uomo

replace wkhtot = 0 if wkhtot == .a & (mnactic == 2 | mnactic == 3 | mnactic == 4 | mnactic == 6 | mnactic == 8)
replace wkhtotp = 0 if wkhtotp == .a & (mnactp == 2 | mnactp == 3 | mnactp == 4 | mnactp == 6 | mnactp == 8)
*metto 0 ore di lavoro ai not applicable che sono studenti, disoccupati, pensionati, casalinghi
gen ore_uomo = wkhtot if gndr == 1
replace ore_uomo = wkhtotp if ore_uomo == .
drop if ore_uomo == . | ore_uomo == .a | ore_uomo == .b | ore_uomo == .c | ore_uomo == .d | ore_uomo > 100
gen ore_donna = wkhtot if gndr == 2
replace ore_donna = wkhtotp if ore_donna == .
drop if ore_donna == . | ore_donna == .a | ore_donna == .b | ore_donna == .c | ore_donna == .d | ore_donna > 100
*ore di lavoro retribuito

gen fac_uomo = hwwkhs if gndr == 1
replace fac_uomo = phwwkhs if fac_uomo == .
gen fac_donna = hwwkhs if gndr == 2
replace fac_donna = phwwkhs if fac_donna == .
gen y = fac_donna - fac_uomo
*ore lavoro domestico

recode wmcpwrk (4/5 = 0 "Le donne dovrebbero lavorare") (3 = 1 "Nè d'accordo nè in disaccordo") (1/2 = 2 "Le donne dovrebbero stare a casa"), gen(donne_lav)
la var donne_lav "Le donne dovrebbero smettere di lavorare per il bene della famiglia"

encode cntry, gen(country)

*******************************************************************************

reg y i.country, r

reg y i.country i.istruzione, r
*

reg y i.country i.istruzione anni_donna, r

reg y i.country i.istruzione i.sposati anni_donna, r

reg y i.country i.istruzione i.sposati i.religione anni_donna, r

reg y i.country i.istruzione i.sposati i.donne_lav anni_donna, r

reg y i.country i.istruzione i.sposati i.donne_lav i.religione, r
*

reg y i.country i.istruzione i.sposati i.donne_lav i.religione i.children, r

reg y i.country i.istruzione i.sposati i.donne_lav i.religione i.children ore_uomo ore_donna, r

reg y i.country i.istruzione i.sposati i.donne_lav i.religione i.children ore_uomo ore_donna anni_donna anni_uomo, r
*anni uomo non significativo

reg y i.country i.istruzione i.sposati i.donne_lav i.religione i.children ore_uomo ore_donna anni_donna, r
estimate store modello
*

reg y i.country i.istruzione i.sposati i.donne_lav i.religione i.children ore_uomo ore_donna anni_donna children#c.anni_donna, r
*interazione significativa ma non si sa interpretare

mean y, over(country)

bys country: egen media_y = mean(y)
bys country: egen media_ore_uomo = mean(ore_uomo)
bys country: egen media_ore_donna = mean(ore_donna)
bys country: egen moda_religione = mode(religione)
replace moda_religione = 1 if cntry == "BE"
*in belgio moda è sia 0 che 1, metto 1 perchè ci sono anche tanti molto religiosi

twoway (scatter media_y media_ore_uomo, msize(med) msymbol(o) mlabel(country) mlabpos(1)) (lfit media_y media_ore_uomo, lpattern(solid))

twoway (scatter media_y media_ore_donna, msize(med) msymbol(o) mlabel(country) mlabpos(1)) (lfit media_y media_ore_donna, lpattern(solid))

twoway (scatter media_y moda_religione, msize(med) msymbol(o) mlabel(country) mlabpos(1)) (lfit media_y moda_religione, lpattern(solid))

*******************************************************************************

gen uomini_istruiti = 0 if eisced < eiscedp & gndr == 1
replace uomini_istruiti = 1 if uomini_istruiti == . & gndr == 1
*se rispondono gli uomini, metto 1 se è la donna ad essere >= istruita, zero altrimenti

gen donne_istruite =1 if eisced >= eiscedp & gndr == 2
replace donne_istruite = 0 if donne_istruite == . & gndr == 2
*se rispondono le donne, metto 1 quando la donna è più o ugualmente istruita, zero altrimenti

clonevar donne_piu_istruite = donne_istruite
replace donne_piu_istruite = uomini_istruiti if donne_piu_istruite == .
summarize donne_piu_istruite
*unisco i due vettori e calcolo la media (% donne più istruite), 1- è la percentuale di uomini più istruiti
*al 72% sono le donne le più istruite (o ugualmente istruite)

*******************************************************************************
bys country: egen mediana_istruzione = median(istruzione)
bys country: egen moda_sposati = mode(sposati)
bys country: egen moda_donne_lav = mode(donne_lav)
bys country: egen moda_children = mode(children)
bys country: egen media_anni_donna = mean(anni_donna)
estimate restore modello

*situazione migliore
di -5.431237+7.712613 -4.246852-.1405937*100+.0572405*25
*SE

di -4.707674+7.712613 -4.246852-.1405937*100+.0572405*25
*NO

di -3.993173+7.712613 -4.246852-.1405937*100+.0572405*25
*GB

di -2.462074+7.712613 -4.246852-.1405937*100+.0572405*25
*IL

di .1581048+7.712613 -4.246852-.1405937*100+.0572405*25
*ES

di 6.143311+7.712613 -4.246852-.1405937*100+.0572405*25
*GR

*situazione peggiore
di -5.431237+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*SE

di -4.707674+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*NO

di -3.993173+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*GB

di -2.462074+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*IL

di .1581048+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*ES

di 6.143311+7.712613 +.9002187+2.660894+2.253597+3.277698+100*.1112144+.0572405*85
*GR
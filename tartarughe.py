#importo i moduli necessari
from turtle import Turtle, Screen, setup, bgcolor, clearscreen
from random import random, randint, randrange
from matplotlib.pyplot import plot, subplots, bar
from numpy import arange

#funzione per far ammalare le tartarughe in x = [175,250], y = [175,250]
def postiSpeciali(tartaruga):
    if tartaruga.xcor() >= 175 and tartaruga.xcor() <= 250 and tartaruga.ycor() >= 175 and tartaruga.ycor() <= 250:
        if tartaruga.fillcolor() == 'green':
            if random() > 0.5:
                tartaruga.color('red')
            else:
                tartaruga.color('yellow')
    return tartaruga

#funzione per stabilire se t1 contagia t2
def contagi(t1, raggio, t2):
    distanza = ((t1.xcor()-t2.xcor())**2 + (t1.ycor()-t2.ycor())**2)**0.5
    if distanza <= raggio:
        if random() > 0.5:
            t2.color('red')
        else:
            t2.color('yellow')
    return t2

#funzione principale        
def simulazione(r=250, n=50, p=0.2, d=10, v=20, g=60, c=10, h=15, m=0.01):

    #inizializzo l'area grafica
    setup(width = 2*r, height = 2*r)
    sfondo = ['#FBFCD4', '#CAFCFC']

    #disegno il muro
    muro=Turtle()
    muro.hideturtle()
    muro.speed(0)
 
    muro.penup()
    muro.setpos(r, r)
    muro.pendown()

    for i in range(4):
        muro.right(90)
        muro.forward(2*r)

    muro.penup()
    muro.setpos(-100, r)
    muro.pendown()
    muro.right(90)
    muro.pendown()
    muro.forward(2*r)

    muro.penup()
    muro.setpos(175, 250)
    muro.pendown()
    muro.color('#FFA9A9')

    for i in range(2):
        muro.forward(250-175)
        muro.left(90)

    #posiziono le tartarughe sullo schermo e le coloro
    tartarughe=[]
    for tartaruga in range(n):
        tartarughe.append(Turtle())
        tartarughe[tartaruga].penup()
        tartarughe[tartaruga].setpos(random()*2*r-r, random()*2*r-r)

        if random() < p:
            if random() > 0.5:
                tartarughe[tartaruga].color('red')
            else:
                tartarughe[tartaruga].color('yellow')
        else:
            tartarughe[tartaruga].color('green')

        tartarughe[tartaruga] = postiSpeciali(tartarughe[tartaruga])

        tartarughe[tartaruga].pendown()
        
    #creo il dizionario delle età
    anni = {tartaruga:None for tartaruga in range(n) if randint(5, 70) > 49}

    #quali tartarughe si muovono senza distanziamento sociale
    noDistanziamento = {tartaruga:None for tartaruga in range(n) if random() < 0.1}

    #creo il dizionario che memorizza i giorni di malattia
    giorniMalattia = {malata:0 for malata in range(n) if tartarughe[malata].fillcolor() == 'red' or tartarughe[malata].fillcolor() == 'yellow'}

    statistiche = []

    #simulazione dei giorni
    morti=0
    for giorno in range(1, g+1):
        Screen().bgcolor(sfondo[giorno % 2])

        #tartaruga malata random
        found = False
        while not found and len(giorniMalattia) < n:
            tartaruga = randrange(0, n)
            if tartarughe[tartaruga].fillcolor() == 'green' and tartaruga not in giorniMalattia:
                found = True
                if random() > 0.5:
                    tartarughe[tartaruga].color('red')
                else:
                    tartarughe[tartaruga].color('yellow')
                giorniMalattia[tartaruga] = 0
        
        #aumento i giorni di malattia
        for malata in giorniMalattia:
            if giorniMalattia[malata] >= 0:
                giorniMalattia[malata] += 1

        #estraggo chi muore
        for malata in giorniMalattia:
            if tartarughe[malata].fillcolor() == 'red' and giorniMalattia[malata] <= h and random() < m:
                giorniMalattia[malata] = -1
                tartarughe[malata].color('black')
                morti += 1

        #le tartarughe malate da più di h giorni guariscono
        for tartaruga in giorniMalattia:
            if giorniMalattia[tartaruga] == h+1:
                tartarughe[tartaruga].color('green')

        #conto morti, malati e sani
        asintomatici = 0
        sintomatici = 0
        for tartaruga in giorniMalattia:
            if tartarughe[tartaruga].fillcolor() == 'yellow':
                asintomatici += 1
            elif tartarughe[tartaruga].fillcolor() == 'red':
                sintomatici += 1
        malati = asintomatici + sintomatici
        sani = n - malati - morti
        print('Giorno:',giorno,'Morti:',morti,'Malati:',malati,'Sani:',sani)
        statistiche.append((giorno, malati, morti))

        #movimenti
        i = 0
        while i < v and (sani + asintomatici) > 0:

	    #scelgo quale tartaruga si muove
            tartaruga = randrange(0, n)
            if tartarughe[tartaruga].fillcolor() == 'green' or tartarughe[tartaruga].fillcolor() == 'yellow':
                i += 1

                #calcolo l'area in cui si può muovere
                if tartarughe[tartaruga].xcor() < -100:
                    estremoSinistro = -r
                    estremoDestro = -100-random()
                else:
                    estremoSinistro = -100
                    estremoDestro = r

                #calcolo la nuova posizione
                if tartaruga in noDistanziamento:
                    nuovaX = random()*(estremoDestro-estremoSinistro)+estremoSinistro
                    nuovaY = random()* 2*r - r
                else:
                    nuovaX = tartarughe[tartaruga].xcor()+random()*2*d-d
                    nuovaY = tartarughe[tartaruga].ycor()+random()*2*d-d
                    if nuovaX < estremoSinistro:
                        nuovaX = estremoSinistro
                    if nuovaX > estremoDestro:
                        nuovaX = estremoDestro
                    if nuovaY < -r:
                        nuovaY = -r
                    if nuovaY > r:
                        nuovaY = r

                #muovo la tartaruga
                tartarughe[tartaruga].setx(nuovaX)
                tartarughe[tartaruga].sety(nuovaY)

                #controllo quali tartarughe si ammalano
                if tartaruga not in giorniMalattia:
                    tartarughe[tartaruga] = postiSpeciali(tartarughe[tartaruga])
                    if tartarughe[tartaruga].fillcolor() == 'red' or tartarughe[tartaruga].fillcolor() == 'yellow':
                        giorniMalattia[tartaruga] = 1

                #controllo se viene contagiata
                if tartaruga not in giorniMalattia:
                    for malata in giorniMalattia:
                        if giorniMalattia[malata] <= h and giorniMalattia[malata] > -1:
                            if tartarughe[malata].xcor() <= estremoDestro and tartarughe[malata].xcor() >= estremoSinistro:
                                if malata in anni:
                                    tartarughe[tartaruga] = contagi(tartarughe[malata], 2*c, tartarughe[tartaruga])
                                else:
                                    tartarughe[tartaruga] = contagi(tartarughe[malata], c, tartarughe[tartaruga])
                    if tartarughe[tartaruga].fillcolor() == 'red' or tartarughe[tartaruga].fillcolor() == 'yellow':
                        giorniMalattia[tartaruga] = 1

                #controllo se contagia altri
                if tartarughe[tartaruga].fillcolor() == 'red' or tartarughe[tartaruga].fillcolor() == 'yellow':
                    if tartaruga in anni:
                        raggio = 2*c
                    else:
                        raggio = c
                    for j in range(n):
                        if j not in giorniMalattia:
                            if tartarughe[j].xcor() <= estremoDestro and tartarughe[j].xcor() >= estremoSinistro:
                                tartarughe[j] = contagi(tartarughe[tartaruga], raggio, tartarughe[j])
                                if tartarughe[j].fillcolor() == 'red' or tartarughe[j].fillcolor() == 'yellow':
                                    giorniMalattia[j] = 1            

    return statistiche

#simulazione al variare della probabilià di essere malato all'inizio
def simVaryingP(P = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]):
    malati = []
    morti = []
    for p in P:
        print('Simulazione con p =',p)
        valori = simulazione(p=p)[29]
        malati.append(valori[1])
        morti.append(valori[2])
        clearscreen()

    grafico, g = subplots()
    x = arange(len(P))
    g.plot(x, malati, '-o', label = 'Malati', color = 'red')
    g.plot(x, morti, '-o', label = 'Morti', color = 'black')
    g.set_title('Variazione del numero di malati e morti al variare di p (giorno 30)')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Probabilità di essere malato al giorno 1')
    g.set_xticks(x)
    g.set_xticklabels(P)
    g.legend()
    grafico.show()

#simulazione al variare del distanziamento sociale
def simVaryingD(D = [1, 3, 6, 12, 25, 50, 100, 200]):
    malati = []
    morti = []
    for d in D:
        print('Simulazione con d =',d)
        valori = simulazione(d=d)[29]
        malati.append(valori[1])
        morti.append(valori[2])
        clearscreen()

    grafico, g = subplots()
    x = arange(len(D))
    g.plot(x, malati, '-o', label = 'Malati', color = 'red')
    g.plot(x, morti, '-o', label = 'Morti', color = 'black')
    g.set_title('Variazione del numero di malati e morti al variare di d (giorno 30)')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Distanziamento sociale')
    g.set_xticks(x)
    g.set_xticklabels(D)
    g.legend()
    grafico.show()

#simulazione al variare del numero di movimenti giornalieri
def simVaryingV(V = [5, 10, 20, 40]):
    malati = []
    morti = []
    for v in V:
        print('Simulazione con v =',v)
        valori = simulazione(v=v)[29]
        malati.append(valori[1])
        morti.append(valori[2])
        clearscreen()

    grafico, g = subplots()
    x = arange(len(V))
    g.plot(x, malati, '-o', label = 'Malati', color = 'red')
    g.plot(x, morti, '-o', label = 'Morti', color = 'black')
    g.set_title('Variazione del numero di malati e morti al variare di v (giorno 30)')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Numero di movimenti giornalieri')
    g.set_xticks(x)
    g.set_xticklabels(V)
    g.legend()
    grafico.show()

#simulazione al variare della mortalità giornaliera
def simVaryingM(M = [0.01, 0.05, 0.1, 0.2, 0.5]):
    malati = []
    morti = []
    for m in M:
        print('Simulazione con m =',m)
        valori = simulazione(m=m)[29]
        malati.append(valori[1])
        morti.append(valori[2])
        clearscreen()

    grafico, g = subplots()
    x = arange(len(M))
    g.plot(x, malati, '-o', label = 'Malati', color = 'red')
    g.plot(x, morti, '-o', label = 'Morti', color = 'black')
    g.set_title('Variazione del numero di malati e morti al variare di m (giorno 30)')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Mortalità giornaliera')
    g.set_xticks(x)
    g.set_xticklabels(M)
    g.legend()
    grafico.show()

#simulazione al variare del raggio di contagio
def simVaryingC(C = [1, 10, 20, 40]):
    malati = []
    morti = []
    for c in C:
        print('Simulazione con c =',c)
        valori = simulazione(c=c)[29]
        malati.append(valori[1])
        morti.append(valori[2])
        clearscreen()

    grafico, g = subplots()
    x = arange(len(C))
    g.plot(x, malati, '-o', label = 'Malati', color = 'red')
    g.plot(x, morti, '-o', label = 'Morti', color = 'black')
    g.set_title('Variazione del numero di malati e morti al variare di c (giorno 30)')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Raggio di contagio')
    g.set_xticks(x)
    g.set_xticklabels(C)
    g.legend()
    grafico.show()

#andamento del numero di malati e morti all'aumentare dei giorni
def graficoSimulazione():
    valori = simulazione()
    malati = [valori[i][1] for i in range(len(valori))]
    morti = [valori[i][2] for i in range(len(valori))]
    x = arange(len(valori))
    width = 0.4
    grafico, g = subplots()
    g.bar(x - width/2, malati, width, label = 'Malati', color = 'red')
    g.bar(x + width/2, morti, width, label = 'Morti', color = 'blue')
    g.set_title('Variazione del numero di malati e morti al variare dei giorni')
    g.set_ylabel('Numero di malati/morti')
    g.set_xlabel('Giorno')
    g.set_xticks(x)
    g.set_xticklabels(range(1,len(valori)+1))
    g.legend()
    grafico.show()

simulazione()

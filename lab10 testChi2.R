# alfa  < p-value  -> brak podstaw do odrzucenia H0, HIPOTEZA H1 JEST FALSZYWA
# alfa  > p-value  -> odrzucamy H0, HIPOTEZA H1 JEST PRAWDZIWA

# Czy przy istotności X% dane potwierdzają, że dane pomiarów
# czegoś różnią się od ich odpowiedników gdzieś tam?

# ZADANIE 1: --------------

# H0: rozkład częstotliwości emerytów, którzy wrócili do pracy w hrabstwie Allegheny
# odpowiada ogólnemu rozkładowi podanymu przez stowarzyszeniee Russela Raynlda
# H1: ~H0

#Emeryci: ZIF, S, F, WF
observedf=c(122, 85, 76, 17) #częstotliwość zaobserwowana
expectedp=c(0.38, 0.32, 0.23, 0.07) # oczekiwane prawdopodobieństwo
chisq.test(observedf, p = expectedp)
alfa = 0.1
# alfa=0.1 < p-value = 0.3485 więc brak podstaw do odrzucenia H0

# Na poziomie istotności 10% dane nie potwierdzają hipotezy że 
# rozkład częstotliwości emerytów, którzy wrócili do pracy w hrabstwie Allegheny
# nie odpowiada ogólnemu rozkładowi podanymu przez stowarzyszenie Russela Reynlda

# Zatem rozkład częstotliwości emerytów, którzy wrócili do pracy w hrabstwie Allegheny
# odpowiada ogólnemu rozkładowi podanymu przez stowarzyszeniee Russela Raynlda

# ZADANIE 2: ---------------------------------------------------

# H0: rozkład zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat
# w ubiegłym roku w okręgu badacza odpowiada rozkładowi opisanemu w artykule 
# H1: ~H0

#zgony: W, Z, S
observedf=c(68, 27, 5) #częstotliwość zaobserwowana
expectedp=c(0.74, 0.16, 0.1) # oczekiwane prawdopodobieństwo
chisq.test(observedf, p = expectedp)
alfa = 0.1
# alfa=0.1 > p-value = 0.005121 odrzucamy H0 i przyjmujemy H1

# Zatem  rozkład zgonów związanych z bronią palną wśród osób w wieku od 1 do 18 lat
# w ubiegłym roku w okręgu badacza nie odpowiada rozkładowi opisanemu w artykule


# ZADANIE 3: ----------------------------

# H0: rozkład smaków w cukierkach skittles wynosi 20%
# H1: ~H0

# smaki: Cytrynowy, Limonkowy, Pomarańczowy, Truskawkowy, Winogronowy

# JAK JEST WIĘCEJ PRÓB TO TRZEBA DODAĆ

observedf=c(43,50,44,44,52) #częstotliwość zaobserwowana
expectedp=c(0.2, 0.2, 0.2, 0.2, 0.2) # oczekiwane prawdopodobieństwo
chisq.test(observedf, p = expectedp)
# p-value = 0.836
alfa = 0.05

# alfa=0.05 < p-value = 0.3485 więc brak podstaw do odrzucenia H0

# Na podstawie istotności 10% dane nie potwierdzają hipotezy że 
# rozkład smaków w cukierkach skittles różni się od 20%

# ZADANIE 4: CZY ROZKŁAD NORMALNY ----------------------------------------------
dane = read.csv("normalnosc_ozon.csv", sep=";", dec=",")
ozon = dane$ozon

install.packages("nortest")
library("nortest")

# H0: stężenie ozonu ma rozkład normalny
# H1: stężenie ozonu nie ma rozkładu normalnego

# RÓŻNE TESTY (wystarczy tylko jeden):
pearson.test(ozon, adjust = FALSE) # jak ponad 100 obserwacji
# alfa=0.05 < p-value = 0.2689 więc brak podstaw do odrzucenia H0

pearson.test(ozon, adjust = TRUE) # jak ponad 100 obserwacji
# alfa=0.05 < p-value = 0.146 więc brak podstaw do odrzucenia H0

lillie.test(ozon) # gdy mniej niż 80 obserwacji
# alfa=0.05 < p-value = 0.2774 więc brak podstaw do odrzucenia H0

shapiro.test(ozon) #NAJLEPSZY I UNIWERSALNY
# alfa=0.05 < p-value = 0.1098 więc brak podstaw do odrzucenia H0

# Na poziomie istotności 5% dane nie potwierdzają hipotezy że 
# stężenie ozonu nie ma rozkładu normalnego

# ZADANIE 6: CZY ROZKŁAD NORMALNY -------------------------------------------
dane = read.csv("normalnosc_punkty.csv", sep=";")
punkty = dane$punkty

# H0: punkty uzyskane przez studentów mają rozkład normalny
# H1: punkty uzyskane przez studentów nie mają rozkładu normalnego

library("nortest")
shapiro.test(punkty) #NAJLEPSZY I UNIWERSALNY
# alfa = 0.01 > p-value = 0.0006203 więc odrzucam H0 i przyjmuje H1

# Na poziomie istotności 1% dane potwierdzają hipotezę że 
# liczba punktów uzyskanych przez studentów nie ma rozkładu normalnego

# ZADANIE 7: CZY COŚ ZALEŻY ---------------------

# H0: poziom wykształcenia jest niezależny od miejsca zamieszkania
# H1: poziom wykształcenia jest zależny od miejsca zamieszkania

miejski = c(15, 12, 8)
podmiejski = c(8, 15, 9)
wiejski = c(6, 8, 7)

TK = data.frame(miejski, podmiejski, wiejski)
chisq.test(TK)
# alfa = 0.05 < p-value = 0.5569 więc brak podstaw do odrzucenia H0

# Na poziomie istotności 5% dane nie potwierdzają hipotezy że 
# poziom wykształcenia jest zależny od miejsca zamieszkania

# ZADANIE 8: CZY COŚ ZALEŻY -------------------

# H0: odsetek pasażerów ze zagubionym bagażem niezależy od linii lotniczej
# H1: odsetek pasażerów ze zagubionym bagażem zależy od linii lotniczej

tak = c(10, 7, 4)
nie = c(90, 93, 96)

TK = data.frame(tak, nie)
chisq.test(TK)
# alfa = 0.05 < p-value = 0.251 więc brak podstaw do odrzucenia H0

# Na poziomie istotności 5% dane nie potwierdzają hipotezy że 
# odsetek pasażerów ze zagubionym bagażem zależy od linii lotniczej

# ZADANIE 9: CZY COŚ ZALEŻY ---------------------------------

# H0: opinia nie zależy od wieku
# H1: opinia zależy od wieku

za = c(96, 96, 90, 36)
przeciw = c(201, 189, 195, 234)
nie_wiem = c(3, 15, 15, 30)

TK = data.frame(za, przeciw, nie_wiem)
chisq.test(TK)
# alfa = 0.05 > p-value = 2.511e-11 ~ 0 więc odrzucam H0 i przyjmuje H1

# Na poziomie istotności 5% dane potwierdzają hipotezę że 
# opinia zależy od wieku

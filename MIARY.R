#wykład:
x=c(1, 2, 3.6, 4, 3, 3, 1, 3)
sum(x)
oz = read.csv("ozon.csv", head=TRUE)
sum(oz)
print(oz)
szer=table(x)
print(szer)
#dane ilościowe dyskretne (szereg rozdzielczy punktowy i wykres)
szer=table(x)
library(arm)
discrete.histogram(x, freq=T, main="TEST", prob.col="orange", xlab="test czy dziala")
discrete.histogram(x,) #prawdopodobieństwo
discrete.histogram(x, freq=T) #liczebność
(table(x))
#dane ilościowe ciągłe (szereg rozdzielczy przedziałowy i wykres)
table(cut(x, 3))
hist(x) #liczebność
hist(x, freq=FALSE) #gęstość
pie(table(cut(x, 2)))

#Zadanie 1:
loty = read.csv("loty.csv", sep=";", head = TRUE)

class(loty) #a (odp: data.frame)

#b
nazwy = names(loty)

#-----------------------ŚREDNIA----------------------------------

apply(loty, 2, mean) #średnia
#lub
for (i in 1:6){ #----------------
  print(paste("srednia w roku", nazwy[i], mean(loty[,i])))
}
#srednia liczba pasazerów w roku 1956 wynosiła 328 osób (ludzi nie da się podzielić na cześci!!!)

#------------------------MEDIANA---------------------------------

apply(loty, 2, function(x) quantile(x, prob=0.5)) #mediana
#lub
for (i in 1:6){ #----------------
  print(paste("mediana (2 kwantyl)",nazwy[i],quantile(loty[,i])[3]))
}
#W 6 miesiącach liczba pasażerów pewnej linii lotniczej w roku 1956 była mniejsza
#lub równa 315 i w pozostałych 6 miesiącach liczba pasażerów była większa lub równa
# 315 osób!!!!

#-------------------------1.KWANTYL--------------------------------

apply(loty, 2, function(x) quantile(x, prob=0.25)) #pierwszy kwantyl
#lub
for (i in 1:6){ #----------------
  print(paste("1 kwantyl",nazwy[i],quantile(loty[,i])[2]))
}
# W trzech miesiacach w roku 1956 byla mniejsza lub rowna 301 osob
# i w 9 miesiacach liczba pasazerow byla wieksza lub rowna 301 osob

#--------------------------3.KWANTYL-------------------------------

apply(loty, 2, function(x) quantile(x, prob=0.75)) #trzeci kwantyl
#lub
for (i in 1:6){ #----------------
  print(paste("3 kwantyl",nazwy[i],quantile(loty[,i])[4]))
}

#------------------------ODCHYLENIE---------------------------------

apply(loty, 2, sd) #odchylenie standardowe
#lub
for (i in 1:6){
  print(paste("odchylenie ",nazwy[i],sd(loty[,i])))
}
#dla pewnej linii lotniczej w roku 1956 przeciętnie liczba pasażerów odchyla się od średniej o 48 osób (zaokrąglamy bo osób nie można podzielić)

#------------------------WSP.ZMIENNOSCI---------------------------------
apply(loty, 2, function(x) sd(x)/mean(x)*100) #wsp. zmienności w %
#lub
for (i in 1:6){ #----------------
  print(paste("wspolczynnik zmiennosci: ",((sd(loty[,i]) / mean(loty[,i])*100))))
}
#wspolczynnik zmiennosci, slabe zroznicowanie liczby pasażerów w roku 1956

#c
#min(loty)
#max(loty)
#przedzialy = seq(200, 650, length=4) #generowanie przedziałów
par(mfrow = c(2,3)) #podział okna wykresów
nazwy = names(loty) #wektor nazw wykresów
kolory=c("red", "yellow", "orange", "pink", "blue", "purple")
for (i in 1:6){
  przedzialy = seq(min(loty[i]), max(loty[i]), length=4)
  hist(loty[,i], breaks=przedzialy, xlab="ilość", main=paste('loty w', nazwy[i]), col=kolory[i]) #wykresy
}

#d
par(mfrow = c(1,1))
boxplot(loty)



#Zadanie 2:
dane2=read.csv("oceny.csv", sep = ";", head = TRUE)

class(oceny) #a (odp: data.frame)

oceny = read.csv("oceny.csv", sep=";", dec=",") #b

#c POPRAWNE!!!:
for (i in 1:4){
  sr=mean(na.omit(oceny[,i]))
  med=quantile(na.omit(oceny[,i]), prob=0.5)
  q1=quantile(na.omit(oceny[,i]), prob=0.25)
  q3=quantile(na.omit(oceny[,i]), prob=0.75)
  odch=sd(na.omit(oceny[,i]))
  wsp=sd(na.omit(oceny[,i]))/mean(na.omit(oceny[,i]))*100
  mi=min(na.omit(oceny[,i]))
  ma=max(na.omit(oceny[,i]))
  print(paste0(names(oceny)[i], ": średnia: ", sr, " mediana: ", med, " kwantyl1: ", q1, " kwantyl3: ", q3, " odchylenie: ", odch, " wsp zmiennosci: ", wsp, " min: ", mi, " max: ", ma))
}


#d: Diagramy odcinkowe
library(arm)
nazwy_grup=names(oceny)
par(mfrow=c(2,2)) 
for (i in 1:4){
  discrete.histogram(oceny[,i], main=nazwy_grup[i], freq=TRUE)
}

#e:
par(mfrow=c(1,1)) 
boxplot(na.omit(oceny))

#f: szereg rozdzielczy punktowy
for (i in 1:4){
  print(table(oceny[,i]))
}

#g:
par(mfrow=c(2,2)) 
for (i in 1:4){
  pie(table(oceny[,i]), main=paste("wykres kołowy", nazwy_grup[i]))
}



#Zadanie 3:
truskawki = read.csv("truskawki.csv", sep=";", head = TRUE)

#a
class(truskawki) #odp: data.frame

#b
nazwy_plonow=names(truskawki)
plon00=truskawki$plon.2000
plon10=na.omit(truskawki$plon.2010)

#plon00
mean(plon00)
quantile(plon00, prob=0.5)
quantile(plon00, prob=0.25)
quantile(plon00, prob=0.75)
sd(plon00)
sd(plon00)/mean(plon00)*100

#plon10
mean(plon10)
quantile(plon10, prob=0.5)
quantile(plon10, prob=0.25)
quantile(plon10, prob=0.75)
sd(plon10)
sd(plon10)/mean(plon10)*100

#c SZEREGI ROZDZIELCZE PRZEDZIALOWE
sqrt(length(plon00))
sqrt(length(plon10))
table(cut(plon00,5))
table(cut(plon10,5))

#d WYKRESY KOŁOWE
par(mfrow = c(1,2))
pie(table(cut(plon00,5)), main=paste("wykres kołowy", nazwy_grup[1]))
pie(table(cut(plon10,5)), main=paste("wykres kołowy", nazwy_grup[2]))

#e HISTOGRAMY PROPABILISTYCZNE
par(mfrow = c(1,2))
for (i in 1:2){
  hist(na.omit(truskawki[,i]), breaks=5, main=paste("histogram probabilistyczny", nazwy_plonow[i]), xlab="liczba plonów", freq=FALSE)
}

#f 
par(mfrow = c(1,1))
boxplot(na.omit(truskawki))


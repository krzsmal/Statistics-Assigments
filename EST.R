dane = read.csv("dane_est.csv", sep=";",dec=",")

library("BSDA")
library("TeachingDemos")

#ZADANIE 1:
#A:
#Populacja to wszystkie syntetyczne diamenty wyprodukowene
#nową metodą produkcji
#Próba to 12 syntetycznech diamentów wyprodukowanych tą metodą
#Badana ziemnna to karaty
karaty=na.omit(dane$diamenty)

#B:
print(karaty)
sr=mean(karaty)
wariancja=var(karaty)
odchylenie=sd(karaty)
print(paste(sr, wariancja, odchylenie))

#C
ufnosc=0.95
alfa=1-ufnosc
n=length(karaty)

L=sr-qt(1-alfa/2,n-1)*odchylenie/sqrt(n)
U=sr+qt(1-alfa/2,n-1)*odchylenie/sqrt(n)
print(paste0("(", L, ", ", U, ")"))

t.test(karaty, conf.level=1-alfa)
#Z ufnością 95% możemy powiedzieć, że przedział od 0,498 karata do
# 0,57 karata pokrywa nieznaną prawdziwą średnią wagę wszystkich syntetycznych
#diamentów produkowanych nową metodą

#D
ufnosc=0.99
alfa=1-ufnosc
n=length(karaty)

L=sr-qt(1-alfa/2,n-1)*odchylenie/sqrt(n)
U=sr+qt(1-alfa/2,n-1)*odchylenie/sqrt(n)
print(paste0("(", L, ", ", U, ")"))

#E
ufnosc=0.95
alfa=1-ufnosc
library("TeachingDemos")
chi=sigma.test(karaty, conf.level=1-alfa)
print(chi$conf.int)
#Z ufnością 95% możemy powiedzieć, że przedział od 0,001 karata^2 do
# 0,009 karata^2 pokrywa nieznaną prawdziwą wariancję wagi wszystkich syntetycznych
#diamentów produkowanych nową metodą

sqrt(chi$conf.int[[1]])
sqrt(chi$conf.int[[2]])
#Z ufnością 95% możemy powiedzieć, że przedział od 0,039 karata do
# 0,095 karata pokrywa nieznane prawdziwe odchylenie standardowe wagi wszystkich syntetycznych
#diamentów produkowanych nową metodą od średniej

#ZADANIE 2:
#A
#Populacja to kobiety karmiące piersią
#Próba to 20 kobiet karmiących piersią
#Badana zienna to poziom PCB u kobiet karmiących piersią

#B
mleko=na.omit(dane$mleko)
srednia=mean(mleko)
srednia

#C
wariancja=var(mleko)
odchylenie=sd(mleko)
wariancja
odchylenie

#D
ufnosc=0.95
alfa=1-ufnosc
t.test(mleko, conf.level = 1-alfa)
#Z ufnością 95% możemy powiedzieć, że przedział od 3,42 poziomu PCB do
# 8,18 poziomu PCB pokrywa nieznaną prawdziwą średnią poziomu PCB
#u wszystkich matek karmiących piersią

#E
library("TeachingDemos")
chi=sigma.test(mleko, conf.level=1-alfa)
chi=chi$conf.int
print(chi)
#Z ufnością 95% możemy powiedzieć, że przedział od 14.95 poziomu PCB ^2 do
# 55.16 poziomu PCB ^2 pokrywa nieznaną prawdziwą wariancję poziomu PCB
#u wszystkich matek karmiących piersią

sqrt(chi[[1]])
sqrt(chi[[2]])
#Z ufnością 95% możemy powiedzieć, że przedział od 3.86 poziomu PCB do
# 7.43 poziomu PCB pokrywa nieznaną prawdziwą wartość odchylenia standardowego
#zawartości poziomu PCB u wszystkich matek karmiących piersią

#ZADANIE 3
#Populacja to wszystkie paczki papierosów nowej marki
#Próba to 15 paczek papierosów nowej marki
#Badana zmienna to zawartość nikotyny w mg w papierosach nowej marki

#A
szlugi=na.omit(dane$papierosy)
ufnosc=0.95
alfa=1-ufnosc
odchylenie=0.7
n=length(szlugi)
L=mean(szlugi)-qnorm(1-alfa/2)*odchylenie/sqrt(n)
U=mean(szlugi)+qnorm(1-alfa/2)*odchylenie/sqrt(n)
z.test(szlugi, sigma.x=odchylenie, conf.level=1-alfa)$conf.int
print(paste0("(", L, ", ", U, ")"))
#Z ufnością 95% możemy powiedzieć, że przedział od 1.45 mg do
# 2.17 mg pokrywa nieznaną prawdziwą średnią zawartość nikotyny wszystkich
#paczek papierosów nowej marki

#B KOLOS
n=84 #sposób eksperymentalny
L=mean(szlugi)-qnorm(1-alfa/2)*odchylenie/sqrt(n)
U=mean(szlugi)+qnorm(1-alfa/2)*odchylenie/sqrt(n)
U-L
#Aby długość 95% przedziału ufności była nie większa niż 0.3mg próbka powinna mieć 84 elementy

# 0.3=<2*qnorm(1-alfa/2)*odchylenie/sqrt(n)
# sqrt(n)=<2*qnorm(1-alfa/2)*odchylenie/0.3
# n=<(2*qnorm(1-alfa/2)*odchylenie/0.3)^2
(2*qnorm(1-alfa/2)*odchylenie/0.3)^2 #obliczenie
2*qnorm(1-alfa/2)*odchylenie/sqrt(84) #sprawdzenie
#n>=83,61, czyli n=84
# Aby długość 95% przedziału ufności była nie większ aniż 0,3 mg
# potrzebna jest próbka 84 paczek papierosów


#C
sd(szlugi)
#proba ma nizsze odchylenie od populacji

#ZADANIE 4
#Populacja to wszystkie wodorosty
#Próba to 18 50-kilogramowych próbek wodorostów
#Badana zmienna to zawartość białka w 50-kilogramowych porcjach wodorostów

#A
wodo=na.omit(dane$wodorosty)
mean(wodo)
var(wodo)

#B
ufnosc=0.9
alfa=1-ufnosc
t.test(wodo, conf.level=1-alfa)
#Z ufnością 90% możemy powiedzieć, że przedział od 3.11 do
# 3.77 pokrywa nieznaną prawdziwą średnią zawartość białka wszystkich wodorostów

#C
library("TeachingDemos")
sigma.test(wodo, conf.level = 1 - alfa)
#Z ufnością 90% możemy powiedzieć, że przedział od 0.38 do
# 1.24 pokrywa nieznaną prawdziwą wariancję zawartości białka wszystkich wodorostów

#ZADANIE 5
sygnal=na.omit(dane$sygnal)
mean(sygnal) #oszacowanie punktowe

odchylenie=3
ufnosc=0.95
alfa=1-ufnosc
library("BSDA")
z.test(sygnal, sigma.x = odchylenie, conf.level = 1-alfa)
#Z ufnością 90% możemy powiedzieć, że przedział od 17.44 do
# 21.16 pokrywa nieznaną prawdziwą średnią zawartość natężenia sygnału transmitowanego
# z lokalizacji A który jest odbierany w lokalizacji B

#ZADANIE 6
srednia=4.7
odchylenie=2.2
ufnosc=0.95
alfa=1-ufnosc
library("BSDA")
zsum.test(srednia, odchylenie, 1200, conf.level = 1-alfa)
# Z ufnością 95% możemy powiedzieć, że przedział od 4.57 minut do
# 4.83 minut pokrywa nieznaną prawdziwą średnią długość trwania wszystkich połączeń

n=1200
L=sqrt(((n-1)*odchylenie^2)/qchisq(1-alfa/2,n-1))
U=sqrt(((n-1)*odchylenie^2)/qchisq(alfa/2,n-1))
print(paste0("(", L, ", ", U, ")"))
# Z ufnością 95% możemy powiedzieć, że przedział od 2.11 minut do
# 2.30 minut pokrywa nieznaną prawdziwą wartość odchylenia
# standardowego długości trwania wszystkich połączeń

#ZADANIE 7
n=365
srednia=102
wariancja=81
odchylenie=sqrt(wariancja)

#A
ufnosc=0.98
alfa=1-ufnosc
zsum.test(102, odchylenie, 365, conf.level = 1-alfa)
# Z ufnością 95% możemy powiedzieć, że przedział od 100,9 hl do
# 103,1 hl pokrywa nieznane prawdziwe średnie zużycie wody każdego
# dnia roku w fabryce

#B
#122 nie trzeba się bać, bo 122 leży poza przedziałem ufności
#raczej tak jeżeli przedział ufności by był np<120,122>


#ZADANIE 8
wariancja=25
odchylenie=sqrt(wariancja)
ufnosc=0.95
alfa=1-ufnosc
# 1<=qnorm(1-alfa/2)*odchylenie/sqrt(n)
# sqrt(n)<=qnorm(1-alfa/2)*odchylenie/1
# n<=(qnorm(1-alfa/2)*odchylenie/1)^2
(qnorm(1-alfa/2)*odchylenie/1)^2
qnorm(1-alfa/2)*odchylenie/sqrt(97) #n=97
# Aby uzyskać błąd estymacji +-1 średniego czasu wiązania mieszanki
# na poziomie 95% liczebność próby powinna wynosić 97

#ZADANIE 9
odchylenie=0.3
ufnosc=0.9
alfa=1-ufnosc
# 0.1<=qnorm(1-alfa/2)*odchylenie/sqrt(n)
# *sqrt(n)<=qnorm(1-alfa/2)*odchylenie*10
# n<=(qnorm(1-alfa/2)*odchylenie*10)^2
(qnorm(1-alfa/2)*odchylenie*10)^2
qnorm(1-alfa/2)*odchylenie/sqrt(25) #n=25

ufnosc=0.99
alfa=1-ufnosc
(qnorm(1-alfa/2)*odchylenie*10)^2
qnorm(1-alfa/2)*odchylenie/sqrt(60) #n=60

#ZADANIE 10
n=100
T=4
ufnosc=0.95
alfa=1-ufnosc
phat=T/n
L=phat-qnorm(1-alfa/2)*sqrt(phat*(1-phat)/n)
U=phat+qnorm(1-alfa/2)*sqrt(phat*(1-phat)/n)
print(paste0("(", L, ", ", U, ")"))
# Z ufnością 95% przedział od 0,15% do 7,85% pokrywa nieznaną
# prawdziwą proporcję wszystkich niedopełnionych puszek

binom.test(T, n, conf.level = 1-alfa)$conf.int
prop.test(T, n, conf.level = 1-alfa)$conf.int

#ZADANIE 11
n=120
T=24
ufnosc=0.9
alfa=1-ufnosc
phat=T/n
L=phat-qnorm(1-alfa/2)*sqrt(phat*(1-phat)/n)
U=phat+qnorm(1-alfa/2)*sqrt(phat*(1-phat)/n)
print(paste0("(", L, ", ", U, ")"))
# Z ufnością 90% przedział od 13,9% do 26,5% pokrywa nieznaną
# prawdziwą proporcję wszystkich monterów nie przestrzegających BHP

#ZADANIE 12
ufnosc=0.98
alfa=1-ufnosc
#A
phat=0.3
# 0.05=qnorm(1-alfa/2)*sqrt(phat*(1-phat)/n)
# n=((qnorm(1-alfa/2)/0.05)^2)*(phat*(1-phat))
((qnorm(1-alfa/2)/0.05)^2)*(phat*(1-phat))
qnorm(1-alfa/2)*sqrt(phat*(1-phat)/455) #n=455
# Aby uzyskać błąd oszacowania +-0,05 na poziomie ufności 98%
# z proporzją równą 0,3 trzeba zbadać 455 osób

#B
p = 0.5
(qnorm(1-alfa/2) * sqrt(p*(1-p))/0.05)^2

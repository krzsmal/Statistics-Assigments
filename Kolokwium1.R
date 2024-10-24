#Kolokwium 17.04.2024 Krzysztof Smal 155990

dane = read.csv("dane.csv", sep=";",dec=",")

#Zadanie 5
chlorki=na.omit(dane$chlorki)
chlorki
#A
wsp=sd(chlorki)/mean(chlorki)*100 #wsp. zmienności w %
wsp
#wspolczynnik zmiennosci wynosi 9.022081%, więc zróżnicowane zawartości chlorków w próbkach jest słabe

odchylenie=sd(chlorki)
odchylenie
#dla pobranych 40 próbek przeciętnie liczba zawartości chlorku odchyla się od średniej o 7,74 mg/l

#B
szereg = seq(65, 100, length=7)
szereg = cut(chlorki, breaks=szereg)
szereg #szereg rozdzielczy przedziałowy: (65,70.8] (70.8,76.7] (76.7,82.5] (82.5,88.3] (88.3,94.2] (94.2,100]
table(szereg)

#C
boxplot(chlorki)


#Zadanie 1
mu=13.7
sig=2.2
# T - temperatura w lipcu na Hali Gąsienicowej o godz. 12.00
# T ∼ N(13.7, 2.2)

pnorm(15,mu,sig)-pnorm(13,mu,sig) #P(13<T<15)=F(15)-F(13)
#Prawdopodobieństwo że temperatura będzie wynosiła między 13 a 15 stopni jest równe 34,76%

curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig, xlab="temperatura", ylab="prawdopodobieństwo") #99,7% obserwacji znajduje sie w przedziale


#Zadanie 2
p=0.45
#Phat T ∼ N(p, sqrt(p(1-p)/n))
n = 100
T = 40
pnorm(T/n, p, sqrt(p*(1-p)/n))
#Prawdopodobieństwo że nie więcej niż 40 losowo wybranych osób spośród 100 wzięło udział w wyborach wynosi 15,75%


#Zadanie 3
n=16
moc=na.omit(dane$moc)
moc
#Zmienna ma rozklad normalny
ufnosc=(90+9)/100
alfa=1-ufnosc

library("TeachingDemos")
war=sigma.test(moc, conf.level = 1-alfa ) #wariancja

L=sqrt(war$conf.int[[1]])
U=sqrt(war$conf.int[[2]])
print(paste0("(", L, ", ", U, ")"))
# Z ufnością 99% możemy powiedzieć, że przedział od 34,99 kW (0,3499*100, bo wartości w poleceniu były w setkach kW) do
# 93,45 kW pokrywa nieznaną prawdziwą wartość odchylenia
# standardowego uzyskanej mocy

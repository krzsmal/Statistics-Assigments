#TRIKI NA OSTATNIM SLAJDZIE W2
# W wykładniczym z podaną średnią:
mean = 5.3
lambda = 1 / mean


# A - liczba samochodów zespsutą klimatyzacją
# A ~ bin(10, 0.3) / A ∼ Exp(0,01) / A ∼ N(0.13, 0.005)

#Prawdopodobieństwo że dokładnie X ... jest równe
#Prawdopodobieństwo że co najmniej X ... jest równe
#Prawdopodobieństwo że mniej niż X ... jest równe

#-------------- ROZKŁAD DWUMIANOWY --------------
n=3
x=0:n
p=0.7

dbinom(1,n,p) #prawdopodobieństwo że 1 będzie spełniony P(A=1)
A=dbinom(x,n,p) #rozkład dwumianowy zmiennej A
print(A)
pbinom(2, n, p) #dystrybuanta dla 2 P(A<=2)
rbind(x,A)

expect=sum(x*A) #wartość oczekiwana
expect = n*p #to samo innym wzorem
print(expect)
#przecietnie mozemy spodziewac sie, ze 2 żarówki przekroczą zywotnosc 500 godzin 

variance=sum(x^2*A)-expect^2 #wariancja
print(variance)

std=sqrt(variance) #odchylenie standardowe
print(std)
#przeciętnie mozemy spodziewac się odchylenia od średniej o 1 żarówkę

rbinom(5,n,p) #losowa obserwacja - losowe wyniki eksperymentu

plot(x,A,type="h", lwd=10) # wykres rozkładu dyskretnego-
#-------------- ROZKŁAD WYKŁADNICZY -------------
rm(x)
lam=0.01
curve(dexp(x, lam), 0, 500) #wykresik
curve(lam*exp(-lam*x),0,500)
# lam*exp(-lam*x) = dexp(x, lam)

f=function(x){lam*exp(-lam*x)} #deklarowanie funkcji gęstości
integrate(f, 0, 50) # prawdopodobieństwo że rysa zostanie
#znaleziona na kolejnych 50 cm taśmy

#policzenie tego samego co wyżej XD
lam=0.01
pexp(50,lam) #P(X<=50)

#wartość oczekiwana (w funkcji trzeba dopisać x*) i zrobić całkę od 0 do nieskończoniści
f=function(x){x*lam*exp(-lam*x)}
expect=integrate(f, 0, Inf)
print(expect)

#wariancja ???
f=function(x){(x^2)*lam*exp(-lam*x)}
variance=integrate(f, 0, Inf)$value-expect$value^2
print(variance)


#-------------- ROZKŁAD NORMALNY ------------------------------------
# x ~ N(1, 0.001)
# P(X<1.0015)=F(1.0015)
mu = 1 # µ - średnia/wartość oczekiwana
sig = 0.1 # σ - odchylenie standardowe

curve(dnorm(x, mu, sig), 0.70, 1.3) #wykresik
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig) #99,7% obserwacji znajduje sie w przedziale

pnorm(1.0015,mu,sig) #P(X<=1.0015)

# P(X>0.9995)=1-F(0.9995)
1-pnorm(0.9995,mu,sig)

# P(0.9998<X<1,0004)=F(1.0004)-F(0.9998)
pnorm(1.0004,mu,sig)-pnorm(0.9998,mu,sig)

#-------------- PZRYBLIŻENIE ROZK. DWUM. ROZK. NORM. -------
p=0.25
n=100
x=0:n
pbinom(15, n, p) #P(X<15) 
#Przybliżenie 
mu=n*p
sig=sqrt(n*p*(1-p))
pnorm(15,mu,sig) #P(X<15)
#-------------- ROZKŁAD ŚREDNIEJ I SUMY(CAŁKOWITA...) -----
mu = 200
sig = 10
n=25
#avg (srednia) ma rozkład N z parametrami mu i sig/sqrt(n)
# Dbar ~ norm(mu, sig/sqrt(n))
pnorm(202,mu,sig/sqrt(n)) - pnorm(199,mu,sig/sqrt(n)) #P(199<avg(X)<202)

#T = X1+X2+...+X25, T ma rozklad normalny N z parametrami n*mu i sqrt(n)*sig 
#T ~ norm(n*mu, sqrt(n)*sig)
pnorm(5100, n*mu,sqrt(n)*sig) #P(T<=5100)

#-------------- PROPORCJA POPULACYJNA / PRAWD. SUKCESU -----
# Jeżeli 25% konsumentów rzeczywiście zna nowy produkt, to jakie
# jest prawdopodobieństwo, że nie więcej niż 232 losowo wybranych
# konsumentów spośród 1000 zna produkt?
p = 0.25
n = 1000
T = 232 #liczba sukcesów w próbie
pnorm(T/n, p, sqrt(p*(1-p)/n))

#-------------- GENEROWANIE DANYCH ŚREDNIJ I SUMY ----
mu = 5
sig = 2
n = 50

srednie = 1:200
sumy = 1:200
for (i in 1:length(srednie)) {
  sample = rnorm(50, mu, sig)
  srednie[i] = mean(sample)
  sumy[i] = sum(sample)
}

par(mfrow = c(1,2))

# mean
hist(srednie, freq = F, col="Blue")
sig_M = sig / sqrt(n)
curve(dnorm(x, mu, sig_M), mu - sig_M * 3, mu + sig_M * 3, col="Red", add=T)

# T
hist(sumy, freq = F, col="Blue")
mu_T = mu * n
sig_T = sig * sqrt(n)
curve(dnorm(x, mu_T, sig_T), mu_T - sig_T * 3, mu_T + sig_T * 3, col="Red", add=T)


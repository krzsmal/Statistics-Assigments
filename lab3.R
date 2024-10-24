#Zadanie 1:
#rozklad dwumianowy Bin(n, p)
#a:
n=5
x=0:n
p=0.3
prawd=dbinom(x,n,p) #GĘSTOŚĆ rozkład dwumianowy
dyst=pbinom(x,n,p) #DYSTRYBUANTA
rbind(x, prawd)
plot(x,prawd, type="h", lwd=10, xlab="x", ylab = "f(x)", main="histogram")

#bCZERWONE WZORY (NIE MA DOKŁADNIE TYCH - TRZEBA ODWRÓCIĆ ZNAK RÓWNOŚCI)
dbinom(3,n,p) #P(S)=3, TRZEBA PISAĆ CO SIĘ LICZY
1-pbinom(2, n, p) #P(S)>=3 czyli P(S>2)
pbinom(2, n, p) #P(S)<3 czyli P(S<=2)

#zadanie 2:
#rozklad dwumianowy Bin(n, p)
n=8
x=0:n
p=0.9
prawd=dbinom(x,n,p)
rbind(x, prawd)
plot(x,prawd, type="h", lwd=10, xlab="x", ylab = "f(x)", main="histogram")
dbinom(8,n,p) #a
dbinom(7,n,p) #b
1-pbinom(5, n, p) #c
expect = sum(x*prawd) #d
expect = n*p #d to samo co wyżej ale innym wzorem
print(expect)
#przecietnie mozemy spodziewac sie, ze 7 zarowek przekroczy zywotnosc 500 godzin 
wariancja=sum(x^2*prawd)-expect^2
print(wariancja)
sd=sqrt(wariancja) #e
sd=sqrt(n*p*(1-p)) #to samo innym wzorem
print(sd)
#przeciętnie mozemy spodziewac się odchylenia od średniej o 1 żarówkę

#Zadanie 3:
#rozkłd wykładniczy
lambda = 0.01
curve(dexp(x, lambda), 0, 1000)
1 - pexp(200, lambda)#a P(X>=200) to jest to samo co P(X>200) bo P(200) = 0, prawdopodobieństwo w punkcie jest równe 0
pexp(100, lambda) #b P(X<100)
pexp(500, lambda) #c P(X<500)

#Zadanie 4:
lambda = 1/2.4 #lambda = 1/expect
curve(dexp(x, lambda), 0, 20)
1 - pexp(3, lambda) #a P(x>3)
pexp(3, lambda) - pexp(2, lambda) #b P(2<x<3)=P(x>3)-P(x>2)
#SPRAWDZONE wartości oczekiwanej za pomocą całki
f=function(x) {x*dexp(x, lambda)}
integrate(f,lower=0,upper=Inf)

#Zadanie 5 ROZKŁAD NORMALNY
mu = 0.13 #średnia/wartość oczekiwana
sig = 0.005 #odchylenie standardowe
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig) #99,7% obserwacji znajduje sie w przedziale
pnorm(0.14, mu, sig)-pnorm(0.12, mu, sig) #p(0.12<x<0.14) = P(x>0.14)-P(x>0.12)

#Zadanie 6 ROZKŁAD NORMALNY
mu = 120
sig = 15
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
pnorm(135, mu, sig)-pnorm(111, mu, sig)

#Zadanie 7 ROZKŁAD NORMALNY
mu = 46.8
sig = 1.75
pnorm(50, mu, sig) #a 
1 - pnorm(48, mu, sig) #b

#Zadanie 8
# próba duża to taka która jest większa od 30
n = 100
p = 0.25
#P(X<=15)
pbinom(15,n,p) # rozklad dokladny to rozkład dwumianowy
#rozkład przybliżony (aproksymacyjny) to rozkład normalny (z odpowiednimi parametrami):
mu = n*p
sig = sqrt(n*p*(1-p))
curve(dnorm(x,mu,sig),mu-3*sig,mu+3*sig)
pnorm(15, mu, sig)

#Zadanie 9 SPRAWDZIĆ NA CHACIE
mu = 200
sig = 10
n=25
#avg (srednia) ma rozkład N z parametrami mu i sig/sqrt(n)
pnorm(202,mu,sig/sqrt(n)) - pnorm(199,mu,sig/sqrt(n)) #P(199<avg(X)<202)

#T = X1+X2+...+X25, T ma rozklad normalny N z parametrami N*mu i sqrt(n)*sig 
pnorm(5100, n*mu,sqrt(n)*sig) #P(T<=5100)

#Zadanie 10
mu = 202
sig = 14
n=64
pnorm(206,mu,sig/sqrt(n)) - pnorm(198,mu,sig/sqrt(n)) #P(198<avg(X)<206)

#Zadaneie 11
mu = 0.5
sig = 0.2
n=100
1 - pnorm(47,n*mu,sig*sqrt(n)) #P(avg(X)<47)

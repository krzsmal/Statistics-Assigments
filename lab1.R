#Zadanie 1:
sin(2*pi)
tan(pi)
log10(100)
log(15)
log(1/7, base=7)
exp(3)
64^(1/3)

#Zadanie 2:
w = seq(1, 10, 1)
sum(w)

#Zadanie 3:
x = seq(2, 20, 2)
length(x)
y = rev(x)
x*x
x^2
sqrt(sum(x^2))  #długość euklidesowa wektora
t(x)%*%y
x%*%t(y)

#Zadanie 4:
w = seq(5, 10, length=13)

#Zadanie 5:
z1=rep(c(1,2), times=5)
z2=rep(c(1,2), each=5)
z1=z1+4
z3=z2[1:length(z2)-1]
c=z1+z3
z4=z1[z1>1] #Wybranie z wektora tylko wartości większych od 1

#Zadanie 6: OPERACJE NA MACIERZY
m=rbind(c(2,3,0), c(1,-1,2), c(1,1,-1))
print(m)
m^2
m%*%m
t(m)
det(m)        #wyzacznik
solve(m)      #odwrotność
sum(diag(m))  #ślad macierzy (suma przekątnej macierzy)
b=m[3,]

#Zadanie 7:
x=seq(1, 10, by=1)
y=rev(x)
par(mfrow=c(2,2))
plot(x,y)
plot(data.frame(x, y))
plot(rbind(x, y))
plot(cbind(x, y))

#Zadanie 8: #RYSOWANIE FUNKCJI
par(mfrow=c(2,2))
curve(x^2+3*x-5, from=-3, to=4, col="orange")
curve(x^2, from=-10, to=10, col="gray")
curve(5*x^4-10*x^2-2, from=-500, to=200, col="red")
curve(2*x, from=-500, to=200, col="blue")
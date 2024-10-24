#Przykład 5 z wykładu nr 3:
#P(phat<=232/1000) 
p = 0.25
n = 1000
T =232
proba = pnorm(T/n,p,sqrt(p*(1-p)/n))
proba

#Zadanie 1:
dane = read.csv("dane_est.csv", sep=";",dec=",")
print(dane)
print(dane[,1])
diamenty = na.omit(dane$diamenty)
diamenty
#A
# Populacja - wszystkie syntentyczne diamenty wyprudkowane nowa metoda
# Proba - 12 syntentycznych diamentow wyprudkowanych nowa metoda
# Badana zmienna - karaty
#B
srednia = mean(diamenty)
wariancja = var(diamenty)
odchylenie = sd(diamenty)
srednia
wariancja
odchylenie

#D
PrzedzialUfnosciMU=function(srednia,odchylenie,sigma,liczebnosc,ufnosc){
  #srednia próby-X, odchylenie próby- S(a sigma to odchylenie dla populacji), 
  #liczebosc-n,
  alfa=1-ufnosc
  Lt=srednia-qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Pt=srednia+qt(1-alfa/2,liczebnosc-1)*odchylenie/sqrt(liczebnosc)
  Lz=srednia-qnorm(1-alfa/2)*odchylenie/sqrt(liczebnosc)
  Pz=srednia+qnorm(1-alfa/2)*odchylenie/sqrt(liczebnosc)
  return(
    if(liczebnosc<30){
      if(sigma==FALSE){print(paste("(",Lt,";",Pt,")"))}
      else {print(paste("(",Lz,";",Pz,")"))}
    }
    else {print(paste("(",Lz,";",Pz,")"))}
  )
}
PrzedzialUfnosciMU(srednia,odchylenie,FALSE,12,0.95)

t.test(diamenty, conf.level=0.95)

# z ufnoscia 0.95 przedzial (0.498;0.57)
# pokrywa prawdziwą srednią wage wszystkich syntentycznych diamentów produkowanych nową metodą

#C
PrzedzialUfnoscidlaSig2 = function(liczebnosc, odchylenie, ufnosc){
  alfa = 1 - ufnosc
  if(liczebnosc<30){
    x1 = (liczebnosc - 1)*odchylenie^2/qchisq(1-alfa/2,liczebnosc-1)
    x2 = (liczebnosc - 1)*odchylenie^2/qchisq(alfa/2,liczebnosc-1)
  }
  else{
    x1 = (liczebnosc - 1) + qnorm(1 - (1-ufnosc/2)) * sqrt(2*(liczebnosc-1))
    x2 = (liczebnosc - 1) - qnorm(1 - (1-ufnosc/2)) * sqrt(2*(liczebnosc-1))
    
  }
  return (print(paste(x1, x2)))
}
wynik = PrzedzialUfnoscidlaSig2(12, odchylenie, 0.95)

chi=sigma.test(diamenty,conf.level = 0.95)

#Z ufnoscia 95% przedział (0.001; 0.009) pokrywa nieznaną
#wariancje wagi wszystkch syntetycznych diamentów wyprodukowanyh nową metodą


c=chi$conf.int
L=c[[1]]
P = c[[2]]
L_odchylenie = sqrt(L)
P_odchylenie = sqrt(P)
# Z ufnoscia 0.95 przedzial (0.039;0.094) pokrywa nieznana wartosc odchyelania standardowego dla popoulacji sigma

#ZAD2
mleko = na.omit(dane$mleko)
#populacja to kobiety karmiace piersia
#proba to 12 kobiet karmiacych piersia
# badana zmienna to poziom pcb u kobiety
print(mleko)
srednia = mean(mleko)
wariancja = var(mleko)
odchylenie = sd(mleko)
t.test(mleko, conf.level=0.95)
#Z ufnoscia 0.95 przedzial (3.42;8.18) pokrywa nieznana srednia populacyjna mu
chi = sigma.test(mleko,conf.level = 0.95)
#Z ufnoscia 0.95 przedzial (14.95;55.15) pokrywa nieznana prawdziwa wartosc warniacji dla populacji sigma^2
c=chi$conf.int
L=c[[1]]
P = c[[2]]
L_odchylenie = sqrt(L)
P_odchylenie = sqrt(P)
# Z ufnoscia 0.95 przedzial (3.86;7.43) pokrywa nieznana wartosc odchyelania standardowego dla popoulacji sigma

#ZAD3
papierosy = na.omit(dane$papierosy)
print(papierosy)
srednia = mean(papierosy)
wariancja = var(papierosy)
odchylenie = sd(papierosy)

#COŚ TU JEST DO POPRAWY!!!!

z.test(papierosy,sigma.x = 0.7,conf.level=0.95) #WYLACZYC TEACHING EMOS. WLACZYC BDSA
#z ufnoscia 95% przedział pokrywa...
#proba ma nizsze odchylenie od populacji

#B
#zdj na tel, odp: n=84 (trzeba to napisać)

#ZAD4
wodorosty = na.omit(dane$wodorosty)
srednia = mean(wodorosty)
wariancja = var(wodorosty)
odchylenie = sd(wodorosty)
t.test(wodorosty, conf.level=0.90)
chi = sigma.test(wodorosty,conf.level = 0.90) #teaching demos


#---- ESTYMACJA PUNKTOWA ŚREDNIEJ POPULACYJNEJ ----
time = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)
x = mean(time)
#---- ESTYMACJA PUNKTOWA ŚREDNIEJ POPULACYJNEJ ----
time = c(8, 12, 26, 10, 23, 21, 16, 22, 18, 17, 36, 9)
n=length(time)
sr=mean(time)
wariancja=var(time)
a=1-0.95 #0.95 to procent z polecenia

L=sr-qt(1-a/2, n-1)*sqrt(wariancja/n)
P=sr+qt(1-a/2, n-1)*sqrt(wariancja/n)
print(paste0('(',L, ", ", P, ")"))

sd(time)
#drugi sposób: 
meanCI = t.test(time, conf.level = 0.95)$conf.int
meanCI$conf.int
#---- PRZEDZIAŁ UFNOŚCI DLA PROPROCJI ----
n=150
T=70
phat=T/n
a=1-0.99
L=(phat-qnorm(1-a/2)*sqrt(phat*(1-phat)/n))
U=(phat+qnorm(1-a/2)*sqrt(phat*(1-phat)/n))
L
U
propCI = binom.test(70, 150, conf.level = 0.99)
propCI$conf.int

propCI = binom.test(70, 150, conf.level = 0.99)
propCI$conf.int
# INTERPRETACJA: Z ufnością 99% przedział od 36,09% do 57,46% pokrywa nieznaną prawdziwą proporcję wszystkich uczniów pozytywnie nastawionych do nowego programu nauczania.

((n-1)*var(time))/qchisq(0.025, 11)


varCI = sigma.test(time, conf.level =0.95) #wariancja
varCI$conf.int

#INTERPRETACJA: Z ufnością 95% przedział od 33 do 190 godzin2 pokrywa nieznaną
#wariancję czasu korzystania z badanego urządzenia przez wszystkich pacjentów poddanych terapii.
#INTERPRETACJA: Z ufnością 95% przedział od 5 godzin i 42 minuty do 13 godzin
#i 48 minut pokrywa nieznane odchylenie standardowe czasu korzystania z badanego
#urządzenia przez wszystkich pacjentów poddanych terapii.

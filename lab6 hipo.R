# alfa  < p-value  -> brak podstaw do odrzucenia H0, HIPOTEZA H1 JEST FALSZYWA
# alfa  > p-value  -> odrzucamy H0, HIPOTEZA H1 JEST PRAWDZIWA

# NIE należy do R ->  brak podstaw do odrzucenia H0
# należy do R     ->  odrzucamy H0


dane = read.csv("dane_hip.csv", sep=";",dec=",")

# ZADANIE 1: ŚREDNIA NIE ZNANE ODCHYLENIE: -------------------------------------------------------------------------
wiatr = na.omit(dane$wiatr)
alfa=0.05
mu=4

# H0: mu <= 4 m/s
# H1: mu > 4 m/s

t.test(wiatr, mu = mu, alternative="greater")
# t=2.4186
# p-value=0.01705

# alfa > p-value
# 0.05 > 0.01705 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.05 dane potwierdzają hipotezę że 
# średnia prędkość wiatru przekracza 4 m/s.
# Okolice Darłowa nadają się do budowy elektrowni wiatrowej

# ZADANIE 2: ŚREDNIA NIE ZNANE ODCHYLENIE: ------------------------------------------------------------------------
pompa = na.omit(dane$pompa)
alfa=0.01
mu=3.5

# H0: mu >= 3.5
# H1: mu < 3.5

t.test(pompa, mu = mu, alternative="less")
# t=-1.0898
# p-value=0.1521

# alfa < p-value
# 0.01 < 0.01705 brak podstaw do odrzucenia H0

# Na poziome istotności 0.01 dane nie potwierdzają hipotezy że 
# współczynnik efektywności pompy cieplnej w gospodarstwie domowym 
# potencjalnego nabywcy jest znacznie mniejszy niż 3,5.
# Wątpliwości nabywcy nie są słuszne.




# ZADANIE 3: ŚREDNIA ZNANE ODCHYLENIE: ---------------------------------------------------------------------
morze = na.omit(dane$morze)
alfa=0.05
mu=870
sig=5

# H0: mu = 870 m
# H1: mu != 870 m

library("BSDA")
# !!!!! UWAGA SIGMA.X MOŻE NIE DZIAŁAĆ I TRZEBA DAĆ SD !!!!!
z.test(morze, sigma.x = sig, mu = mu, alternative="two.sided")
# z=-0.44721
# p-value=0.6547

# alfa < p-value
# 0.05 < 0.6547 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że 
# średnia głębokość morza w tym rejonie jest różna od 870m.
# Okolice Darłowa nadają się do budowy elektrowni wiatrowej


# ZADANIE 4: ŚREDNIA NIE ZNANE ODCHYLENIE DUŻA PRÓBA: ----------------------------------------------------------------
blaszki = na.omit(dane$blaszki)
alfa=0.02
mu=0.04
length(blaszki)

# H0: mu <= 0.04 mm
# H1: mu > 0.04 mm

library("BSDA")
zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = mu, alternative="greater")
# z=1.6409
# p-value=0.05041

# alfa < p-value
# 0.05 < 0.05041 brak podstaw do odrzucenia H0

# Na poziome istotności 0.02 dane nie potwierdzają hipotezy że 
# produkowane przez ten automat blaszki są grubsze niż nominalna grubość, 
# czyli 0.04mm

# ZADANIE 5: ŚREDNIA(A) I WARIANCJA(B) ------------------------------------------------------------------
mleko = na.omit(dane$mleko)
alfa=0.05
mu=1.7

# A) ŚREDNIA 

# H0: mu = 1.7%
# H1: mu != 1.7%

t.test(mleko, mu = mu, alternative="two.sided")
# t=-1.765
# p-value=0.1114

# alfa < p-value
# 0.05 < 0.1114 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie przeczą hipotezie że 
# średnia średnia zawartość tłuszczu w mleku jest równa 1.7%

# B) WARIANCJA
var=0.02

# H0: var >= 0.02 %^2
# H1: var < 0.02 %^2

library("TeachingDemos")
sigma.test(mleko, sigmasq=0.02, alternative="less")
# x^2=5.2 (chi2)
# p-value 0.1835

# alfa < p-value
# 0.05 < 0.1835 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że 
# wariancja zawartości tłuszczu w mleku jest mniejsza od 0.02 %^2

# ZADANIE 6: ŚREDNIA I WARIANCJA ----------------------------------------------------
kukulki = na.omit(dane$kukulki)
alfa=0.05
mu=17
sig=2.5

# A)
alfa=0.05

# 1)

# H0: mu = 17 mm
# H1: mu != 17 mm

t.test(kukulki, mu = mu, alternative="two.sided")
# t=2.8404
# p-value=0.01011

# alfa > p-value
# 0.05 > 0.01011 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.05 dane potwierdzają hipotezę że 
# średnia długość jaj zniesionych przez kukułki jest różna od średniej
# długości jaj strzyżka, która wynosi 17mm.
# Przyrodnicy nie mają racji.

# 2) 
var = sig^2

# H0: var = 6.25 mm^2
# H1: var != 6.25 mm^2

library("TeachingDemos")
sigma.test(kukulki, sigmasq=var, alternative="two.sided")
# x^2=15.438 (chi2)
# p-value=0.4984

# alfa < p-value
# 0.05 < 0.4984 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że 
# wariancja długość jaj zniesionych przez kukułki jest różna od wariancji
# długości jaj strzyżka.
# Przyrodnicy mają racjię.

# B) PRZEDZIAŁ UFNOŚCI DLA ŚREDNIEJ ZE ZNANYM ODCHYLENIEM
ufnosc=0.95
alfa=1-ufnosc
odchylenie=2.5
z.test(kukulki, sd=odchylenie, conf.level=1-alfa)
#z.test(kukulki, sigma.x=odchylenie, conf.level=1-alfa)

# Z ufnością 95% możemy powiedzieć, że przedział od 17.2918 mm do 19.43068 mm
# długości jaj pokrywa nieznaną prawdziwą średnią długość wszystkich jaj
# podrzuconych strzyżyką 

# ZADANIE 7: !!! WARIANCJA BEZ DANYCH !!!--------------------------------------------------------------
mu=55
sig=18
mu_p=60
sig_p=20
alfa=0.01
n=100

# H0: mu <= 55
# H1: mu > 55

zsum.test(mu_p, sig_p, 100, mu = mu, alternative='greater')
# z=2.5
# p-value=0.00621

# alfa > p-value
# 0.01 > 0.00621 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.01 dane potwierdzają hipotezę że 
# średnie zanieczyszczenie na zbadanym terenie przemysłowym
# jest większe niż dopuszczalne średnie zanieczyszczenie na terenie przemysłowym.
# Fabryka działa niezgodnie z prawem.

var=sig^2

# H0: var = 324
# H1: var != 324

#Chi2 = (n-1)*var_próby / var_populacji
chi2=((n-1)*sig_p^2)/sig^2
# chi2=122.2222

# Z PREZKI WYBRAĆ ODPOWIEDNI OBSZAR KRYTYCZNY
# UWAGA TUTAJ ARGUMENTY TRZEBA PODAĆ NA ODWRÓT NIŻ JEST WE WZORZE W PREZCE

qchisq(alfa/2, n-1) # 66.51011
qchisq(1-alfa/2, n-1) # 138.9868
# R = (0; 66.51011) sum (138.9868; inf)

# Chi2 nie należy do R więc nie mamy podstaw do odrzucenia H0
# Na poziomie istotności alfa = 0.01 dane nie potwierdzają hipotezy,
# że wariancja pomiarów jest różna od 18^2.



# ZADANIE 8: PROPORCJA POPULACYJNA --------------------------------------------------------------

# JAK NIE MA DANYCH TO SPRAWDZIĆ JAK ADAM TO ZROBIŁ

alfa=0.05
n=2500
T=1600
p=0.6

phat = T/n
z = (phat-p)/(sqrt(p*(1-p))/sqrt(n))
# z=4.082483

# H0: p = 0.6
# H1: p != 0.6

binom.test(T, n, p = p, alternative="two.sided")
# p-value = 0.00004864

# alfa > p-value
# 0.05 > 0.00004864 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.05 dane potwierdzają hipotezę że 
# osoby chcące wziąć udział w wyborach nie stanowią 60% ogółu.
# Próba przeczy twierdzeniu, że 60% ogółu osób zamierza wziąć udział w wyborach.

# ZADANIE 9: PROPORCJA POPULACYJNA ----------------------------------------------------------------
p=0.02
n=1200
T=16
alfa=0.05

phat = T/n
z = (phat-p)/(sqrt(p*(1-p))/sqrt(n))
# z=-1.649572

# H0: p => 0.02
# H1: p < 0.02

binom.test (T , n, p = p, alternative="less") 

# alfa=0.05 < p-value = 0.05451 => brak podstaw do odrzucenia H0

# na poziomie istotności alfa=0.05 dane nie potwierdzają hipotezy, że frakcja ta w badanej fermie jest mniejsza

# ZADANIE 10: PROPORCJA POPULACYJNA --------------------------------------
n=1100
T=1000
alfa=0.05
p=0.9

phat = T/n
z = (phat-p)/(sqrt(p*(1-p))/sqrt(n))
# z=1.005038

# H0: p <= 0.9
# H1: p > 0.9

prop.test (T, n, p = p, alternative="greater")
# p-value=0.1698

# alfa < p-value
# 0.05 < 0.1698 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że 
# procent Polaktów, którzy nei przecztyali żadneij książki jest większy niż
# 90%.

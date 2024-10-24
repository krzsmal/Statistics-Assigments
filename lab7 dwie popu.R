# alfa  < p-value  -> brak podstaw do odrzucenia H0, HIPOTEZA H1 JEST FALSZYWA
# alfa  > p-value  -> odrzucamy H0, HIPOTEZA H1 JEST PRAWDZIWA

# NIE należy do R ->  brak podstaw do odrzucenia H0
# należy do R     ->  odrzucamy H0

dane = read.csv("DwiePopulacje.csv", sep=";")

# ZADANIE 1: ŚR, WAR, PRZEDZIAŁ ŚR ----------------------------
cel1 = na.omit(dane$cel1)
cel2 = na.omit(dane$cel2)
alfa=0.02

# A) !!! ŚREDNIE SIĘ RÓŻNIĄ !!!

# H0: mu1 = mu2
# H1: mu1 != mu2

t.test(cel1, cel2, var.equal=TRUE, conf.level=(1-alfa))
# t = -1.5398
# p-value = 0.1352

# alfa < p-value
# 0.02 < 0.1352 brak podstaw do odrzucenia H0

# Na poziome istotności 0.02 dane nie potwierdzają hipotezy że 
# przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
# zawartości celulozy dla regionu II.


# B) !!! WARIANCJE SĄ RÓŻNE !!!

# H0: var1 = var2
# H1: var1 != var2

library(PairedData)
var.test(cel1, cel2, alternative = "two.sided")
# F = 0.4786
# p-value = 0.3225

# alfa < p-value
# 0.02 < 0.3225 brak podstaw do odrzucenia H0

# Na poziome istotności 0.02 dane nie potwierdzają hipotezy o różności wariancji.
# Założenie o równości wariancji było słuszne.

# C) !!! PRZEDZIAŁ UFNOŚCI (jak 0 należy do niego to git) !!!
# OCEŃ METODĄ PRZEDZIAŁOWĄ

alfa = 0.98

t.test(cel1, cel2, var.equal = TRUE, conf.level = alfa) # TRUE jest z testu wariancji
#przedział ufności (-13.519332, 3.143023)

# Na poziomie ufności 98% przedział (-13.519332, 3.143023) pokrywa nieznaną 
# prawdziwą różnice średnich zawartości celulozy w drewnie w dwóch regionach.
# Ponieważ przedzial ufności pokrywa wartość 0, zatem nie mamy podstaw 
# do odrzucenia H0.

# ZADANIE 2: WAR i ŚR -----------------------------------------
trad = na.omit(dane$tradycyjna)
nowa = na.omit(dane$nowa)
alfa = 0.1

# H0: var1 = var2
# H1: var1 != var2

library(PairedData)
var.test(trad, nowa, alternative = "two.sided")
# F = 0.5354
# p-value = 0.3613

# alfa < p-value
# 0.1 < 0.3613 brak podstaw do odrzucenia H0

# Na poziome istotności 0.1 dane nie potwierdzają hipotezy o różności wariancji.


# H0: mu_t - mu_n <= 0
# H1: mut_t - mu_n > 0

# TRUE na podstawie testu zgodności 
t.test(trad, nowa, var.equal = TRUE, mu=0, alternative="greater")
# t = -0.29815
# p-value = 0.6139

# alfa < p-value
# 0.1 < 0.6139 brak podstaw do odrzucenia H0

# Na poziome istotności 0.1 dane nie potwierdzają hipotezy że 
# średni czas budowy metodą tradycyjną jest dłuższy od średniego czasu budowy
# nową metodą.

# ZADANIE 3: WAR i ŚR ---------------------------
publ = na.omit(dane$publiczny)
pryw = na.omit(dane$prywatny)
alfa = 0.1

# H0: var1 = var2
# H1: var1 != var2

library(PairedData)
var.test(publ, pryw, alternative = "two.sided")
# F = 0.33605
# p-value = 0.08687

# alfa > p-value
# 0.1 > 0.08687 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę o 
# różności wariancji.


# H0: mu_pub - mu_pry => 0
# H1: mu_pub - mu_pry < 0

t.test(publ, pryw, var.equal = FALSE, mu=0, alternative="less")
# t = -2.1021
# p-value = 0.023

# H0: mu_pry - mu_pub <= 0
# H1: mu_pry - mu_pub > 0

# TO SAMO DRUGI RAZ:
# H0: mu_pry - mu_pub <= 0
# H1: mu_pry - mu_pub > 0
t.test(pryw, publ, var.equal = FALSE, mu=0, alternative="greater")
# t = 2.1021
# p-value = 0.023

# alfa > p-value
# 0.1 > 0.023 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę że 
# publiczne źródła finansowania udzielają, przeciętnie rzecz biorąc,
# mniejszych kredytów.

# ZADANIE 4: WAR (REGULARNOŚĆ WYNIKÓW) --------------------------------------------------------------
# MNIEJSZA WARIANCJA = WIĘKSZA REGULARNOŚĆ

z1 = na.omit(dane$zawodnik1)
z2 = na.omit(dane$zawodnik2)
alfa = 0.05

# H0: var_z1 - var_z2 => 0
# H1: var_z1 - var_z2 < 0

var.test(z1, z2, alternative="less")
# F = 0.59781
# p-value = 0.2108

# alfa < p-value
# 0.05 < 0.2108 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy
# o większej regularności wyników pierwszego zawodnika. 

# ZADANIE 5: WAR i ŚR --------------------------------------------------------------
l1 = na.omit(dane$L1)
l2 = na.omit(dane$L2)
alfa = 0.1

# H0: var_l1 != var_l2
# H1: var_l1 != var_l2

library(PairedData)
var.test(l1, l2, alternative = "two.sided")
# F = 0.72654
# p-value = 0.6412

# alfa < p-value
# 0.1 < 0.6412 brak podstaw do odrzucenia H0

# Na poziome istotności 0.02 dane nie potwierdzają hipotezy o różności wariancji.


# H0: mu_l1 - mu_l2 <= 0
# H1: mu_l1 - mu_l2 > 0

t.test(l1, l2, var.equal = TRUE, mu=0, alternative="greater")
# t = 1.4352
# p-value = 0.08234

# alfa > p-value
# 0.1 > 0.08234 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę że 
# średni czas działania leku L1 jest istotnie dłuższy niż dla leku L2.



# ZADANIE 6: PROPORCJA POPULACYJNA --------------------------------------
n_pol = 1200
p_pol = 0.78
T_pol = n_pol * p_pol 
n_am = 2000
p_am = 0.8
T_am = n_am * p_am

# A) PRZEDZIAŁ UFNOŚCI
prop.test(c(T_pol, T_am), c(n_pol, n_am), conf.level = 0.9)
# przedział ufności (-0.045229569  0.005229569)

# Z ufnością 90% przedział (-4.53%; 0.53%) pokrywa nieznaną prawdziwą różnice
# proporcji osób zadowolonch z pracy w Polsce i w USA.


# B) RÓŻNICA PROPORCJI
alfa = 0.1

# H0: p_pl >= p_am
# H1: p_pl < p_am

prop.test(c(T_pol, T_am), c(n_pol, n_am), alternative = "less")
# p-value = 0.09583

# alfa > p-value
# 0.1 > 0.09583 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę że proporcja
# zadowolonych Polaków jest mniejsza niż zadowolonych Amerykanów.


# C) TO JEST Z LAB6  HIPOTEZY
p = 0.75

# H0: p_pl <= 0.75
# H1: p_pl > 0.75

prop.test (T_pol, n_pol, p = p, alternative="greater")
# p-value = 0.008975

# alfa > p-value
# 0.1 > 0.09583 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę że
# procent Polaków zadowolonych z pracy jest większy niż 75.
# Socjolodzy mieli rację.

# ZADANIE 7: PROPORCJA POPULACYJNA -----------------------------------------------------
T_azja = 313
T_afryka = 145
n_azja = 313 + 28
n_afryka = 145 + 56

# A) PORÓWNANIE PROPORCJI
alfa = 0.05

# H0: p_azja = p_afryka
# H1: p_azja != p_afryka

prop.test(c(T_azja, T_afryka), c(n_azja, n_afryka), alternative = "two.sided")
# p-value = 2.189e-09

# alfa > p-value
# 0.1 > 2.189e-09 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.1 dane potwierdzają hipotezę że procent występowania 
# malarii typu A w Azji jest różny od procentu występowania 
# malarii typu A w Afryce. Częstotliwość występowania malarii typu A zależy od regionu

# B) PRZEDZIAŁ UFNOŚCI
alfa = 0.95

prop.test(c(T_azja, T_afryka), c(n_azja, n_afryka), conf.level = alfa)
#przedział ufności (0.1240564 0.2689346)

# Na poziomie ufności 95% przedział (0.1240564 0.2689346) pokrywa nieznaną 
# prawdziwą różnice badanych częstości występowania malarii typu A.
# Ponieważ przedzial ufności nie pokrywa wartość 0 odrzucamy H0 

# ZADANIE 8: PROPORCJA POPULACYJNA -----------------------------------------------------------------------------
T_11 = 73
T_30 = 102
n_11 = 105
n_30 = 110

# A) PORÓWNANIE PROPOROCJI
alfa = 0.05

# H0: p_11 = p_30
# H1: p_11 != p_30

prop.test(c(T_11, T_30), c(n_11, n_30), alternative = "two.sided")
# p-value = 2.728e-05

# alfa > p-value
# 0.05 > 2.728e-05 więc odrzucam hipotezę H0 i przyjmuję H1

# Na poziome istotności 0.05 dane potwierdzają hipotezę że proporcja 
# przeżywalności zależy od temperatury.

# B) PRZEDZIAŁ UFNOŚCI
alfa = 0.95

prop.test(c(T_11, T_30), c(n_11, n_30), conf.level = alfa)
#przedział ufności (-0.3418749 -0.1221943)

# Na poziomie ufności 95% przedział (-0.3418749 -0.1221943) pokrywa nieznaną 
# prawdziwą różnice proporcji przeżywalności w badanych temperaturach.
# Ponieważ przedzial ufności nie pokrywa wartość 0 odrzucamy H0

# ZADANIE 9: DWA ZESTAWY DANYCH DO ODJECIA ------------------------------------------
alfa = 0.05
przed = c(15, 4, 9, 9, 10, 10, 12, 17, 14)
po = c(14, 4, 10, 8, 10, 9, 10, 15, 14)
roznica=przed-po

# H0: mu = 0
# H1: mu != 0

t.test(roznica, conf.level = 1-alfa)
# t = 2
# p-value = 0.08052

# alfa < p-value
# 0.05 < 0.2108 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że
# dany rodzaj leku zmienia wartości określonego parametru biochemicznego. 
# Lekarz nie ma racji.

# ZADANIE 10: DWA ZESTAWY DANYCH DO ODJECIA -----------------------------------
alfa = 0.1
wyzej = c(6.55, 5.98, 5.59, 6.17, 5.92, 6.18, 6.43, 5.68)
nizej = c(6.78, 6.14, 5.80, 5.91, 6.10, 6.01, 8.18, 5.88)
roznica=wyzej-nizej

# A) ŚREDNIA

# H0: mu = 0
# H1: mu != 0

t.test(roznica, conf.level = 1-alfa)
# t = -1.3111
# p-value = 0.2312



# alfa < p-value
# 0.1 < 0.2312 brak podstaw do odrzucenia H0

# Na poziome istotności 0.05 dane nie potwierdzają hipotezy że
# pH wody zależy od głębokości.

# B) PRZEDZIAŁ UFNOŚCI

t.test(roznica, conf.level = 1-alfa)
# Na poziomie ufności 90% przedział (-0.702948  0.127948) pokrywa nieznaną 
# prawdziwą różnice średniego pH na różnych głębokościach
# Ponieważ przedzial ufności pokrywa wartość 0, zatem nie mamy podstaw 
# do odrzucenia H0.


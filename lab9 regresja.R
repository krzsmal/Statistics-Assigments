# ZADANIE 1: --------------
dane = read.csv("Reg_chemikalia.csv", sep=";", dec=",")
dane

surowiec_x = dane$surowiec
produkt_y = dane$produkt

# A) Narysuj wykres punktowy
plot(surowiec_x, produkt_y)

# B) Wyznacz i zinterpretuj kowariancję próbkową
cov(surowiec_x, produkt_y)
# S_XY = 138.4889
# Kowarjacja jest różna od zera, więc istnieje liniowa zależność
# między ilości zużytego surowca, a końcową wielkością produkcji środków chemicznych.
# Ponieważ kawarjacja jest dodatnia, zatem wraz ze wzrostem ilości
# zużytego surowca wzrasta końcowa wielkość produkcji.

# C) Wyznacz i zinterpretuj współczynnik korelacji
cor(surowiec_x, produkt_y)
# r_XY = 0.8953468
# współczynnik korelacji r=0.895>|0.8| zatem zatem istnieje bardzo silny związek
# liniowy między ilością zużytego surowca, a wielkością produkcji

# D) Wyznacz ocenę prostej regresji
prosta=lm(produkt_y~surowiec_x)
prosta
# (y = b0 + b1*x, gdzie b1 to jest współczynnik regresji liniowej)
# y = 22.41 + 3.61*x - równanie prostej regresji liniowej
# między wielkością produkcji a ilością zużytego surowca

# E) Dodaj do wykresu punktowego prostą regresji
summary(prosta)
plot(surowiec_x, produkt_y); abline(prosta)

# F) W jaki sposób zmieni się wielkość produkcji, jeśli ilość surowca wzrośnie o 1 litr?
# Jeśli ilość surowca wzrośnie o 1 litr to końcowa wielkość
# produkcji środków chemicznych wzrośnie 3.61 kg.
# (interpretacja współczynnika regresji liniowej)

# G) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 20 litrów surowca?
# H) Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 15 litrów surowca?
predict(prosta, data.frame(surowiec_x=c(20,15)))
# Jeśli zużyjemy do produkcji 20 litrów surowca to końcowa wielkość
# produkcji wyniesie 94.79 kg,
# a jeśli zużyjemy do produkcji 15 litrów surowca to
# końcowa wielkośc produkcji wyniesie 76.69048 kg

# I) Oceń dopasowanie prostej regresji do danych 
# !!! (na potrzeby ćwiczeń >70% to dobre dopasowanie) !!!
summary(prosta)
# współczynnik determinacji R-squared = 0.8016*100% = 80,16%
# prosta regresji liniowej jest dobrze dopasowana do danych
# koncowa wielkosc produkcji srodków chemicznych jest wyjasniona
# w ok 80% przez ilości zużytego surowca

# H) Zweryfikuj test o istotności regresji. Przyjmij poziom istotności 5%. Zinterpretuj wynik.

# H0: b1=0 (Regresja liniowa jest nieistotna)
# H1: b1!=0 (Regresja liniowa jest istotna)

anova(prosta)
# p-value = 0.0004617

# alfa > p-value
# 0.05 > 0.0004617 więc odrzucamy H0 i przyjmujemy H1

# Na poziomie istotności 5% dane potwierdzają hipotezę że regresja liniowa jest istotna.

# ZADANIE 2: --------------------------------
dane = read.csv("Reg_urzadzenie.csv", sep=";")
dane

efektywnosc_x = dane$efektywnosc
zywotnosc_y = dane$zywotnosc

# A)
plot(efektywnosc_x, zywotnosc_y)

# B) Oblicz i zinterpretuj kowariancję
cov(efektywnosc_x, zywotnosc_y)
# S_XY = -8.652778
# Kowarjacja jest różna od zera, więc istnieje liniowa zależność
# między żywotnością i efektywnościa urządzenia.
# Ponieważ kawarjacja jest ujemna, zatem wraz ze wzrostem efektywności
# wzrasta końcowa żywotność.


# C) Oblicz i zinterpretuj współczynnik korelacji
cor(efektywnosc_x, zywotnosc_y)
# r_XY = -0.9094164
# współczynnik korelacji r=-0.9094164, |r|>0.8 zatem
# istnieje bardzo silny związek liniowy między efektywnością a żywotnością

# D) Wyznacz ocenę prostej regresji
prosta=lm(zywotnosc_y~efektywnosc_x)
prosta
# y = 18.8823 - 0.8629*x - równanie prostej regresji liniowej
# między żywotnościa a efektywnością

# E) Jak zmieni się żywotność urządzenia jeśli efektywność wzrośnie o 1 element?
# Jeśli efektywność wzrośnie o 1 element to końcowa żywotność
# zmaleje o 0.8629 miesiąca.

# F) Oszacuj żywotność urządzenia przy efektywności 11 elementów.
# G) Oszacuj żywotność urządzenia przy efektywności 19 elementów.
predict(prosta, data.frame(efektywnosc_x=c(11,19)))
# Przy efektywności równej 11 elementów żywotność wyniesie 9.390582 miesiąca,
# a przy efektywności równej 19 elementów źywotność wyniesie 2.487535 miesiąca.

# H) Oceń dopasowanie prostej regresji.
summary(prosta)
# współczynnik determinacji R-squared = 0.827*100% = 82,7%
# prosta regresji liniowej jest dobrze dopasowana do danych
# żywotność urządzenia jest wyjaśniona w ok 82% przez jego efektywność

# I) Zweryfikuj test istotności regresji.
# H0: b1=0 (Regresja liniowa jest nieistotna)
# H1: b1!=0 (Regresja liniowa jest istotna)

anova(prosta)
# F = 33.471
# p-value = 0.0006735 

# alfa > p-value
# 0.01 > 0.0006735 więc odrzucamy H0 i przyjmujemy H1

# Na poziomie istotności 1% dane potwierdzają hipotezę że regresja liniowa jest istotna.

# ZADANIE 3: ------------------------------------------
dane = read.csv("Reg_arszenik.csv", sep=";", dec=",")
dane

pH_x = dane$pH
arszenik_y = dane$arszenik

# A) 
plot(pH_x, arszenik_y)

# B) Oblicz i zinterpretuj kowariancję i współczynnik korelacji
cov(pH_x, arszenik_y)
# S_XY = -18.32216
# Kowarjacja jest różna od zera, więc istnieje liniowa zależność
# między ilością usuniętego arszeniku, a zakwaszeniem gleby (pH).
# Ponieważ kawarjacja jest ujemna, zatem wraz ze wzrostem zakwaszenia ziemi
# maleje ilość usuniętego arszeniku.

cor(pH_x, arszenik_y)
# r_XY = 0.9504953
# współczynnik korelacji r=0.9504953, |-0.9504953| > 0.8 zatem zatem istnieje bardzo silny związek
# liniowy między zakwaszeniem ziemi a ilością usuniętego arszeniku

# C) Wyznacz prostą regresji 
prosta=lm(arszenik_y~pH_x)
prosta
# (y = b0 + b1*x, gdzie b1 to jest współczynnik regresji liniowej)
# y = 190.27 - 18.03*x - równanie prostej regresji liniowej
# między zakwaszeniem ziemi a ilością usuniętego arszeniku

# D) W jaki sposób zmieni się ilość usuniętego...
# Jeśli pH gleby wzrośnie o 1 to końcowa wielkość
# ilość usuniętego przez proces arszeniku zmaleje 18,03 %.


# E) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 7,5?
# F) Ile arszeniku zostanie usunięte, jeśli pH gleby wyniesie 9?
predict(prosta, data.frame(pH_x=c(7.5, 9)))
# Gdy pH gleby wyniesie 7,5 to 55,01% arszeniku zostanie usunięte.
# Gdy pH gleby wyniesie 9 to 27.96 % arszeniku zostanie usutnięte.

# G) Jak dobra jest ocena liniowa regresji
summary(prosta)
# współczynnik determinacji R-squared = 0.9034*100% = 90,34%
# prosta regresji liniowej jest dobrze dopasowana do danych
# koncowa ilość usuniętego arszeniku jest wyjasniona
# w ok 90% przez zakwaszenie gleby

# H) Zweryfikuj test istotności regresji.
# H0: b1=0 (Regresja liniowa jest nieistotna)
# H1: b1!=0 (Regresja liniowa jest istotna)

anova(prosta)
# p-value = 1.552e-09

# alfa > p-value
# 0.01 > 1.552e-09 więc odrzucamy H0 i przyjmujemy H1

# Na poziomie istotności 1% dane potwierdzają hipotezę że regresja liniowa jest istotna.


# ANOVA() można zrobić tylko jak wariancje są jednorodne
# Czyli jak nie są jednorodne to hipoteza o równych średnich albo braku istotnuch różnic jest fałszywa

# alfa  < p-value  -> brak podstaw do odrzucenia H0, HIPOTEZA H1 JEST FALSZYWA
# alfa  > p-value  -> odrzucamy H0, HIPOTEZA H1 JEST PRAWDZIWA


# ZADANIE 1 --------------------------
cisnienie = read.csv("Anova_cisnienie.csv", sep=";", dec=",")

obiekty = rep(names(cisnienie), c(
            length(na.omit(cisnienie$Niskie)),
            length(na.omit(cisnienie$Srednie)),
            length(na.omit(cisnienie$Silne)),
            length(na.omit(cisnienie$BardzoSilne))
            ))
            
wyniki = c(
            na.omit(cisnienie$Niskie),
            na.omit(cisnienie$Srednie),
            na.omit(cisnienie$Silne),
            na.omit(cisnienie$BardzoSilne)
          )

cisnienieTest = data.frame(obiekty,wyniki)

alfa = 0.05

# średnie dla poszczególnych prób (ŚREDNIE PRÓBKOWE)
srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty),mean)
srednie

# WARIANCJA:

# H0: var1 = var2 = var3 = var4 H1: ~H0
# H0: sig_1^2=sig_2^2=sig_3^2=sig_4^2  H1: ~H0
# H0: wariancje są jednorodne H1: ~H0
  
bartlett.test(wyniki~obiekty)
# p-value = 0.5009

# alfa < p-value
# 0.05 < 0.5009 brak podstaw do odrzucenia H0

# Na poziomie istotności 5% nie ma podstaw do odrzucenia hipotezy
# o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.


# ŚREDNIA:

#H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 2.2665
# p-value = 0.09735 ( !!! JEST ZAPISANE JAKO Pr(>F) !!! )

# SPOSÓB 1 SZYBSZY: 
# alfa < p-value
# 0.05 < 0.09735, więc brak podstaw do odrzucenia H0

# SPOSÓB 2:
n=length(obiekty) #40
k=length(cisnienie) #4
qf(1-alfa, k-1, n-k)
# F = 2.2665 < F_t = 2.866266 -> brak podstaw do odrzucenia H0

# Na poziomie istotności 5% nie mamy podstaw do odrzucenia H0 zatem
# cisnienie nie ma wpływu na wielkość produkcji.

# ZADANIE 2: ------------------------------
kopalnie = read.csv("Anova_kopalnie.csv", sep=";", dec=",")

obiekty = rep(names(kopalnie), c(
  length(na.omit(kopalnie$K1)),
  length(na.omit(kopalnie$K2)),
  length(na.omit(kopalnie$K3)),
  length(na.omit(kopalnie$K4)),
  length(na.omit(kopalnie$K5))
))

wyniki = c(
  na.omit(kopalnie$K1),
  na.omit(kopalnie$K2),
  na.omit(kopalnie$K3),
  na.omit(kopalnie$K4),
  na.omit(kopalnie$K5)
)

kopalnieTest = data.frame(obiekty,wyniki)

# H0: var1 = var2 = var3 = var4 H1: ~H0

bartlett.test(wyniki~obiekty)
# p-value = 0.03188

# alfa < p-value
# 0.01 < 0.03188 brak podstaw do odrzucenia H0

# Na poziomie istotności 1% nie ma podstaw do odrzucenia hipotezy
# o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

#H0: mu1 = mu2 = mu3 = mu4 = mu5 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 0.9563
# p-value = 0.4594

# alfa < p-value
# 0.01 < 0.09735, więc brak podstaw do odrzucenia H0

# Na poziomie istotności 1% nie mamy podstaw do odrzucenia H0 zatem
# średnie zawartości popiołu dla ekogroszku produkowanego w pięciu
# kopalniach można uznać za jednakowe

# ZADANIE 3: ------------------------
mikrometr = read.csv("Anova_mikrometr.csv", sep=";", dec=",")

obiekty = rep(names(mikrometr), c(
  length(na.omit(mikrometr$mikrometrI)),
  length(na.omit(mikrometr$mikrometrII)),
  length(na.omit(mikrometr$mikrometrIII))
))

wyniki = c(
  na.omit(mikrometr$mikrometrI),
  na.omit(mikrometr$mikrometrII),
  na.omit(mikrometr$mikrometrIII)
)

mikrometrTest = data.frame(obiekty,wyniki)
#H0: mu1 = mu2 = mu3 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 3.377
# p-value = 0.06859

# alfa < p-value
# 0.05 < 0.06859, więc brak podstaw do odrzucenia H0

# Na poziomie istotności 5% nie mamy podstaw do odrzucenia H0 zatem
# wybór mikrometru ma wpływ na uzyskane wyniki

# ZADANIE 4: -------------------------------------------------------
sportowcy = read.csv("Anova_sportowcy.csv", sep=";")

obiekty = rep(names(sportowcy), c(
  length(na.omit(sportowcy$Niepalacy)),
  length(na.omit(sportowcy$Lekkopalacy)),
  length(na.omit(sportowcy$Sredniopalacy)),
  length(na.omit(sportowcy$Duzopalacy))
))

wyniki = c(
  na.omit(sportowcy$Niepalacy),
  na.omit(sportowcy$Lekkopalacy),
  na.omit(sportowcy$Sredniopalacy),
  na.omit(sportowcy$Duzopalacy)
)

sportowcyTest=data.frame(obiekty,wyniki)
sportowcyTest

# A)

# H0: var1 = var2 = var3 = var4 H1: ~H0

bartlett.test(wyniki~obiekty)
# p-value = 0.148

# alfa < p-value
# 0.01 < 0.148 brak podstaw do odrzucenia H0

# Na poziomie istotności 1% nie ma podstaw do odrzucenia hipotezy
# o jednorodności wariancji.
# Zatem zakładamy, że wariancje są jednorodne i możemy przeprowadzić ANOVE.

#H0: mu1 = mu2 = mu3 = mu4 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 2.2665
# p-value = 0.003979  

# alfa > p-value
# 0.01 > 0.003979 , więc odrzucamy H0 i przyjmujemy H1

# Na poziomie istotnosci 1% odrzucamy H0 stwierdzamy zatem,
# że palenie papierosow moze wplywac na rytm zatokowy serca


# B) (GRUPY JEDNORODNE)
TukeyHSD(aov(wyniki~obiekty))

par(mar = c(5, 12, 4, 2) + 0.1) #zwiększenie lewego marginesu (druga wartość to lewy margines)
plot(TukeyHSD(aov(wyniki~obiekty)), las=1)

#Grupy ktore nie roznia sie miedzy soba istotnie:
#Lekkopalacy-Duzopalacy, Niepalacy-Duzopalacy, Sredniopalacy-Duzopalacy, 
#Sredniopalacy-Niepalacy.

# Grupy jednorodne:
# (L-D), (N-D), (Ś-D), (Ś-N)
# (N-D), (Ś-D), (Ś-N) -> grupa jednorodna (N-Ś-D)
# Ostatecznie dwie grupy jednorodne:
# L-D, N-Ś-D

# ZADANIE 5: +GRUPY JEDNORODNE ----------------------------------
chomiki = read.csv("Anova_chomiki.csv", sep=";")

obiekty = rep(names(chomiki), c(
  length(na.omit(chomiki$I)),
  length(na.omit(chomiki$II)),
  length(na.omit(chomiki$III)),
  length(na.omit(chomiki$IV))
))

wyniki = c(
  na.omit(chomiki$I),
  na.omit(chomiki$II),
  na.omit(chomiki$III),
  na.omit(chomiki$IV)
)

chomikiTest = data.frame(obiekty,wyniki)
chomikiTest

# A)
# H0: var1 = var2 = var3 = var4 H1: ~H0
bartlett.test(wyniki~obiekty)
# p-value = 0.2139

# alfa < p-value
# 0.05 < 0.2139 brak podstaw do odrzucenia H0

# Na poziomie istotności 5% nie ma podstaw do odrzucenia hipotezy
# o jednorodności wariancji.

#H0: mu1 = mu2 = mu3 = mu4 = mu5 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 3.9515 
# p-value = 0.02398 

# alfa > p-value
# 0.05 > 0.02398, więc odrzucam hipotezę H0 i przyjmuję H1 

# Na poziomie istotności 5% mamy podstawę do odrzucenia H0 zatem
# masa gruczołu tarczycowego zależy od poziomu inbredu.

# B) GRUPY JEDNORODNE
TukeyHSD(aov(wyniki~obiekty))

par(mar = c(5, 12, 4, 2) + 0.1) #zwiększenie lewego marginesu (druga wartość to lewy margines)
plot(TukeyHSD(aov(wyniki~obiekty)), las=1)

#Grupy ktore nie roznia sie miedzy soba istotnie:
# I-II, I-III, II-III, II-IV, III-IV

# Grupy jednorodne:
# (I-II), (I-III), (II-III), (II-IV), (III-IV)
# (I-II), (I-III), (II-III) -> grupa jednorodna (I-II-III)
# (II-III), (II-IV), (IV-III) -> grupa jednorodna (II-III-IV)
# Ostatecznie dwie grupy jednorodne:
# I-II-III, II-III-IV

# ZADANIE 6: +GRUPY JEDNORODNE --------------------------------------
pulapki = read.csv("Anova_pulapki.csv", sep=";")
pulapki

obiekty = rep(names(pulapki), c(
  length(na.omit(pulapki$rozsiany)),
  length(na.omit(pulapki$skoncentrowany)),
  length(na.omit(pulapki$roslina.zywicielka)),
  length(na.omit(pulapki$powietrzny)),
  length(na.omit(pulapki$gruntowy))
))

wyniki = c(
  na.omit(pulapki$rozsiany),
  na.omit(pulapki$skoncentrowany),
  na.omit(pulapki$roslina.zywicielka),
  na.omit(pulapki$powietrzny),
  na.omit(pulapki$gruntowy)
)

pulapkiTest = data.frame(obiekty,wyniki)
pulapkiTest

# A)
# H0: var1 = var2 = var3 = var4 H1: ~H0
bartlett.test(wyniki~obiekty)
# p-value = 0.06804

# alfa < p-value
# 0.05 < 0.06804 brak podstaw do odrzucenia H0

# Na poziomie istotności 5% nie ma podstaw do odrzucenia hipotezy
# o jednorodności wariancji.

#H0: mu1 = mu2 = mu3 = mu4 = mu5 H1: ~H0
anova(lm(wyniki~obiekty))
# F = 39.382  
# p-value = 3.252e-9 ~ 0 

# alfa > p-value
# 0.05 > 3.252e-9, więc odrzucam hipotezę H0 i przyjmuję H1 

# Na poziomie istotności 5% mamy podstawę do odrzucenia H0 zatem
# strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich.

# B)
TukeyHSD(aov(wyniki~obiekty))

par(mar = c(5, 12, 4, 2) + 0.1) #zwiększenie lewego marginesu (druga wartość to lewy margines)
plot(TukeyHSD(aov(wyniki~obiekty)), las=1)

#Grupy ktore nie roznia sie miedzy soba istotnie:
# R-G, RŻ-P, S-P, S-RŻ

# Grupy jednorodne:
# (R-G), (RŻ-P), (S-P), (S-RŻ)
# (RŻ-P), (S-P), (S-RŻ) -> grupa jednorodna (S-P-RŻ)
# Ostatecznie dwie grupy jednorodne:
# S-P-RŻ, R-G
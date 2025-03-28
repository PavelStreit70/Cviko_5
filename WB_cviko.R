#Načtení jednotlivých knihoven potřebných pro toto cviko

library(dplyr)
library(tidyr)
library(nortest)  # normality testing 
library(car) # Levene test
library(readxl)
library(lme4) # lmer
library(lmerTest)
library(multcomp) # Attaching package: ‘MASS’, dplyr::select masked
library(emmeans) # emmeans
library(effects) # allEffects
library(ggplot2)

#Načtení dat

library(readxl)
data_examples_1_ <- read_excel("data_examples (1).xlsx", sheet = "WB quantification")
View(data_examples_1_)

#### UKOL 1: 

# Nactete data data_examples.xlsx, list "WB quantification". 

library(readxl)
WB <- read_excel("data_examples (1).xlsx", sheet = "WB quantification")
View(WB)

# Data si adekvatne prevedte do long formatu, viz minule cviceni. Použití pivot.longer

#WB <- WB %>% 
#  pivot_longer(starts_with("Condition"), names_to = "Condition", values_to = "WB")
#WB <- WB %>% 
#  mutate(Conditon = factor(gsub("Condition_", "",Condition)))

WB <- WB %>% #Potřeba vysvětlit!
  pivot_longer(Condition_1:Condition_6, names_to = "Condition", values_to = "WB") %>% 
  separate(Condition, into = c("del", "Condition"), sep = "_", convert = TRUE) %>% 
  dplyr::select(-del)

WB <- WB %>% #Potřeba vysvětlit!
  mutate(Condition = as.factor(Condition), 
         Replicate = as.factor(Replicate))

# Otestujte, zda existuji rozdily v hodnotach WB mezi ruznymi kondicemi?

plot(WB ~ Condition, data = WB)

# Ověřte předpoklady použití zvoleného testu.

boxplot(WB ~ Condition, WB)
tapply(WB$WB, WB$Condition, hist)
tapply(WB$WB, WB$Condition, function (x) shapiro.test(x)$p.value)

leveneTest(WB ~ factor(Condition), WB) 

# data sice nejsou dokonale symetricky rozlozena, ale vzhledem k nizkemu N volime parametricke metody 

# ANOVA 

a <- aov(WB ~ Condition, WB)  
summary(a) 
TukeyHSD(a) 

# Vypočítejte popisné statistiky (např. průměr, SD, medián) hodnoceného parametru ve srovnávaných skupinách.

WB %>% 
  group_by(Condition) %>% 
  summarise(
    WB_mean = mean(WB), 
    WB_sd = sd(WB), 
    WB_median = median(WB))

# Aplikujte ekvivalentní lineární model. 

b <- lm(WB ~ Condition, WB)  
summary(b) 
summary(glht(b))  # adjustovane p-hodnoty 
emmeans(b, pairwise ~ Condition)

# Jsou mereni nezavisla? Overte graficky.

boxplot(WB ~ Replicate, WB)
boxplot(WB ~ Replicate + Condition, WB)

# Pokud nejsou nezavisla, modifikujte model

c <- lmer(WB ~ Condition + (1|Replicate), WB) 

#Srovnání modelů pomocí AIC

AIC(WB,c)

#Zbytek

anova(c)
summary(c)
summary(glht(c))
emmeans(c, pairwise ~ Condition)

#### UKOL 2: 
# Nactete data data_examples.xlsx, list "reporter assay". 

library(readxl)
RA <- read_excel("data_examples (1).xlsx", sheet = "reporter assay")
View(RA)
RA <- RA %>% mutate(
  Condition = factor(Condition))

# Otestujte, zda existuji rozdily v hodnotach Ratio mezi ruznymi kondicemi?

plot(Ratio ~ Condition, RA)

#ANOVA tu není vhodná, nutné otestování jinými testy...níže

# Nezapomente na overeni predpokladu.

kruskal.test(Ratio ~ Condition, RA)
pairwise.wilcox.test(RA$Ratio,RA$Condition, p.adjust.method = "BH")

RA %>% 
  filter(Condition == 1) %>% 
  t.test(Ratio ~ 1, mu = 1, data = .)

RA %>% 
  filter(Condition == 3) %>% 
  t.test(Ratio ~ 1, mu = 1, data = .)

#t1$p.value
#t2$p.value

#p_values <- c(t1$p.value, t2$p.value)

#p.adjust(c(t1$p.value), t2$p.value), method = "bonferroni")

#Opravený kód

# Kruskal-Wallis test
kruskal.test(Ratio ~ Condition, RA)

# Párové Wilcoxonovy testy s BH korekcí

pairwise.wilcox.test(RA$Ratio, RA$Condition, p.adjust.method = "BH")

# T-test pro Condition == 1
t1 <- RA %>% 
  filter(Condition == 1) %>% 
  t.test(Ratio ~ 1, mu = 1, data = .)

# T-test pro Condition == 3
t2 <- RA %>% 
  filter(Condition == 3) %>% 
  t.test(Ratio ~ 1, mu = 1, data = .)

# Extrakce p-hodnot
p_values <- c(t1$p.value, t2$p.value)

# Bonferroniho korekce
p_adjusted <- p.adjust(p_values, method = "bonferroni")

# Výstup opraveného kódu
p_adjusted

# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 

RA %>% 
  group_by(Condition) %>% 
  summarise(
    RA_mean = mean(Ratio), 
    RA_sd = sd(Ratio), 
    RA_median = median(Ratio))

# Aplikujte dva samostatné ekvivalentní lineární modely. 

fit1 = lm(Ratio -1 ~ 1, data = RA[RA$Condition %in% c(1),])   
summary(fit1) 
fit3 = lm(Ratio -1 ~ 1, data = RA[RA$Condition %in% c(3),])   
summary(fit3) 

names(summary(fit1))
summary(fit1)$coefficients[4]
summary(fit3)$coefficients[4]

p <- c(
  summary(fit1)$coefficients[4], 
  summary(fit3)$coefficients[4])

# a korekce viz vyse  

p_adj = p.adjust(p, method = "bonferroni", n = length(p))
p; p_adj

#### UKOL 3: 
# Nactete data data_examples.xlsx, list "qPCR". 

library(readxl)
C <- read_excel("data_examples (1).xlsx", sheet = "qPCR")
View(C)

# Seradte si faktory dle obrazku, viz minule cviceni.

C <- C %>% 
  mutate(
    Condition = factor(Condition, levels = c("wt", "#1", "#2", "#3")),
    Gene = factor(Gene, levels = c("Prickle1", "Fzd2", "Wnt9a")))

# Otestujte, zda existuji rozdily v hodnotach exprese mezi ruznymi kondicemi u ruznych genu.

boxplot(Expression ~ Condition * Gene,C)
boxplot(log(Expression) ~ Condition * Gene, data = C)
mod <- aov(Expression ~ Condition * Gene,C)
summary(mod)

TukeyHSD(mod)
TukeyHSD(mod, "Gene")
TukeyHSD(mod, "Condition")

# nesplněna homogenity rozptylů, klasický Kruskal-Walis test nepracuje se dvěma faktory vyřešíme příště modely 
# Aplikujte ekvivalentní lineární model. 

b <- lm(Expression ~ Gene * Condition, C)
anova(b)
summary(glht(b))
emmeans(b, pairwise ~ Gene)
emmeans(b, pairwise ~ Condition)
emmeans(b, pairwise ~ Gene * Condition)
emmeans(b, consec ~ Gene * Condition)
emmeans(b, consec ~ Condition * Gene)

names(emmeans(b, pairwise ~ Gene * Condition))
emmeans(b, pairwise ~ Gene * Condition)$contrasts %>% 
  as.data.frame() %>% filter(p.value < 0.05)

# Jsou mereni v ruznych replikatech nezavisla? Overte graficky. 
# Pokud je to treba modifikujte model. 

boxplot(Expression ~ Gene + Replicate, C, las = 2)
boxplot(Expression ~ Condition + Replicate, C, las = 2)

# lmer model 
c <- lmer(Expression ~ Gene * Condition + (1|Replicate), C)
anova(c)
summary(glht(c))
emmeans(c, pairwise ~ Gene * Condition)

# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 

C %>% group_by(Condition, Gene) %>% 
  summarise(expr_mean = mean(Expression), 
            expr_sd = sd(Expression),
            expr_median = median(Expression))

#### UKOL 4: 
# Nactete data data_examples.xlsx, list "immunofluorescence". 

library(readxl)
D <- read_excel("data_examples (1).xlsx", sheet = "immunofluorescence")
View(D)
str(D)

# Otestujte, zda se lisi frekvence u ruznych kondic.
# Napoveda: pracujte pouze s "axis.duplication", hodnota "normal je doplnek do 100 %.

barplot(axis.duplication ~ Condition, data = D)

# Jakeho typu je vstupni promenna (kategoriální) Zvolte spravny test (fischerův nebo chí sq)

freq_table <- table(D$axis.duplication)
chisq.test(freq_table)

chisq.test(D$axis.duplication)

# Aplikujte ekvivalentní lineární model včetně post-hoc testů. 

### PRO RYCHLIKY: 
# Vyberte vhodny test pro hodnoceni dat na listu "immunofluorescence B". 
# Aplikujte ho. 

library(readxl)
E <- read_excel("data_examples (1).xlsx", sheet = "immunofluorescence B")
View(E)

chisq.test(E %>% select(-Condition))

#Nebo takto - vychází stejně

# Vytvoření matice četností (kontingenční tabulky)
freq_table <- as.matrix(RA[, 2:4])  # Sloupce wt, mild, severe

# Nastavení názvů řádků (podmínky)
rownames(freq_table) <- RA$Condition

# Chí-kvadrát test
chisq.test(freq_table)

chisq.residuals <- chisq.test(freq_table)$residuals
print(chisq.residuals)

#Graficky

E_long <- E %>% 
  pivot_longer(cols = c(wt, mild, severe), names_to = "group") 
  barplot(value ~ group + Condition, data = E_long)
  
# Aplikujte ekvivalentní zobecněný lineární model včetně post-hoc testů.


############  řešení pro Rychlíky z ChatGPT
# Předpokládáme, že E_long je již vytvořený v dlouhém formátu
  
  E_long <- E %>% 
    pivot_longer(cols = c(wt, mild, severe), names_to = "group")
  
# Vytvoření matice pro barplot
  
  barplot_matrix <- tapply(E_long$value, list(E_long$group, E_long$Condition), sum)
  
#Vytvoření barplotu
  
  barplot(barplot_matrix, beside = TRUE, col = c("blue", "orange", "red"), 
          legend.text = colnames(barplot_matrix), args.legend = list(title = "Condition"))
  
# Přidání názvů a popisků
  
  title(main = "Bar Plot of Groups by Condition", xlab = "Group", ylab = "Value")

# Vysledky interpretujte. 

#Rozdíl mezi očekávanými a skutečnými četnostmi
#Statisticky významný rozdíl - skupiny se mezi sebou liší





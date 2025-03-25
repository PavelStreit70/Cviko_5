#Načtení jednotlivých knihoven potřebných pro toto cviko

library(dplyr)
library(tidyr)
library(nortest)  # normality testing 
library(car) # Levene test

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

t1$p.value
t2$p.value

p_values <- c(t1$p.value, t2$p.value)

p.adjust(c(t1$p.value), t2$p.value), method = "bonferroni")

# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 

RA %>% 
  group_by(Condition) %>% 
  summarise(
    RA_mean = mean(Ratio), 
    RA_sd = sd(Ratio), 
    RA_median = median(Ratio))


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

# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 

C %>% group_by(Condition, Gene) %>% 
  summarise(expr_mean = mean(Expression), 
            expr_sd = sd(Expression),
            expr_median = median(Expression))

#### UKOL 4: 
# Nactete data data_examples.xlsx, list "immunofluorescence". 

# Otestujte, zda se lisi frekvence u ruznych kondic. 
# Napoveda: pracujte pouze s "axis.duplication", hodnota "normal je doplnek do 100 %. 
# Jakeho typu je vstupni promenna. Zvolte spravny test.  



### PRO RYCHLIKY: 
# Vyberte vhodny test pro hodnoceni dat na listu "immunofluorescence B". 
# Aplikujte ho. 
# Vysledky interpretujte. 





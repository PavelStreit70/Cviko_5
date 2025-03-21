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

WB <- WB %>% 
  pivot_longer(Condition_1:Condition_6, names_to = "Condition", values_to = "WB") %>% 
  separate(Condition, into = c("del", "Condition"), sep = "_", convert = TRUE) %>% 
  dplyr::select(-del)

WB <- WB %>% 
  mutate(Condition = as.factor(Condition), 
         Replicate = as.factor(Replicate))

# Otestujte, zda existuji rozdily v hodnotach WB mezi ruznymi kondicemi?

plot(WB ~ Condition, data = WB)

# Ověřte předpoklady použití zvoleného testu.

boxplot(WB ~ Condition, WB)
tapply(WB$WB, WB$Condition, hist)
tapply(WB$WB, WB$Condition, function (x) shapiro.test(x)$p.value)

leveneTest(WB ~ factor(Condition), WB) 

# Vypočítejte popisné statistiky (např. průměr, SD, medián) hodnoceného parametru ve srovnávaných skupinách.

WB %>% 
  group_by(Condition) %>% 
  summarise(
    WB_mean = mean(WB), 
    WB_sd = sd(WB), 
    WB_median = median(WB))
 

#### UKOL 2: 
# Nactete data data_examples.xlsx, list "reporter assay". 

# Otestujte, zda existuji rozdily v hodnotach Ratio mezi ruznymi kondicemi? 
# Nezapomente na overeni predpokladu. 
# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 


#### UKOL 3: 
# Nactete data data_examples.xlsx, list "qPCR". 
# Seradte si faktory dle obrazku, viz minule cviceni. 
# Otestujte, zda existuji rozdily v hodnotach exprese mezi ruznymi kondicemi u ruznych genu.  
# Vypočítejte popisné statistiky hodnoceného parametru ve srovnávaných skupinách. 


#### UKOL 4: 
# Nactete data data_examples.xlsx, list "immunofluorescence". 

# Otestujte, zda se lisi frekvence u ruznych kondic. 
# Napoveda: pracujte pouze s "axis.duplication", hodnota "normal je doplnek do 100 %. 
# Jakeho typu je vstupni promenna. Zvolte spravny test.  



### PRO RYCHLIKY: 
# Vyberte vhodny test pro hodnoceni dat na listu "immunofluorescence B". 
# Aplikujte ho. 
# Vysledky interpretujte. 





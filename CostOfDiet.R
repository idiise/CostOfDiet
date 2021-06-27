
# Import Packages ---------------------------------------------------------

library(openxlsx)
library(janitor)
library(tidyverse)
library(stringr)

# Beginning data processing -----------------------------------------------

base <- openxlsx::read.xlsx("CostOfTheDietSummaryDays_Household.xlsx",sheet = 1)

## détecter les grands blocs
#  The cost of the foods and the daily quantities in grams (g) selected by the software
col_CostDailyQuantities <- which(base[,1]=="The cost of the foods and the daily quantities in grams (g) selected by the software")

# The daily number of servings of the foods selected by the software								
Col_DailyNumber <- which(base[,1] == "The daily number of servings of the foods selected by the software")

# Cost of the Diet (XOF):	
col_CostOfDiet = which(base[,1]=="Cost of the Diet (XOF):")

# The daily quantity of each nutrient provided by the edible portion of foods selected by the software								
col_DailyQuantities <- which(base[,1]=="The daily quantity of each nutrient provided by the edible portion of foods selected by the software")

# The percentage (%) of each nutrient target provided by the edible portion of foods selected by the software
col_NutrientPortion <- which(base[,1]=="The percentage (%) of each nutrient target provided by the edible portion of foods selected by the software")


# The percentage (%) of each nutrient target provided by the foods selected by the software													
col_NutrientFoods <- which(base[,1]=="The percentage (%) of each nutrient target provided by the foods selected by the software")

# Extraction des tableaux non pivotés -------------------------------------
# # Extraction du premier tableau
CDQG <- base %>% slice(
  (col_CostDailyQuantities+1): (Col_DailyNumber-1)
)
names(CDQG) <- CDQG %>% slice(1) %>% unlist()
CDQG <- CDQG[-1,]
colnames(CDQG)[1] <- "indicateurs"
# a <- as.data.frame(t(as.matrix(CDQG)))
# rm(base2)

# # Extraction second tableau
DNSF <- base %>% slice(
  (Col_DailyNumber+1):(col_CostOfDiet-1)
)

names(DNSF) <- names(CDQG)
# # Extraction troisième tableau
COTD <- base %>% slice(
  (col_CostOfDiet+1):(col_DailyQuantities-1)
)
names(COTD) <- names(CDQG)

# Arrangement des bases de données ----------------------------------------

# suppresion des Totals dans les tableaux extraites
CDQG <- CDQG %>% select(-ncol(CDQG)) %>% 
  slice(-which(str_detect(CDQG[,1], "Total"))) %>% 
  select(-c(tidyselect::vars_select(names(CDQG), starts_with("Total", ignore.case = TRUE))))

DNSF <- DNSF %>% select(-ncol(DNSF)) %>% 
  slice(-which(str_detect(DNSF[,1], "Total"))) %>% 
  select(-c(tidyselect::vars_select(names(DNSF), starts_with("Total", ignore.case = TRUE))))

COTD <- COTD %>% select(-ncol(COTD)) %>% 
  slice(-which(str_detect(COTD[,1], "Total"))) %>% 
  select(-which(str_detect(colnames(COTD), "Total")))

# Pivoter les tableaux ----------------------------------------------------

CDQG <- CDQG %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to = "cdqg")

DNSF <- DNSF %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to ="dnsf")

COTD <- COTD %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to ="cotd")

Base1 <- CDQG %>% left_join(DNSF, by = c("indicateurs","variables"))

BaseFinal <- Base1 %>% left_join(COTD, by = c("indicateurs","variables"))


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

# # Extraction du quatrième tableau
TDQNPEPF <- base %>% slice(
  (col_DailyQuantities +1):(col_NutrientPortion-1)
)
names(TDQNPEPF) <- TDQNPEPF %>% slice(1) %>% unlist()
TDQNPEPF <- TDQNPEPF[-1,]
colnames(TDQNPEPF)[1] <- "indicateurs"

difference <- setdiff(TDQNPEPF$indicateurs, BaseFinal$variables)
TDQNPEPF <- TDQNPEPF %>% 
  filter( !(indicateurs %in% difference) )

# # Extraction cinquième tableau
last_row <- nrow(base)
TPNTPEDF <- base %>% slice(
  (col_NutrientPortion + 1):(last_row)
)
names(TPNTPEDF) <- TPNTPEDF %>% slice(1) %>% unlist()
TPNTPEDF <- TPNTPEDF[-1,]
colnames(TPNTPEDF)[1] <- "indicateurs"

difference2 <- setdiff(TPNTPEDF$indicateurs, BaseFinal$variables)
TPNTPEDF <- TPNTPEDF %>% 
  filter( !(indicateurs %in% difference2) )

#  changer les noms des variables du tableau 4 pour avoir la même chose
colnames(TDQNPEPF) <- colnames(TPNTPEDF)
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
# tab1
CDQG <- CDQG %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to = "cdqg")
# tab2
DNSF <- DNSF %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to ="dnsf")
# tab3
COTD <- COTD %>% 
  pivot_longer(!indicateurs, names_to = "variables", values_to ="cotd")

Base1 <- CDQG %>% left_join(DNSF, by = c("indicateurs","variables"))

BaseFinal <- Base1 %>% left_join(COTD, by = c("indicateurs","variables"))
# tab4
TDQNPEPF <- TDQNPEPF %>% rename("variables" = indicateurs  ) %>% 
  pivot_longer(!variables, names_to = "indicateurs", values_to = "tdqnpedf")
# tab5
TPNTPEDF <- TPNTPEDF %>% rename("variables" = indicateurs) %>% 
  pivot_longer(!variables, names_to = "indicateurs", values_to = "tpntpedf")

BaseFinal2 <- TDQNPEPF %>% left_join(TPNTPEDF, by = c("indicateurs","variables"))

setdiff(BaseFinal$variables, BaseFinal2$variables)
setdiff(BaseFinal2$variables, BaseFinal$variables)

FinalData <- BaseFinal %>% left_join(BaseFinal2, by = c("variables"))
FinalData <- FinalData %>% rename("Food Name" = variables)
test <- dplyr::union(BaseFinal, BaseFinal2)

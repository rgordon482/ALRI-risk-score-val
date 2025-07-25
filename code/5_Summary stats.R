# modellingData must be defined before running this code

modellingData = respData_2_to_24_mon

##### Initial stats ####
###### Outcome stats ####
nrow(modellingData)           # Number of admissions
sum(modellingData$died == 1)  # Number of deaths
sum(modellingData$died == 0)  # Number of discharges
mean(modellingData$died)      # Mortality rate


###### Summary stats for sex ####
sum(modellingData$sex == "male")
sum(modellingData$sex == "female")
nrow(filter(modellingData, sex == "male" & died == 1))
nrow(filter(modellingData, sex == "female" & died == 1))
model = glm(died ~ sex, family = "binomial", data = modellingData)
OR = exp(coef(summary(model))[2,1])
CI = confint.default(model)
CI_lower = exp(CI[2,1])
CI_upper = exp(CI[2,2])
print(paste0(round(OR, 1), " (", round(CI_lower, 1), " -",
             round(CI_upper, 1), ")" ))

##### Quick data cleaning ####
###### Creating respiratory rate category vector ####
respRate_category = character(nrow(modellingData))

for(i in 1:nrow(modellingData)){
  if(modellingData$age_months[i] < 12){
    cut_off = 50
  }
  else{
    cut_off = 40
  }
  
  if(modellingData$resp_rate[i] < cut_off){
    respRate_category[i] = "< cut-off"
  }
  else if(modellingData$resp_rate[i] < cut_off + 10){
    respRate_category[i] = "0-9 bpm above cut-off"
  }
  else if(modellingData$resp_rate[i] < cut_off + 20){
    respRate_category[i] = "10-19 bpm above cut-off"
  }
  else if(modellingData$resp_rate[i] >= cut_off + 20){
    respRate_category[i] = ">= 20 bpm above cut-off"
  }
}

table(respRate_category)
modellingData$respRate_category = respRate_category




###### Re-ordering conscious level so "normal" is the reference level ####
modellingData$conscious_level = as.factor(modellingData$conscious_level)
levels(modellingData$conscious_level)
modellingData$conscious_level = relevel(modellingData$conscious_level,
                                        ref = 3)
levels(modellingData$conscious_level)


##### Listing variables for stats ####
###### Binary variable list ####
variables_of_interest_Binary = c("vomiting", 
                                 "vomits_everything",
                                 "unable_to_drink",
                                 "diarrhoea",
                                 "cyanosis",
                                 "head_nodding",
                                 "indrawing",
                                 "flaring",
                                 "resp_deep",
                                 "wheeze",
                                 "crackles",
                                 "cough",
                                 "convulsion",
                                 "lymphadenopathy",
                                 "sunken_eye",
                                 "dec_skin_turgor",
                                 "pallor",
                                 "jaundice",
                                 "lethargy",
                                 "agitation",
                                 "prost_uncons")


###### Ordinal variable list ####
variables_of_interest_ordinal = c("sex",
                                  "malnutrition_muac",
                                  "malnutrition_wfaz",
                                  "malnutrition_wflz",
                                  "conscious_level",
                                  "age_groups",
                                  "respRate_category",
                                  "sats_category")

##### Binary variable summary stats ####
summaryTable_binary = tibble(variable = character(),
                      total_occurrences = numeric(),
                      died_occurrences = numeric(),
                      discharge_occurrences = numeric(),
                      total_proportion = numeric(),
                      died_proportion = numeric(),
                      discharge_proportion = numeric(),
                      OR = numeric(),
                      CI_lower = numeric(),
                      CI_upper = numeric(),
                      aOR = numeric(),
                      aCI_lower = numeric(),
                      aCI_upper = numeric())


N = length(modellingData$serial)
N_died = sum(modellingData$disch_type == "died")
N_discharge = sum(modellingData$disch_type == "discharge")


for(varName in variables_of_interest_Binary){
  # Calculate summary statistics, stratified by outcome
  totalOcc = sum(modellingData[[varName]] %in% c("y"))
  diedOcc = sum(modellingData[[varName]][which(modellingData$disch_type == "died")] %in% c("y"))
  dischOcc = sum(modellingData[[varName]][which(modellingData$disch_type == "discharge")] %in% c("y"))
  
  
  # Calculate unadjusted Odds Ratio with 95% confidence interval
  formula = paste0("died ~ ", varName)
  model = glm(formula, data = modellingData, family = binomial)
  OR = exp(coef(summary(model))[2,1])
  CI = confint.default(model)
  CI_lower = exp(CI[2,1])
  CI_upper = exp(CI[2,2])
  
  # Calculate adjusted odds ratios with 95% confidence interval
  predictors = paste0(varName, "+ age_groups + sex")
  formula = paste0("died ~ ", predictors)
  model = glm(formula, data = modellingData, family = binomial)
  aOR = exp(coef(summary(model))[2,1])
  aCI = confint.default(model)
  aCI_lower = exp(aCI[2,1])
  aCI_upper = exp(aCI[2,2])
  
  # Add row to data frame
  summaryTable_binary = summaryTable_binary %>% add_row(variable = varName,
                                          total_occurrences = totalOcc,
                                          died_occurrences = diedOcc,
                                          discharge_occurrences = dischOcc,
                                          total_proportion = totalOcc/N,
                                          died_proportion = diedOcc/N_died,
                                          discharge_proportion = dischOcc/N_discharge,
                                          OR = OR,
                                          CI_lower = CI_lower,
                                          CI_upper = CI_upper,
                                          aOR = aOR,
                                          aCI_lower = aCI_lower,
                                          aCI_upper = aCI_upper)
}




###### Format summary stats into paste-able table ####
summaryTable_binary_print = tibble(variable = character(),
                            total = character(),
                            died = character(),
                            discharged = character(),
                            OR_with_CI = character(),
                            aOR_with_CI = character())



summaryTable_binary_print = tibble(variable = summaryTable_binary$variable,
                            total = paste0(summaryTable_binary$total_occurrences, 
                                           " (", 
                                           round(summaryTable_binary$total_proportion*100, 1),
                                           "%)"),
                            died = paste0(summaryTable_binary$died_occurrences, 
                                          " (", 
                                          round(summaryTable_binary$died_proportion*100, 1),
                                          "%)"),
                            discharged = paste0(summaryTable_binary$discharge_occurrences, 
                                                " (", 
                                                round(summaryTable_binary$discharge_proportion*100, 1),
                                                "%)"),
                            OR_with_CI = paste0(round(summaryTable_binary$OR,1), 
                                                " (", 
                                                round(summaryTable_binary$CI_lower, 1),
                                                " - ",
                                                round(summaryTable_binary$CI_upper, 1),
                                                ")"),
                            aOR_with_CI = paste0(round(summaryTable_binary$aOR,1), 
                                                " (", 
                                                round(summaryTable_binary$aCI_lower, 1),
                                                " - ",
                                                round(summaryTable_binary$aCI_upper, 1),
                                                ")"))




view(summaryTable_binary_print)

##### Ordinal variable summary stats ####
summaryTable_ordinal = tibble(variable = character(),
                             level = character(),
                             total_occurrences = numeric(),
                             died_occurrences = numeric(),
                             discharge_occurrences = numeric(),
                             total_proportion = numeric(),
                             died_proportion = numeric(),
                             discharge_proportion = numeric(),
                             OR = numeric(),
                             CI_lower = numeric(),
                             CI_upper = numeric(),
                             aOR = numeric(),
                             aCI_lower = numeric(),
                             aCI_upper = numeric())


N = length(modellingData$serial)
N_died = sum(modellingData$disch_type == "died")
N_discharge = sum(modellingData$disch_type == "discharge")


for(varName in variables_of_interest_ordinal){
  # Univariable logistic regression for crude ORs
  formula_1 = paste0("died ~ as.factor(", varName, ")")
  model_1 = glm(formula_1, data = modellingData, family = binomial)
  
  # Multivariable logistic regression for adjusted ORs
  predictors = paste0("as.factor(", varName, ") + age_groups + sex")
  formula_2 = paste0("died ~ ", predictors)
  model_2 = glm(formula_2, data = modellingData, family = binomial)
  
  for(i in unique(modellingData[[varName]])){
  # Calculate summary statistics, stratified by outcome
  totalOcc = sum(modellingData[[varName]] == i)
  diedOcc = sum(modellingData[[varName]][which(modellingData$disch_type == "died")] == i)
  dischOcc = sum(modellingData[[varName]][which(modellingData$disch_type == "discharge")] == i)
  
  # Get unadjusted odds ratio
  index = paste0("as.factor(", varName, ")", i)
  if(index %in% names(model_1$coefficients)){
  coef = model_1$coefficients[[index]]
  OR = exp(coef)
  CI = confint.default(model_1)
  rowNum = which(rownames(CI) == index)
  CI_lower = exp(CI[rowNum,1])
  CI_upper = exp(CI[rowNum,2])
  
  # Get adjusted odds ratio
  coef = model_2$coefficients[[index]]
  aOR = exp(coef)
  aCI = confint.default(model_2)
  rowNum = which(rownames(aCI) == index)
  aCI_lower = exp(aCI[rowNum,1])
  aCI_upper = exp(aCI[rowNum,2])
  }
  else{
    OR = 0
    CI_lower = 0
    CI_upper = 0
    aOR = 0
    aCI_lower = 0
    aCI_upper = 0
  }
  
  # Add row to data frame
  summaryTable_ordinal = summaryTable_ordinal %>% add_row(variable = varName,
                                                        level = as.character(i),
                                                        total_occurrences = totalOcc,
                                                        died_occurrences = diedOcc,
                                                        discharge_occurrences = dischOcc,
                                                        total_proportion = totalOcc/N,
                                                        died_proportion = diedOcc/N_died,
                                                        discharge_proportion = dischOcc/N_discharge,
                                                        OR = OR,
                                                        CI_lower = CI_lower,
                                                        CI_upper = CI_upper,
                                                        aOR = aOR,
                                                        aCI_lower = aCI_lower,
                                                        aCI_upper = aCI_upper)
  }
}




###### Format summary stats into paste-able table ####
summaryTable_ordinal_print = tibble(variable = character(),
                                   total = character(),
                                   died = character(),
                                   discharged = character(),
                                   OR_with_CI = character(),
                                   aOR_with_CI = character())

summaryTable_ordinal_print = tibble(variable = summaryTable_ordinal$variable,
                                    level = summaryTable_ordinal$level,
                                   total = paste0(summaryTable_ordinal$total_occurrences, 
                                                  " (", 
                                                  round(summaryTable_ordinal$total_proportion*100, 1),
                                                  "%)"),
                                   died = paste0(summaryTable_ordinal$died_occurrences, 
                                                 " (", 
                                                 round(summaryTable_ordinal$died_proportion*100, 1),
                                                 "%)"),
                                   discharged = paste0(summaryTable_ordinal$discharge_occurrences, 
                                                       " (", 
                                                       round(summaryTable_ordinal$discharge_proportion*100, 1),
                                                       "%)"),
                                   OR_with_CI = paste0(round(summaryTable_ordinal$OR,1), 
                                                       " (", 
                                                       round(summaryTable_ordinal$CI_lower, 1),
                                                       " - ",
                                                       round(summaryTable_ordinal$CI_upper, 1),
                                                       ")"),
                                   aOR_with_CI = paste0(round(summaryTable_ordinal$aOR,1), 
                                                        " (", 
                                                        round(summaryTable_ordinal$aCI_lower, 1),
                                                        " - ",
                                                        round(summaryTable_ordinal$aCI_upper, 1),
                                                        ")"))




view(summaryTable_ordinal_print)

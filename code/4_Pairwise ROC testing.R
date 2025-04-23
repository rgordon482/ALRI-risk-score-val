##### Pairwise ROC hypothesis testing #####
# Define scores to test
scoresToTest = c("mRISC","PERCH", "PREPARE","ReSVinet", "ReSVinet_Maln_MUAC", 
                 "RISC",  "RISC_Malawi_MUAC", "RISC_Malawi_WAZ")

n = length(scoresToTest)

###### Calculating and storing ROC values for each score ####
roc_RISC = roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC)
roc_mRISC = roc(respData_2_to_24_mon$died, respData_2_to_24_mon$mRISC)
roc_RISC_Malawi_MUAC = roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_MUAC)
roc_RISC_Malawi_WAZ = roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_WAZ)
roc_PERCH =  roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PERCH)
roc_PREPARE = roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PREPARE)
roc_ReSVinet =  roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet)
roc_ReSVinet_Maln_MUAC =  roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet_Maln_MUAC)



###### Performing hypothesis test for each pair of scores ####
# Creating output matrix
resultsMat = matrix(nrow = n, ncol = n, dimnames = list(scoresToTest, scoresToTest))

# Performing hypothesis tests
for(i in scoresToTest){
  roc1 = get(paste0("roc_", i))
  for(j in scoresToTest){
    if(i != j){
      roc2 = get(paste0("roc_", j))
      roc_test = roc.test(roc1, roc2, paired = T) # Performing paired ROC test
      resultsMat[i,j] = roc_test$p.value          # Storing p-value
    }
  }
}

# Printing results matrix
view(resultsMat)
view(round(resultsMat, 3))
view(round(resultsMat, 4))


##### Test for difference in AUROC for each ReSVinet modification ####
ReSVinetMods = c("ReSVinet",
                 "ReSVinet_Maln_MUAC", 
                 "ReSVinet_Maln_wfaz",
                 "ReSVinet_Maln_wflz")

###### Calculate and store ROCs ####
roc_ReSVinet_Maln_MUAC = roc(respData_2_to_24_mon$died,
                             respData_2_to_24_mon$ReSVinet_Maln_MUAC)

roc_ReSVinet_Maln_wfaz = roc(respData_2_to_24_mon$died,
                             respData_2_to_24_mon$ReSVinet_Maln_wfaz)

roc_ReSVinet_Maln_wflz = roc(respData_2_to_24_mon$died,
                             respData_2_to_24_mon$ReSVinet_Maln_wflz)

# Create output matrix
resultsMat_ReSVinetMods = matrix(nrow = 4, ncol = 4, dimnames = list(ReSVinetMods,
                                                                  ReSVinetMods))

###### Performing hypothesis test for each pair of scores ####
for(i in ReSVinetMods){
  roc1 = get(paste0("roc_", i))
  for(j in ReSVinetMods){
    if(i != j){
      roc2 = get(paste0("roc_", j))
      roc_test = roc.test(roc1, roc2, paired = T) # Performing paired ROC test
      resultsMat_ReSVinetMods[i,j] = roc_test$p.value          # Storing p-value
    }
  }
}

# Printing results matrix
view(resultsMat_ReSVinetMods)
view(round(resultsMat_ReSVinetMods, 5))


##### Sub-group analysis: ROC  tests for age-stratified ROCs####
# Create new dataframe for each age sub-group
respData_2_to_24_mon_under6 = filter(respData_2_to_24_mon, age_groups == "<6 months")
respData_2_to_24_mon_6_to_12 = filter(respData_2_to_24_mon, age_groups == "6-12 months")
respData_2_to_24_mon_13_to_24 = filter(respData_2_to_24_mon, age_groups == "13-24 months")

nrow(respData_2_to_24_mon_under6)
nrow(respData_2_to_24_mon_6_to_12)
nrow(respData_2_to_24_mon_13_to_24)

# Sanity check that all admissions included
nrow(respData_2_to_24_mon_under6) + nrow(respData_2_to_24_mon_6_to_12) +nrow(respData_2_to_24_mon_13_to_24)

###### Calculate and store ROCs ####
for(age in c("under6", "6_to_12", "13_to_24")){
  df_name = paste0("respData_2_to_24_mon_", age)
  df = get(df_name)
  for(score in scoresToTest){
    tmp = roc(df[["died"]], df[[score]])
    assign((paste0("ROC_", age, "_", score)),tmp)
    
  }
}

###### Performing hypothesis tests for each score ####
age_strat_roc_tests = tibble(score = character(),
                             under6_vs_6_to_12  = numeric(),
                             under6_vs_13_to_24 = numeric(),
                             six_to_12_vs_13_to_24 = numeric())

for(score in scoresToTest){
  roc_test_under6_vs_6_to_12 = roc.test(get(paste0("ROC_under6_", score)),
                                        get(paste0("ROC_6_to_12_", score)))
  roc_test_under6_vs_13_to_24 = roc.test(get(paste0("ROC_under6_", score)),
                                         get(paste0("ROC_13_to_24_", score)))
  roc_test_six_to_12_vs_13_to_24 = roc.test(get(paste0("ROC_6_to_12_", score)),
                                            get(paste0("ROC_13_to_24_", score)))
  
  
  age_strat_roc_tests = age_strat_roc_tests %>%
    add_row(score = score,
            under6_vs_6_to_12 = roc_test_under6_vs_6_to_12$p.value,
            under6_vs_13_to_24 = roc_test_under6_vs_13_to_24$p.value,
            six_to_12_vs_13_to_24 = roc_test_six_to_12_vs_13_to_24$p.value
    )
}

view(age_strat_roc_tests)


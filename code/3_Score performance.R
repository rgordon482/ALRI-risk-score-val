##### Main sample analysis ####
# Filtering for main sample population: Children 2-24 months with ALRI discharge diagnosis
respData_2_to_24_mon = filter(KenyaData,age_months >= 2 & age_months <=24)
respData_2_to_24_mon = filter(respData_2_to_24_mon, 
                              diag1_disch %in% c("LRTI", "bronchiolitis")|
                              diag2_disch %in% c("LRTI", "bronchiolitis"))

# Defining scores to test
scoresToTest = c("mRISC","PERCH", "PREPARE", "ReSVinet",
                 "RISC",  "RISC_Malawi_MUAC", "RISC_Malawi_WAZ")

# AUROC for each original score
aucTib = AUC_fun_mort(respData_2_to_24_mon, scoresToTest)
view(aucTib)

# AUROC for each modification of ReSVinet
aucTib = AUC_fun_mort(respData_2_to_24_mon, c("ReSVinet_Maln_MUAC", 
                                              "ReSVinet_Maln_wfaz",
                                              "ReSVinet_Maln_wflz"))
view(aucTib)
## MUAC gives largest increase in AUROC of all malnutrition criteria



# misClass function for each score.
# Only viewing columnns included in manuscript tables
columnIndex = c(1,2,3,4,6,7,8,15,16)

view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$mRISC)[,columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$PERCH)[, columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$PREPARE)[,columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet)[,columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC)[,columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_MUAC)[,columnIndex])
view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_WAZ)[,columnIndex])

view(misclass_fun(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet_Maln_MUAC)[,columnIndex])

##### Sub-group analysis: Age groups ####
scoresToTest = c("mRISC","PERCH", "PREPARE", "ReSVinet", "ReSVinet_Maln_MUAC",
                 "RISC",  "RISC_Malawi_MUAC", "RISC_Malawi_WAZ")

###### Creating age category vector ####
respData_2_to_24_mon$age_groups = character(nrow(respData_2_to_24_mon))

for(i in 1:nrow(respData_2_to_24_mon)){
  if(respData_2_to_24_mon$age_months[i] < 6){
    respData_2_to_24_mon$age_groups[i] = "<6 months"
  }
  else if(respData_2_to_24_mon$age_months[i] >= 6 & respData_2_to_24_mon$age_months[i] <= 12){
    respData_2_to_24_mon$age_groups[i] = "6-12 months"
  }
  else if(respData_2_to_24_mon$age_months[i] > 12){
    respData_2_to_24_mon$age_groups[i] = "13-24 months"
  }
}

# Check all admissions accounted for
nrow(filter(respData_2_to_24_mon, age_groups %in% c("<6 months", "6-12 months", "13-24 months")))

###### Calculating AUROCs ####
aucTib1 = AUC_fun_mort(filter(respData_2_to_24_mon, age_groups == "<6 months"), scoresToTest)
view(aucTib1)

aucTib2 = AUC_fun_mort(filter(respData_2_to_24_mon, age_groups == "6-12 months"), scoresToTest)
view(aucTib2)

aucTib3 = AUC_fun_mort(filter(respData_2_to_24_mon, age_groups == "13-24 months"), scoresToTest)
view(aucTib3)

# Combine into table
view(tibble(aucTib1[,1],
            under6Mon = aucTib1[,2],
            sixto12Mon = aucTib2[,2],
            thirteento24Mon = aucTib3[,2]))


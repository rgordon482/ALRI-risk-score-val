#### Rewrite score functions to allow missing data ####
##### PERCH score ####
PERCH_fun_missing = function(dataset){
  N = length(dataset$serial)
  attach(dataset)
  
  score_vec = numeric(N)
  for(i in 1:N){
    score = 0
    if(age_months[i] < 12){
      score = score + 2
    }
    if(oxysat[i] < 92){
      score = score + 2
    }
    
    if(cough[i] == "y"){
      score = score - 1
    }
    
    if(conscious_level[i] == "unconscious"||
       conscious_level[i] == "prostrate"){
      if(resp_deep[i] == "y"){
        score = score + 5
      }
      else{
        score = score + 2
      }
    }
    if(sex[i] == "female"){
      score = score + 1
    }
    if(!is.na(wt_for_ht[i])){
      if(wt_for_ht[i] < -3){
        score = score + 3
      }
      else if(wt_for_ht[i]< -2){
        score = score + 2
      }
    }
    score_vec[i] = score
  }
  return(score_vec)
}


##### mRISC ####
mRISC_fun_missing = function(dataset){
  N = length(dataset$serial)
  score_vec = numeric(N)
  attach(dataset)
  
  for(i in 1:N){
    score = 0
    if(dec_skin_turgor[i] == "y"|| sunken_eye[i] == "y"){
      score = score + 1
    }
    if(conscious_level[i] == "prostrate"||
       conscious_level[i] == "unconscious"){
      score = score + 1
    }
    if(unable_to_drink[i] == "y"){
      score = score + 1
    }
    if(indrawing[i] == "y"){
      score = score + 1
    }
    if(bcs[i] < 5){
      score = score + 2
    }
    if(!is.na(wt_for_age[i])){
      if(wt_for_age[i] <= -2){
        score = score + 1
      }
    }
    
    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}

##### RISC (HIV negative version)####
RISC_fun_missing = function(dataset){
  N = length(dataset$serial)
  score_vec = numeric(N)
  attach(dataset)
  
  for(i in 1:N){
    score = 0
    if(oxysat[i] < 90){
      score = score + 3
    }
    else if(indrawing[i] == "y"){
      score = score + 2
    }
    if(wheeze[i] == "y"){
      score = score - 2
    }
    if(!is.na(wt_for_age[i])){
      if(wt_for_age[i] <= -3){
        score = score + 2 
      }
      else if(wt_for_age[i] <= -2){
        score = score + 1
      }
    }
    
    if(unable_to_drink[i] == "y"){
      score = score + 1
    }
    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}

##### PREPARE (only for children 2-59 months)####
PREPARE_fun_missing = function(dataset){
  N = length(dataset$serial)
  score_vec = rep(NA, N)
  attach(dataset)
  
  for(i in 1:N){
    score = 0
    if(age_months[i] < 6){
      score = score + 2
    }
    else if(age_months[i] < 12){
      score = score + 1
    }
    if(sex[i] == "female"){
      score = score + 1
    }
    if(!is.na(wt_for_age[i])){
      if(wt_for_age[i] < -3){
        score = score + 3
      }
      else if(wt_for_age[i] <= -2){
        score = score + 2
      }
    }
    
    if(temp_axill[i] < 35.5){
      score = score + 3
    }
    if(conscious_level[i] == "prostrate"||
       conscious_level[i] == "unconscious"){
      score = score + 1
    }
    if(convulsion[i] == "y"){
      score = score + 2
    }
    if(cyanosis[i] == "y"){
      score = score + 2
    }
    if(oxysat[i] < 90){
      score = score + 2
    }
    if(age_months[i] < 12 && resp_rate[i] >= 70){
      score = score + 1
    }
    else if(age_months[i] >= 12 && resp_rate[i] >= 60){
      score = score + 1
    }
    
    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}

###### RISC-Malawi (WAZ) ####
RISC_Malawi_fun_WAZ_missing = function(dataset){
  N = length(dataset$serial)
  attach(dataset)
  
  score_vec = numeric(N)
  for(i in 1:N){
    score = 0
    if(oxysat[i] < 90){
      score = score + 5
    }
    else if(oxysat[i] <= 92){
      score = score + 1
    }
    
    if(wheeze[i] == "y"){
      score = score - 1
    }
    if(!is.na(wt_for_age[i])){
      if(wt_for_age[i] < -3){
        score = score + 6
      }
      else if(wt_for_age[i] < -2){
        score = score + 3
      }
    }
    if(conscious_level[i] == "prostrate"||
       conscious_level[i] == "unconscious"){
      score = score + 5
    }
    if(sex[i] == "female"){
      score = score + 1
    }
    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}


##### Main sample with no missing data ####
mainSample_noMissing = filter(KenyaData_raw_2_to_24mon, diag1_disch %in% c("LRTI", "bronchiolitis") |
                                diag2_disch %in% c("LRTI", "bronchiolitis"))

# Remove missing outcome
mainSample_noMissing = filter(mainSample_noMissing, disch_type %in% c("died", "discharge"))

# Check for missingness in each variable
colNames = names(mainSample_noMissing)
for(i in 1:length(colNames)){
  num_missing = sum(is.na(mainSample_noMissing[,i]))
  if(num_missing != 0){
    print(paste0(colNames[i], ": ", sum(is.na(mainSample_noMissing[,i]))))
  }
}

# Add new variables
# (Subset of the variables that would be added had we passed through data cleaning script)
mainSample_noMissing$died = if_else(mainSample_noMissing$disch_type == "died", 1, 0)
mainSample_noMissing$vomits_everything = if_else(is.na(mainSample_noMissing$vomits_everything), "n", mainSample_noMissing$vomits_everything)
mainSample_noMissing$bcs = mainSample_noMissing$bcs_eyes + mainSample_noMissing$bcs_motor +
  mainSample_noMissing$bcs_verbal

###### Malnutrition by MUAC ####
# Coding used:
# 2 = Severely malnourished (MUAC < 11.5 cm)
# 1 = Moderately malnourished (MUAC < 12.5cm and >= 11.5cm)
# 0 = No/Mildly malnourished (MUAC >= 12.5cm)
ind = numeric(nrow(mainSample_noMissing))
for(i in 1:nrow(mainSample_noMissing)){
  tmp = mainSample_noMissing$muac[i]
  if(tmp < 11.5){
    ind[i] = 2
  }
  else if(tmp >= 12.5){
    ind[i] = 0
  }
  else{
    ind[i] = 1
  }
}

mainSample_noMissing$malnutrition_muac = ind


# Dataset size
nrow(mainSample_noMissing)
nrow(mainSample_noMissing) - nrow(respData_2_to_24_mon)
view(mainSample_noMissing[is.na(mainSample_noMissing$weight),])
view(mainSample_noMissing[is.na(mainSample_noMissing$wt_for_age),])
view(mainSample_noMissing[is.na(mainSample_noMissing$wt_for_ht),])
sum(mainSample_noMissing$vomits_everything == "d", na.rm = T)

# Cohort stats
sum(mainSample_noMissing$died)
mean(mainSample_noMissing$died)

# Evaluate scores
mainSample_noMissing$RISC_Malawi_MUAC = RISC_Malawi_MUAC_fun(mainSample_noMissing)
mainSample_noMissing$RISC_Malawi_WAZ = RISC_Malawi_fun_WAZ_missing(mainSample_noMissing)
mainSample_noMissing$ReSVinet = ReSVinet_fun(mainSample_noMissing)
mainSample_noMissing$RISC = RISC_fun_missing(mainSample_noMissing)
mainSample_noMissing$PREPARE = PREPARE_fun_missing(mainSample_noMissing)
mainSample_noMissing$PERCH = PERCH_fun_missing(mainSample_noMissing)
mainSample_noMissing$mRISC = mRISC_fun_missing(mainSample_noMissing)
mainSample_noMissing$RISC = RISC_fun_missing(mainSample_noMissing)
mainSample_noMissing$ReSVinet_Maln_MUAC = mainSample_noMissing$ReSVinet + 
  if_else(mainSample_noMissing$malnutrition_muac !=0, mainSample_noMissing$malnutrition_muac + 1, 0)

# Calculate AUROCs
view(AUC_fun_mort(mainSample_noMissing, c("mRISC", "PERCH", "PREPARE", "ReSVinet",
                                          "ReSVinet_Maln_MUAC","RISC",
                                          "RISC_Malawi_MUAC", "RISC_Malawi_WAZ")))

# un-paired ROC testing with main sample results
roc.test(roc_RISC, roc(mainSample_noMissing$died, mainSample_noMissing$RISC))
roc.test(roc_mRISC, roc(mainSample_noMissing$died, mainSample_noMissing$mRISC))
roc.test(roc_RISC_Malawi_MUAC, roc(mainSample_noMissing$died, mainSample_noMissing$RISC_Malawi_MUAC))
roc.test(roc_RISC_Malawi_WAZ, roc(mainSample_noMissing$died, mainSample_noMissing$RISC_Malawi_WAZ))

roc.test(roc_PERCH, roc(mainSample_noMissing$died, mainSample_noMissing$PERCH))
roc.test(roc_PREPARE, roc(mainSample_noMissing$died, mainSample_noMissing$PREPARE))
roc.test(roc_ReSVinet, roc(mainSample_noMissing$died, mainSample_noMissing$ReSVinet))
roc.test(roc_ReSVinet_Maln_MUAC, roc(mainSample_noMissing$died, mainSample_noMissing$ReSVinet_Maln_MUAC))







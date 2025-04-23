##### PERCH score ####
PERCH_fun = function(dataset){
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

    if(wt_for_ht[i] < -3){
      score = score + 3
    }
    else if(wt_for_ht[i]< -2){
      score = score + 2
    }
    score_vec[i] = score
  }
  return(score_vec)
}


##### mRISC ####
mRISC_fun = function(dataset){
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

    if(wt_for_age[i] <= -2){
      score = score + 1
    }

    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}

##### RISC (HIV negative version)####
RISC_fun = function(dataset){
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

    if(wt_for_age[i] <= -3){
      score = score + 2 
    }
    else if(wt_for_age[i] <= -2){
      score = score + 1
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
PREPARE_fun = function(dataset){
  N = length(dataset$serial)
  score_vec = rep(NA, N)
  over2mon_index = which(dataset$age_months >= 2)
  attach(dataset)
  
  for(i in over2mon_index){
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

    if(wt_for_age[i] < -3){
      score = score + 3
    }
    else if(wt_for_age[i] <= -2){
      score = score + 2
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

##### RISC_Malawi (MUAC)####
RISC_Malawi_MUAC_fun = function(dataset){
  N = length(dataset$serial)
  attach(dataset)
  
  score_vec = numeric(N)
  for(i in 1:N){
    score = 0
    if(oxysat[i] < 90){
      score = score + 7
    }
    else if(oxysat[i] <= 92){
      score = score + 2
    }
    
    if(wheeze[i] == "y"){
      score = score - 2
    }
    if(muac[i] < 11.5){
      score = score + 7
    }
    else if(muac[i] <= 13.5){
      score = score + 3
    }
    if(conscious_level[i] == "prostrate"||
       conscious_level[i] == "unconscious"){
      score = score + 8
    }
    if(sex[i] == "female"){
      score = score + 1
    }
    score_vec[i] = score
  }
  detach(dataset)
  return(score_vec)
}

##### RISC_Malawi (WAZ)####
RISC_Malawi_WAZ_fun = function(dataset){
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
    if(wt_for_age[i] < -3){
      score = score + 6
    }
    else if(wt_for_age[i] < -2){
      score = score + 3
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



##### ReSVinet #####
# Functions for scoring each component, followed by a function to evaluate
# the full score.


###### Component functions ####
####### General condition ####
resvi_Condition_fun = function(conscious_level,
                               agitation,
                               convulsion){
  if(conscious_level == "unconscious" ||
     conscious_level == "prostrate"||
     convulsion == "y"){
    score = 3
  }
  else if(conscious_level == "agitated"|| 
          conscious_level == "lethargic"){
    score = 2
  }
  else{
    score = 0
  }
  return(score)
}

###### Respiratory difficulty ####
resvi_RespDiff_fun = function(indrawing,
                              wheeze,
                              flaring,
                              head_nodding,
                              cyanosis,
                              resp_deep){
  if(head_nodding == "y"||
     cyanosis == "y"||
     resp_deep == "y"){
    score = 3
  }
  else if(flaring == "y"|| indrawing == "y"){
    score = 2
  }
  else if(wheeze == "y"){
    score = 1
  }
  else{
    score = 0
  }
  return(score)
}

##### Feeding ####
resvi_Feed_fun = function(vomiting, 
                          vomits_everything, 
                          unable_to_drink){

  if(unable_to_drink == "y"|| vomits_everything == "y"){
    score = 3
  }
  else if(vomiting == "y"){
    score = 2
  }
  else{
    score = 0
  }
  
  return(score)
}

###### Temperature #####
resvi_Temp_fun = function(temp_axilla){
  if(temp_axilla >= 38){
    score = 2
  }
  else if(temp_axilla >= 37.5){
    score = 1
  }
  else{
    score = 0
  }
  return(score)
}

###### Respiratory rate ####
# ONLY SUITABLE FOR <= 36 months
# Age should be provided in months
resvi_RespRate_fun = function(age, resp_rate){
  if(age < 2){
    score = respRate_subFun(resp_rate, 40, 50, 70)
  }
  
  else if(age < 6){
    score = respRate_subFun(resp_rate, 35, 45, 60)
  }
  
  else if(age < 12){
    score = respRate_subFun(resp_rate, 30, 40, 55)
  }
  
  else if(age < 24){
    score = respRate_subFun(resp_rate, 25, 35, 50)
  }
  
  else if(age <= 36){
    score = respRate_subFun(resp_rate, 20, 30, 40)
  }
  return(score)
}

respRate_subFun = function(resp_rate, val1, val2, val3){
  if(resp_rate > val3){
    score = 3
  }
  else if(resp_rate >= val2){
    score = 2
  }
  else{
    score = 0
  }
  return(score)
}

###### Total score eval ####
ReSVinet_fun = function(dataset){
  under3year_index = which(dataset$age_years <= 3)
  N = length(dataset$serial)
  
  ##### Evaluating each score component ####
  ###### Feeding ####
  ReSVinet_Feed = rep(NA,N)
  for(i in under3year_index){
    ReSVinet_Feed[i] = resvi_Feed_fun(dataset$vomiting[i],
                                      dataset$vomits_everything[i],
                                      dataset$unable_to_drink[i])
  }
  
  ###### Fever ####
  ReSVinet_Fever = rep(NA,N)
  for(i in under3year_index){
    ReSVinet_Fever[i] = resvi_Temp_fun(dataset$temp_axill[i])
  }
  
  ###### Respiratory rate ####
  ReSVinet_RR = rep(NA,N)
  for(i in under3year_index){
    ReSVinet_RR[i] = resvi_RespRate_fun(dataset$age_months[i],
                                        dataset$resp_rate[i])
  }
  
  ###### Respiratory distress ####
  ReSVinet_Diff = rep(NA,N)
  for(i in under3year_index){
    ReSVinet_Diff[i] = resvi_RespDiff_fun(dataset$indrawing[i],
                                          dataset$wheeze[i],
                                          dataset$flaring[i],
                                          dataset$head_nodding[i],
                                          dataset$cyanosis[i],
                                          dataset$resp_deep[i])
  }
  
  ###### General condition ####
  ReSVinet_Condition = rep(NA,N)
  for(i in under3year_index){
    ReSVinet_Condition[i] = resvi_Condition_fun(dataset$conscious_level[i],
                                                dataset$agitation[i],
                                                dataset$convulsion[i])
  }
  
  scores =  ReSVinet_Condition + ReSVinet_Diff + ReSVinet_Feed + ReSVinet_Fever +
    ReSVinet_RR
  return(scores)
}


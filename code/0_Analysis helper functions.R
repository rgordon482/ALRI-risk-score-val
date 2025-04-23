##### Defining helper functions used to assess performance ####

###### AUC calculation for each score ####
## Assesses AUC for outcomes of death and death <= X days since admission
AUC_fun = function(dataset, X, scoresToTest){
  output = tibble(score = character(),
                  AUC_died = character(),
                  AUC_died_withinXdays = character())

  died_withinXdays = if_else(dataset$died + (dataset$los <= X) == 2, 1, 0)

  # WHO Danger
  for(y in scoresToTest){
    AUC_died_ci = round(as.numeric(ci.auc(dataset$died,
                                          dataset[[y]])),2)

    AUC_died_withinXdays_ci =
      round(as.numeric(ci.auc(died_withinXdays, dataset[[y]])),2)

    output = output %>% add_row(score = y,
                                AUC_died = paste0(AUC_died_ci[2],
                                                  " (", AUC_died_ci[1],
                                                  " - ", AUC_died_ci[3], ")"),
                                AUC_died_withinXdays = paste0(AUC_died_withinXdays_ci[2],
                                                              " (", AUC_died_withinXdays_ci[1],
                                                              " - ", AUC_died_withinXdays_ci[3], ")"))

  }
  return(output)
}

###### Misclassification rate function ####
# Calculates misclassification rate at every possible cut-off point
## Input: events - Binary vector of events
##        predictors - Corresponding vector of risk scores

misclass_fun = function(events, predictors){
  # List all unique score values
  uniquePredictors = sort(unique(predictors))
  
  N = length(events)
  
  # Create output data frame
  output = tibble(cutOff = numeric(),
                  nScore = numeric(),
                  eventScore = numeric(),
                  mortRate = numeric(),
                  misClass  = numeric(),
                  sensitivity = numeric(),
                  specificity = numeric(),
                  percent_high_risk_over_cutoff = numeric(),
                  mortRisk_cutoff = numeric(),
                  TP = numeric(),
                  FP = numeric(),
                  TN = numeric(),
                  FN = numeric())
  
  # Calculate stats for each potential cut-off value
  # Note: High-risk taken to be >= cut-off
  for(i in uniquePredictors){
  
    # Calculate # true/false positives and true/false negatives
    TP = sum(events[predictors >= i])
    TN = sum(1 - events[predictors < i])
    FP = sum(1 - events[predictors >= i])
    FN = sum(events[predictors < i])
    
    # Sanity check to ensure all individuals accounted for
    print(TP+TN+FP+FN)
    
    # Number of individuals with score
    nScore = sum(predictors == i)
    
    # Number of individuals with score who die
    eventScore = sum(events[predictors == i])
    
   # Add row to output, calculating stats at the same time
    output = output %>% add_row(cutOff = i,
                                nScore = nScore,
                                eventScore = eventScore,
                                mortRate = round(eventScore/nScore*100,1),  # Mortality rate amongst individuals with score = i
                                misClass = round((FP+FN)/N*100,1),     # misclassification rate
                                sensitivity = round(TP/(TP+FN)*100,1),
                                specificity = round(TN/(TN+FP)*100,1),
                                percent_high_risk_over_cutoff = 
                                  round((TP+FP)/N*100,1),
                                TP = TP,
                                FP = FP,
                                TN = TN,
                                FN = FN)
    
    
    
  }
  
  # Adding sensitivity + specificity and Youden's J index
  output = output %>% add_column(sens_plus_spec = output$sensitivity + 
                                   output$specificity,
                                 Youdens_J = output$sensitivity/100 + 
                                   output$specificity/100 - 1)
  
  # Adding mortality rate amongst high-risk patients for each cut-off value
  mortRate_in_highRisk = numeric(nrow(output))
  
  for(i in 1:length(uniquePredictors)){
    scoreVal = uniquePredictors[i]
    mortRate_in_highRisk[i] = sum(output$eventScore[output$cutOff >= scoreVal])/
      sum(output$nScore[output$cutOff >= scoreVal])
  }
  output = output %>% add_column(mortRate_in_highRisk = round(mortRate_in_highRisk*100,1))

  return(output)
}


###### AUC calculation for each score ####
# Assesses AUC for outcome of death for each score
## Input: dataset - full dataset on which AUC should be assessed
##                  - Must contain all scores in scoresToTest as columns
##        scoresToTest - character vector containing names of scores to test

# Note ci.auc function uses DeLong's method by default

AUC_fun_mort = function(dataset,  scoresToTest){
  output = tibble(score = character(),
                  AUC_died = character())
  
  # Calculate AUC and 95% CI for each score
  for(y in scoresToTest){
    AUC_died_ci = round(as.numeric(ci.auc(dataset$died, 
                                          dataset[[y]])),2)

    output = output %>% add_row(score = y,
                                AUC_died = paste0(AUC_died_ci[2],
                                                  " (", AUC_died_ci[1],
                                                  " - ", AUC_died_ci[3], ")"))
    
  }
  return(output)
}

# Load data
KenyaData = read_excel("KCH_admission_2015to2024.xls")

##### Check for missingness ####
# Check whole dataset
sum(is.na(KenyaData))
## Appears to be some missing data

# Check for missingness in each variable
colNames = names(KenyaData)
for(i in 1:length(colNames)){
  num_missing = sum(is.na(KenyaData[,i]))
  if(num_missing != 0){
    print(paste0(colNames[i], ": ", sum(is.na(KenyaData[,i]))))
  }
}
rm(num_missing)

##### Cleaning variables ####
###### Examine missingness in vomits_everything ####
# Check if children missing vomits_everything have vomiting = FALSE
vomiting_n = which(KenyaData$vomiting == "no")
vomiting_y = setdiff(1:length(KenyaData$serial), vomiting_n)
sum(is.na(KenyaData$vomits_everything[vomiting_n]))
sum(is.na(KenyaData$vomits_everything[vomiting_y]))
## vomits_everything is only filled in for individuals who have vomiting = "yes"

## Replace NA's with "no" in "vomits_everything" variable
for(i in 1:length(KenyaData$serial)){
  if(is.na(KenyaData$vomits_everything[i])){
    KenyaData$vomits_everything[i] = "n"
  }
}


###### Examining discharge outcomes ####
unique(KenyaData$disch_type)
table(KenyaData$disch_type)

# Extract admissions with no discharge date for future analysis
KenyaData_noDischDate = KenyaData[is.na(KenyaData$date_disch),]

# Remove children without a discharge date
KenyaData = KenyaData[!is.na(KenyaData$date_disch),]

###### Binary variables responses ####
## From visual inspection of data, we see some binary variables use yes/no
## and some use y/n.
## Change all from yes/no to y/n for consistency

# Manually selecting variables to replace
binary_variables = c(11:14,16, 21:39,42,61,62,65)

# Change "yes" to "y" and "no" to "n"
for(i in binary_variables){
  if(class(KenyaData[[i]])[1] == "character"){
    for(j in 1:length(KenyaData$serial)){
      if(KenyaData[j,i] == "yes"){
        KenyaData[j,i] = "y"
      }
      else if(KenyaData[j,i] == "no"){
        KenyaData[j,i] = "n"
      }
    }
  }
}



##### Creating new variables ####
###### Length of admission ####
# Calculating length of stay in days
KenyaData$los = as.numeric(KenyaData$date_disch - KenyaData$date_admission)/(60*60*24)
summary(KenyaData$los)

###### Aggregate BCS ####
KenyaData$bcs = KenyaData$bcs_eyes + 
  KenyaData$bcs_motor +
  KenyaData$bcs_verbal
table(KenyaData$bcs)



###### Sats < 90 ####
KenyaData$sats_under_90 = if_else(KenyaData$oxysat < 90, "y", "n")

###### RSV postitivity ####
# RSV-A
ind = numeric(length(KenyaData$serial))
for(i in 1:length(KenyaData$serial)){
  tmp = KenyaData$rsva[i]
  if(is.na(tmp)){
    ind[i] = NA
  }
  else if(tmp == 0){
    ind[i] = 9
  }
  else if(tmp > 35){
    ind[i] = 0
  }
  else{
    ind[i] = 1
  }
}

KenyaData$rsva_ind = ind
table(KenyaData$rsva_ind)

# RSV-B
ind = numeric(length(KenyaData$serial))
for(i in 1:length(KenyaData$serial)){
  tmp = KenyaData$rsvb[i]
  if(is.na(tmp)){
    ind[i] = NA
  }
  else if(tmp == 0){
    ind[i] = 9
  }
  else if(tmp > 35){
    ind[i] = 0
  }
  else{
    ind[i] = 1
  }
}

KenyaData$rsvb_ind = ind
table(KenyaData$rsvb_ind)
table(KenyaData$rsva_ind, KenyaData$rsvb_ind)

# All RSV
tmp1 = which(KenyaData$rsva_ind == 1)
tmp2 = which(KenyaData$rsvb_ind == 1)
KenyaData$rsv_total = numeric(length(KenyaData$serial))
KenyaData$rsv_total[union(tmp1, tmp2)] = 1

###### Malnutrition by MUAC ####
# Coding used:
# 2 = Severely malnourished (MUAC < 11.5 cm)
# 1 = Moderately malnourished (MUAC < 12.5cm and >= 11.5cm)
# 0 = No/Mildly malnourished (MUAC >= 12.5cm)
ind = numeric(nrow(KenyaData))
for(i in 1:nrow(KenyaData)){
  tmp = KenyaData$muac[i]
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

KenyaData$malnutrition_muac = ind




##### Remove admissions with missing weight####
# Store entries with missing weights for analysis
KenyaData_missingWeight = KenyaData[is.na(KenyaData$weight),]

# Remove these from main data
KenyaData = KenyaData[!is.na(KenyaData$weight),]

##### Remove admissions with discharge type other than died or discharge ####
# Store for future analysis
KenyaData_disch = KenyaData[!(KenyaData$disch_type %in% c("discharge", "died")),]
KenyaData = KenyaData[(KenyaData$disch_type %in% c("discharge", "died")),]


##### Creating more new variables ####
###### Died or discharged? ####
KenyaData$died = if_else(KenyaData$disch_type == "died", 1, 0)
table(KenyaData$died)

###### Died within 7 days of admission ####
tmp1 = which(KenyaData$died == 1)
tmp2 = which(KenyaData$los <= 7)
tmp = intersect(tmp1,tmp2)
KenyaData$died_7days = numeric(length(KenyaData$serial))
KenyaData$died_7days[tmp] = 1

##### Removing missing data again ####
###### REMOVAL OF admissions with "d" in VOMITS_EVERYTHING. ######
table(KenyaData$vomits_everything)
tmp = which(KenyaData$vomits_everything == "d")
KenyaData[tmp, c("vomiting", "vomits_everything")]

KenyaData_missingVomits = KenyaData[tmp,]
KenyaData = KenyaData[-tmp,]

##### Remove those missing WFL or WFA #####
index1 = which(is.na(KenyaData$wt_for_ht))
index2 = which(is.na(KenyaData$wt_for_age))
index = union(index1, index2)
KenyaData_missingzScore = KenyaData[index,]
KenyaData = KenyaData[-index,]

##### Creating more new variables ####
###### Malnutrition by weight-for-age z-score ####
tmp = numeric(nrow(KenyaData))
for(i in 1:length(tmp)){
  if(KenyaData$wt_for_age[i] < -3){
    tmp[i] = 2
  }
  else if(KenyaData$wt_for_age[i] < -2){
    tmp[i] = 1
  }
}
KenyaData$malnutrition_wfaz = tmp

###### Malnutrition by weight-for-length z-score ####
tmp = numeric(nrow(KenyaData))
for(i in 1:length(tmp)){
  if(KenyaData$wt_for_ht[i] < -3){
    tmp[i] = 2
  }
  else if(KenyaData$wt_for_ht[i] < -2){
    tmp[i] = 1
  }
}
KenyaData$malnutrition_wflz = tmp

##### Review missing data again ####
## Review missing data after data cleaning completed
# Check for missingness in each variable
colNames = names(KenyaData)
for(i in 1:length(colNames)){
  num_missing = sum(is.na(KenyaData[,i]))
  if(num_missing != 0){
    print(paste0(colNames[i], ": ", sum(is.na(KenyaData[,i]))))
  }
}


# Remove temporary variables
rm(num_missing, binary_variables, colNames, i, j, ind, tmp, tmp1, tmp2, 
   vomiting_n, vomiting_y)





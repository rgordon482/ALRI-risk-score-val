### Calcultaing values for data flow chart

# Loading raw dataset (uncleaned)
KenyaData_raw =  read_excel("KCH_admission_2015to2024.xls")

# Split into age groups
KenyaData_raw_under2mon = filter(KenyaData_raw, age_months < 2)
KenyaData_raw_2_to_24mon = filter(KenyaData_raw, age_months >= 2 & age_months<= 24)
KenyaData_raw_over_24mon = filter(KenyaData_raw, age_months > 24)

# Number of admissions in each age group
nrow(KenyaData_raw_under2mon)
nrow(KenyaData_raw_2_to_24mon)
nrow(KenyaData_raw_over_24mon)

# Sanity check to ensure no admissions missed
nrow(KenyaData_raw_under2mon)+nrow(KenyaData_raw_2_to_24mon)+nrow(KenyaData_raw_over_24mon)


##### 2-24 mon ####
# Splitting into with and without ALRI discharge diagnosis
KenyaData_raw_2_to_24_mon_ALRI = filter(KenyaData_raw_2_to_24mon, diag1_disch %in% c("LRTI", "bronchiolitis") |
                                           diag2_disch %in% c("LRTI", "bronchiolitis"))

KenyaData_raw_2_to_24_mon_noALRI = KenyaData_raw_2_to_24mon[!(KenyaData_raw_2_to_24mon$serial %in% KenyaData_raw_2_to_24_mon_ALRI$serial),]

###### Analysis of admissions with discharge diagnosis of ALRI ####
# Number of admissions
nrow(KenyaData_raw_2_to_24_mon_ALRI)
nrow(KenyaData_raw_2_to_24mon) - nrow(KenyaData_raw_2_to_24_mon_ALRI)

# Removing children with missing data
KenyaData_raw_2_to_24_mon_ALRI_noMissing = KenyaData_raw_2_to_24_mon_ALRI[!is.na(KenyaData_raw_2_to_24_mon_ALRI$weight),]
KenyaData_raw_2_to_24_mon_ALRI_noMissing = KenyaData_raw_2_to_24_mon_ALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_ALRI_noMissing$wt_for_age),]
KenyaData_raw_2_to_24_mon_ALRI_noMissing = KenyaData_raw_2_to_24_mon_ALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_ALRI_noMissing$wt_for_ht),]
KenyaData_raw_2_to_24_mon_ALRI_noMissing = KenyaData_raw_2_to_24_mon_ALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_ALRI_noMissing$date_disch),]
tmp = which(KenyaData_raw_2_to_24_mon_ALRI_noMissing$vomits_everything == "d")
length(tmp)
KenyaData_raw_2_to_24_mon_ALRI_noMissing = KenyaData_raw_2_to_24_mon_ALRI_noMissing[-tmp,]

# Number of admissions with no missing data
nrow(KenyaData_raw_2_to_24_mon_ALRI_noMissing)

# Number of admissions with missing data
nrow(KenyaData_raw_2_to_24_mon_ALRI) - nrow(KenyaData_raw_2_to_24_mon_ALRI_noMissing)

# Number of admissions with discharge type other than died or discharge
sum(!(KenyaData_raw_2_to_24_mon_ALRI_noMissing$disch_type %in% c("died", "discharge")))
table(KenyaData_raw_2_to_24_mon_ALRI_noMissing$disch_type)

# Number of admissions with discharge type of died or discharge
sum((KenyaData_raw_2_to_24_mon_ALRI_noMissing$disch_type %in% c("died", "discharge")))


###### Analysis of children with no ALRI ####
# Number of admissions
nrow(KenyaData_raw_2_to_24_mon_noALRI)

# Removing admissions with missing data
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI[!is.na(KenyaData_raw_2_to_24_mon_noALRI$weight),]
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_noALRI_noMissing$weight),]
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_noALRI_noMissing$wt_for_age),]
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_noALRI_noMissing$wt_for_ht),]
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI_noMissing[!is.na(KenyaData_raw_2_to_24_mon_noALRI_noMissing$date_disch),]
tmp = which(KenyaData_raw_2_to_24_mon_noALRI_noMissing$vomits_everything == "d")
length(tmp)
KenyaData_raw_2_to_24_mon_noALRI_noMissing = KenyaData_raw_2_to_24_mon_noALRI_noMissing[-tmp,]

# Number of admissions with no missing data
nrow(KenyaData_raw_2_to_24_mon_noALRI_noMissing)

# Number of admissions with missing data
nrow(KenyaData_raw_2_to_24_mon_noALRI) - nrow(KenyaData_raw_2_to_24_mon_noALRI_noMissing)
table(KenyaData_raw_2_to_24_mon_noALRI_noMissing$disch_type)

# Number of admissions with outcome other than died or discharge
sum(!(KenyaData_raw_2_to_24_mon_noALRI_noMissing$disch_type %in% c("died", "discharge")))



##### Under 2 months ####
# Removing children without discharge diagnosis of ALRI
KenyaData_raw_under_2mon_ALRI = filter(KenyaData_raw_under2mon, diag1_disch %in% c("LRTI", "bronchiolitis") |
                                           diag2_disch %in% c("LRTI", "bronchiolitis"))

# Number of admissions
nrow(KenyaData_raw_under_2mon_ALRI)

# Removing admissions with missing data
KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI[!is.na(KenyaData_raw_under_2mon_ALRI$weight),]
KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI_noMissing[!is.na(KenyaData_raw_under_2mon_ALRI_noMissing$weight),]
KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI_noMissing[!is.na(KenyaData_raw_under_2mon_ALRI_noMissing$wt_for_age),]
KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI_noMissing[!is.na(KenyaData_raw_under_2mon_ALRI_noMissing$wt_for_ht),]
KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI_noMissing[!is.na(KenyaData_raw_under_2mon_ALRI_noMissing$date_disch),]
tmp = which(KenyaData_raw_under_2mon_ALRI_noMissing$vomits_everything == "d")
length(tmp)
#KenyaData_raw_under_2mon_ALRI_noMissing = KenyaData_raw_under_2mon_ALRI_noMissing[-tmp,]

# Number of admissions without missing data
nrow(KenyaData_raw_under_2mon_ALRI_noMissing)

# Number of admissions with missing data
nrow(KenyaData_raw_under_2mon_ALRI) - nrow(KenyaData_raw_under_2mon_ALRI_noMissing)

# Number of admissions with discharge type other than discharged or died
sum(!(KenyaData_raw_under_2mon_ALRI_noMissing$disch_type %in% c("died", "discharge")))


##### 25-59 months ####
# Removing children without discharge diagnosis of ALRI
KenyaData_raw_over_25mon_ALRI = filter(KenyaData_raw_over_24mon, diag1_disch %in% c("LRTI", "bronchiolitis") |
                                         diag2_disch %in% c("LRTI", "bronchiolitis"))

# Number of admissions
nrow(KenyaData_raw_over_25mon_ALRI)

# Remove admissions with missing data
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI[!is.na(KenyaData_raw_over_25mon_ALRI$weight),]
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI_noMissing[!is.na(KenyaData_raw_over_25mon_ALRI_noMissing$weight),]
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI_noMissing[!is.na(KenyaData_raw_over_25mon_ALRI_noMissing$wt_for_age),]
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI_noMissing[!is.na(KenyaData_raw_over_25mon_ALRI_noMissing$wt_for_ht),]
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI_noMissing[!is.na(KenyaData_raw_over_25mon_ALRI_noMissing$date_disch),]
tmp = which(KenyaData_raw_over_25mon_ALRI_noMissing$vomits_everything == "d")
length(tmp)
KenyaData_raw_over_25mon_ALRI_noMissing = KenyaData_raw_over_25mon_ALRI_noMissing[-tmp,]

# Number of admissions without missing data
nrow(KenyaData_raw_over_25mon_ALRI_noMissing)

# Number of admissions with missing data
nrow(KenyaData_raw_over_25mon_ALRI) - nrow(KenyaData_raw_over_25mon_ALRI_noMissing)

# Number of admissions with outcome other than died or discharge
sum(!(KenyaData_raw_over_25mon_ALRI_noMissing$disch_type %in% c("died", "discharge")))

##### Main sample with no missing data ####
mainSample_noMissing = filter(KenyaData_raw_2_to_24mon, diag1_disch %in% c("LRTI", "bronchiolitis") |
                                diag2_disch %in% c("LRTI", "bronchiolitis"))
# Remove missing outcome
mainSample_noMissing = filter(mainSample_noMissing, disch_type %in% c("died", "discharge"))

# Try to evaluate scores
mainSample_noMissing$RISC = RISC_fun(mainSample_noMissing)

# Check for missingness in each variable
colNames = names(mainSample_noMissing)
for(i in 1:length(colNames)){
  num_missing = sum(is.na(mainSample_noMissing[,i]))
  if(num_missing != 0){
    print(paste0(colNames[i], ": ", sum(is.na(mainSample_noMissing[,i]))))
  }
}

nrow(mainSample_noMissing) - nrow(respData_2_to_24_mon)
view(mainSample_noMissing[is.na(weight),])
view(mainSample_noMissing[is.na(wt_for_age),])

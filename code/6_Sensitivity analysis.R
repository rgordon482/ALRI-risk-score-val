##### COHORT B ####
# AUC of scores in children 2 - 24 months with any discharge diagnosis 
# Filter data
KenyaData_2_to_24_mon = filter(KenyaData, age_months >= 2 &
                                 age_months <= 24)

# Cohort stats
nrow(KenyaData_2_to_24_mon)
sum(KenyaData_2_to_24_mon$died)
mean(KenyaData_2_to_24_mon$died)

# Calculate AUROCs
aucTib = AUC_fun_mort(KenyaData_2_to_24_mon, scoresToTest)
view(aucTib)

roc.test(roc_mRISC,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$mRISC))
roc.test(roc_PERCH,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$PERCH))
roc.test(roc_PREPARE,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$PREPARE))
roc.test(roc_ReSVinet,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$ReSVinet))
roc.test(roc_ReSVinet_Maln_MUAC,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$ReSVinet_Maln_MUAC))
roc.test(roc_RISC,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$RISC))
roc.test(roc_RISC_Malawi_MUAC,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$RISC_Malawi_MUAC))
roc.test(roc_RISC_Malawi_WAZ,
         roc(KenyaData_2_to_24_mon$died, KenyaData_2_to_24_mon$RISC_Malawi_WAZ))




##### COHORT C ####
# AUC of scores in children 2 - 59 months with ALRI discharge diagnosis

# Filter data
respData_2_to_59_mon = filter(KenyaData, age_months >= 2)
respData_2_to_59_mon = filter(respData_2_to_59_mon, diag1_disch %in% c("LRTI", "bronchiolitis")|
                              diag2_disch %in% c("LRTI", "bronchiolitis") )
# Cohort stats
nrow(respData_2_to_59_mon)
sum(respData_2_to_59_mon$died)
mean(respData_2_to_59_mon$died)

# Calculate AUROCs
## Note removal of ReSVinet from assessed scores, as it can only be assessed up
## to 36 months
aucTib = AUC_fun_mort(respData_2_to_59_mon, c("mRISC", "PERCH", "PREPARE", "RISC", "RISC_Malawi_MUAC",
                                              "RISC_Malawi_WAZ"))
view(aucTib)

roc.test(roc_RISC,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$RISC))
roc.test(roc_mRISC,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$mRISC))
roc.test(roc_RISC_Malawi_MUAC,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$RISC_Malawi_MUAC))
roc.test(roc_PERCH,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$PERCH))
roc.test(roc_PREPARE,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$PREPARE))
roc.test(roc_RISC_Malawi_WAZ,
         roc(respData_2_to_59_mon$died, respData_2_to_59_mon$RISC_Malawi_WAZ))




##### Main sample, split before and after COVID ####
# 13 March documented as first detected case of COVID in Kenya

# Date of first COVID case in Kenya
covidDate = as.POSIXct("2020-03-13", "%Y-%m-%d", tz = "UTC")

# Split data into pre- and post- COVID
respData_2_to_24_mon_preCovid = filter(respData_2_to_24_mon, date_admission < covidDate)
respData_2_to_24_mon_postCovid = filter(respData_2_to_24_mon, date_admission >= covidDate)

# Number of admissions pre and post covid
nrow(respData_2_to_24_mon_preCovid)
nrow(respData_2_to_24_mon_postCovid)

# Sanity check to ensure all admissions accounted for
nrow(respData_2_to_24_mon_preCovid) + nrow(respData_2_to_24_mon_postCovid)

# Summary stats
nrow(respData_2_to_24_mon_preCovid)
sum(respData_2_to_24_mon_preCovid$died)
mean(respData_2_to_24_mon_preCovid$died)

###### COHORT F ####
# Calculate AUROC for pre-COVID admissions
aucTib1 = AUC_fun_mort(respData_2_to_24_mon_preCovid, scoresToTest)
view(aucTib1)


# Uncorrelated ROC tests: pre vs. all
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$mRISC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$mRISC))
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$PERCH),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PERCH))
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$PREPARE),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PREPARE))
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$ReSVinet),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet))
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$ReSVinet_Maln_MUAC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet_Maln_MUAC))
roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$RISC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC))

roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$RISC_Malawi_MUAC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_MUAC))$p.value

roc.test(roc(respData_2_to_24_mon_preCovid$died, respData_2_to_24_mon_preCovid$RISC_Malawi_WAZ),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_WAZ))



##### Excluding first COVID period ###
# Excluding the year following the first case of COVID
# Date of first COVID case in Kenya
covidDate = as.POSIXct("2020-03-13", "%Y-%m-%d", tz = "UTC")

# One year from first COVID case
covidDate1 = as.POSIXct("2021-03-13", "%Y-%m-%d", tz = "UTC")

# Admissions over year following first COVID period
respData_2_to_24_mon_CovidPeriod = filter(respData_2_to_24_mon, date_admission >=  covidDate &
                                            date_admission <= covidDate1)

nrow(respData_2_to_24_mon_CovidPeriod)
view(respData_2_to_24_mon_CovidPeriod)

 
###### COHORT E ####
# Admissions outside of covid period
respData_2_to_24_mon_outCovidPeriod = respData_2_to_24_mon[!(respData_2_to_24_mon$serial %in% 
                                                               respData_2_to_24_mon_CovidPeriod$serial),]

nrow(respData_2_to_24_mon_CovidPeriod) + nrow(respData_2_to_24_mon_outCovidPeriod)

# Cohort stats
nrow(respData_2_to_24_mon_outCovidPeriod)
sum(respData_2_to_24_mon_outCovidPeriod$died)
mean(respData_2_to_24_mon_outCovidPeriod$died)

# AUROCs
view(AUC_fun_mort(respData_2_to_24_mon_outCovidPeriod, scoresToTest))

####### unpaired ROC tests: Covid period excluded vs. included ####
roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$mRISC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$mRISC))$p.value
roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$PERCH),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PERCH))$p.value
roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$PREPARE),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$PREPARE))$p.value
roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$ReSVinet),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet))$p.value
roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$ReSVinet_Maln_MUAC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$ReSVinet_Maln_MUAC))

roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$RISC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC))

roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$RISC_Malawi_MUAC),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_MUAC))

roc.test(roc(respData_2_to_24_mon_outCovidPeriod$died, respData_2_to_24_mon_outCovidPeriod$RISC_Malawi_WAZ),
         roc(respData_2_to_24_mon$died, respData_2_to_24_mon$RISC_Malawi_WAZ))





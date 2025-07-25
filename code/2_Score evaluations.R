# RISC-Malawi (MUAC)
KenyaData$RISC_Malawi_MUAC = RISC_Malawi_MUAC_fun(KenyaData)

# RISC-Malawi (WAZ)
KenyaData$RISC_Malawi_WAZ = RISC_Malawi_WAZ_fun(KenyaData)

# mRISC
KenyaData$mRISC = mRISC_fun(KenyaData)

# RISC (HIV-Negative)
KenyaData$RISC = RISC_fun(KenyaData)

# PERCH
KenyaData$PERCH = PERCH_fun(KenyaData)

# PREPARE
KenyaData$PREPARE = PREPARE_fun(KenyaData)

# ReSVinet
KenyaData$ReSVinet = ReSVinet_fun(KenyaData)

##### ReSVinet + Nutrition status ####
## +2 for Moderate malnutrition, +3 for Severe malnutrition

## Using MUAC
KenyaData$ReSVinet_Maln_MUAC = KenyaData$ReSVinet + if_else(KenyaData$malnutrition_muac !=0, KenyaData$malnutrition_muac + 1, 0)

## Using weight-for-age
KenyaData$ReSVinet_Maln_wfaz = KenyaData$ReSVinet + if_else(KenyaData$malnutrition_wfaz !=0, KenyaData$malnutrition_wfaz + 1, 0)

## Using weight-for-length
KenyaData$ReSVinet_Maln_wflz = KenyaData$ReSVinet + if_else(KenyaData$malnutrition_wflz !=0, KenyaData$malnutrition_wflz+ 1, 0)

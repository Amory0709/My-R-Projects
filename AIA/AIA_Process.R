library(Hmisc)
library(data.table)
library(rms)

load("C:/Users/S0047176/Desktop/AIA/AIA_ID_Claim_20190605.rdata") 
#61823 records 55 variables
load("C:/Users/S0047176/Desktop/AIA/AIA_ID_Policy_20190605.rdata")
#1131437 records 40 variables

#1. Data cleaning (Source by: Xingpu)

#Policy Data
AIA_ID_Policy_20190605$PlanOption_v2 = 
  ifelse(AIA_ID_Policy_20190605$PlanOption == "silver",
         "Silver",
         as.character(AIA_ID_Policy_20190605$PlanOption))#fix planOption

#define unique record
AIA_ID_Policy_20190605$new_id = paste(
  as.character(AIA_ID_Policy_20190605$DummyID),
  as.character(AIA_ID_Policy_20190605$CalendarMonth),
  as.character(AIA_ID_Policy_20190605$RiderType),
  as.character(AIA_ID_Policy_20190605$PlanOption_v2),
  sep = "_")
length(unique(AIA_ID_Policy_20190605$new_id)) # 1131240
#
AIA_ID_Policy_20190605$new_id = paste(
as.character(AIA_ID_Policy_20190605$DummyID),
as.character(AIA_ID_Policy_20190605$CalendarMonth),
as.character(AIA_ID_Policy_20190605$RiderType),
as.character(AIA_ID_Policy_20190605$PlanOption_v2),
as.character(AIA_ID_Policy_20190605$PolicyType),
sep = "_")
length(unique(AIA_ID_Policy_20190605$new_id)) # 1131267

AIA_ID_Policy_20190605$new_id = paste(
  as.character(AIA_ID_Policy_20190605$DummyID),
  as.character(AIA_ID_Policy_20190605$CalendarMonth),
  as.character(AIA_ID_Policy_20190605$RiderType),
  as.character(AIA_ID_Policy_20190605$PlanOption_v2),
  as.character(AIA_ID_Policy_20190605$PolicyType),
  as.character(AIA_ID_Policy_20190605$AttainedAge_LB),
  sep = "_")
length(unique(AIA_ID_Policy_20190605$new_id)) # 1131266

# Final choice:
AIA_ID_Policy_20190605$new_id = paste(
  as.character(AIA_ID_Policy_20190605$DummyID),
  as.character(AIA_ID_Policy_20190605$CalendarMonth),
  as.character(AIA_ID_Policy_20190605$RiderType),
  as.character(AIA_ID_Policy_20190605$PlanOption_v2),
  sep = "_")
length(unique(AIA_ID_Policy_20190605$new_id)) # 1131240

test = subset(
  AIA_ID_Policy_20190605,
  AIA_ID_Policy_20190605$new_id 
  %in% AIA_ID_Policy_20190605$new_id[duplicated(AIA_ID_Policy_20190605$new_id)])



policy_v1 = AIA_ID_Policy_20190605[!duplicated(AIA_ID_Policy_20190605$new_id), ]
length(unique(policy_v1$new_id)) # 1131240

# claim data
names(AIA_ID_Claim_20190605)[c(42, 43, 46, 47)] = 
  c("Perc_Paid_Claims", 
    "Perc_Paid_Claims_with_IBNS",
    "Perc_Incurred_Claims",
    "Perc_Incurred_Claims_with_IBNS")

setDT(AIA_ID_Claim_20190605)
claim_v1 =
  AIA_ID_Claim_20190605[,
                        list(
                          PAYAMT = sum(Perc_Paid_Claims, na.rm = TRUE),
                          PAYAMT_IBNS = sum(Perc_Paid_Claims_with_IBNS, na.rm = TRUE),
                          RI_PAYAMT = sum(RI_Paid_Claims, na.rm = TRUE),
                          RI_PAYAMT_IBNS = sum(RI_Paid_Claims_with_IBNS, na.rm = TRUE),
                          INCURRED_AMT = sum(Perc_Incurred_Claims, na.rm = TRUE),
                          INCURRED_AMT_IBNS = sum(Perc_Incurred_Claims_with_IBNS, na.rm = TRUE),
                          RI_INCURRED_AMT = sum(RI_Incurred_Claims, na.rm = TRUE),
                          RI_INCURRED_AMT_IBNS = sum(RI_Incurred_Claims_with_IBNS, na.rm = TRUE)
                        ),
                        by = list(DummyID, IncurredMonth, ClaimNo, RiderType, PlanOption)]

sum(is.na(claim_v1)) # 0
#claim_v1: 23740 14

claim_v1$CLAIM_CNT = 1

claim_v2 =
  claim_v1[,
           list(
             PAYAMT = sum(PAYAMT),
             PAYAMT_IBNS = sum(PAYAMT_IBNS),
             RI_PAYAMT = sum(RI_PAYAMT),
             RI_PAYAMT_IBNS = sum(RI_PAYAMT_IBNS),
             INCURRED_AMT = sum(INCURRED_AMT),
             INCURRED_AMT_IBNS = sum(INCURRED_AMT_IBNS),
             RI_INCURRED_AMT = sum(RI_INCURRED_AMT),
             RI_INCURRED_AMT_IBNS = sum(RI_INCURRED_AMT_IBNS),
             CLAIM_CNT = sum(CLAIM_CNT)
           ),
           by = list(DummyID, IncurredMonth, RiderType, PlanOption)]

claim_v2$new_id = paste(
  as.character(claim_v2$DummyID),
  as.character(claim_v2$IncurredMonth),
  as.character(claim_v2$RiderType),
  as.character(claim_v2$PlanOption),
  sep = "_")
length(unique(claim_v2$new_id)) # 13562

sum(claim_v2$CLAIM_CNT) # 23740

# Merge
AIA_MOD_DATA = merge(policy_v1, claim_v2, by = "new_id", all.x = TRUE) #1131240

sum(AIA_MOD_DATA$CLAIM_CNT, na.rm = TRUE) # 23575

test2 = subset(claim_v2, claim_v2$new_id %in% policy_v1$new_id) #13453

#Fill NA
AIA_MOD_DATA$PAYAMT[is.na(AIA_MOD_DATA$PAYAMT)]<-0
AIA_MOD_DATA$PAYAMT_IBNS[is.na(AIA_MOD_DATA$PAYAMT_IBNS)]<-0
AIA_MOD_DATA$RI_PAYAMT[is.na(AIA_MOD_DATA$RI_PAYAMT)]<-0
AIA_MOD_DATA$RI_PAYAMT_IBNS[is.na(AIA_MOD_DATA$RI_PAYAMT_IBNS)]<-0

AIA_MOD_DATA$INCURRED_AMT[is.na(AIA_MOD_DATA$INCURRED_AMT)]<-0
AIA_MOD_DATA$INCURRED_AMT_IBNS[is.na(AIA_MOD_DATA$INCURRED_AMT_IBNS)]<-0
AIA_MOD_DATA$RI_INCURRED_AMT[is.na(AIA_MOD_DATA$RI_INCURRED_AMT)]<-0
AIA_MOD_DATA$RI_INCURRED_AMT_IBNS[is.na(AIA_MOD_DATA$RI_INCURRED_AMT_IBNS)]<-0

AIA_MOD_DATA$CLAIM_CNT[is.na(AIA_MOD_DATA$CLAIM_CNT)] <- 0

# remove duplicated columns
# remove dummyID.y
length(AIA_MOD_DATA$DummyID.y[!is.na(AIA_MOD_DATA$DummyID.y)]) #13453
AIA_MOD_DATA$DummyID.y <- NULL
names(AIA_MOD_DATA)[4] <- "DummyID"
names(AIA_MOD_DATA)[4] 

#remove PlanOption.x&y keep PlanOption_v2
length(AIA_MOD_DATA$PlanOption.y[!is.na(AIA_MOD_DATA$PlanOption.y)]) #13453
table(AIA_MOD_DATA$PlanOption.y)
table(AIA_MOD_DATA$PlanOption.x)
AIA_MOD_DATA$PlanOption.y <- NULL
AIA_MOD_DATA$PlanOption.x <- NULL

#remove RiderType.y
length(AIA_MOD_DATA$RiderType.y[!is.na(AIA_MOD_DATA$RiderType.y)]) #13453
AIA_MOD_DATA$RiderType.y <- NULL
names(AIA_MOD_DATA)
names(AIA_MOD_DATA)[7] <- "RiderType"
names(AIA_MOD_DATA)[7] 


save(AIA_MOD_DATA, file = 'AIA_MOD_DATA.Rdata')

# 2. Data Exploration
AIA_MOD_DATA$Exposure[1]*12
# Target: Claim count and amount and exposure count and amount in different age
setDT(AIA_MOD_DATA)
library("dplyr")
attach(AIA_MOD_DATA)

#overall data exploration
AIA_MOD_GroupBy_Age <-
  AIA_MOD_DATA %>% 
  group_by(AttainedAge_LB)%>% 
  summarise(
    PAYAMT = sum(PAYAMT),
    PAYAMT_IBNS = sum(PAYAMT_IBNS),
    RI_PAYAMT = sum(RI_PAYAMT),
    RI_PAYAMT_IBNS = sum(RI_PAYAMT_IBNS),
    INCURRED_AMT = sum(INCURRED_AMT),
    INCURRED_AMT_IBNS = sum(INCURRED_AMT_IBNS),
    RI_INCURRED_AMT = sum(RI_INCURRED_AMT),
    RI_INCURRED_AMT_IBNS = sum(RI_INCURRED_AMT_IBNS),
    CLAIM_CNT = sum(CLAIM_CNT),
    #Exposure_CNT = count(Exposure),
    Exposure_AMT = sum(Exposure, na.rm = TRUE)
  ) %>% plot

#Besides from Exposure_amt
# reduce from 0- 10
# little fluctuate between 10 - 20
# grow from 20-40
# peak in around 50-60
# reduce after 50-60

# Exporsure amt
# reduce from 0- 10
# little fluctuate between 10 - 20
# grow from 20-40
# peak in around 40
# reduce after 40
names(AIA_ID_Policy_20190605)
names(AIA_MOD_DATA)
AIA_MOD_DATA %>% 
  group_by(AttainedAge_LB)%>% 
  count(Exposure)%>% plot 
# reduce from 0- 10
# little fluctuate between 10 - 20
# grow from 20-40
# peak in around 40
# reduce after 40

# not claimed policy
no_claim_policy = AIA_MOD_DATA$CLAIM_CNT == 0
AIA_MOD_DATA[no_claim_policy,] %>%
  group_by(AttainedAge_LB)%>% 
  count(Exposure)%>% plot 
# reduce from 0- 10
# little fluctuate between 10 - 20
# grow from 20-40
# peak in around 40
# reduce after 40

# claimed policy
AIA_MOD_DATA[!no_claim_policy,] %>%
  group_by(AttainedAge_LB)%>% 
  count(Exposure)%>% plot 
# reduce from 0- 10
# little fluctuate between 10 - 20
# grow from 20-40
# peak in around 45
# reduce after 45
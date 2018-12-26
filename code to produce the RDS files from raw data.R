## Code to get the per person spending

#import full data - 47mb of RAM
financeData <- read.csv("/home/sam/data/ASC Finance Report/ASCFR Data with descriptions.csv")

# Change ITEMVALUE from a FACTOR to NUMERIC
tmpItemValue <- as.character(financeData$ITEMVALUE)
financeData$ITEMVALUE <- parse_number(tmpItemValue, na = ":")
rm(tmpItemValue)


grossCurrentExpenditureAllPSRs <- filter(financeData, DATA_LEVEL == "Gross Current Expenditure" & 
                                                GEOGRAPHY_LEVEL == "Local Authority" &
                                                CareType_Key != 99 & 
                                                PrimarySupportReason_Key !=99 &
                                                SupportSetting_Key != 99 &
                                                Purpose_Key != 99 &
                                                CarerSupportType_Key !=99
)

# Take out the 173 (out of 15k) negative values which must be errors as we are looking at 
# gross current 
grossCurrentExpenditureAllPSRs <- filter(grossCurrentExpenditureAllPSRs, ITEMVALUE >= 0)

# Make the value pounds rather than millions
grossCurrentExpenditureAllPSRs$ITEMVALUE <- grossCurrentExpenditureAllPSRs$ITEMVALUE * 1000

# NHS Digital have explained that the SupportSetting_Key field only applies
# to long term care, and is blank if the care type is "short term care" or other.
# So essentially I want to create a new field where either it contains SupportSetting_Key
# or if that is blank, it contains CareType_Key
grossCurrentExpenditureAllPSRs$supportOrCareType <- ifelse(grossCurrentExpenditureAllPSRs$SupportSetting_Key != "", 
                                                           as.character(grossCurrentExpenditureAllPSRs$SupportSetting_Key),
                                                           as.character(grossCurrentExpenditureAllPSRs$CareType_Key)
)

# Write out all gross current expenditure out so I can use it again
write_rds(grossCurrentExpenditureAllPSRs, "grossCurrentExpenditure.RDS", compress = "none")

# Read it back in for now
grossCurrentExpenditure <- read_rds("/home/sam/data/ShinyR tutorial/shinyCare/grossCurrentExpenditure.RDS")

# Read in the UK 2011 census population data
UKpop <- read.csv("/home/sam/data/ASC Finance Report/England population density GATESHEAD NORTHUMBERLAND NEW CODES.csv")

# get the name of the relevant column the same for join
names(UKpop)[1] <- "GEOGRAPHY_CODE" 

# make them both tibbles for join
grossCurrentExpenditure <- tbl_df(grossCurrentExpenditure)
UKpop <- tbl_df(UKpop)

# check dimensions to make sure join works
dim(grossCurrentExpenditure) # 14875    24
dim(UKpop) #439 3

# want to do a left join so we get all rows from X and all columns from Y
# so outcome should be 14875 by 26
# and there should be no NA values in the last 3 as they should all match
grossCurrentExpenditureWithPop <-  left_join(grossCurrentExpenditure, UKpop, by = "GEOGRAPHY_CODE")

# check it's right
sum(is.na(grossCurrentExpenditureWithPop$Area.name)) # 0 NAs - all matched

# Divide each point by its population for a total
grossCurrentExpenditureWithPop$SpendingPerPerson <- grossCurrentExpenditureWithPop$ITEMVALUE / grossCurrentExpenditureWithPop$Usual.residents

# Take out Isles of Sicilly as the figures are really high and break the graphs
# They are outliers with massive costs because it's an island and not much use

grossCurrentExpenditureWithPop <- filter(grossCurrentExpenditureWithPop, REGION_GO_CODE != "E12000009")


# write newSupportTable out just in case I want to start from here later
write_rds(grossCurrentExpenditureWithPop, "Support Type With Populations all PSRs excl Sicilly.rds")

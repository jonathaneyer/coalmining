# Build coal data

library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)
library(gdata)
# ---- start --------------------------------------------------------------


fuelreceipts_2014 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2014/fuelreceipts_2014.csv") %>%
  mutate(FUEL_COST = as.numeric(FUEL_COST)) %>%
  select(-`Average Mercury Content`)
fuelreceipts_2013 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2013/fuelreceipts_2013.csv") %>%
  mutate(FUEL_COST = as.numeric(FUEL_COST)) %>%
  setNames(names(fuelreceipts_2014))
fuelreceipts_2012 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2012/fuelreceipts_2012.csv")%>%
  mutate(COALMINE_MSHA_ID = as.numeric(COALMINE_MSHA_ID),
         FUEL_COST = as.numeric(FUEL_COST))%>%
  setNames(names(fuelreceipts_2014))
fuelreceipts_2011 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2011/fuelreceipts_2011.csv") %>%
  mutate(COALMINE_MSHA_ID = as.numeric(COALMINE_MSHA_ID),
         FUEL_COST = as.numeric(FUEL_COST))%>%
  setNames(names(fuelreceipts_2014))
fuelreceipts_2010 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2010/fuelreceipts_2010.csv") %>%
  mutate(CoalMine_MSHA_ID = as.numeric(CoalMine_MSHA_ID),
         Contract_Exp_Date = as.character(Contract_Exp_Date))%>%
  setNames(names(fuelreceipts_2014))
fuelreceipts_2009 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2009/fuelreceipts_2009.csv") %>%
  mutate(CoalMine_MSHA_ID = as.numeric(CoalMine_MSHA_ID),
         Contract_Exp_Date = as.character(Contract_Exp_Date))%>%
  setNames(names(fuelreceipts_2014))
fuelreceipts_2009 <- fuelreceipts_2009[,1:27] # get rid of a bunch of NA fields
fuelreceipts_2008 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2008/fuelreceipts_2008.csv") %>%
  mutate(Contract_Exp_Date = as.character(Contract_Exp_Date))%>%
  setNames(names(fuelreceipts_2014))

fercreceipts2007 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232007.xls") 
fercreceipts2006 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232006.xls")
fercreceipts2005 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232005.xls")
fercreceipts2004 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232004.xls")
fercreceipts2003 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232003.xls")
fercreceipts2002 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232002.xls")
eiareceipts2007 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232007/f4232007.xls") %>%
    mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2006 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232006/f4232006.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2005 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232005/f4232005.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2004 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232004/f4232004.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2003 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232003/e4232003.xls") %>%
  mutate(EXPIRATION = as.Date(EXPIRATION, format = '%m/%d/%Y'))
eiareceipts2002 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232002/f4232002.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
names(eiareceipts2003) <- names(eiareceipts2002)

receipts2001 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232001.xls")
receipts2000 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232000.xls")
receipts1999 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231999.xls")
receipts1998 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231998.xls")
receipts1997 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231997.xls")
receipts1996 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231996.xls")
receipts1995 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231995.xls")
receipts1994 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231994.xls")
receipts1993 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231993.xls")
receipts1992 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231992.xls")
receipts1991 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231991.xls")
receipts1990 <- read_excel("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231990.xls")

fuelreceipts90_98 <- bind_rows(receipts1990,receipts1991,receipts1992,receipts1993,receipts1994,receipts1995,
                               receipts1996,receipts1997,receipts1998)
fuelreceipts99_01 <- bind_rows(receipts1999,receipts2000,receipts2001)
fercreceipts02_07 <- bind_rows(fercreceipts2002,fercreceipts2003,fercreceipts2004,fercreceipts2005, fercreceipts2006, fercreceipts2007)
eiareceipts02_07 <- bind_rows(eiareceipts2002,eiareceipts2003,eiareceipts2004,eiareceipts2005, eiareceipts2006, eiareceipts2007)
fuelreceipts08_14 <- bind_rows(fuelreceipts_2008,fuelreceipts_2009,fuelreceipts_2010,fuelreceipts_2011,fuelreceipts_2012,fuelreceipts_2013,fuelreceipts_2014)
eiareceipts02_07$COST <- NA


eiareceipts02_07$FACILITY_STATE <- as.character(eiareceipts02_07$FACILITY_STATE)
eiareceipts02_07$FUEL_TYPE_CODE <- as.character(eiareceipts02_07$FUEL_TYPE_CODE)
eiareceipts02_07$COALMINE_TYPE   <- as.character(eiareceipts02_07$COALMINE_TYPE)
eiareceipts02_07$COALMINE_STATE   <- as.character(eiareceipts02_07$COALMINE_STATE)
eiareceipts02_07$COALMINE_COUNTY   <- as.character(eiareceipts02_07$COALMINE_COUNTY)
eiareceipts02_07$QUANTITY_RECEIVED <- as.numeric(eiareceipts02_07$QUANTITY_RECEIVED )
fercreceipts02_07$PLT_CODE <- as.character(fercreceipts02_07$PLT_CODE)
fercreceipts02_07$`PLST ST` <- as.character(fercreceipts02_07$`PLST ST`)
fercreceipts02_07$GENERFUEL <- as.character(fercreceipts02_07$GENERFUEL )
fercreceipts02_07$SPECF_FUEL <- as.character(fercreceipts02_07$SPECF_FUEL )
fercreceipts02_07$`ORIG ST` <- as.character(fercreceipts02_07$`ORIG ST` )
fercreceipts02_07$MINE_TYPE <- as.character(fercreceipts02_07$MINE_TYPE )
fercreceipts02_07$COUNTY <- as.character(fercreceipts02_07$COUNTY )
fercreceipts02_07$QUANTITY <- as.numeric(fercreceipts02_07$QUANTITY )
fercreceipts02_07$PLT_CODE <- as.numeric(fercreceipts02_07$PLT_CODE )
fuelreceipts08_14$`Plant State` <- as.character(fuelreceipts08_14$`Plant State`)
fuelreceipts08_14$ENERGY_SOURCE <- as.character(fuelreceipts08_14$ENERGY_SOURCE)
fuelreceipts08_14$FUEL_GROUP <- as.character(fuelreceipts08_14$FUEL_GROUP)
fuelreceipts08_14$FUEL_COST <- as.character(fuelreceipts08_14$FUEL_COST)
fuelreceipts08_14$QUANTITY <-  as.numeric(as.character(gsub(",","",x = fuelreceipts08_14$QUANTITY)))
fuelreceipts08_14$FUEL_COST <-  as.numeric(as.character(gsub(",","",x = fuelreceipts08_14$FUEL_COST)))
fuelreceipts08_14$`Coalmine Type` <- as.character(fuelreceipts08_14$`Coalmine Type`)
fuelreceipts08_14$`Coalmine State` <- as.character(fuelreceipts08_14$`Coalmine State`)
fuelreceipts08_14$`Coalmine County` <- as.character(fuelreceipts08_14$`Coalmine County`)
fuelreceipts99_01$GENER_FUEL <- as.character(fuelreceipts99_01$GENER_FUEL)
fuelreceipts99_01$MINE_TYPE <- as.character(fuelreceipts99_01$MINE_TYPE)
fuelreceipts99_01$SPECF_FUEL <- as.character(fuelreceipts99_01$SPECF_FUEL)
fuelreceipts99_01$QUANTITY <-  as.numeric(as.character(gsub(",","",x = fuelreceipts99_01$QUANTITY)))
fuelreceipts99_01$YEAR <- as.numeric(fuelreceipts99_01$YEAR)
fuelreceipts99_01$MONTH <- as.numeric(fuelreceipts99_01$MONTH)
fuelreceipts99_01$PLT_CODE <- as.numeric(fuelreceipts99_01$PLT_CODE)
fuelreceipts90_98$GENER_FUEL <- as.character(fuelreceipts90_98$GENER_FUEL)
fuelreceipts90_98$SPECF_FUEL <- as.character(fuelreceipts90_98$SPECF_FUEL)
fuelreceipts90_98$QUANTITY <-  as.numeric(as.character(gsub(",","",x = fuelreceipts90_98$QUANTITY)))
fuelreceipts90_98$YEAR <- as.numeric(fuelreceipts90_98$YEAR)
fuelreceipts90_98$MONTH <- as.numeric(fuelreceipts90_98$MONTH)
fuelreceipts90_98$PLT_CODE <- as.numeric(fuelreceipts90_98$PLT_CODE)

eiareceipts02_07$ENERGY_SOURCE <- 0
eiareceipts02_07$ENERGY_SOURCE[eiareceipts02_07$FUEL_TYPE_CODE %in% c("BIT","SUB","ANT","LIG")] <- 0
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 25] <- "PL"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 30] <- "SA"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 35] <- "AU"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 40] <- "CN"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 45] <- "CL"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 50] <- "VZ"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 55] <- "IS"


fuelreceipts08_14 <- fuelreceipts08_14[,c("YEAR","MONTH","Plant Id","Plant State","ENERGY_SOURCE","Coalmine Type","Coalmine State",
    "Coalmine County","QUANTITY","Average Heat Content","Average Sulfur Content","Average Ash Content",
    "FUEL_COST")]   
  #mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
  #       FUEL_COST = as.numeric(as.character(gsub(",","",x = FUEL_COST))))
fuelreceipts99_01 <- fuelreceipts99_01[,c("YEAR","MONTH","PLT_CODE","PLT_ST","SPECF_FUEL",
  "MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")] %>%
  #mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
   #      YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH), PLT_CODE = as.numeric(PLT_CODE)) %>%
  setNames(names(fuelreceipts08_14))
fuelreceipts90_98 <- fuelreceipts90_98[,c("YEAR","MONTH","PLT_CODE","PLT_ST","SPECF_FUEL",
  "MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")] %>%
  #mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
  #       YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH), PLT_CODE = as.numeric(PLT_CODE))%>%
  setNames(names(fuelreceipts08_14))
fercreceipts02_07 <- fercreceipts02_07[c("YEAR","MONTH","PLT_CODE","PLST ST","SPECF_FUEL",
  "MINE_TYPE","ORIG ST","COUNTY","QUANTITY","BTUCONTENT","SULFUR","ASH","COST")] %>%
  #mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))), PLT_CODE = as.numeric(PLT_CODE))%>%
  setNames(names(fuelreceipts08_14))
eiareceipts02_07 <- eiareceipts02_07[c("YEAR","MONTH","FACILITY_CODE","FACILITY_STATE",
  "FUEL_TYPE_CODE","COALMINE_TYPE","COALMINE_STATE","COALMINE_COUNTY","QUANTITY_RECEIVED","BTU_CONTENT",
  "SULFUR_CONTENT","ASH_CONTENT","COST")] %>%
  #mutate(QUANTITY_RECEIVED = as.numeric(as.character(gsub(",","",x = QUANTITY_RECEIVED))))%>%
  setNames(names(fuelreceipts08_14))

coalreceipts <- bind_rows(fuelreceipts08_14, fuelreceipts99_01,fuelreceipts90_98,fercreceipts02_07,eiareceipts02_07) %>%
  filter(ENERGY_SOURCE %in% c("BIT","SUB","LIG","ANT", `Coalmine County` != "")) %>%
  na.omit(`Coalmine County`, `Coalmine State`) %>%
  mutate(`Coalmine County` = as.numeric(`Coalmine County`)) 
names(coalreceipts)[names(coalreceipts) == "Coalmine State"] <- "Coalmine.State"
names(coalreceipts)[names(coalreceipts) == "Coalmine County"] <- "Coalmine.County"

coalreceipts$YEAR[coalreceipts$YEAR < 100] <- coalreceipts$YEAR[coalreceipts$YEAR < 100] + 1900

coalreceipts$coalminefips <- 0
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AL"] <- 1000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AL"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AK"] <- 2000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AK"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AZ"] <- 4000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AZ"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AR"] <- 5000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AR"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CO"] <- 8000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "CO"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IL"] <- 17000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "IL"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IN"] <- 18000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "IN"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "KS"] <- 20000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "KS"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "KY"] <- 21000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "KY"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "LA"] <- 22000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "LA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MD"] <- 24000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MD"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MO"] <- 29000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MO"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MS"] <- 28000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MS"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MT"] <- 30000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MT"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "ND"] <- 38000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "ND"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "NM"] <- 35000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "NM"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OH"] <- 39000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "OH"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OK"] <- 40000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "OK"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "PA"] <- 42000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "PA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "TN"] <- 47000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "TN"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "TX"] <- 48000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "TX"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "UT"] <- 49000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "UT"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "VA"] <- 51000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "VA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WV"] <- 54000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WV"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WY"] <- 56000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WY"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WA"] <- 53000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WA"]))

coalreceipts$coalminefips[coalreceipts$Coalmine.State == "1"] <- 1000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "1"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "17"] <- 17000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "17"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "18"] <- 18000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "18"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "19"] <- 19000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "19"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "20"] <- 20000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "20"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "21"] <- 21000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "21"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "22"] <- 22000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "22"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "24"] <- 24000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "24"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "29"] <- 29000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "29"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "3"] <- 3000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "3"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "30"] <- 30000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "30"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "35"] <- 35000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "35"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "38"] <- 38000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "38"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "39"] <- 39000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "39"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "4"] <- 4000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "4"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "40"] <- 40000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "40"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "42"] <- 42000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "42"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "47"] <- 47000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "47"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "48"] <- 48000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "48"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "49"] <- 49000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "49"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "51"] <- 51000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "51"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "53"] <- 53000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "53"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "54"] <- 54000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "54"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "56"] <- 56000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "56"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "8"] <- 8000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "8"]))

coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AU"] <- 1000000 ## Australia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CL"] <- 2000000 ## Columbia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CN"] <- 3000000 ## Canada
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IS"] <- 4000000 ## Indonesia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "ID"] <- 4000000 ## Indonesia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "PL"] <- 5000000 ## Poland
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "RS"] <- 6000000 ## Russia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "UK"] <- 7000000 ## UK
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "VZ"] <- 8000000 ## Venezuela
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OT"] <- 9000000 ## Other (to be dropped)
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IM"] <- 9000000 ## Other (to be dropped)

write_rds(coalreceipts,path =  "C:/Users/Jonathan/Google Drive/coalmining/coalmining_git/0-data/coalreceipts.rds")

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

fercreceipts2007 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232007.xls") 
fercreceipts2006 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232006.xls")
fercreceipts2005 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232005.xls")
fercreceipts2004 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232004.xls")
fercreceipts2003 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232003.xls")
fercreceipts2002 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232002.xls")
eiareceipts2007 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232007/f4232007.xls") %>%
    mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2006 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232006/f4232006.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2005 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232005/f4232005.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2004 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232004/f4232004.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
eiareceipts2003 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232003/e4232003.xls") %>%
  mutate(EXPIRATION = as.Date(EXPIRATION, format = '%m/%d/%Y'))
eiareceipts2002 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232002/f4232002.xls")%>%
  mutate(EXPIRATION_DT = as.Date(EXPIRATION_DT, format = '%m/%d/%Y'))
names(eiareceipts2003) <- names(eiareceipts2002)

receipts2001 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232001.xls")
receipts2000 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232000.xls")
receipts1999 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231999.xls")
receipts1998 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231998.xls")
receipts1997 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231997.xls")
receipts1996 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231996.xls")
receipts1995 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231995.xls")
receipts1994 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231994.xls")
receipts1993 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231993.xls")
receipts1992 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231992.xls")
receipts1991 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231991.xls")
receipts1990 <- read_xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231990.xls")

fuelreceipts90_98 <- bind_rows(receipts1990,receipts1991,receipts1992,receipts1993,receipts1994,receipts1995,
                               receipts1996,receipts1997,receipts1998)
fuelreceipts99_01 <- bind_rows(receipts1999,receipts2000,receipts2001)
fercreceipts02_07 <- bind_rows(fercreceipts2002,fercreceipts2003,fercreceipts2004,fercreceipts2005, fercreceipts2006, fercreceipts2007)
eiareceipts02_07 <- bind_rows(eiareceipts2002,eiareceipts2003,eiareceipts2004,eiareceipts2005, eiareceipts2006, eiareceipts2007)
fuelreceipts08_14 <- bind_rows(fuelreceipts_2008,fuelreceipts_2009,fuelreceipts_2010,fuelreceipts_2011,fuelreceipts_2012,fuelreceipts_2013,fuelreceipts_2014)
eiareceipts02_07$COST <- NA
rm(receipts1990,receipts1991,receipts1992,receipts1993,receipts1994,receipts1995,receipts1996,receipts1997,receipts1998)
rm(receipts1999,receipts2000,receipts2001)
rm(fercreceipts2002,fercreceipts2003,fercreceipts2004,fercreceipts2005, fercreceipts2006, fercreceipts2007)
rm(eiareceipts2002,eiareceipts2003,eiareceipts2004,eiareceipts2005, eiareceipts2006, eiareceipts2007)
rm(fuelreceipts_2008,fuelreceipts_2009,fuelreceipts_2010,fuelreceipts_2011,fuelreceipts_2012,fuelreceipts_2013,fuelreceipts_2014)


eiareceipts02_07$FACILITY_STATE <- as.character(eiareceipts02_07$FACILITY_STATE)
eiareceipts02_07$FUEL_TYPE_CODE <- as.character(eiareceipts02_07$FUEL_TYPE_CODE)
eiareceipts02_07$COALMINE_TYPE   <- as.character(eiareceipts02_07$COALMINE_TYPE)
eiareceipts02_07$COALMINE_STATE   <- as.character(eiareceipts02_07$COALMINE_STATE)
eiareceipts02_07$COALMINE_COUNTY   <- as.character(eiareceipts02_07$COALMINE_COUNTY)
fercreceipts02_07$PLT_CODE <- as.character(fercreceipts02_07$PLT_CODE)
fercreceipts02_07$`PLST ST` <- as.character(fercreceipts02_07$`PLST ST`)
fercreceipts02_07$GENERFUEL <- as.character(fercreceipts02_07$GENERFUEL )
fercreceipts02_07$SPECF_FUEL <- as.character(fercreceipts02_07$SPECF_FUEL )
fercreceipts02_07$`ORIG ST` <- as.character(fercreceipts02_07$`ORIG ST` )
fercreceipts02_07$MINE_TYPE <- as.character(fercreceipts02_07$MINE_TYPE )
fercreceipts02_07$COUNTY <- as.character(fercreceipts02_07$COUNTY )
fercreceipts02_07$QUANTITY <- as.character(fercreceipts02_07$QUANTITY )
fuelreceipts08_14$`Plant State` <- as.character(fuelreceipts08_14$`Plant State`)
fuelreceipts08_14$ENERGY_SOURCE <- as.character(fuelreceipts08_14$ENERGY_SOURCE)
fuelreceipts08_14$FUEL_GROUP <- as.character(fuelreceipts08_14$FUEL_GROUP)
fuelreceipts08_14$FUEL_COST <- as.character(fuelreceipts08_14$FUEL_COST)
fuelreceipts08_14$`Coalmine Type` <- as.character(fuelreceipts08_14$`Coalmine Type`)
fuelreceipts08_14$`Coalmine State` <- as.character(fuelreceipts08_14$`Coalmine State`)
fuelreceipts08_14$`Coalmine County` <- as.character(fuelreceipts08_14$`Coalmine County`)
fuelreceipts99_01$GENER_FUEL <- as.character(fuelreceipts99_01$GENER_FUEL)
fuelreceipts99_01$MINE_TYPE <- as.character(fuelreceipts99_01$MINE_TYPE)
fuelreceipts99_01$SPECF_FUEL <- as.character(fuelreceipts99_01$SPECF_FUEL)
fuelreceipts90_98$GENER_FUEL <- as.character(fuelreceipts90_98$GENER_FUEL)
fuelreceipts90_98$SPECF_FUEL <- as.character(fuelreceipts90_98$SPECF_FUEL)
eiareceipts02_07$ENERGY_SOURCE <- 0
eiareceipts02_07$ENERGY_SOURCE[eiareceipts02_07$FUEL_TYPE_CODE %in% c("BIT","SUB","ANT","LIG")] <- 0
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 25] <- "PL"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 30] <- "SA"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 35] <- "AU"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 40] <- "CN"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 45] <- "CL"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 50] <- "VZ"
fercreceipts02_07$`ORIG ST`[fercreceipts02_07$BOM_DIST == 55] <- "IS"


fuelreceipts08_14 <- fuelreceipts08_14[,c("YEAR","MONTH","Plant Id","Plant State","FUEL_GROUP","ENERGY_SOURCE","Coalmine Type","Coalmine State",
    "Coalmine County","QUANTITY","Average Heat Content","Average Sulfur Content","Average Ash Content",
    "FUEL_COST")]   %>%
  mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
         FUEL_COST = as.numeric(as.character(gsub(",","",x = FUEL_COST))))
fuelreceipts99_01 <- fuelreceipts99_01[,c("YEAR","MONTH","PLT_CODE","PLT_ST","GENER_FUEL","SPECF_FUEL",
  "MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")] %>%
  mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
         YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH), PLT_CODE = as.numeric(PLT_CODE)) %>%
  setNames(names(fuelreceipts08_14))
fuelreceipts90_98 <- fuelreceipts90_98[,c("YEAR","MONTH","PLT_CODE","PLT_ST","GENER_FUEL","SPECF_FUEL",
  "MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")] %>%
  mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))),
         YEAR = as.numeric(YEAR), MONTH = as.numeric(MONTH), PLT_CODE = as.numeric(PLT_CODE))%>%
  setNames(names(fuelreceipts08_14))
fercreceipts02_07 <- fercreceipts02_07[c("YEAR","MONTH","PLT_CODE","PLST ST","GENERFUEL","SPECF_FUEL",
  "MINE_TYPE","ORIG ST","COUNTY","QUANTITY","BTUCONTENT","SULFUR","ASH","COST")] %>%
  mutate(QUANTITY = as.numeric(as.character(gsub(",","",x = QUANTITY))), PLT_CODE = as.numeric(PLT_CODE))%>%
  setNames(names(fuelreceipts08_14))
eiareceipts02_07 <- eiareceipts02_07[c("YEAR","MONTH","FACILITY_CODE","FACILITY_STATE","ENERGY_SOURCE",
  "FUEL_TYPE_CODE","COALMINE_TYPE","COALMINE_STATE","COALMINE_COUNTY","QUANTITY_RECEIVED","BTU_CONTENT",
  "SULFUR_CONTENT","ASH_CONTENT","COST")] %>%
  mutate(QUANTITY_RECEIVED = as.numeric(as.character(gsub(",","",x = QUANTITY_RECEIVED))))%>%
  setNames(names(fuelreceipts08_14))

fuelreceipts <- bind_rows(fuelreceipts08_14, fuelreceipts99_01,fuelreceipts90_98,fercreceipts02_07,eiareceipts02_07)
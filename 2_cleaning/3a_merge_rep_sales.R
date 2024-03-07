# Setup ------------------------------------------------------------------------
# clear environment
rm(list = ls())

# load package function
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE) #install if necessary
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("rstudioapi","tidyverse","data.table","beepr")

lapply(packages, pkgTest)

# the following line is for getting the path of the current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one
setwd(dirname(current_path ))
# make sure it is the right directory
print( getwd() )

# Load -------------------------------------------------------------------------

dat <- fread("../3_output_data/ct_ma_rep_sales.csv")


# Missing permits by zip
missingZipYr <- fread("../3_output_data/Permits_missing_zip_year.csv")
dat <- merge(dat,missingZipYr[,c("zip","year","missing","yrHas5")],
      by.x=c("SITUS_ZIP_CODE","year"),
      by.y=c("zip","year"),
      all.x=T)
dat[is.na(yrHas5),"yrHas5"] <- 0
dat[is.na(missing),"missing"] <- 0
rm(missingZipYr)

solar <- fread("../3_output_data/solar.csv")

tax <- fread("../3_output_data/ctTax2.csv")
tax2 <- fread("../3_output_data/maTax2.csv")
tax <- rbind(tax,tax2)
rm(tax2)

# Merge ------------------------------------------------------------------------
## TAX -------------------------------
tax <- tax[order(TAX_YEAR,decreasing = T)]
tax <- tax[!duplicated(tax[,c("FIPS_CODE","APN_SEQUENCE_NBR","P_ID_IRIS_FRMTD")])]
dat <- merge(dat,tax,
      by.x=c("FIPS","APN_SEQUENCE_NUMBER","PCL_ID_IRIS_FRMTD"),
        by.y=c("FIPS_CODE","APN_SEQUENCE_NBR","P_ID_IRIS_FRMTD"),
      all.x=T)
is.na(dat$LIVING_SQUARE_FEET)%>% sum
rm(tax)

## MLS  -------------------------------

mls <- fread("../1_input_data/ct_ma_mls.csv",encoding = "Latin-1")
mls2 <- fread("../1_input_data/ct_ma_mls2.csv",encoding = "Latin-1")
mls$listDate <- mls$listDate %>%as.character
mls$ListDate <- mls$listDate
mls$listDate <- NULL
mls2$ListDate <- mls2$ListDate %>%as.character
mls <- rbindlist(list(mls2,mls),fill = T)
rm(mls2)

mls$year <- ((mls$ListDate%>%as.Date) + (mls$DOM %>%as.numeric)) %>% 
  substr(.,1,4) %>%as.numeric
mls$year[is.na(mls$year) | mls$year > 2019] <- mls$ListDate[is.na(mls$year)| mls$year > 2019] %>% 
  substr(.,1,4) %>% as.numeric

mls <- mls[!duplicated(mls[,c("ParcelNumber","AddressPostalCode","year")])]
mls$ParcelNumber <- NULL
mls$mlsID <- 1:nrow(mls)

mls$AddressStreetNumber <- stringi::stri_extract_first_regex(mls$AddressStreetNumber, 
                                                    "[0-9]+") %>% as.numeric
mls$AddressStreetName <- toupper(mls$AddressStreetName)
mls$AddressPostalCode <- mls$AddressPostalCode %>% as.numeric

dat$zip <- substr(dat$SITUS_ZIP_CODE,1,4) %>% as.numeric

mls <- mls[!ParcelNumber%in%c("","999999999")]

mls$ParcelNumber <- mls$ParcelNumber %>% as.integer
dat$APN__Parcel_Number___unformatted <- dat$APN__Parcel_Number___unformatted %>% as.numeric
dat$UNFORMATTED_APN <- dat$UNFORMATTED_APN%>%as.integer
dat$ORIGINAL_APN <- dat$ORIGINAL_APN%>%as.integer
dat$number


dat <- merge(dat[PCL_ID_IRIS_FRMTD!=""],
             mls[`ONLINE FORMATTED PARCEL ID`!="",
                 c("mlsID","year","ParcelNumber",
                   "AddressPostalCode")],
             by.x=c("APN__Parcel_Number___unformatted","year"),
             by.y=c("ParcelNumber","year"),
             all.x=T)

#Using PCL ID
dat <- merge(dat[PCL_ID_IRIS_FRMTD!=""],
             mls[`ONLINE FORMATTED PARCEL ID`!="",c("mlsID","year","ONLINE FORMATTED PARCEL ID")],
             by.x=c("PCL_ID_IRIS_FRMTD","year"),
             by.y=c("ONLINE FORMATTED PARCEL ID","year"),
             all.x=T)

dat <- merge(dat,mls[,c("AddressPostalCode","ParcelNumber","year","mlsID")],
             by.x=c("zip","UNFORMATTED_APN","year"),
             by.y=c("AddressPostalCode","ParcelNumber","year"),
             all.x=T)

dat$mlsID[is.na(dat$mlsID)] <- dat$mlsID.x[is.na(dat$mlsID)]
dat$mlsID[is.na(dat$mlsID)] <- dat$mlsID.y[is.na(dat$mlsID)]
dat$mlsID.x <- NULL
dat$mlsID.y <- NULL

is.na(dat$mlsID) %>%sum

dat <- merge(dat,mls[,c("AddressPostalCode","ParcelNumber","year","mlsID")],
             by.x=c("zip","ORIGINAL_APN","year"),
             by.y=c("AddressPostalCode","ParcelNumber","year"),
             all.x=T)

mls$street1 <- word(mls$AddressStreetName,1)
dat$street1 <- word(dat$SITUS_STREET_NAME,1)
dat$SITUS_HOUSE_NUMBER <- dat$SITUS_HOUSE_NUMBER %>% as.numeric
mls$AddressStreetNumber <- mls$AddressStreetNumber %>% as.numeric

dat <- merge(dat,mls[,c("AddressPostalCode","AddressStreetNumber","street1",
                        "year","mlsID")],
             by.x=c("zip","SITUS_HOUSE_NUMBER","street1","year"),
             by.y=c("AddressPostalCode","AddressStreetNumber",
                    "street1","year"),
             all.x=T)

dat$mlsID[is.na(dat$mlsID)] <- dat$mlsID.x[is.na(dat$mlsID)]
dat$mlsID[is.na(dat$mlsID)] <- dat$mlsID.y[is.na(dat$mlsID)]
dat$mlsID.x <- NULL
dat$mlsID.y <- NULL
dat$mlsID %>% is.na %>% sum


## SOLAR ------------------------------

## Exact on zip
dat$zip <- dat$SITUS_ZIP_CODE %>% substr(.,1,4) %>%as.numeric

dat <- merge(dat,
      solar[,c("hostcustomerstreetaddress","hostcustomerzip__4_","ProjectNumber")],
      by.x=c("street","zip"),
      by.y=c("hostcustomerstreetaddress","hostcustomerzip__4_"),
      all.x=T)

matched <- dat$ProjectNumber %>% unique
solar_no_match <- solar[!(ProjectNumber%in%matched)]
solar$state %>% table
solar_no_match$state %>% table

## Exact match on city
solar_no_match$hostcustomercity <- solar_no_match$hostcustomercity %>% toupper
dat$SITUS_CITY <- dat$SITUS_CITY %>% toupper
dat <- merge(dat,
              solar_no_match[,c("hostcustomerstreetaddress","hostcustomercity","ProjectNumber",
                       "state")],
              by.x=c("street","SITUS_CITY","SITUS_STATE"),
              by.y=c("hostcustomerstreetaddress","hostcustomercity","state"),
              all.x=T)

matched <- dat$ProjectNumber.y %>% unique
solar_no_match <- solar_no_match[!(ProjectNumber%in%matched)]
solar$state %>% table
solar_no_match$state %>% table

dat <- dat %>% dplyr::rename("ProjectNumber"="ProjectNumber.x")
dat$ProjectNumber[is.na(dat$ProjectNumber)] <- dat$ProjectNumber.y[is.na(dat$ProjectNumber)]
dat$ProjectNumber.y <- NULL

matchmat <- fread("../1_input_data/CT_solar_ev_matching.csv")

dat <- merge(dat,
      matchmat,
      by.x=c("PCL_ID_IRIS_FRMTD","FIPS","APN_SEQUENCE_NUMBER"),
      by.y=c("P-ID-IRIS-FRMTD","fips","APN"),
      all.x=T)

matched <- dat$ProjectNumber.y %>% unique
solar_no_match <- solar_no_match[!(ProjectNumber%in%matched)]
solar$state %>% table
solar_no_match$state %>% table

dat <- dplyr::rename(dat,"ProjectNumber"="ProjectNumber.x")
dat$ProjectNumber[is.na(dat$ProjectNumber)] <- dat$ProjectNumber.y[is.na(dat$ProjectNumber)]
dat$ProjectNumber.y <- NULL

dat <- merge(dat,solar, by="ProjectNumber",all.x=T)

# add vars 
dat <- dplyr::rename(dat, "year.dat"="year.x", "year.solar"="year.y")
dat$age.solar <- 0
dat$age.solar <- dat$year.dat - dat$year.solar + 1
dat$age.solar[dat$age.solar<0] <- 0
dat$age.solar[is.na(dat$age.solar)] <- 0
dat$age.solar[dat$age.solar>10] <- 1
  
## voting -----------------------
zip_pol <- fread("../1_input_data/aip_zips_ideology_v2022a.tab",sep="\t")
zip_pol <- zip_pol[grep(" MA| CT",zip_name),]
zip_pol <- zip_pol[,c("survey_period","population_2020","mrp_ideology_se",
                        "demshare_pres","mrp_ideology","presidential_year","zip")]


zip_pol$ideology_tercile <- cut(zip_pol$mrp_ideology,quantile(zip_pol$mrp_ideology,
                                                      probs=c(0,.33,.67,1),na.rm=T),
                            include.lowest=TRUE,labels=FALSE)
zip_pol$demshare_tercile <- cut(zip_pol$demshare_pres,quantile(zip_pol$demshare_pres,
                                                              probs=c(0,.33,.67,1),na.rm=T),
                                include.lowest=TRUE,labels=FALSE)
  
dat$survey_period <- "none"
dat$survey_period[dat$year.dat%in%2017:2021] <- "2017-2021"
dat$survey_period[dat$year.dat%in%2012:2016] <- "2012-2016"
dat$survey_period[dat$year.dat%in%2004:2011] <- "2004-2011"

dat <- merge(dat,zip_pol,by = c("zip","survey_period"),all.x=T)
dat[is.na(mrp_ideology),"mrp_ideology"] <- 0
dat[is.na(mrp_ideology),"population_2020"] <- 0
dat[is.na(mrp_ideology),"mrp_ideology_se"] <- 0
dat[is.na(mrp_ideology),"demshare_pres"] <- 0

## Permits -----

sumdPermits <- fread("../3_output_data/Permits.csv")

### Merge ----
rm(matchmat,mls,solar,solar_no_match,zip_pol)
gc()
dat <- merge(dat,sumdPermits,
             by.x=c("PCL_ID_IRIS_FRMTD","year.dat"),
             by.y=c("ONLINE FORMATTED PARCEL ID","year"),
             all.x=T)
Cnames <- sumdPermits %>% colnames
Cnames <- Cnames[3:length(Cnames)]

gc()
dat <- as.data.frame(dat)
dat[,Cnames] <- lapply(Cnames,function(x){
  dat[is.na(dat[,x]),x] <- 0
  dat[,x]
}) %>% do.call(cbind,.)
gc()
dat <- as.data.table(dat) 

# more cleaning -----------
dat$year.dat[is.na(dat$year.dat)] <- (dat$RECORDING_DATE %>%
                                          substr(.,1,4) %>% as.numeric)[is.na(dat$year.dat)]


# regression vars ------------
dat$kwhYr[is.na(dat$kwhYr)] <- 0
dat$watts[is.na(dat$watts)] <- 0

dat$age <- dat$year.dat-dat$year.solar
dat$age[dat$age<0] <- 0
dat$age[is.na(dat$age)] <- 0
dat$hasSolar <- (dat$age>0)*1
dat$age%>%summary
dat$storage[is.na(dat$storage)] <- 0

dat$CoYr <- paste0(dat$FIPS,dat$year.dat)
dat$ZipYr <- paste0(dat$zip,dat$year.dat)

dat <- dat %>% as.data.table
dat <- dat[REFI_FLAG!="Y" & CORPORATE_INDICATOR!="Y"]

dat$month <- dat$SALE_DATE%>%substr(.,5,6)

# save -------------------------
fwrite(dat,"../3_output_data/merged_data.csv");beep(3)

# Merge solar and mls ------------------------

solar <- fread("../3_output_data/solar.csv")

solar$houseNum <- word(solar$hostcustomerstreetaddress,1) %>% as.numeric
solar$street <- word(solar$hostcustomerstreetaddress,2)

mls <- merge(mls,
      solar[,c("street","houseNum","hostcustomerzip__4_","ProjectNumber","year")],
      by.x=c("AddressStreetNumber","street1","AddressPostalCode"),
      by.y=c("houseNum","street","hostcustomerzip__4_"),
      all.x = T)

mls$hasSolar <- (!is.na(mls$ProjectNumber))*1
mls$hasSolar[which(mls$year.x<mls$year.y)] <- 0

fwrite(mls,"../3_output_data/mls_solar.csv");beep(3)

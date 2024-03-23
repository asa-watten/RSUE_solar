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


# Load ------------------------------------------------------------------------
dat <- fread("../3_output_data/ct_ma_rep_sales.csv")
uID <- unique(dat$PCL_ID_IRIS_FRMTD)
rm(dat)

CTpermit <- fread("../1_input_data/CT_permit.csv")
MApermit <- fread("../1_input_data/MA_permit.csv")
permit <- rbind(CTpermit,MApermit)
rm(CTpermit,MApermit)

# Get year and month
permit$year <- permit$`PERMIT EFFECTIVE DATE` %>% substr(.,1,4) %>% as.numeric
permit$year[permit$year<2000 |permit$year > 2023] <- NA
permit$year[is.na(permit$year)] <-
  permit$`PERMIT EXPIRATION DATE`[is.na(permit$year)] %>% substr(.,1,4) %>% 
  as.numeric
permit$year[permit$year<2000 |permit$year > 2023] <- NA
permit$month <- permit$`PERMIT EFFECTIVE DATE` %>% substr(.,5,6) %>% as.numeric
permit$month[is.na(permit$month)] <- 
  permit$`PERMIT EFFECTIVE DATE`[is.na(permit$month)] %>% substr(.,5,6) %>% 
  as.numeric

# Missing zip-years ------------------------------------------------------------

permit$one <- 1 
permit$zip <- permit$`SITUS ZIP CODE` %>% substr(.,1,4) %>% as.numeric
permit$zip[!is.na(permit$`PERMIT PROPERTY ZIP CODE`)] <- 
  permit$`PERMIT PROPERTY ZIP CODE`[!is.na(permit$`PERMIT PROPERTY ZIP CODE`)] %>% 
  substr(.,1,4) %>% as.numeric
yrStats <- permit[!is.na(year),.(N=sum(one)),by=c("zip","year")]
zipStats <- yrStats[,.(mean=mean(N),sd=sd(N)),by=c("zip")]
temp <- expand.grid("zip"=unique(yrStats$zip),"year"=c(2000:2022))
missingZipYr <- merge(temp,yrStats,by=c("zip","year"),all.x=T)
missingZipYr <- merge(missingZipYr,zipStats,by="zip",all.x=T) %>% as.data.table
missingZipYr$N[is.na(missingZipYr$N)] <- 0
missingZipYr$sd[is.na(missingZipYr$sd)] <- 1
missingZipYr <- missingZipYr[zip>=1000,]
missingZipYr$missing <- 0
missingZipYr$missing[missingZipYr$N<=10] <- 1

setorder(missingZipYr, cols = "zip", "year")
Consec <- missingZipYr[missing==0]
Consec$Consec <- 1

Consec$Consec[1:(nrow(Consec)-1)] <- 
  (Consec$year[1:(nrow(Consec)-1)] == Consec$year[2:nrow(Consec)]-1) |
  (Consec$zip[1:(nrow(Consec)-1)] != Consec$zip[2:nrow(Consec)])

while(sum(Consec$Consec==0)>=1){
  Consec <- Consec[Consec==1]
  Consec$Consec[1:(nrow(Consec)-1)] <- 
    (Consec$year[1:(nrow(Consec)-1)] == Consec$year[2:nrow(Consec)]-1) |
    (Consec$zip[1:(nrow(Consec)-1)] != Consec$zip[2:nrow(Consec)])
}

temp <- Consec[,.(N=sum(Consec),minYr=min(year),maxYr=max(year)),by="zip"]
temp$N %>% table
temp$permitZip <- 0
temp$permitZip[temp$minYr>2008 & temp$maxYr>2018] <- 1

#merge
missingZipYr <- merge(missingZipYr,temp,by="zip",all.x=T)
missingZipYr[is.na(permitZip),"permitZip"] <- 0
missingZipYr[is.na(permitZip),"permitZip"] <- 0

#frac permits by state
missingZipYr$state <- NA

missingZipYr$state[missingZipYr$zip%in%c(1000:2999)] <- "MA"
missingZipYr$state[missingZipYr$zip%in%c(5999:6999)] <- "CT"
missingZipYr[!is.na(state),.(frac=1-mean(missing)),by=c("year","state")] %>% 
  ggplot(aes(x=year,y=frac,group=state,linetype=state))+
  geom_line()+
  theme_minimal()+
  xlim(2000,2020)+
  labs(title="",
       x="",
       y="fraction")

ggsave("../5_analysis/fig_frac_permits.pdf",width = 5, height=5, 
       units="in")
missingZipYr$yrHas5 <- (missingZipYr$year >= (missingZipYr$minYr + 5))*1
missingZipYr$yrHas5[is.na(missingZipYr$yrHas5)] <- 0
fwrite(missingZipYr,"../3_output_data/Permits_missing_zip_year.csv")


# reduce obs -------------------------------------------------------------------

#reduce observations to sold properties
permit <- permit[`ONLINE FORMATTED PARCEL ID`%in%uID]
#remove very large permits
permit <- permit[`PERMIT JOB VALUE`<100000 | is.na(`PERMIT JOB VALUE`)]



# solar vars -------------------------------------------------------------------
permit$solar <- (grepl("solar",permit$`PERMIT DESCRIPTION`,ignore.case=T)&
                   !grepl("sky light",permit$`PERMIT DESCRIPTION`,ignore.case=T))*1 
permit$solarAndstorage <- (grepl("batteries|battery|storage|powerwall",
                                 permit$`PERMIT DESCRIPTION`,ignore.case=T) & 
                             grepl("solar",permit$`PERMIT DESCRIPTION`,ignore.case=T) & 
                             !grepl("no batt",permit$`PERMIT DESCRIPTION`,ignore.case=T))*1 
permit$storage <- (grepl("batteries|battery|energy storage|powerwall",
                         permit$`PERMIT DESCRIPTION`,ignore.case=T) & 
                     !grepl("no batt|no storage|remodel|sump|lift|battery operated|pump|smoke|
           burglar|fire|alarm|detector|generator",permit$`PERMIT DESCRIPTION`,
           ignore.case=T))*1
permit$storage[permit$storage==0 &permit$solarAndstorage==1] <- 1

# Remodel vars -------------------------------------------------------------------
permit$roof <- (grepl("roof",permit$`PERMIT DESCRIPTION`,ignore.case=T) &
                  grepl("replace|reroof|new|re-roof",permit$`PERMIT DESCRIPTION`,ignore.case=T)
)*1

permit$`PERMIT DESCRIPTION`[permit$roof==1 & permit$solar==1] %>% sample(.,50)

sum(permit$roof)
sum(permit$solar)

permit$kitchen <- (grepl("kitchen",permit$`PERMIT DESCRIPTION`,ignore.case=T) &
                     grepl("remodel|replace|update",permit$`PERMIT DESCRIPTION`,ignore.case=T))*1

permit$bath <- (grepl("bath",permit$`PERMIT DESCRIPTION`,ignore.case=T) &
                  grepl("remodel|replace|update|new",permit$`PERMIT DESCRIPTION`,
                        ignore.case=T))*1

permit$remodel <- (grepl("remodel",permit$`PERMIT DESCRIPTION`,
                         ignore.case=T))*1

permit$heatpump  <- (grepl("heatpump|heat pump|heat-pump|mini split|mini-split|minisplit",permit$`PERMIT DESCRIPTION`,
                           ignore.case=T))*1

permit$geothermal  <- (grepl("geothermal|geo-thermal|geo thermal",permit$`PERMIT DESCRIPTION`,
                           ignore.case=T))*1

# Make long year-property-value and project data  ------------------------------

discount <- 0.1
sumdPermits <- data.table()

for(y in 2009:2022){

  temp <- permit[year<y,.("year"=y,
                           #sum of discounted present value for NON SOLAR
                           "permitPresentVal"=sum(`PERMIT JOB VALUE`*
                                                    exp((year-y)*discount),
                                                  na.rm=T),
                          "permitPresentValwSolar"=sum(`PERMIT JOB VALUE`*
                                                   exp((year-y)*discount)*solar,
                                                 na.rm=T),
                           #sum of discounted missing
                           "NpermitMissing"=sum(is.na(`PERMIT JOB VALUE`)*
                                                  exp((year-y)*discount)),
                           "missingRoofVal"=(sum(is.na(`PERMIT JOB VALUE`)*roof)>0)*
                             exp((year-y)*discount),
                           "missingRemodelVal"=(sum(is.na(`PERMIT JOB VALUE`)*remodel)>0)*
                             exp((year-y)*discount),
                           "missingKitchenVal"=(sum(is.na(`PERMIT JOB VALUE`)*kitchen)>0)*
                             exp((year-y)*discount),
                           "missingBathroomVal"=(sum(is.na(`PERMIT JOB VALUE`)*bath)>0)*
                             exp((year-y)*discount)),
                 by=c("ONLINE FORMATTED PARCEL ID")]
  sumdPermits <- rbindlist(list(sumdPermits,temp))
  message(y)
  rm(temp)
}


permit$missing[is.na(permit$missing)] <- 0
sumdPermitsMissing <- data.table()

for(y in 2009:2022){
  
  temp <- permit[year<y & year>(y-5),.("year"=y,
                                        #sum of discounted present value for NON SOLAR
                                        "hasMissing"=(sum(missing)>0)*1),
                 by=c("ONLINE FORMATTED PARCEL ID")]
  sumdPermitsMissing <- rbindlist(list(sumdPermitsMissing,temp))
  message(y)
  rm(temp)
}


sumdPermits2 <- data.table()

for(y in 2009:2022){
  
  temp <- permit[year<y & year>(y-5),.("year"=y,
                           #sum of discounted present value for NON SOLAR
                           "Npermit"=sum(one*(solar==0)*(storage==0)),
                           "batteryStoragefromPermit"=sum(storage),
                           "newRoof"=(sum(roof)>0)*1,
                           "remodel"=(sum(remodel)>0)*1,
                           "newKitchen"=(sum(kitchen)>0)*1,
                           "newBathroom"=(sum(bath)>0)*1),
                 by=c("ONLINE FORMATTED PARCEL ID")]
  sumdPermits2 <- rbindlist(list(sumdPermits2,temp))
  message(y)
  rm(temp)
}

sumdPermits <- merge(sumdPermits,sumdPermits2,by=c("ONLINE FORMATTED PARCEL ID","year"),all=T)
sumdPermits[is.na(permitPresentVal),"permitPresentVal"] <- 0
sumdPermits[is.na(newRoof),"newRoof"] <- 0
sumdPermits[is.na(remodel),"remodel"] <- 0
sumdPermits[is.na(newKitchen),"newKitchen"] <- 0
sumdPermits[is.na(newBathroom),"newBathroom"] <- 0

# Save ------------------------------------------------------------------------

fwrite(sumdPermits,"../3_output_data/Permits.csv");beep(2)


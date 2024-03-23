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
packages <- c("rstudioapi","tidyverse","data.table","haven","sas7bdat","readxl",
              "beepr")

lapply(packages, pkgTest)

# the following line is for getting the path of the current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one
setwd(dirname(current_path ))
# make sure it is the right directory
print( getwd() )

# Properties -------------------------------------------------------------

#CT
ctTax <- fread("../3_output_data/ctTax2.csv")
ct <- fread("../3_output_data/ct.csv")

#fill in sale date if missing
ct$SALE_DATE[is.na(ct$SALE_DATE)] <-  ct$RECORDING_DATE[is.na(ct$SALE_DATE)]
ct$SALE_DATE[is.na(ct$SALE_DATE)] <-  ct$MORTGAGE_DATE[is.na(ct$SALE_DATE)]
ct$year <- substr(ct$SALE_DATE,1,4) %>%as.numeric

ct$ID <- paste(ct$APN_SEQUENCE_NUMBER,ct$PCL_ID_IRIS_FRMTD,ct$FIPS)
temp <- ct$ID %>%table %>%as.data.table
ct <- merge(ct,temp,by.x="ID",by.y=".")
ct$N %>% summary
#ct <- ct[ct$N%in%c(2:10),] #number of sales between 2 and 10.

ct$street <- paste(ct$SITUS_HOUSE_NUMBER_PREFIX,
                   ct$SITUS_HOUSE_NUMBER,
                   ct$SITUS_HOUSE_NUMBER_SUFFIX,
                   ct$SITUS_STREET_NAME,
                   ct$SITUS_MODE,
                   ct$SITUS_STREET_DIRECTION)

#MA
maTax <- fread("../3_output_data/maTax2.csv")
ma <- fread("../3_output_data/ma.csv")

#fill in sale date
ma$SALE_DATE[is.na(ma$SALE_DATE)] <-  ma$RECORDING_DATE[is.na(ma$SALE_DATE)]
ma$SALE_DATE[is.na(ma$SALE_DATE)] <-  ma$MORTGAGE_DATE[is.na(ma$SALE_DATE)]
ma$year <- substr(ma$SALE_DATE,1,4) %>%as.numeric

ma$ID <- paste(ma$APN_SEQUENCE_NUMBER,ma$PCL_ID_IRIS_FRMTD,ma$FIPS)
temp <- ma$ID %>%table %>%as.data.table
ma <- merge(ma,temp,by.x="ID",by.y=".")
#ma <- ma[ma$N%in%c(2:10),] #only properties sold between 2 and 10 times

ma$street <- paste(ma$SITUS_HOUSE_NUMBER_PREFIX,
                   ma$SITUS_HOUSE_NUMBER,
                   ma$SITUS_HOUSE_NUMBER_SUFFIX,
                   ma$SITUS_STREET_NAME,
                   ma$SITUS_MODE,
                   ma$SITUS_STREET_DIRECTION)

## Combine
ct$APN__Parcel_Number___unformatted <- ct$APN__Parcel_Number___unformatted %>%as.numeric
both <- rbind(ct,ma)
rm(ma,ct)
gc()

# Clean addresses -------------------------
stsuf <- fread("../1_input_data/address_concordance.csv")

cleanStreets <- function(vec){

    vec <- toupper(vec)
  
  vec <- gsub("^N | N$| N "," NORTH ",vec, perl = T)
  vec <- gsub("^S | S$| S "," SOUTH ",vec, perl = T)
  vec <- gsub("^E | E$| E "," EAST ",vec, perl = T)
  vec <- gsub("^W | W$| W "," WEST ",vec, perl = T)
  
  vec <- toupper(vec)
  vec <- gsub("^0","",vec, perl = T)
  vec <- gsub("^0","",vec, perl = T)
  vec <- gsub("/","",vec, perl = T)
  
  # common mistakes
  vec <- gsub(" PK"," PARK",vec, perl = T)
  vec <- gsub(" AVEN"," AVE",vec, perl = T)
  vec <- gsub(" AVENU"," AVE",vec, perl = T)
  vec <- gsub(" AVNUE"," AVE",vec, perl = T)
  
  vec <- gsub(" CICLE"," CIR",vec, perl = T)
  vec <- gsub(" CIRL"," CIR",vec, perl = T)
  vec <- gsub(" CIRLCE"," CIR",vec, perl = T)
  vec <- gsub(" CIRLE"," CIR",vec, perl = T)
  
  vec <- gsub("^ ","",vec, perl = T)
  vec <- gsub("  "," ",vec, perl = T)
  
  vec_split <- tstrsplit(vec, " ")
  vec_combined <- data.table()
  for (i in 1:length(vec_split)){
    # i <- 3
    vec_col <- as.data.table(vec_split[i])
    vec_col_replaced <- vec_col[stsuf, on = c("V1" = "abv"), V1 := i.full]
    
    vec_combined <- data.table(vec_combined, vec_col_replaced)
  }
  vec <- vec_combined %>%
    unite("replaced", sep = " ", na.rm = T)
  
  vec <- vec$replaced
  
  return(vec)
}

both$street <- gsub("NA |NA","",both$street)
both$street <- cleanStreets(both$street)
both$street <- toupper(both$street)
both$street <- gsub("  |   | ^","",both$street)

both <- both[order(year)]
temp <- both$ID %>% as.factor %>% levels
temp <- data.table("ID"=temp,"ID_n"=1:length(temp))
both <- merge(both,temp, by="ID")
both <- both[!is.na(year)]
both <- both[order(ID_n,year)]
both$own_length[2:nrow(both)] <- both$year[2:nrow(both)] - both$year[1:(nrow(both)-1)]
both$own_length[!duplicated(both$ID_n)] <- NA

fwrite(both[N %in% 2:10],"../3_output_data/ct_ma_rep_sales.csv")
fwrite(both[N <= 10 & year>=2010],"../3_output_data/ct_ma_2010_to_2019_sales.csv")
rm(both)

# Solar ----------------------

solarMA <- fread("../1_input_data/MA_Solar_20220615_sm.csv")
solarMA$one <- 1

solarCT <- read_excel("../1_input_data/2022-05-01 SQL RSIP KPI Data.xlsx") %>% as.data.table
solarCT$one <- 1


# Add TPO from TTS to MA

tts <- fread("../1_input_data/TTS_MA_CT_public.csv")
tts$year <- tts$installation_date %>% substr(.,8,11) %>% as.numeric

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ttsMA <- tts[state=="MA"]
ttsMA$ID <- ttsMA$system_ID_1 %>% substrRight(.,5)
solarMA$ID <- solarMA$program1projectid %>% substrRight(.,5)
solarMA$year <- solarMA$year %>% as.numeric
solarMA <- merge(solarMA,
      ttsMA[,c("zip_code","ID","year","third_party_owned")], by=c("ID","year"))
(solarMA$zip_code-solarMA$hostcustomerzip__4_) %>% sum #check match

solar <- solarCT[grepl("sunrun",solarCT$Installer,ignore.case = T)]
solar <- solar[`Lease $/Mo`>0]


# reduce projects and rename --------------------------------------------

## CT -----
solarCT <- solarCT[Duplicate=="No"]
solarCT$year <- substr(solarCT$`Completed Date`,1,4)%>%as.numeric
solarCT[is.na(`PBI Rate 1`),"PBI Rate 1"] <- 0
solarCT$`Batteries/Storage Cost`[is.na(solarCT$`Batteries/Storage Cost`)] <- 0

solarCT$storage <- solarCT$Battery %>%as.numeric

solarCT$TPO <- (!(solarCT$`System Owner` == "Homeowner Owned"))*1
solarCT$TPO[is.na(solarCT$TPO)] <- 0
solarCT$PPA <- (solarCT$`PPA Lease Term`>=10| solarCT$`PPA $/kWh`>0)*1
solarCT$PPA[is.na(solarCT$PPA)] <- 0
solarCT$lease <- (solarCT$`Lease $/Mo`>0)*1
solarCT$lease[is.na(solarCT$lease)] <- 0

solarCT$state <- "CT"

### collapse CT solar ----
solarCT <- 
  solarCT[,.(watts=sum(Watts),
             year=min(year),
             cost=sum(`Total System Cost`-`Batteries/Storage Cost`),
             pbi=max(`PBI Rate 1`),
             rebate=sum(`Incentive Amount`),
             kwhYr=sum(`Expected Annual Generation (kWh)`),
             lease=sum(lease),
             PPA=sum(PPA),
             TPO=sum(TPO),
             storage=max(storage),
             ProjectNumber=first(ProjectNumber)),
          by=c("Host Customer First",
               "Host Customer Last",
               "Host Customer Line 1",
               "Host Customer Line 2",
               "Host Customer City",
               "Host Customer Zip Code",
               "state")]

solarCT <- solarCT[,.(`Host Customer First`,`Host Customer Last`,
           hostcustomerstreetaddress=`Host Customer Line 1`,
           hostcustomercity=`Host Customer City`,
           hostcustomerzip__4_=`Host Customer Zip Code`,
           `state`, `watts`,`year`,`cost`,`pbi`,`rebate`,`kwhYr`,`lease`,
           `PPA`,`TPO`,
           `storage`,`ProjectNumber`)]

## MA ----

solarMA <- solarMA[!duplicated(solarMA)]
solarMA$storage <- solarMA$battery_y_n_ %>%as.numeric
solarMA$storage[is.nan(solarMA$storage)] <- 0

### Collapse MA solar ----

solarMA[order(year),.(mean(third_party_owned==1),
           mean(third_party_owned==-9999)),
        by="year"]

solarMA$TPO <- (solarMA$third_party_owned==1)*1

solarMA <- 
  solarMA[,.(watts=sum(systemsizeindcstc_kw_*1000),
             year=min(year),
             cost=sum(totalinstalledcost___),
             storage=max(storage),
             ProjectNumber=first(program1projectid),
             TPO=mean(TPO)),
          by=c(
               "hostcustomerstreetaddress",
               "hostcustomercity",
               "state",
               "hostcustomerzip__4_")]

solar <- rbind(solarCT,solarMA,fill=T)
rm(solarCT,solarMA)

# Clean addresses -------------------------------

solar$hostcustomerstreetaddress <- gsub("\\.","",solar$hostcustomerstreetaddress)
solar$hostcustomerstreetaddress <- cleanStreets(solar$hostcustomerstreetaddress)
solar$hostcustomerstreetaddress <- gsub("  |   | ^","",
                                        solar$hostcustomerstreetaddress)
solar$hostcustomerstreetaddress <- toupper(solar$hostcustomerstreetaddress)

fwrite(solar,"../3_output_data/solar.csv")



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
packages <- c("rstudioapi","tidyverse","data.table","fixest","lmtest","plm",
              "modelsummary","car","hexbin","knitr","kableExtra","readxl",
              "datawizard")

lapply(packages, pkgTest)

# the following line is for getting the path of the current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one
setwd(dirname(current_path ))
# make sure it is the right directory
print( getwd() )

# Loop over two input files ----------------------------------------------------

files <- c("../3_output_data/merged_data.csv",
           "../3_output_data/merged_2010_2019_data.csv")

for(f in files){
  dat <- fread(f)
  solar <- fread("../3_output_data/solar.csv")
  uscs <- fread("../1_input_data/USCS/nhgis0009_ds239_20185_zcta.csv")
  zip_pol <- fread("../1_input_data/aip_zips_ideology_v2022a.tab",sep="\t")
  zip_pol <- zip_pol[grep(" MA| CT",zip_name),]
  zip_pol <- zip_pol[,c("survey_period","population_2020","mrp_ideology_se",
                        "demshare_pres","mrp_ideology","presidential_year","zip")]
  eia <- fread("../1_input_data/eia_elec_price.csv")
  
  # Main data prep  --------------------------------------------------------
  
  ## Misc. ----
  dat$year <- dat$year.dat
  dat$year.dat <- NULL
  dat <- dat[!is.na(year)]
  
  ### Third party owned ----
  dat$TPO <- (dat$TPO>0)*1
  dat$TPO[is.na(dat$TPO)] <- 0
  
  ## Present value ----
  
  ### Electricty prices ----
  eia <- rbindlist(list(eia,data.table("year"=2023:2040, "state"="CT")),fill=T)
  eia <- rbindlist(list(eia,data.table("year"=2023:2040, "state"="MA")),fill=T)
  
  # Linear model of future prices
  lm_elec_linear <- lm(cents_kwh ~ state + state:year ,eia)
  eia$predict_linear <- predict(lm_elec_linear,eia)
  lm_elec_log <- lm(log(cents_kwh) ~ state + state:year ,eia)
  eia$predict_log <- predict(lm_elec_log,eia) %>%exp
  dat <- merge(dat,eia,by=c("state","year"),all.x=T)
  dat$predict_log%>%summary
  dat$m_pbi %>% summary
  
  temp <- solar[,.(m_pbi=mean(pbi,na.rm=T)),by=c("state","year")]
  dat <- merge(dat,temp,by=c("state","year"),all.x=T)
  dat$m_pbi[dat$state=="MA"] <- .3
  dat$m_pbi[is.na(dat$m_pbi)] <- 0
  
  ### Present value with different discount rates ----
  PVfun <- function(r,escElec,escTPO,pbi,TPO){
    1.14*dat$watts*(((1-exp((dat$age.solar-26)*(r-escElec)))/(r-escElec))*((dat$cents_kwh/100) + pbi)-
                      ((1-exp((dat$age.solar-26)*(r-escTPO)))/(r-escTPO))*TPO)
  }
  
  dat$pv15  <- PVfun(r=.15,
                     escElec =0.034,
                     escTPO=0,
                     pbi=dat$m_pbi,
                     TPO=0)
  
  dat$pv10  <- PVfun(r=.1,
                     escElec =0.034,
                     escTPO=0,
                     pbi=dat$m_pbi,
                     TPO=0)
  
  dat$pv2  <- PVfun(r=.02,
                    escElec =0.034,
                    escTPO=0,
                    pbi=dat$m_pbi,
                    TPO=0)
  
  dat$pv5  <- PVfun(r=.05,
                    escElec =0.034,
                    escTPO=0,
                    pbi=dat$m_pbi,
                    TPO=0)
  
  dat$pv15_flat_elec <- PVfun(r=.15,
                              escElec =0,
                              escTPO=0,
                              pbi=dat$m_pbi,
                              TPO=0)
  
  dat$pv10_flat_elec <- PVfun(r=.1,
                              escElec =0,
                              escTPO=0,
                              pbi=dat$m_pbi,
                              TPO=0)
  
  dat$pv5_flat_elec <- PVfun(r=.05,
                             escElec =0,
                             escTPO=0,
                             pbi=dat$m_pbi,
                             TPO=0)
  
  ### Present value of third party owned systems ----
  
  solarCT <- read_excel("../1_input_data/2022-05-01 SQL RSIP KPI Data.xlsx") %>% as.data.table
  
  solarCT$`Lease $/Mo`
  solarCT$year <- solarCT$`Estimated Date of Installation` %>% substr(.,1,4) %>% as.numeric
  solarCT$`PPA Lease Term`
  solarCT_PPA <- solarCT[`PPA $/kWh`>0,c("PPA $/kWh","year","Escalation Rate",
                                         "PPA Lease Term","System Financing Type",
                                         "Watts")]
  dat <- as.data.table(dat)
  dat[state=="CT",.(cents_kw=mean(cents_kwh)),by="year"]
  
  solarCT_PPA$`Escalation Rate`[solarCT_PPA$`Escalation Rate`>1] <- 
    solarCT_PPA$`Escalation Rate`[solarCT_PPA$`Escalation Rate`>1]/100
  
  solarCT_PPA$`PPA $/kWh`[solarCT_PPA$`PPA $/kWh`>1] <- 
    solarCT_PPA$`PPA $/kWh`[solarCT_PPA$`PPA $/kWh`>1]/100
  
  solarCT_lease <- solarCT[`Lease $/Mo`>0,c("Lease $/Mo","year","Escalation Rate",
                                            "System Financing Type","Watts")]
  solarCT_lease$`Escalation Rate`[solarCT_lease$`Escalation Rate`>1] <- 
    solarCT_lease$`Escalation Rate`[solarCT_lease$`Escalation Rate`>1]/100
  
  solarCT_lease$USDperKWH <- solarCT_lease$`Lease $/Mo`/(1.14*solarCT_lease$Watts/12)
  
  solarCT_lease[order(year),
                .(mean_USDperKWH=median(USDperKWH),
                  sd_USDperKWH=sd(USDperKWH),
                  mean_esc=median(`Escalation Rate`)),
                by="year"]
  
  solarCT_PPA[order(year),
              .(mean_USDperKWH=median(`PPA $/kWh`),
                sd_USDperKWH=sd(`PPA $/kWh`,na.rm=T),
                mean_esc=median(`Escalation Rate`)),
              by="year"]
  
  solarCT$USDperKWH <- NA
  solarCT$Escalator <- NA
  solarCT$USDperKWH[solarCT$`PPA $/kWh`>0& !is.na(solarCT$`PPA $/kWh`)] <- 
    solarCT$`PPA $/kWh`[solarCT$`PPA $/kWh`>0 & !is.na(solarCT$`PPA $/kWh`)]
  solarCT$Escalator[solarCT$`PPA $/kWh`>0 & !is.na(solarCT$`PPA $/kWh`)] <- 
    solarCT$`Escalation Rate`[solarCT$`PPA $/kWh`>0 & !is.na(solarCT$`PPA $/kWh`)]
  
  solarCT$USDperKWH[solarCT$`Lease $/Mo`>0& !is.na(solarCT$`Lease $/Mo`)] <- 
    solarCT$`Lease $/Mo`[solarCT$`Lease $/Mo`>0& !is.na(solarCT$`Lease $/Mo`)]/
    (1.14*solarCT$Watts[solarCT$`Lease $/Mo`>0& !is.na(solarCT$`Lease $/Mo`)]/12)
  solarCT$Escalator[solarCT$`Lease $/Mo`>0& !is.na(solarCT$`Lease $/Mo`)] <- 
    solarCT$`Escalation Rate`[solarCT$`Lease $/Mo`>0& !is.na(solarCT$`Lease $/Mo`)]
  
  solarCT$USDperKWH[solarCT$USDperKWH>1 & !is.na(solarCT$USDperKWH)] <- 
    solarCT$USDperKWH[solarCT$USDperKWH>1& !is.na(solarCT$USDperKWH)]/100
  solarCT$Escalator[solarCT$Escalator>1 & !is.na(solarCT$Escalator)] <- 
    solarCT$Escalator[solarCT$Escalator>1& !is.na(solarCT$Escalator)]/100
  
  TPO_CT <- solarCT[order(year),.(median_TPO_pKWH=median(USDperKWH,na.rm=T),
                                  median_TPO_escalator=median(Escalator,na.rm=T)),
                    by="year"]
  
  TPO_CT <- TPO_CT[year %in% c(2013:2022)]
  dat <- merge(dat,TPO_CT,by.x="year.solar",by.y="year",all.x=T)
  
  
  dat$pv15[dat$TPO==1] <-  PVfun(r=.15,
                                 escElec =0.034,
                                 escTPO=dat$median_TPO_escalator,
                                 pbi=0,
                                 TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  
  dat$pv15_flat_elec[dat$TPO==1] <-  PVfun(r=.15,
                                           escElec =0,
                                           escTPO=dat$median_TPO_escalator,
                                           pbi=0,
                                           TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  dat$pv10[dat$TPO==1] <-  PVfun(r=.1,
                                 escElec =0.034,
                                 escTPO=dat$median_TPO_escalator,
                                 pbi=0,
                                 TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  
  dat$pv10_flat_elec[dat$TPO==1] <-  PVfun(r=.1,
                                           escElec =0,
                                           escTPO=dat$median_TPO_escalator,
                                           pbi=0,
                                           TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  
  dat$pv5[dat$TPO==1] <- PVfun(r=.05,
                               escElec =0.034,
                               escTPO=dat$median_TPO_escalator,
                               pbi=0,
                               TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  
  dat$pv2[dat$TPO==1] <-  PVfun(r=.02,
                                escElec =0.034,
                                escTPO=dat$median_TPO_escalator,
                                pbi=0,
                                TPO=dat$median_TPO_pKWH)[dat$TPO==1]
  
  ### PV cleanup ----
  dat$pv15[is.na(dat$pv15)] <- 0
  dat$pv10[is.na(dat$pv10)] <- 0
  dat$pv2[is.na(dat$pv2)] <- 0
  dat$pv5[is.na(dat$pv5)] <- 0
  dat$pv5_flat_elec[is.na(dat$pv5_flat_elec)] <- 0
  dat$pv15_flat_elec[is.na(dat$pv15_flat_elec)] <- 0
  dat$pv10_flat_elec[is.na(dat$pv10_flat_elec)] <- 0
  
  dat$pv15_tercile[dat$pv15>0] <- cut(dat$pv15[dat$pv15>0],
                                      quantile(dat$pv15[dat$pv15>0],
                                               probs=c(0,.33,.67,1),na.rm=T),
                                      include.lowest=TRUE,labels=FALSE)
  
  dat$pv10_tercile[is.na(dat$pv10_tercile)] <- 0
  dat$pv10_tercile <- dat$pv10_tercile%>%as.factor
  
  dat$pv10_tercile[dat$pv10>0] <- cut(dat$pv10[dat$pv10>0],
                                      quantile(dat$pv10[dat$pv10>0],
                                               probs=c(0,.33,.67,1),na.rm=T),
                                      include.lowest=TRUE,labels=FALSE)
  
  dat$pv10_tercile[is.na(dat$pv10_tercile)] <- 0
  dat$pv10_tercile <- dat$pv10_tercile%>%as.factor
  
  ## USCS ----
  
  uscs <- dplyr::rename(uscs,"med_inc"="AJZAE001","pop"="AJYPE001")
  uscs$frac_BA <- (uscs$AJYPE022 + uscs$AJYPE023 + uscs$AJYPE024 + uscs$AJYPE025)/uscs$pop
  
  dat <- merge(dat,uscs[,c("ZCTA5A","frac_BA","med_inc","pop")],
               by.x="zip",
               by.y="ZCTA5A",
               all.x=T)
  
  dat$one <- 1
  counts <- dat[,.(N=sum(hasSolar),frac=sum(hasSolar)/sum(one)),by=c("year","state")]
  dat$age5 <- ceiling(dat$age/5)
  dat$inc_tercile <- cut(dat$med_inc,quantile(dat$med_inc,probs=c(0,.33,.67,1),na.rm=T),
                         include.lowest=TRUE,labels=FALSE)
  
  dat$BA_tercile <- cut(dat$frac_BA,quantile(dat$frac_BA,probs=c(0,.33,.67,1),na.rm=T),
                        include.lowest=TRUE,labels=FALSE)
  
  counts_inc <- dat[year>=2000 & !is.na(inc_tercile),
                    .(N=sum(hasSolar),frac=sum(hasSolar)/sum(one)),
                    by=c("year","inc_tercile")]
  counts_dem <- dat[year>=2000 & !is.na(demshare_tercile),
                    .(N=sum(hasSolar),frac=sum(hasSolar)/sum(one)),
                    by=c("year","demshare_tercile")]
  
  
  solar$one <- 1
  temp <- solar[,.(Nsolar=sum(one)),by=c("hostcustomerzip__4_","year")]
  temp <- dplyr::rename(temp,"zip"="hostcustomerzip__4_")
  dat <- merge(dat,temp,by=c("zip","year"),all.x=T)
  dat$frac_solar <- dat$Nsolar/dat$pop
  dat$frac_solar[is.na(dat$frac_solar)] <- 0
  dat$Nsolar[is.na(dat$Nsolar)] <- 0
  
  dat$own_length%>%summary
  dat$own_length[dat$hasSolar==1]%>%summary
  
  dat <- dat[SALE_AMOUNT>50000 & year>1975]
  dat$cost[is.na(dat$cost)] <- 0
  dat$ID <- as.factor(dat$ID)
  dat$ZipYr <- as.factor(dat$ZipYr)
  dat$FIPSMonth <- paste(dat$FIPS,floor(dat$month/4))
  dat$FIPSMonth <- as.factor(dat$FIPSMonth)
  
  dat <- dat[cost<100000]
  dat <- dat[watts<15000]
  
  ## demshare -----
  zip_pol <- zip_pol[!duplicated(zip_pol[,c("zip","presidential_year","survey_period")])]
  zip_pol$demshare_tercile <- cut(zip_pol$demshare_pres,
                                  quantile(zip_pol$demshare_pres,probs=c(0,.33,.67,1),na.rm=T),
                                  include.lowest=TRUE,labels=FALSE)
  
  dat$demshare_tercile <- NULL
  dat$survey_period[dat$year%in%2004:2011] <- "2004-2011"
  dat$survey_period[dat$year%in%2012:2016] <- "2012-2016"
  dat$survey_period[dat$year%in%2017:2021] <- "2017-2021"
  dat <- merge(dat,zip_pol,
               by=c("zip","survey_period"),
               all.x=T)
  dat$demshare_tercile[is.na(dat$demshare_tercile)] <- "old"
  
  zip_pol_2016 <- zip_pol[presidential_year==2016]
  zip_pol_2016$demshare_pres_2016 <- zip_pol_2016$demshare_pres
  dat <- merge(dat, 
               zip_pol_2016[,c("zip","demshare_pres_2016")],
               by="zip",
               all.x=T)
  
  ## Replace cost ----
  
  solar$rebate[is.na(solar$rebate)] <- 0
  replace <- solar[year>2000,
                   .(cw=mean((cost-rebate)/watts,na.rm=T)),
                   by=c("year","state")]
  dat <- merge(dat,replace,
               by=c("year","state"),
               all.x=T)
  
  replace$state <- as.factor(replace$state)
  replace$state <- factor(replace$state, levels = rev(levels(replace$state)))
  
  dat$replaceCost <- dat$cw * dat$watts
  dat[is.na(replaceCost),"replaceCost"] <- 0
  
  # Battery --------------------------------------------------------------------
  
  dat$batteryStoragefromPermit <- (dat$batteryStoragefromPermit>0)*1
  dat[is.na(batteryStoragefromPermit),"batteryStoragefromPermit"] <- 0
  dat[storage==0,"storage"] <- dat[storage==0,"batteryStoragefromPermit"]
  
  # Permit vars ----------------------------------------------------------------
  
  dat$otherMissing <- ((dat$NpermitMissing -dat$missingRemodelVal - 
                          dat$missingKitchenVal - dat$missingBathroomVal - 
                          dat$missingRoofVal)>0)*1
  
  # Save repeat sales data -----------------------------------------------------
  
  if(f=="../3_output_data/merged_data.csv"){
    fwrite(dat,"../3_output_data/main_model_data.csv")
  }else{
    fwrite(dat,"../3_output_data/alt_model_data.csv")
  }
};library(beepr);beep(2)

# Fin  -------------------------------------------------------------------------

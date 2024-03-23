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
              "modelsummary","car","hexbin","knitr","kableExtra")

lapply(packages, pkgTest)

# the following line is for getting the path of the current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one
setwd(dirname(current_path ))
# make sure it is the right directory
print( getwd() )

# Load -------------------------------------------------------------------------

solar <- fread("../3_output_data/solar.csv")
dat <- fread("../3_output_data/main_model_data.csv")
uscs <- fread("../1_input_data/USCS/nhgis0009_ds239_20185_zcta.csv")
zip_pol <- fread("../1_input_data/aip_zips_ideology_v2022a.tab",sep="\t")
zip_pol <- zip_pol[grep(" MA| CT",zip_name),]
zip_pol <- zip_pol[,c("survey_period","population_2020","mrp_ideology_se",
                      "demshare_pres","mrp_ideology","presidential_year","zip")]

eia <- fread("../1_input_data/eia_elec_price.csv")

# Stats data prep -------------------------------------------------------------

dat$year <- dat$year.dat
dat$one <- 1
solar$one <- 1
dat$year <- dat$SALE_DATE %>% substr(1,4) %>% as.numeric()

counts <- dat[,.(N=sum(hasSolar),
                                 frac=sum(hasSolar)/sum(one)),
              by=c("year","state")]

counts_TPO <- dat[year>=2000 & hasSolar==1,
                  .(N=sum(TPO),frac=sum(TPO)/sum(one)),
                  by=c("year","state")]

counts_TPO_install_year <- solar[year>=2000,
                  .(N=sum(TPO),frac=sum(TPO)/sum(one)),
                  by=c("year","state")]

counts_TPO[order(year)]

counts_inc <- dat[year>=2000 & !is.na(inc_tercile),
                  .(N=sum(hasSolar),frac=sum(hasSolar)/sum(one)),
                  by=c("year","inc_tercile")]

counts_dem <- dat[year>=2000 & !is.na(demshare_tercile),
                  .(N=sum(hasSolar),frac=sum(hasSolar)/sum(one)),
                  by=c("year","demshare_tercile")]


# Summary stats table ----------------------------------------------------------

sum_stats_nonsolar <- data.table(
  " "=c(
    "sale price ($)",
    "ZIP med. income ($)",
    "ZIP D vote share (%)",
    "ZIP college share (%)"
    ),
  "mean"=c(
    mean(dat$SALE_AMOUNT[dat$hasSolar==0 &dat$year>2009],na.rm = T),
    mean(dat$med_inc[dat$hasSolar==0 & dat$year>2009],na.rm=T),
    mean(dat$demshare_pres.x[dat$hasSolar==0 & dat$year>2009] ,na.rm=T)*100,
    mean(dat$frac_BA[dat$hasSolar==0 & dat$year>2009],na.rm=T)*100
  ),
  "median"=c(
    median(dat$SALE_AMOUNT[dat$hasSolar==0 & dat$year>2009],na.rm = T),
    median(dat$med_inc[dat$hasSolar==0 & dat$year>2009],na.rm=T),
    median(dat$demshare_pres.x[dat$hasSolar==0],na.rm=T)*100,
    median(dat$frac_BA[dat$hasSolar==0 & dat$year>2009],na.rm=T)*100
  ),
  "first quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==0 & dat$year>2009],c(.25),na.rm = T),

    quantile(dat$med_inc[dat$hasSolar==0 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==0 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==0 & dat$year>2009],c(.25),na.rm = T)*100
  ),
  "third quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==0 &dat$year>2009],c(.75),na.rm = T),
    
    quantile(dat$med_inc[dat$hasSolar==0&dat$year>2009],c(.75),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==0&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==0&dat$year>2009],c(.75),na.rm = T)*100
  )
)

sum_stats_solar_NTPO <- data.table(
  " "=c(
    "sale price ($)",
    "ZIP med. income ($)",
    "ZIP D vote share (%)",
    "ZIP college share (%)",
    "solar installed cost",
    "solar replacement cost",
    "present value --14% discount rate ($)",
    "present value --5% discount rate ($)",
    "age of solar",
    "capacity (watts)"
  ),
  "mean"=c(
    mean(dat$SALE_AMOUNT[dat$hasSolar==1 &dat$year>2009],na.rm = T),
    mean(dat$med_inc[dat$hasSolar==1 & dat$year>2009],na.rm=T),
    mean(dat$demshare_pres.x[dat$hasSolar==1 & dat$year>2009] ,na.rm=T)*100,
    mean(dat$frac_BA[dat$hasSolar==1 & dat$year>2009],na.rm=T)*100,
    mean(dat$cost[dat$hasSolar==1 & dat$year>2009]),
    mean(dat$replaceCost[dat$hasSolar==1 & dat$year>2009]),
    mean(dat$pv14[dat$hasSolar==1 & dat$year>2009]),
    mean(dat$pv5[dat$hasSolar==1 & dat$year>2009]),
    mean(dat$age[dat$hasSolar==1 & dat$year>2009]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009])
  ),
  "median"=c(
    median(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$year>2009],na.rm = T),
    median(dat$med_inc[dat$hasSolar==1 & dat$year>2009],na.rm=T),
    median(dat$demshare_pres.x[dat$hasSolar==1],na.rm=T)*100,
    median(dat$frac_BA[dat$hasSolar==1 & dat$year>2009],na.rm=T)*100,
    median(dat$cost[dat$hasSolar==1 & dat$year>2009]),
    median(dat$replaceCost[dat$hasSolar==1 & dat$year>2009]),
    median(dat$pv14[dat$hasSolar==1 & dat$year>2009]),
    median(dat$pv5[dat$hasSolar==1 & dat$year>2009]),
    median(dat$age[dat$hasSolar==1 & dat$year>2009]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009])
  ),
  "first quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv14[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009],.25,na.rm=T)
  ),
  "third quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 &dat$year>2009],c(.75),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1&dat$year>2009],c(.75),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv14[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009],.75,na.rm=T)
  )
)

sum_stats_solar_NTPO <- data.table(
  " "=c(
    "sale price ($)",
    "ZIP med. income ($)",
    "ZIP D vote share (%)",
    "ZIP college share (%)",
    "solar installed cost",
    "solar replacement cost",
    "present value --15% discount rate ($)",
    "present value --5% discount rate ($)",
    "age of solar",
    "capacity (watts)"
  ),
  "mean"=c(
    mean(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==0 &dat$year>2009],na.rm = T),
    mean(dat$med_inc[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],na.rm=T),
    mean(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009] ,na.rm=T)*100,
    mean(dat$frac_BA[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],na.rm=T)*100,
    mean(dat$cost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    mean(dat$replaceCost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    mean(dat$pv15[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    mean(dat$pv5[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    mean(dat$age[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    mean(dat$watts[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009])
  ),
  "median"=c(
    median(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],na.rm = T),
    median(dat$med_inc[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],na.rm=T),
    median(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==0],na.rm=T)*100,
    median(dat$frac_BA[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],na.rm=T)*100,
    median(dat$cost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    median(dat$replaceCost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    median(dat$pv15[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    median(dat$pv5[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    median(dat$age[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009]),
    median(dat$watts[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009])
  ),
  "first quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv15[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.25,na.rm=T)
  ),
  "third quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==0 &dat$year>2009],c(.75),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1 & dat$TPO==0&dat$year>2009],c(.75),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==0&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1 & dat$TPO==0&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv15[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$TPO==0 & dat$year>2009],.75,na.rm=T)
  )
)

sum_stats_solar_TPO <- data.table(
  " "=c(
    "sale price ($)",
    "ZIP med. income ($)",
    "ZIP D vote share (%)",
    "ZIP college share (%)",
    "solar installed cost",
    "solar replacement cost",
    "present value --15% discount rate ($)",
    "present value --5% discount rate ($)",
    "age of solar",
    "capacity (watts)"
  ),
  "mean"=c(
    mean(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==1 &dat$year>2009],na.rm = T),
    mean(dat$med_inc[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],na.rm=T),
    mean(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009] ,na.rm=T)*100,
    mean(dat$frac_BA[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],na.rm=T)*100,
    mean(dat$cost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    mean(dat$replaceCost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    mean(dat$pv15[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    mean(dat$pv5[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    mean(dat$age[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    mean(dat$watts[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009])
  ),
  "median"=c(
    median(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],na.rm = T),
    median(dat$med_inc[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],na.rm=T),
    median(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==1],na.rm=T)*100,
    median(dat$frac_BA[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],na.rm=T)*100,
    median(dat$cost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    median(dat$replaceCost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    median(dat$pv15[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    median(dat$pv5[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    median(dat$age[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009]),
    median(dat$watts[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009])
  ),
  "first quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],c(.25),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],c(.25),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv15[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.25,na.rm=T)
  ),
  "third quartile"=c(
    quantile(dat$SALE_AMOUNT[dat$hasSolar==1 & dat$TPO==1 &dat$year>2009],c(.75),na.rm = T),
    quantile(dat$med_inc[dat$hasSolar==1 & dat$TPO==1&dat$year>2009],c(.75),na.rm = T),
    quantile(dat$demshare_pres.x[dat$hasSolar==1 & dat$TPO==1&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$frac_BA[dat$hasSolar==1 & dat$TPO==1&dat$year>2009],c(.75),na.rm = T)*100,
    quantile(dat$cost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$replaceCost[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv15[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$pv5[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$age[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$TPO==1 & dat$year>2009],.75,na.rm=T)
  )
)


sum_stats_cap_tercile <- data.table(
  " "=c(
    "cap. med. inc. tercile 1",
    "cap. med. inc. tercile 2",
    "cap. med. inc. tercile 3",
    "cap. D share inc. tercile 1",
    "cap. D share inc. tercile 2",
    "cap. D share inc. tercile 3",
    "cap. college share tercile 1",
    "cap. college share tercile 2",
    "cap. college share tercile 3"
  ),
  "mean"=c(
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==1 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==2 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==3 ]),
    
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==1 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==2 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==3 ]),
    
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==1 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==2 ]),
    mean(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==3 ])
  ),
  "median"=c(
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==1 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==2 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==3 ]),
    
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==1 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==2 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==3 ]),
    
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==1 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==2 ]),
    median(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==3 ])
  ),
  "first quartile"=c(
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==1 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==2 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==3 ],.25,na.rm=T),
    
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==1 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==2 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==3 ],.25,na.rm=T),
    
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==1 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==2 ],.25,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==3 ],.25,na.rm=T)
  ),
  "third quartile"=c(
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==1 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==2 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$inc_tercile==3 ],.75,na.rm=T),
    
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==1 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==2 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$demshare_tercile==3 ],.75,na.rm=T),
    
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==1 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==2 ],.75,na.rm=T),
    quantile(dat$watts[dat$hasSolar==1 & dat$year>2009  & dat$BA_tercile==3 ],.75,na.rm=T)
  )
)

kab_tab <- function(tab){
  kab <-  kbl(tab, "latex", 
              # caption="Summary statistics",
              booktabs=T, label="kabletable",
              digits=0,
              format.args = list(big.mark = ",",scientific = FALSE))
  
  kable_classic_2(kab, full_width=F, latex_options="hold_position")
}

kab_tab(sum_stats_nonsolar)
kab_tab(sum_stats_solar_NTPO)
kab_tab(sum_stats_solar_TPO)
kab_tab(sum_stats_cap_tercile)


# Summary plots  ---------------------------------------------------------------

temp <- dat[year>2009 & hasSolar==1,
    .(N=sum(one),
      demshare_pres_2016=mean(demshare_pres_2016),
      frac_BA=mean(frac_BA),
      med_inc=mean(med_inc)),by="zip"]

dat[year>2009,"demshare_pres_2016"] %>% quantile(.,c(.33,.67),na.rm=T)
dat[year>2009,"frac_BA"] %>% quantile(.,c(.33,.67),na.rm=T)
dat[year>2009,"med_inc"] %>% quantile(.,c(.33,.67),na.rm=T)

ggplot(temp) + 
  geom_vline(xintercept=70754,linetype=2,alpha=.5) +
  geom_vline(xintercept=97853,linetype=2,alpha=.5) +
  geom_hline(yintercept=54,linetype=2,alpha=.5) +
  geom_hline(yintercept=65,linetype=2,alpha=.5) +
  geom_point(aes(x=med_inc,y=demshare_pres_2016*100,size=N),alpha=.3, color="steelblue") +
  labs(size="N solar",x="median income",y="2016 pres. Dem share (%)") +
         theme_minimal() + theme(legend.position = "none") 

ggsave("../5_analysis/fig_inc_D_corr.pdf",width=4,height = 5,units="in")

ggplot(temp) + 
  geom_vline(xintercept=70754,linetype=2,alpha=.5) +
  geom_vline(xintercept=97853,linetype=2,alpha=.5) +
  geom_hline(yintercept=33,linetype=2,alpha=.5) +
  geom_hline(yintercept=51,linetype=2,alpha=.5) +
  geom_point(aes(x=med_inc,y=frac_BA*100,size=N),alpha=.2,color="tomato3") +
  labs(size="N solar",x="median income",y="% college")+
  theme_minimal()+ theme(legend.position = "none") 

ggsave("../5_analysis/fig_inc_BA_corr.pdf",width=4,height = 5,units="in")

ggplot(temp) + 
  geom_vline(xintercept=54,linetype=2,alpha=.5) +
  geom_vline(xintercept=65,linetype=2,alpha=.5) +
  geom_hline(yintercept=33,linetype=2,alpha=.5) +
  geom_hline(yintercept=51,linetype=2,alpha=.5) +
  geom_point(aes(x=demshare_pres_2016*100,y=frac_BA*100,size=N),alpha=.2) +
  labs(size="N solar",x="2016 pres. Dem share (%)",y="% college")+
  theme_minimal()+ theme(legend.position = "none") 

ggsave("../5_analysis/fig_BA_D_corr.pdf",width=4,height = 5,units="in")


##Replace cost ----

ggplot(replace) + geom_line(aes(x=year,y=cw,linetype=state)) +
  xlim(2010,2020) + 
  scale_x_continuous(limits=c(2010,2020),breaks=c(2010,2015,2020))+
  ylim(0,7) + ylab("mean net $/watt") +
  theme_minimal()

ggsave("../5_analysis/fig_cw.pdf",width=6,height = 3,units="in")

##EIA ----

ggplot(eia) + 
  # geom_line(aes(x=year,y=predict_linear,linetype=state),color="tomato") +
  geom_line(aes(x=year,y=predict_log,linetype=state),color="tomato") +
  geom_line(aes(x=year,y=cents_kwh,linetype=state)) +
  ylab("cents per KWh") + xlab("year") +
  # ylim(0,30) +
  theme_minimal()

ggsave("../5_analysis/fig_elec_price.pdf",width=8,height = 5,units="in")

## Sale price -------

counts <- counts[state%in%c("CT","MA")]

ggplot(counts)+geom_line(aes(x=year,y=N,linetype=state)) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("number") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_Nsolar.pdf",width = 5, height=5, 
       units="in")

ggplot(counts)+geom_line(aes(x=year,y=frac,linetype=state)) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("fraction") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_frac_solar.pdf",width = 5, height=5, 
       units="in")

## TPO -------

counts_TPO <- counts_TPO[state%in%c("CT","MA")]

ggplot(counts_TPO)+geom_line(aes(x=year,y=frac,linetype=state)) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("fraction") +
  ylim(0,1) +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_fracTPO.pdf",width = 5, height=5, 
       units="in")

ggplot(counts_TPO_install_year)+
  annotate('rect', ymin=0,ymax=1,xmin=2008, xmax=2010, alpha=.1, fill='black') +
  geom_line(aes(x=year,y=frac,linetype=state)) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("fraction") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_fracTPO_install_year.pdf",width = 5, height=5, 
       units="in")

##By income tercile ----
ggplot(counts_inc)+geom_line(aes(x=year,y=N,linetype=as.factor(inc_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("number") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_Nsolar_inc.pdf",width = 5, height=5, 
       units="in")

ggplot(counts_inc)+geom_line(aes(x=year,y=frac,linetype=as.factor(inc_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("fraction") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_frac_solar_inc.pdf",width = 5, height=5, 
       units="in")

temp <- unique(dat[,c("zip","inc_tercile")])
temp <- merge(solar,temp,by.x="hostcustomerzip__4_",by.y="zip")
temp <- merge(temp,zip_pol,
              by.x=c("survey_period","hostcustomerzip__4_"),
              by.y=c("survey_period","zip"))
temp <- temp[,.(N=sum(one),frac=sum(one)/sum(population_2020/1000)),
             by=c("year","inc_tercile")]

temp <- temp[order(year)]
temp <- temp[!is.na(inc_tercile)]
temp$cumfrac[temp$inc_tercile==1] <- cumsum(temp$frac[temp$inc_tercile==1])
temp$cumfrac[temp$inc_tercile==2] <- cumsum(temp$frac[temp$inc_tercile==2])
temp$cumfrac[temp$inc_tercile==3] <- cumsum(temp$frac[temp$inc_tercile==3])

ggplot(temp)+geom_line(aes(x=year,y=cumfrac,linetype=as.factor(inc_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("cumulative PV per 1000 pop.") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_cumfrac_solar_inc.pdf",width = 5, height=5, 
       units="in")

##By dem share ----

temp <- merge(solar,zip_pol[survey_period=="2012-2016"],
              by.x=c("hostcustomerzip__4_"),
              by.y=c("zip"))

ggplot(counts_dem)+geom_line(aes(x=year,y=N,linetype=as.factor(demshare_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("number") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_Nsolar_dem.pdf",width = 5, height=5, 
       units="in")

ggplot(counts_dem)+geom_line(aes(x=year,y=frac,linetype=as.factor(demshare_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("fraction") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_frac_solar_dem.pdf",width = 5, height=5, 
       units="in")

temp$one <- 1
temp <- temp[,.(N=sum(one),frac=sum(one)/sum(population_2020/1000)),
     by=c("year","demshare_tercile")]

ggplot(temp[!is.na(demshare_tercile)])+geom_line(aes(x=year,y=N,linetype=as.factor(demshare_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("number") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_Nsolar_instal_demshare.pdf",width = 5, height=5, 
       units="in")
temp <- temp[order(year)]
temp <- temp[!is.na(demshare_tercile)]
temp$cumfrac[temp$demshare_tercile==1] <- cumsum(temp$frac[temp$demshare_tercile==1])
temp$cumfrac[temp$demshare_tercile==2] <- cumsum(temp$frac[temp$demshare_tercile==2])
temp$cumfrac[temp$demshare_tercile==3] <- cumsum(temp$frac[temp$demshare_tercile==3])

ggplot(temp)+
  geom_line(aes(x=year,y=cumfrac,linetype=as.factor(demshare_tercile))) +
  xlim(2000,2020) +
  xlab("year") +
  ylab("cumulative PV per 1000 pop.") +
  guides(linetype=guide_legend(title=""))+
  theme_minimal()

ggsave("../5_analysis/fig_cumrate_instal_demshare.pdf",width = 5, height=5, 
       units="in")
#!!!need to add income and BAs
## else

lmZipYr <- feols(SALE_AMOUNT ~1| ZipYr, dat)
dat$ZipYrResid <- lmZipYr$residuals

give.n <- function(x){
  return(c(y = 0, label = length(x)))
}

ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=SALE_AMOUNT/1000)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  xlab("year") +
  ylab("$ (thousands)") +
  ylim(0,1000) +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_price.pdf",width = 5, height=5, 
       units="in")

give.n <- function(x){
  return(c(y = -500, label = length(x)))
}
ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=ZipYrResid/1000)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  xlab("year") +
  ylab("$ (thousands)") +
  ylim(-500,500) +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_resid.pdf",width = 5, height=5, 
       units="in")

give.n <- function(x){
  return(c(y = 0, label = length(x)))
}
ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=age)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  xlab("year") +
  ylab("age") +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_age.pdf",width = 5, height=5, 
       units="in")

ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=watts/1000)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red",fill="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  ylim(0,30) +
  ylab("kw capacity") +
  xlab("year") +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_cap.pdf",width = 5, height=5, 
       units="in")

ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=cost)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red",fill="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  ylim(0,100000) +
  ylab("orig. cost") +
  xlab("year") +
  theme_minimal()
ggsave("../5_analysis/fig_solar_sale_cost.pdf",width = 5, height=5, 
       units="in")

ggplot(dat[hasSolar==1],aes(x=as.factor(year),y=cost/watts)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red",fill="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue3") +
  ylab("$/watt capacity") +
  xlab("year") +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_cost_per_watt.pdf",width = 5, height=5, 
       units="in")

ggplot(dat[hasSolar==1 & state=="CT"]) + 
  geom_bar(aes(x=as.factor(year),group=TPO,fill=as.factor(TPO)))+
  scale_fill_manual(values = c("black", "steelblue"),
                    labels=c("own","TPO"), name=NULL)+
  xlab("year") +
  theme_minimal()

ggsave("../5_analysis/fig_solar_sale_TPO.pdf",width = 8, height=5, 
       units="in")

ggplot(solar[year>=2010 & !is.na(2010)],
       aes(x=as.factor(year),y=cost/watts)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", size=1, color="red",fill="red") +
  stat_summary(fun.data = give.n, geom = "text", size = 3,color="steelblue1") +
  ylim(0,20) + 
  ylab("$/watt capacity") +
  xlab("year") +
  theme_minimal()

ggsave("../5_analysis/fig_solar_cost_per_watt.pdf",width = 8, height=5, 
       units="in")


ggplot(dat[hasSolar==1 ]) + stat_bin_hex(aes(x=resid,y=pv2)) +
  xlim(-100000,100000) +ylim(0,40000)

# FIN --------------------------------------------------------------------------

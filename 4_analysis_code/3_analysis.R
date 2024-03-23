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
              "modelsummary","car","hexbin","knitr","kableExtra","datawizard",
              "beepr")

lapply(packages, pkgTest)

# the following line is for getting the path of the current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one
setwd(dirname(current_path ))
# make sure it is the right directory
print(getwd())

# Load main --------------------------------------------------------------------

dat <- fread("../3_output_data/main_model_data.csv")

# Correct class ----------------------------------------------------------------

dat$zip <- dat$zip %>% as.factor
dat$ZipYr <- dat$ZipYr %>% as.factor
dat$CoYr <- dat$CoYr%>% as.factor
dat$demshare_tercile <- dat$demshare_tercile %>% as.character

# Permit correlations ----------------------------------------------------------

## OLS ----

dat[is.na(permitPresentVal),"permitPresentVal"] <- 0
dat[is.na(permitPresentVal),"permitPresentVal"] <- 0
dat[is.na(newRoof),"newRoof"] <- 0
dat[is.na(remodel),"remodel"] <- 0
dat[is.na(newKitchen),"newKitchen"] <- 0
dat[is.na(newBathroom),"newBathroom"] <- 0
dat$cost2 <- dat$cost*exp(-dat$age.solar*.1)

dat$permitPresentVal <-  dat$permitPresentVal - 
  dat$permitPresentValwSolar + 
  sapply(dat$permitPresentValwSolar-dat$cost2,function(x) max(x,0)) 
dat[permitPresentVal>100000,"permitPresentVal"] <- 0
dat$permitPresentVal %>% summary

lmPermitVal <- lm(permitPresentVal~ hasSolar, dat)
lmnewRoof <- lm(newRoof ~ hasSolar , dat)
lmremodel <- lm(remodel~hasSolar, dat)
lmNewKitchen <- lm(newKitchen~hasSolar, dat)
lmNewBathroom <- lm(newBathroom~hasSolar, dat)
lmNpermits <- lm(Npermit~hasSolar,dat)
summary(lmPermitVal)

feols(permitPresentVal ~ 
        LIVING_SQUARE_FEET*(hasSolar + missingBathroomVal + missingRoofVal + missingKitchenVal +
                              missingRemodelVal + otherMissing ) + 
        missingBathroomVal + missingRoofVal + missingKitchenVal +
        missingRemodelVal + otherMissing |ZipYr, dat)

dat$permitAdjPrice <- dat$SALE_AMOUNT - dat$hasSolar*dat$LIVING_SQUARE_FEET*
  1.658458 + 1574.241055
  

## FE Regressions ----

fePermitVal <- feols(permitPresentVal~hasSolar|ZipYr,dat)
fenewRoof <- feols(newRoof~hasSolar|ZipYr,dat)
feremodel <- feols(remodel~hasSolar|ZipYr,dat)
feNewKitchen <- feols(newKitchen~hasSolar|ZipYr,dat)
feNewBathroom <- feols(newBathroom~hasSolar|ZipYr,dat)
feNpermits <- feols(Npermit~hasSolar|ZipYr,dat)
summary(fePermitVal)

## Tables ----
#FE
modelsummary( list("tot. permit val."=fePermitVal,
                   "N permits" = feNpermits,
                   "new roof"=fenewRoof,
                   "remodel"=feremodel,
                   "new kitchen"=feNewKitchen,
                   "new bathroom"=feNewBathroom),
              output="../5_analysis/tab_permit_FE.tex",
              gof_omit=c("R2|R2 Adj.|AIC|BIC|Log.Lik.|RMSE|Std.Errors"),
              stars=T,
              escape = F,
              fmt = fmt_decimal(3,3),
              coef_map=c("hasSolar"="solar")
)

#OLS
modelsummary( list("tot. permit val."=lmPermitVal,
                   "N permits" = lmNpermits,
                   "new roof"=lmnewRoof,
                   "remodel"=lmremodel,
                   "new kitchen"=lmNewKitchen,
                   "new bathroom"=lmNewBathroom),
              output="../5_analysis/tab_permit_lm.tex",
              stars=T,
              escape = F,
              gof_omit=c("R2|R2 Adj.|AIC|BIC|Log.Lik.|RMSE"),
              fmt = fmt_decimal(3,3),
              coef_map=c("(Intercept)"="cons.",
                         "hasSolar"="solar")
)

rm(lmPermitVal,lmnewRoof,lmremodel,lmNewKitchen,lmNewBathroom,lmNpermits,
   fePermitVal,fenewRoof,feremodel,feNewKitchen,feNewBathroom,feNpermits)

# MLS regressions --------------------------------------------------------------

## Load -----
mls <- fread("../3_output_data/mls_solar.csv")

## Data prep ----

mls$wellMaint[is.na(mls$wellMaint)] <- 0
mls$zip <- mls$AddressPostalCode %>%as.factor
mls$zipyr <- paste0(mls$AddressPostalCode,mls$year.x)
mls <- mls[year.x>1990]
mls$year.x%>%summary
mls$co <- mls$CMAS_FIPS_CODE %>% as.factor

## OLS ----

lmnewRoof <- lm(newRoof~hasSolar,mls)
lmnewElec <- lm(newElec~hasSolar,mls)
lmheatPump <- lm(heatPump~hasSolar,mls)
lmgeothermal <- lm(geothermal~hasSolar,mls)
lmremodel <- lm(remodel~hasSolar,mls)
lmwellMaint <- lm(wellMaint~hasSolar,mls)
lmupdates <- lm(updates~hasSolar,mls)

## FE Regressions ----

FEnewRoof <- feols(newRoof~hasSolar| zip,mls)
FEnewRoofCo <- feols(newRoof~hasSolar| zip,mls)
FEnewElec <- feols(newElec~hasSolar| zip,mls)
FEheatPump <- feols(heatPump~hasSolar| zip,mls)
FEgeothermal <- feols(geothermal~hasSolar| zip, mls)
FEremodel <- feols(remodel~hasSolar| zip,mls)
FEwellMaint <- feols(wellMaint~hasSolar| zip,mls)
FEupdates <- feols(updates~hasSolar| zip,mls)

## Tables ----

#FE
modelsummary( list("roof"=FEnewRoof,
                   "electric"=FEnewElec,
                   "heatpump"=FEheatPump,
                   "geothermal"=FEgeothermal,
                   "remodel"=FEremodel,
                   "well maint."=FEwellMaint,
                   "updates"=FEupdates),
              output="../5_analysis/tab_mls_FE.tex",
              gof_omit=c("R2|R2 Adj.|AIC|BIC|Log.Lik.|RMSE|Std.Errors"),
              stars=T,
              escape = F,
              fmt = fmt_decimal(3,3),
              coef_map=c("hasSolar"="solar")
              )

#OLS
modelsummary( list("roof"=lmnewRoof,
                   "electric"=lmnewElec,
                   "heatpump"=lmheatPump,
                   "geothermal"=lmgeothermal,
                   "remodel"=lmremodel,
                   "well maint."=lmwellMaint,
                   "updates"=lmupdates),
              output="../5_analysis/tab_mls_lm.tex",
              stars=T,
              escape = F,
              gof_omit=c("R2|R2 Adj.|AIC|BIC|Log.Lik.|RMSE"),
              fmt = fmt_decimal(3,3),
              coef_map=c("(Intercept)"="cons.",
                         "hasSolar"="solar")
)


# replication  -----------------------------------------------------------------

## Fixed effects ----
lmRep1 <- feols(log(SALE_AMOUNT) ~ hasSolar + hasSolar:TPO |
                 ZipYr + ID + FIPSMonth,
               dat)

lmRep2 <- feols(log(SALE_AMOUNT) ~ hasSolar + hasSolar:TPO|
                 ZipYr + ID + FIPSMonth+ CONDITION + permitPresentVal +
                  missingRoofVal + missingBathroomVal+ missingRemodelVal+
                  missingKitchenVal,
               dat)

lmRep3 <- feols(log(SALE_AMOUNT) ~ hasSolar + hasSolar:TPO + 
                   hasSolar:as.factor(demshare_tercile) + 
                  hasSolar:TPO:as.factor(demshare_tercile)|
                 ZipYr + ID + FIPSMonth + CONDITION + permitPresentVal +
                  missingRoofVal + missingBathroomVal+ missingRemodelVal+
                  missingKitchenVal,
               dat)

lmRep4 <- feols(log(SALE_AMOUNT) ~ 
                  hasSolar + hasSolar:TPO + 
                  hasSolar:as.factor(inc_tercile) + 
                  hasSolar:TPO:as.factor(inc_tercile) +
                  missingRoofVal + missingBathroomVal+ missingRemodelVal+
                  missingKitchenVal,
               dat)

lmRep5 <- feols(log(SALE_AMOUNT) ~ 
                  hasSolar + hasSolar:TPO + 
                  hasSolar:as.factor(BA_tercile) + 
                  hasSolar:TPO:as.factor(BA_tercile)|
                 ZipYr + ID + FIPSMonth+ CONDITION + permitPresentVal +
                  missingRoofVal + missingBathroomVal+ missingRemodelVal+
                  missingKitchenVal,
               dat)

# replication comparison to mean
#From model 1
mean_solar_val <- mean(log(dat$SALE_AMOUNT[dat$hasSolar==1])) %>% exp

mean_solar_val*coef(lmRep1)[1] #$27211 premium for home-owner owned
mean_solar_val*(coef(lmRep1)[1]+ coef(lmRep1)[2]) #13333 premium for TPO

mean_solar_val*(coef(lmRep2)[1]) #$33272 premium for home-owner owned
mean_solar_val*(coef(lmRep2)[1]+coef(lmRep2)[2]) #5527 premium for TPO


## Table ----

modelsummary( list("baseline"=lmRep1,
                   "property improvements"=lmRep2,
                   "income"=lmRep4,
                   "D share"=lmRep3,
                   "college"=lmRep5), 
              output="../5_analysis/tab_logs.tex",
              title="Log-linear replication \\label{tab:log}",
              stars=T,
              escape = F,
              fmt = fmt_decimal(3, 3),
              coef_map=c("hasSolar"="PVBenefits",
                         "hasSolar:TPO"="TPO $\\times$ solar",
                         "hasSolar:as.factor(inc_tercile)2"="solar $\\times$ tercile 2",
                         "hasSolar:as.factor(inc_tercile)3"="solar $\\times$ tercile 3",
                         "hasSolar:demshare_tercile2"="solar $\\times$ tercile 2",
                         "hasSolar:demshare_tercile3"="solar $\\times$ tercile 3",
                         "hasSolar:as.factor(BA_tercile)2"="solar $\\times$ tercile 2",
                         "hasSolar:as.factor(BA_tercile)3"="solar $\\times$ tercile 3",
                         "hasSolar:TPO:as.factor(inc_tercile)2"="TPO $\\times$ solar $\\times$ tercile 2",
                         "hasSolar:TPO:as.factor(inc_tercile)3"="TPO $\\times$ solar $\\times$ tercile 3",
                         "hasSolar:TPO:demshare_tercile2"="TPO $\\times$ solar $\\times$ tercile 2",
                         "hasSolar:TPO:demshare_tercile3"="TPO $\\times$ solar $\\times$ tercile 3",
                         "hasSolar:TPO:as.factor(BA_tercile)2"="TPO $\\times$ solar $\\times$ tercile 2",
                         "hasSolar:TPO:as.factor(BA_tercile)3"="TPO $\\times$ solar $\\times$ tercile 3"
              ),
              gof_map=tibble::tribble(
                ~raw,        ~clean,          ~fmt,
                "nobs",      "N",             0,
                "r.squared", "R$^2$", 2,
                "FE: ZipYr","zip $\\times$ year FE",NA,
                "FE: ID","house FE",NA,
                "FE: FIPSMonth","county $\\times$ month FE",NA),
              add_rows=data.frame("name"=c("condition","permit vars"),
                                  "c1"=c("",""),
                                  "c2"=c("X","X"),
                                  "c3"=c("X","X"),
                                  "c4"=c("X","X"),
                                  "c5"=c("X","X")),
              note="Note: The dependent variable is log home sale price. An 
              observation is a property transaction. Solar is the dummy variable 
              indicating solar at the time of sale. Tercile interactions show the 
              heterogeneous interactions for the column heading. Columns (2)-(5) use 
              permit and MLS controls. Standard errors clustered by ZIP-year. 
              Appraised condition is a categorical variable ranging from “poor” 
              to “excellent” determined by the bank loan appraiser. Properties 
              with missing appraised condition data are considered to be in their 
              own category. In column (4), the first and third terciles are 
              statistically different at the 5\\% level for non-TPO and TPO.")

rm(lmRep1,lmRep2,lmRep3,lmRep4,lmRep5)


# Solar present value regressions ----------------------------------------------

lmPV1 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO |
                ZipYr + ID + FIPSMonth,
               dat)

lmPV2 <- feols(SALE_AMOUNT ~  hasSolar:pv15 + hasSolar:pv15:TPO + 
                  CONDITION + permitPresentVal + storage + 
                 missingRoofVal + missingBathroomVal+ 
                                         missingRemodelVal+
                  missingKitchenVal + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV3 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                 hasSolar:pv15:factor(demshare_tercile) + 
                 hasSolar:pv15:TPO:factor(demshare_tercile)  + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV4 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                 hasSolar:pv15:factor(inc_tercile) + 
                 hasSolar:pv15:TPO:factor(inc_tercile)  +
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV5 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                 hasSolar:pv15:factor(BA_tercile) + 
                 hasSolar:pv15:TPO:factor(BA_tercile)  + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

## Table ----

save( lmPV1, lmPV2, lmPV3, lmPV4, lmPV5, file="lmPV.RData")
# load(file="lmPV.RData")

modelsummary( list("baseline"=lmPV1,
                   "prop. improvement controls"=lmPV2,
                   "income"=lmPV4,
                   "Dem. share"=lmPV3,
                   "college"=lmPV5),
              output="../5_analysis/tab_PV.tex",
              title="Solar present value linear regressions \\label{tab:pv}",
              stars=T,
              escape = F,
              fmt = fmt_decimal(2, 2),
              coef_map=c("hasSolar:pv15"="PVBenefits",
                         "hasSolar:pv15:TPO"="TPO $\\times$ PVBenefits",
                         "hasSolar:pv15:factor(inc_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(inc_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:factor(demshare_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(demshare_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:factor(BA_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(BA_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(inc_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(inc_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(demshare_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(demshare_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(BA_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(BA_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3"
              ),
              gof_map=tibble::tribble(
                ~raw,        ~clean,          ~fmt,
                "nobs",      "N",             0,
                "r.squared", "R$^2$", 2,
                "FE: ZipYr","zip $\\times$ year FE",NA,
                "FE: ID","house FE",NA,
                "FE: FIPSMonth","county $\\times$ month FE",NA),
              add_rows=data.frame("name"=c("condition","permit vars"),
                                  "c1"=c("",""),
                                  "c2"=c("X","X"),
                                  "c3"=c("X","X"),
                                  "c4"=c("X","X"),
                                  "c5"=c("X","X")),
              notes = "Note: The dependent variable is home sale price. An observation is a property trans-
action. PVBenefits is the net present value of the remaining future benefits of solar at
the time of transaction. We assume a 10\\% discount rate. Tercile interactions show the
heterogeneous estimate for the column heading. Columns (2)-(5) adjust the sale price
by the expected difference in roof value for homes with solar (see \\ref{ap:roof}).
In (5), the coefficient on the third tercile is statistically different than
the first tercile at the 5\\% level. Standard errors clustered by ZIP-year. Appraised con-
dition is a categorical variable ranging from “poor” to “excellent” determined by the
bank loan appraiser. Property permit controls include discounted present
value of reported permited job values and dummy variables for categories of missing
permit values.");beep(1)

# alt discount rates -----------------------------------------------------------------

lmPV1_2 <- feols(SALE_AMOUNT ~ hasSolar:factor(TPO):pv2 |
                   ZipYr + ID + FIPSMonth,
                 dat)

lmPV1_5 <- feols(SALE_AMOUNT ~ hasSolar:factor(TPO):pv5 |
                  ZipYr + ID + FIPSMonth,
                 dat)

lmPV1_10 <- feols(SALE_AMOUNT ~ hasSolar:factor(TPO):pv10 |
                   ZipYr + ID + FIPSMonth,
                 dat)

lmPV2_2 <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv2 + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat[(missing==0 & year>2009) | year<2010])
lmPV2_5 <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv5 + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat[(missing==0 & year>2009) | year<2010])
lmPV2_10 <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv10 + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat[(missing==0 & year>2009) | year<2010])
lmPV2_5_flat <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv5_flat_elec + 
                   CONDITION + permitPresentVal +
                   missingRoofVal + missingBathroomVal+ missingRemodelVal+
                   missingKitchenVal + storage + otherMissing|
                   ZipYr + ID + FIPSMonth,
                 dat[(missing==0 & year>2009) | year<2010])
lmPV2_10_flat <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv15_flat_elec + 
                         CONDITION + permitPresentVal +
                         missingRoofVal + missingBathroomVal+ missingRemodelVal+
                         missingKitchenVal + storage + otherMissing|
                         ZipYr + ID + FIPSMonth,
                       dat[(missing==0 & year>2009) | year<2010])
lmPV2_15_flat <- feols(SALE_AMOUNT ~  hasSolar:factor(TPO):pv15_flat_elec + 
                    CONDITION + permitPresentVal +
                    missingRoofVal + missingBathroomVal+ missingRemodelVal+
                    missingKitchenVal + storage + otherMissing|
                    ZipYr + ID + FIPSMonth,
                  dat[(missing==0 & year>2009) | year<2010])

# load("discount_rate_sensitivity.RData")
save(lmPV1_2,lmPV1_5,lmPV1,lmPV1_10,
     lmPV2_2,lmPV2_5,lmPV2,lmPV2_10,
     lmPV2_5_flat,lmPV2_10_flat,lmPV2_15_flat,
     file="discount_rate_sensitivity.RData")

datPV <- map_dfr(c(.95), function(x) {
  modelplot(list(lmPV1_2,lmPV1_5,lmPV1,lmPV1_10,
                 lmPV2_2,lmPV2_5,lmPV2,lmPV2_10,
                 lmPV2_5_flat,lmPV2_10_flat,lmPV2_15_flat), 
            conf_level = x, draw = FALSE) %>%
    mutate(.width = x)
})%>% as.data.table

datPV$r <- gsub('\\D+','', datPV$term) %>% substr(.,2,3) %>% as.numeric
datPVTPO <- datPV[grepl("factor\\(TPO\\)1",term)]
datPV <- datPV[grepl("factor\\(TPO\\)0",term)]
datPV$model <- c(rep(c("(1)","(2)"),4),rep("(3)",3))
datPVTPO$model <- c(rep(c("(1)","(2)"),4),rep("(3)",3))

ggplot(datPV[model!="(3)"], aes(
  x = r, y = estimate,
  color = model)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggdist::geom_pointinterval( aes(ymin = conf.low, ymax = conf.high),
                              # position = "dodge",
                              interval_size_range = c(.1, .8),
                              # width=.2,
                              position=position_dodge(1),
                              fatten_point = 2) +  theme_minimal() + 
  theme(legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks=c(2,5,10,15)) +
  scale_y_continuous(breaks=c(0:2)) +
  ylab("est.") + xlab("")  + 
  # ylim(0,2.5) +
  scale_color_manual(values = c("black","tomato2","darkgrey"),
                     labels = c("(1) baseline", 
                                "(2) renovation controls",
                                "(3) reno. controls + flat elec.")) +
  labs(color = "model:")
 
ggsave("../5_analysis/fig_coef_r.pdf",width = 5, height=3, 
       units="in")

ggplot(datPVTPO[model!="(3)"], aes(
  x = r, y = estimate,
  color = model)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  ggdist::geom_pointinterval( aes(ymin = conf.low, ymax = conf.high),
                              # position = "dodge",
                              interval_size_range = c(.1, .8),
                              # width=.2,
                              position=position_dodge(1),
                              fatten_point = 2) +  theme_minimal() + 
  theme(legend.position="bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks=c(2,5,10,15)) +
  scale_y_continuous(breaks=c(-2:5)) +
  ylab("est.") + xlab("")  + 
  # ylim(0,5) +
  scale_color_manual(values = c("black","tomato2","darkgrey"),
                     labels = c("(1) baseline", 
                                "(2) renovation controls",
                                "(3) reno. controls + flat elec.")) +
  labs(color = "model:")

ggsave("../5_analysis/fig_coef_r_TPO.pdf",width = 5, height=3, 
       units="in")


rm(lmPV1_2,lmPV1_5,lmPV1,lmPV1_10,
     lmPV2_2,lmPV2_5,lmPV2,lmPV2_10,
     lmPV2_5_flat,lmPV2_10_flat,lmPV2_15_flat)

# Solar present value w/o TPO ----------------------------------------------

lmPV1_NoTPO <- feols(SALE_AMOUNT ~ hasSolar:pv15|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV2_NoTPO <- feols(SALE_AMOUNT ~  hasSolar:pv15 + 
                 CONDITION + permitPresentVal + storage + 
                 missingRoofVal + missingBathroomVal+ 
                 missingRemodelVal+
                 missingKitchenVal + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV3_NoTPO <- feols(SALE_AMOUNT ~ hasSolar:pv15 + 
                 hasSolar:pv15:factor(demshare_tercile) + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV4_NoTPO <- feols(SALE_AMOUNT ~ hasSolar:pv15 + 
                 hasSolar:pv15:factor(inc_tercile) + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

lmPV5_NoTPO <- feols(SALE_AMOUNT ~ hasSolar:pv15 + 
                 hasSolar:pv15:factor(BA_tercile) + 
                 hasSolar:pv15:TPO:factor(BA_tercile)  + 
                 CONDITION + permitPresentVal +
                 missingRoofVal + missingBathroomVal+ missingRemodelVal+
                 missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat)

## Table ----

save( lmPV1, lmPV2, lmPV3, lmPV4, lmPV5, file="lmPV.RData")
# load(file="lmPV.RData")

modelsummary( list("baseline"=lmPV1,
                   "prop. improvement controls"=lmPV2,
                   "income"=lmPV4,
                   "Dem. share"=lmPV3,
                   "college"=lmPV5),
              output="../5_analysis/tab_PV.tex",
              title="Solar present value linear regressions \\label{tab:pv}",
              stars=T,
              escape = F,
              fmt = fmt_decimal(2, 2),
              coef_map=c("hasSolar:pv15"="PVBenefits",
                         "hasSolar:pv15:TPO"="TPO $\\times$ PVBenefits",
                         "hasSolar:pv15:factor(inc_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(inc_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:factor(demshare_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(demshare_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:factor(BA_tercile)2"="PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:factor(BA_tercile)3"="PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(inc_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(inc_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(demshare_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(demshare_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3",
                         "hasSolar:pv15:TPO:factor(BA_tercile)2"="TPO $\\times$ PVBenefits $\\times$ tercile 2",
                         "hasSolar:pv15:TPO:factor(BA_tercile)3"="TPO $\\times$ PVBenefits $\\times$ tercile 3"
              ),
              gof_map=tibble::tribble(
                ~raw,        ~clean,          ~fmt,
                "nobs",      "N",             0,
                "r.squared", "R$^2$", 2,
                "FE: ZipYr","zip $\\times$ year FE",NA,
                "FE: ID","house FE",NA,
                "FE: FIPSMonth","county $\\times$ month FE",NA),
              add_rows=data.frame("name"=c("condition","permit vars"),
                                  "c1"=c("",""),
                                  "c2"=c("X","X"),
                                  "c3"=c("X","X"),
                                  "c4"=c("X","X"),
                                  "c5"=c("X","X")),
              notes = "Note: The dependent variable is home sale price. An observation is a property trans-
action. PVBenefits is the net present value of the remaining future benefits of solar at
the time of transaction. We assume a 10\\% discount rate. Tercile interactions show the
heterogeneous estimate for the column heading. Columns (2)-(5) adjust the sale price
by the expected difference in roof value for homes with solar (see \\ref{ap:roof}).
In (5), the coefficient on the third tercile is statistically different than
the first tercile at the 5\\% level. Standard errors clustered by ZIP-year. Appraised con-
dition is a categorical variable ranging from “poor” to “excellent” determined by the
bank loan appraiser. Property permit controls include discounted present
value of reported permited job values and dummy variables for categories of missing
permit values.");beep(1)


# Replace cost regressions -----------------------------------------------------
  
lmC1 <- feols(SALE_AMOUNT ~ replaceCost:hasSolar |
               ZipYr + ID + FIPSMonth,
             dat)

lmC2 <- feols(SALE_AMOUNT ~ replaceCost:hasSolar |
               ZipYr + ID + FIPSMonth+ CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal,
             dat)

lmC3 <- feols(SALE_AMOUNT ~ replaceCost:hasSolar +
               replaceCost:hasSolar:factor(demshare_tercile)|
               ZipYr + ID + FIPSMonth+ CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal,
             dat)



lmC4 <- feols(SALE_AMOUNT ~ replaceCost:hasSolar +
               replaceCost:hasSolar:factor(inc_tercile)|
               ZipYr + ID + FIPSMonth + CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal,
             dat)


lmC5 <- feols(SALE_AMOUNT ~ replaceCost:hasSolar+
                replaceCost:hasSolar:factor(BA_tercile)|
                ZipYr + ID + FIPSMonth+ CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal,
              dat)

## Table ----
modelsummary( list("baseline"=lmC1,
                   "property improvements"=lmC2,
                   "income"=lmC4,
                   "D share"=lmC3,
                   "college"=lmC5),
              output="../5_analysis/tab_replace_costs.tex",
              stars=T,
              escape = F,
              title="Solar replacement cost linear regressions \\label{tab:replace_cost}",
              fmt = fmt_decimal(2, 2),
              coef_map=c("replaceCost:hasSolar"="replace cost",
                         "replaceCost:hasSolar:TPO == 1TRUE"="TPO",
                         "storage"="storage",
                         "replaceCost:hasSolar:factor(inc_tercile)1"="replace cost $\\times$ tercile 1",
                         "replaceCost:hasSolar:factor(inc_tercile)2"="replace cost $\\times$ tercile 2",
                         "replaceCost:hasSolar:factor(inc_tercile)3"="replace cost $\\times$ tercile 3",
                         "replaceCost:hasSolar:factor(demshare_tercile)1"="replace cost $\\times$ tercile 1",
                         "replaceCost:hasSolar:factor(demshare_tercile)2"="replace cost $\\times$ tercile 2",
                         "replaceCost:hasSolar:factor(demshare_tercile)3"="replace cost $\\times$ tercile 3",
                         "replaceCost:hasSolar:factor(BA_tercile)1"="replace cost $\\times$ tercile 1",
                         "replaceCost:hasSolar:factor(BA_tercile)2"="replace cost $\\times$ tercile 2",
                         "replaceCost:hasSolar:factor(BA_tercile)3"="replace cost $\\times$ tercile 3"
              ),
              gof_map=tibble::tribble(
                ~raw,        ~clean,          ~fmt,
                "nobs",      "N",             0,
                "r.squared", "R$^2$", 2,
                "FE: ZipYr","zip $\\times$ year FE",NA,
                "FE: ID","house FE",NA,
                "FE: FIPSMonth","county $\\times$ month FE",NA),
              add_rows=data.frame("name"=c("appraised condition"),
                                  "c1"=c(""),
                                  "c2"=c("X"),
                                  "c3"=c("X"),
                                  "c4"=c("X"),
                                  "c5"=c("X")),
              notes = "Note: The dependent variable is home sale price. An observation is a property trans-
action. Replace cost is the computed average cost of replacement for a given size of
solar array in a given year. Tercile interactions show the heterogeneous estimate for
the column heading. Columns (2)-(5) adjust the sale price by the expected difference
in roof value for homes with solar (see \\ref{ap:roof}).In (3), the coefficient on the
third tercile is statistically different than the second tercile at the 10\\% level. In (4),
the coefficient on the third tercile is statistically different than the first tercile at the
5\\% level. In column (5), the coefficient on the third tercile is statistically different than
the second tercile at the 10\\% level and different than the first tercile at the 1\\% level.
Standard errors are clustered by ZIP-year.")


# Robustness checks ------------------------------------------------------------

repSale <- duplicated(dat$ID) %>% which %>% dat$ID[.] %>% unique
dat2 <- fread("../3_output_data/alt_model_data.csv")
dat2$repSale <- 0
dat2$repSale[dat2$ID %in% repSale] <- 1
dat2$repSale %>% sum

dat$repSale <- 0
dat$repSale[dat$ID %in% repSale] <- 1
dat$repSale %>% sum

lmR1 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                LIVING_SQUARE_FEET|
                ZipYr + FIPSMonth+ FIPSMonth,
              dat)

lmR2 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                LIVING_SQUARE_FEET|
                ZipYr + FIPSMonth + FIPSMonth,
              dat2)

dat$monthYr <- paste(dat$month,dat$year)

lmR3 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + monthYr,
               dat) #different fixed effects

lmR4 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                hasSolar:pv15:I((year>2014)) + hasSolar:pv15:TPO:I((year>2014)) +
                CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal + storage + otherMissing|
                ZipYr + ID + FIPSMonth,
              dat)

lmR5 <- feols(SALE_AMOUNT ~ hasSolar:pv15 + hasSolar:pv15:TPO + 
                hasSolar:pv15:I(year-2010) + hasSolar:pv15:TPO:I(year-2010) +
                CONDITION + permitPresentVal +
                missingRoofVal + missingBathroomVal+ missingRemodelVal+
                missingKitchenVal + storage + otherMissing|
                 ZipYr + ID + FIPSMonth,
               dat) #time trend in marginal effect


## Table ----

modelsummary( list("no home fe full sample"=lmR1,
                   "no home fe repeat only"=lmR2,
                   "month-yr fe"=lmR3,
                   "solar after 2010"=lmR4,
                   "time trend"=lmR5),
              output="../5_analysis/tab_robust.tex",
              stars=T,
              escape = F,
              booktabs =T,
              title="Robustness checks \\label{tab:robust}",
              fmt = fmt_decimal(2, 2),
              coef_map=c("hasSolar:pv15"="PVBenefits",
                         "hasSolar:pv15:TPO"="TPO $\\times$ PVBenefits",
                         "hasSolar:pv15:I((year > 2014))TRUE"="PVBenefits $\\times$ $\\geq$ 2015",
                         "hasSolar:pv15:TPO:I((year > 2014))TRUE"="TPO $\\times$ PVBenefits $\\times$ $\\geq$ 2015",
                         "hasSolar:pv15:I(year - 2010)"="PVBenefits $\\times$ t",
                         "hasSolar:pv15:TPO:I(year - 2010)"="TPO $\\times$ PVBenefits $\\times$ t"
              ),
              gof_map=tibble::tribble(
                ~raw,        ~clean,          ~fmt,
                "nobs",      "N",             0,
                "r.squared", "R$^2$", 2,
                "FE: ZipYr","zip $\\times$ year FE",NA,
                "FE: ID","house FE",NA,
                "FE: FIPSMonth","county $\\times$ month FE",NA,
                "FE: monthYr","month $\\times$ year FE",NA),

              notes = list(c("\tabnote[.9\\textwidth]{Note: The dependent variable 
                             for all models is the property transaction
              price in levels. Columns (1)-(2) test if there is slection
              bias arising from the repeat sales sample. Column (1) includes all
              sales after 2009 and no property fixed effects. Column (2) adds 
              renovation controls. Column (3) restricts the sample to property 
              sales with repeat sales but omits property
              fixed effects. Column (4) tests the effect of alternative fixed effects
              for repeat sales---replacing county $\\times$ month fixed effects with month $\\times$ year
              fixed effects. Column (5) removes homes with solar sold before 2011.
              Model (6) includes a time trend interaction.}")))


# Fin --------------------------------------------------------------------------
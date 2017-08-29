 library(plyr)
library(reshape2)
library(SmarterPoland)
library(xlsx)
library(devtools)
library(dplyr)
library(zoo)
library(gdata)
library(gridExtra)
library(pdfetch)
library(ecb)
library(lubridate)

setwd("/Users/tomokeeffe/Desktop/R/")
nasa_10_nf_tr<-getEurostatRaw("nasa_10_nf_tr")
nasa_10_f_tr<-getEurostatRaw("nasa_10_f_tr")
prc_hicp_aind <- getEurostatRaw("prc_hicp_aind")

nasq_10_nf_tr<-getEurostatRaw("nasq_10_nf_tr")
nasq_10_f_tr<-getEurostatRaw("nasq_10_f_tr")
prc_hicp_midx<- getEurostatRaw("prc_hicp_midx")

##################################################################################
#SAVING RAW DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################

#raw esa 95
save(nasa_10_nf_tr, file="data-eurostat/raw/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/raw/nasa_10_f_tr.RData")
save(prc_hicp_aind, file="data-eurostat/raw/prc_hicp_aind.RData")

save(nasq_10_nf_tr, file="data-eurostat/raw/nasq_10_nf_tr.RData")
save(nasq_10_f_tr, file="data-eurostat/raw/nasq_10_f_tr.RData")
save(prc_hicp_midx, file="data-eurostat/raw/prc_hicp_midx.RData")

##################################################################################
#transformation function
##################################################################################

#This transformation function converts the raw data to long format
#it assumes you've already loaded the data
trans<-function(df){
  data<-melt(df)
  colsplit<-colsplit(data[,1],",", as.vector(strsplit(names(data)[1],split=",")[[1]]))
  output<-data.frame(colsplit,data[2:length(data)])
  names<-names(output)
  names<-gsub("variable","time",names)
  names(output)<-names
  return(output)
}
##################################################################################
#TRANSFORMING DATA
##################################################################################
#now we transform the raw data into long format

#trans esa 95
nasa_10_nf_tr<-trans(nasa_10_nf_tr)
nasa_10_f_tr<-trans(nasa_10_f_tr)
prc_hicp_aind<-trans(prc_hicp_aind)
nasq_10_nf_tr<-trans(nasq_10_nf_tr)
nasq_10_f_tr<-trans(nasq_10_f_tr)
prc_hicp_midx<-trans(prc_hicp_midx)
##################################################################################
#SAVING LONG DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################
#long esa 95

#long esa 95
save(nasa_10_nf_tr, file="data-eurostat/long/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/long/nasa_10_f_tr.RData")
save(prc_hicp_aind, file="data-eurostat/long/prc_hicp_aind.RData")

save(nasq_10_nf_tr, file="data-eurostat/long/nasq_10_nf_tr.RData")
save(nasq_10_f_tr, file="data-eurostat/long/nasq_10_f_tr.RData")
save(prc_hicp_midx, file="data-eurostat/long/prc_hicp_midx.RData")


###################################################################################
#Filtering Data
###################################################################################

#Load from my wd() to save time
load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/nasa_10_f_tr.RData")
load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/nasa_10_nf_tr.RData")
load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/prc_hicp_aind.RData")

load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/nasq_10_f_tr.RData")
load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/nasq_10_nf_tr.RData")
load("/Users/tomokeeffe/Desktop/R/data-eurostat/long/prc_hicp_midx.RData")

nasa_10_f_tr$time <-  as.Date(nasa_10_f_tr$time, format("%Y"))
nasa_10_f_tr$time <- year(nasa_10_f_tr$time)

nasa_10_nf_tr$time <-  as.Date(nasa_10_nf_tr$time, format("%Y"))
nasa_10_nf_tr$time <- year(nasa_10_nf_tr$time)

prc_hicp_aind$time <-  as.Date(prc_hicp_aind$time, format("%Y"))
prc_hicp_aind$time <- year(prc_hicp_aind$time)

nasa_10_f_tr <- nasa_10_f_tr[order(as.Date(nasa_10_f_tr$time, format="%Y")),]
nasa_10_nf_tr <- nasa_10_nf_tr[order(as.Date(nasa_10_nf_tr$time, format="%Y")),]
prc_hicp_aind <- prc_hicp_aind[order(as.Date(prc_hicp_aind$time, format="%Y")),]

##############################################
#Annual
##############################################

{
  
  hh_nl_prv <- filter(nasa_10_f_tr, finpos == "LIAB", sector == "S14_S15", co_nco == "CO", unit == "MIO_EUR", na_item == "B9F")
  #NFC NET LENDING
  nfc_nl_prv<- filter(nasa_10_f_tr,unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S11", co_nco == "CO")
  #FC NET LENDING
  fc_nl_prv<- filter(nasa_10_f_tr, unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S12", co_nco == "CO")
  
  tmp2<-filter(nasa_10_nf_tr, unit=="CP_MEUR")
  tmp21<-filter(prc_hicp_aind, coicop=="CP00",unit=="INX_A_AVG")
  
  hh_nl_prv <- filter(hh_nl_prv,  finpos == "LIAB", sector == "S14_S15", co_nco == "CO", unit == "MIO_EUR", na_item == "B9F")
  #hh_nl_prv <- hh_nl_prv[order(as.Date(hh_nl_prv$time, format="%Y-%m-%d")),]
  
  #NFC NET LENDING
  nfc_nl_prv<- filter(nfc_nl_prv,unit == "MIO_EUR",na_item == "B9F",finpos == "LIAB", sector == "S11", co_nco == "CO")
  # nfc_nl_prv <- nfc_nl_prv[order(as.Date(nfc_nl_prv$time, format="%Y-%m-%d")),]
  
  #FC NET LENDING
  fc_nl_prv<- filter(fc_nl_prv,unit == "MIO_EUR",na_item == "B9F", finpos == "LIAB", sector == "S12", co_nco == "CO")
  #fc_nl_prv <- fc_nl_prv[order(as.Date(fc_nl_prv$time, format="%Y-%m-%d")),]
  
  gdp<-filter(tmp2,na_item=="B1GQ", direct=="PAID")
  Annual <- subset(gdp,select = c("geo.time","time", "value"))
  Annual <- plyr::rename(Annual, c("value" = "gdp"))
  
  #general govt balance
  gen_gov_balance<-filter(tmp2,na_item=="B9",direct=="PAID", sector=="S13")
  gen_gov_balance <- subset(gen_gov_balance,select = c("geo.time","time", "value") )
  Annual <- merge(Annual,gen_gov_balance, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "gen_gov_balance"))
  Annual$gen_gov_balance_pc<-as.numeric(as.character(Annual$gen_gov_balance))/as.numeric(as.character(Annual$gdp))*100
  
  Annual$gdp_d <- (Annual$gdp/lag(Annual$gdp, 1) -1)*100
  
  gov_exp<-filter(tmp2,na_item=="OTE", direct=="PAID", sector=="S13")
  gov_exp <- subset(gov_exp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,gov_exp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "gov_exp"))
  
  #government tax revenue
  gov_trev <- filter(tmp2,na_item == "OTR",sector == "S13", direct == "RECV")
  gov_trev <- subset(gov_trev,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,gov_trev, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "gov_trev"))    
  
  #theta - tax share
  Annual$theta<-as.numeric(as.character(Annual$gov_trev))/as.numeric(as.character(Annual$gdp))
  
  #Adjusted fiscal ratio
  Annual$AFR<-as.numeric(as.character(Annual$gov_exp))/as.numeric(as.character(Annual$theta))
  Annual$AFR_d <- (Annual$AFR/lag(Annual$AFR,1) -1)*100
  
  #current extrnal balance - BOP - ROW view # REVERSE IN PLOT
  bop<-filter(tmp2,na_item=="B12",direct=="PAID",sector=="S2")
  bop$value<-bop$value*-1   #ok it's reversed here and a pc of gdp value is done
  bop <- subset(bop,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,bop, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "bop"))
  Annual$bop_pc<-as.numeric(as.character(Annual$bop))/as.numeric(as.character(Annual$gdp))*100
  
  #exports in goods
  bop_gexp<-filter(tmp2,na_item=="P61",direct=="PAID",sector=="S2")
  bop_gexp <- subset(bop_gexp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,bop_gexp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "bop_gexp"))
  
  #imports in goods
  bop_gimp<-filter(tmp2,na_item=="P71",direct=="RECV",sector=="S2")
  bop_gimp <- subset(bop_gimp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,bop_gimp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "bop_gimp"))    
  
  #balance of trade in goods
  Annual$bop_goods<-as.numeric(as.character(Annual$bop_gexp))-as.numeric(as.character(Annual$bop_gimp))
  
  #balance of trade in goods (pc of gdp)
  Annual$bop_goods_pc<-as.numeric(as.character(Annual$bop_goods))/as.numeric(as.character(Annual$gdp))*100
  
  #exports in services
  bop_sexp<-filter(tmp2,na_item=="P62",direct=="PAID",sector=="S2")
  bop_sexp <- subset(bop_sexp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,bop_sexp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "bop_sexp")) 
  
  #imports in services
  bop_simp<-filter(tmp2,na_item=="P72",direct=="RECV",sector=="S2")
  bop_simp <- subset(bop_simp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,bop_simp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "bop_simp")) 
  
  #balance of trade in services
  Annual$bop_soods<-as.numeric(as.character(Annual$bop_sexp))-as.numeric(as.character(Annual$bop_simp))
  #balance of trade in services (pc of gdp)
  Annual$bop_soods_pc<-as.numeric(as.character(Annual$bop_soods))/as.numeric(as.character(Annual$gdp))*100
  
  #primary/secondary balances
  pb_bal1<-filter(tmp2, na_item %in% c("IN1","IN21"),direct=="PAID",sector=="S2")
  pb_bal1<-aggregate(pb_bal1$value, by=list(Category= pb_bal1$unit, pb_bal1$direct, pb_bal1$sector,pb_bal1$geo.time,pb_bal1$time), FUN=sum)
  pb_bal1 <- plyr::rename(pb_bal1, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  pb_bal1 <- subset(pb_bal1,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,pb_bal1, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "pb_bal1"))  
  
  pb_bal2<-filter(tmp2,na_item%in%c("IN1","IN21"),direct=="RECV",sector=="S2")
  pb_bal2<-aggregate(pb_bal2$value, by=list(Category= pb_bal2$unit, pb_bal2$direct, pb_bal2$sector,pb_bal2$geo.time,pb_bal2$time), FUN=sum)
  pb_bal2 <- plyr::rename(pb_bal2, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  pb_bal2 <- subset(pb_bal2,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,pb_bal2, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "pb_bal2"))
  
  Annual$pb_bal<-(as.numeric(as.character(Annual$pb_bal1)))-(as.numeric(as.character(Annual$pb_bal2)))
  Annual$pb_bal_pc<- (as.numeric(as.character(Annual$pb_bal))/as.numeric(as.character(Annual$gdp)))*100
  
  #Import propensity
  Annual$mu<-(Annual$bop_simp+Annual$bop_gimp+as.numeric(Annual$pb_bal2))/Annual$gdp
  
  #Adjusted Trade Ratio 
  Annual$ATR<-(Annual$bop_sexp+Annual$bop_gexp+as.numeric(Annual$pb_bal1))/Annual$mu
  
  Annual$ATR_d <- (Annual$ATR/lag(Annual$ATR,1) -1)*100
  
  #CFTR
  Annual$CFTR<-(Annual$bop_sexp+Annual$bop_gexp+Annual$pb_bal1+Annual$gov_exp)/(Annual$mu+Annual$theta)
  Annual$CFTR_d <- (Annual$CFTR/lag(Annual$CFTR,1) -1)*100
  
  #FINANCIAL BALANCES
  #Saving 
  S <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B9", direct == "PAID")
  S<-aggregate(S$value, by=list(Category= S$unit, S$direct, S$na_item,S$geo.time,S$time), FUN=sum)
  S <- plyr::rename(S, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  S <- subset(S,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,S, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "S"))     
  
  Annual$S_pc<-(Annual$S/Annual$gdp)*100
  
  #government Balance
  GDEF<-subset(gen_gov_balance, select = c( "geo.time","time", "value"))
  Annual$GDEF <- Annual$gen_gov_balance*-1
  Annual$GDEF_pc<-(Annual$GDEF/Annual$gdp)*100
  #Current Account
  CA<-filter(tmp2,na_item=="B9",direct=="PAID",sector=="S2")
  CA <- subset(CA,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,CA, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "CA"))  
  Annual$CA_pc<-(Annual$CA/Annual$gdp)*100
  
  hh_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S14_S15")
  hh_saving <- subset(hh_saving,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,hh_saving, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "hh_saving"))
  
  hh_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S14_S15")
  hh_inv <- subset(hh_inv,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,hh_inv, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "hh_inv"))
  
  Annual$hh_sav_m_inv<-as.numeric(Annual$hh_saving)-as.numeric(Annual$hh_inv)
  Annual$hh_sav_m_inv_pc<-(Annual$hh_sav_m_inv/Annual$gdp)*100
  
  nfc_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S11")
  nfc_saving <- subset(nfc_saving,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,nfc_saving, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "nfc_saving"))
  
  nfc_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S11")
  nfc_inv <- subset(nfc_inv,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,nfc_inv, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "nfc_inv"))
  
  Annual$nfc_sav_m_inv<-as.numeric(Annual$nfc_saving)-as.numeric(Annual$nfc_inv)
  Annual$nfc_sav_m_inv_pc<-(Annual$nfc_sav_m_inv/Annual$gdp)*100
  
  #Non_Fin Saving
  S_nonfin <- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B9", direct == "PAID")
  S_nonfin<-aggregate(S_nonfin$value, by=list(Category= S_nonfin$unit, S_nonfin$direct, S_nonfin$na_item,S_nonfin$geo.time,S_nonfin$time), FUN=sum)
  S_nonfin <- plyr::rename(S_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  S_nonfin <- subset(S_nonfin,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,S_nonfin, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "S_nonfin")) 
  
  Annual$S_pc2<-(Annual$S_nonfin/Annual$gdp)*100
  
  #real private disposable income
  disp_inc_nonfin<- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B6G", direct == "RECV")
  disp_inc_nonfin<-aggregate(disp_inc_nonfin$value, by=list(Category= disp_inc_nonfin$unit, disp_inc_nonfin$direct, disp_inc_nonfin$na_item,disp_inc_nonfin$geo.time,disp_inc_nonfin$time), FUN=sum)
  
  disp_inc_nonfin <- plyr::rename(disp_inc_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  disp_inc_nonfin <- subset(disp_inc_nonfin,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,disp_inc_nonfin, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "disp_inc_nonfin"))
  
  cpi <- subset(tmp21, select = c("geo.time","time","value"))
  Annual<- merge(Annual, cpi, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "cpi"))
  
  #real private disposable income
  disp_inc <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B6G", direct == "RECV")
  disp_inc<-aggregate(disp_inc$value, by=list(Category= disp_inc$unit, disp_inc$direct, disp_inc$na_item,disp_inc$geo.time,disp_inc$time), FUN=sum)
  disp_inc <- plyr::rename(disp_inc, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  disp_inc <- subset(disp_inc,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,disp_inc, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "disp_inc"))    
  
  Annual$disp_inc<-(Annual$disp_inc/Annual$cpi)*100
  Annual$disp_inc2_d <- (Annual$disp_inc/lag(Annual$disp_inc,1) -1)*100
  
  #real private expenditure
  prv_exp <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "P3", direct == "PAID")
  prv_exp<-aggregate(prv_exp$value, by=list(Category= prv_exp$unit, prv_exp$direct, prv_exp$na_item, prv_exp$geo.time, prv_exp$time), FUN=sum)
  prv_exp <- plyr::rename(prv_exp, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
  prv_exp <- subset(prv_exp,select = c("geo.time","time", "value"))
  Annual <- merge(Annual,prv_exp, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual,c("value" = "prv_exp"))     
  
  Annual$prv_exp<-(Annual$prv_exp/Annual$cpi)*100
  Annual$prv_exp_d <- (Annual$prv_exp/lag(Annual$prv_exp,1) -1)*100
  
  nl_prv <- subset(hh_nl_prv, select = c("geo.time","time", "value"))
  Annual<- merge(Annual, nl_prv, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "nl_prv"))
  
  nfc_nl_prv <- subset(nfc_nl_prv, select = c("geo.time","time", "value"))
  Annual<- merge(Annual, nfc_nl_prv, all = TRUE, by =  c("geo.time","time"))
  Annual <- plyr::rename(Annual, c("value" = "nfc_nl_prv"))
  
  Annual$nl_prv<- Annual$nl_prv +as.numeric(as.character(Annual$nfc_nl_prv)) 
  Annual$nl_prv_pc<-(as.numeric(Annual$nl_prv))/(Annual$disp_inc_nonfin)*-100
  
  
  Annual$S_pc_disp<-(Annual$S_nonfin/Annual$disp_inc_nonfin)*100
  
  Annual <- filter(Annual, !(geo.time %in% c("US", "EA","EA18","EEA","EU","HR","ME","RS")))
  Annual$geo.time<-as.character(Annual$geo.time)
  
  
  
  #Renaming
  Annual$geo.time[Annual$geo.time=="AT"] <- "Austria"
  Annual$geo.time[Annual$geo.time=="BE"] <- "Belgium"
  Annual$geo.time[Annual$geo.time=="BG"] <- "Bulgaria"
  Annual$geo.time[Annual$geo.time=="CH"] <- "Switzerland"
  Annual$geo.time[Annual$geo.time=="CY"] <- "Cyprus"
  Annual$geo.time[Annual$geo.time=="CZ"] <- "Czech Republic"
  Annual$geo.time[Annual$geo.time=="DE"] <- "Germany"
  Annual$geo.time[Annual$geo.time=="DK"] <- "Denmark"
  Annual$geo.time[Annual$geo.time=="EA19"] <- "Euro Area(19 Countries)"
  Annual$geo.time[Annual$geo.time=="EE"] <- "Estonia"
  Annual$geo.time[Annual$geo.time=="EL"] <- "Greece"
  Annual$geo.time[Annual$geo.time=="ES"] <- "Spain"
  Annual$geo.time[Annual$geo.time=="EU28"] <- "EU 28 Countries"
  Annual$geo.time[Annual$geo.time=="FI"] <- "Finland"
  Annual$geo.time[Annual$geo.time=="FR"] <- "France"
  Annual$geo.time[Annual$geo.time=="HU"] <- "Hungary"
  Annual$geo.time[Annual$geo.time=="IE"] <- "Ireland"
  Annual$geo.time[Annual$geo.time=="IS"] <- "Iceland"
  Annual$geo.time[Annual$geo.time=="IT"] <- "Italy"
  Annual$geo.time[Annual$geo.time=="LT"] <- "Lithuania"
  Annual$geo.time[Annual$geo.time=="LU"] <- "Luxembourg"
  Annual$geo.time[Annual$geo.time=="LV"] <- "Latvia"
  Annual$geo.time[Annual$geo.time=="MT"] <- "Malta"
  Annual$geo.time[Annual$geo.time=="NL"] <- "Netherlands"
  Annual$geo.time[Annual$geo.time=="NO"] <- "Norway"
  Annual$geo.time[Annual$geo.time=="PL"] <- "Poland"
  Annual$geo.time[Annual$geo.time=="PT"] <- "Portugal"
  Annual$geo.time[Annual$geo.time=="RO"] <- "Romania"
  Annual$geo.time[Annual$geo.time=="SE"] <- "Sweden"
  Annual$geo.time[Annual$geo.time=="SI"] <- "Slovenia"
  Annual$geo.time[Annual$geo.time=="SK"] <- "Slovakia"
  Annual$geo.time[Annual$geo.time=="TR"] <- "Turkey"
  Annual$geo.time[Annual$geo.time=="UK"] <- "UK"
  
  is.num <- sapply(Annual, is.numeric)
  Annual[is.num] <- lapply(Annual[is.num], round, 2)
  save(Annual, file = "/Users/tomokeeffe/Desktop/R/Apps/Quarterly7Processes/AnnualData.RData")
}

##############################################
#Quarterly
##############################################
{
nasq_10_nf_tr<-as.data.frame(nasq_10_nf_tr)
nasq_10_f_tr<-as.data.frame(nasq_10_f_tr)
prc_hicp_midx<- as.data.frame(prc_hicp_midx)
nasq_10_nf_tr$time<- as.Date(as.yearqtr(nasq_10_nf_tr$time, format = "%YQ%q"))
nasq_10_f_tr$time<- as.Date(as.yearqtr(nasq_10_f_tr$time, format = "%YQ%q"))
prc_hicp_midx$time<- as.Date(as.yearmon(prc_hicp_midx$time, format = "%YM%m"))



nasq_10_f_tr <- nasq_10_f_tr[order(as.Date(nasq_10_f_tr$time, format="%Y")),]
nasq_10_nf_tr <- nasq_10_nf_tr[order(as.Date(nasq_10_nf_tr$time, format="%Y")),]
prc_hicp_midx <- prc_hicp_midx[order(as.Date(prc_hicp_midx$time, format="%Y")),]

hh_nl_prv <- filter(nasq_10_f_tr, finpos == "NET", sector == "S14_S15", unit == "MIO_EUR", na_item == "B9F")
#NFC NET LENDING
nfc_nl_prv<- filter(nasq_10_f_tr,unit == "MIO_EUR",na_item == "B9F", finpos == "NET", sector == "S11")
#FC NET LENDING
fc_nl_prv<- filter(nasq_10_f_tr, unit == "MIO_EUR",na_item == "B9F", finpos == "NET", sector == "S12")

tmp2<-filter(nasq_10_nf_tr, unit=="CP_MEUR", s_adj=="NSA")
#tmp2<-filter(nasq_10_nf_tr, unit=="CP_MEUR", s_adj=="SCA")

tmp21<-filter(prc_hicp_midx, coicop=="CP00",unit=="I15")
tmp21<- filter(tmp21, grepl("01-01",time)|grepl("04-01",time)|grepl("07-01",time)|grepl("10-01",time))


gdp<-filter(tmp2,na_item=="B1GQ", direct=="PAID")
Quarterly <- subset(gdp,select = c("geo.time","time", "value"))
Quarterly <- plyr::rename(Quarterly, c("value" = "gdp"))

#general govt balance
gen_gov_balance<-filter(tmp2,na_item=="B9",direct=="PAID", sector=="S13")
gen_gov_balance <- subset(gen_gov_balance,select = c("geo.time","time", "value") )
Quarterly <- merge(Quarterly,gen_gov_balance, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "gen_gov_balance"))
Quarterly$gen_gov_balance_pc<-as.numeric(as.character(Quarterly$gen_gov_balance))/as.numeric(as.character(Quarterly$gdp))*100

Quarterly$gdp_d <- (Quarterly$gdp/lag(Quarterly$gdp, 4) -1)*100

gov_exp<-filter(tmp2,na_item=="OTE", direct=="PAID", sector=="S13")
gov_exp <- subset(gov_exp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,gov_exp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "gov_exp"))

#government tax revenue
gov_trev <- filter(tmp2,na_item == "OTR",sector == "S13", direct == "RECV")
gov_trev <- subset(gov_trev,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,gov_trev, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "gov_trev"))    

#theta - tax share
Quarterly$theta<-as.numeric(as.character(Quarterly$gov_trev))/as.numeric(as.character(Quarterly$gdp))

#Adjusted fiscal ratio
Quarterly$AFR<-as.numeric(as.character(Quarterly$gov_exp))/as.numeric(as.character(Quarterly$theta))
Quarterly$AFR_d <- (Quarterly$AFR/lag(Quarterly$AFR,4) -1)*100

#current extrnal balance - BOP - ROW view # REVERSE IN PLOT
bop<-filter(tmp2,na_item=="B12",direct=="PAID",sector=="S2")
bop$value<-bop$value*-1   #ok it's reversed here and a pc of gdp value is done
bop <- subset(bop,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,bop, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "bop"))
Quarterly$bop_pc<-as.numeric(as.character(Quarterly$bop))/as.numeric(as.character(Quarterly$gdp))*100

#exports in goods
bop_gexp<-filter(tmp2,na_item=="P61",direct=="PAID",sector=="S2")
bop_gexp <- subset(bop_gexp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,bop_gexp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "bop_gexp"))

#imports in goods
bop_gimp<-filter(tmp2,na_item=="P71",direct=="RECV",sector=="S2")
bop_gimp <- subset(bop_gimp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,bop_gimp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "bop_gimp"))    

#balance of trade in goods
Quarterly$bop_goods<-as.numeric(as.character(Quarterly$bop_gexp))-as.numeric(as.character(Quarterly$bop_gimp))

#balance of trade in goods (pc of gdp)
Quarterly$bop_goods_pc<-as.numeric(as.character(Quarterly$bop_goods))/as.numeric(as.character(Quarterly$gdp))*100

#exports in services
bop_sexp<-filter(tmp2,na_item=="P62",direct=="PAID",sector=="S2")
bop_sexp <- subset(bop_sexp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,bop_sexp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "bop_sexp")) 

#imports in services
bop_simp<-filter(tmp2,na_item=="P72",direct=="RECV",sector=="S2")
bop_simp <- subset(bop_simp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,bop_simp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "bop_simp")) 

#balance of trade in services
Quarterly$bop_soods<-as.numeric(as.character(Quarterly$bop_sexp))-as.numeric(as.character(Quarterly$bop_simp))
#balance of trade in services (pc of gdp)
Quarterly$bop_soods_pc<-as.numeric(as.character(Quarterly$bop_soods))/as.numeric(as.character(Quarterly$gdp))*100

#primary/secondary balances
pb_bal1<-filter(tmp2, na_item %in% c("IN1","IN21"),direct=="PAID",sector=="S2")
pb_bal1<-aggregate(pb_bal1$value, by=list(Category= pb_bal1$unit, pb_bal1$direct, pb_bal1$sector,pb_bal1$geo.time,pb_bal1$time), FUN=sum)
pb_bal1 <- plyr::rename(pb_bal1, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
pb_bal1 <- subset(pb_bal1,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,pb_bal1, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "pb_bal1"))  

pb_bal2<-filter(tmp2,na_item%in%c("IN1","IN21"),direct=="RECV",sector=="S2")
pb_bal2<-aggregate(pb_bal2$value, by=list(Category= pb_bal2$unit, pb_bal2$direct, pb_bal2$sector,pb_bal2$geo.time,pb_bal2$time), FUN=sum)
pb_bal2 <- plyr::rename(pb_bal2, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
pb_bal2 <- subset(pb_bal2,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,pb_bal2, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "pb_bal2"))

Quarterly$pb_bal<-(as.numeric(as.character(Quarterly$pb_bal1)))-(as.numeric(as.character(Quarterly$pb_bal2)))
Quarterly$pb_bal_pc<- (as.numeric(as.character(Quarterly$pb_bal))/as.numeric(as.character(Quarterly$gdp)))*100

#Import propensity
Quarterly$mu<-(Quarterly$bop_simp+Quarterly$bop_gimp+as.numeric(Quarterly$pb_bal2))/Quarterly$gdp

#Adjusted Trade Ratio 
Quarterly$ATR<-(Quarterly$bop_sexp+Quarterly$bop_gexp+as.numeric(Quarterly$pb_bal1))/Quarterly$mu

Quarterly$ATR_d <- (Quarterly$ATR/lag(Quarterly$ATR,4) -1)*100

#CFTR
Quarterly$CFTR<-(Quarterly$bop_sexp+Quarterly$bop_gexp+Quarterly$pb_bal1+Quarterly$gov_exp)/(Quarterly$mu+Quarterly$theta)
Quarterly$CFTR_d <- (Quarterly$CFTR/lag(Quarterly$CFTR,4) -1)*100

#FINANCIAL BALANCES
#Saving 
S <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B9", direct == "PAID")
S<-aggregate(S$value, by=list(Category= S$unit, S$direct, S$na_item,S$geo.time,S$time), FUN=sum)
S <- plyr::rename(S, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
S <- subset(S,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,S, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "S"))     

Quarterly$S_pc<-(Quarterly$S/Quarterly$gdp)*100

#government Balance
GDEF<-subset(gen_gov_balance, select = c( "geo.time","time", "value"))
Quarterly$GDEF <- Quarterly$gen_gov_balance*-1
Quarterly$GDEF_pc<-(Quarterly$GDEF/Quarterly$gdp)*100
#Current Account
CA<-filter(tmp2,na_item=="B9",direct=="PAID",sector=="S2")
CA <- subset(CA,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,CA, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "CA"))  
Quarterly$CA_pc<-(Quarterly$CA/Quarterly$gdp)*100

hh_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S14_S15")
hh_saving <- subset(hh_saving,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,hh_saving, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "hh_saving"))

hh_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S14_S15")
hh_inv <- subset(hh_inv,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,hh_inv, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "hh_inv"))

Quarterly$hh_sav_m_inv<-as.numeric(Quarterly$hh_saving)-as.numeric(Quarterly$hh_inv)
Quarterly$hh_sav_m_inv_pc<-(Quarterly$hh_sav_m_inv/Quarterly$gdp)*100

nfc_saving<-filter(tmp2,na_item=="B8G",direct=="RECV",sector=="S11")
nfc_saving <- subset(nfc_saving,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,nfc_saving, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "nfc_saving"))

nfc_inv<-filter(tmp2,na_item=="P5G",direct=="PAID",sector=="S11")
nfc_inv <- subset(nfc_inv,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,nfc_inv, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "nfc_inv"))

Quarterly$nfc_sav_m_inv<-as.numeric(Quarterly$nfc_saving)-as.numeric(Quarterly$nfc_inv)
Quarterly$nfc_sav_m_inv_pc<-(Quarterly$nfc_sav_m_inv/Quarterly$gdp)*100

#Non_Fin Saving
S_nonfin <- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B9", direct == "PAID")
S_nonfin<-aggregate(S_nonfin$value, by=list(Category= S_nonfin$unit, S_nonfin$direct, S_nonfin$na_item,S_nonfin$geo.time,S_nonfin$time), FUN=sum)
S_nonfin <- plyr::rename(S_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
S_nonfin <- subset(S_nonfin,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,S_nonfin, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "S_nonfin")) 

Quarterly$S_pc2<-(Quarterly$S_nonfin/Quarterly$gdp)*100

#real private disposable income
disp_inc_nonfin<- filter(tmp2,sector %in% c("S14_S15","S11"),na_item == "B6G", direct == "RECV")
disp_inc_nonfin<-aggregate(disp_inc_nonfin$value, by=list(Category= disp_inc_nonfin$unit, disp_inc_nonfin$direct, disp_inc_nonfin$na_item,disp_inc_nonfin$geo.time,disp_inc_nonfin$time), FUN=sum)

disp_inc_nonfin <- plyr::rename(disp_inc_nonfin, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
disp_inc_nonfin <- subset(disp_inc_nonfin,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,disp_inc_nonfin, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "disp_inc_nonfin"))


###########
cpi <- subset(tmp21, select = c("geo.time","time","value"))
Quarterly<- merge(Quarterly, cpi, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "cpi"))

#real private disposable income
disp_inc <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "B6G", direct == "RECV")
disp_inc<-aggregate(disp_inc$value, by=list(Category= disp_inc$unit, disp_inc$direct, disp_inc$na_item,disp_inc$geo.time,disp_inc$time), FUN=sum)
disp_inc <- plyr::rename(disp_inc, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
disp_inc <- subset(disp_inc,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,disp_inc, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "disp_inc"))    

Quarterly$disp_inc<-(Quarterly$disp_inc/Quarterly$cpi)*100
Quarterly$disp_inc2_d <- (Quarterly$disp_inc/lag(Quarterly$disp_inc,4) -1)*100

#real private expenditure
prv_exp <- filter(tmp2,sector %in% c("S14_S15","S11","S12"),na_item == "P3", direct == "PAID")
prv_exp<-aggregate(prv_exp$value, by=list(Category= prv_exp$unit, prv_exp$direct, prv_exp$na_item, prv_exp$geo.time, prv_exp$time), FUN=sum)
prv_exp <- plyr::rename(prv_exp, c("x" = "value", "Group.5"= "time", "Group.4"="geo.time"))
prv_exp <- subset(prv_exp,select = c("geo.time","time", "value"))
Quarterly <- merge(Quarterly,prv_exp, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "prv_exp"))     

Quarterly$prv_exp<-(Quarterly$prv_exp/Quarterly$cpi)*100
Quarterly$prv_exp_d <- (Quarterly$prv_exp/lag(Quarterly$prv_exp,4) -1)*100

nl_prv <- subset(hh_nl_prv, select = c("geo.time","time", "value"))
Quarterly<- merge(Quarterly, nl_prv, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "nl_prv"))

nfc_nl_prv <- subset(nfc_nl_prv, select = c("geo.time","time", "value"))
Quarterly<- merge(Quarterly, nfc_nl_prv, all = TRUE, by =  c("geo.time","time"))
Quarterly <- plyr::rename(Quarterly, c("value" = "nfc_nl_prv"))

Quarterly$nl_prv<- Quarterly$nl_prv +as.numeric(as.character(Quarterly$nfc_nl_prv)) 
Quarterly$nl_prv_pc<-(as.numeric(Quarterly$nl_prv))/(Quarterly$disp_inc_nonfin)*-100


Quarterly$S_pc_disp<-(Quarterly$S_nonfin/Quarterly$disp_inc_nonfin)*100

Quarterly <- filter(Quarterly, !(geo.time %in% c("US", "EA","EA18","EEA","EU","HR","ME","RS","IS","MT","LU","TR","HU","CY","LV","LT","SK")))


Quarterly$geo.time<-as.character(Quarterly$geo.time)
#Renaming
Quarterly$geo.time[Quarterly$geo.time=="AT"] <- "Austria"
Quarterly$geo.time[Quarterly$geo.time=="BE"] <- "Belgium"
Quarterly$geo.time[Quarterly$geo.time=="BG"] <- "Bulgaria"
Quarterly$geo.time[Quarterly$geo.time=="CH"] <- "Switzerland"
Quarterly$geo.time[Quarterly$geo.time=="CY"] <- "Cyprus"
Quarterly$geo.time[Quarterly$geo.time=="CZ"] <- "Czech Republic"
Quarterly$geo.time[Quarterly$geo.time=="DE"] <- "Germany"
Quarterly$geo.time[Quarterly$geo.time=="DK"] <- "Denmark"
Quarterly$geo.time[Quarterly$geo.time=="EA19"] <- "Euro Area(19 Countries)"
Quarterly$geo.time[Quarterly$geo.time=="EE"] <- "Estonia"
Quarterly$geo.time[Quarterly$geo.time=="EL"] <- "Greece"
Quarterly$geo.time[Quarterly$geo.time=="ES"] <- "Spain"
Quarterly$geo.time[Quarterly$geo.time=="EU28"] <- "EU 28 Countries"
Quarterly$geo.time[Quarterly$geo.time=="FI"] <- "Finland"
Quarterly$geo.time[Quarterly$geo.time=="FR"] <- "France"
Quarterly$geo.time[Quarterly$geo.time=="HU"] <- "Hungary"
Quarterly$geo.time[Quarterly$geo.time=="IE"] <- "Ireland"
Quarterly$geo.time[Quarterly$geo.time=="IS"] <- "Iceland"
Quarterly$geo.time[Quarterly$geo.time=="IT"] <- "Italy"
Quarterly$geo.time[Quarterly$geo.time=="LT"] <- "Lithuania"
Quarterly$geo.time[Quarterly$geo.time=="LU"] <- "Luxembourg"
Quarterly$geo.time[Quarterly$geo.time=="LV"] <- "Latvia"
Quarterly$geo.time[Quarterly$geo.time=="MT"] <- "Malta"
Quarterly$geo.time[Quarterly$geo.time=="NL"] <- "Netherlands"
Quarterly$geo.time[Quarterly$geo.time=="NO"] <- "Norway"
Quarterly$geo.time[Quarterly$geo.time=="PL"] <- "Poland"
Quarterly$geo.time[Quarterly$geo.time=="PT"] <- "Portugal"
Quarterly$geo.time[Quarterly$geo.time=="RO"] <- "Romania"
Quarterly$geo.time[Quarterly$geo.time=="SE"] <- "Sweden"
Quarterly$geo.time[Quarterly$geo.time=="SI"] <- "Slovenia"
Quarterly$geo.time[Quarterly$geo.time=="SK"] <- "Slovakia"
Quarterly$geo.time[Quarterly$geo.time=="TR"] <- "Turkey"
Quarterly$geo.time[Quarterly$geo.time=="UK"] <- "UK"

is.num <- sapply(Quarterly, is.numeric)
Quarterly[is.num] <- lapply(Quarterly[is.num], round, 2)

save(Quarterly, file = "/Users/tomokeeffe/Desktop/R/Apps/Quarterly7Processes/QuarterlyData.RData")}





M3 <- get_data("BSI.M.U2.Y.V.M30.X.I.U2.2300.Z01.V")
M3$obstime <- as.Date(as.yearmon(M3$obstime))
save(M3, file = "/Users/tomokeeffe/Desktop/R/Apps/Quarterly7Processes/M3.RData")

#00c0ef Blue
#00a65a Green
#006312 dark green
#f39c12 warning




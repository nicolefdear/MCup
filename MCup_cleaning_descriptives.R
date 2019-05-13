#MCup analysis
#Created by: Nicole Dear
#Created on: May 7, 2019

library(tidyr)
library(tidyverse)
library(dplyr)
library(gmodels)
library(knitr)
library(Hmisc)

setwd("H:/MCup/Paper/Data")
baseline <- read.csv("DREAMSMCupBaseline_DATA_2019-05-07_1124.csv") %>% select(c(1:3,5:174))
mo1 <- read.csv("DREAMSMCupMonth1_DATA_2019-05-07_1125.csv") %>% select(c(1:3,5:113))
mo6 <- read.csv("DREAMSMCupMonth6_DATA_2019-05-07_1126.csv") %>% select(c(1:3,5:120))
mo12 <- read.csv("DREAMSMCupMonth12Fol_DATA_2019-05-07_1126.csv") %>% select(c(1:3,5:118))
mo12 <- mo12[-c(194), ]

#manually change site id error in mo12 dataset
mo12[mo12$record_id_exit==169, ]$m1site_code_exit <- 104

#create unique id for all datasets
baseline$unique_id <- paste0(baseline$pin, baseline$site_id) 
mo1$unique_id <- paste0(mo1$m1pin_id, mo1$m1site_code)
mo6$unique_id <- paste0(mo6$m1pin_id_m6, mo6$m1site_code_m6) 
mo12$unique_id <- paste0(mo12$m1pin_id_exit, mo12$m1site_code_exit)

#create flags for each dataset
baseline$flag_baseline <- 1
mo1$flag_mo1 <- 1
mo6$flag_mo6 <- 1
mo12$flag_mo12 <- 1

#make sure all unique ids match across all datasets
baseline_id <- baseline %>% select(c(174,175))
mo1_id <- mo1 %>% select(c(113,114))
mo6_id <- mo6 %>% select(c(120,121))
mo12_id <- mo12 %>% select(c(118,119))

a <- merge(baseline_id, mo1_id, by="unique_id", all=T)
b <- merge(a, mo6_id, by="unique_id", all=T)
c <- merge(b, mo12_id, by="unique_id", all=T)

#BASELINE CLEANING

#pattern match for other residence = lives with friend
baseline[baseline$unique_id==15113, ]$other_resi <- ""
baseline[baseline$unique_id==15113, ]$live_term___3 <- 1
#pattern match for other residence = renting/rents flat or room
baseline$live_term___7 <- grepl(paste("rent|flat|alone",collapse="|"), baseline$other_resi, ignore.case=TRUE)
#pattern match for other residence = communal residence/house sharing/lives with other tenants
baseline$live_term___8 <- grepl(paste("commun|sharing|other",collapse="|"), baseline$other_resi, ignore.case=TRUE)
#pattern match for other residence = lives with guardian
baseline$live_term___9 <- grepl(paste("guardian",collapse="|"), baseline$other_resi, ignore.case=TRUE)

#new variable for "lives with"
baseline$residence[baseline$live_term___1==1] <- 1 #Family (at home and commutes to college)
baseline$residence[baseline$live_term___2==1] <- 2 #College residences
baseline$residence[baseline$live_term___3==1] <- 3 #Friends
baseline$residence[baseline$live_term___4==1] <- 4 #Partner
baseline$residence[baseline$live_term___5==1] <- 5 #Family members (not family home)
baseline$residence[baseline$live_term___7==TRUE] <- 7 #Renting flat/room alone
baseline$residence[baseline$live_term___8==TRUE] <- 8 #Communal residence
baseline$residence[baseline$live_term___9==TRUE] <- 9 #Guardian
table(baseline$residence)

#pattern match for water source = tap/sink
baseline[baseline$unique_id==6101, ]$wtr_hme <- 1
baseline[baseline$unique_id==22101, ]$wtr_hme <- 1
#pattern match for water source = river
baseline$river <- grepl(paste("river",collapse="|"), baseline$other_wtr, ignore.case=TRUE)
#pattern match for water source = jojo tank
baseline$jojo <- grepl(paste("jojo|water tank|tank water|rain|kan",collapse="|"), baseline$other_wtr, ignore.case=TRUE)
#pattern match for water source = water truck
baseline$truck <- grepl(paste("truck|delivered",collapse="|"), baseline$other_wtr, ignore.case=TRUE)

#new variable for "Water source for hand washing"
baseline$watersource[baseline$wtr_hme==1] <- 1  #Piped water in dwelling
baseline$watersource[baseline$wtr_hme==2] <- 2  #Piped water on site or yard
baseline$watersource[baseline$wtr_hme==3] <- 3  #Public tap/ Standpipe
baseline$watersource[baseline$wtr_hme==4] <- 4  #Sinks or taps at Community Ablution Block
baseline$watersource[baseline$river==TRUE] <- 5 #River
baseline$watersource[baseline$jojo==TRUE] <- 6  #Jojo tank
baseline$watersource[baseline$truck==TRUE] <- 7 #Water truck
                                                #unknown = 3
table(baseline$watersource)

#pattern match for who pays college fees = grant
baseline$grant1 <- grepl(paste("nsfas|dsd|nrf|nmds|scholarship|trust|Lignotech|irr|hsa|funding|grant|burs|dhet|dept",collapse="|"), baseline$grnt_name, ignore.case=TRUE)
baseline$grant2 <- grepl(paste("nsfas|dsd|sponsor|burs|Premier|nyda|isfap|career wise",collapse="|"), baseline$othr_src_fee, ignore.case=TRUE)

#new variable for "Who pays college fees"
baseline$collegefees[baseline$pay_fees==1] <- 1  #Herself
baseline$collegefees[baseline$pay_fees==2] <- 2  #Family member
baseline$collegefees[baseline$pay_fees==3] <- 3  #Partner
baseline$collegefees[baseline$grant1==TRUE|baseline$grant2==TRUE] <- 4  #Grant
table(baseline$collegefees)

#MONTH 1 CLEANING
names(mo1)
mo1_sub <- mo1 %>% select(c(1,50:56))
table(mo1$diffi_insert___5)

#pattern match for difficulty with insertion = fear
mo1$diffi_insert___6 <- grepl(paste("scared|nervous|worried|doubt|a bit|fear|rear|concerned",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)
table(mo1$diffi_insert___6)
#pattern match for difficulty with insertion = forgot instructions
mo1$diffi_insert___7 <- grepl(paste("instructions|practice|forgot",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)
table(mo1$diffi_insert___7)
#pattern match for difficulty with insertion = uncomfortable
mo1$diffi_insert___8 <- grepl(paste("uncomfortable|new",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)
table(mo1$diffi_insert___8)

#recode to main options
mo1[mo1$unique_id==2105, ]$diffi_insert___2 <- 1
mo1[mo1$unique_id==2105, ]$diffi_insert___5 <- 0
mo1[mo1$unique_id==28104, ]$diffi_insert___1 <- 1
mo1[mo1$unique_id==28104, ]$diffi_insert___5 <- 0
mo1[mo1$unique_id==6107, ]$diffi_insert___1 <- 1
mo1[mo1$unique_id==6107, ]$diffi_insert___5 <- 0
mo1[mo1$unique_id==80104, ]$diffi_insert___1 <- 1
mo1[mo1$unique_id==80104, ]$diffi_insert___5 <- 0
mo1[mo1$unique_id==7108, ]$diffi_insert___1 <- 2
mo1[mo1$unique_id==7108, ]$diffi_insert___5 <- 0

#new variable for "difficulty with insertion"
mo1$insertion_problems[mo1$diffi_insert___1==1|mo1$diffi_insert___4==1] <- 1 #pain/discomfort
mo1$insertion_problems[mo1$diffi_insert___2==1|mo1$diffi_insert___3==1|mo1$diffi_insert___7==TRUE] <- 2 #technical issues
mo1$insertion_problems[mo1$diffi_insert___6==TRUE] <- 3 #fear
table(mo1$insertion_problems)

#pattern match for difficulty with removal = fear
mo1$prob_removal___7 <- grepl(paste("scared|nervous|worried|doubt|a bit|fear|concerned",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)

#pattern match for difficulty with removal = forgot instructions
mo1$prob_removal___8 <- grepl(paste("instructions|practice|forgot",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)

#pattern match for difficulty with removal = slippery stem/couldn't reach stem
mo1$prob_removal___9 <- grepl(paste("stem|slippery|pushed far|higher up",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)

#recode to main options
mo1[mo1$unique_id==3101, ]$prob_removal___3 <- 1
mo1[mo1$unique_id==3101, ]$prob_removal___6 <- 0
mo1[mo1$unique_id==8109, ]$prob_removal___3 <- 1
mo1[mo1$unique_id==8109, ]$prob_removal___6 <- 0

#new variable for "difficulty with removal"
mo1$removal_problems[mo1$prob_removal___1==1|mo1$prob_removal___4==1] <- 1 #pain/discomfort
mo1$removal_problems[mo1$prob_removal___2==1|mo1$prob_removal___3==1|mo1$prob_removal___5==1|mo1$prob_removal___8==TRUE|mo1$prob_removal___9==TRUE] <- 2 #technical issues
mo1$removal_problems[mo1$prob_removal___7==TRUE] <- 3 #fear

#pull all pids for ppts that used mcup at any point
mo1_used <- mo1 %>% subset(mo1$mcup_used==1, select=c(113))
mo6_used <- mo6 %>% subset(mo6$mcup_used==1, select=c(120))
mo12_used <- mo12 %>% subset(mo12$mcup_used==1, select=c(118))

d <- merge(mo1_used, mo6_used, by="unique_id", all=T)
e <- merge(d, mo12_used, by="unique_id", all=T)
f <- unique(e)
f$flag_usedmcup <- 1

#merge pids where mcup_used=1 to baseline dataset
base <- merge(baseline, f, by="unique_id", all=T)
base[c("flag_usedmcup")][is.na(base[c("flag_usedmcup")])] <- 0

table(base$flag_usedmcup)

#descriptives table 1
base_sub1 <- base %>% select(c("unique_id", "residence", "watersource", "toi_type_hme___1", "toi_type_hme___2", "toi_type_hme___3", "lev_edu", "collegefees", "rel_status", "flag_usedmcup"))
lapply(names(base_sub1)[-1],function(x) CrossTable(base_sub1[,x],base_sub1[,"flag_usedmcup"],format="SAS",prop.chisq=FALSE,digits=2))

#descriptives table 2
base_sub2 <- base %>% select(c("unique_id", "pay_sanitary___1", "pay_sanitary___2", "pay_sanitary___3", "pay_sanitary___5", "affordable_saniary", 
                               "pads_disposable___5", "pads_washable___5", "tampons___5", "cloth_rags___5", "panty_liner___5", "paper_sanitary___5",
                               "paper_tempon___5", "newspaper___5", "cotton_wool___5", "sponge___5", "mcup___5", "mcup___3", "mcup___1", 
                               "why_trymcup___1", "why_trymcup___2", "why_trymcup___3", "why_trymcup___4", "why_trymcup___5", "why_trymcup___6", "flag_usedmcup"))
lapply(names(base_sub2)[-1],function(x) CrossTable(base_sub2[,x],base_sub2[,"flag_usedmcup"],format="SAS",prop.chisq=FALSE,digits=2))


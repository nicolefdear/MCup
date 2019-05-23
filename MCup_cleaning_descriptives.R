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
baseline$residence[baseline$live_term___7==TRUE] <- 6 #Renting flat/room alone
baseline$residence[baseline$live_term___8==TRUE] <- 7 #Communal residence
baseline$residence[baseline$live_term___9==TRUE] <- 8 #Guardian
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
#pattern match for reason why didn't use mcup = virginity reasons
mo1$reason_notused1 <- grepl(paste("hymen|virginity test|virgin",collapse="|"), mo1$othr_reson1, ignore.case=TRUE)
#pattern match for reason why didn't use mcup = forgot
mo1$reason_notused2 <- grepl(paste("forgot about it|had forgotten the cup|had left it at school campus",collapse="|"), mo1$othr_reson1, ignore.case=TRUE)
#pattern match for reason why didn't use mcup = prefer other sanitary product
mo1$reason_notused3 <- grepl(paste("pads|preferred to use panty liners",collapse="|"), mo1$othr_reson1, ignore.case=TRUE)
#pattern match for reason why didn't use mcup = she felt uncomfortable
mo1$reason_notused4 <- grepl(paste("was uncomfortable with using it|she felt uncomfortable using the MCup|She felt uncomfortable inserting it",collapse="|"), mo1$othr_reson1, ignore.case=TRUE)
#pattern match for reason why didn't use mcup = painful
mo1$reason_notused5 <- grepl(paste("painful",collapse="|"), mo1$othr_reson1, ignore.case=TRUE)

#new variable for why didn't use mcup
mo1$reason_notused[mo1$cup_notused==1] <- 1 #has not had menstrual cycle
mo1$reason_notused[mo1$cup_notused==2|mo1$reason_notused5==TRUE] <- 2 #uncomfortable/painful on insertion
mo1$reason_notused[mo1$cup_notused==3] <- 3 #fear
mo1$reason_notused[mo1$cup_notused==4] <- 4 #gave it to friend/relative
mo1$reason_notused[mo1$reason_notused1==TRUE] <- 5 #reasons relating to virginity/testing
mo1$reason_notused[mo1$reason_notused2==TRUE] <- 6 #forgot about it
mo1$reason_notused[mo1$reason_notused3==TRUE] <- 7 #prefer other sanitary product
mo1$reason_notused[mo1$reason_notused4==TRUE] <- 8 #she felt uncomfortable

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
mo1$prob_removal___7 <- grepl(paste("scared|nervous|worried|doubt|a bit|fear|concerned|afraid",collapse="|"), mo1$othr_dificlt, ignore.case=TRUE)
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

#MONTH 1/6/12 CLEANING for reasons why didn't use cup for entire period (pattern match)
#Prefer to use another sanitary product or alternate products
mo1$whynotentire_1 <- grepl(paste("wanted to finish sanitary pads|preferred to use a pad|alternate|pad|liner|Prefer to use another sanitary product|wanted to finish off the remaining pads",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_1 <- grepl(paste("wanted to finish sanitary pads|preferred to use a pad|alternate|pad|liner|Prefer to use another sanitary product|wanted to finish off the remaining pads",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_1 <- grepl(paste("wanted to finish sanitary pads|preferred to use a pad|alternate|pad|liner|Prefer to use another sanitary product|wanted to finish off the remaining pads",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Worried about leakage
mo1$whynotentire_2 <- grepl(paste("Leakage|leaked|leak|worried that it will get full|flow was heavy|MCup leaked|She had a heavy flow on the first day so she decided not to use it|Her flow was to high|heavy bleeding|The MCup filled up very quickly|It quickly gets filled up|only used it on the last day because she only spots|was bleeding heavly|her flow was heavy|Bleeds too much|she has a heavy flow on day one|prefer to use pads during heavy days|Uses it on the last days of her periods because her flow is not heavy|Flow too heavy on the first days|Uses it her last days because her flow is less heavier",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_2 <- grepl(paste("Leakage|leaked|leak|worried that it will get full|flow was heavy|MCup leaked|She had a heavy flow on the first day so she decided not to use it|Her flow was to high|heavy bleeding|The MCup filled up very quickly|It quickly gets filled up|only used it on the last day because she only spots|was bleeding heavly|her flow was heavy|Bleeds too much|she has a heavy flow on day one|prefer to use pads during heavy days|Uses it on the last days of her periods because her flow is not heavy|Flow too heavy on the first days|Uses it her last days because her flow is less heavier",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_2 <- grepl(paste("Leakage|leaked|leak|worried that it will get full|flow was heavy|MCup leaked|She had a heavy flow on the first day so she decided not to use it|Her flow was to high|heavy bleeding|The MCup filled up very quickly|It quickly gets filled up|only used it on the last day because she only spots|was bleeding heavly|her flow was heavy|Bleeds too much|she has a heavy flow on day one|prefer to use pads during heavy days|Uses it on the last days of her periods because her flow is not heavy|Flow too heavy on the first days|Uses it her last days because her flow is less heavier",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Pain/discomfort
mo1$whynotentire_3 <- grepl(paste("Discomfort|painful|pain|it did not sit well|Mcup was uncomfortable|It is uncomfortable|It was uncomfortable|It felt uncomfortable|It was a bit uncomfortable|cup was uncomfortable|It is very uncomfortable|cup was very uncomfortable|it did not feel comfortable|stabbing|cup was uncomfortable|It is a bit uncomfortable|cup feels uncomfortable|It feels uncomfortable|It is still uncomfrtable",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_3 <- grepl(paste("Discomfort|painful|pain|it did not sit well|Mcup was uncomfortable|It is uncomfortable|It was uncomfortable|It felt uncomfortable|It was a bit uncomfortable|cup was uncomfortable|It is very uncomfortable|cup was very uncomfortable|it did not feel comfortable|stabbing|cup was uncomfortable|It is a bit uncomfortable|cup feels uncomfortable|It feels uncomfortable|It is still uncomfrtable",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_3 <- grepl(paste("Discomfort|painful|pain|it did not sit well|Mcup was uncomfortable|It is uncomfortable|It was uncomfortable|It felt uncomfortable|It was a bit uncomfortable|cup was uncomfortable|It is very uncomfortable|cup was very uncomfortable|it did not feel comfortable|stabbing|cup was uncomfortable|It is a bit uncomfortable|cup feels uncomfortable|It feels uncomfortable|It is still uncomfrtable",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Fear
mo1$whynotentire_4 <- grepl(paste("Worried|fear|afraid|scared|nervous|worries",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_4 <- grepl(paste("Worried|fear|afraid|scared|nervous|worries",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_4 <- grepl(paste("Worried|fear|afraid|scared|nervous|worries",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Prefers not to use when flow is light
mo1$whynotentire_5 <- grepl(paste("Prefer to use it on the first 2 days of heavy flow|flow was not heavy|prefer not to use it on the last day|prefer to use on heavy flow days|did not like using it when the flow is less|prefer not to use the cup when the blood flow is very less|flow was light|flow was too little|last day she was spoting|her periods were too light|flow was low|only used it during heavy flow days|She usually uses a pantyliner on the last day of her periods|used it during  days of heavy flow only|Her flow was lower on her last day|Prefer not to use mcup when the flow is very light|There were only drops of blood on her last day|on the last day she was spotting|Her flow was light on the last day|She had only spotting on the last day|flow was lower on the last day|flow was not as heavy o the last day|bleeding was little|Her flow was not heavy on her last day|flow was not much|get less flow|flow is very weak|prefer not to use it when the flow is very light|prefer to use the cup during heavy days|light flow so i don't use|doesn't use the Mcup on the last day because bleeding is very little|Her flow was low on the last day|Her flow was lower on her last days|She uses it will she is bleeding heavily|Her flow is not that heavy on the last day|Her flow was light on the last day|spotting|light flow|less bleeding|Don't use the MCup when my period is lighter|drops|She bleeds a lot on the second day so she uses the MCup|less bleeding|flow was not heavy|prefer to use during heavy flow|use heavy flow days|prefer to use a pad on the last day|flow was not that heavy|flow on the last day is low|flow was not too heavy|prefer to use MCup only on heavy days|low flow on the last day|Prefer to use a pad during light flow days|prefer to use it during heavy flow days|flow was low on the last day|Don't bleed much|Light flow on her last day|She wears a pad on the last day of her periods because her flow is low|Her flow is low on the last day|uses it on the second day when her flow is heavier|light flow on the last day of her periods|flow was heavy on the first two days so she used the MCup|She uses it on the first days of her periods when her flow is heavy and then uses a pantyliner on the last day since her flow is lighter|prefer to use pads during light flow days|Her flow is heavy on the first day so she uses the MCup|uses it on her heavy days|prefer to use a pad during light flow|prefer to use a pantyliner on the last day|only on her heavy days|only use the mcup during first 3 heavy flow days|She uses it on the first few days when her flow is heavy|Only uses it when her flow is heavy|Does not bleed a lot on her last day",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_5 <- grepl(paste("Prefer to use it on the first 2 days of heavy flow|flow was not heavy|prefer not to use it on the last day|prefer to use on heavy flow days|did not like using it when the flow is less|prefer not to use the cup when the blood flow is very less|flow was light|flow was too little|last day she was spoting|her periods were too light|flow was low|only used it during heavy flow days|She usually uses a pantyliner on the last day of her periods|used it during  days of heavy flow only|Her flow was lower on her last day|Prefer not to use mcup when the flow is very light|There were only drops of blood on her last day|on the last day she was spotting|Her flow was light on the last day|She had only spotting on the last day|flow was lower on the last day|flow was not as heavy o the last day|bleeding was little|Her flow was not heavy on her last day|flow was not much|get less flow|flow is very weak|prefer not to use it when the flow is very light|prefer to use the cup during heavy days|light flow so i don't use|doesn't use the Mcup on the last day because bleeding is very little|Her flow was low on the last day|Her flow was lower on her last days|She uses it will she is bleeding heavily|Her flow is not that heavy on the last day|Her flow was light on the last day|spotting|light flow|less bleeding|Don't use the MCup when my period is lighter|drops|She bleeds a lot on the second day so she uses the MCup|less bleeding|flow was not heavy|prefer to use during heavy flow|use heavy flow days|prefer to use a pad on the last day|flow was not that heavy|flow on the last day is low|flow was not too heavy|prefer to use MCup only on heavy days|low flow on the last day|Prefer to use a pad during light flow days|prefer to use it during heavy flow days|flow was low on the last day|Don't bleed much|Light flow on her last day|She wears a pad on the last day of her periods because her flow is low|Her flow is low on the last day|uses it on the second day when her flow is heavier|light flow on the last day of her periods|flow was heavy on the first two days so she used the MCup|She uses it on the first days of her periods when her flow is heavy and then uses a pantyliner on the last day since her flow is lighter|prefer to use pads during light flow days|Her flow is heavy on the first day so she uses the MCup|uses it on her heavy days|prefer to use a pad during light flow|prefer to use a pantyliner on the last day|only on her heavy days|only use the mcup during first 3 heavy flow days|She uses it on the first few days when her flow is heavy|Only uses it when her flow is heavy|Does not bleed a lot on her last day",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_5 <- grepl(paste("Prefer to use it on the first 2 days of heavy flow|flow was not heavy|prefer not to use it on the last day|prefer to use on heavy flow days|did not like using it when the flow is less|prefer not to use the cup when the blood flow is very less|flow was light|flow was too little|last day she was spoting|her periods were too light|flow was low|only used it during heavy flow days|She usually uses a pantyliner on the last day of her periods|used it during  days of heavy flow only|Her flow was lower on her last day|Prefer not to use mcup when the flow is very light|There were only drops of blood on her last day|on the last day she was spotting|Her flow was light on the last day|She had only spotting on the last day|flow was lower on the last day|flow was not as heavy o the last day|bleeding was little|Her flow was not heavy on her last day|flow was not much|get less flow|flow is very weak|prefer not to use it when the flow is very light|prefer to use the cup during heavy days|light flow so i don't use|doesn't use the Mcup on the last day because bleeding is very little|Her flow was low on the last day|Her flow was lower on her last days|She uses it will she is bleeding heavily|Her flow is not that heavy on the last day|Her flow was light on the last day|spotting|light flow|less bleeding|Don't use the MCup when my period is lighter|drops|She bleeds a lot on the second day so she uses the MCup|less bleeding|flow was not heavy|prefer to use during heavy flow|use heavy flow days|prefer to use a pad on the last day|flow was not that heavy|flow on the last day is low|flow was not too heavy|prefer to use MCup only on heavy days|low flow on the last day|Prefer to use a pad during light flow days|prefer to use it during heavy flow days|flow was low on the last day|Don't bleed much|Light flow on her last day|She wears a pad on the last day of her periods because her flow is low|Her flow is low on the last day|uses it on the second day when her flow is heavier|light flow on the last day of her periods|flow was heavy on the first two days so she used the MCup|She uses it on the first days of her periods when her flow is heavy and then uses a pantyliner on the last day since her flow is lighter|prefer to use pads during light flow days|Her flow is heavy on the first day so she uses the MCup|uses it on her heavy days|prefer to use a pad during light flow|prefer to use a pantyliner on the last day|only on her heavy days|only use the mcup during first 3 heavy flow days|She uses it on the first few days when her flow is heavy|Only uses it when her flow is heavy|Does not bleed a lot on her last day",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Felt uncomfortable/getting used to it
mo1$whynotentire_6 <- grepl(paste("She felt uncomfortable|She was uncomfortable|did not feel comfortable|I feel a bit uncomfortable|She was not comfortable|She felt a bit uncomfortable|She just felt uncomfortable|first time|testing|teasting|still learning to adjust|trying|She felt weird|not used to it|she was a bit unconformable|first time|not yet used to|needs to get used to|I'm not comfortable|getting used to|not used to it|She is not yet comfortable|make her to be uncomfortable|She still feels uncomfortable",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_6 <- grepl(paste("She felt uncomfortable|She was uncomfortable|did not feel comfortable|I feel a bit uncomfortable|She was not comfortable|She felt a bit uncomfortable|She just felt uncomfortable|first time|testing|teasting|still learning to adjust|trying|She felt weird|not used to it|she was a bit unconformable|first time|not yet used to|needs to get used to|I'm not comfortable|getting used to|not used to it|She is not yet comfortable|make her to be uncomfortable|She still feels uncomfortable",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_6 <- grepl(paste("She felt uncomfortable|She was uncomfortable|did not feel comfortable|I feel a bit uncomfortable|She was not comfortable|She felt a bit uncomfortable|She just felt uncomfortable|first time|testing|teasting|still learning to adjust|trying|She felt weird|not used to it|she was a bit unconformable|first time|not yet used to|needs to get used to|I'm not comfortable|getting used to|not used to it|She is not yet comfortable|make her to be uncomfortable|She still feels uncomfortable",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#Not with her when she started period/forgot
mo1$whynotentire_7 <- grepl(paste("She forgot she had a menstrual cup|She had already started|had not anticipated that she would go on her periods|forgot to use it|forgotten to use it|it was at home|got preiods unexpectedly|The Mcup was not with her|did not have Mcup with her|mcup was not near|mcup was not available|She forgot to insert it on the first day her periods started",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_7 <- grepl(paste("She forgot she had a menstrual cup|She had already started|had not anticipated that she would go on her periods|forgot to use it|forgotten to use it|it was at home|got preiods unexpectedly|The Mcup was not with her|did not have Mcup with her|mcup was not near|mcup was not available|She forgot to insert it on the first day her periods started",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_7 <- grepl(paste("She forgot she had a menstrual cup|She had already started|had not anticipated that she would go on her periods|forgot to use it|forgotten to use it|it was at home|got preiods unexpectedly|The Mcup was not with her|did not have Mcup with her|mcup was not near|mcup was not available|She forgot to insert it on the first day her periods started",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#No clean water to wash it with (traveling)
mo1$whynotentire_8 <- grepl(paste("Clean|cannot wash it|no water to wash it|not a lot of water available",collapse="|"), mo1$other_sanitary, ignore.case=TRUE)
mo6$whynotentire_8 <- grepl(paste("Clean|cannot wash it|no water to wash it|not a lot of water available",collapse="|"), mo6$other_sanitary, ignore.case=TRUE)
mo12$whynotentire_8 <- grepl(paste("Clean|cannot wash it|no water to wash it|not a lot of water available",collapse="|"), mo12$other_sanitary, ignore.case=TRUE)

#new variables for "reason why didn't use entire period"
mo1$whynotentire[mo1$whynotentire_1==TRUE|mo1$notentire_period==1] <- 1
mo1$whynotentire[mo1$whynotentire_2==TRUE|mo1$notentire_period==2] <- 2
mo1$whynotentire[mo1$whynotentire_3==TRUE] <- 3
mo1$whynotentire[mo1$whynotentire_4==TRUE] <- 4
mo1$whynotentire[mo1$whynotentire_5==TRUE] <- 5
mo1$whynotentire[mo1$whynotentire_6==TRUE] <- 6
mo1$whynotentire[mo1$whynotentire_7==TRUE] <- 7
mo1$whynotentire[mo1$whynotentire_8==TRUE] <- 8

mo6$whynotentire[mo6$whynotentire_1==TRUE|mo6$notentire_period==1] <- 1
mo6$whynotentire[mo6$whynotentire_2==TRUE|mo6$notentire_period==2] <- 2
mo6$whynotentire[mo6$whynotentire_3==TRUE] <- 3
mo6$whynotentire[mo6$whynotentire_4==TRUE] <- 4
mo6$whynotentire[mo6$whynotentire_5==TRUE] <- 5
mo6$whynotentire[mo6$whynotentire_6==TRUE] <- 6
mo6$whynotentire[mo6$whynotentire_7==TRUE] <- 7
mo6$whynotentire[mo6$whynotentire_8==TRUE] <- 8

mo12$whynotentire[mo12$whynotentire_1==TRUE|mo12$notentire_period==1] <- 1
mo12$whynotentire[mo12$whynotentire_2==TRUE|mo12$notentire_period==2] <- 2
mo12$whynotentire[mo12$whynotentire_3==TRUE] <- 3
mo12$whynotentire[mo12$whynotentire_4==TRUE] <- 4
mo12$whynotentire[mo12$whynotentire_5==TRUE] <- 5
mo12$whynotentire[mo12$whynotentire_6==TRUE] <- 6
mo12$whynotentire[mo12$whynotentire_7==TRUE] <- 7
mo12$whynotentire[mo12$whynotentire_8==TRUE] <- 8

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

#DESCRIPTIVE ANALYSIS
#table 1
prop.table(table(baseline$residence))*100
table(baseline$residence)
prop.table(table(baseline$watersource))*100
table(baseline$watersource)
prop.table(table(baseline$toi_type_hme___1))*100
prop.table(table(baseline$toi_type_hme___2))*100
prop.table(table(baseline$toi_type_hme___3))*100
table(baseline$toi_type_hme___1)
table(baseline$toi_type_hme___2)
table(baseline$toi_type_hme___3)
prop.table(table(baseline$lev_edu))*100
table(baseline$lev_edu)
prop.table(table(baseline$collegefees))*100
table(baseline$collegefees)
prop.table(table(baseline$rel_status))*100
table(baseline$rel_status)

#table 2
prop.table(table(baseline$pay_sanitary___1))*100
table(baseline$pay_sanitary___1)
prop.table(table(baseline$pay_sanitary___2))*100
table(baseline$pay_sanitary___2)
prop.table(table(baseline$pay_sanitary___3))*100
table(baseline$pay_sanitary___3)
prop.table(table(baseline$pay_sanitary___4))*100
table(baseline$pay_sanitary___4)
prop.table(table(baseline$pay_sanitary___5))*100
table(baseline$pay_sanitary___5)
prop.table(table(baseline$affordable_saniary))*100
table(baseline$affordable_saniary)
prop.table(table(baseline$pads_disposable___5))*100
table(baseline$pads_disposable___5)
prop.table(table(baseline$pads_washable___5))*100
table(baseline$pads_washable___5)
prop.table(table(baseline$tampons___5))*100
table(baseline$tampons___5)
prop.table(table(baseline$cloth_rags___5))*100
table(baseline$cloth_rags___5)
prop.table(table(baseline$panty_liner___5))*100
table(baseline$panty_liner___5)
prop.table(table(baseline$paper_sanitary___5))*100
table(baseline$paper_sanitary___5)
prop.table(table(baseline$paper_tempon___5))*100
table(baseline$paper_tempon___5)
prop.table(table(baseline$newspaper___5))*100
table(baseline$newspaper___5)
prop.table(table(baseline$cotton_wool___5))*100
table(baseline$cotton_wool___5)
prop.table(table(baseline$sponge___5))*100
table(baseline$sponge___5)
prop.table(table(baseline$mcup___5))*100
table(baseline$mcup___5)
prop.table(table(baseline$mcup___3))*100
table(baseline$mcup___3)
prop.table(table(baseline$mcup___1))*100
table(baseline$mcup___1)
prop.table(table(baseline$why_trymcup___6))*100
table(baseline$why_trymcup___6)

#table 1 by mcup used
base_sub1 <- base %>% select(c("unique_id", "residence", "watersource", "toi_type_hme___1", "toi_type_hme___2", "toi_type_hme___3", "lev_edu", "collegefees", "rel_status", "flag_usedmcup"))
lapply(names(base_sub1)[-1],function(x) CrossTable(base_sub1[,x],base_sub1[,"flag_usedmcup"],format="SAS",prop.chisq=FALSE,digits=2))

#table 2 by mcup used
base_sub2 <- base %>% select(c("unique_id", "pay_sanitary___1", "pay_sanitary___2", "pay_sanitary___3", "pay_sanitary___5", "affordable_saniary", 
                               "pads_disposable___5", "pads_washable___5", "tampons___5", "cloth_rags___5", "panty_liner___5", "paper_sanitary___5",
                               "paper_tempon___5", "newspaper___5", "cotton_wool___5", "sponge___5", "mcup___5", "mcup___3", "mcup___1", 
                               "why_trymcup___1", "why_trymcup___2", "why_trymcup___3", "why_trymcup___4", "why_trymcup___5", "why_trymcup___6", "flag_usedmcup"))
lapply(names(base_sub2)[-1],function(x) CrossTable(base_sub2[,x],base_sub2[,"flag_usedmcup"],format="SAS",prop.chisq=FALSE,digits=2))

#table 3
mo1_used_all <- mo1 %>% subset(mo1$mcup_used==1)
mo6_used_all <- mo6 %>% subset(mo6$mcup_used==1)
mo12_used_all <- mo12 %>% subset(mo12$mcup_used==1)

prop.table(table(mo6_used_all$first_insert))*100
table(mo6_used_all$first_insert)
prop.table(table(mo6_used_all$insert_improve))*100
table(mo6_used_all$insert_improve)
prop.table(table(mo6_used_all$easy_remove))*100
table(mo6_used_all$easy_remove)
prop.table(table(mo6_used_all$remove_improve))*100
table(mo6_used_all$remove_improve)
prop.table(table(mo1_used_all$insertion_problems))*100
table(mo1_used_all$insertion_problems)
prop.table(table(mo6_used_all$removal_problems))*100
table(mo6_used_all$removal_problems)

prop.table(table(mo1$mcup_used))*100
table(mo1$mcup_used)
prop.table(table(mo1$reason_notused))*100
table(mo1$reason_notused)

#table 4
prop.table(table(mo12_used_all$entire_period))*100
table(mo12_used_all$entire_period)
prop.table(table(mo12_used_all$whynotentire))*100
table(mo12_used_all$whynotentire)
prop.table(table(mo12_used_all$anyother_sanitary))*100
table(mo12_used_all$anyother_sanitary)
prop.table(table(mo12_used_all$prefer_cup))*100
table(mo12_used_all$prefer_cup)
prop.table(table(mo12_used_all$cont_cup))*100
table(mo12_used_all$cont_cup)


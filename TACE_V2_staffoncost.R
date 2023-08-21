# MN Oncology porject - TACE



setwd("C:/Users/senanay7/OneDrive - Queensland University of Technology/AusHSI work/00_AusHSI projects/2021_SIRT Oncology/R Analysis")


# Library
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)
library(stringi)
library(janitor)




# @@@@ Get the data sets @@@@
TACE_data = read.delim("TACE_data.csv", sep = ",") %>% clean_names()






##############################################################
############ Demographic characteristics #####################
##############################################################

TACE_demo = TACE_data %>%
  select(1: 13) %>%
  filter(redcap_repeat_instrument == "demo")



# Age
TACE_demo %>%
  summarise(age_mean = mean(age),
            sd = sd(age))


# Gender
TACE_demo %>%
  filter(sex == 1) %>%
  summarise(num = n(),
            per = num/86) # Change the number


# primary_site
TACE_demo$primary_site = as.factor(TACE_demo$primary_site)

TACE_demo %>%
  group_by(primary_site) %>%
  summarise(num = n(),
            per = num/86) # Change the number


# secondary_site
TACE_demo$secondary_site = as.factor(TACE_demo$secondary_site)

TACE_demo %>%
  group_by(secondary_site) %>%
  summarise(num = n(),
            per = num/86) # Change the number


# albi_score
TACE_demo$albi_score = as.factor(TACE_demo$albi_score)

TACE_demo %>%
  group_by(albi_score) %>%
  summarise(num = n(),
            per = num/86) # Change the number


# albi_score_at_tace
TACE_demo$albi_score_at_tace = as.factor(TACE_demo$albi_score_at_tace)

TACE_demo %>%
  group_by(albi_score_at_tace) %>%
  summarise(num = n(),
            per = num/86) # Change the number




##############################################################
################## admission_details  ########################
##############################################################

TACE_admin = TACE_data %>%
  select(1:4, admin_reason : wau_received) %>%
  filter(redcap_repeat_instrument == "admission_details")


# admin_reason



#  days
TACE_admin$days = as.factor(TACE_admin$days)

TACE_admin %>%
  group_by(days) %>%
  summarise(num = n(),
            per = num/86) # Change the number

TACE_admin$days = as.double(TACE_admin$days)
TACE_admin %>%
  summarise(ave = mean(days),
            sd = sd(days))


# ar_drg
TACE_admin$ar_drg = as.factor(TACE_admin$ar_drg)

TACE_admin %>%
  group_by(ar_drg) %>%
  summarise(num = n(),
            per = num/86) # Change the number


# wau
#### Boot strapping - mean & 95% CI


set.seed(13579)
n = 86
B = 100000

Boot.tot.wau.1 <- matrix(sample(TACE_admin$wau, size= B*n, 
                                replace=TRUE), ncol=B, nrow=n)

dim(Boot.tot.wau.1)
Boot.tot.wau.1[1:5,1:5]
Boot.tot.wau.1.Means_1 <- colMeans(Boot.tot.wau.1)
length(Boot.tot.wau.1.Means_1)
Boot.tot.wau.1.Means_1[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

tot_cost_1_L = quantile(Boot.tot.wau.1.Means_1, prob=0.025)
tot_cost_1_M = quantile(Boot.tot.wau.1.Means_1, prob=0.5)
tot_cost_1_U = quantile(Boot.tot.wau.1.Means_1, prob=0.975)

wau_total_cost = matrix(
  c(-1, tot_cost_1_L, tot_cost_1_M, tot_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(wau_total_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(wau_total_cost)




TACE_admin$wau = as.factor(TACE_admin$wau)

TACE_admin %>%
  group_by(wau) %>%
  summarise(num = n(),
            per = num/86) # Change the number

TACE_admin$wau = as.double(TACE_admin$wau)

TACE_admin %>%
  summarise(mean = mean(wau),
            sd = sd(wau))



# admit_ward vs wau_received
TACE_admin$admit_ward = as.factor(TACE_admin$admit_ward)
TACE_admin$wau_received = as.factor(TACE_admin$wau_received)

levels(TACE_admin$admit_ward)
levels(TACE_admin$wau_received)


Wau_fun = function(v1, v2) {
  TACE_admin %>%
    filter(admit_ward == v1) %>%
    group_by(wau_received) %>%
    summarise(num = n(),
              per = num/v2) # Change the number
  
  
}

Wau_fun (v1 = "23 HR", v2 = 19)
Wau_fun (v1 = "23HR", v2 = 1)
Wau_fun (v1 = "4C", v2 = 3)
Wau_fun (v1 = "4E", v2 = 47)
Wau_fun (v1 = "5D", v2 = 7)
Wau_fun (v1 = "7AN", v2 = 1)
Wau_fun (v1 = "8BS", v2 = 1)
Wau_fun (v1 = "8BW", v2 = 1)
Wau_fun (v1 = "9AN", v2 = 2)
Wau_fun (v1 = "9AS", v2 = 2)
Wau_fun (v1 = "PAH 11", v2 = 1)
Wau_fun (v1 = "XRAY", v2 = 1)



##############################################################
############################## proms  ########################
##############################################################

TACE_proms = TACE_data %>%
  select(1:4, proms_time : proms_vas) %>%
  filter(!is.na(proms_time))


write.csv(TACE_proms, "TACE_proms.csv")

TACE_proms = read.delim("TACE_proms_1.csv", sep = ",") %>% clean_names() # added the utlity score

TACE_proms$proms_vas = as.double(TACE_proms$proms_vas)


TACE_proms %>%
  filter(proms_time == 1 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas),
            num = n())

TACE_proms %>%
  filter(proms_time == 5 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas),
            num = n())



##############################################################
############################## TACE procedure  ###############
##############################################################

TACE_proced = TACE_data %>%
  select(1:4, tace_procedure : tace_post_bed) %>%
  filter(redcap_repeat_instrument == "tace") %>%
  mutate(tace_out_time_1 = tace_out_time)


TACE_proced = separate(TACE_proced, col = tace_out_time_1, into = c("hours", "minutes")) # Diff time produce negative values as post time is next day

TACE_proced$hours = as.numeric(TACE_proced$hours)
TACE_proced$minutes = as.numeric(TACE_proced$minutes)

TACE_proced$tace_out_time_2 = make_datetime(year = 0000, month = 01, day = 02, hour = TACE_proced$hours, min = TACE_proced$minutes) # Created a separate column with the same time next day


TACE_proced$tace_in_room = parse_date_time(TACE_proced$tace_in_room, orders = c("hm")) # Procedure time
TACE_proced$tace_out_time = parse_date_time(TACE_proced$tace_out_time, orders = c("hm")) 

TACE_proced$tace_start_reco = parse_date_time(TACE_proced$tace_start_reco, orders = c("hm")) # Recovery time
TACE_proced$tace_end_recov = parse_date_time(TACE_proced$tace_end_recov, orders = c("hm"))



# time difference - Procedure
TACE_proced$timediff = difftime(TACE_proced$tace_out_time, TACE_proced$tace_in_room, units = "hours") # this has negative time points


TACE_proced = TACE_proced %>%
  mutate(tace_out_time_3 = if_else (timediff > 0, TACE_proced$tace_out_time, TACE_proced$tace_out_time_2))

TACE_proced$timediff_procedure = difftime(TACE_proced$tace_out_time_3, TACE_proced$tace_in_room, units = "hours")



# time difference - recovery
TACE_proced$timediff_recovery = difftime(TACE_proced$tace_end_recov, TACE_proced$tace_start_reco, units = "hours") # this has negative time points


# Calculating average time

TACE_proced %>%
  filter(tace_procedure == 1) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))

TACE_proced %>%
  filter(tace_procedure == 2) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))

TACE_proced %>%
  filter(tace_procedure == 3) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))


TACE_proced %>% # Recovery time
  filter(tace_procedure == 2) %>%
  summarise(mean = mean(timediff_recovery),
            median = median(timediff_recovery),
            sd = sd(timediff_recovery))


# Recovery place
TACE_proced %>%
  filter(tace_procedure == 2) %>%
  group_by(tace_recovery_place) %>%
  summarise(num = n(),
            ave = num/86)



##############################################################
############################## TACE imaging  ###############
##############################################################

TACE_imaging = TACE_data %>%
  select(1:4, tace_procedure, tace_imaging_text : tace_other_2) %>%
  filter(redcap_repeat_instrument == "tace")


TACE_imagindg_cost = read.delim("Imaging_cost_tace.csv", sep = ",") %>% clean_names() # Imaging cost


TACE_imaging_1 = TACE_imaging %>%
  select(1, 5, 7:17)


TACE_imaging_2 = gather(TACE_imaging_1, key = imaging_method, value = number, -1, -2)

TACE_imaging_2$number = as.double(TACE_imaging_2$number)

TACE_imaging_2 = TACE_imaging_2 %>%
  mutate(number_1 = if_else (is.na(number), 0, TACE_imaging_2$number))


TACE_imaging_2 = TACE_imaging_2 %>%
  mutate(num_used = if_else(number_1 > 0, 1, 0))


# Pre-procedure
TACE_imaging_2 %>%
  filter(tace_procedure == 3) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/86)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))


# Merge with cost variable
TACE_imaging_3 = merge(x = TACE_imaging_2, y = TACE_imagindg_cost, by = "imaging_method", all = TRUE)



TACE_imaging_3 = TACE_imaging_3 %>%
  mutate(total_cost = number_1 * cost)

TACE_imaging_3 %>%
  filter(tace_procedure == 1 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in tace_procedure == 1 --> 84
TACE_imaging_3 %>%
  filter(tace_procedure == 1) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 84)


TACE_imaging_3 %>%
  filter(tace_procedure == 2 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in tace_procedure == 1 --> 83
TACE_imaging_3 %>%
  filter(tace_procedure == 2) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 83)


TACE_imaging_3 %>%
  filter(tace_procedure == 3 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in tace_procedure == 1 --> 6
TACE_imaging_3 %>%
  filter(tace_procedure == 3) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 6)



#### Boot strapping

## Total cost
TACE_imaging_4 = aggregate(TACE_imaging_3$total_cost, by = list(TACE_imaging_3$record_id), FUN = sum)


colnames(TACE_imaging_4) = c("record_id", "TACE_imaging_cost")


TACE_imaging_4 %>%
  summarise(sum = sum(TACE_imaging_cost))


## -- Boot strapping -- ##
set.seed(13579)
n = 86
B = 100000

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.tot.imaging_cost.1 <- matrix(sample(TACE_imaging_4$TACE_imaging_cost, size= B*n, 
                                replace=TRUE), ncol=B, nrow=n)


# check those
dim(Boot.tot.imaging_cost.1)


# check to make sure they are not empty!
Boot.tot.imaging_cost.1[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.tot.imaging_cost.1.Means_1 <- colMeans(Boot.tot.imaging_cost.1)

# check that
length(Boot.tot.imaging_cost.1.Means_1)

# and, look at the first 10 diff in means
Boot.tot.imaging_cost.1.Means_1[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
tot_cost_1_L = quantile(Boot.tot.imaging_cost.1.Means_1, prob=0.025)
tot_cost_1_M = quantile(Boot.tot.imaging_cost.1.Means_1, prob=0.5)
tot_cost_1_U = quantile(Boot.tot.imaging_cost.1.Means_1, prob=0.975)


Imaging_total_cost = matrix(
  c(-1, tot_cost_1_L, tot_cost_1_M, tot_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(Imaging_total_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(Imaging_total_cost)



## Pre-procedure
TACE_imaging_pre = TACE_imaging_3 %>%
  filter(tace_procedure == 1) 

TACE_imaging_pre = aggregate(TACE_imaging_pre$total_cost, by = list(TACE_imaging_pre$record_id), FUN = sum)

colnames(TACE_imaging_pre) = c("record_id", "cost_pre_imaging")


TACE_imaging_pre %>%
  summarise(sum = sum(cost_pre_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.pre.imaging_cost.1 <- matrix(sample(TACE_imaging_pre$cost_pre_imaging, size= B*n, 
                                 replace=TRUE), ncol=B, nrow=n)


# check those
dim(Boot.pre.imaging_cost.1)


# check to make sure they are not empty!
Boot.pre.imaging_cost.1[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.pre.imaging_cost.1.Means_1 <- colMeans(Boot.pre.imaging_cost.1)

# check that
length(Boot.pre.imaging_cost.1.Means_1)

# and, look at the first 10 diff in means
Boot.pre.imaging_cost.1.Means_1[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
pre_cost_1_L = quantile(Boot.pre.imaging_cost.1.Means_1, prob=0.025)
pre_cost_1_M = quantile(Boot.pre.imaging_cost.1.Means_1, prob=0.5)
pre_cost_1_U = quantile(Boot.pre.imaging_cost.1.Means_1, prob=0.975)


Imaging_pre_cost = matrix(
  c(-1, pre_cost_1_L, pre_cost_1_M, pre_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(Imaging_pre_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(Imaging_pre_cost)


## Intra-procedure
TACE_imaging_intra = TACE_imaging_3 %>%
  filter(tace_procedure == 2) 

TACE_imaging_intra = aggregate(TACE_imaging_intra$total_cost, by = list(TACE_imaging_intra$record_id), FUN = sum)

colnames(TACE_imaging_intra) = c("record_id", "cost_intra_imaging")

TACE_imaging_intra %>%
  summarise(sum = sum(cost_intra_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.intra.imagin_cost.1 <- matrix(sample(TACE_imaging_intra$cost_intra_imaging, size= B*n, 
                                 replace=TRUE), ncol=B, nrow=n)


# check those
dim(Boot.intra.imagin_cost.1)


# check to make sure they are not empty!
Boot.intra.imagin_cost.1[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.intra.imagin_cost.1.Means_1 <- colMeans(Boot.intra.imagin_cost.1)

# check that
length(Boot.intra.imagin_cost.1.Means_1)

# and, look at the first 10 diff in means
Boot.intra.imagin_cost.1.Means_1[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
intra_cost_1_L = quantile(Boot.intra.imagin_cost.1.Means_1, prob=0.025)
intra_cost_1_M = quantile(Boot.intra.imagin_cost.1.Means_1, prob=0.5)
intra_cost_1_U = quantile(Boot.intra.imagin_cost.1.Means_1, prob=0.975)


Imaging_intra_cost = matrix(
  c(-1, intra_cost_1_L, intra_cost_1_M, intra_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(Imaging_intra_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(Imaging_intra_cost)



## Post-procedure
TACE_imaging_post = TACE_imaging_3 %>%
  filter(tace_procedure == 3) 

TACE_imaging_post = aggregate(TACE_imaging_post$total_cost, by = list(TACE_imaging_post$record_id), FUN = sum)

colnames(TACE_imaging_post) = c("record_id", "cost_post_imaging")


TACE_imaging_post %>%
  summarise(sum = sum(cost_post_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.post.imaging_cost.1 <- matrix(sample(TACE_imaging_post$cost_post_imaging, size= B*n, 
                                   replace=TRUE), ncol=B, nrow=n)


# check those
dim(Boot.post.imaging_cost.1)


# check to make sure they are not empty!
Boot.post.imaging_cost.1[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.post.imaging_cost.1.Means_1 <- colMeans(Boot.post.imaging_cost.1)

# check that
length(Boot.post.imaging_cost.1.Means_1)

# and, look at the first 10 diff in means
Boot.post.imaging_cost.1.Means_1[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
post_cost_1_L = quantile(Boot.post.imaging_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.post.imaging_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.post.imaging_cost.1.Means_1, prob=0.975)


Imaging_post_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(Imaging_post_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(Imaging_post_cost)



##############################################################
################## Consumables and medicine  #################
##############################################################

TACE_consumableMed_cost = read.delim("TACE_consumable_cost.csv", sep = ",") %>% clean_names()


########### Pre- procedure #################

TACE_pre_ConMed = TACE_data %>%
  filter(tace_consu_pre == 1) %>%
  select(1, 3, tace_pre_bd_cannula : tace_pre_freestyle)


TACE_pre_ConMed_1 = gather(TACE_pre_ConMed, key = item_name, value = number, -1, -2)

TACE_pre_ConMed_1$number = as.double(TACE_pre_ConMed_1$number)

TACE_pre_ConMed_2 = TACE_pre_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, TACE_pre_ConMed_1$number))

TACE_pre_ConMed_2 = TACE_pre_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

TACE_pre_ConMed_2$item_name = as.factor(TACE_pre_ConMed_2$item_name)



# Merge with cost variable
TACE_pre_ConMed_2 = merge(x = TACE_pre_ConMed_2, y = TACE_consumableMed_cost, by = "item_name", all = TRUE)


TACE_pre_ConMed_2 = TACE_pre_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)


# Aggregate with item
TACE_pre_ConMed_2a = TACE_pre_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(num_items = sum(number_ConMed),
            item_cost = median(cost),
            tot_cot = sum(tot_cost))

write.csv(TACE_pre_ConMed_2a, file = "Consumables/TACE_Pre_Con.csv")




# Aggregate cost per item
TACE_pre_ConMed_4 = aggregate(TACE_pre_ConMed_2$tot_cost, by = list(TACE_pre_ConMed_2$item_name), FUN = sum)
colnames(TACE_pre_ConMed_4) = c("item_name", "cost_pre_conMed")

TACE_pre_ConMed_4 <- TACE_pre_ConMed_4[order(-TACE_pre_ConMed_4$cost_pre_conMed),]



# Aggregate cost per person
TACE_pre_ConMed_3 = aggregate(TACE_pre_ConMed_2$tot_cost, by = list(TACE_pre_ConMed_2$record_id), FUN = sum)

colnames(TACE_pre_ConMed_3) = c("record_id", "cost_pre_conMed")

TACE_pre_ConMed_3 %>%
  summarise(sum = sum(cost_pre_conMed))


## -- Boot strapping -- ##


Boot.pre.ConMed_cost.1 <- matrix(sample(TACE_pre_ConMed_3$cost_pre_conMed, size= B*n, 
                                  replace=TRUE), ncol=B, nrow=n)


dim(Boot.pre.ConMed_cost.1)

Boot.pre.ConMed_cost.1[1:5,1:5]

Boot.pre.ConMed_cost.1.Means_1 <- colMeans(Boot.pre.ConMed_cost.1)

length(Boot.pre.ConMed_cost.1.Means_1)

Boot.pre.ConMed_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.pre.ConMed_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.pre.ConMed_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.pre.ConMed_cost.1.Means_1, prob=0.975)


pre.ConMed_cost_tab = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(pre.ConMed_cost_tab) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(pre.ConMed_cost_tab)




########### Intra- procedure #################

TACE_intra_ConMed = TACE_data %>%
  filter(tace_consu_intra == 1) %>%
  select(1, 3, tace_intra_absorbable_smal : tace_intra_vectorio_lipiod, tace_intra_fentanyl_100mcg : tace_intra_ondansetron)


TACE_intra_ConMed_1 = gather(TACE_intra_ConMed, key = item_name, value = number, -1, -2)

TACE_intra_ConMed_1$number = as.double(TACE_intra_ConMed_1$number)

TACE_intra_ConMed_2 = TACE_intra_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, TACE_intra_ConMed_1$number))

TACE_intra_ConMed_2 = TACE_intra_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

TACE_intra_ConMed_2$item_name = as.factor(TACE_intra_ConMed_2$item_name)



# Merge with cost variable
TACE_intra_ConMed_2 = merge(x = TACE_intra_ConMed_2, y = TACE_consumableMed_cost, by = "item_name", all = TRUE)

TACE_intra_ConMed_2 = TACE_intra_ConMed_2 %>%  # Remove pre
  filter(!is.na(record_id))

TACE_intra_ConMed_2 = TACE_intra_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)


# Aggregate with item
TACE_intra_ConMed_2a = TACE_intra_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(num_items = sum(number_ConMed),
            item_cost = median(cost),
            tot_cot = sum(tot_cost))

write.csv(TACE_intra_ConMed_2a, file = "Consumables/TACE_intra_Con.csv")



# Aggregate cost per item
TACE_intra_ConMed_4 = aggregate(TACE_intra_ConMed_2$tot_cost, by = list(TACE_intra_ConMed_2$item_name), FUN = sum)
colnames(TACE_intra_ConMed_4) = c("item_name", "cost_intra_conMed")

TACE_intra_ConMed_4 <- TACE_intra_ConMed_4[order(-TACE_intra_ConMed_4$cost_intra_conMed),]

TACE_intra_ConMed_4 = TACE_intra_ConMed_4 %>%
  mutate(per_pt = cost_intra_conMed / 86) 

TACE_intra_ConMed_4 %>%
  summarise(sum = sum(per_pt))



# Aggregate cost per person
TACE_intra_ConMed_3 = aggregate(TACE_intra_ConMed_2$tot_cost, by = list(TACE_intra_ConMed_2$record_id), FUN = sum)

colnames(TACE_intra_ConMed_3) = c("record_id", "cost_intra_conMed")

TACE_intra_ConMed_3 %>%
  summarise(sum = sum(cost_intra_conMed))


## -- Boot strapping -- ##

Boot.intra.ConMed_cost.1 <- matrix(sample(TACE_intra_ConMed_3$cost_intra_conMed, size= B*n, 
                                        replace=TRUE), ncol=B, nrow=n)


dim(Boot.intra.ConMed_cost.1)

Boot.intra.ConMed_cost.1[1:5,1:5]

Boot.intra.ConMed_cost.1.Means_1 <- colMeans(Boot.intra.ConMed_cost.1)

length(Boot.intra.ConMed_cost.1.Means_1)

Boot.intra.ConMed_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.intra.ConMed_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.intra.ConMed_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.intra.ConMed_cost.1.Means_1, prob=0.975)


intra.ConMed_cost_tab = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(intra.ConMed_cost_tab) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(intra.ConMed_cost_tab)





########### Post- procedure #################

TACE_post_ConMed = TACE_data %>%
  filter(tace_consu_post == 1) %>%
  select(1, 3, tace_post_iv_dot : tace_post_freestyle)


TACE_post_ConMed_1 = gather(TACE_post_ConMed, key = item_name, value = number, -1, -2)

TACE_post_ConMed_1$number = as.double(TACE_post_ConMed_1$number)

TACE_post_ConMed_2 = TACE_post_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, TACE_post_ConMed_1$number))

TACE_post_ConMed_2 = TACE_post_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

TACE_post_ConMed_2$item_name = as.factor(TACE_post_ConMed_2$item_name)



# Merge with cost variable
TACE_post_ConMed_2 = merge(x = TACE_post_ConMed_2, y = TACE_consumableMed_cost, by = "item_name", all = TRUE)

TACE_post_ConMed_2 = TACE_post_ConMed_2 %>%  # Remove pre & intra
  filter(!is.na(record_id))

TACE_post_ConMed_2 = TACE_post_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)


# Aggregate with item
TACE_post_ConMed_2a = TACE_post_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(item_cost = median(cost),
            num_items = sum(number_ConMed),
            tot_cot = sum(tot_cost))
write.csv(TACE_post_ConMed_2a, file = "Consumables/TACE_postCon.csv")



# Aggregate cost per person
TACE_post_ConMed_3 = aggregate(TACE_post_ConMed_2$tot_cost, by = list(TACE_post_ConMed_2$record_id), FUN = sum)

colnames(TACE_post_ConMed_3) = c("record_id", "cost_post_conMed")

TACE_post_ConMed_3 %>%
  summarise(sum = sum(cost_post_conMed))


## -- Boot strapping -- ##


Boot.post.ConMed_cost.1 <- matrix(sample(TACE_post_ConMed_3$cost_post_conMed, size= B*n, 
                                          replace=TRUE), ncol=B, nrow=n)


dim(Boot.post.ConMed_cost.1)

Boot.post.ConMed_cost.1[1:5,1:5]

Boot.post.ConMed_cost.1.Means_1 <- colMeans(Boot.post.ConMed_cost.1)

length(Boot.post.ConMed_cost.1.Means_1)

Boot.post.ConMed_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.post.ConMed_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.post.ConMed_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.post.ConMed_cost.1.Means_1, prob=0.975)


post.ConMed_cost_tab = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(post.ConMed_cost_tab) = c("Year", "L_CI","Mean","U_CI")
post.ConMed_cost_tab = as.data.frame(post.ConMed_cost_tab)




## Combine pre, intra & post data sets
TACE_combine_ConMed = merge(x = TACE_pre_ConMed_3, y = TACE_intra_ConMed_3, by = "record_id", all = TRUE)
TACE_combine_ConMed = merge(x = TACE_combine_ConMed, y = TACE_post_ConMed_3, by = "record_id", all = TRUE)

TACE_combine_ConMed[is.na(TACE_combine_ConMed)]<-0

TACE_combine_ConMed = TACE_combine_ConMed %>%
  mutate(total_ConMed = cost_pre_conMed + cost_intra_conMed +cost_post_conMed)


TACE_combine_ConMed %>%
  summarise(sum = sum(total_ConMed))


TACE_combine_ConMed = TACE_combine_ConMed %>%
  select(1, 5)



## -- Boot strapping -- ##


Boot.combine.ConMed_cost.1 <- matrix(sample(TACE_combine_ConMed$total_ConMed, size= B*n, 
                                         replace=TRUE), ncol=B, nrow=n)


dim(Boot.combine.ConMed_cost.1)

Boot.combine.ConMed_cost.1[1:5,1:5]

Boot.combine.ConMed_cost.1.Means_1 <- colMeans(Boot.combine.ConMed_cost.1)

length(Boot.combine.ConMed_cost.1.Means_1)

Boot.combine.ConMed_cost.1.Means_1[1:10]

combine_cost_1_L = quantile(Boot.combine.ConMed_cost.1.Means_1, prob=0.025)
combine_cost_1_M = quantile(Boot.combine.ConMed_cost.1.Means_1, prob=0.5)
combine_cost_1_U = quantile(Boot.combine.ConMed_cost.1.Means_1, prob=0.975)


combine.ConMed_cost_tab = matrix(
  c(-1, combine_cost_1_L, combine_cost_1_M, combine_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(combine.ConMed_cost_tab) = c("Year", "L_CI","Mean","U_CI")
combine.ConMed_cost_tab = as.data.frame(combine.ConMed_cost_tab)




##############################################################
################## Staff cost   ##############################
##############################################################


TACE_staff = TACE_data %>%
  select(1, 2, 3,  tace_staff_al: tace_staff_mab_nm_10_hours) %>%
  filter(redcap_repeat_instrument == "tace")


TACE_pre_staff = TACE_staff %>%
  filter(redcap_repeat_instance == 1)
TACE_intra_staff = TACE_staff %>%
  filter(redcap_repeat_instance == 2)  
TACE_post_staff = TACE_staff %>%
  filter(redcap_repeat_instance == 3)

TACE_pre_staff$tace_staff_al_01_other = as.factor(TACE_pre_staff$tace_staff_al_01_other)




# To check if number are more than 10 in a staff cat
TACE_pre_staff_check = TACE_pre_staff %>%
  select(1, tace_staff_al, tace_staff_ass_n, tace_staff_enr_n, tace_staff_reg_n, tace_staff_cln_n, tace_staff_anum, tace_staff_case_m, 
         tace_staff_nur_um, tace_staff_in_res, tace_staff_registrar, tace_staff_fellow, tace_staff_consultant, tace_staff_radio,
         tace_staff_ultra, tace_staff_nuc_mt, tace_staff_nm_ph, tace_staff_mab_nm)

TACE_intra_staff_check = TACE_intra_staff %>%
  select(1, tace_staff_al, tace_staff_ass_n, tace_staff_enr_n, tace_staff_reg_n, tace_staff_cln_n, tace_staff_anum, tace_staff_case_m, 
         tace_staff_nur_um, tace_staff_in_res, tace_staff_registrar, tace_staff_fellow, tace_staff_consultant, tace_staff_radio,
         tace_staff_ultra, tace_staff_nuc_mt, tace_staff_nm_ph, tace_staff_mab_nm)

TACE_post_staff_check = TACE_post_staff %>%
  select(1, tace_staff_al, tace_staff_ass_n, tace_staff_enr_n, tace_staff_reg_n, tace_staff_cln_n, tace_staff_anum, tace_staff_case_m, 
         tace_staff_nur_um, tace_staff_in_res, tace_staff_registrar, tace_staff_fellow, tace_staff_consultant, tace_staff_radio,
         tace_staff_ultra, tace_staff_nuc_mt, tace_staff_nm_ph, tace_staff_mab_nm)



Administrative_labour	        = 40.43555556*1.25 #
Assistant_in_nursing	        = 34.38583333*1.25 #
Enrolled_nurse	              = 38.35694444*1.25 #
Registered_nurse	            = 47.09972222*1.25 #
Clinical_nurse	              = 55.74361111*1.25 #
Case_manager	                = 55.74361111*1.25 #
Assistant_nurse_unit_manager	= 60.51555556*1.25 #
Nurse_unit_manager	          = 68.93944444*1.25 #
Anaesthetic_Technician 	      = 38.35694444*1.25
Radiographer	                = 66.34666667*1.25 #
Ultrasonographer	            = 66.34666667*1.25 #
Nuc_Med_Tech	                = 66.34666667*1.25 #
Labour_nuclear_medicine	      = 66.34666667*1.25 #
Oncology_Pharmacy	            = 66.34666667*1.25
Intern_or_Resident	          = 48.69833333*1.25 #
Registrar	                    = 69.77138889*1.25 #
Fellow	                      = 96.49916667*1.25 # 
Consultant	                  = 125.4936111*1.25 #
NM_physicist	                = 125.4936111*1.25 #
Interventional_Radiologist	  = 125.4936111*1.25


TACE_pre_staff[is.na(TACE_pre_staff)]<-0
TACE_intra_staff[is.na(TACE_intra_staff)]<-0
TACE_post_staff[is.na(TACE_post_staff)]<-0


# Administrative_labour
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_al))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_al))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_al))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Admin_labour_cost = (tace_staff_al_01_hours + tace_staff_al_02_hours + tace_staff_al_03_hours + tace_staff_al_04_hours + tace_staff_al_05_hours +
           tace_staff_al_06_hours + tace_staff_al_07_hours + tace_staff_al_08_hours + tace_staff_al_09_hours + tace_staff_al_10_hours)*Administrative_labour)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Admin_labour_cost = (tace_staff_al_01_hours + tace_staff_al_02_hours + tace_staff_al_03_hours + tace_staff_al_04_hours + tace_staff_al_05_hours +
                                 tace_staff_al_06_hours + tace_staff_al_07_hours + tace_staff_al_08_hours + tace_staff_al_09_hours + tace_staff_al_10_hours)*Administrative_labour)

TACE_post_staff = TACE_post_staff %>%
  mutate(Admin_labour_cost = (tace_staff_al_01_hours + tace_staff_al_02_hours + tace_staff_al_03_hours + tace_staff_al_04_hours + tace_staff_al_05_hours +
                                tace_staff_al_06_hours + tace_staff_al_07_hours + tace_staff_al_08_hours + tace_staff_al_09_hours + tace_staff_al_10_hours)*Administrative_labour)


TACE_pre_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Admin_labour_cost))


# Assistant_in_nursing
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_ass_n))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_ass_n))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_ass_n))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Assistant_in_nursing_cost = (tace_staff_ass_n_01_hours + tace_staff_ass_n_02_hours + tace_staff_ass_n_03_hours + tace_staff_ass_n_04_hours + tace_staff_ass_n_05_hours +
                                tace_staff_ass_n_06_hours + tace_staff_ass_n_07_hours + tace_staff_ass_n_08_hours + tace_staff_ass_n_09_hours + tace_staff_ass_n_10_hours)*Assistant_in_nursing)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Assistant_in_nursing_cost = (tace_staff_ass_n_01_hours + tace_staff_ass_n_02_hours + tace_staff_ass_n_03_hours + tace_staff_ass_n_04_hours + tace_staff_ass_n_05_hours +
                                        tace_staff_ass_n_06_hours + tace_staff_ass_n_07_hours + tace_staff_ass_n_08_hours + tace_staff_ass_n_09_hours + tace_staff_ass_n_10_hours)*Assistant_in_nursing)

TACE_post_staff = TACE_post_staff %>%
  mutate(Assistant_in_nursing_cost = (tace_staff_ass_n_01_hours + tace_staff_ass_n_02_hours + tace_staff_ass_n_03_hours + tace_staff_ass_n_04_hours + tace_staff_ass_n_05_hours +
                                        tace_staff_ass_n_06_hours + tace_staff_ass_n_07_hours + tace_staff_ass_n_08_hours + tace_staff_ass_n_09_hours + tace_staff_ass_n_10_hours)*Assistant_in_nursing)

TACE_pre_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))



# Enrolled_nurse
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_enr_n))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_enr_n))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_enr_n))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Enrolled_nurse_cost = (tace_staff_enr_n_01_hours + tace_staff_enr_n_02_hours + tace_staff_enr_n_03_hours + tace_staff_enr_n_04_hours + tace_staff_enr_n_05_hours +
                                        tace_staff_enr_n_06_hours + tace_staff_enr_n_07_hours + tace_staff_enr_n_08_hours + tace_staff_enr_n_09_hours + tace_staff_enr_n_10_hours)*Enrolled_nurse)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Enrolled_nurse_cost = (tace_staff_enr_n_01_hours + tace_staff_enr_n_02_hours + tace_staff_enr_n_03_hours + tace_staff_enr_n_04_hours + tace_staff_enr_n_05_hours +
                                        tace_staff_enr_n_06_hours + tace_staff_enr_n_07_hours + tace_staff_enr_n_08_hours + tace_staff_enr_n_09_hours + tace_staff_enr_n_10_hours)*Enrolled_nurse)

TACE_post_staff = TACE_post_staff %>%
  mutate(Enrolled_nurse_cost = (tace_staff_enr_n_01_hours + tace_staff_enr_n_02_hours + tace_staff_enr_n_03_hours + tace_staff_enr_n_04_hours + tace_staff_enr_n_05_hours +
                                        tace_staff_enr_n_06_hours + tace_staff_enr_n_07_hours + tace_staff_enr_n_08_hours + tace_staff_enr_n_09_hours + tace_staff_enr_n_10_hours)*Enrolled_nurse)

TACE_pre_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))


# Registered_nurse
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_reg_n))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_reg_n))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_reg_n))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Registered_nurse_cost = (tace_staff_reg_n_01_hours + tace_staff_reg_n_02_hours + tace_staff_reg_n_03_hours + tace_staff_reg_n_04_hours + tace_staff_reg_n_05_hours +
                                  tace_staff_reg_n_06_hours + tace_staff_reg_n_07_hours + tace_staff_reg_n_08_hours + tace_staff_reg_n_09_hours + tace_staff_reg_n_10_hours)*Registered_nurse)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Registered_nurse_cost = (tace_staff_reg_n_01_hours + tace_staff_reg_n_02_hours + tace_staff_reg_n_03_hours + tace_staff_reg_n_04_hours + tace_staff_reg_n_05_hours +
                                  tace_staff_reg_n_06_hours + tace_staff_reg_n_07_hours + tace_staff_reg_n_08_hours + tace_staff_reg_n_09_hours + tace_staff_reg_n_10_hours)*Registered_nurse)

TACE_post_staff = TACE_post_staff %>%
  mutate(Registered_nurse_cost = (tace_staff_reg_n_01_hours + tace_staff_reg_n_02_hours + tace_staff_reg_n_03_hours + tace_staff_reg_n_04_hours + tace_staff_reg_n_05_hours +
                                  tace_staff_reg_n_06_hours + tace_staff_reg_n_07_hours + tace_staff_reg_n_08_hours + tace_staff_reg_n_09_hours + tace_staff_reg_n_10_hours)*Registered_nurse)



TACE_pre_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))



# Clinical_nurse
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_cln_n))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_cln_n))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_cln_n))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Clinical_nurse_cost = (tace_staff_cln_n_01_hours + tace_staff_cln_n_02_hours + tace_staff_cln_n_03_hours + tace_staff_cln_n_04_hours + tace_staff_cln_n_05_hours +
                                    tace_staff_cln_n_06_hours + tace_staff_cln_n_07_hours + tace_staff_cln_n_08_hours + tace_staff_cln_n_09_hours + tace_staff_cln_n_10_hours)*Clinical_nurse)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Clinical_nurse_cost = (tace_staff_cln_n_01_hours + tace_staff_cln_n_02_hours + tace_staff_cln_n_03_hours + tace_staff_cln_n_04_hours + tace_staff_cln_n_05_hours +
                                    tace_staff_cln_n_06_hours + tace_staff_cln_n_07_hours + tace_staff_cln_n_08_hours + tace_staff_cln_n_09_hours + tace_staff_cln_n_10_hours)*Clinical_nurse)

TACE_post_staff = TACE_post_staff %>%
  mutate(Clinical_nurse_cost = (tace_staff_cln_n_01_hours + tace_staff_cln_n_02_hours + tace_staff_cln_n_03_hours + tace_staff_cln_n_04_hours + tace_staff_cln_n_05_hours +
                                    tace_staff_cln_n_06_hours + tace_staff_cln_n_07_hours + tace_staff_cln_n_08_hours + tace_staff_cln_n_09_hours + tace_staff_cln_n_10_hours)*Clinical_nurse)



TACE_pre_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))



# Assistant_nurse_unit_manager
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_anum))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_anum))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_anum))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (tace_staff_anum_01_hours + tace_staff_anum_02_hours + tace_staff_anum_03_hours + tace_staff_anum_04_hours + tace_staff_anum_05_hours +
                                  tace_staff_anum_06_hours + tace_staff_anum_07_hours + tace_staff_anum_08_hours + tace_staff_anum_09_hours + tace_staff_anum_10_hours)*Assistant_nurse_unit_manager)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (tace_staff_anum_01_hours + tace_staff_anum_02_hours + tace_staff_anum_03_hours + tace_staff_anum_04_hours + tace_staff_anum_05_hours +
                                                tace_staff_anum_06_hours + tace_staff_anum_07_hours + tace_staff_anum_08_hours + tace_staff_anum_09_hours + tace_staff_anum_10_hours)*Assistant_nurse_unit_manager)

TACE_post_staff = TACE_post_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (tace_staff_anum_01_hours + tace_staff_anum_02_hours + tace_staff_anum_03_hours + tace_staff_anum_04_hours + tace_staff_anum_05_hours +
                                                tace_staff_anum_06_hours + tace_staff_anum_07_hours + tace_staff_anum_08_hours + tace_staff_anum_09_hours + tace_staff_anum_10_hours)*Assistant_nurse_unit_manager)

TACE_pre_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))



# Case_manager
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_case_m))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_case_m))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_case_m))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Case_manager_cost = (tace_staff_case_m_01_hours + tace_staff_case_m_02_hours + tace_staff_case_m_03_hours + tace_staff_case_m_04_hours + tace_staff_case_m_05_hours +
                                                tace_staff_case_m_06_hours + tace_staff_case_m_07_hours + tace_staff_case_m_08_hours + tace_staff_case_m_09_hours + tace_staff_case_m_10_hours)*Case_manager)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Case_manager_cost = (tace_staff_case_m_01_hours + tace_staff_case_m_02_hours + tace_staff_case_m_03_hours + tace_staff_case_m_04_hours + tace_staff_case_m_05_hours +
                                                tace_staff_case_m_06_hours + tace_staff_case_m_07_hours + tace_staff_case_m_08_hours + tace_staff_case_m_09_hours + tace_staff_case_m_10_hours)*Case_manager)

TACE_post_staff = TACE_post_staff %>%
  mutate(Case_manager_cost = (tace_staff_case_m_01_hours + tace_staff_case_m_02_hours + tace_staff_case_m_03_hours + tace_staff_case_m_04_hours + tace_staff_case_m_05_hours +
                                                tace_staff_case_m_06_hours + tace_staff_case_m_07_hours + tace_staff_case_m_08_hours + tace_staff_case_m_09_hours + tace_staff_case_m_10_hours)*Case_manager)

TACE_pre_staff  %>%
  summarise(sum = sum(Case_manager_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Case_manager_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Case_manager_cost))


# Nurse_unit_manager
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_nur_um))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_nur_um))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_nur_um))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Nurse_unit_manager_cost = (tace_staff_nur_um_01_hours + tace_staff_nur_um_02_hours + tace_staff_nur_um_03_hours + tace_staff_nur_um_04_hours + tace_staff_nur_um_05_hours +
                                tace_staff_nur_um_06_hours + tace_staff_nur_um_07_hours + tace_staff_nur_um_08_hours + tace_staff_nur_um_09_hours + tace_staff_nur_um_10_hours)*Nurse_unit_manager)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Nurse_unit_manager_cost = (tace_staff_nur_um_01_hours + tace_staff_nur_um_02_hours + tace_staff_nur_um_03_hours + tace_staff_nur_um_04_hours + tace_staff_nur_um_05_hours +
                                tace_staff_nur_um_06_hours + tace_staff_nur_um_07_hours + tace_staff_nur_um_08_hours + tace_staff_nur_um_09_hours + tace_staff_nur_um_10_hours)*Nurse_unit_manager)

TACE_post_staff = TACE_post_staff %>%
  mutate(Nurse_unit_manager_cost = (tace_staff_nur_um_01_hours + tace_staff_nur_um_02_hours + tace_staff_nur_um_03_hours + tace_staff_nur_um_04_hours + tace_staff_nur_um_05_hours +
                                tace_staff_nur_um_06_hours + tace_staff_nur_um_07_hours + tace_staff_nur_um_08_hours + tace_staff_nur_um_09_hours + tace_staff_nur_um_10_hours)*Nurse_unit_manager)

TACE_pre_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))


# Intern_or_Resident
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_in_res))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_in_res))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_in_res))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Intern_or_Resident_cost = (tace_staff_in_res_01_hours + tace_staff_in_res_02_hours + tace_staff_in_res_03_hours + tace_staff_in_res_04_hours + tace_staff_in_res_05_hours +
                                      tace_staff_in_res_06_hours + tace_staff_in_res_07_hours + tace_staff_in_res_08_hours + tace_staff_in_res_09_hours + tace_staff_in_res_10_hours)*Intern_or_Resident)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Intern_or_Resident_cost = (tace_staff_in_res_01_hours + tace_staff_in_res_02_hours + tace_staff_in_res_03_hours + tace_staff_in_res_04_hours + tace_staff_in_res_05_hours +
                                      tace_staff_in_res_06_hours + tace_staff_in_res_07_hours + tace_staff_in_res_08_hours + tace_staff_in_res_09_hours + tace_staff_in_res_10_hours)*Intern_or_Resident)

TACE_post_staff = TACE_post_staff %>%
  mutate(Intern_or_Resident_cost = (tace_staff_in_res_01_hours + tace_staff_in_res_02_hours + tace_staff_in_res_03_hours + tace_staff_in_res_04_hours + tace_staff_in_res_05_hours +
                                      tace_staff_in_res_06_hours + tace_staff_in_res_07_hours + tace_staff_in_res_08_hours + tace_staff_in_res_09_hours + tace_staff_in_res_10_hours)*Intern_or_Resident)

TACE_pre_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))


# Registrar
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_registrar))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_registrar))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_registrar))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Registrar_cost = (tace_staff_registrar_01_hours + tace_staff_registrar_02_hours + tace_staff_registrar_03_hours + tace_staff_registrar_04_hours + tace_staff_registrar_05_hours +
                                      tace_staff_registrar_06_hours + tace_staff_registrar_07_hours + tace_staff_registrar_08_hours + tace_staff_registrar_09_hours + tace_staff_registrar_10_hours)*Registrar)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Registrar_cost = (tace_staff_registrar_01_hours + tace_staff_registrar_02_hours + tace_staff_registrar_03_hours + tace_staff_registrar_04_hours + tace_staff_registrar_05_hours +
                                      tace_staff_registrar_06_hours + tace_staff_registrar_07_hours + tace_staff_registrar_08_hours + tace_staff_registrar_09_hours + tace_staff_registrar_10_hours)*Registrar)

TACE_post_staff = TACE_post_staff %>%
  mutate(Registrar_cost = (tace_staff_registrar_01_hours + tace_staff_registrar_02_hours + tace_staff_registrar_03_hours + tace_staff_registrar_04_hours + tace_staff_registrar_05_hours +
                                      tace_staff_registrar_06_hours + tace_staff_registrar_07_hours + tace_staff_registrar_08_hours + tace_staff_registrar_09_hours + tace_staff_registrar_10_hours)*Registrar)

TACE_pre_staff  %>%
  summarise(sum = sum(Registrar_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Registrar_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Registrar_cost))



# Fellow
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_fellow))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_fellow))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_fellow))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Fellow_cost = (tace_staff_fellow_01_hours + tace_staff_fellow_02_hours + tace_staff_fellow_03_hours + tace_staff_fellow_04_hours + tace_staff_fellow_05_hours +
                             tace_staff_fellow_06_hours + tace_staff_fellow_07_hours + tace_staff_fellow_08_hours + tace_staff_fellow_09_hours + tace_staff_fellow_10_hours)*Fellow)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Fellow_cost = (tace_staff_fellow_01_hours + tace_staff_fellow_02_hours + tace_staff_fellow_03_hours + tace_staff_fellow_04_hours + tace_staff_fellow_05_hours +
                             tace_staff_fellow_06_hours + tace_staff_fellow_07_hours + tace_staff_fellow_08_hours + tace_staff_fellow_09_hours + tace_staff_fellow_10_hours)*Fellow)

TACE_post_staff = TACE_post_staff %>%
  mutate(Fellow_cost = (tace_staff_fellow_01_hours + tace_staff_fellow_02_hours + tace_staff_fellow_03_hours + tace_staff_fellow_04_hours + tace_staff_fellow_05_hours +
                             tace_staff_fellow_06_hours + tace_staff_fellow_07_hours + tace_staff_fellow_08_hours + tace_staff_fellow_09_hours + tace_staff_fellow_10_hours)*Fellow)

TACE_pre_staff  %>%
  summarise(sum = sum(Fellow_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Fellow_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Fellow_cost))


# Consultant
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_consultant))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_consultant))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_consultant))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Consultant_cost = (tace_staff_consultant_01_hours + tace_staff_consultant_02_hours + tace_staff_consultant_03_hours + tace_staff_consultant_04_hours + tace_staff_consultant_05_hours +
                          tace_staff_consultant_06_hours + tace_staff_consultant_07_hours + tace_staff_consultant_08_hours + tace_staff_consultant_09_hours + tace_staff_consultant_10_hours)*Consultant)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Consultant_cost = (tace_staff_consultant_01_hours + tace_staff_consultant_02_hours + tace_staff_consultant_03_hours + tace_staff_consultant_04_hours + tace_staff_consultant_05_hours +
                          tace_staff_consultant_06_hours + tace_staff_consultant_07_hours + tace_staff_consultant_08_hours + tace_staff_consultant_09_hours + tace_staff_consultant_10_hours)*Consultant)

TACE_post_staff = TACE_post_staff %>%
  mutate(Consultant_cost = (tace_staff_consultant_01_hours + tace_staff_consultant_02_hours + tace_staff_consultant_03_hours + tace_staff_consultant_04_hours + tace_staff_consultant_05_hours +
                          tace_staff_consultant_06_hours + tace_staff_consultant_07_hours + tace_staff_consultant_08_hours + tace_staff_consultant_09_hours + tace_staff_consultant_10_hours)*Consultant)

TACE_pre_staff  %>%
  summarise(sum = sum(Consultant_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Consultant_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Consultant_cost))



# Radiographer
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_radio))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_radio))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_radio))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Radiographer_cost = (tace_staff_radio_01_hours + tace_staff_radio_02_hours + tace_staff_radio_03_hours + tace_staff_radio_04_hours + tace_staff_radio_05_hours +
                              tace_staff_radio_06_hours + tace_staff_radio_07_hours + tace_staff_radio_08_hours + tace_staff_radio_09_hours + tace_staff_radio_10_hours)*Radiographer)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Radiographer_cost = (tace_staff_radio_01_hours + tace_staff_radio_02_hours + tace_staff_radio_03_hours + tace_staff_radio_04_hours + tace_staff_radio_05_hours +
                              tace_staff_radio_06_hours + tace_staff_radio_07_hours + tace_staff_radio_08_hours + tace_staff_radio_09_hours + tace_staff_radio_10_hours)*Radiographer)

TACE_post_staff = TACE_post_staff %>%
  mutate(Radiographer_cost = (tace_staff_radio_01_hours + tace_staff_radio_02_hours + tace_staff_radio_03_hours + tace_staff_radio_04_hours + tace_staff_radio_05_hours +
                              tace_staff_radio_06_hours + tace_staff_radio_07_hours + tace_staff_radio_08_hours + tace_staff_radio_09_hours + tace_staff_radio_10_hours)*Radiographer)

TACE_pre_staff  %>%
  summarise(sum = sum(Radiographer_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Radiographer_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Radiographer_cost))



# Ultrasonographer
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_ultra))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_ultra))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_ultra))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Ultrasonographer_cost = (tace_staff_ultra_01_hours + tace_staff_ultra_02_hours + tace_staff_ultra_03_hours + tace_staff_ultra_04_hours + tace_staff_ultra_05_hours +
                                tace_staff_ultra_06_hours + tace_staff_ultra_07_hours + tace_staff_ultra_08_hours + tace_staff_ultra_09_hours + tace_staff_ultra_10_hours)*Ultrasonographer)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Ultrasonographer_cost = (tace_staff_ultra_01_hours + tace_staff_ultra_02_hours + tace_staff_ultra_03_hours + tace_staff_ultra_04_hours + tace_staff_ultra_05_hours +
                                tace_staff_ultra_06_hours + tace_staff_ultra_07_hours + tace_staff_ultra_08_hours + tace_staff_ultra_09_hours + tace_staff_ultra_10_hours)*Ultrasonographer)

TACE_post_staff = TACE_post_staff %>%
  mutate(Ultrasonographer_cost = (tace_staff_ultra_01_hours + tace_staff_ultra_02_hours + tace_staff_ultra_03_hours + tace_staff_ultra_04_hours + tace_staff_ultra_05_hours +
                                tace_staff_ultra_06_hours + tace_staff_ultra_07_hours + tace_staff_ultra_08_hours + tace_staff_ultra_09_hours + tace_staff_ultra_10_hours)*Ultrasonographer)

TACE_pre_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))



# Nuc_Med_Tech
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_nuc_mt))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_nuc_mt))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_nuc_mt))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Nuc_Med_Tech_cost = (tace_staff_nuc_mt_01_hours + tace_staff_nuc_mt_02_hours + tace_staff_nuc_mt_03_hours + tace_staff_nuc_mt_04_hours + tace_staff_nuc_mt_05_hours +
                                    tace_staff_nuc_mt_06_hours + tace_staff_nuc_mt_07_hours + tace_staff_nuc_mt_08_hours + tace_staff_nuc_mt_09_hours + tace_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Nuc_Med_Tech_cost = (tace_staff_nuc_mt_01_hours + tace_staff_nuc_mt_02_hours + tace_staff_nuc_mt_03_hours + tace_staff_nuc_mt_04_hours + tace_staff_nuc_mt_05_hours +
                                    tace_staff_nuc_mt_06_hours + tace_staff_nuc_mt_07_hours + tace_staff_nuc_mt_08_hours + tace_staff_nuc_mt_09_hours + tace_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

TACE_post_staff = TACE_post_staff %>%
  mutate(Nuc_Med_Tech_cost = (tace_staff_nuc_mt_01_hours + tace_staff_nuc_mt_02_hours + tace_staff_nuc_mt_03_hours + tace_staff_nuc_mt_04_hours + tace_staff_nuc_mt_05_hours +
                                    tace_staff_nuc_mt_06_hours + tace_staff_nuc_mt_07_hours + tace_staff_nuc_mt_08_hours + tace_staff_nuc_mt_09_hours + tace_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

TACE_pre_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))


# NM_physicist
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_nm_ph))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_nm_ph))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_nm_ph))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(NM_physicist_cost = (tace_staff_nm_ph_01_hours + tace_staff_nm_ph_02_hours + tace_staff_nm_ph_03_hours + tace_staff_nm_ph_04_hours + tace_staff_nm_ph_05_hours +
                                tace_staff_nm_ph_06_hours + tace_staff_nm_ph_07_hours + tace_staff_nm_ph_08_hours + tace_staff_nm_ph_09_hours + tace_staff_nm_ph_10_hours)*NM_physicist)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(NM_physicist_cost = (tace_staff_nm_ph_01_hours + tace_staff_nm_ph_02_hours + tace_staff_nm_ph_03_hours + tace_staff_nm_ph_04_hours + tace_staff_nm_ph_05_hours +
                                tace_staff_nm_ph_06_hours + tace_staff_nm_ph_07_hours + tace_staff_nm_ph_08_hours + tace_staff_nm_ph_09_hours + tace_staff_nm_ph_10_hours)*NM_physicist)

TACE_post_staff = TACE_post_staff %>%
  mutate(NM_physicist_cost = (tace_staff_nm_ph_01_hours + tace_staff_nm_ph_02_hours + tace_staff_nm_ph_03_hours + tace_staff_nm_ph_04_hours + tace_staff_nm_ph_05_hours +
                                tace_staff_nm_ph_06_hours + tace_staff_nm_ph_07_hours + tace_staff_nm_ph_08_hours + tace_staff_nm_ph_09_hours + tace_staff_nm_ph_10_hours)*NM_physicist)

TACE_pre_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
TACE_post_staff  %>%
  summarise(sum = sum(NM_physicist_cost))


# Labour_nuclear_medicine
TACE_pre_staff  %>%
  summarise(sum = sum(tace_staff_mab_nm))
TACE_intra_staff  %>%
  summarise(sum = sum(tace_staff_mab_nm))
TACE_post_staff  %>%
  summarise(sum = sum(tace_staff_mab_nm))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(Labour_nuclear_medicine_cost = (tace_staff_mab_nm_01_hours + tace_staff_mab_nm_02_hours + tace_staff_mab_nm_03_hours + tace_staff_mab_nm_04_hours + tace_staff_mab_nm_05_hours +
                                tace_staff_mab_nm_06_hours + tace_staff_mab_nm_07_hours + tace_staff_mab_nm_08_hours + tace_staff_mab_nm_09_hours + tace_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

TACE_intra_staff = TACE_intra_staff %>%
  mutate(Labour_nuclear_medicine_cost = (tace_staff_mab_nm_01_hours + tace_staff_mab_nm_02_hours + tace_staff_mab_nm_03_hours + tace_staff_mab_nm_04_hours + tace_staff_mab_nm_05_hours +
                                tace_staff_mab_nm_06_hours + tace_staff_mab_nm_07_hours + tace_staff_mab_nm_08_hours + tace_staff_mab_nm_09_hours + tace_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

TACE_post_staff = TACE_post_staff %>%
  mutate(Labour_nuclear_medicine_cost = (tace_staff_mab_nm_01_hours + tace_staff_mab_nm_02_hours + tace_staff_mab_nm_03_hours + tace_staff_mab_nm_04_hours + tace_staff_mab_nm_05_hours +
                                tace_staff_mab_nm_06_hours + tace_staff_mab_nm_07_hours + tace_staff_mab_nm_08_hours + tace_staff_mab_nm_09_hours + tace_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

TACE_pre_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
TACE_intra_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
TACE_post_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))



## Final staff cost
TACE_pre_staff_final = TACE_pre_staff %>%
  select(1, 547: 563) %>%
  mutate(TACE_pre_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

TACE_pre_staff_final %>%
  summarise(sum = sum(TACE_pre_staff_final))

TACE_pre_staff_final_1 = TACE_pre_staff_final %>%
  select(1, TACE_pre_staff_final)



TACE_intra_staff_final = TACE_intra_staff %>%
  select(1, 547: 563) %>%
  mutate(TACE_intra_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

TACE_intra_staff_final %>%
  summarise(sum = sum(TACE_intra_staff_final))

TACE_intra_staff_final_1 = TACE_intra_staff_final %>%
  select(1, TACE_intra_staff_final)



TACE_post_staff_final = TACE_post_staff %>%
  select(1, 547: 563) %>%
  mutate(TACE_post_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

TACE_post_staff_final %>%
  summarise(sum = sum(TACE_post_staff_final))

TACE_post_staff_final_1 = TACE_post_staff_final %>%
  select(1, TACE_post_staff_final)



## -- Boot strapping -- ## TACE_pre_staff_final


Boot.pre_staff_cost.1 <- matrix(sample(TACE_pre_staff_final$TACE_pre_staff_final, size= B*n, 
                                         replace=TRUE), ncol=B, nrow=n)


dim(Boot.pre_staff_cost.1)

Boot.pre_staff_cost.1[1:5,1:5]

Boot.pre_staff_cost.1.Means_1 <- colMeans(Boot.pre_staff_cost.1)

length(Boot.pre_staff_cost.1.Means_1)

Boot.pre_staff_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.pre_staff_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.pre_staff_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.pre_staff_cost.1.Means_1, prob=0.975)


pre_staff_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(pre_staff_cost) = c("Year", "L_CI","Mean","U_CI")
pre_staff_cost = as.data.frame(pre_staff_cost)
pre_staff_cost


## -- Boot strapping -- ## TACE_intra_staff_final

Boot.intra_staff_cost.1 <- matrix(sample(TACE_intra_staff_final$TACE_intra_staff_final, size= B*n, 
                                       replace=TRUE), ncol=B, nrow=n)


dim(Boot.intra_staff_cost.1)

Boot.intra_staff_cost.1[1:5,1:5]

Boot.intra_staff_cost.1.Means_1 <- colMeans(Boot.intra_staff_cost.1)

length(Boot.intra_staff_cost.1.Means_1)

Boot.intra_staff_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.intra_staff_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.intra_staff_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.intra_staff_cost.1.Means_1, prob=0.975)


intra_staff_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(intra_staff_cost) = c("Year", "L_CI","Mean","U_CI")
intra_staff_cost = as.data.frame(intra_staff_cost)
intra_staff_cost


## -- Boot strapping -- ## TACE_post_staff_final
set.seed(13579)
n = 86
B = 100000

Boot.post_staff_cost.1 <- matrix(sample(TACE_post_staff_final$TACE_post_staff_final, size= B*n, 
                                         replace=TRUE), ncol=B, nrow=n)


dim(Boot.post_staff_cost.1)

Boot.post_staff_cost.1[1:5,1:5]

Boot.post_staff_cost.1.Means_1 <- colMeans(Boot.post_staff_cost.1)

length(Boot.post_staff_cost.1.Means_1)

Boot.post_staff_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.post_staff_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.post_staff_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.post_staff_cost.1.Means_1, prob=0.975)


post_staff_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(post_staff_cost) = c("Year", "L_CI","Mean","U_CI")
post_staff_cost = as.data.frame(post_staff_cost)
post_staff_cost



TACE_staff_total = merge(x = TACE_pre_staff_final, y = TACE_intra_staff_final, by = "record_id", all = TRUE)
TACE_staff_total = merge(x = TACE_staff_total, y = TACE_post_staff_final, by = "record_id", all = TRUE)

TACE_staff_total = TACE_staff_total %>%
  select(1, TACE_pre_staff_final, TACE_intra_staff_final, TACE_post_staff_final) %>%
  mutate(TACE_staff_total = TACE_pre_staff_final + TACE_intra_staff_final +TACE_post_staff_final)

TACE_staff_total %>%
  summarise(sum = sum(TACE_staff_total))

TACE_staff_total_1 = TACE_staff_total %>%
  select(1, 5)


## -- Boot strapping -- ## TACE_total_staff_final

Boot.total_staff_cost.1 <- matrix(sample(TACE_staff_total$TACE_staff_total, size= B*n, 
                                       replace=TRUE), ncol=B, nrow=n)


dim(Boot.total_staff_cost.1)

Boot.total_staff_cost.1[1:5,1:5]

Boot.total_staff_cost.1.Means_1 <- colMeans(Boot.total_staff_cost.1)

length(Boot.total_staff_cost.1.Means_1)

Boot.total_staff_cost.1.Means_1[1:10]

post_cost_1_L = quantile(Boot.total_staff_cost.1.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.total_staff_cost.1.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.total_staff_cost.1.Means_1, prob=0.975)


total_staff_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(total_staff_cost) = c("Year", "L_CI","Mean","U_CI")
total_staff_cost = as.data.frame(total_staff_cost)
total_staff_cost



###########################################################
########### Total cost ####################################

## Total
Total_cost = merge(x = TACE_imaging_4, y = TACE_combine_ConMed, by = "record_id", all = TRUE)
Total_cost = merge(x = Total_cost, y = TACE_staff_total_1, by = "record_id", all = TRUE)

Total_cost = Total_cost %>%
  mutate(TACE_total_cost = TACE_imaging_cost + total_ConMed + TACE_staff_total)

Total_cost %>%
  summarise(sum = sum(TACE_total_cost))


## Pre
Total_pre_cost = merge(x = TACE_imaging_pre, y = TACE_pre_ConMed_3, by = "record_id", all = TRUE)
Total_pre_cost = merge(x = Total_pre_cost, y = TACE_pre_staff_final_1, by = "record_id", all = TRUE)


Total_pre_cost = Total_pre_cost %>%
  mutate(TACE_pre_total_cost = cost_pre_imaging + cost_pre_conMed + TACE_pre_staff_final)

Total_pre_cost %>%
  summarise(sum = sum(TACE_pre_total_cost))


## INtra
Total_intra_cost = merge(x = TACE_imaging_intra, y = TACE_intra_ConMed_3, by = "record_id", all = TRUE)
Total_intra_cost = merge(x = Total_intra_cost, y = TACE_intra_staff_final_1, by = "record_id", all = TRUE)


Total_intra_cost = Total_intra_cost %>%
  mutate(TACE_intra_total_cost = cost_intra_imaging + cost_intra_conMed + TACE_intra_staff_final)
 
Total_intra_cost %>%
  summarise(sum = sum(TACE_intra_total_cost))


## Post
Total_post_cost = merge(x = TACE_imaging_post, y = TACE_post_ConMed_3, by = "record_id", all = TRUE)
Total_post_cost = merge(x = Total_post_cost, y = TACE_post_staff_final_1, by = "record_id", all = TRUE)

Total_post_cost[is.na(Total_post_cost)]<-0

Total_post_cost = Total_post_cost %>%
  mutate(TACE_post_total_cost = cost_post_imaging + cost_post_conMed + TACE_post_staff_final)

Total_post_cost %>%
  summarise(sum = sum(TACE_post_total_cost))




###### Boot strap total cost
Boot.tace_total_cost <- matrix(sample(Total_cost$TACE_total_cost, size= B*n, 
                                        replace=TRUE), ncol=B, nrow=n)


dim(Boot.tace_total_cost)

Boot.tace_total_cost[1:5,1:5]

Boot.tace_total_cost.Means_1 <- colMeans(Boot.tace_total_cost)

length(Boot.tace_total_cost.Means_1)

Boot.tace_total_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.tace_total_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.tace_total_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.tace_total_cost.Means_1, prob=0.975)


TACE_total_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(TACE_total_cost) = c("Year", "L_CI","Mean","U_CI")
TACE_total_cost = as.data.frame(TACE_total_cost)
TACE_total_cost


###### Boot strap pre cost
Boot.tace_total_pre_cost <- matrix(sample(Total_pre_cost$TACE_pre_total_cost, size= B*n, 
                                      replace=TRUE), ncol=B, nrow=n)


dim(Boot.tace_total_pre_cost)

Boot.tace_total_pre_cost[1:5,1:5]

Boot.tace_total_pre_cost.Means_1 <- colMeans(Boot.tace_total_pre_cost)

length(Boot.tace_total_pre_cost.Means_1)

Boot.tace_total_pre_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.tace_total_pre_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.tace_total_pre_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.tace_total_pre_cost.Means_1, prob=0.975)


TACE_pre_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(TACE_pre_cost) = c("Year", "L_CI","Mean","U_CI")
TACE_pre_cost = as.data.frame(TACE_pre_cost)
TACE_pre_cost


###### Boot strap intra cost
Boot.tace_total_intra_cost <- matrix(sample(Total_intra_cost$TACE_intra_total_cost, size= B*n, 
                                          replace=TRUE), ncol=B, nrow=n)


dim(Boot.tace_total_intra_cost)

Boot.tace_total_intra_cost[1:5,1:5]

Boot.tace_total_intra_cost.Means_1 <- colMeans(Boot.tace_total_intra_cost)

length(Boot.tace_total_intra_cost.Means_1)

Boot.tace_total_intra_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.tace_total_intra_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.tace_total_intra_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.tace_total_intra_cost.Means_1, prob=0.975)


TACE_intra_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(TACE_intra_cost) = c("Year", "L_CI","Mean","U_CI")
TACE_intra_cost = as.data.frame(TACE_intra_cost)
TACE_intra_cost


###### Boot strap post cost
Boot.tace_total_post_cost <- matrix(sample(Total_post_cost$TACE_post_total_cost, size= B*n, 
                                            replace=TRUE), ncol=B, nrow=n)


dim(Boot.tace_total_post_cost)

Boot.tace_total_post_cost[1:5,1:5]

Boot.tace_total_post_cost.Means_1 <- colMeans(Boot.tace_total_post_cost)

length(Boot.tace_total_post_cost.Means_1)

Boot.tace_total_post_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.tace_total_post_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.tace_total_post_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.tace_total_post_cost.Means_1, prob=0.975)


TACE_post_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(TACE_post_cost) = c("Year", "L_CI","Mean","U_CI")
TACE_post_cost = as.data.frame(TACE_post_cost)
TACE_post_cost




#################  To estimate the staff clinical team level cost - Pre ##########


######### Administrative_labour

TACE_pre_staff %>%
  summarise(max = max(tace_staff_al)) # 3



#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_01_team = case_when(
    tace_staff_al_01_team %in% 1 ~ "Oncology",
    tace_staff_al_01_team %in% 2 ~ "Radiology",
    tace_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_01_team = coalesce(tace_staff_al_01_team, tace_staff_al_01_other))
TACE_pre_staff$tace_staff_al_01_team = as.factor(TACE_pre_staff$tace_staff_al_01_team)

levels(TACE_pre_staff$tace_staff_al_01_team)

TACE_pre_staff %>%
  filter(tace_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_pre_staff %>%
  filter(tace_staff_al_01_team == "Admissions") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)


#2
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_02_team = case_when(
    tace_staff_al_02_team %in% 1 ~ "Oncology",
    tace_staff_al_02_team %in% 2 ~ "Radiology",
    tace_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_02_team = coalesce(tace_staff_al_02_team, tace_staff_al_02_other))
TACE_pre_staff$tace_staff_al_02_team = as.factor(TACE_pre_staff$tace_staff_al_02_team)

levels(TACE_pre_staff$tace_staff_al_02_team)

TACE_pre_staff %>%
  filter(tace_staff_al_02_team == "Admission" | tace_staff_al_02_team == "admissions" |tace_staff_al_02_team == "Admissions") %>%
  summarise(sum = sum(tace_staff_al_02_hours),
            cost = sum*Administrative_labour)

TACE_pre_staff %>%
  filter(tace_staff_al_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_al_02_hours),
            cost = sum*Administrative_labour)

#3
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_03_team = case_when(
    tace_staff_al_03_team %in% 1 ~ "Oncology",
    tace_staff_al_03_team %in% 2 ~ "Radiology",
    tace_staff_al_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_03_team %in% 4 ~ "Anaesthesia"
  ))

TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_al_03_team = coalesce(tace_staff_al_03_team, tace_staff_al_03_other))
TACE_pre_staff$tace_staff_al_03_team = as.factor(TACE_pre_staff$tace_staff_al_03_team)

levels(TACE_pre_staff$tace_staff_al_03_team)

TACE_pre_staff %>%
  filter(tace_staff_al_03_team == "Admissions") %>%
  summarise(sum = sum(tace_staff_al_03_hours),
            cost = sum*Administrative_labour)

TACE_pre_staff %>%
  filter(tace_staff_al_03_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_al_03_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

TACE_pre_staff %>%
  summarise(max = max(tace_staff_ass_n)) # 0


######## Enrolled_nurse
TACE_pre_staff %>%
  summarise(max = max(tace_staff_enr_n)) # 0


####### Registered_nurse
TACE_pre_staff %>%
  summarise(max = max(tace_staff_reg_n)) # 3


#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_01_team = case_when(
    tace_staff_reg_n_01_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_01_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_01_team = coalesce(tace_staff_reg_n_01_team, tace_staff_reg_n_01_other))
TACE_pre_staff$tace_staff_reg_n_01_team = as.factor(TACE_pre_staff$tace_staff_reg_n_01_team)

levels(TACE_pre_staff$tace_staff_reg_n_01_team)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "SCU") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_01_team == "SSU") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)



#2
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_02_team = case_when(
    tace_staff_reg_n_02_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_02_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_02_team = coalesce(tace_staff_reg_n_02_team, tace_staff_reg_n_02_other))
TACE_pre_staff$tace_staff_reg_n_02_team = as.factor(TACE_pre_staff$tace_staff_reg_n_02_team)

levels(TACE_pre_staff$tace_staff_reg_n_02_team)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_02_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_02_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_02_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)



#3
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_03_team = case_when(
    tace_staff_reg_n_03_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_03_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_reg_n_03_team = coalesce(tace_staff_reg_n_03_team, tace_staff_reg_n_03_other))
TACE_pre_staff$tace_staff_reg_n_03_team = as.factor(TACE_pre_staff$tace_staff_reg_n_03_team)

levels(TACE_pre_staff$tace_staff_reg_n_03_team)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_03_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_03_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_03_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_pre_staff %>%
  filter(tace_staff_reg_n_03_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



######### Clinical_nurse
TACE_pre_staff %>%
  summarise(max = max(tace_staff_cln_n)) # 1


#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_cln_n_01_team = case_when(
    tace_staff_cln_n_01_team %in% 1 ~ "Oncology",
    tace_staff_cln_n_01_team %in% 2 ~ "Radiology",
    tace_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_cln_n_01_team = coalesce(tace_staff_cln_n_01_team, tace_staff_cln_n_01_other))
TACE_pre_staff$tace_staff_cln_n_01_team = as.factor(TACE_pre_staff$tace_staff_cln_n_01_team)

levels(TACE_pre_staff$tace_staff_cln_n_01_team)

TACE_pre_staff %>%
  filter(tace_staff_cln_n_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)



############ Assistant_nurse_unit_manager
TACE_pre_staff %>%
  summarise(max = max(tace_staff_anum)) # 0



############ Case_manager
TACE_pre_staff %>%
  summarise(max = max(tace_staff_case_m)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_case_m_01_team = case_when(
    tace_staff_case_m_01_team %in% 1 ~ "Oncology",
    tace_staff_case_m_01_team %in% 2 ~ "Radiology",
    tace_staff_case_m_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_case_m_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_case_m_01_team = coalesce(tace_staff_case_m_01_team, tace_staff_case_m_01_other))
TACE_pre_staff$tace_staff_case_m_01_team = as.factor(TACE_pre_staff$tace_staff_case_m_01_team)

levels(TACE_pre_staff$tace_staff_case_m_01_team)

TACE_pre_staff %>%
  filter(tace_staff_case_m_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_case_m_01_hours),
            cost = sum*Case_manager)


TACE_pre_staff %>%
  filter(tace_staff_case_m_01_team == "HCC/ HPB" | tace_staff_case_m_01_team == "HCC/HPB" | tace_staff_case_m_01_team == "HPB / HCC" |
           tace_staff_case_m_01_team == "HPB/HCC") %>%
  summarise(sum = sum(tace_staff_case_m_01_hours),
            cost = sum*Case_manager)

TACE_pre_staff %>%
  filter(tace_staff_case_m_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_case_m_01_hours),
            cost = sum*Case_manager)




############## Nurse_unit_manager	
TACE_pre_staff %>%
  summarise(max = max(tace_staff_nur_um)) # 0



############## Intern_or_Resident
TACE_pre_staff %>%
  summarise(max = max(tace_staff_in_res)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_in_res_01_team = case_when(
    tace_staff_in_res_01_team %in% 1 ~ "Oncology",
    tace_staff_in_res_01_team %in% 2 ~ "Radiology",
    tace_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_in_res_01_team = coalesce(tace_staff_in_res_01_team, tace_staff_in_res_01_other))
TACE_pre_staff$tace_staff_in_res_01_team = as.factor(TACE_pre_staff$tace_staff_in_res_01_team)

levels(TACE_pre_staff$tace_staff_in_res_01_team)

TACE_pre_staff %>%
  filter(tace_staff_in_res_01_team == "GAST" | tace_staff_in_res_01_team == "Gastro" | tace_staff_in_res_01_team == "GASTRO" |
           tace_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)


TACE_pre_staff %>%
  filter(tace_staff_in_res_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_pre_staff %>%
  filter(tace_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)




############# Registrar
TACE_pre_staff %>%
  summarise(max = max(tace_staff_registrar)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_registrar_01_team = case_when(
    tace_staff_registrar_01_team %in% 1 ~ "Oncology",
    tace_staff_registrar_01_team %in% 2 ~ "Radiology",
    tace_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_registrar_01_team = coalesce(tace_staff_registrar_01_team, tace_staff_registrar_01_other))
TACE_pre_staff$tace_staff_registrar_01_team = as.factor(TACE_pre_staff$tace_staff_registrar_01_team)

levels(TACE_pre_staff$tace_staff_registrar_01_team)

TACE_pre_staff %>%
  filter(tace_staff_registrar_01_team == "GAST" | tace_staff_registrar_01_team == "Gastro" | tace_staff_registrar_01_team == "GASTRO" |
           tace_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)



############ Fellow
TACE_pre_staff %>%
  summarise(max = max(tace_staff_fellow)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_fellow_01_team = case_when(
    tace_staff_fellow_01_team %in% 1 ~ "Oncology",
    tace_staff_fellow_01_team %in% 2 ~ "Radiology",
    tace_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_fellow_01_team = coalesce(tace_staff_fellow_01_team, tace_staff_fellow_01_other))
TACE_pre_staff$tace_staff_fellow_01_team = as.factor(TACE_pre_staff$tace_staff_fellow_01_team)

levels(TACE_pre_staff$tace_staff_fellow_01_team)

TACE_pre_staff %>%
  filter(tace_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_fellow_01_hours),
            cost = sum*Fellow)


################# Consultant
TACE_pre_staff %>%
  summarise(max = max(tace_staff_consultant)) # 2

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_consultant_01_team = case_when(
    tace_staff_consultant_01_team %in% 1 ~ "Oncology",
    tace_staff_consultant_01_team %in% 2 ~ "Radiology",
    tace_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_consultant_01_team = coalesce(tace_staff_consultant_01_team, tace_staff_consultant_01_other))
TACE_pre_staff$tace_staff_consultant_01_team = as.factor(TACE_pre_staff$tace_staff_consultant_01_team)

levels(TACE_pre_staff$tace_staff_consultant_01_team)

TACE_pre_staff %>%
  filter(tace_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_consultant_02_team = case_when(
    tace_staff_consultant_02_team %in% 1 ~ "Oncology",
    tace_staff_consultant_02_team %in% 2 ~ "Radiology",
    tace_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_consultant_02_team = coalesce(tace_staff_consultant_02_team, tace_staff_consultant_02_other))
TACE_pre_staff$tace_staff_consultant_02_team = as.factor(TACE_pre_staff$tace_staff_consultant_02_team)

levels(TACE_pre_staff$tace_staff_consultant_02_team)

TACE_pre_staff %>%
  filter(tace_staff_consultant_02_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_consultant_02_hours),
            cost = sum*Consultant)






############### Radiographer
TACE_pre_staff %>%
  summarise(max = max(tace_staff_radio)) # 2

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_radio_01_team = case_when(
    tace_staff_radio_01_team %in% 1 ~ "Oncology",
    tace_staff_radio_01_team %in% 2 ~ "Radiology",
    tace_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_radio_01_team = coalesce(tace_staff_radio_01_team, tace_staff_radio_01_other))
TACE_pre_staff$tace_staff_radio_01_team = as.factor(TACE_pre_staff$tace_staff_radio_01_team)

levels(TACE_pre_staff$tace_staff_radio_01_team)

TACE_pre_staff %>%
  filter(tace_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_radio_02_team = case_when(
    tace_staff_radio_02_team %in% 1 ~ "Oncology",
    tace_staff_radio_02_team %in% 2 ~ "Radiology",
    tace_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_radio_02_team = coalesce(tace_staff_radio_02_team, tace_staff_radio_02_other))
TACE_pre_staff$tace_staff_radio_02_team = as.factor(TACE_pre_staff$tace_staff_radio_02_team)

levels(TACE_pre_staff$tace_staff_radio_02_team)

TACE_pre_staff %>%
  filter(tace_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
TACE_pre_staff %>%
  summarise(max = max(tace_staff_ultra)) # 2

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_ultra_01_team = case_when(
    tace_staff_ultra_01_team %in% 1 ~ "Oncology",
    tace_staff_ultra_01_team %in% 2 ~ "Radiology",
    tace_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_ultra_01_team = coalesce(tace_staff_ultra_01_team, tace_staff_ultra_01_other))
TACE_pre_staff$tace_staff_ultra_01_team = as.factor(TACE_pre_staff$tace_staff_ultra_01_team)

levels(TACE_pre_staff$tace_staff_ultra_01_team)

TACE_pre_staff %>%
  filter(tace_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)


#2
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_ultra_02_team = case_when(
    tace_staff_ultra_02_team %in% 1 ~ "Oncology",
    tace_staff_ultra_02_team %in% 2 ~ "Radiology",
    tace_staff_ultra_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_ultra_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_ultra_02_team = coalesce(tace_staff_ultra_02_team, tace_staff_ultra_02_other))
TACE_pre_staff$tace_staff_ultra_02_team = as.factor(TACE_pre_staff$tace_staff_ultra_02_team)

levels(TACE_pre_staff$tace_staff_ultra_02_team)

TACE_pre_staff %>%
  filter(tace_staff_ultra_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_ultra_02_hours),
            cost = sum*Ultrasonographer)




################ Nuc_Med_Tech
TACE_pre_staff %>%
  summarise(max = max(tace_staff_nuc_mt)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_nuc_mt_01_team = case_when(
    tace_staff_nuc_mt_01_team %in% 1 ~ "Oncology",
    tace_staff_nuc_mt_01_team %in% 2 ~ "Radiology",
    tace_staff_nuc_mt_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_nuc_mt_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_nuc_mt_01_team = coalesce(tace_staff_nuc_mt_01_team, tace_staff_nuc_mt_01_other))
TACE_pre_staff$tace_staff_nuc_mt_01_team = as.factor(TACE_pre_staff$tace_staff_nuc_mt_01_team)

levels(TACE_pre_staff$tace_staff_nuc_mt_01_team)

TACE_pre_staff %>%
  filter(tace_staff_nuc_mt_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(tace_staff_nuc_mt_01_hours),
            cost = sum*Nuc_Med_Tech)




###################### NM_physicist
TACE_pre_staff %>%
  summarise(max = max(tace_staff_nm_ph)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_nm_ph_01_team = case_when(
    tace_staff_nm_ph_01_team %in% 1 ~ "Oncology",
    tace_staff_nm_ph_01_team %in% 2 ~ "Radiology",
    tace_staff_nm_ph_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_nm_ph_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_nm_ph_01_team = coalesce(tace_staff_nm_ph_01_team, tace_staff_nm_ph_01_other))
TACE_pre_staff$tace_staff_nm_ph_01_team = as.factor(TACE_pre_staff$tace_staff_nm_ph_01_team)

levels(TACE_pre_staff$tace_staff_nm_ph_01_team)

TACE_pre_staff %>%
  filter(tace_staff_nm_ph_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(tace_staff_nm_ph_01_hours),
            cost = sum*NM_physicist)



########################### Labour_nuclear_medicine
TACE_pre_staff %>%
  summarise(max = max(tace_staff_mab_nm)) # 1

#1
TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_mab_nm_01_team = case_when(
    tace_staff_mab_nm_01_team %in% 1 ~ "Oncology",
    tace_staff_mab_nm_01_team %in% 2 ~ "Radiology",
    tace_staff_mab_nm_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_mab_nm_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_pre_staff = TACE_pre_staff %>%
  mutate(tace_staff_mab_nm_01_team = coalesce(tace_staff_mab_nm_01_team, tace_staff_mab_nm_01_other))
TACE_pre_staff$tace_staff_mab_nm_01_team = as.factor(TACE_pre_staff$tace_staff_mab_nm_01_team)

levels(TACE_pre_staff$tace_staff_mab_nm_01_team)

TACE_pre_staff %>%
  filter(tace_staff_mab_nm_01_team == "Oncology") %>%
  summarise(sum = sum(tace_staff_mab_nm_01_hours),
            cost = sum*Labour_nuclear_medicine)

TACE_pre_staff %>%
  filter(tace_staff_mab_nm_01_team == "Oncology pharmacist" | tace_staff_mab_nm_01_team == "Oncology Pharmacy" | tace_staff_mab_nm_01_team == "Pharmacy" ) %>%
  summarise(sum = sum(tace_staff_mab_nm_01_hours),
            cost = sum*Labour_nuclear_medicine)








#######################################################  To estimate the staff clinical team level cost - intra ##########


######### Administrative_labour

TACE_intra_staff %>%
  summarise(max = max(tace_staff_al)) # 2



#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_al_01_team = case_when(
    tace_staff_al_01_team %in% 1 ~ "Oncology",
    tace_staff_al_01_team %in% 2 ~ "Radiology",
    tace_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_al_01_team = coalesce(tace_staff_al_01_team, tace_staff_al_01_other))
TACE_intra_staff$tace_staff_al_01_team = as.factor(TACE_intra_staff$tace_staff_al_01_team)

levels(TACE_intra_staff$tace_staff_al_01_team)

TACE_intra_staff %>%
  filter(tace_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)



#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_al_02_team = case_when(
    tace_staff_al_02_team %in% 1 ~ "Oncology",
    tace_staff_al_02_team %in% 2 ~ "Radiology",
    tace_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_al_02_team = coalesce(tace_staff_al_02_team, tace_staff_al_02_other))
TACE_intra_staff$tace_staff_al_02_team = as.factor(TACE_intra_staff$tace_staff_al_02_team)

levels(TACE_intra_staff$tace_staff_al_02_team)

TACE_intra_staff %>%
  filter(tace_staff_al_02_team == "Admissions") %>%
  summarise(sum = sum(tace_staff_al_02_hours),
            cost = sum*Administrative_labour)





######### Assistant_in_nursing

TACE_intra_staff %>%
  summarise(max = max(tace_staff_ass_n)) # 0


######## Enrolled_nurse
TACE_intra_staff %>%
  summarise(max = max(tace_staff_enr_n)) # 1

TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_enr_n_01_team = case_when(
    tace_staff_enr_n_01_team %in% 1 ~ "Oncology",
    tace_staff_enr_n_01_team %in% 2 ~ "Radiology",
    tace_staff_enr_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_enr_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_enr_n_01_team = coalesce(tace_staff_enr_n_01_team, tace_staff_enr_n_01_other))
TACE_intra_staff$tace_staff_enr_n_01_team = as.factor(TACE_intra_staff$tace_staff_enr_n_01_team)

levels(TACE_intra_staff$tace_staff_enr_n_01_team)

TACE_intra_staff %>%
  filter(tace_staff_enr_n_01_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_enr_n_01_hours),
            cost = sum*Enrolled_nurse)




####### Registered_nurse
TACE_intra_staff %>%
  summarise(max = max(tace_staff_reg_n)) # 4


#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_01_team = case_when(
    tace_staff_reg_n_01_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_01_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_01_team = coalesce(tace_staff_reg_n_01_team, tace_staff_reg_n_01_other))
TACE_intra_staff$tace_staff_reg_n_01_team = as.factor(TACE_intra_staff$tace_staff_reg_n_01_team)

levels(TACE_intra_staff$tace_staff_reg_n_01_team)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)



#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_02_team = case_when(
    tace_staff_reg_n_02_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_02_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_02_team = coalesce(tace_staff_reg_n_02_team, tace_staff_reg_n_02_other))
TACE_intra_staff$tace_staff_reg_n_02_team = as.factor(TACE_intra_staff$tace_staff_reg_n_02_team)

levels(TACE_intra_staff$tace_staff_reg_n_02_team)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)



#3
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_03_team = case_when(
    tace_staff_reg_n_03_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_03_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_03_team = coalesce(tace_staff_reg_n_03_team, tace_staff_reg_n_03_other))
TACE_intra_staff$tace_staff_reg_n_03_team = as.factor(TACE_intra_staff$tace_staff_reg_n_03_team)

levels(TACE_intra_staff$tace_staff_reg_n_03_team)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_03_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_03_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_03_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)


#4
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_04_team = case_when(
    tace_staff_reg_n_04_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_04_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_reg_n_04_team = coalesce(tace_staff_reg_n_04_team, tace_staff_reg_n_04_other))
TACE_intra_staff$tace_staff_reg_n_04_team = as.factor(TACE_intra_staff$tace_staff_reg_n_04_team)

levels(TACE_intra_staff$tace_staff_reg_n_04_team)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_04_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

TACE_intra_staff %>%
  filter(tace_staff_reg_n_04_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)





######### Clinical_nurse
TACE_intra_staff %>%
  summarise(max = max(tace_staff_cln_n)) # 2


#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_cln_n_01_team = case_when(
    tace_staff_cln_n_01_team %in% 1 ~ "Oncology",
    tace_staff_cln_n_01_team %in% 2 ~ "Radiology",
    tace_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_cln_n_01_team = coalesce(tace_staff_cln_n_01_team, tace_staff_cln_n_01_other))
TACE_intra_staff$tace_staff_cln_n_01_team = as.factor(TACE_intra_staff$tace_staff_cln_n_01_team)

levels(TACE_intra_staff$tace_staff_cln_n_01_team)

TACE_intra_staff %>%
  filter(tace_staff_cln_n_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_cln_n_02_team = case_when(
    tace_staff_cln_n_02_team %in% 1 ~ "Oncology",
    tace_staff_cln_n_02_team %in% 2 ~ "Radiology",
    tace_staff_cln_n_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_cln_n_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_cln_n_02_team = coalesce(tace_staff_cln_n_02_team, tace_staff_cln_n_02_other))
TACE_intra_staff$tace_staff_cln_n_02_team = as.factor(TACE_intra_staff$tace_staff_cln_n_02_team)

levels(TACE_intra_staff$tace_staff_cln_n_02_team)

TACE_intra_staff %>%
  filter(tace_staff_cln_n_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_cln_n_02_hours),
            cost = sum*Clinical_nurse)



############ Assistant_nurse_unit_manager
TACE_intra_staff %>%
  summarise(max = max(tace_staff_anum)) # 0



############ Case_manager
TACE_intra_staff %>%
  summarise(max = max(tace_staff_case_m)) # 0




############## Nurse_unit_manager	
TACE_intra_staff %>%
  summarise(max = max(tace_staff_nur_um)) # 0



############## Intern_or_Resident
TACE_intra_staff %>%
  summarise(max = max(tace_staff_in_res)) # 1

#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_in_res_01_team = case_when(
    tace_staff_in_res_01_team %in% 1 ~ "Oncology",
    tace_staff_in_res_01_team %in% 2 ~ "Radiology",
    tace_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_in_res_01_team = coalesce(tace_staff_in_res_01_team, tace_staff_in_res_01_other))
TACE_intra_staff$tace_staff_in_res_01_team = as.factor(TACE_intra_staff$tace_staff_in_res_01_team)

levels(TACE_intra_staff$tace_staff_in_res_01_team)

TACE_intra_staff %>%
  filter(tace_staff_in_res_01_team == "GAST" | tace_staff_in_res_01_team == "Gastro" | tace_staff_in_res_01_team == "GASTRO" |
           tace_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)




############# Registrar
TACE_intra_staff %>%
  summarise(max = max(tace_staff_registrar)) # 2

#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_registrar_01_team = case_when(
    tace_staff_registrar_01_team %in% 1 ~ "Oncology",
    tace_staff_registrar_01_team %in% 2 ~ "Radiology",
    tace_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_registrar_01_team = coalesce(tace_staff_registrar_01_team, tace_staff_registrar_01_other))
TACE_intra_staff$tace_staff_registrar_01_team = as.factor(TACE_intra_staff$tace_staff_registrar_01_team)

levels(TACE_intra_staff$tace_staff_registrar_01_team)

TACE_intra_staff %>%
  filter(tace_staff_registrar_01_team == "GAST" | tace_staff_registrar_01_team == "Gastro" | tace_staff_registrar_01_team == "GASTRO" |
           tace_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_intra_staff %>%
  filter(tace_staff_registrar_01_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_intra_staff %>%
  filter(tace_staff_registrar_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_intra_staff %>%
  filter(tace_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)


#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_registrar_02_team = case_when(
    tace_staff_registrar_02_team %in% 1 ~ "Oncology",
    tace_staff_registrar_02_team %in% 2 ~ "Radiology",
    tace_staff_registrar_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_registrar_02_team = coalesce(tace_staff_registrar_02_team, tace_staff_registrar_02_other))
TACE_intra_staff$tace_staff_registrar_02_team = as.factor(TACE_intra_staff$tace_staff_registrar_02_team)

levels(TACE_intra_staff$tace_staff_registrar_02_team)

TACE_intra_staff %>%
  filter(tace_staff_registrar_02_team == "GAST" | tace_staff_registrar_02_team == "Gastro" | tace_staff_registrar_02_team == "GASTRO" |
           tace_staff_registrar_02_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)





############ Fellow
TACE_intra_staff %>%
  summarise(max = max(tace_staff_fellow)) # 1

#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_fellow_01_team = case_when(
    tace_staff_fellow_01_team %in% 1 ~ "Oncology",
    tace_staff_fellow_01_team %in% 2 ~ "Radiology",
    tace_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_fellow_01_team = coalesce(tace_staff_fellow_01_team, tace_staff_fellow_01_other))
TACE_intra_staff$tace_staff_fellow_01_team = as.factor(TACE_intra_staff$tace_staff_fellow_01_team)

levels(TACE_intra_staff$tace_staff_fellow_01_team)

TACE_intra_staff %>%
  filter(tace_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_fellow_01_hours),
            cost = sum*Fellow)



################# Consultant
TACE_intra_staff %>%
  summarise(max = max(tace_staff_consultant)) # 2

#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_consultant_01_team = case_when(
    tace_staff_consultant_01_team %in% 1 ~ "Oncology",
    tace_staff_consultant_01_team %in% 2 ~ "Radiology",
    tace_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_consultant_01_team = coalesce(tace_staff_consultant_01_team, tace_staff_consultant_01_other))
TACE_intra_staff$tace_staff_consultant_01_team = as.factor(TACE_intra_staff$tace_staff_consultant_01_team)

levels(TACE_intra_staff$tace_staff_consultant_01_team)

TACE_intra_staff %>%
  filter(tace_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)

TACE_intra_staff %>%
  filter(tace_staff_consultant_01_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_consultant_02_team = case_when(
    tace_staff_consultant_02_team %in% 1 ~ "Oncology",
    tace_staff_consultant_02_team %in% 2 ~ "Radiology",
    tace_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_consultant_02_team = coalesce(tace_staff_consultant_02_team, tace_staff_consultant_02_other))
TACE_intra_staff$tace_staff_consultant_02_team = as.factor(TACE_intra_staff$tace_staff_consultant_02_team)

levels(TACE_intra_staff$tace_staff_consultant_02_team)

TACE_intra_staff %>%
  filter(tace_staff_consultant_02_team == "Anaesthesia") %>%
  summarise(sum = sum(tace_staff_consultant_02_hours),
            cost = sum*Consultant)

TACE_intra_staff %>%
  filter(tace_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_02_hours),
            cost = sum*Consultant)




############### Radiographer
TACE_intra_staff %>%
  summarise(max = max(tace_staff_radio)) # 2

#1
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_radio_01_team = case_when(
    tace_staff_radio_01_team %in% 1 ~ "Oncology",
    tace_staff_radio_01_team %in% 2 ~ "Radiology",
    tace_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_radio_01_team = coalesce(tace_staff_radio_01_team, tace_staff_radio_01_other))
TACE_intra_staff$tace_staff_radio_01_team = as.factor(TACE_intra_staff$tace_staff_radio_01_team)

levels(TACE_intra_staff$tace_staff_radio_01_team)

TACE_intra_staff %>%
  filter(tace_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_radio_02_team = case_when(
    tace_staff_radio_02_team %in% 1 ~ "Oncology",
    tace_staff_radio_02_team %in% 2 ~ "Radiology",
    tace_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_intra_staff = TACE_intra_staff %>%
  mutate(tace_staff_radio_02_team = coalesce(tace_staff_radio_02_team, tace_staff_radio_02_other))
TACE_intra_staff$tace_staff_radio_02_team = as.factor(TACE_intra_staff$tace_staff_radio_02_team)

levels(TACE_intra_staff$tace_staff_radio_02_team)

TACE_intra_staff %>%
  filter(tace_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
TACE_intra_staff %>%
  summarise(max = max(tace_staff_ultra)) # 0





################ Nuc_Med_Tech
TACE_intra_staff %>%
  summarise(max = max(tace_staff_nuc_mt)) # 0





###################### NM_physicist
TACE_intra_staff %>%
  summarise(max = max(tace_staff_nm_ph)) # 0




##################### Labour_nuclear_medicine
TACE_intra_staff %>%
  summarise(max = max(tace_staff_mab_nm)) # 0





#################  To estimate the staff clinical team level cost - Post ##########


######### Administrative_labour

TACE_post_staff %>%
  summarise(max = max(tace_staff_al)) # 1



#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_al_01_team = case_when(
    tace_staff_al_01_team %in% 1 ~ "Oncology",
    tace_staff_al_01_team %in% 2 ~ "Radiology",
    tace_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_al_01_team = coalesce(tace_staff_al_01_team, tace_staff_al_01_other))
TACE_post_staff$tace_staff_al_01_team = as.factor(TACE_post_staff$tace_staff_al_01_team)

levels(TACE_post_staff$tace_staff_al_01_team)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "4C") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "4E") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)


TACE_post_staff %>%
  filter(tace_staff_al_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "GAST" | tace_staff_al_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "Medical") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "Oncology") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)

TACE_post_staff %>%
  filter(tace_staff_al_01_team == "Urology") %>%
  summarise(sum = sum(tace_staff_al_01_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

TACE_post_staff %>%
  summarise(max = max(tace_staff_ass_n)) # 0


######## Enrolled_nurse
TACE_post_staff %>%
  summarise(max = max(tace_staff_enr_n)) # 0






####### Registered_nurse
TACE_post_staff %>%
  summarise(max = max(tace_staff_reg_n)) # 18


#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_01_team = case_when(
    tace_staff_reg_n_01_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_01_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_01_team = coalesce(tace_staff_reg_n_01_team, tace_staff_reg_n_01_other))
TACE_post_staff$tace_staff_reg_n_01_team = as.factor(TACE_post_staff$tace_staff_reg_n_01_team)

levels(TACE_post_staff$tace_staff_reg_n_01_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "GAST" | tace_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "Medical") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "Oncology") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "Surgery" | tace_staff_reg_n_01_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_01_team == "Urology") %>%
  summarise(sum = sum(tace_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)



#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_02_team = case_when(
    tace_staff_reg_n_02_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_02_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_02_team = coalesce(tace_staff_reg_n_02_team, tace_staff_reg_n_02_other))
TACE_post_staff$tace_staff_reg_n_02_team = as.factor(TACE_post_staff$tace_staff_reg_n_02_team)

levels(TACE_post_staff$tace_staff_reg_n_02_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Gast" | tace_staff_reg_n_02_team == "Gastro" | tace_staff_reg_n_02_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "HPB") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Medical") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Oncology") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Surgery" | tace_staff_reg_n_02_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_02_team == "Urology") %>%
  summarise(sum = sum(tace_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)




#3
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_03_team = case_when(
    tace_staff_reg_n_03_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_03_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_03_team = coalesce(tace_staff_reg_n_03_team, tace_staff_reg_n_03_other))
TACE_post_staff$tace_staff_reg_n_03_team = as.factor(TACE_post_staff$tace_staff_reg_n_03_team)

levels(TACE_post_staff$tace_staff_reg_n_03_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "5D") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Gast" | tace_staff_reg_n_03_team == "Gastro" | tace_staff_reg_n_03_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "HPB") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Medical") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Oncology") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Surgery" | tace_staff_reg_n_03_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_03_team == "Urology") %>%
  summarise(sum = sum(tace_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



#4
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_04_team = case_when(
    tace_staff_reg_n_04_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_04_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_04_team = coalesce(tace_staff_reg_n_04_team, tace_staff_reg_n_04_other))
TACE_post_staff$tace_staff_reg_n_04_team = as.factor(TACE_post_staff$tace_staff_reg_n_04_team)

levels(TACE_post_staff$tace_staff_reg_n_04_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_04_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_04_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


TACE_post_staff %>%
  filter(tace_staff_reg_n_04_team == "GAST" | tace_staff_reg_n_04_team == "Gastro" | tace_staff_reg_n_04_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_04_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


#5
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_05_team = case_when(
    tace_staff_reg_n_05_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_05_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_05_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_05_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_05_team = coalesce(tace_staff_reg_n_05_team, tace_staff_reg_n_05_other))
TACE_post_staff$tace_staff_reg_n_05_team = as.factor(TACE_post_staff$tace_staff_reg_n_05_team)

levels(TACE_post_staff$tace_staff_reg_n_05_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_05_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_05_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)


TACE_post_staff %>%
  filter(tace_staff_reg_n_05_team == "GAST" | tace_staff_reg_n_05_team == "Gastro" | tace_staff_reg_n_05_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)



#6
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_06_team = case_when(
    tace_staff_reg_n_06_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_06_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_06_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_06_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_06_team = coalesce(tace_staff_reg_n_06_team, tace_staff_reg_n_06_other))
TACE_post_staff$tace_staff_reg_n_06_team = as.factor(TACE_post_staff$tace_staff_reg_n_06_team)

levels(TACE_post_staff$tace_staff_reg_n_06_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_06_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_06_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)


TACE_post_staff %>%
  filter(tace_staff_reg_n_06_team == "GAST" | tace_staff_reg_n_06_team == "Gastro" | tace_staff_reg_n_06_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)


#7
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_07_team = case_when(
    tace_staff_reg_n_07_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_07_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_07_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_07_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_07_team = coalesce(tace_staff_reg_n_07_team, tace_staff_reg_n_07_other))
TACE_post_staff$tace_staff_reg_n_07_team = as.factor(TACE_post_staff$tace_staff_reg_n_07_team)

levels(TACE_post_staff$tace_staff_reg_n_07_team)

TACE_post_staff %>%
  filter(tace_staff_reg_n_07_team == "4C") %>%
  summarise(sum = sum(tace_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

TACE_post_staff %>%
  filter(tace_staff_reg_n_07_team == "4E") %>%
  summarise(sum = sum(tace_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)


TACE_post_staff %>%
  filter(tace_staff_reg_n_07_team == "GAST" | tace_staff_reg_n_07_team == "Gastro" | tace_staff_reg_n_07_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)


#8
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_08_team = case_when(
    tace_staff_reg_n_08_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_08_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_08_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_08_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_08_team = coalesce(tace_staff_reg_n_08_team, tace_staff_reg_n_08_other))
TACE_post_staff$tace_staff_reg_n_08_team = as.factor(TACE_post_staff$tace_staff_reg_n_08_team)

levels(TACE_post_staff$tace_staff_reg_n_08_team)


TACE_post_staff %>%
  filter(tace_staff_reg_n_08_team == "GAST" | tace_staff_reg_n_08_team == "Gastro" | tace_staff_reg_n_08_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_08_hours),
            cost = sum*Registered_nurse)


#9
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_09_team = case_when(
    tace_staff_reg_n_09_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_09_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_09_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_09_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_09_team = coalesce(tace_staff_reg_n_09_team, tace_staff_reg_n_09_other))
TACE_post_staff$tace_staff_reg_n_09_team = as.factor(TACE_post_staff$tace_staff_reg_n_09_team)

levels(TACE_post_staff$tace_staff_reg_n_09_team)


TACE_post_staff %>%
  filter(tace_staff_reg_n_09_team == "GAST" | tace_staff_reg_n_09_team == "Gastro" | tace_staff_reg_n_09_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_09_hours),
            cost = sum*Registered_nurse)


#10
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_10_team = case_when(
    tace_staff_reg_n_10_team %in% 1 ~ "Oncology",
    tace_staff_reg_n_10_team %in% 2 ~ "Radiology",
    tace_staff_reg_n_10_team %in% 3 ~ "Nuclear medicine",
    tace_staff_reg_n_10_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_reg_n_10_team = coalesce(tace_staff_reg_n_10_team, tace_staff_reg_n_10_other))
TACE_post_staff$tace_staff_reg_n_10_team = as.factor(TACE_post_staff$tace_staff_reg_n_10_team)

levels(TACE_post_staff$tace_staff_reg_n_10_team)


TACE_post_staff %>%
  filter(tace_staff_reg_n_10_team == "GAST" | tace_staff_reg_n_10_team == "Gastro" | tace_staff_reg_n_10_team == "GAST") %>%
  summarise(sum = sum(tace_staff_reg_n_10_hours),
            cost = sum*Registered_nurse)






######### Clinical_nurse
TACE_post_staff %>%
  summarise(max = max(tace_staff_cln_n)) # 2


#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_cln_n_01_team = case_when(
    tace_staff_cln_n_01_team %in% 1 ~ "Oncology",
    tace_staff_cln_n_01_team %in% 2 ~ "Radiology",
    tace_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_cln_n_01_team = coalesce(tace_staff_cln_n_01_team, tace_staff_cln_n_01_other))
TACE_post_staff$tace_staff_cln_n_01_team = as.factor(TACE_post_staff$tace_staff_cln_n_01_team)

levels(TACE_post_staff$tace_staff_cln_n_01_team)

TACE_post_staff %>%
  filter(tace_staff_cln_n_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


TACE_post_staff %>%
  filter(tace_staff_cln_n_01_team == "HCC CNC") %>%
  summarise(sum = sum(tace_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)

TACE_post_staff %>%
  filter(tace_staff_cln_n_01_team == "PAH 11") %>%
  summarise(sum = sum(tace_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_cln_n_02_team = case_when(
    tace_staff_cln_n_02_team %in% 1 ~ "Oncology",
    tace_staff_cln_n_02_team %in% 2 ~ "Radiology",
    tace_staff_cln_n_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_cln_n_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_cln_n_02_team = coalesce(tace_staff_cln_n_02_team, tace_staff_cln_n_02_other))
TACE_post_staff$tace_staff_cln_n_02_team = as.factor(TACE_post_staff$tace_staff_cln_n_02_team)

levels(TACE_post_staff$tace_staff_cln_n_02_team)


TACE_post_staff %>%
  filter(tace_staff_cln_n_02_team == "HCC CNC") %>%
  summarise(sum = sum(tace_staff_cln_n_02_hours),
            cost = sum*Clinical_nurse)




############ Assistant_nurse_unit_manager
TACE_post_staff %>%
  summarise(max = max(tace_staff_anum)) # 0



############ Case_manager
TACE_post_staff %>%
  summarise(max = max(tace_staff_case_m)) # 0




############## Nurse_unit_manager	
TACE_post_staff %>%
  summarise(max = max(tace_staff_nur_um)) # 1


#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_nur_um_01_team = case_when(
    tace_staff_nur_um_01_team %in% 1 ~ "Oncology",
    tace_staff_nur_um_01_team %in% 2 ~ "Radiology",
    tace_staff_nur_um_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_nur_um_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_nur_um_01_team = coalesce(tace_staff_nur_um_01_team, tace_staff_nur_um_01_other))
TACE_post_staff$tace_staff_nur_um_01_team = as.factor(TACE_post_staff$tace_staff_nur_um_01_team)

levels(TACE_post_staff$tace_staff_nur_um_01_team)


TACE_post_staff %>%
  filter(tace_staff_nur_um_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Nurse_unit_manager)





############## Intern_or_Resident
TACE_post_staff %>%
  summarise(max = max(tace_staff_in_res)) # 14

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_01_team = case_when(
    tace_staff_in_res_01_team %in% 1 ~ "Oncology",
    tace_staff_in_res_01_team %in% 2 ~ "Radiology",
    tace_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_01_team = coalesce(tace_staff_in_res_01_team, tace_staff_in_res_01_other))
TACE_post_staff$tace_staff_in_res_01_team = as.factor(TACE_post_staff$tace_staff_in_res_01_team)

levels(TACE_post_staff$tace_staff_in_res_01_team)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "GAST" | tace_staff_in_res_01_team == "Gastro" | tace_staff_in_res_01_team == "GASTRO" |
           tace_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "LIVR" ) %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "Renal" ) %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_01_team == "Surgery" | tace_staff_in_res_01_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_02_team = case_when(
    tace_staff_in_res_02_team %in% 1 ~ "Oncology",
    tace_staff_in_res_02_team %in% 2 ~ "Radiology",
    tace_staff_in_res_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_02_team = coalesce(tace_staff_in_res_02_team, tace_staff_in_res_02_other))
TACE_post_staff$tace_staff_in_res_02_team = as.factor(TACE_post_staff$tace_staff_in_res_02_team)

levels(TACE_post_staff$tace_staff_in_res_02_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_02_team == "GAST" | tace_staff_in_res_02_team == "Gastro" | tace_staff_in_res_02_team == "GASTRO" |
           tace_staff_in_res_02_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_02_team == "LIVR" ) %>%
  summarise(sum = sum(tace_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_02_team == "Radiology" ) %>%
  summarise(sum = sum(tace_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_02_team == "Surgery" | tace_staff_in_res_02_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

#3
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_03_team = case_when(
    tace_staff_in_res_03_team %in% 1 ~ "Oncology",
    tace_staff_in_res_03_team %in% 2 ~ "Radiology",
    tace_staff_in_res_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_03_team = coalesce(tace_staff_in_res_03_team, tace_staff_in_res_03_other))
TACE_post_staff$tace_staff_in_res_03_team = as.factor(TACE_post_staff$tace_staff_in_res_03_team)

levels(TACE_post_staff$tace_staff_in_res_03_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_03_team == "GAST" | tace_staff_in_res_03_team == "Gastro" | tace_staff_in_res_03_team == "GASTRO" |
           tace_staff_in_res_03_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_03_hours),
            cost = sum*Intern_or_Resident)

#4
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_04_team = case_when(
    tace_staff_in_res_04_team %in% 1 ~ "Oncology",
    tace_staff_in_res_04_team %in% 2 ~ "Radiology",
    tace_staff_in_res_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_04_team = coalesce(tace_staff_in_res_04_team, tace_staff_in_res_04_other))
TACE_post_staff$tace_staff_in_res_04_team = as.factor(TACE_post_staff$tace_staff_in_res_04_team)

levels(TACE_post_staff$tace_staff_in_res_04_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_04_team == "GAST" | tace_staff_in_res_04_team == "Gastro" | tace_staff_in_res_04_team == "GASTRO" |
           tace_staff_in_res_04_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_04_hours),
            cost = sum*Intern_or_Resident)

TACE_post_staff %>%
  filter(tace_staff_in_res_04_team == "Vascular") %>%
  summarise(sum = sum(tace_staff_in_res_04_hours),
            cost = sum*Intern_or_Resident)



#5
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_05_team = case_when(
    tace_staff_in_res_05_team %in% 1 ~ "Oncology",
    tace_staff_in_res_05_team %in% 2 ~ "Radiology",
    tace_staff_in_res_05_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_05_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_05_team = coalesce(tace_staff_in_res_05_team, tace_staff_in_res_05_other))
TACE_post_staff$tace_staff_in_res_05_team = as.factor(TACE_post_staff$tace_staff_in_res_05_team)

levels(TACE_post_staff$tace_staff_in_res_05_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_05_team == "GAST" | tace_staff_in_res_05_team == "Gastro" | tace_staff_in_res_05_team == "GASTRO" |
           tace_staff_in_res_05_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_05_hours),
            cost = sum*Intern_or_Resident)

#6
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_06_team = case_when(
    tace_staff_in_res_06_team %in% 1 ~ "Oncology",
    tace_staff_in_res_06_team %in% 2 ~ "Radiology",
    tace_staff_in_res_06_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_06_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_06_team = coalesce(tace_staff_in_res_06_team, tace_staff_in_res_06_other))
TACE_post_staff$tace_staff_in_res_06_team = as.factor(TACE_post_staff$tace_staff_in_res_06_team)

levels(TACE_post_staff$tace_staff_in_res_06_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_06_team == "GAST" | tace_staff_in_res_06_team == "Gastro" | tace_staff_in_res_06_team == "GASTRO" |
           tace_staff_in_res_06_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_06_hours),
            cost = sum*Intern_or_Resident)

#7
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_07_team = case_when(
    tace_staff_in_res_07_team %in% 1 ~ "Oncology",
    tace_staff_in_res_07_team %in% 2 ~ "Radiology",
    tace_staff_in_res_07_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_07_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_07_team = coalesce(tace_staff_in_res_07_team, tace_staff_in_res_07_other))
TACE_post_staff$tace_staff_in_res_07_team = as.factor(TACE_post_staff$tace_staff_in_res_07_team)

levels(TACE_post_staff$tace_staff_in_res_07_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_07_team == "GAST" | tace_staff_in_res_07_team == "Gastro" | tace_staff_in_res_07_team == "GASTRO" |
           tace_staff_in_res_07_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_07_hours),
            cost = sum*Intern_or_Resident)

#8
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_08_team = case_when(
    tace_staff_in_res_08_team %in% 1 ~ "Oncology",
    tace_staff_in_res_08_team %in% 2 ~ "Radiology",
    tace_staff_in_res_08_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_08_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_08_team = coalesce(tace_staff_in_res_08_team, tace_staff_in_res_08_other))
TACE_post_staff$tace_staff_in_res_08_team = as.factor(TACE_post_staff$tace_staff_in_res_08_team)

levels(TACE_post_staff$tace_staff_in_res_08_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_08_team == "GAST" | tace_staff_in_res_08_team == "Gastro" | tace_staff_in_res_08_team == "GASTRO" |
           tace_staff_in_res_08_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_08_hours),
            cost = sum*Intern_or_Resident)

#9
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_09_team = case_when(
    tace_staff_in_res_09_team %in% 1 ~ "Oncology",
    tace_staff_in_res_09_team %in% 2 ~ "Radiology",
    tace_staff_in_res_09_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_09_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_09_team = coalesce(tace_staff_in_res_09_team, tace_staff_in_res_09_other))
TACE_post_staff$tace_staff_in_res_09_team = as.factor(TACE_post_staff$tace_staff_in_res_09_team)

levels(TACE_post_staff$tace_staff_in_res_09_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_09_team == "GAST" | tace_staff_in_res_09_team == "Gastro" | tace_staff_in_res_09_team == "GASTRO" |
           tace_staff_in_res_09_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_09_hours),
            cost = sum*Intern_or_Resident)


#10
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_10_team = case_when(
    tace_staff_in_res_10_team %in% 1 ~ "Oncology",
    tace_staff_in_res_10_team %in% 2 ~ "Radiology",
    tace_staff_in_res_10_team %in% 3 ~ "Nuclear medicine",
    tace_staff_in_res_10_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_in_res_10_team = coalesce(tace_staff_in_res_10_team, tace_staff_in_res_10_other))
TACE_post_staff$tace_staff_in_res_10_team = as.factor(TACE_post_staff$tace_staff_in_res_10_team)

levels(TACE_post_staff$tace_staff_in_res_10_team)


TACE_post_staff %>%
  filter(tace_staff_in_res_10_team == "GAST" | tace_staff_in_res_10_team == "Gastro" | tace_staff_in_res_10_team == "GASTRO" |
           tace_staff_in_res_10_team == "GAST") %>%
  summarise(sum = sum(tace_staff_in_res_10_hours),
            cost = sum*Intern_or_Resident)





############# Registrar
TACE_post_staff %>%
  summarise(max = max(tace_staff_registrar)) # 14

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_01_team = case_when(
    tace_staff_registrar_01_team %in% 1 ~ "Oncology",
    tace_staff_registrar_01_team %in% 2 ~ "Radiology",
    tace_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_01_team = coalesce(tace_staff_registrar_01_team, tace_staff_registrar_01_other))
TACE_post_staff$tace_staff_registrar_01_team = as.factor(TACE_post_staff$tace_staff_registrar_01_team)

levels(TACE_post_staff$tace_staff_registrar_01_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "GAST" | tace_staff_registrar_01_team == "Gastro" | tace_staff_registrar_01_team == "GASTRO" |
           tace_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "5D") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)


TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "LIVR") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "Medical") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_01_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_registrar_01_hours),
            cost = sum*Registrar)



#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_02_team = case_when(
    tace_staff_registrar_02_team %in% 1 ~ "Oncology",
    tace_staff_registrar_02_team %in% 2 ~ "Radiology",
    tace_staff_registrar_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_02_team = coalesce(tace_staff_registrar_02_team, tace_staff_registrar_02_other))
TACE_post_staff$tace_staff_registrar_02_team = as.factor(TACE_post_staff$tace_staff_registrar_02_team)

levels(TACE_post_staff$tace_staff_registrar_02_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "GAST" | tace_staff_registrar_02_team == "Gastro" | tace_staff_registrar_02_team == "GASTRO" |
           tace_staff_registrar_02_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "HPB") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)


TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "LIVR") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "Medical") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_02_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_registrar_02_hours),
            cost = sum*Registrar)


#3
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_03_team = case_when(
    tace_staff_registrar_03_team %in% 1 ~ "Oncology",
    tace_staff_registrar_03_team %in% 2 ~ "Radiology",
    tace_staff_registrar_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_03_team = coalesce(tace_staff_registrar_03_team, tace_staff_registrar_03_other))
TACE_post_staff$tace_staff_registrar_03_team = as.factor(TACE_post_staff$tace_staff_registrar_03_team)

levels(TACE_post_staff$tace_staff_registrar_03_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_03_team == "GAST" | tace_staff_registrar_03_team == "Gastro" | tace_staff_registrar_03_team == "GASTRO" |
           tace_staff_registrar_03_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_03_hours),
            cost = sum*Registrar)

TACE_post_staff %>%
  filter(tace_staff_registrar_03_team == "HPB") %>%
  summarise(sum = sum(tace_staff_registrar_03_hours),
            cost = sum*Registrar)


TACE_post_staff %>%
  filter(tace_staff_registrar_03_team == "Vascular") %>%
  summarise(sum = sum(tace_staff_registrar_03_hours),
            cost = sum*Registrar)


#4
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_04_team = case_when(
    tace_staff_registrar_04_team %in% 1 ~ "Oncology",
    tace_staff_registrar_04_team %in% 2 ~ "Radiology",
    tace_staff_registrar_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_04_team = coalesce(tace_staff_registrar_04_team, tace_staff_registrar_04_other))
TACE_post_staff$tace_staff_registrar_04_team = as.factor(TACE_post_staff$tace_staff_registrar_04_team)

levels(TACE_post_staff$tace_staff_registrar_04_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_04_team == "GAST" | tace_staff_registrar_04_team == "Gastro" | tace_staff_registrar_04_team == "GASTRO" |
           tace_staff_registrar_04_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_04_hours),
            cost = sum*Registrar)



#5
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_05_team = case_when(
    tace_staff_registrar_05_team %in% 1 ~ "Oncology",
    tace_staff_registrar_05_team %in% 2 ~ "Radiology",
    tace_staff_registrar_05_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_05_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_05_team = coalesce(tace_staff_registrar_05_team, tace_staff_registrar_05_other))
TACE_post_staff$tace_staff_registrar_05_team = as.factor(TACE_post_staff$tace_staff_registrar_05_team)

levels(TACE_post_staff$tace_staff_registrar_05_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_05_team == "GAST" | tace_staff_registrar_05_team == "Gastro" | tace_staff_registrar_05_team == "GASTRO" |
           tace_staff_registrar_05_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_05_hours),
            cost = sum*Registrar)



#6
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_06_team = case_when(
    tace_staff_registrar_06_team %in% 1 ~ "Oncology",
    tace_staff_registrar_06_team %in% 2 ~ "Radiology",
    tace_staff_registrar_06_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_06_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_06_team = coalesce(tace_staff_registrar_06_team, tace_staff_registrar_06_other))
TACE_post_staff$tace_staff_registrar_06_team = as.factor(TACE_post_staff$tace_staff_registrar_06_team)

levels(TACE_post_staff$tace_staff_registrar_06_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_06_team == "GAST" | tace_staff_registrar_06_team == "Gastro" | tace_staff_registrar_06_team == "GASTRO" |
           tace_staff_registrar_06_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_06_hours),
            cost = sum*Registrar)



#7
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_07_team = case_when(
    tace_staff_registrar_07_team %in% 1 ~ "Oncology",
    tace_staff_registrar_07_team %in% 2 ~ "Radiology",
    tace_staff_registrar_07_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_07_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_07_team = coalesce(tace_staff_registrar_07_team, tace_staff_registrar_07_other))
TACE_post_staff$tace_staff_registrar_07_team = as.factor(TACE_post_staff$tace_staff_registrar_07_team)

levels(TACE_post_staff$tace_staff_registrar_07_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_07_team == "GAST" | tace_staff_registrar_07_team == "Gastro" | tace_staff_registrar_07_team == "GASTRO" |
           tace_staff_registrar_07_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_07_hours),
            cost = sum*Registrar)



#8
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_08_team = case_when(
    tace_staff_registrar_08_team %in% 1 ~ "Oncology",
    tace_staff_registrar_08_team %in% 2 ~ "Radiology",
    tace_staff_registrar_08_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_08_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_08_team = coalesce(tace_staff_registrar_08_team, tace_staff_registrar_08_other))
TACE_post_staff$tace_staff_registrar_08_team = as.factor(TACE_post_staff$tace_staff_registrar_08_team)

levels(TACE_post_staff$tace_staff_registrar_08_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_08_team == "GAST" | tace_staff_registrar_08_team == "Gastro" | tace_staff_registrar_08_team == "GASTRO" |
           tace_staff_registrar_08_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_08_hours),
            cost = sum*Registrar)



#9
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_09_team = case_when(
    tace_staff_registrar_09_team %in% 1 ~ "Oncology",
    tace_staff_registrar_09_team %in% 2 ~ "Radiology",
    tace_staff_registrar_09_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_09_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_09_team = coalesce(tace_staff_registrar_09_team, tace_staff_registrar_09_other))
TACE_post_staff$tace_staff_registrar_09_team = as.factor(TACE_post_staff$tace_staff_registrar_09_team)

levels(TACE_post_staff$tace_staff_registrar_09_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_09_team == "GAST" | tace_staff_registrar_09_team == "Gastro" | tace_staff_registrar_09_team == "GASTRO" |
           tace_staff_registrar_09_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_09_hours),
            cost = sum*Registrar)



#10
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_10_team = case_when(
    tace_staff_registrar_10_team %in% 1 ~ "Oncology",
    tace_staff_registrar_10_team %in% 2 ~ "Radiology",
    tace_staff_registrar_10_team %in% 3 ~ "Nuclear medicine",
    tace_staff_registrar_10_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_registrar_10_team = coalesce(tace_staff_registrar_10_team, tace_staff_registrar_10_other))
TACE_post_staff$tace_staff_registrar_10_team = as.factor(TACE_post_staff$tace_staff_registrar_10_team)

levels(TACE_post_staff$tace_staff_registrar_10_team)

TACE_post_staff %>%
  filter(tace_staff_registrar_10_team == "GAST" | tace_staff_registrar_10_team == "Gastro" | tace_staff_registrar_10_team == "GASTRO" |
           tace_staff_registrar_10_team == "GAST") %>%
  summarise(sum = sum(tace_staff_registrar_10_hours),
            cost = sum*Registrar)





############ Fellow
TACE_post_staff %>%
  summarise(max = max(tace_staff_fellow)) # 2

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_fellow_01_team = case_when(
    tace_staff_fellow_01_team %in% 1 ~ "Oncology",
    tace_staff_fellow_01_team %in% 2 ~ "Radiology",
    tace_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_fellow_01_team = coalesce(tace_staff_fellow_01_team, tace_staff_fellow_01_other))
TACE_post_staff$tace_staff_fellow_01_team = as.factor(TACE_post_staff$tace_staff_fellow_01_team)

levels(TACE_post_staff$tace_staff_fellow_01_team)

TACE_post_staff %>%
  filter(tace_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_fellow_01_hours),
            cost = sum*Fellow)

TACE_post_staff %>%
  filter(tace_staff_fellow_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_fellow_01_hours),
            cost = sum*Fellow)

TACE_post_staff %>%
  filter(tace_staff_fellow_01_team == "GAST" | tace_staff_fellow_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_fellow_01_hours),
            cost = sum*Fellow)


#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_fellow_02_team = case_when(
    tace_staff_fellow_02_team %in% 1 ~ "Oncology",
    tace_staff_fellow_02_team %in% 2 ~ "Radiology",
    tace_staff_fellow_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_fellow_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_fellow_02_team = coalesce(tace_staff_fellow_02_team, tace_staff_fellow_02_other))
TACE_post_staff$tace_staff_fellow_02_team = as.factor(TACE_post_staff$tace_staff_fellow_02_team)

levels(TACE_post_staff$tace_staff_fellow_02_team)

TACE_post_staff %>%
  filter(tace_staff_fellow_02_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_fellow_02_hours),
            cost = sum*Fellow)

TACE_post_staff %>%
  filter(tace_staff_fellow_02_team == "GAST" | tace_staff_fellow_02_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_fellow_02_hours),
            cost = sum*Fellow)





################# Consultant
TACE_post_staff %>%
  summarise(max = max(tace_staff_consultant)) # 4

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_01_team = case_when(
    tace_staff_consultant_01_team %in% 1 ~ "Oncology",
    tace_staff_consultant_01_team %in% 2 ~ "Radiology",
    tace_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_01_team = coalesce(tace_staff_consultant_01_team, tace_staff_consultant_01_other))
TACE_post_staff$tace_staff_consultant_01_team = as.factor(TACE_post_staff$tace_staff_consultant_01_team)

levels(TACE_post_staff$tace_staff_consultant_01_team)

TACE_post_staff %>%
  filter(tace_staff_consultant_01_team == "GAST" | tace_staff_consultant_01_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)

TACE_post_staff %>%
  filter(tace_staff_consultant_01_team == "HPB") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)

TACE_post_staff %>%
  filter(tace_staff_consultant_01_team == "LIVR") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)

TACE_post_staff %>%
  filter(tace_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)

TACE_post_staff %>%
  filter(tace_staff_consultant_01_team == "Surgical") %>%
  summarise(sum = sum(tace_staff_consultant_01_hours),
            cost = sum*Consultant)



#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_02_team = case_when(
    tace_staff_consultant_02_team %in% 1 ~ "Oncology",
    tace_staff_consultant_02_team %in% 2 ~ "Radiology",
    tace_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_02_team = coalesce(tace_staff_consultant_02_team, tace_staff_consultant_02_other))
TACE_post_staff$tace_staff_consultant_02_team = as.factor(TACE_post_staff$tace_staff_consultant_02_team)

levels(TACE_post_staff$tace_staff_consultant_02_team)

TACE_post_staff %>%
  filter(tace_staff_consultant_02_team == "Gastro") %>%
  summarise(sum = sum(tace_staff_consultant_02_hours),
            cost = sum*Consultant)

TACE_post_staff %>%
  filter(tace_staff_consultant_02_team == "LIVR") %>%
  summarise(sum = sum(tace_staff_consultant_02_hours),
            cost = sum*Consultant)



#3
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_03_team = case_when(
    tace_staff_consultant_03_team %in% 1 ~ "Oncology",
    tace_staff_consultant_03_team %in% 2 ~ "Radiology",
    tace_staff_consultant_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_03_team = coalesce(tace_staff_consultant_03_team, tace_staff_consultant_03_other))
TACE_post_staff$tace_staff_consultant_03_team = as.factor(TACE_post_staff$tace_staff_consultant_03_team)

levels(TACE_post_staff$tace_staff_consultant_03_team)

TACE_post_staff %>%
  filter(tace_staff_consultant_03_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_03_hours),
            cost = sum*Consultant)


#4
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_04_team = case_when(
    tace_staff_consultant_04_team %in% 1 ~ "Oncology",
    tace_staff_consultant_04_team %in% 2 ~ "Radiology",
    tace_staff_consultant_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_consultant_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_consultant_04_team = coalesce(tace_staff_consultant_04_team, tace_staff_consultant_04_other))
TACE_post_staff$tace_staff_consultant_04_team = as.factor(TACE_post_staff$tace_staff_consultant_04_team)

levels(TACE_post_staff$tace_staff_consultant_04_team)

TACE_post_staff %>%
  filter(tace_staff_consultant_04_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_consultant_04_hours),
            cost = sum*Consultant)




############### Radiographer
TACE_post_staff %>%
  summarise(max = max(tace_staff_radio)) # 4

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_01_team = case_when(
    tace_staff_radio_01_team %in% 1 ~ "Oncology",
    tace_staff_radio_01_team %in% 2 ~ "Radiology",
    tace_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_01_team = coalesce(tace_staff_radio_01_team, tace_staff_radio_01_other))
TACE_post_staff$tace_staff_radio_01_team = as.factor(TACE_post_staff$tace_staff_radio_01_team)

levels(TACE_post_staff$tace_staff_radio_01_team)

TACE_post_staff %>%
  filter(tace_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_02_team = case_when(
    tace_staff_radio_02_team %in% 1 ~ "Oncology",
    tace_staff_radio_02_team %in% 2 ~ "Radiology",
    tace_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_02_team = coalesce(tace_staff_radio_02_team, tace_staff_radio_02_other))
TACE_post_staff$tace_staff_radio_02_team = as.factor(TACE_post_staff$tace_staff_radio_02_team)

levels(TACE_post_staff$tace_staff_radio_02_team)

TACE_post_staff %>%
  filter(tace_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_02_hours),
            cost = sum*Radiographer)

#3
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_03_team = case_when(
    tace_staff_radio_03_team %in% 3 ~ "Oncology",
    tace_staff_radio_03_team %in% 2 ~ "Radiology",
    tace_staff_radio_03_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_03_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_03_team = coalesce(tace_staff_radio_03_team, tace_staff_radio_03_other))
TACE_post_staff$tace_staff_radio_03_team = as.factor(TACE_post_staff$tace_staff_radio_03_team)

levels(TACE_post_staff$tace_staff_radio_03_team)

TACE_post_staff %>%
  filter(tace_staff_radio_03_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_03_hours),
            cost = sum*Radiographer)

#4
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_04_team = case_when(
    tace_staff_radio_04_team %in% 4 ~ "Oncology",
    tace_staff_radio_04_team %in% 2 ~ "Radiology",
    tace_staff_radio_04_team %in% 3 ~ "Nuclear medicine",
    tace_staff_radio_04_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_radio_04_team = coalesce(tace_staff_radio_04_team, tace_staff_radio_04_other))
TACE_post_staff$tace_staff_radio_04_team = as.factor(TACE_post_staff$tace_staff_radio_04_team)

levels(TACE_post_staff$tace_staff_radio_04_team)

TACE_post_staff %>%
  filter(tace_staff_radio_04_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_radio_04_hours),
            cost = sum*Radiographer)





############# Ultrasonographer
TACE_post_staff %>%
  summarise(max = max(tace_staff_ultra)) # 2

#1
TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_ultra_01_team = case_when(
    tace_staff_ultra_01_team %in% 1 ~ "Oncology",
    tace_staff_ultra_01_team %in% 2 ~ "Radiology",
    tace_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    tace_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


TACE_post_staff = TACE_post_staff %>%
  mutate(tace_staff_ultra_01_team = coalesce(tace_staff_ultra_01_team, tace_staff_ultra_01_other))
TACE_post_staff$tace_staff_ultra_01_team = as.factor(TACE_post_staff$tace_staff_ultra_01_team)

levels(TACE_post_staff$tace_staff_ultra_01_team)

TACE_post_staff %>%
  filter(tace_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(tace_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)





################ Nuc_Med_Tech
TACE_post_staff %>%
  summarise(max = max(tace_staff_nuc_mt)) # 0




###################### NM_physicist
TACE_post_staff %>%
  summarise(max = max(tace_staff_nm_ph)) # 0



########################### Labour_nuclear_medicine
TACE_post_staff %>%
  summarise(max = max(tace_staff_mab_nm)) # 0



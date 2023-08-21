# MN Oncology porject - ablation



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
abl_data = read.delim("ablation_data.csv", sep = ",") %>% clean_names()






##############################################################
############ Demographic characteristics #####################
##############################################################

abl_demo = abl_data %>%
  select(1: 13) %>%
  filter(redcap_repeat_instrument == "demo")



# Age
abl_demo %>%
  summarise(age_mean = mean(age),
            sd = sd(age))


# Gender
abl_demo %>%
  filter(sex == 1) %>%
  summarise(num = n(),
            per = num/41) # Change the number


# primary_site
abl_demo$primary_site = as.factor(abl_demo$primary_site)

abl_demo %>%
  group_by(primary_site) %>%
  summarise(num = n(),
            per = num/41) # Change the number


# secondary_site
abl_demo$secondary_site = as.factor(abl_demo$secondary_site)

abl_demo %>%
  group_by(secondary_site) %>%
  summarise(num = n(),
            per = num/41) # Change the number


# albi_score
abl_demo$albi_score = as.factor(abl_demo$albi_score)

abl_demo %>%
  group_by(albi_score) %>%
  summarise(num = n(),
            per = num/41) # Change the number


# albi_score_at_abl
abl_demo$albi_score_at_abl = as.factor(abl_demo$albi_score_at_abl)

abl_demo %>%
  group_by(albi_score_at_tace) %>%
  summarise(num = n(),
            per = num/41) # Change the number




##############################################################
################## admission_details  ########################
##############################################################

abl_admin = abl_data %>%
  select(1:4, admin_reason : wau_received) %>%
  filter(redcap_repeat_instrument == "admission_details")


# admin_reason



#  days
abl_admin$days = as.factor(abl_admin$days)

abl_admin %>%
  group_by(days) %>%
  summarise(num = n(),
            per = num/41) # Change the number

abl_admin$days = as.double(abl_admin$days)
abl_admin %>%
  summarise(ave = mean(days),
            sd = sd(days))


# ar_drg
abl_admin$ar_drg = as.factor(abl_admin$ar_drg)

tab_1 = abl_admin %>%
  group_by(ar_drg) %>%
  summarise(num = n(),
            per = num/41) # Change the number

write.csv(tab_1, "Test_tab_1.csv")



# wau
#### Boot strapping - mean & 95% CI


set.seed(13579)
n = 41
B = 100000

Boot.tot.wau.1 <- matrix(sample(abl_admin$wau, size= B*n, 
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




abl_admin$wau = as.factor(abl_admin$wau)

tab_1 = abl_admin %>%
  group_by(wau) %>%
  summarise(num = n(),
            per = num/41) # Change the number

write.csv(tab_1, "Test_tab_1.csv")

abl_admin$wau = as.double(abl_admin$wau)
abl_admin %>%
  summarise(mean = mean(wau),
            sd = sd(wau))






# admit_ward vs wau_received
abl_admin$admit_ward = as.factor(abl_admin$admit_ward)
abl_admin$wau_received = as.factor(abl_admin$wau_received)

levels(abl_admin$admit_ward)
levels(abl_admin$wau_received)


Wau_fun = function(v1, v2) {
  abl_admin %>%
    filter(admit_ward == v1) %>%
    group_by(wau_received) %>%
    summarise(num = n(),
              per = num/v2) # Change the number
  
  
}

Wau_fun (v1 = "23 HR", v2 = 19)
Wau_fun (v1 = "23HR", v2 = 1)
Wau_fun (v1 = "2A", v2 = 3)
Wau_fun (v1 = "2C", v2 = 47)
Wau_fun (v1 = "4BR", v2 = 7)
Wau_fun (v1 = "4C", v2 = 1)
Wau_fun (v1 = "4E", v2 = 1)
Wau_fun (v1 = "4E GAST", v2 = 1)
Wau_fun (v1 = "6BN", v2 = 1)
Wau_fun (v1 = "7BW", v2 = 2)
Wau_fun (v1 = "8BS", v2 = 2)
Wau_fun (v1 = "9AN", v2 = 1)
Wau_fun (v1 = "9AS", v2 = 1)
Wau_fun (v1 = "9BS", v2 = 1)
Wau_fun (v1 = "CCU", v2 = 1)
Wau_fun (v1 = "SCU", v2 = 1)


##############################################################
############################## proms  ########################
##############################################################

abl_proms = abl_data %>%
  select(1:4, proms_time : proms_vas) %>%
  filter(!is.na(proms_time))


write.csv(abl_proms, "abl_proms.csv")

abl_proms = read.delim("abl_proms_1.csv", sep = ",") %>% clean_names() # added the utlity score

abl_proms$proms_vas = as.double(abl_proms$proms_vas)


abl_proms %>%
  filter(proms_time == 1 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas))

abl_proms %>%
  filter(proms_time == 5 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas))



##############################################################
############################## abl procedure  ###############
##############################################################

abl_proced = abl_data %>%
  select(1:4, abl_procedure : abl_post_bed) %>%
  filter(redcap_repeat_instrument == "ablation") %>%
  mutate(abl_out_time_1 = abl_out_time)


abl_proced = separate(abl_proced, col = abl_out_time_1, into = c("hours", "minutes")) # Diff time produce negative values as post time is next day

abl_proced$hours = as.numeric(abl_proced$hours)
abl_proced$minutes = as.numeric(abl_proced$minutes)

abl_proced$abl_out_time_2 = make_datetime(year = 0000, month = 01, day = 02, hour = abl_proced$hours, min = abl_proced$minutes) # Created a separate column with the same time next day


abl_proced$abl_in_room = parse_date_time(abl_proced$abl_in_room, orders = c("hm")) # Procedure time
abl_proced$abl_out_time = parse_date_time(abl_proced$abl_out_time, orders = c("hm")) 

abl_proced$abl_start_reco = parse_date_time(abl_proced$abl_start_reco, orders = c("hm")) # Recovery time
abl_proced$abl_end_recov = parse_date_time(abl_proced$abl_end_recov, orders = c("hm"))



# time difference - Procedure
abl_proced$timediff = difftime(abl_proced$abl_out_time, abl_proced$abl_in_room, units = "hours") # this has negative time points


abl_proced = abl_proced %>%
  mutate(abl_out_time_3 = if_else (timediff > 0, abl_proced$abl_out_time, abl_proced$abl_out_time_2))

abl_proced$timediff_procedure = difftime(abl_proced$abl_out_time_3, abl_proced$abl_in_room, units = "hours")



# time difference - recovery
abl_proced$timediff_recovery = difftime(abl_proced$abl_end_recov, abl_proced$abl_start_reco, units = "hours") # this has negative time points


# Calculating average time

abl_proced %>%
  filter(abl_procedure == 1) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))

abl_proced %>%
  filter(abl_procedure == 2) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))

abl_proced %>%
  filter(abl_procedure == 3 & !(is.na(timediff_procedure))) %>%
  summarise(num = n (),
            mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure))


abl_proced %>% # Recovery time
  filter(abl_procedure == 2) %>%
  summarise(mean = mean(timediff_recovery),
            median = median(timediff_recovery),
            sd = sd(timediff_recovery))


# Recovery place
abl_proced %>%
  filter(abl_procedure == 2) %>%
  group_by(abl_recovery_place) %>%
  summarise(num = n(),
            ave = num/41)



###########A##################################################
############################## abl imaging  ###############
##############################################################

abl_imaging = abl_data %>%
  select(1:4, abl_procedure, abl_imaging_text : abl_other_2) %>%
  filter(redcap_repeat_instrument == "ablation")


abl_imagindg_cost = read.delim("Imaging_cost_abl.csv", sep = ",") %>% clean_names() # Imaging cost


abl_imaging_1 = abl_imaging %>%
  select(1, 5, 7:17)


abl_imaging_2 = gather(abl_imaging_1, key = imaging_method, value = number, -1, -2)

abl_imaging_2$number = as.double(abl_imaging_2$number)

abl_imaging_2 = abl_imaging_2 %>%
  mutate(number_1 = if_else (is.na(number), 0, abl_imaging_2$number))


abl_imaging_2 = abl_imaging_2 %>%
  mutate(num_used = if_else(number_1 > 0, 1, 0))


# Imaging numbers
abl_imaging_2 %>%
  filter(abl_procedure == 1) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/41)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))

abl_imaging_2 %>%
  filter(abl_procedure == 2) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/41)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))

abl_imaging_2 %>%
  filter(abl_procedure == 3) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/41)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))



# Merge with cost variable
abl_imaging_3 = merge(x = abl_imaging_2, y = abl_imagindg_cost, by = "imaging_method", all = TRUE)



abl_imaging_3 = abl_imaging_3 %>%
  mutate(total_cost = number_1 * cost)

abl_imaging_3 %>%
  filter(abl_procedure == 1 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in abl_procedure == 1 --> 31
abl_imaging_3 %>%
  filter(abl_procedure == 1) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 31)


abl_imaging_3 %>%
  filter(abl_procedure == 2 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in abl_procedure == 1 --> 40
abl_imaging_3 %>%
  filter(abl_procedure == 2) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 40)


abl_imaging_3 %>%
  filter(abl_procedure == 3 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in abl_procedure == 1 --> 16
abl_imaging_3 %>%
  filter(abl_procedure == 3) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 16)



#### Boot strapping

## Total cost
abl_imaging_4 = aggregate(abl_imaging_3$total_cost, by = list(abl_imaging_3$record_id), FUN = sum)


colnames(abl_imaging_4) = c("record_id", "abl_imaging_cost")


abl_imaging_4 %>%
  summarise(sum = sum(abl_imaging_cost))


## -- Boot strapping -- ##
set.seed(13579)
n = 41
B = 100000

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.tot.imaging_cost.1 <- matrix(sample(abl_imaging_4$abl_imaging_cost, size= B*n, 
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
abl_imaging_pre = abl_imaging_3 %>%
  filter(abl_procedure == 1) 

abl_imaging_pre = aggregate(abl_imaging_pre$total_cost, by = list(abl_imaging_pre$record_id), FUN = sum)

colnames(abl_imaging_pre) = c("record_id", "cost_pre_imaging")


abl_imaging_pre %>%
  summarise(sum = sum(cost_pre_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.pre.imaging_cost.1 <- matrix(sample(abl_imaging_pre$cost_pre_imaging, size= B*n, 
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
abl_imaging_intra = abl_imaging_3 %>%
  filter(abl_procedure == 2) 

abl_imaging_intra = aggregate(abl_imaging_intra$total_cost, by = list(abl_imaging_intra$record_id), FUN = sum)

colnames(abl_imaging_intra) = c("record_id", "cost_intra_imaging")

abl_imaging_intra %>%
  summarise(sum = sum(cost_intra_imaging))


## -- Boot strapping -- ##

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.intra.imagin_cost.1 <- matrix(sample(abl_imaging_intra$cost_intra_imaging, size= B*n, 
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
abl_imaging_post = abl_imaging_3 %>%
  filter(abl_procedure == 3) 

abl_imaging_post = aggregate(abl_imaging_post$total_cost, by = list(abl_imaging_post$record_id), FUN = sum)

colnames(abl_imaging_post) = c("record_id", "cost_post_imaging")


abl_imaging_post %>%
  summarise(sum = sum(cost_post_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.post.imaging_cost.1 <- matrix(sample(abl_imaging_post$cost_post_imaging, size= B*n, 
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

abl_consumableMed_cost = read.delim("abl_consumable_cost.csv", sep = ",") %>% clean_names()


########### Pre- procedure #################

abl_pre_ConMed = abl_data %>%
  filter(abl_consu_pre == 1) %>%
  select(1, 3, abl_pre_bd_cannula : abl_pre_freestyle)


abl_pre_ConMed_1 = gather(abl_pre_ConMed, key = item_name, value = number, -1, -2)

abl_pre_ConMed_1$number = as.double(abl_pre_ConMed_1$number)

abl_pre_ConMed_2 = abl_pre_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, abl_pre_ConMed_1$number))

abl_pre_ConMed_2 = abl_pre_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

abl_pre_ConMed_2$item_name = as.factor(abl_pre_ConMed_2$item_name)



# Merge with cost variable
abl_pre_ConMed_2 = merge(x = abl_pre_ConMed_2, y = abl_consumableMed_cost, by = "item_name", all = TRUE)


abl_pre_ConMed_2 = abl_pre_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)

# Aggregate with item
abl_pre_ConMed_2a = abl_pre_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(item_cost = median(cost),
            num_items = sum(number_ConMed),
            tot_cot = sum(tot_cost))
write.csv(abl_pre_ConMed_2a, file = "Consumables/Abl_pre_Con.csv")




# Aggregate cost per person
abl_pre_ConMed_3 = aggregate(abl_pre_ConMed_2$tot_cost, by = list(abl_pre_ConMed_2$record_id), FUN = sum)

colnames(abl_pre_ConMed_3) = c("record_id", "cost_pre_conMed")

abl_pre_ConMed_3 %>%
  summarise(sum = sum(cost_pre_conMed))


## -- Boot strapping -- ##


Boot.pre.ConMed_cost.1 <- matrix(sample(abl_pre_ConMed_3$cost_pre_conMed, size= B*n, 
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

abl_intra_ConMed = abl_data %>%
  filter(abl_consu_intra == 1) %>%
  select(1, 3, abl_intra_absorbable_smal : abl_intra_tegaderm_pad, abl_intra_cefazolin : abl_intra_remifentanyl)


abl_intra_ConMed_1 = gather(abl_intra_ConMed, key = item_name, value = number, -1, -2)

abl_intra_ConMed_1$number = as.double(abl_intra_ConMed_1$number)

abl_intra_ConMed_2 = abl_intra_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, abl_intra_ConMed_1$number))

abl_intra_ConMed_2 = abl_intra_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

abl_intra_ConMed_2$item_name = as.factor(abl_intra_ConMed_2$item_name)



# Merge with cost variable
abl_intra_ConMed_2 = merge(x = abl_intra_ConMed_2, y = abl_consumableMed_cost, by = "item_name", all = TRUE)

abl_intra_ConMed_2 = abl_intra_ConMed_2 %>%  # Remove pre
  filter(!is.na(record_id))

abl_intra_ConMed_2 = abl_intra_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)


# Aggregate with item
abl_intra_ConMed_2a = abl_intra_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(item_cost = median(cost),
            num_items = sum(number_ConMed),
            tot_cot = sum(tot_cost))
write.csv(abl_intra_ConMed_2a, file = "Consumables/Abl_intra_Con.csv")



# Aggregate cost per item
abl_intra_ConMed_4 = aggregate(abl_intra_ConMed_2$tot_cost, by = list(abl_intra_ConMed_2$item_name), FUN = sum)
colnames(abl_intra_ConMed_4) = c("item_name", "cost_intra_conMed")

abl_intra_ConMed_4 <- abl_intra_ConMed_4[order(-abl_intra_ConMed_4$cost_intra_conMed),]

abl_intra_ConMed_4 = abl_intra_ConMed_4 %>%
  mutate(per_pt = cost_intra_conMed / 41) 

abl_intra_ConMed_4 %>%
  summarise(sum = sum(per_pt))




# Aggregate cost per person
abl_intra_ConMed_3 = aggregate(abl_intra_ConMed_2$tot_cost, by = list(abl_intra_ConMed_2$record_id), FUN = sum)

colnames(abl_intra_ConMed_3) = c("record_id", "cost_intra_conMed")

abl_intra_ConMed_3 %>%
  summarise(sum = sum(cost_intra_conMed))


## -- Boot strapping -- ##
set.seed(13579)
n = 41
B = 100000

Boot.intra.ConMed_cost.1 <- matrix(sample(abl_intra_ConMed_3$cost_intra_conMed, size= B*n, 
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

abl_post_ConMed = abl_data %>%
  filter(abl_consu_post == 1) %>%
  select(1, 3, abl_post_iv_dot : abl_post_freestyle)


abl_post_ConMed_1 = gather(abl_post_ConMed, key = item_name, value = number, -1, -2)

abl_post_ConMed_1$number = as.double(abl_post_ConMed_1$number)

abl_post_ConMed_2 = abl_post_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, abl_post_ConMed_1$number))

abl_post_ConMed_2 = abl_post_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

abl_post_ConMed_2$item_name = as.factor(abl_post_ConMed_2$item_name)



# Merge with cost variable
abl_post_ConMed_2 = merge(x = abl_post_ConMed_2, y = abl_consumableMed_cost, by = "item_name", all = TRUE)

abl_post_ConMed_2 = abl_post_ConMed_2 %>%  # Remove pre & intra
  filter(!is.na(record_id))

abl_post_ConMed_2 = abl_post_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)


# Aggregate with item
abl_post_ConMed_2a = abl_post_ConMed_2 %>%
  group_by(item_name) %>%
  summarise(item_cost = median(cost),
            num_items = sum(number_ConMed),
            tot_cot = sum(tot_cost))
write.csv(abl_post_ConMed_2a, file = "Consumables/Abl_post_Con.csv")



# Aggregate cost per person
abl_post_ConMed_3 = aggregate(abl_post_ConMed_2$tot_cost, by = list(abl_post_ConMed_2$record_id), FUN = sum)

colnames(abl_post_ConMed_3) = c("record_id", "cost_post_conMed")

abl_post_ConMed_3 %>%
  summarise(sum = sum(cost_post_conMed))


## -- Boot strapping -- ##
set.seed(13579)
n = 40
B = 100000

Boot.post.ConMed_cost.1 <- matrix(sample(abl_post_ConMed_3$cost_post_conMed, size= B*n, 
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
abl_combine_ConMed = merge(x = abl_pre_ConMed_3, y = abl_intra_ConMed_3, by = "record_id", all = TRUE)
abl_combine_ConMed = merge(x = abl_combine_ConMed, y = abl_post_ConMed_3, by = "record_id", all = TRUE)

abl_combine_ConMed[is.na(abl_combine_ConMed)]<-0

abl_combine_ConMed = abl_combine_ConMed %>%
  mutate(total_ConMed = cost_pre_conMed + cost_intra_conMed +cost_post_conMed)


abl_combine_ConMed %>%
  summarise(sum = sum(total_ConMed))


abl_combine_ConMed = abl_combine_ConMed %>%
  select(1, 5)



## -- Boot strapping -- ##
set.seed(13579)
n = 41
B = 100000

Boot.combine.ConMed_cost.1 <- matrix(sample(abl_combine_ConMed$total_ConMed, size= B*n, 
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


abl_staff = abl_data %>%
  select(1, 2, 3,  abl_staff_al: abl_staff_mab_nm_10_hours) %>%
  filter(redcap_repeat_instrument == "ablation")


abl_pre_staff = abl_staff %>%
  filter(redcap_repeat_instance == 1)
abl_intra_staff = abl_staff %>%
  filter(redcap_repeat_instance == 2)  
abl_post_staff = abl_staff %>%
  filter(redcap_repeat_instance == 3)

abl_pre_staff$abl_staff_al_01_other = as.factor(abl_pre_staff$abl_staff_al_01_other)




# To check if number are more than 10 in a staff cat
abl_pre_staff_check = abl_pre_staff %>%
  select(1, abl_staff_al, abl_staff_ass_n, abl_staff_enr_n, abl_staff_reg_n, abl_staff_cln_n, abl_staff_anum, abl_staff_case_m, 
         abl_staff_nur_um, abl_staff_in_res, abl_staff_registrar, abl_staff_fellow, abl_staff_consultant, abl_staff_radio,
         abl_staff_ultra, abl_staff_nuc_mt, abl_staff_nm_ph, abl_staff_mab_nm)

abl_intra_staff_check = abl_intra_staff %>%
  select(1, abl_staff_al, abl_staff_ass_n, abl_staff_enr_n, abl_staff_reg_n, abl_staff_cln_n, abl_staff_anum, abl_staff_case_m, 
         abl_staff_nur_um, abl_staff_in_res, abl_staff_registrar, abl_staff_fellow, abl_staff_consultant, abl_staff_radio,
         abl_staff_ultra, abl_staff_nuc_mt, abl_staff_nm_ph, abl_staff_mab_nm)

abl_post_staff_check = abl_post_staff %>%
  select(1, abl_staff_al, abl_staff_ass_n, abl_staff_enr_n, abl_staff_reg_n, abl_staff_cln_n, abl_staff_anum, abl_staff_case_m, 
         abl_staff_nur_um, abl_staff_in_res, abl_staff_registrar, abl_staff_fellow, abl_staff_consultant, abl_staff_radio,
         abl_staff_ultra, abl_staff_nuc_mt, abl_staff_nm_ph, abl_staff_mab_nm)



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
Registrar	                    = 69.77138889*1.25#
Fellow	                      = 96.49916667*1.25 # 
Consultant	                  = 125.4936111*1.25 #
NM_physicist	                = 125.4936111*1.25 #
Interventional_Radiologist	  = 125.4936111*1.25


abl_pre_staff[is.na(abl_pre_staff)]<-0
abl_intra_staff[is.na(abl_intra_staff)]<-0
abl_post_staff[is.na(abl_post_staff)]<-0


# Administrative_labour
abl_pre_staff  %>%
  filter(!(is.na(abl_staff_al))) %>%
  summarise(sum = sum(abl_staff_al))
abl_intra_staff  %>%
  filter(!(is.na(abl_staff_al))) %>%
  summarise(sum = sum(abl_staff_al))
abl_post_staff  %>%
  filter(!(is.na(abl_staff_al))) %>%
  summarise(sum = sum(abl_staff_al))


abl_pre_staff = abl_pre_staff %>%
  mutate(Admin_labour_cost = (abl_staff_al_01_hours + abl_staff_al_02_hours + abl_staff_al_03_hours + abl_staff_al_04_hours + abl_staff_al_05_hours +
           abl_staff_al_06_hours + abl_staff_al_07_hours + abl_staff_al_08_hours + abl_staff_al_09_hours + abl_staff_al_10_hours)*Administrative_labour)

abl_intra_staff = abl_intra_staff %>%
  mutate(Admin_labour_cost = (abl_staff_al_01_hours + abl_staff_al_02_hours + abl_staff_al_03_hours + abl_staff_al_04_hours + abl_staff_al_05_hours +
                                 abl_staff_al_06_hours + abl_staff_al_07_hours + abl_staff_al_08_hours + abl_staff_al_09_hours + abl_staff_al_10_hours)*Administrative_labour)

abl_post_staff = abl_post_staff %>%
  mutate(Admin_labour_cost = (abl_staff_al_01_hours + abl_staff_al_02_hours + abl_staff_al_03_hours + abl_staff_al_04_hours + abl_staff_al_05_hours +
                                abl_staff_al_06_hours + abl_staff_al_07_hours + abl_staff_al_08_hours + abl_staff_al_09_hours + abl_staff_al_10_hours)*Administrative_labour)


abl_pre_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
abl_post_staff  %>%
  summarise(sum = sum(Admin_labour_cost))


# Assistant_in_nursing
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_ass_n))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_ass_n))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_ass_n))


abl_pre_staff = abl_pre_staff %>%
  mutate(Assistant_in_nursing_cost = (abl_staff_ass_n_01_hours + abl_staff_ass_n_02_hours + abl_staff_ass_n_03_hours + abl_staff_ass_n_04_hours + abl_staff_ass_n_05_hours +
                                abl_staff_ass_n_06_hours + abl_staff_ass_n_07_hours + abl_staff_ass_n_08_hours + abl_staff_ass_n_09_hours + abl_staff_ass_n_10_hours)*Assistant_in_nursing)

abl_intra_staff = abl_intra_staff %>%
  mutate(Assistant_in_nursing_cost = (abl_staff_ass_n_01_hours + abl_staff_ass_n_02_hours + abl_staff_ass_n_03_hours + abl_staff_ass_n_04_hours + abl_staff_ass_n_05_hours +
                                        abl_staff_ass_n_06_hours + abl_staff_ass_n_07_hours + abl_staff_ass_n_08_hours + abl_staff_ass_n_09_hours + abl_staff_ass_n_10_hours)*Assistant_in_nursing)

abl_post_staff = abl_post_staff %>%
  mutate(Assistant_in_nursing_cost = (abl_staff_ass_n_01_hours + abl_staff_ass_n_02_hours + abl_staff_ass_n_03_hours + abl_staff_ass_n_04_hours + abl_staff_ass_n_05_hours +
                                        abl_staff_ass_n_06_hours + abl_staff_ass_n_07_hours + abl_staff_ass_n_08_hours + abl_staff_ass_n_09_hours + abl_staff_ass_n_10_hours)*Assistant_in_nursing)

abl_pre_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
abl_post_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))



# Enrolled_nurse
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_enr_n))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_enr_n))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_enr_n))


abl_pre_staff = abl_pre_staff %>%
  mutate(Enrolled_nurse_cost = (abl_staff_enr_n_01_hours + abl_staff_enr_n_02_hours + abl_staff_enr_n_03_hours + abl_staff_enr_n_04_hours + abl_staff_enr_n_05_hours +
                                        abl_staff_enr_n_06_hours + abl_staff_enr_n_07_hours + abl_staff_enr_n_08_hours + abl_staff_enr_n_09_hours + abl_staff_enr_n_10_hours)*Enrolled_nurse)

abl_intra_staff = abl_intra_staff %>%
  mutate(Enrolled_nurse_cost = (abl_staff_enr_n_01_hours + abl_staff_enr_n_02_hours + abl_staff_enr_n_03_hours + abl_staff_enr_n_04_hours + abl_staff_enr_n_05_hours +
                                        abl_staff_enr_n_06_hours + abl_staff_enr_n_07_hours + abl_staff_enr_n_08_hours + abl_staff_enr_n_09_hours + abl_staff_enr_n_10_hours)*Enrolled_nurse)

abl_post_staff = abl_post_staff %>%
  mutate(Enrolled_nurse_cost = (abl_staff_enr_n_01_hours + abl_staff_enr_n_02_hours + abl_staff_enr_n_03_hours + abl_staff_enr_n_04_hours + abl_staff_enr_n_05_hours +
                                        abl_staff_enr_n_06_hours + abl_staff_enr_n_07_hours + abl_staff_enr_n_08_hours + abl_staff_enr_n_09_hours + abl_staff_enr_n_10_hours)*Enrolled_nurse)

abl_pre_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
abl_post_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))


# Registered_nurse
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_reg_n))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_reg_n))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_reg_n))


abl_pre_staff = abl_pre_staff %>%
  mutate(Registered_nurse_cost = (abl_staff_reg_n_01_hours + abl_staff_reg_n_02_hours + abl_staff_reg_n_03_hours + abl_staff_reg_n_04_hours + abl_staff_reg_n_05_hours +
                                  abl_staff_reg_n_06_hours + abl_staff_reg_n_07_hours + abl_staff_reg_n_08_hours + abl_staff_reg_n_09_hours + abl_staff_reg_n_10_hours)*Registered_nurse)

abl_intra_staff = abl_intra_staff %>%
  mutate(Registered_nurse_cost = (abl_staff_reg_n_01_hours + abl_staff_reg_n_02_hours + abl_staff_reg_n_03_hours + abl_staff_reg_n_04_hours + abl_staff_reg_n_05_hours +
                                  abl_staff_reg_n_06_hours + abl_staff_reg_n_07_hours + abl_staff_reg_n_08_hours + abl_staff_reg_n_09_hours + abl_staff_reg_n_10_hours)*Registered_nurse)

abl_post_staff = abl_post_staff %>%
  mutate(Registered_nurse_cost = (abl_staff_reg_n_01_hours + abl_staff_reg_n_02_hours + abl_staff_reg_n_03_hours + abl_staff_reg_n_04_hours + abl_staff_reg_n_05_hours +
                                  abl_staff_reg_n_06_hours + abl_staff_reg_n_07_hours + abl_staff_reg_n_08_hours + abl_staff_reg_n_09_hours + abl_staff_reg_n_10_hours)*Registered_nurse)



abl_pre_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
abl_post_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))



# Clinical_nurse
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_cln_n))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_cln_n))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_cln_n))


abl_pre_staff = abl_pre_staff %>%
  mutate(Clinical_nurse_cost = (abl_staff_cln_n_01_hours + abl_staff_cln_n_02_hours + abl_staff_cln_n_03_hours + abl_staff_cln_n_04_hours + abl_staff_cln_n_05_hours +
                                    abl_staff_cln_n_06_hours + abl_staff_cln_n_07_hours + abl_staff_cln_n_08_hours + abl_staff_cln_n_09_hours + abl_staff_cln_n_10_hours)*Clinical_nurse)

abl_intra_staff = abl_intra_staff %>%
  mutate(Clinical_nurse_cost = (abl_staff_cln_n_01_hours + abl_staff_cln_n_02_hours + abl_staff_cln_n_03_hours + abl_staff_cln_n_04_hours + abl_staff_cln_n_05_hours +
                                    abl_staff_cln_n_06_hours + abl_staff_cln_n_07_hours + abl_staff_cln_n_08_hours + abl_staff_cln_n_09_hours + abl_staff_cln_n_10_hours)*Clinical_nurse)

abl_post_staff = abl_post_staff %>%
  mutate(Clinical_nurse_cost = (abl_staff_cln_n_01_hours + abl_staff_cln_n_02_hours + abl_staff_cln_n_03_hours + abl_staff_cln_n_04_hours + abl_staff_cln_n_05_hours +
                                    abl_staff_cln_n_06_hours + abl_staff_cln_n_07_hours + abl_staff_cln_n_08_hours + abl_staff_cln_n_09_hours + abl_staff_cln_n_10_hours)*Clinical_nurse)



abl_pre_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
abl_post_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))



# Assistant_nurse_unit_manager
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_anum))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_anum))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_anum))


abl_pre_staff = abl_pre_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (abl_staff_anum_01_hours + abl_staff_anum_02_hours + abl_staff_anum_03_hours + abl_staff_anum_04_hours + abl_staff_anum_05_hours +
                                  abl_staff_anum_06_hours + abl_staff_anum_07_hours + abl_staff_anum_08_hours + abl_staff_anum_09_hours + abl_staff_anum_10_hours)*Assistant_nurse_unit_manager)

abl_intra_staff = abl_intra_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (abl_staff_anum_01_hours + abl_staff_anum_02_hours + abl_staff_anum_03_hours + abl_staff_anum_04_hours + abl_staff_anum_05_hours +
                                                abl_staff_anum_06_hours + abl_staff_anum_07_hours + abl_staff_anum_08_hours + abl_staff_anum_09_hours + abl_staff_anum_10_hours)*Assistant_nurse_unit_manager)

abl_post_staff = abl_post_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (abl_staff_anum_01_hours + abl_staff_anum_02_hours + abl_staff_anum_03_hours + abl_staff_anum_04_hours + abl_staff_anum_05_hours +
                                                abl_staff_anum_06_hours + abl_staff_anum_07_hours + abl_staff_anum_08_hours + abl_staff_anum_09_hours + abl_staff_anum_10_hours)*Assistant_nurse_unit_manager)

abl_pre_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
abl_post_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))



# Case_manager
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_case_m))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_case_m))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_case_m))


abl_pre_staff = abl_pre_staff %>%
  mutate(Case_manager_cost = (abl_staff_case_m_01_hours + abl_staff_case_m_02_hours + abl_staff_case_m_03_hours + abl_staff_case_m_04_hours + abl_staff_case_m_05_hours +
                                                abl_staff_case_m_06_hours + abl_staff_case_m_07_hours + abl_staff_case_m_08_hours + abl_staff_case_m_09_hours + abl_staff_case_m_10_hours)*Case_manager)

abl_intra_staff = abl_intra_staff %>%
  mutate(Case_manager_cost = (abl_staff_case_m_01_hours + abl_staff_case_m_02_hours + abl_staff_case_m_03_hours + abl_staff_case_m_04_hours + abl_staff_case_m_05_hours +
                                                abl_staff_case_m_06_hours + abl_staff_case_m_07_hours + abl_staff_case_m_08_hours + abl_staff_case_m_09_hours + abl_staff_case_m_10_hours)*Case_manager)

abl_post_staff = abl_post_staff %>%
  mutate(Case_manager_cost = (abl_staff_case_m_01_hours + abl_staff_case_m_02_hours + abl_staff_case_m_03_hours + abl_staff_case_m_04_hours + abl_staff_case_m_05_hours +
                                                abl_staff_case_m_06_hours + abl_staff_case_m_07_hours + abl_staff_case_m_08_hours + abl_staff_case_m_09_hours + abl_staff_case_m_10_hours)*Case_manager)

abl_pre_staff  %>%
  summarise(sum = sum(Case_manager_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Case_manager_cost))
abl_post_staff  %>%
  summarise(sum = sum(Case_manager_cost))


# Nurse_unit_manager
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_nur_um))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_nur_um))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_nur_um))


abl_pre_staff = abl_pre_staff %>%
  mutate(Nurse_unit_manager_cost = (abl_staff_nur_um_01_hours + abl_staff_nur_um_02_hours + abl_staff_nur_um_03_hours + abl_staff_nur_um_04_hours + abl_staff_nur_um_05_hours +
                                abl_staff_nur_um_06_hours + abl_staff_nur_um_07_hours + abl_staff_nur_um_08_hours + abl_staff_nur_um_09_hours + abl_staff_nur_um_10_hours)*Nurse_unit_manager)

abl_intra_staff = abl_intra_staff %>%
  mutate(Nurse_unit_manager_cost = (abl_staff_nur_um_01_hours + abl_staff_nur_um_02_hours + abl_staff_nur_um_03_hours + abl_staff_nur_um_04_hours + abl_staff_nur_um_05_hours +
                                abl_staff_nur_um_06_hours + abl_staff_nur_um_07_hours + abl_staff_nur_um_08_hours + abl_staff_nur_um_09_hours + abl_staff_nur_um_10_hours)*Nurse_unit_manager)

abl_post_staff = abl_post_staff %>%
  mutate(Nurse_unit_manager_cost = (abl_staff_nur_um_01_hours + abl_staff_nur_um_02_hours + abl_staff_nur_um_03_hours + abl_staff_nur_um_04_hours + abl_staff_nur_um_05_hours +
                                abl_staff_nur_um_06_hours + abl_staff_nur_um_07_hours + abl_staff_nur_um_08_hours + abl_staff_nur_um_09_hours + abl_staff_nur_um_10_hours)*Nurse_unit_manager)

abl_pre_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
abl_post_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))


# Intern_or_Resident
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_in_res))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_in_res))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_in_res))


abl_pre_staff = abl_pre_staff %>%
  mutate(Intern_or_Resident_cost = (abl_staff_in_res_01_hours + abl_staff_in_res_02_hours + abl_staff_in_res_03_hours + abl_staff_in_res_04_hours + abl_staff_in_res_05_hours +
                                      abl_staff_in_res_06_hours + abl_staff_in_res_07_hours + abl_staff_in_res_08_hours + abl_staff_in_res_09_hours + abl_staff_in_res_10_hours)*Intern_or_Resident)

abl_intra_staff = abl_intra_staff %>%
  mutate(Intern_or_Resident_cost = (abl_staff_in_res_01_hours + abl_staff_in_res_02_hours + abl_staff_in_res_03_hours + abl_staff_in_res_04_hours + abl_staff_in_res_05_hours +
                                      abl_staff_in_res_06_hours + abl_staff_in_res_07_hours + abl_staff_in_res_08_hours + abl_staff_in_res_09_hours + abl_staff_in_res_10_hours)*Intern_or_Resident)

abl_post_staff = abl_post_staff %>%
  mutate(Intern_or_Resident_cost = (abl_staff_in_res_01_hours + abl_staff_in_res_02_hours + abl_staff_in_res_03_hours + abl_staff_in_res_04_hours + abl_staff_in_res_05_hours +
                                      abl_staff_in_res_06_hours + abl_staff_in_res_07_hours + abl_staff_in_res_08_hours + abl_staff_in_res_09_hours + abl_staff_in_res_10_hours)*Intern_or_Resident)

abl_pre_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
abl_post_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))


# Registrar
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_registrar))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_registrar))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_registrar))


abl_pre_staff = abl_pre_staff %>%
  mutate(Registrar_cost = (abl_staff_registrar_01_hours + abl_staff_registrar_02_hours + abl_staff_registrar_03_hours + abl_staff_registrar_04_hours + abl_staff_registrar_05_hours +
                                      abl_staff_registrar_06_hours + abl_staff_registrar_07_hours + abl_staff_registrar_08_hours + abl_staff_registrar_09_hours + abl_staff_registrar_10_hours)*Registrar)

abl_intra_staff = abl_intra_staff %>%
  mutate(Registrar_cost = (abl_staff_registrar_01_hours + abl_staff_registrar_02_hours + abl_staff_registrar_03_hours + abl_staff_registrar_04_hours + abl_staff_registrar_05_hours +
                                      abl_staff_registrar_06_hours + abl_staff_registrar_07_hours + abl_staff_registrar_08_hours + abl_staff_registrar_09_hours + abl_staff_registrar_10_hours)*Registrar)

abl_post_staff = abl_post_staff %>%
  mutate(Registrar_cost = (abl_staff_registrar_01_hours + abl_staff_registrar_02_hours + abl_staff_registrar_03_hours + abl_staff_registrar_04_hours + abl_staff_registrar_05_hours +
                                      abl_staff_registrar_06_hours + abl_staff_registrar_07_hours + abl_staff_registrar_08_hours + abl_staff_registrar_09_hours + abl_staff_registrar_10_hours)*Registrar)

abl_pre_staff  %>%
  summarise(sum = sum(Registrar_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Registrar_cost))
abl_post_staff  %>%
  summarise(sum = sum(Registrar_cost))



# Fellow
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_fellow))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_fellow))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_fellow))


abl_pre_staff = abl_pre_staff %>%
  mutate(Fellow_cost = (abl_staff_fellow_01_hours + abl_staff_fellow_02_hours + abl_staff_fellow_03_hours + abl_staff_fellow_04_hours + abl_staff_fellow_05_hours +
                             abl_staff_fellow_06_hours + abl_staff_fellow_07_hours + abl_staff_fellow_08_hours + abl_staff_fellow_09_hours + abl_staff_fellow_10_hours)*Fellow)

abl_intra_staff = abl_intra_staff %>%
  mutate(Fellow_cost = (abl_staff_fellow_01_hours + abl_staff_fellow_02_hours + abl_staff_fellow_03_hours + abl_staff_fellow_04_hours + abl_staff_fellow_05_hours +
                             abl_staff_fellow_06_hours + abl_staff_fellow_07_hours + abl_staff_fellow_08_hours + abl_staff_fellow_09_hours + abl_staff_fellow_10_hours)*Fellow)

abl_post_staff = abl_post_staff %>%
  mutate(Fellow_cost = (abl_staff_fellow_01_hours + abl_staff_fellow_02_hours + abl_staff_fellow_03_hours + abl_staff_fellow_04_hours + abl_staff_fellow_05_hours +
                             abl_staff_fellow_06_hours + abl_staff_fellow_07_hours + abl_staff_fellow_08_hours + abl_staff_fellow_09_hours + abl_staff_fellow_10_hours)*Fellow)

abl_pre_staff  %>%
  summarise(sum = sum(Fellow_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Fellow_cost))
abl_post_staff  %>%
  summarise(sum = sum(Fellow_cost))


# Consultant
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_consultant))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_consultant))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_consultant))


abl_pre_staff = abl_pre_staff %>%
  mutate(Consultant_cost = (abl_staff_consultant_01_hours + abl_staff_consultant_02_hours + abl_staff_consultant_03_hours + abl_staff_consultant_04_hours + abl_staff_consultant_05_hours +
                          abl_staff_consultant_06_hours + abl_staff_consultant_07_hours + abl_staff_consultant_08_hours + abl_staff_consultant_09_hours + abl_staff_consultant_10_hours)*Consultant)

abl_intra_staff = abl_intra_staff %>%
  mutate(Consultant_cost = (abl_staff_consultant_01_hours + abl_staff_consultant_02_hours + abl_staff_consultant_03_hours + abl_staff_consultant_04_hours + abl_staff_consultant_05_hours +
                          abl_staff_consultant_06_hours + abl_staff_consultant_07_hours + abl_staff_consultant_08_hours + abl_staff_consultant_09_hours + abl_staff_consultant_10_hours)*Consultant)

abl_post_staff = abl_post_staff %>%
  mutate(Consultant_cost = (abl_staff_consultant_01_hours + abl_staff_consultant_02_hours + abl_staff_consultant_03_hours + abl_staff_consultant_04_hours + abl_staff_consultant_05_hours +
                          abl_staff_consultant_06_hours + abl_staff_consultant_07_hours + abl_staff_consultant_08_hours + abl_staff_consultant_09_hours + abl_staff_consultant_10_hours)*Consultant)

abl_pre_staff  %>%
  summarise(sum = sum(Consultant_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Consultant_cost))
abl_post_staff  %>%
  summarise(sum = sum(Consultant_cost))



# Radiographer
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_radio))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_radio))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_radio))


abl_pre_staff = abl_pre_staff %>%
  mutate(Radiographer_cost = (abl_staff_radio_01_hours + abl_staff_radio_02_hours + abl_staff_radio_03_hours + abl_staff_radio_04_hours + abl_staff_radio_05_hours +
                              abl_staff_radio_06_hours + abl_staff_radio_07_hours + abl_staff_radio_08_hours + abl_staff_radio_09_hours + abl_staff_radio_10_hours)*Radiographer)

abl_intra_staff = abl_intra_staff %>%
  mutate(Radiographer_cost = (abl_staff_radio_01_hours + abl_staff_radio_02_hours + abl_staff_radio_03_hours + abl_staff_radio_04_hours + abl_staff_radio_05_hours +
                              abl_staff_radio_06_hours + abl_staff_radio_07_hours + abl_staff_radio_08_hours + abl_staff_radio_09_hours + abl_staff_radio_10_hours)*Radiographer)

abl_post_staff = abl_post_staff %>%
  mutate(Radiographer_cost = (abl_staff_radio_01_hours + abl_staff_radio_02_hours + abl_staff_radio_03_hours + abl_staff_radio_04_hours + abl_staff_radio_05_hours +
                              abl_staff_radio_06_hours + abl_staff_radio_07_hours + abl_staff_radio_08_hours + abl_staff_radio_09_hours + abl_staff_radio_10_hours)*Radiographer)

abl_pre_staff  %>%
  summarise(sum = sum(Radiographer_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Radiographer_cost))
abl_post_staff  %>%
  summarise(sum = sum(Radiographer_cost))



# Ultrasonographer
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_ultra))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_ultra))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_ultra))


abl_pre_staff = abl_pre_staff %>%
  mutate(Ultrasonographer_cost = (abl_staff_ultra_01_hours + abl_staff_ultra_02_hours + abl_staff_ultra_03_hours + abl_staff_ultra_04_hours + abl_staff_ultra_05_hours +
                                abl_staff_ultra_06_hours + abl_staff_ultra_07_hours + abl_staff_ultra_08_hours + abl_staff_ultra_09_hours + abl_staff_ultra_10_hours)*Ultrasonographer)

abl_intra_staff = abl_intra_staff %>%
  mutate(Ultrasonographer_cost = (abl_staff_ultra_01_hours + abl_staff_ultra_02_hours + abl_staff_ultra_03_hours + abl_staff_ultra_04_hours + abl_staff_ultra_05_hours +
                                abl_staff_ultra_06_hours + abl_staff_ultra_07_hours + abl_staff_ultra_08_hours + abl_staff_ultra_09_hours + abl_staff_ultra_10_hours)*Ultrasonographer)

abl_post_staff = abl_post_staff %>%
  mutate(Ultrasonographer_cost = (abl_staff_ultra_01_hours + abl_staff_ultra_02_hours + abl_staff_ultra_03_hours + abl_staff_ultra_04_hours + abl_staff_ultra_05_hours +
                                abl_staff_ultra_06_hours + abl_staff_ultra_07_hours + abl_staff_ultra_08_hours + abl_staff_ultra_09_hours + abl_staff_ultra_10_hours)*Ultrasonographer)

abl_pre_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
abl_post_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))



# Nuc_Med_Tech
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_nuc_mt))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_nuc_mt))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_nuc_mt))


abl_pre_staff = abl_pre_staff %>%
  mutate(Nuc_Med_Tech_cost = (abl_staff_nuc_mt_01_hours + abl_staff_nuc_mt_02_hours + abl_staff_nuc_mt_03_hours + abl_staff_nuc_mt_04_hours + abl_staff_nuc_mt_05_hours +
                                    abl_staff_nuc_mt_06_hours + abl_staff_nuc_mt_07_hours + abl_staff_nuc_mt_08_hours + abl_staff_nuc_mt_09_hours + abl_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

abl_intra_staff = abl_intra_staff %>%
  mutate(Nuc_Med_Tech_cost = (abl_staff_nuc_mt_01_hours + abl_staff_nuc_mt_02_hours + abl_staff_nuc_mt_03_hours + abl_staff_nuc_mt_04_hours + abl_staff_nuc_mt_05_hours +
                                    abl_staff_nuc_mt_06_hours + abl_staff_nuc_mt_07_hours + abl_staff_nuc_mt_08_hours + abl_staff_nuc_mt_09_hours + abl_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

abl_post_staff = abl_post_staff %>%
  mutate(Nuc_Med_Tech_cost = (abl_staff_nuc_mt_01_hours + abl_staff_nuc_mt_02_hours + abl_staff_nuc_mt_03_hours + abl_staff_nuc_mt_04_hours + abl_staff_nuc_mt_05_hours +
                                    abl_staff_nuc_mt_06_hours + abl_staff_nuc_mt_07_hours + abl_staff_nuc_mt_08_hours + abl_staff_nuc_mt_09_hours + abl_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

abl_pre_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
abl_post_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))


# NM_physicist
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_nm_ph))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_nm_ph))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_nm_ph))


abl_pre_staff = abl_pre_staff %>%
  mutate(NM_physicist_cost = (abl_staff_nm_ph_01_hours + abl_staff_nm_ph_02_hours + abl_staff_nm_ph_03_hours + abl_staff_nm_ph_04_hours + abl_staff_nm_ph_05_hours +
                                abl_staff_nm_ph_06_hours + abl_staff_nm_ph_07_hours + abl_staff_nm_ph_08_hours + abl_staff_nm_ph_09_hours + abl_staff_nm_ph_10_hours)*NM_physicist)

abl_intra_staff = abl_intra_staff %>%
  mutate(NM_physicist_cost = (abl_staff_nm_ph_01_hours + abl_staff_nm_ph_02_hours + abl_staff_nm_ph_03_hours + abl_staff_nm_ph_04_hours + abl_staff_nm_ph_05_hours +
                                abl_staff_nm_ph_06_hours + abl_staff_nm_ph_07_hours + abl_staff_nm_ph_08_hours + abl_staff_nm_ph_09_hours + abl_staff_nm_ph_10_hours)*NM_physicist)

abl_post_staff = abl_post_staff %>%
  mutate(NM_physicist_cost = (abl_staff_nm_ph_01_hours + abl_staff_nm_ph_02_hours + abl_staff_nm_ph_03_hours + abl_staff_nm_ph_04_hours + abl_staff_nm_ph_05_hours +
                                abl_staff_nm_ph_06_hours + abl_staff_nm_ph_07_hours + abl_staff_nm_ph_08_hours + abl_staff_nm_ph_09_hours + abl_staff_nm_ph_10_hours)*NM_physicist)

abl_pre_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
abl_intra_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
abl_post_staff  %>%
  summarise(sum = sum(NM_physicist_cost))


# Labour_nuclear_medicine
abl_pre_staff  %>%
  summarise(sum = sum(abl_staff_mab_nm))
abl_intra_staff  %>%
  summarise(sum = sum(abl_staff_mab_nm))
abl_post_staff  %>%
  summarise(sum = sum(abl_staff_mab_nm))


abl_pre_staff = abl_pre_staff %>%
  mutate(Labour_nuclear_medicine_cost = (abl_staff_mab_nm_01_hours + abl_staff_mab_nm_02_hours + abl_staff_mab_nm_03_hours + abl_staff_mab_nm_04_hours + abl_staff_mab_nm_05_hours +
                                abl_staff_mab_nm_06_hours + abl_staff_mab_nm_07_hours + abl_staff_mab_nm_08_hours + abl_staff_mab_nm_09_hours + abl_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

abl_intra_staff = abl_intra_staff %>%
  mutate(Labour_nuclear_medicine_cost = (abl_staff_mab_nm_01_hours + abl_staff_mab_nm_02_hours + abl_staff_mab_nm_03_hours + abl_staff_mab_nm_04_hours + abl_staff_mab_nm_05_hours +
                                abl_staff_mab_nm_06_hours + abl_staff_mab_nm_07_hours + abl_staff_mab_nm_08_hours + abl_staff_mab_nm_09_hours + abl_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

abl_post_staff = abl_post_staff %>%
  mutate(Labour_nuclear_medicine_cost = (abl_staff_mab_nm_01_hours + abl_staff_mab_nm_02_hours + abl_staff_mab_nm_03_hours + abl_staff_mab_nm_04_hours + abl_staff_mab_nm_05_hours +
                                abl_staff_mab_nm_06_hours + abl_staff_mab_nm_07_hours + abl_staff_mab_nm_08_hours + abl_staff_mab_nm_09_hours + abl_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

abl_pre_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
abl_intra_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
abl_post_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))



## Final staff cost
abl_pre_staff_final = abl_pre_staff %>%
  select(1, 547: 563) %>%
  mutate(abl_pre_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

abl_pre_staff_final %>%
  summarise(sum = sum(abl_pre_staff_final))

abl_pre_staff_final_1 = abl_pre_staff_final %>%
  select(1, abl_pre_staff_final)



abl_intra_staff_final = abl_intra_staff %>%
  select(1, 547: 563) %>%
  mutate(abl_intra_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

abl_intra_staff_final %>%
  summarise(sum = sum(abl_intra_staff_final))

abl_intra_staff_final_1 = abl_intra_staff_final %>%
  select(1, abl_intra_staff_final)



abl_post_staff_final = abl_post_staff %>%
  select(1, 547: 563) %>%
  mutate(abl_post_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

abl_post_staff_final %>%
  summarise(sum = sum(abl_post_staff_final))

abl_post_staff_final_1 = abl_post_staff_final %>%
  select(1, abl_post_staff_final)


## -- Boot strapping -- ## abl_pre_staff_final
set.seed(13579)
n = 41
B = 100000

Boot.pre_staff_cost.1 <- matrix(sample(abl_pre_staff_final$abl_pre_staff_final, size= B*n, 
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


## -- Boot strapping -- ## abl_intra_staff_final
set.seed(13579)
n = 41
B = 100000

Boot.intra_staff_cost.1 <- matrix(sample(abl_intra_staff_final$abl_intra_staff_final, size= B*n, 
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


## -- Boot strapping -- ## abl_post_staff_final
set.seed(13579)
n = 41
B = 100000

Boot.post_staff_cost.1 <- matrix(sample(abl_post_staff_final$abl_post_staff_final, size= B*n, 
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




abl_staff_total = merge(x = abl_pre_staff_final, y = abl_intra_staff_final, by = "record_id", all = TRUE)
abl_staff_total = merge(x = abl_staff_total, y = abl_post_staff_final, by = "record_id", all = TRUE)

abl_staff_total = abl_staff_total %>%
  select(1, abl_pre_staff_final, abl_intra_staff_final, abl_post_staff_final) %>%
  mutate(abl_staff_total = abl_pre_staff_final + abl_intra_staff_final +abl_post_staff_final)

abl_staff_total %>%
  summarise(sum = sum(abl_staff_total))

abl_staff_total_1 = abl_staff_total %>%
  select(1, 5)


## -- Boot strapping -- ## abl_total_staff_final
set.seed(13579)
n = 41
B = 100000

Boot.total_staff_cost.1 <- matrix(sample(abl_staff_total$abl_staff_total, size= B*n, 
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
Total_cost = merge(x = abl_imaging_4, y = abl_combine_ConMed, by = "record_id", all = TRUE)
Total_cost = merge(x = Total_cost, y = abl_staff_total_1, by = "record_id", all = TRUE)

Total_cost = Total_cost %>%
  mutate(abl_total_cost = abl_imaging_cost + total_ConMed + abl_staff_total)

Total_cost %>%
  summarise(sum = sum(abl_total_cost))


## Pre
Total_pre_cost = merge(x = abl_imaging_pre, y = abl_pre_ConMed_3, by = "record_id", all = TRUE)
Total_pre_cost = merge(x = Total_pre_cost, y = abl_pre_staff_final_1, by = "record_id", all = TRUE)


Total_pre_cost = Total_pre_cost %>%
  mutate(abl_pre_total_cost = cost_pre_imaging + cost_pre_conMed + abl_pre_staff_final)

Total_pre_cost %>%
  summarise(sum = sum(abl_pre_total_cost))


## INtra
Total_intra_cost = merge(x = abl_imaging_intra, y = abl_intra_ConMed_3, by = "record_id", all = TRUE)
Total_intra_cost = merge(x = Total_intra_cost, y = abl_intra_staff_final_1, by = "record_id", all = TRUE)


Total_intra_cost = Total_intra_cost %>%
  mutate(abl_intra_total_cost = cost_intra_imaging + cost_intra_conMed + abl_intra_staff_final)
 
Total_intra_cost %>%
  summarise(sum = sum(abl_intra_total_cost))


## Post
Total_post_cost = merge(x = abl_imaging_post, y = abl_post_ConMed_3, by = "record_id", all = TRUE)
Total_post_cost = merge(x = Total_post_cost, y = abl_post_staff_final_1, by = "record_id", all = TRUE)

Total_post_cost[is.na(Total_post_cost)]<-0

Total_post_cost = Total_post_cost %>%
  mutate(abl_post_total_cost = cost_post_imaging + cost_post_conMed + abl_post_staff_final)

Total_post_cost %>%
  summarise(sum = sum(abl_post_total_cost))




###### Boot strap total cost
Boot.abl_total_cost <- matrix(sample(Total_cost$abl_total_cost, size= B*n, 
                                        replace=TRUE), ncol=B, nrow=n)


dim(Boot.abl_total_cost)

Boot.abl_total_cost[1:5,1:5]

Boot.abl_total_cost.Means_1 <- colMeans(Boot.abl_total_cost)

length(Boot.abl_total_cost.Means_1)

Boot.abl_total_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.abl_total_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.abl_total_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.abl_total_cost.Means_1, prob=0.975)


abl_total_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(abl_total_cost) = c("Year", "L_CI","Mean","U_CI")
abl_total_cost = as.data.frame(abl_total_cost)
abl_total_cost



###### Boot strap pre cost
Boot.abl_total_pre_cost <- matrix(sample(Total_pre_cost$abl_pre_total_cost, size= B*n, 
                                      replace=TRUE), ncol=B, nrow=n)


dim(Boot.abl_total_pre_cost)

Boot.abl_total_pre_cost[1:5,1:5]

Boot.abl_total_pre_cost.Means_1 <- colMeans(Boot.abl_total_pre_cost)

length(Boot.abl_total_pre_cost.Means_1)

Boot.abl_total_pre_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.abl_total_pre_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.abl_total_pre_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.abl_total_pre_cost.Means_1, prob=0.975)


abl_pre_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(abl_pre_cost) = c("Year", "L_CI","Mean","U_CI")
abl_pre_cost = as.data.frame(abl_pre_cost)
abl_pre_cost


###### Boot strap intra cost
Boot.abl_total_intra_cost <- matrix(sample(Total_intra_cost$abl_intra_total_cost, size= B*n, 
                                          replace=TRUE), ncol=B, nrow=n)


dim(Boot.abl_total_intra_cost)

Boot.abl_total_intra_cost[1:5,1:5]

Boot.abl_total_intra_cost.Means_1 <- colMeans(Boot.abl_total_intra_cost)

length(Boot.abl_total_intra_cost.Means_1)

Boot.abl_total_intra_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.abl_total_intra_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.abl_total_intra_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.abl_total_intra_cost.Means_1, prob=0.975)


abl_intra_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(abl_intra_cost) = c("Year", "L_CI","Mean","U_CI")
abl_intra_cost = as.data.frame(abl_intra_cost)
abl_intra_cost


###### Boot strap post cost
Boot.abl_total_post_cost <- matrix(sample(Total_post_cost$abl_post_total_cost, size= B*n, 
                                            replace=TRUE), ncol=B, nrow=n)


dim(Boot.abl_total_post_cost)

Boot.abl_total_post_cost[1:5,1:5]

Boot.abl_total_post_cost.Means_1 <- colMeans(Boot.abl_total_post_cost)

length(Boot.abl_total_post_cost.Means_1)

Boot.abl_total_post_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.abl_total_post_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.abl_total_post_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.abl_total_post_cost.Means_1, prob=0.975)


abl_post_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(abl_post_cost) = c("Year", "L_CI","Mean","U_CI")
abl_post_cost = as.data.frame(abl_post_cost)
abl_post_cost




#################  To estimate the staff clinical team level cost - Pre ##########


######### Administrative_labour

abl_pre_staff %>%
  summarise(max = max(abl_staff_al)) # 3



#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_01_team = case_when(
    abl_staff_al_01_team %in% 1 ~ "Oncology",
    abl_staff_al_01_team %in% 2 ~ "Radiology",
    abl_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_01_team = coalesce(abl_staff_al_01_team, abl_staff_al_01_other))
abl_pre_staff$abl_staff_al_01_team = as.factor(abl_pre_staff$abl_staff_al_01_team)

levels(abl_pre_staff$abl_staff_al_01_team)

abl_pre_staff %>%
  filter(abl_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)



#2
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_02_team = case_when(
    abl_staff_al_02_team %in% 1 ~ "Oncology",
    abl_staff_al_02_team %in% 2 ~ "Radiology",
    abl_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_02_team = coalesce(abl_staff_al_02_team, abl_staff_al_02_other))
abl_pre_staff$abl_staff_al_02_team = as.factor(abl_pre_staff$abl_staff_al_02_team)

levels(abl_pre_staff$abl_staff_al_02_team)

abl_pre_staff %>%
  filter(abl_staff_al_02_team == "Admission" | abl_staff_al_02_team == "admissions" |abl_staff_al_02_team == "Admissions") %>%
  summarise(sum = sum(abl_staff_al_02_hours),
            cost = sum*Administrative_labour)



#3
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_03_team = case_when(
    abl_staff_al_03_team %in% 1 ~ "Oncology",
    abl_staff_al_03_team %in% 2 ~ "Radiology",
    abl_staff_al_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_03_team %in% 4 ~ "Anaesthesia"
  ))

abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_al_03_team = coalesce(abl_staff_al_03_team, abl_staff_al_03_other))
abl_pre_staff$abl_staff_al_03_team = as.factor(abl_pre_staff$abl_staff_al_03_team)

levels(abl_pre_staff$abl_staff_al_03_team)


abl_pre_staff %>%
  filter(abl_staff_al_03_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_al_03_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

abl_pre_staff %>%
  summarise(max = max(abl_staff_ass_n)) # 0


######## Enrolled_nurse
abl_pre_staff %>%
  summarise(max = max(abl_staff_enr_n)) # 0


####### Registered_nurse
abl_pre_staff %>%
  summarise(max = max(abl_staff_reg_n)) # 4


#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_01_team = case_when(
    abl_staff_reg_n_01_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_01_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_01_team = coalesce(abl_staff_reg_n_01_team, abl_staff_reg_n_01_other))
abl_pre_staff$abl_staff_reg_n_01_team = as.factor(abl_pre_staff$abl_staff_reg_n_01_team)

levels(abl_pre_staff$abl_staff_reg_n_01_team)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "4E") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "Oncology") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "SCU") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_01_team == "SSU") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)




#2
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_02_team = case_when(
    abl_staff_reg_n_02_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_02_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_02_team = coalesce(abl_staff_reg_n_02_team, abl_staff_reg_n_02_other))
abl_pre_staff$abl_staff_reg_n_02_team = as.factor(abl_pre_staff$abl_staff_reg_n_02_team)

levels(abl_pre_staff$abl_staff_reg_n_02_team)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "4E") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_02_team == "Oncology") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)



#3
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_03_team = case_when(
    abl_staff_reg_n_03_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_03_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_03_team = coalesce(abl_staff_reg_n_03_team, abl_staff_reg_n_03_other))
abl_pre_staff$abl_staff_reg_n_03_team = as.factor(abl_pre_staff$abl_staff_reg_n_03_team)

levels(abl_pre_staff$abl_staff_reg_n_03_team)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "4E GAST") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "PACU") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_pre_staff %>%
  filter(abl_staff_reg_n_03_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



#4
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_04_team = case_when(
    abl_staff_reg_n_04_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_04_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_reg_n_04_team = coalesce(abl_staff_reg_n_04_team, abl_staff_reg_n_04_other))
abl_pre_staff$abl_staff_reg_n_04_team = as.factor(abl_pre_staff$abl_staff_reg_n_04_team)

levels(abl_pre_staff$abl_staff_reg_n_04_team)

abl_pre_staff %>%
  filter(abl_staff_reg_n_04_team == "PACU") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)




######### Clinical_nurse
abl_pre_staff %>%
  summarise(max = max(abl_staff_cln_n)) # 0





############ Assistant_nurse_unit_manager
abl_pre_staff %>%
  summarise(max = max(abl_staff_anum)) # 0



############ Case_manager
abl_pre_staff %>%
  summarise(max = max(abl_staff_case_m)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_case_m_01_team = case_when(
    abl_staff_case_m_01_team %in% 1 ~ "Oncology",
    abl_staff_case_m_01_team %in% 2 ~ "Radiology",
    abl_staff_case_m_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_case_m_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_case_m_01_team = coalesce(abl_staff_case_m_01_team, abl_staff_case_m_01_other))
abl_pre_staff$abl_staff_case_m_01_team = as.factor(abl_pre_staff$abl_staff_case_m_01_team)

levels(abl_pre_staff$abl_staff_case_m_01_team)

abl_pre_staff %>%
  filter(abl_staff_case_m_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_case_m_01_hours),
            cost = sum*Case_manager)


abl_pre_staff %>%
  filter(abl_staff_case_m_01_team == "HCC/ HPB" | abl_staff_case_m_01_team == "HCC/HPB" | abl_staff_case_m_01_team == "HPB / HCC" |
           abl_staff_case_m_01_team == "HPB/HCC") %>%
  summarise(sum = sum(abl_staff_case_m_01_hours),
            cost = sum*Case_manager)

abl_pre_staff %>%
  filter(abl_staff_case_m_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_case_m_01_hours),
            cost = sum*Case_manager)




############## Nurse_unit_manager	
abl_pre_staff %>%
  summarise(max = max(abl_staff_nur_um)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nur_um_01_team = case_when(
    abl_staff_nur_um_01_team %in% 1 ~ "Oncology",
    abl_staff_nur_um_01_team %in% 2 ~ "Radiology",
    abl_staff_nur_um_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_nur_um_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nur_um_01_team = coalesce(abl_staff_nur_um_01_team, abl_staff_nur_um_01_other))
abl_pre_staff$abl_staff_nur_um_01_team = as.factor(abl_pre_staff$abl_staff_nur_um_01_team)

levels(abl_pre_staff$abl_staff_nur_um_01_team)

abl_pre_staff %>%
  filter(abl_staff_nur_um_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_nur_um_01_hours),
            cost = sum*Nurse_unit_manager)



############## Intern_or_Resident
abl_pre_staff %>%
  summarise(max = max(abl_staff_in_res)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_in_res_01_team = case_when(
    abl_staff_in_res_01_team %in% 1 ~ "Oncology",
    abl_staff_in_res_01_team %in% 2 ~ "Radiology",
    abl_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_in_res_01_team = coalesce(abl_staff_in_res_01_team, abl_staff_in_res_01_other))
abl_pre_staff$abl_staff_in_res_01_team = as.factor(abl_pre_staff$abl_staff_in_res_01_team)

levels(abl_pre_staff$abl_staff_in_res_01_team)

abl_pre_staff %>%
  filter(abl_staff_in_res_01_team == "GAST" | abl_staff_in_res_01_team == "Gastro" | abl_staff_in_res_01_team == "GASTRO" |
           abl_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)


abl_pre_staff %>%
  filter(abl_staff_in_res_01_team == "HPB") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_pre_staff %>%
  filter(abl_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_pre_staff %>%
  filter(abl_staff_in_res_01_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)



############# Registrar
abl_pre_staff %>%
  summarise(max = max(abl_staff_registrar)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_registrar_01_team = case_when(
    abl_staff_registrar_01_team %in% 1 ~ "Oncology",
    abl_staff_registrar_01_team %in% 2 ~ "Radiology",
    abl_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_registrar_01_team = coalesce(abl_staff_registrar_01_team, abl_staff_registrar_01_other))
abl_pre_staff$abl_staff_registrar_01_team = as.factor(abl_pre_staff$abl_staff_registrar_01_team)

levels(abl_pre_staff$abl_staff_registrar_01_team)

abl_pre_staff %>%
  filter(abl_staff_registrar_01_team == "GAST" | abl_staff_registrar_01_team == "Gastro" | abl_staff_registrar_01_team == "GASTRO" |
           abl_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)



############ Fellow
abl_pre_staff %>%
  summarise(max = max(abl_staff_fellow)) # 0




################# Consultant
abl_pre_staff %>%
  summarise(max = max(abl_staff_consultant)) # 2

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_consultant_01_team = case_when(
    abl_staff_consultant_01_team %in% 1 ~ "Oncology",
    abl_staff_consultant_01_team %in% 2 ~ "Radiology",
    abl_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_consultant_01_team = coalesce(abl_staff_consultant_01_team, abl_staff_consultant_01_other))
abl_pre_staff$abl_staff_consultant_01_team = as.factor(abl_pre_staff$abl_staff_consultant_01_team)

levels(abl_pre_staff$abl_staff_consultant_01_team)

abl_pre_staff %>%
  filter(abl_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_pre_staff %>%
  filter(abl_staff_consultant_01_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)



#2
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_consultant_02_team = case_when(
    abl_staff_consultant_02_team %in% 1 ~ "Oncology",
    abl_staff_consultant_02_team %in% 2 ~ "Radiology",
    abl_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_consultant_02_team = coalesce(abl_staff_consultant_02_team, abl_staff_consultant_02_other))
abl_pre_staff$abl_staff_consultant_02_team = as.factor(abl_pre_staff$abl_staff_consultant_02_team)

levels(abl_pre_staff$abl_staff_consultant_02_team)

abl_pre_staff %>%
  filter(abl_staff_consultant_02_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_pre_staff %>%
  filter(abl_staff_consultant_02_team == "Oncology") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_pre_staff %>%
  filter(abl_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)






############### Radiographer
abl_pre_staff %>%
  summarise(max = max(abl_staff_radio)) # 2

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_radio_01_team = case_when(
    abl_staff_radio_01_team %in% 1 ~ "Oncology",
    abl_staff_radio_01_team %in% 2 ~ "Radiology",
    abl_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_radio_01_team = coalesce(abl_staff_radio_01_team, abl_staff_radio_01_other))
abl_pre_staff$abl_staff_radio_01_team = as.factor(abl_pre_staff$abl_staff_radio_01_team)

levels(abl_pre_staff$abl_staff_radio_01_team)

abl_pre_staff %>%
  filter(abl_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_radio_02_team = case_when(
    abl_staff_radio_02_team %in% 1 ~ "Oncology",
    abl_staff_radio_02_team %in% 2 ~ "Radiology",
    abl_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_radio_02_team = coalesce(abl_staff_radio_02_team, abl_staff_radio_02_other))
abl_pre_staff$abl_staff_radio_02_team = as.factor(abl_pre_staff$abl_staff_radio_02_team)

levels(abl_pre_staff$abl_staff_radio_02_team)

abl_pre_staff %>%
  filter(abl_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
abl_pre_staff %>%
  summarise(max = max(abl_staff_ultra)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_ultra_01_team = case_when(
    abl_staff_ultra_01_team %in% 1 ~ "Oncology",
    abl_staff_ultra_01_team %in% 2 ~ "Radiology",
    abl_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_ultra_01_team = coalesce(abl_staff_ultra_01_team, abl_staff_ultra_01_other))
abl_pre_staff$abl_staff_ultra_01_team = as.factor(abl_pre_staff$abl_staff_ultra_01_team)

levels(abl_pre_staff$abl_staff_ultra_01_team)

abl_pre_staff %>%
  filter(abl_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)






################ Nuc_Med_Tech
abl_pre_staff %>%
  summarise(max = max(abl_staff_nuc_mt)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nuc_mt_01_team = case_when(
    abl_staff_nuc_mt_01_team %in% 1 ~ "Oncology",
    abl_staff_nuc_mt_01_team %in% 2 ~ "Radiology",
    abl_staff_nuc_mt_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_nuc_mt_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nuc_mt_01_team = coalesce(abl_staff_nuc_mt_01_team, abl_staff_nuc_mt_01_other))
abl_pre_staff$abl_staff_nuc_mt_01_team = as.factor(abl_pre_staff$abl_staff_nuc_mt_01_team)

levels(abl_pre_staff$abl_staff_nuc_mt_01_team)

abl_pre_staff %>%
  filter(abl_staff_nuc_mt_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(abl_staff_nuc_mt_01_hours),
            cost = sum*Nuc_Med_Tech)




###################### NM_physicist
abl_pre_staff %>%
  summarise(max = max(abl_staff_nm_ph)) # 1

#1
abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nm_ph_01_team = case_when(
    abl_staff_nm_ph_01_team %in% 1 ~ "Oncology",
    abl_staff_nm_ph_01_team %in% 2 ~ "Radiology",
    abl_staff_nm_ph_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_nm_ph_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_pre_staff = abl_pre_staff %>%
  mutate(abl_staff_nm_ph_01_team = coalesce(abl_staff_nm_ph_01_team, abl_staff_nm_ph_01_other))
abl_pre_staff$abl_staff_nm_ph_01_team = as.factor(abl_pre_staff$abl_staff_nm_ph_01_team)

levels(abl_pre_staff$abl_staff_nm_ph_01_team)

abl_pre_staff %>%
  filter(abl_staff_nm_ph_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(abl_staff_nm_ph_01_hours),
            cost = sum*NM_physicist)



########################### Labour_nuclear_medicine
abl_pre_staff %>%
  summarise(max = max(abl_staff_mab_nm)) # 0






#######################################################  To estimate the staff clinical team level cost - intra ##########


######### Administrative_labour

abl_intra_staff %>%
  summarise(max = max(abl_staff_al)) # 2



#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_al_01_team = case_when(
    abl_staff_al_01_team %in% 1 ~ "Oncology",
    abl_staff_al_01_team %in% 2 ~ "Radiology",
    abl_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_al_01_team = coalesce(abl_staff_al_01_team, abl_staff_al_01_other))
abl_intra_staff$abl_staff_al_01_team = as.factor(abl_intra_staff$abl_staff_al_01_team)

levels(abl_intra_staff$abl_staff_al_01_team)

abl_intra_staff %>%
  filter(abl_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)



#2
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_al_02_team = case_when(
    abl_staff_al_02_team %in% 1 ~ "Oncology",
    abl_staff_al_02_team %in% 2 ~ "Radiology",
    abl_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_al_02_team = coalesce(abl_staff_al_02_team, abl_staff_al_02_other))
abl_intra_staff$abl_staff_al_02_team = as.factor(abl_intra_staff$abl_staff_al_02_team)

levels(abl_intra_staff$abl_staff_al_02_team)

abl_intra_staff %>%
  filter(abl_staff_al_02_team == "Admission") %>%
  summarise(sum = sum(abl_staff_al_02_hours),
            cost = sum*Administrative_labour)





######### Assistant_in_nursing

abl_intra_staff %>%
  summarise(max = max(abl_staff_ass_n)) # 0


######## Enrolled_nurse
abl_intra_staff %>%
  summarise(max = max(abl_staff_enr_n)) # 1

abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_enr_n_01_team = case_when(
    abl_staff_enr_n_01_team %in% 1 ~ "Oncology",
    abl_staff_enr_n_01_team %in% 2 ~ "Radiology",
    abl_staff_enr_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_enr_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_enr_n_01_team = coalesce(abl_staff_enr_n_01_team, abl_staff_enr_n_01_other))
abl_intra_staff$abl_staff_enr_n_01_team = as.factor(abl_intra_staff$abl_staff_enr_n_01_team)

levels(abl_intra_staff$abl_staff_enr_n_01_team)

abl_intra_staff %>%
  filter(abl_staff_enr_n_01_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_enr_n_01_hours),
            cost = sum*Enrolled_nurse)

abl_intra_staff %>%
  filter(abl_staff_enr_n_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_enr_n_01_hours),
            cost = sum*Enrolled_nurse)



####### Registered_nurse
abl_intra_staff %>%
  summarise(max = max(abl_staff_reg_n)) # 4


#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_01_team = case_when(
    abl_staff_reg_n_01_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_01_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_01_team = coalesce(abl_staff_reg_n_01_team, abl_staff_reg_n_01_other))
abl_intra_staff$abl_staff_reg_n_01_team = as.factor(abl_intra_staff$abl_staff_reg_n_01_team)

levels(abl_intra_staff$abl_staff_reg_n_01_team)

abl_intra_staff %>%
  filter(abl_staff_reg_n_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_01_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)


#2
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_02_team = case_when(
    abl_staff_reg_n_02_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_02_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_02_team = coalesce(abl_staff_reg_n_02_team, abl_staff_reg_n_02_other))
abl_intra_staff$abl_staff_reg_n_02_team = as.factor(abl_intra_staff$abl_staff_reg_n_02_team)

levels(abl_intra_staff$abl_staff_reg_n_02_team)

abl_intra_staff %>%
  filter(abl_staff_reg_n_02_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_02_team == "GAST") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)


#3
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_03_team = case_when(
    abl_staff_reg_n_03_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_03_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_03_team = coalesce(abl_staff_reg_n_03_team, abl_staff_reg_n_03_other))
abl_intra_staff$abl_staff_reg_n_03_team = as.factor(abl_intra_staff$abl_staff_reg_n_03_team)

levels(abl_intra_staff$abl_staff_reg_n_03_team)

abl_intra_staff %>%
  filter(abl_staff_reg_n_03_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_03_team == "GAST") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_03_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_intra_staff %>%
  filter(abl_staff_reg_n_03_team == "PACU") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)


#4
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_04_team = case_when(
    abl_staff_reg_n_04_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_04_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_reg_n_04_team = coalesce(abl_staff_reg_n_04_team, abl_staff_reg_n_04_other))
abl_intra_staff$abl_staff_reg_n_04_team = as.factor(abl_intra_staff$abl_staff_reg_n_04_team)

levels(abl_intra_staff$abl_staff_reg_n_04_team)

abl_intra_staff %>%
  filter(abl_staff_reg_n_04_team == "PACU") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)




######### Clinical_nurse
abl_intra_staff %>%
  summarise(max = max(abl_staff_cln_n)) # 2


#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_cln_n_01_team = case_when(
    abl_staff_cln_n_01_team %in% 1 ~ "Oncology",
    abl_staff_cln_n_01_team %in% 2 ~ "Radiology",
    abl_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_cln_n_01_team = coalesce(abl_staff_cln_n_01_team, abl_staff_cln_n_01_other))
abl_intra_staff$abl_staff_cln_n_01_team = as.factor(abl_intra_staff$abl_staff_cln_n_01_team)

levels(abl_intra_staff$abl_staff_cln_n_01_team)

abl_intra_staff %>%
  filter(abl_staff_cln_n_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


#2
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_cln_n_02_team = case_when(
    abl_staff_cln_n_02_team %in% 1 ~ "Oncology",
    abl_staff_cln_n_02_team %in% 2 ~ "Radiology",
    abl_staff_cln_n_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_cln_n_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_cln_n_02_team = coalesce(abl_staff_cln_n_02_team, abl_staff_cln_n_02_other))
abl_intra_staff$abl_staff_cln_n_02_team = as.factor(abl_intra_staff$abl_staff_cln_n_02_team)

levels(abl_intra_staff$abl_staff_cln_n_02_team)

abl_intra_staff %>%
  filter(abl_staff_cln_n_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_cln_n_02_hours),
            cost = sum*Clinical_nurse)



############ Assistant_nurse_unit_manager
abl_intra_staff %>%
  summarise(max = max(abl_staff_anum)) # 0



############ Case_manager
abl_intra_staff %>%
  summarise(max = max(abl_staff_case_m)) # 1


#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_case_m_01_team = case_when(
    abl_staff_case_m_01_team %in% 1 ~ "Oncology",
    abl_staff_case_m_01_team %in% 2 ~ "Radiology",
    abl_staff_case_m_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_case_m_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_case_m_01_team = coalesce(abl_staff_case_m_01_team, abl_staff_case_m_01_other))
abl_intra_staff$abl_staff_case_m_01_team = as.factor(abl_intra_staff$abl_staff_case_m_01_team)

levels(abl_intra_staff$abl_staff_case_m_01_team)

abl_intra_staff %>%
  filter(abl_staff_case_m_01_team == "HPB/HCC") %>%
  summarise(sum = sum(abl_staff_case_m_01_hours),
            cost = sum*Clinical_nurse)





############## Nurse_unit_manager	
abl_intra_staff %>%
  summarise(max = max(abl_staff_nur_um)) # 0



############## Intern_or_Resident
abl_intra_staff %>%
  summarise(max = max(abl_staff_in_res)) # 1

#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_in_res_01_team = case_when(
    abl_staff_in_res_01_team %in% 1 ~ "Oncology",
    abl_staff_in_res_01_team %in% 2 ~ "Radiology",
    abl_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_in_res_01_team = coalesce(abl_staff_in_res_01_team, abl_staff_in_res_01_other))
abl_intra_staff$abl_staff_in_res_01_team = as.factor(abl_intra_staff$abl_staff_in_res_01_team)

levels(abl_intra_staff$abl_staff_in_res_01_team)

abl_intra_staff %>%
  filter(abl_staff_in_res_01_team == "GAST" | abl_staff_in_res_01_team == "Gastro" | abl_staff_in_res_01_team == "GASTRO" |
           abl_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)




############# Registrar
abl_intra_staff %>%
  summarise(max = max(abl_staff_registrar)) # 1

#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_registrar_01_team = case_when(
    abl_staff_registrar_01_team %in% 1 ~ "Oncology",
    abl_staff_registrar_01_team %in% 2 ~ "Radiology",
    abl_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_registrar_01_team = coalesce(abl_staff_registrar_01_team, abl_staff_registrar_01_other))
abl_intra_staff$abl_staff_registrar_01_team = as.factor(abl_intra_staff$abl_staff_registrar_01_team)

levels(abl_intra_staff$abl_staff_registrar_01_team)

abl_intra_staff %>%
  filter(abl_staff_registrar_01_team == "GAST" | abl_staff_registrar_01_team == "Gastro" | abl_staff_registrar_01_team == "GASTRO" |
           abl_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_intra_staff %>%
  filter(abl_staff_registrar_01_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_intra_staff %>%
  filter(abl_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)






############ Fellow
abl_intra_staff %>%
  summarise(max = max(abl_staff_fellow)) # 1

#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_fellow_01_team = case_when(
    abl_staff_fellow_01_team %in% 1 ~ "Oncology",
    abl_staff_fellow_01_team %in% 2 ~ "Radiology",
    abl_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_fellow_01_team = coalesce(abl_staff_fellow_01_team, abl_staff_fellow_01_other))
abl_intra_staff$abl_staff_fellow_01_team = as.factor(abl_intra_staff$abl_staff_fellow_01_team)

levels(abl_intra_staff$abl_staff_fellow_01_team)

abl_intra_staff %>%
  filter(abl_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_fellow_01_hours),
            cost = sum*Fellow)



################# Consultant
abl_intra_staff %>%
  summarise(max = max(abl_staff_consultant)) # 2

#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_consultant_01_team = case_when(
    abl_staff_consultant_01_team %in% 1 ~ "Oncology",
    abl_staff_consultant_01_team %in% 2 ~ "Radiology",
    abl_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_consultant_01_team = coalesce(abl_staff_consultant_01_team, abl_staff_consultant_01_other))
abl_intra_staff$abl_staff_consultant_01_team = as.factor(abl_intra_staff$abl_staff_consultant_01_team)

levels(abl_intra_staff$abl_staff_consultant_01_team)

abl_intra_staff %>%
  filter(abl_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_intra_staff %>%
  filter(abl_staff_consultant_01_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_consultant_02_team = case_when(
    abl_staff_consultant_02_team %in% 1 ~ "Oncology",
    abl_staff_consultant_02_team %in% 2 ~ "Radiology",
    abl_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_consultant_02_team = coalesce(abl_staff_consultant_02_team, abl_staff_consultant_02_other))
abl_intra_staff$abl_staff_consultant_02_team = as.factor(abl_intra_staff$abl_staff_consultant_02_team)

levels(abl_intra_staff$abl_staff_consultant_02_team)

abl_intra_staff %>%
  filter(abl_staff_consultant_02_team == "Anaesthesia") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_intra_staff %>%
  filter(abl_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)




############### Radiographer
abl_intra_staff %>%
  summarise(max = max(abl_staff_radio)) # 2

#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_radio_01_team = case_when(
    abl_staff_radio_01_team %in% 1 ~ "Oncology",
    abl_staff_radio_01_team %in% 2 ~ "Radiology",
    abl_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_radio_01_team = coalesce(abl_staff_radio_01_team, abl_staff_radio_01_other))
abl_intra_staff$abl_staff_radio_01_team = as.factor(abl_intra_staff$abl_staff_radio_01_team)

levels(abl_intra_staff$abl_staff_radio_01_team)

abl_intra_staff %>%
  filter(abl_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_radio_02_team = case_when(
    abl_staff_radio_02_team %in% 1 ~ "Oncology",
    abl_staff_radio_02_team %in% 2 ~ "Radiology",
    abl_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_radio_02_team = coalesce(abl_staff_radio_02_team, abl_staff_radio_02_other))
abl_intra_staff$abl_staff_radio_02_team = as.factor(abl_intra_staff$abl_staff_radio_02_team)

levels(abl_intra_staff$abl_staff_radio_02_team)

abl_intra_staff %>%
  filter(abl_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
abl_intra_staff %>%
  summarise(max = max(abl_staff_ultra)) # 1


#1
abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_ultra_01_team = case_when(
    abl_staff_ultra_01_team %in% 1 ~ "Oncology",
    abl_staff_ultra_01_team %in% 2 ~ "Radiology",
    abl_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_intra_staff = abl_intra_staff %>%
  mutate(abl_staff_ultra_01_team = coalesce(abl_staff_ultra_01_team, abl_staff_ultra_01_other))
abl_intra_staff$abl_staff_ultra_01_team = as.factor(abl_intra_staff$abl_staff_ultra_01_team)

levels(abl_intra_staff$abl_staff_ultra_01_team)

abl_intra_staff %>%
  filter(abl_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)




################ Nuc_Med_Tech
abl_intra_staff %>%
  summarise(max = max(abl_staff_nuc_mt)) # 0





###################### NM_physicist
abl_intra_staff %>%
  summarise(max = max(abl_staff_nm_ph)) # 0




##################### Labour_nuclear_medicine
abl_intra_staff %>%
  summarise(max = max(abl_staff_mab_nm)) # 0





#################  To estimate the staff clinical team level cost - Post ##########


######### Administrative_labour

abl_post_staff %>%
  summarise(max = max(abl_staff_al)) # 1



#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_al_01_team = case_when(
    abl_staff_al_01_team %in% 1 ~ "Oncology",
    abl_staff_al_01_team %in% 2 ~ "Radiology",
    abl_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_al_01_team = coalesce(abl_staff_al_01_team, abl_staff_al_01_other))
abl_post_staff$abl_staff_al_01_team = as.factor(abl_post_staff$abl_staff_al_01_team)

levels(abl_post_staff$abl_staff_al_01_team)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "2A") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "2C") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "4BR") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "4C") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "4E") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "CARD") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "CCU") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "GAST" | abl_staff_al_01_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "GenMedicine" | abl_staff_al_01_team == "Medical") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "RESP") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "SCU") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "surgical" | abl_staff_al_01_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)

abl_post_staff %>%
  filter(abl_staff_al_01_team == "UROL") %>%
  summarise(sum = sum(abl_staff_al_01_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

abl_post_staff %>%
  summarise(max = max(abl_staff_ass_n)) # 0


######## Enrolled_nurse
abl_post_staff %>%
  summarise(max = max(abl_staff_enr_n)) # 0






####### Registered_nurse
abl_post_staff %>%
  summarise(max = max(abl_staff_reg_n)) # 18


#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_01_team = case_when(
    abl_staff_reg_n_01_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_01_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_01_team = coalesce(abl_staff_reg_n_01_team, abl_staff_reg_n_01_other))
abl_post_staff$abl_staff_reg_n_01_team = as.factor(abl_post_staff$abl_staff_reg_n_01_team)

levels(abl_post_staff$abl_staff_reg_n_01_team)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "4E") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "CCU") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "GAST" | abl_staff_reg_n_01_team == "Gasto" | abl_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "Medical" | abl_staff_reg_n_01_team == "GenMedicine") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "SCU") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_01_team == "UROL") %>%
  summarise(sum = sum(abl_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)




#2
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_02_team = case_when(
    abl_staff_reg_n_02_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_02_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_02_team = coalesce(abl_staff_reg_n_02_team, abl_staff_reg_n_02_other))
abl_post_staff$abl_staff_reg_n_02_team = as.factor(abl_post_staff$abl_staff_reg_n_02_team)

levels(abl_post_staff$abl_staff_reg_n_02_team)


abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "4E") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "CCU") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "GAST" | abl_staff_reg_n_02_team == "Gasto" | abl_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "Medical" | abl_staff_reg_n_02_team == "GenMedicine") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "SCU") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_02_team == "UROL") %>%
  summarise(sum = sum(abl_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)




#3
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_03_team = case_when(
    abl_staff_reg_n_03_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_03_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_03_team = coalesce(abl_staff_reg_n_03_team, abl_staff_reg_n_03_other))
abl_post_staff$abl_staff_reg_n_03_team = as.factor(abl_post_staff$abl_staff_reg_n_03_team)

levels(abl_post_staff$abl_staff_reg_n_03_team)



abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "4BR") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "4C") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "4E") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "CCU") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "GAST" | abl_staff_reg_n_03_team == "Gasto" | abl_staff_reg_n_03_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "Medical" | abl_staff_reg_n_03_team == "GenMedicine") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "SCU") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_03_team == "UROL") %>%
  summarise(sum = sum(abl_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



#4
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_04_team = case_when(
    abl_staff_reg_n_04_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_04_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_04_team = coalesce(abl_staff_reg_n_04_team, abl_staff_reg_n_04_other))
abl_post_staff$abl_staff_reg_n_04_team = as.factor(abl_post_staff$abl_staff_reg_n_04_team)

levels(abl_post_staff$abl_staff_reg_n_04_team)

abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "GAST" | abl_staff_reg_n_04_team == "Gastro" | abl_staff_reg_n_04_team == "GAST") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "CCU") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


abl_post_staff %>%
  filter(abl_staff_reg_n_04_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)






#5
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_05_team = case_when(
    abl_staff_reg_n_05_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_05_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_05_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_05_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_05_team = coalesce(abl_staff_reg_n_05_team, abl_staff_reg_n_05_other))
abl_post_staff$abl_staff_reg_n_05_team = as.factor(abl_post_staff$abl_staff_reg_n_05_team)

levels(abl_post_staff$abl_staff_reg_n_05_team)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_05_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)





#6
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_06_team = case_when(
    abl_staff_reg_n_06_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_06_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_06_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_06_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_06_team = coalesce(abl_staff_reg_n_06_team, abl_staff_reg_n_06_other))
abl_post_staff$abl_staff_reg_n_06_team = as.factor(abl_post_staff$abl_staff_reg_n_06_team)

levels(abl_post_staff$abl_staff_reg_n_06_team)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "2A") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_06_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)



#7
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_07_team = case_when(
    abl_staff_reg_n_07_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_07_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_07_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_07_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_07_team = coalesce(abl_staff_reg_n_07_team, abl_staff_reg_n_07_other))
abl_post_staff$abl_staff_reg_n_07_team = as.factor(abl_post_staff$abl_staff_reg_n_07_team)

levels(abl_post_staff$abl_staff_reg_n_07_team)

abl_post_staff %>%
  filter(abl_staff_reg_n_07_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_07_team == "CARD") %>%
  summarise(sum = sum(abl_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_07_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_07_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)




#8
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_08_team = case_when(
    abl_staff_reg_n_08_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_08_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_08_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_08_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_08_team = coalesce(abl_staff_reg_n_08_team, abl_staff_reg_n_08_other))
abl_post_staff$abl_staff_reg_n_08_team = as.factor(abl_post_staff$abl_staff_reg_n_08_team)

levels(abl_post_staff$abl_staff_reg_n_08_team)


abl_post_staff %>%
  filter(abl_staff_reg_n_08_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_08_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_08_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_08_hours),
            cost = sum*Registered_nurse)


#9
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_09_team = case_when(
    abl_staff_reg_n_09_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_09_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_09_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_09_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_09_team = coalesce(abl_staff_reg_n_09_team, abl_staff_reg_n_09_other))
abl_post_staff$abl_staff_reg_n_09_team = as.factor(abl_post_staff$abl_staff_reg_n_09_team)

levels(abl_post_staff$abl_staff_reg_n_09_team)


abl_post_staff %>%
  filter(abl_staff_reg_n_09_team == "2C") %>%
  summarise(sum = sum(abl_staff_reg_n_09_hours),
            cost = sum*Registered_nurse)

abl_post_staff %>%
  filter(abl_staff_reg_n_09_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_09_hours),
            cost = sum*Registered_nurse)




#10
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_10_team = case_when(
    abl_staff_reg_n_10_team %in% 1 ~ "Oncology",
    abl_staff_reg_n_10_team %in% 2 ~ "Radiology",
    abl_staff_reg_n_10_team %in% 3 ~ "Nuclear medicine",
    abl_staff_reg_n_10_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_reg_n_10_team = coalesce(abl_staff_reg_n_10_team, abl_staff_reg_n_10_other))
abl_post_staff$abl_staff_reg_n_10_team = as.factor(abl_post_staff$abl_staff_reg_n_10_team)

levels(abl_post_staff$abl_staff_reg_n_10_team)


abl_post_staff %>%
  filter(abl_staff_reg_n_10_team == "RESP") %>%
  summarise(sum = sum(abl_staff_reg_n_10_hours),
            cost = sum*Registered_nurse)






######### Clinical_nurse
abl_post_staff %>%
  summarise(max = max(abl_staff_cln_n)) # 0


#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_cln_n_01_team = case_when(
    abl_staff_cln_n_01_team %in% 1 ~ "Oncology",
    abl_staff_cln_n_01_team %in% 2 ~ "Radiology",
    abl_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_cln_n_01_team = coalesce(abl_staff_cln_n_01_team, abl_staff_cln_n_01_other))
abl_post_staff$abl_staff_cln_n_01_team = as.factor(abl_post_staff$abl_staff_cln_n_01_team)

levels(abl_post_staff$abl_staff_cln_n_01_team)

abl_post_staff %>%
  filter(abl_staff_cln_n_01_team == "5D") %>%
  summarise(sum = sum(abl_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


abl_post_staff %>%
  filter(abl_staff_cln_n_01_team == "HCC CNC") %>%
  summarise(sum = sum(abl_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)

abl_post_staff %>%
  filter(abl_staff_cln_n_01_team == "PAH 11") %>%
  summarise(sum = sum(abl_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)






############ Assistant_nurse_unit_manager
abl_post_staff %>%
  summarise(max = max(abl_staff_anum)) # 0



############ Case_manager
abl_post_staff %>%
  summarise(max = max(abl_staff_case_m)) # 0




############## Nurse_unit_manager	
abl_post_staff %>%
  summarise(max = max(abl_staff_nur_um)) # 0


#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_nur_um_01_team = case_when(
    abl_staff_nur_um_01_team %in% 1 ~ "Oncology",
    abl_staff_nur_um_01_team %in% 2 ~ "Radiology",
    abl_staff_nur_um_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_nur_um_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_nur_um_01_team = coalesce(abl_staff_nur_um_01_team, abl_staff_nur_um_01_other))
abl_post_staff$abl_staff_nur_um_01_team = as.factor(abl_post_staff$abl_staff_nur_um_01_team)

levels(abl_post_staff$abl_staff_nur_um_01_team)


abl_post_staff %>%
  filter(abl_staff_nur_um_01_team == "HPB") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Nurse_unit_manager)





############## Intern_or_Resident
abl_post_staff %>%
  summarise(max = max(abl_staff_in_res)) # 10

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_01_team = case_when(
    abl_staff_in_res_01_team %in% 1 ~ "Oncology",
    abl_staff_in_res_01_team %in% 2 ~ "Radiology",
    abl_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_01_team = coalesce(abl_staff_in_res_01_team, abl_staff_in_res_01_other))
abl_post_staff$abl_staff_in_res_01_team = as.factor(abl_post_staff$abl_staff_in_res_01_team)

levels(abl_post_staff$abl_staff_in_res_01_team)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "UROL" | abl_staff_in_res_01_team == "Urology") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "GAST" | abl_staff_in_res_01_team == "Gastro" | abl_staff_in_res_01_team == "GASTRO" |
           abl_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "HPB") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "LIVR" ) %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_01_team == "Surgery" | abl_staff_in_res_01_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)


#2
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_02_team = case_when(
    abl_staff_in_res_02_team %in% 1 ~ "Oncology",
    abl_staff_in_res_02_team %in% 2 ~ "Radiology",
    abl_staff_in_res_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_02_team = coalesce(abl_staff_in_res_02_team, abl_staff_in_res_02_other))
abl_post_staff$abl_staff_in_res_02_team = as.factor(abl_post_staff$abl_staff_in_res_02_team)

levels(abl_post_staff$abl_staff_in_res_02_team)


abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "UROL" | abl_staff_in_res_02_team == "Urology") %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "GAST" | abl_staff_in_res_02_team == "Gastro" | abl_staff_in_res_02_team == "GASTRO" |
           abl_staff_in_res_02_team == "GAST") %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "HPB") %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "LIVR" ) %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_02_team == "Surgery" | abl_staff_in_res_02_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_in_res_02_hours),
            cost = sum*Intern_or_Resident)




#3
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_03_team = case_when(
    abl_staff_in_res_03_team %in% 1 ~ "Oncology",
    abl_staff_in_res_03_team %in% 2 ~ "Radiology",
    abl_staff_in_res_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_03_team = coalesce(abl_staff_in_res_03_team, abl_staff_in_res_03_other))
abl_post_staff$abl_staff_in_res_03_team = as.factor(abl_post_staff$abl_staff_in_res_03_team)

levels(abl_post_staff$abl_staff_in_res_03_team)


abl_post_staff %>%
  filter(abl_staff_in_res_03_team == "GAST" | abl_staff_in_res_03_team == "Gastro" | abl_staff_in_res_03_team == "GASTRO" |
           abl_staff_in_res_03_team == "GAST") %>%
  summarise(sum = sum(abl_staff_in_res_03_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_03_team == "HPB") %>%
  summarise(sum = sum(abl_staff_in_res_03_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_03_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_in_res_03_hours),
            cost = sum*Intern_or_Resident)

abl_post_staff %>%
  filter(abl_staff_in_res_03_team == "RESP") %>%
  summarise(sum = sum(abl_staff_in_res_03_hours),
            cost = sum*Intern_or_Resident)




#4
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_04_team = case_when(
    abl_staff_in_res_04_team %in% 1 ~ "Oncology",
    abl_staff_in_res_04_team %in% 2 ~ "Radiology",
    abl_staff_in_res_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_04_team = coalesce(abl_staff_in_res_04_team, abl_staff_in_res_04_other))
abl_post_staff$abl_staff_in_res_04_team = as.factor(abl_post_staff$abl_staff_in_res_04_team)

levels(abl_post_staff$abl_staff_in_res_04_team)


abl_post_staff %>%
  filter(abl_staff_in_res_04_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_in_res_04_hours),
            cost = sum*Intern_or_Resident)



#5
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_05_team = case_when(
    abl_staff_in_res_05_team %in% 1 ~ "Oncology",
    abl_staff_in_res_05_team %in% 2 ~ "Radiology",
    abl_staff_in_res_05_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_05_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_05_team = coalesce(abl_staff_in_res_05_team, abl_staff_in_res_05_other))
abl_post_staff$abl_staff_in_res_05_team = as.factor(abl_post_staff$abl_staff_in_res_05_team)

levels(abl_post_staff$abl_staff_in_res_05_team)


abl_post_staff %>%
  filter(abl_staff_in_res_05_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_05_hours),
            cost = sum*Intern_or_Resident)

#6
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_06_team = case_when(
    abl_staff_in_res_06_team %in% 1 ~ "Oncology",
    abl_staff_in_res_06_team %in% 2 ~ "Radiology",
    abl_staff_in_res_06_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_06_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_06_team = coalesce(abl_staff_in_res_06_team, abl_staff_in_res_06_other))
abl_post_staff$abl_staff_in_res_06_team = as.factor(abl_post_staff$abl_staff_in_res_06_team)

levels(abl_post_staff$abl_staff_in_res_06_team)


abl_post_staff %>%
  filter(abl_staff_in_res_06_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_06_hours),
            cost = sum*Intern_or_Resident)

#7
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_07_team = case_when(
    abl_staff_in_res_07_team %in% 1 ~ "Oncology",
    abl_staff_in_res_07_team %in% 2 ~ "Radiology",
    abl_staff_in_res_07_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_07_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_07_team = coalesce(abl_staff_in_res_07_team, abl_staff_in_res_07_other))
abl_post_staff$abl_staff_in_res_07_team = as.factor(abl_post_staff$abl_staff_in_res_07_team)

levels(abl_post_staff$abl_staff_in_res_07_team)


abl_post_staff %>%
  filter(abl_staff_in_res_07_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_in_res_07_hours),
            cost = sum*Intern_or_Resident)

#8
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_08_team = case_when(
    abl_staff_in_res_08_team %in% 1 ~ "Oncology",
    abl_staff_in_res_08_team %in% 2 ~ "Radiology",
    abl_staff_in_res_08_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_08_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_08_team = coalesce(abl_staff_in_res_08_team, abl_staff_in_res_08_other))
abl_post_staff$abl_staff_in_res_08_team = as.factor(abl_post_staff$abl_staff_in_res_08_team)

levels(abl_post_staff$abl_staff_in_res_08_team)


abl_post_staff %>%
  filter(abl_staff_in_res_08_team == "RESP") %>%
  summarise(sum = sum(abl_staff_in_res_08_hours),
            cost = sum*Intern_or_Resident)

#9
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_09_team = case_when(
    abl_staff_in_res_09_team %in% 1 ~ "Oncology",
    abl_staff_in_res_09_team %in% 2 ~ "Radiology",
    abl_staff_in_res_09_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_09_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_09_team = coalesce(abl_staff_in_res_09_team, abl_staff_in_res_09_other))
abl_post_staff$abl_staff_in_res_09_team = as.factor(abl_post_staff$abl_staff_in_res_09_team)

levels(abl_post_staff$abl_staff_in_res_09_team)


abl_post_staff %>%
  filter(abl_staff_in_res_09_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_in_res_09_hours),
            cost = sum*Intern_or_Resident)


#10
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_10_team = case_when(
    abl_staff_in_res_10_team %in% 1 ~ "Oncology",
    abl_staff_in_res_10_team %in% 2 ~ "Radiology",
    abl_staff_in_res_10_team %in% 3 ~ "Nuclear medicine",
    abl_staff_in_res_10_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_in_res_10_team = coalesce(abl_staff_in_res_10_team, abl_staff_in_res_10_other))
abl_post_staff$abl_staff_in_res_10_team = as.factor(abl_post_staff$abl_staff_in_res_10_team)

levels(abl_post_staff$abl_staff_in_res_10_team)


abl_post_staff %>%
  filter(abl_staff_in_res_10_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_in_res_10_hours),
            cost = sum*Intern_or_Resident)





############# Registrar
abl_post_staff %>%
  summarise(max = max(abl_staff_registrar)) # 10

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_01_team = case_when(
    abl_staff_registrar_01_team %in% 1 ~ "Oncology",
    abl_staff_registrar_01_team %in% 2 ~ "Radiology",
    abl_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_01_team = coalesce(abl_staff_registrar_01_team, abl_staff_registrar_01_other))
abl_post_staff$abl_staff_registrar_01_team = as.factor(abl_post_staff$abl_staff_registrar_01_team)

levels(abl_post_staff$abl_staff_registrar_01_team)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "GAST" | abl_staff_registrar_01_team == "Gastro" | abl_staff_registrar_01_team == "GASTRO" |
           abl_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)


abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "HPB") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)


abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "LIVR") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "Medical") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "RESP") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_01_team == "UROL" | abl_staff_registrar_01_team == "Urology") %>%
  summarise(sum = sum(abl_staff_registrar_01_hours),
            cost = sum*Registrar)



#2
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_02_team = case_when(
    abl_staff_registrar_02_team %in% 1 ~ "Oncology",
    abl_staff_registrar_02_team %in% 2 ~ "Radiology",
    abl_staff_registrar_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_02_team = coalesce(abl_staff_registrar_02_team, abl_staff_registrar_02_other))
abl_post_staff$abl_staff_registrar_02_team = as.factor(abl_post_staff$abl_staff_registrar_02_team)

levels(abl_post_staff$abl_staff_registrar_02_team)


abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "GAST" | abl_staff_registrar_02_team == "Gastro" | abl_staff_registrar_02_team == "GASTRO" |
           abl_staff_registrar_02_team == "GAST") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)


abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "HPB") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)


abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "Ward call") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "Endocrine") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "RESP") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_02_team == "UROL" | abl_staff_registrar_02_team == "Urology") %>%
  summarise(sum = sum(abl_staff_registrar_02_hours),
            cost = sum*Registrar)




#3
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_03_team = case_when(
    abl_staff_registrar_03_team %in% 1 ~ "Oncology",
    abl_staff_registrar_03_team %in% 2 ~ "Radiology",
    abl_staff_registrar_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_03_team = coalesce(abl_staff_registrar_03_team, abl_staff_registrar_03_other))
abl_post_staff$abl_staff_registrar_03_team = as.factor(abl_post_staff$abl_staff_registrar_03_team)

levels(abl_post_staff$abl_staff_registrar_03_team)

abl_post_staff %>%
  filter(abl_staff_registrar_03_team == "Medical") %>%
  summarise(sum = sum(abl_staff_registrar_03_hours),
            cost = sum*Registrar)

abl_post_staff %>%
  filter(abl_staff_registrar_03_team == "RESP") %>%
  summarise(sum = sum(abl_staff_registrar_03_hours),
            cost = sum*Registrar)


abl_post_staff %>%
  filter(abl_staff_registrar_03_team == "Urology") %>%
  summarise(sum = sum(abl_staff_registrar_03_hours),
            cost = sum*Registrar)


#4
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_04_team = case_when(
    abl_staff_registrar_04_team %in% 1 ~ "Oncology",
    abl_staff_registrar_04_team %in% 2 ~ "Radiology",
    abl_staff_registrar_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_04_team = coalesce(abl_staff_registrar_04_team, abl_staff_registrar_04_other))
abl_post_staff$abl_staff_registrar_04_team = as.factor(abl_post_staff$abl_staff_registrar_04_team)

levels(abl_post_staff$abl_staff_registrar_04_team)

abl_post_staff %>%
  filter(abl_staff_registrar_04_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_registrar_04_hours),
            cost = sum*Registrar)



#5
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_05_team = case_when(
    abl_staff_registrar_05_team %in% 1 ~ "Oncology",
    abl_staff_registrar_05_team %in% 2 ~ "Radiology",
    abl_staff_registrar_05_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_05_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_05_team = coalesce(abl_staff_registrar_05_team, abl_staff_registrar_05_other))
abl_post_staff$abl_staff_registrar_05_team = as.factor(abl_post_staff$abl_staff_registrar_05_team)

levels(abl_post_staff$abl_staff_registrar_05_team)

abl_post_staff %>%
  filter(abl_staff_registrar_05_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_registrar_05_hours),
            cost = sum*Registrar)



#6
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_06_team = case_when(
    abl_staff_registrar_06_team %in% 1 ~ "Oncology",
    abl_staff_registrar_06_team %in% 2 ~ "Radiology",
    abl_staff_registrar_06_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_06_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_06_team = coalesce(abl_staff_registrar_06_team, abl_staff_registrar_06_other))
abl_post_staff$abl_staff_registrar_06_team = as.factor(abl_post_staff$abl_staff_registrar_06_team)

levels(abl_post_staff$abl_staff_registrar_06_team)

abl_post_staff %>%
  filter(abl_staff_registrar_06_team == "RESP") %>%
  summarise(sum = sum(abl_staff_registrar_06_hours),
            cost = sum*Registrar)



#7
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_07_team = case_when(
    abl_staff_registrar_07_team %in% 1 ~ "Oncology",
    abl_staff_registrar_07_team %in% 2 ~ "Radiology",
    abl_staff_registrar_07_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_07_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_07_team = coalesce(abl_staff_registrar_07_team, abl_staff_registrar_07_other))
abl_post_staff$abl_staff_registrar_07_team = as.factor(abl_post_staff$abl_staff_registrar_07_team)

levels(abl_post_staff$abl_staff_registrar_07_team)

abl_post_staff %>%
  filter(abl_staff_registrar_07_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_registrar_07_hours),
            cost = sum*Registrar)



#8
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_08_team = case_when(
    abl_staff_registrar_08_team %in% 1 ~ "Oncology",
    abl_staff_registrar_08_team %in% 2 ~ "Radiology",
    abl_staff_registrar_08_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_08_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_08_team = coalesce(abl_staff_registrar_08_team, abl_staff_registrar_08_other))
abl_post_staff$abl_staff_registrar_08_team = as.factor(abl_post_staff$abl_staff_registrar_08_team)

levels(abl_post_staff$abl_staff_registrar_08_team)

abl_post_staff %>%
  filter(abl_staff_registrar_08_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_registrar_08_hours),
            cost = sum*Registrar)



#9
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_09_team = case_when(
    abl_staff_registrar_09_team %in% 1 ~ "Oncology",
    abl_staff_registrar_09_team %in% 2 ~ "Radiology",
    abl_staff_registrar_09_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_09_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_09_team = coalesce(abl_staff_registrar_09_team, abl_staff_registrar_09_other))
abl_post_staff$abl_staff_registrar_09_team = as.factor(abl_post_staff$abl_staff_registrar_09_team)

levels(abl_post_staff$abl_staff_registrar_09_team)

abl_post_staff %>%
  filter(abl_staff_registrar_09_team == "RESP" ) %>%
  summarise(sum = sum(abl_staff_registrar_09_hours),
            cost = sum*Registrar)



#10
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_10_team = case_when(
    abl_staff_registrar_10_team %in% 1 ~ "Oncology",
    abl_staff_registrar_10_team %in% 2 ~ "Radiology",
    abl_staff_registrar_10_team %in% 3 ~ "Nuclear medicine",
    abl_staff_registrar_10_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_registrar_10_team = coalesce(abl_staff_registrar_10_team, abl_staff_registrar_10_other))
abl_post_staff$abl_staff_registrar_10_team = as.factor(abl_post_staff$abl_staff_registrar_10_team)

levels(abl_post_staff$abl_staff_registrar_10_team)

abl_post_staff %>%
  filter(abl_staff_registrar_10_team == "Radiology" ) %>%
  summarise(sum = sum(abl_staff_registrar_10_hours),
            cost = sum*Registrar)





############ Fellow
abl_post_staff %>%
  summarise(max = max(abl_staff_fellow)) # 1

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_fellow_01_team = case_when(
    abl_staff_fellow_01_team %in% 1 ~ "Oncology",
    abl_staff_fellow_01_team %in% 2 ~ "Radiology",
    abl_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_fellow_01_team = coalesce(abl_staff_fellow_01_team, abl_staff_fellow_01_other))
abl_post_staff$abl_staff_fellow_01_team = as.factor(abl_post_staff$abl_staff_fellow_01_team)

levels(abl_post_staff$abl_staff_fellow_01_team)

abl_post_staff %>%
  filter(abl_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_fellow_01_hours),
            cost = sum*Fellow)

abl_post_staff %>%
  filter(abl_staff_fellow_01_team == "UROL") %>%
  summarise(sum = sum(abl_staff_fellow_01_hours),
            cost = sum*Fellow)

abl_post_staff %>%
  filter(abl_staff_fellow_01_team == "GAST" | abl_staff_fellow_01_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_fellow_01_hours),
            cost = sum*Fellow)





################# Consultant
abl_post_staff %>%
  summarise(max = max(abl_staff_consultant)) # 4

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_01_team = case_when(
    abl_staff_consultant_01_team %in% 1 ~ "Oncology",
    abl_staff_consultant_01_team %in% 2 ~ "Radiology",
    abl_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_01_team = coalesce(abl_staff_consultant_01_team, abl_staff_consultant_01_other))
abl_post_staff$abl_staff_consultant_01_team = as.factor(abl_post_staff$abl_staff_consultant_01_team)

levels(abl_post_staff$abl_staff_consultant_01_team)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "GAST" | abl_staff_consultant_01_team == "Gastro") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "RESP") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "LIVR") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "Surgical") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_01_team == "UROL" | abl_staff_consultant_01_team == "Urology") %>%
  summarise(sum = sum(abl_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_02_team = case_when(
    abl_staff_consultant_02_team %in% 1 ~ "Oncology",
    abl_staff_consultant_02_team %in% 2 ~ "Radiology",
    abl_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_02_team = coalesce(abl_staff_consultant_02_team, abl_staff_consultant_02_other))
abl_post_staff$abl_staff_consultant_02_team = as.factor(abl_post_staff$abl_staff_consultant_02_team)

levels(abl_post_staff$abl_staff_consultant_02_team)

abl_post_staff %>%
  filter(abl_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_02_team == "RESP") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_02_team == "Thoracic Medicine") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)

abl_post_staff %>%
  filter(abl_staff_consultant_02_team == "Urology") %>%
  summarise(sum = sum(abl_staff_consultant_02_hours),
            cost = sum*Consultant)



#3
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_03_team = case_when(
    abl_staff_consultant_03_team %in% 1 ~ "Oncology",
    abl_staff_consultant_03_team %in% 2 ~ "Radiology",
    abl_staff_consultant_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_03_team = coalesce(abl_staff_consultant_03_team, abl_staff_consultant_03_other))
abl_post_staff$abl_staff_consultant_03_team = as.factor(abl_post_staff$abl_staff_consultant_03_team)

levels(abl_post_staff$abl_staff_consultant_03_team)

abl_post_staff %>%
  filter(abl_staff_consultant_03_team == "RESP") %>%
  summarise(sum = sum(abl_staff_consultant_03_hours),
            cost = sum*Consultant)


#4
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_04_team = case_when(
    abl_staff_consultant_04_team %in% 1 ~ "Oncology",
    abl_staff_consultant_04_team %in% 2 ~ "Radiology",
    abl_staff_consultant_04_team %in% 3 ~ "Nuclear medicine",
    abl_staff_consultant_04_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_consultant_04_team = coalesce(abl_staff_consultant_04_team, abl_staff_consultant_04_other))
abl_post_staff$abl_staff_consultant_04_team = as.factor(abl_post_staff$abl_staff_consultant_04_team)

levels(abl_post_staff$abl_staff_consultant_04_team)

abl_post_staff %>%
  filter(abl_staff_consultant_04_team == "RESP") %>%
  summarise(sum = sum(abl_staff_consultant_04_hours),
            cost = sum*Consultant)




############### Radiographer
abl_post_staff %>%
  summarise(max = max(abl_staff_radio)) # 3

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_01_team = case_when(
    abl_staff_radio_01_team %in% 1 ~ "Oncology",
    abl_staff_radio_01_team %in% 2 ~ "Radiology",
    abl_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_01_team = coalesce(abl_staff_radio_01_team, abl_staff_radio_01_other))
abl_post_staff$abl_staff_radio_01_team = as.factor(abl_post_staff$abl_staff_radio_01_team)

levels(abl_post_staff$abl_staff_radio_01_team)

abl_post_staff %>%
  filter(abl_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_02_team = case_when(
    abl_staff_radio_02_team %in% 1 ~ "Oncology",
    abl_staff_radio_02_team %in% 2 ~ "Radiology",
    abl_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_02_team = coalesce(abl_staff_radio_02_team, abl_staff_radio_02_other))
abl_post_staff$abl_staff_radio_02_team = as.factor(abl_post_staff$abl_staff_radio_02_team)

levels(abl_post_staff$abl_staff_radio_02_team)

abl_post_staff %>%
  filter(abl_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_02_hours),
            cost = sum*Radiographer)

#3
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_03_team = case_when(
    abl_staff_radio_03_team %in% 3 ~ "Oncology",
    abl_staff_radio_03_team %in% 2 ~ "Radiology",
    abl_staff_radio_03_team %in% 3 ~ "Nuclear medicine",
    abl_staff_radio_03_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_radio_03_team = coalesce(abl_staff_radio_03_team, abl_staff_radio_03_other))
abl_post_staff$abl_staff_radio_03_team = as.factor(abl_post_staff$abl_staff_radio_03_team)

levels(abl_post_staff$abl_staff_radio_03_team)

abl_post_staff %>%
  filter(abl_staff_radio_03_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_radio_03_hours),
            cost = sum*Radiographer)





############# Ultrasonographer
abl_post_staff %>%
  summarise(max = max(abl_staff_ultra)) # 0

#1
abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_ultra_01_team = case_when(
    abl_staff_ultra_01_team %in% 1 ~ "Oncology",
    abl_staff_ultra_01_team %in% 2 ~ "Radiology",
    abl_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    abl_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


abl_post_staff = abl_post_staff %>%
  mutate(abl_staff_ultra_01_team = coalesce(abl_staff_ultra_01_team, abl_staff_ultra_01_other))
abl_post_staff$abl_staff_ultra_01_team = as.factor(abl_post_staff$abl_staff_ultra_01_team)

levels(abl_post_staff$abl_staff_ultra_01_team)

abl_post_staff %>%
  filter(abl_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(abl_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)





################ Nuc_Med_Tech
abl_post_staff %>%
  summarise(max = max(abl_staff_nuc_mt)) # 0




###################### NM_physicist
abl_post_staff %>%
  summarise(max = max(abl_staff_nm_ph)) # 0



########################### Labour_nuclear_medicine
abl_post_staff %>%
  summarise(max = max(abl_staff_mab_nm)) # 0



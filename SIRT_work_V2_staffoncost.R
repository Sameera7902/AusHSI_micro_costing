# MN Oncology porject - SIRT - Workup



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
sirt_data = read.delim("SIRT_data.csv", sep = ",") %>% clean_names()



##############################################################
############ Demographic characteristics #####################  ------ Similar to workup & delivery
##############################################################

sirt_demo = sirt_data %>%
  select(1: 13) %>%
  filter(redcap_repeat_instrument == "demo")



# Age
sirt_demo %>%
  summarise(age_mean = mean(age),
            sd = sd(age))


# Gender
sirt_demo %>%
  filter(sex == 1) %>%
  summarise(num = n(),
            per = num/9) # Change the number


# primary_site
sirt_demo$primary_site = as.factor(sirt_demo$primary_site)

sirt_demo %>%
  group_by(primary_site) %>%
  summarise(num = n(),
            per = num/9) # Change the number


# secondary_site
sirt_demo$secondary_site = as.factor(sirt_demo$secondary_site)

sirt_demo %>%
  group_by(secondary_site) %>%
  summarise(num = n(),
            per = num/9) # Change the number


# albi_score
sirt_demo$albi_score = as.factor(sirt_demo$albi_score)

sirt_demo %>%
  group_by(albi_score) %>%
  summarise(num = n(),
            per = num/9) # Change the number


# albi_score_at_sirt
sirt_demo$albi_score_at_tace = as.factor(sirt_demo$albi_score_at_tace)

sirt_demo %>%
  group_by(albi_score_at_tace) %>%
  summarise(num = n(),
            per = num/9) # Change the number




##############################################################
################## admission_details  ######################## ------ Similar to workup & delivery
##############################################################




sirt_admin = sirt_data %>%
  select(1:4, admin_reason : wau_received) %>%
  filter(redcap_repeat_instrument == "admission_details")

sirt_admin_work = sirt_admin %>%
  filter(redcap_repeat_instance == 1)

sirt_admin_deli = sirt_admin %>%
  filter(redcap_repeat_instance == 2)

# admin_reason




#  days
sirt_admin_work$days = as.factor(sirt_admin_work$days)
sirt_admin_deli$days = as.factor(sirt_admin_deli$days)


sirt_admin_work %>%
  group_by(days) %>%
  summarise(num = n(),
            per = num/9) # Change the number

sirt_admin_deli %>%
  group_by(days) %>%
  summarise(num = n(),
            per = num/8) # Change the number


sirt_admin_work$days = as.double(sirt_admin_work$days)
sirt_admin_deli$days = as.double(sirt_admin_deli$days)

sirt_admin_work %>%
  summarise(ave = mean(days),
            sd = sd(days))

sirt_admin_deli %>%
  summarise(ave = mean(days),
            sd = sd(days))



# ar_drg
sirt_admin_work$ar_drg = as.factor(sirt_admin_work$ar_drg)

tab_1 = sirt_admin_work %>%
  group_by(ar_drg) %>%
  summarise(num = n(),
            per = num/9) # Change the number


sirt_admin_deli$ar_drg = as.factor(sirt_admin_deli$ar_drg)

sirt_admin_deli %>%
  group_by(ar_drg) %>%
  summarise(num = n(),
            per = num/8) # Change the number



# wau
sirt_admin_work$wau = as.factor(sirt_admin_work$wau)

sirt_admin_work %>%
  group_by(wau) %>%
  summarise(num = n(),
            per = num/9) # Change the number


sirt_admin_deli$wau = as.factor(sirt_admin_deli$wau)

sirt_admin_deli %>%
  group_by(wau) %>%
  summarise(num = n(),
            per = num/8) # Change the number



# admit_ward vs wau_received
sirt_admin_work$admit_ward = as.factor(sirt_admin_work$admit_ward)
sirt_admin_work$wau_received = as.factor(sirt_admin_work$wau_received)

sirt_admin_deli$admit_ward = as.factor(sirt_admin_deli$admit_ward)
sirt_admin_deli$wau_received = as.factor(sirt_admin_deli$wau_received)


levels(sirt_admin_work$admit_ward)
levels(sirt_admin_work$wau_received)

levels(sirt_admin_deli$admit_ward)
levels(sirt_admin_deli$wau_received)


Wau_fun_work = function(v1, v2) {
  sirt_admin_work %>%
    filter(admit_ward == v1) %>%
    group_by(wau_received) %>%
    summarise(num = n(),
              per = num/v2) # Change the number
  }

Wau_fun_work (v1 = "23 HR", v2 = 19) #
Wau_fun_work (v1 = "5D", v2 = 3) #
Wau_fun_work (v1 = "Radiology", v2 = 47)
Wau_fun_work (v1 = "WATT", v2 = 7)
Wau_fun_work (v1 = "XRAY", v2 = 1)
Wau_fun_work (v1 = "4E", v2 = 1) #
Wau_fun_work (v1 = "7AN", v2 = 1) #



Wau_fun_deli = function(v1, v2) {
  sirt_admin_deli %>%
    filter(admit_ward == v1) %>%
    group_by(wau_received) %>%
    summarise(num = n(),
              per = num/v2) # Change the number
}

Wau_fun_deli (v1 = "23 HR", v2 = 19) #
Wau_fun_deli (v1 = "4C", v2 = 3) #
Wau_fun_deli (v1 = "4E", v2 = 47)
Wau_fun_deli (v1 = "5D", v2 = 7)
Wau_fun_deli (v1 = "WATT", v2 = 1)




##############################################################
############################## proms  ######################## ------ Similar to workup & delivery
##############################################################

sirt_proms = sirt_data %>%
  select(1:4, proms_time : proms_vas) %>%
  filter(!is.na(proms_time))


write.csv(sirt_proms, "sirt_proms.csv")

sirt_proms = read.delim("sirt_proms_1.csv", sep = ",") %>% clean_names() # added the utlity score

sirt_proms$proms_vas = as.double(sirt_proms$proms_vas)


sirt_proms %>%
  filter(proms_time == 1 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas),
            num = n())

sirt_proms %>%
  filter(proms_time == 5 & !is.na(proms_vas)) %>%
  summarise(mean_utility = mean(utility),
            sd_utility = sd(utility),
            mean_vas = mean(proms_vas),
            sd_vas = sd(proms_vas))




############################################################
########### Selecting oly the work-up data
sirt_data = sirt_data %>%
  filter( sirt_workup == 1)



##############################################################
############################## sirt procedure  ###############
##############################################################

sirt_proced = sirt_data %>%
  select(1:4, sirt_workup : sirt_post_bed) %>%
  filter(redcap_repeat_instrument == "sirt" & sirt_workup == 1) %>%
  mutate(sirt_out_time_1 = sirt_out_time)


sirt_proced = separate(sirt_proced, col = sirt_out_time_1, into = c("hours", "minutes")) # Diff time produce negative values as post time is next day

sirt_proced$hours = as.numeric(sirt_proced$hours)
sirt_proced$minutes = as.numeric(sirt_proced$minutes)

sirt_proced$sirt_out_time_2 = make_datetime(year = 0000, month = 01, day = 02, hour = sirt_proced$hours, min = sirt_proced$minutes) # Created a separate column with the same time next day


sirt_proced$sirt_in_room = parse_date_time(sirt_proced$sirt_in_room, orders = c("hm")) # Procedure time
sirt_proced$sirt_out_time = parse_date_time(sirt_proced$sirt_out_time, orders = c("hm")) 

sirt_proced$sirt_start_reco = parse_date_time(sirt_proced$sirt_start_reco, orders = c("hm")) # Recovery time
sirt_proced$sirt_end_recov = parse_date_time(sirt_proced$sirt_end_recov, orders = c("hm"))



# time difference - Procedure
sirt_proced$timediff = difftime(sirt_proced$sirt_out_time, sirt_proced$sirt_in_room, units = "hours") # this has negative time points


sirt_proced = sirt_proced %>%
  mutate(sirt_out_time_3 = if_else (timediff > 0, sirt_proced$sirt_out_time, sirt_proced$sirt_out_time_2))

sirt_proced$timediff_procedure = difftime(sirt_proced$sirt_out_time_3, sirt_proced$sirt_in_room, units = "hours")



# time difference - recovery
sirt_proced$timediff_recovery = difftime(sirt_proced$sirt_end_recov, sirt_proced$sirt_start_reco, units = "hours") # this has negative time points


# Calculating average time

sirt_proced %>%
  filter(sirt_procedure == 1) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure),
            num = n())

sirt_proced %>%
  filter(sirt_procedure == 2) %>%
  summarise(mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure),
            num = n())

sirt_proced %>%
  filter(sirt_procedure == 3 & !(is.na(timediff_procedure))) %>%
  summarise(num = n (),
            mean = mean(timediff_procedure),
            median = median(timediff_procedure),
            sd = sd(timediff_procedure),
            num = n())


sirt_proced %>% # Recovery time
  filter(sirt_procedure == 2) %>%
  summarise(mean = mean(timediff_recovery),
            median = median(timediff_recovery),
            sd = sd(timediff_recovery))


# Recovery place
sirt_proced %>%
  filter(sirt_procedure == 2) %>%
  group_by(sirt_recovery_place) %>%
  summarise(num = n(),
            ave = num/9)





###########A##################################################
############################## sirt imaging  ###############
##############################################################

sirt_imaging = sirt_data %>%
  select(1:4, sirt_workup, sirt_procedure, sirt_imaging_text : sirt_other_2) %>%
  filter(redcap_repeat_instrument == "sirt" & sirt_workup == 1)


sirt_imagindg_cost = read.delim("Imaging_cost_sirt.csv", sep = ",") %>% clean_names() # Imaging cost


sirt_imaging_1 = sirt_imaging %>%
  select(1, 5, 6, 8:17)


sirt_imaging_2 = gather(sirt_imaging_1, key = imaging_method, value = number, -1, -2, -3)

sirt_imaging_2$number = as.double(sirt_imaging_2$number)

sirt_imaging_2 = sirt_imaging_2 %>%
  mutate(number_1 = if_else (is.na(number), 0, sirt_imaging_2$number))


sirt_imaging_2 = sirt_imaging_2 %>%
  mutate(num_used = if_else(number_1 > 0, 1, 0))


# Imaging numbers
sirt_imaging_2 %>%
  filter(sirt_procedure == 1) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/9)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))

sirt_imaging_2 %>%
  filter(sirt_procedure == 2) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/9)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))

sirt_imaging_2 %>%
  filter(sirt_procedure == 3) %>%
  group_by(imaging_method) %>%
  summarise(num_used = sum(num_used),
            ave_used = (num_used/9)*100,
            tot_used = sum(number_1),
            per_perons = (tot_used/num_used))



# Merge with cost varisirte
sirt_imaging_3 = merge(x = sirt_imaging_2, y = sirt_imagindg_cost, by = "imaging_method", all = TRUE)



sirt_imaging_3 = sirt_imaging_3 %>%
  mutate(total_cost = number_1 * cost)

sirt_imaging_3 %>%
  filter(sirt_procedure == 1 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in sirt_procedure == 1 --> 9
sirt_imaging_3 %>%
  filter(sirt_procedure == 1) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 9)


sirt_imaging_3 %>%
  filter(sirt_procedure == 2 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in sirt_procedure == 1 --> 9
sirt_imaging_3 %>%
  filter(sirt_procedure == 2) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 9)


sirt_imaging_3 %>%
  filter(sirt_procedure == 3 & num_used > 0) %>%
  group_by(record_id) %>%
  summarise(num = n()) # Number used imaging in sirt_procedure == 1 --> 16
sirt_imaging_3 %>%
  filter(sirt_procedure == 3) %>% 
  summarise(total_cost = sum(total_cost),
            average = total_cost / 1)



#### Boot strapping

## Total cost
sirt_imaging_4 = aggregate(sirt_imaging_3$total_cost, by = list(sirt_imaging_3$record_id), FUN = sum)


colnames(sirt_imaging_4) = c("record_id", "sirt_imaging_cost")


sirt_imaging_4 %>%
  summarise(sum = sum(sirt_imaging_cost))


## -- Boot strapping -- ##
set.seed(13579)
n = 9
B = 100000

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.tot.imaging_cost.1 <- matrix(sample(sirt_imaging_4$sirt_imaging_cost, size= B*n, 
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
sirt_imaging_pre = sirt_imaging_3 %>%
  filter(sirt_procedure == 1) 

sirt_imaging_pre = aggregate(sirt_imaging_pre$total_cost, by = list(sirt_imaging_pre$record_id), FUN = sum)

colnames(sirt_imaging_pre) = c("record_id", "cost_pre_imaging")


sirt_imaging_pre %>%
  summarise(sum = sum(cost_pre_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.pre.imaging_cost.1 <- matrix(sample(sirt_imaging_pre$cost_pre_imaging, size= B*n, 
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
sirt_imaging_intra = sirt_imaging_3 %>%
  filter(sirt_procedure == 2) 

sirt_imaging_intra = aggregate(sirt_imaging_intra$total_cost, by = list(sirt_imaging_intra$record_id), FUN = sum)

colnames(sirt_imaging_intra) = c("record_id", "cost_intra_imaging")

sirt_imaging_intra %>%
  summarise(sum = sum(cost_intra_imaging))


## -- Boot strapping -- ##

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.intra.imagin_cost.1 <- matrix(sample(sirt_imaging_intra$cost_intra_imaging, size= B*n, 
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
sirt_imaging_post = sirt_imaging_3 %>%
  filter(sirt_procedure == 3) 

sirt_imaging_post = aggregate(sirt_imaging_post$total_cost, by = list(sirt_imaging_post$record_id), FUN = sum)

colnames(sirt_imaging_post) = c("record_id", "cost_post_imaging")


sirt_imaging_post %>%
  summarise(sum = sum(cost_post_imaging))


## -- Boot strapping -- ##


# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.post.imaging_cost.1 <- matrix(sample(sirt_imaging_post$cost_post_imaging, size= B*n, 
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
################## Consumsirtes and medicine  #################
##############################################################

sirt_consumable_cost = read.delim("sirt_consumable_cost.csv", sep = ",") %>% clean_names()


########### Pre- procedure #################

sirt_pre_ConMed = sirt_data %>%
  filter(sirt_procedure == 1 & sirt_workup == 1) %>%
  select(1, 3, sirt_pre_work_3m : sirt_pre_work_tegaderm)


sirt_pre_ConMed_1 = gather(sirt_pre_ConMed, key = item_name, value = number, -1, -2)

sirt_pre_ConMed_1$number = as.double(sirt_pre_ConMed_1$number)

sirt_pre_ConMed_2 = sirt_pre_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, sirt_pre_ConMed_1$number))

sirt_pre_ConMed_2 = sirt_pre_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

sirt_pre_ConMed_2$item_name = as.factor(sirt_pre_ConMed_2$item_name)



# Merge with cost varisirte
sirt_pre_ConMed_2 = merge(x = sirt_pre_ConMed_2, y = sirt_consumable_cost, by = "item_name", all = TRUE)


sirt_pre_ConMed_2 = sirt_pre_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)



# Aggregate cost per person
sirt_pre_ConMed_3 = aggregate(sirt_pre_ConMed_2$tot_cost, by = list(sirt_pre_ConMed_2$record_id), FUN = sum)

colnames(sirt_pre_ConMed_3) = c("record_id", "cost_pre_conMed")

sirt_pre_ConMed_3 %>%
  summarise(sum = sum(cost_pre_conMed))


## -- Boot strapping -- ##
set.seed(13579)
n = 9
B = 100000

Boot.pre.ConMed_cost.1 <- matrix(sample(sirt_pre_ConMed_3$cost_pre_conMed, size= B*n, 
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

sirt_consumable_cost = read.delim("sirt_consumable_cost.csv", sep = ",") %>% clean_names()


sirt_intra_ConMed = sirt_data %>%
  filter(sirt_procedure == 2) %>%
  select(1, 3, sirt_intra_work_absorb_60 : sirt_intra_work_vect, sirt_intra_work_fentanyl_100 : ondansetron)


sirt_intra_ConMed_1 = gather(sirt_intra_ConMed, key = item_name, value = number, -1, -2)

sirt_intra_ConMed_1$number = as.double(sirt_intra_ConMed_1$number)

sirt_intra_ConMed_2 = sirt_intra_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, sirt_intra_ConMed_1$number))

sirt_intra_ConMed_2 = sirt_intra_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

sirt_intra_ConMed_2$item_name = as.factor(sirt_intra_ConMed_2$item_name)



# Merge with cost varisirte
sirt_intra_ConMed_2 = merge(x = sirt_intra_ConMed_2, y = sirt_consumable_cost, by = "item_name", all = TRUE)

sirt_intra_ConMed_2 = sirt_intra_ConMed_2 %>%  # Remove pre
  filter(!is.na(record_id))

sirt_intra_ConMed_2 = sirt_intra_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)



# Aggregate cost per item
sirt_intra_ConMed_4 = aggregate(sirt_intra_ConMed_2$tot_cost, by = list(sirt_intra_ConMed_2$item_name), FUN = sum)
colnames(sirt_intra_ConMed_4) = c("item_name", "cost_intra_conMed")

sirt_intra_ConMed_4 <- sirt_intra_ConMed_4[order(-sirt_intra_ConMed_4$cost_intra_conMed),]

sirt_intra_ConMed_4 = sirt_intra_ConMed_4 %>%
  mutate(per_pt = cost_intra_conMed / 9) 

sirt_intra_ConMed_4 %>%
  summarise(sum = sum(per_pt))




# Aggregate cost per person
sirt_intra_ConMed_3 = aggregate(sirt_intra_ConMed_2$tot_cost, by = list(sirt_intra_ConMed_2$record_id), FUN = sum)

colnames(sirt_intra_ConMed_3) = c("record_id", "cost_intra_conMed")

sirt_intra_ConMed_3 %>%
  summarise(sum = sum(cost_intra_conMed))


## -- Boot strapping -- ##
set.seed(13579)
n = 9
B = 100000

Boot.intra.ConMed_cost.1 <- matrix(sample(sirt_intra_ConMed_3$cost_intra_conMed, size= B*n, 
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
sirt_consumable_cost = read.delim("sirt_consumable_cost.csv", sep = ",") %>% clean_names()

sirt_post_ConMed = sirt_data %>%
  filter(sirt_procedure == 3) %>%
  select(1, 3, sirt_post_iv_dot : sirt_post_freestyle)


sirt_post_ConMed_1 = gather(sirt_post_ConMed, key = item_name, value = number, -1, -2)

sirt_post_ConMed_1$number = as.double(sirt_post_ConMed_1$number)

sirt_post_ConMed_2 = sirt_post_ConMed_1 %>%
  mutate(number_ConMed = if_else (is.na(number), 0, sirt_post_ConMed_1$number))

sirt_post_ConMed_2 = sirt_post_ConMed_2 %>%
  mutate(num_used = if_else(number_ConMed > 0, 1, 0))

sirt_post_ConMed_2$item_name = as.factor(sirt_post_ConMed_2$item_name)



# Merge with cost varisirte
sirt_post_ConMed_2 = merge(x = sirt_post_ConMed_2, y = sirt_consumable_cost, by = "item_name", all = TRUE)

sirt_post_ConMed_2 = sirt_post_ConMed_2 %>%  # Remove pre & intra
  filter(!is.na(record_id))

sirt_post_ConMed_2 = sirt_post_ConMed_2 %>%
  mutate(tot_cost = number_ConMed * cost)



# Aggregate cost per person
sirt_post_ConMed_3 = aggregate(sirt_post_ConMed_2$tot_cost, by = list(sirt_post_ConMed_2$record_id), FUN = sum)

colnames(sirt_post_ConMed_3) = c("record_id", "cost_post_conMed")

sirt_post_ConMed_3 %>%
  summarise(sum = sum(cost_post_conMed))


## -- Boot strapping -- ## --->
set.seed(13579)
n = 8
B = 100000

Boot.post.ConMed_cost.1 <- matrix(sample(sirt_post_ConMed_3$cost_post_conMed, size= B*n, 
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
as.data.frame(post.ConMed_cost_tab)




## Combine pre, intra & post data sets
sirt_combine_ConMed = merge(x = sirt_pre_ConMed_3, y = sirt_intra_ConMed_3, by = "record_id", all = TRUE)
sirt_combine_ConMed = merge(x = sirt_combine_ConMed, y = sirt_post_ConMed_3, by = "record_id", all = TRUE)

sirt_combine_ConMed[is.na(sirt_combine_ConMed)]<-0

sirt_combine_ConMed = sirt_combine_ConMed %>%
  mutate(total_ConMed = cost_pre_conMed + cost_intra_conMed +cost_post_conMed)


sirt_combine_ConMed %>%
  summarise(sum = sum(total_ConMed))


sirt_combine_ConMed = sirt_combine_ConMed %>%
  select(1, 5)



## -- Boot strapping -- ##
set.seed(13579)
n = 9
B = 100000

Boot.combine.ConMed_cost.1 <- matrix(sample(sirt_combine_ConMed$total_ConMed, size= B*n, 
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
as.data.frame(combine.ConMed_cost_tab)




##############################################################
################## Staff cost   ##############################
##############################################################


sirt_staff = sirt_data %>%
  select(1, 2, 3,  sirt_staff_al: sirt_staff_mab_nm_10_hours) %>%
  filter(redcap_repeat_instrument == "sirt")


sirt_pre_staff = sirt_staff %>%
  filter(redcap_repeat_instance == 1)
sirt_intra_staff = sirt_staff %>%
  filter(redcap_repeat_instance == 2)  
sirt_post_staff = sirt_staff %>%
  filter(redcap_repeat_instance == 3)

sirt_pre_staff$sirt_staff_al_01_other = as.factor(sirt_pre_staff$sirt_staff_al_01_other)




# To check if number are more than 10 in a staff cat
sirt_pre_staff_check = sirt_pre_staff %>%
  select(1, sirt_staff_al, sirt_staff_ass_n, sirt_staff_enr_n, sirt_staff_reg_n, sirt_staff_cln_n, sirt_staff_anum, sirt_staff_case_m, 
         sirt_staff_nur_um, sirt_staff_in_res, sirt_staff_registrar, sirt_staff_fellow, sirt_staff_consultant, sirt_staff_radio,
         sirt_staff_ultra, sirt_staff_nuc_mt, sirt_staff_nm_ph, sirt_staff_mab_nm)

sirt_intra_staff_check = sirt_intra_staff %>%
  select(1, sirt_staff_al, sirt_staff_ass_n, sirt_staff_enr_n, sirt_staff_reg_n, sirt_staff_cln_n, sirt_staff_anum, sirt_staff_case_m, 
         sirt_staff_nur_um, sirt_staff_in_res, sirt_staff_registrar, sirt_staff_fellow, sirt_staff_consultant, sirt_staff_radio,
         sirt_staff_ultra, sirt_staff_nuc_mt, sirt_staff_nm_ph, sirt_staff_mab_nm)

sirt_post_staff_check = sirt_post_staff %>%
  select(1, sirt_staff_al, sirt_staff_ass_n, sirt_staff_enr_n, sirt_staff_reg_n, sirt_staff_cln_n, sirt_staff_anum, sirt_staff_case_m, 
         sirt_staff_nur_um, sirt_staff_in_res, sirt_staff_registrar, sirt_staff_fellow, sirt_staff_consultant, sirt_staff_radio,
         sirt_staff_ultra, sirt_staff_nuc_mt, sirt_staff_nm_ph, sirt_staff_mab_nm)



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


sirt_pre_staff[is.na(sirt_pre_staff)]<-0
sirt_intra_staff[is.na(sirt_intra_staff)]<-0
sirt_post_staff[is.na(sirt_post_staff)]<-0


# Administrative_labour
sirt_pre_staff  %>%
  filter(!(is.na(sirt_staff_al))) %>%
  summarise(sum = sum(sirt_staff_al))
sirt_intra_staff  %>%
  filter(!(is.na(sirt_staff_al))) %>%
  summarise(sum = sum(sirt_staff_al))
sirt_post_staff  %>%
  filter(!(is.na(sirt_staff_al))) %>%
  summarise(sum = sum(sirt_staff_al))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Admin_labour_cost = (sirt_staff_al_01_hours + sirt_staff_al_02_hours + sirt_staff_al_03_hours + sirt_staff_al_04_hours + sirt_staff_al_05_hours +
           sirt_staff_al_06_hours + sirt_staff_al_07_hours + sirt_staff_al_08_hours + sirt_staff_al_09_hours + sirt_staff_al_10_hours)*Administrative_labour)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Admin_labour_cost = (sirt_staff_al_01_hours + sirt_staff_al_02_hours + sirt_staff_al_03_hours + sirt_staff_al_04_hours + sirt_staff_al_05_hours +
                                 sirt_staff_al_06_hours + sirt_staff_al_07_hours + sirt_staff_al_08_hours + sirt_staff_al_09_hours + sirt_staff_al_10_hours)*Administrative_labour)

sirt_post_staff = sirt_post_staff %>%
  mutate(Admin_labour_cost = (sirt_staff_al_01_hours + sirt_staff_al_02_hours + sirt_staff_al_03_hours + sirt_staff_al_04_hours + sirt_staff_al_05_hours +
                                sirt_staff_al_06_hours + sirt_staff_al_07_hours + sirt_staff_al_08_hours + sirt_staff_al_09_hours + sirt_staff_al_10_hours)*Administrative_labour)


sirt_pre_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Admin_labour_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Admin_labour_cost))


# Assistant_in_nursing
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_ass_n))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_ass_n))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_ass_n))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Assistant_in_nursing_cost = (sirt_staff_ass_n_01_hours + sirt_staff_ass_n_02_hours + sirt_staff_ass_n_03_hours + sirt_staff_ass_n_04_hours + sirt_staff_ass_n_05_hours +
                                sirt_staff_ass_n_06_hours + sirt_staff_ass_n_07_hours + sirt_staff_ass_n_08_hours + sirt_staff_ass_n_09_hours + sirt_staff_ass_n_10_hours)*Assistant_in_nursing)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Assistant_in_nursing_cost = (sirt_staff_ass_n_01_hours + sirt_staff_ass_n_02_hours + sirt_staff_ass_n_03_hours + sirt_staff_ass_n_04_hours + sirt_staff_ass_n_05_hours +
                                        sirt_staff_ass_n_06_hours + sirt_staff_ass_n_07_hours + sirt_staff_ass_n_08_hours + sirt_staff_ass_n_09_hours + sirt_staff_ass_n_10_hours)*Assistant_in_nursing)

sirt_post_staff = sirt_post_staff %>%
  mutate(Assistant_in_nursing_cost = (sirt_staff_ass_n_01_hours + sirt_staff_ass_n_02_hours + sirt_staff_ass_n_03_hours + sirt_staff_ass_n_04_hours + sirt_staff_ass_n_05_hours +
                                        sirt_staff_ass_n_06_hours + sirt_staff_ass_n_07_hours + sirt_staff_ass_n_08_hours + sirt_staff_ass_n_09_hours + sirt_staff_ass_n_10_hours)*Assistant_in_nursing)

sirt_pre_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Assistant_in_nursing_cost))



# Enrolled_nurse
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_enr_n))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_enr_n))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_enr_n))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Enrolled_nurse_cost = (sirt_staff_enr_n_01_hours + sirt_staff_enr_n_02_hours + sirt_staff_enr_n_03_hours + sirt_staff_enr_n_04_hours + sirt_staff_enr_n_05_hours +
                                        sirt_staff_enr_n_06_hours + sirt_staff_enr_n_07_hours + sirt_staff_enr_n_08_hours + sirt_staff_enr_n_09_hours + sirt_staff_enr_n_10_hours)*Enrolled_nurse)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Enrolled_nurse_cost = (sirt_staff_enr_n_01_hours + sirt_staff_enr_n_02_hours + sirt_staff_enr_n_03_hours + sirt_staff_enr_n_04_hours + sirt_staff_enr_n_05_hours +
                                        sirt_staff_enr_n_06_hours + sirt_staff_enr_n_07_hours + sirt_staff_enr_n_08_hours + sirt_staff_enr_n_09_hours + sirt_staff_enr_n_10_hours)*Enrolled_nurse)

sirt_post_staff = sirt_post_staff %>%
  mutate(Enrolled_nurse_cost = (sirt_staff_enr_n_01_hours + sirt_staff_enr_n_02_hours + sirt_staff_enr_n_03_hours + sirt_staff_enr_n_04_hours + sirt_staff_enr_n_05_hours +
                                        sirt_staff_enr_n_06_hours + sirt_staff_enr_n_07_hours + sirt_staff_enr_n_08_hours + sirt_staff_enr_n_09_hours + sirt_staff_enr_n_10_hours)*Enrolled_nurse)

sirt_pre_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Enrolled_nurse_cost))


# Registered_nurse
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_reg_n))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_reg_n))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_reg_n))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Registered_nurse_cost = (sirt_staff_reg_n_01_hours + sirt_staff_reg_n_02_hours + sirt_staff_reg_n_03_hours + sirt_staff_reg_n_04_hours + sirt_staff_reg_n_05_hours +
                                  sirt_staff_reg_n_06_hours + sirt_staff_reg_n_07_hours + sirt_staff_reg_n_08_hours + sirt_staff_reg_n_09_hours + sirt_staff_reg_n_10_hours)*Registered_nurse)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Registered_nurse_cost = (sirt_staff_reg_n_01_hours + sirt_staff_reg_n_02_hours + sirt_staff_reg_n_03_hours + sirt_staff_reg_n_04_hours + sirt_staff_reg_n_05_hours +
                                  sirt_staff_reg_n_06_hours + sirt_staff_reg_n_07_hours + sirt_staff_reg_n_08_hours + sirt_staff_reg_n_09_hours + sirt_staff_reg_n_10_hours)*Registered_nurse)

sirt_post_staff = sirt_post_staff %>%
  mutate(Registered_nurse_cost = (sirt_staff_reg_n_01_hours + sirt_staff_reg_n_02_hours + sirt_staff_reg_n_03_hours + sirt_staff_reg_n_04_hours + sirt_staff_reg_n_05_hours +
                                  sirt_staff_reg_n_06_hours + sirt_staff_reg_n_07_hours + sirt_staff_reg_n_08_hours + sirt_staff_reg_n_09_hours + sirt_staff_reg_n_10_hours)*Registered_nurse)



sirt_pre_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Registered_nurse_cost))



# Clinical_nurse
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_cln_n))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_cln_n))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_cln_n))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Clinical_nurse_cost = (sirt_staff_cln_n_01_hours + sirt_staff_cln_n_02_hours + sirt_staff_cln_n_03_hours + sirt_staff_cln_n_04_hours + sirt_staff_cln_n_05_hours +
                                    sirt_staff_cln_n_06_hours + sirt_staff_cln_n_07_hours + sirt_staff_cln_n_08_hours + sirt_staff_cln_n_09_hours + sirt_staff_cln_n_10_hours)*Clinical_nurse)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Clinical_nurse_cost = (sirt_staff_cln_n_01_hours + sirt_staff_cln_n_02_hours + sirt_staff_cln_n_03_hours + sirt_staff_cln_n_04_hours + sirt_staff_cln_n_05_hours +
                                    sirt_staff_cln_n_06_hours + sirt_staff_cln_n_07_hours + sirt_staff_cln_n_08_hours + sirt_staff_cln_n_09_hours + sirt_staff_cln_n_10_hours)*Clinical_nurse)

sirt_post_staff = sirt_post_staff %>%
  mutate(Clinical_nurse_cost = (sirt_staff_cln_n_01_hours + sirt_staff_cln_n_02_hours + sirt_staff_cln_n_03_hours + sirt_staff_cln_n_04_hours + sirt_staff_cln_n_05_hours +
                                    sirt_staff_cln_n_06_hours + sirt_staff_cln_n_07_hours + sirt_staff_cln_n_08_hours + sirt_staff_cln_n_09_hours + sirt_staff_cln_n_10_hours)*Clinical_nurse)



sirt_pre_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Clinical_nurse_cost))



# Assistant_nurse_unit_manager
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_anum))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_anum))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_anum))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (sirt_staff_anum_01_hours + sirt_staff_anum_02_hours + sirt_staff_anum_03_hours + sirt_staff_anum_04_hours + sirt_staff_anum_05_hours +
                                  sirt_staff_anum_06_hours + sirt_staff_anum_07_hours + sirt_staff_anum_08_hours + sirt_staff_anum_09_hours + sirt_staff_anum_10_hours)*Assistant_nurse_unit_manager)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (sirt_staff_anum_01_hours + sirt_staff_anum_02_hours + sirt_staff_anum_03_hours + sirt_staff_anum_04_hours + sirt_staff_anum_05_hours +
                                                sirt_staff_anum_06_hours + sirt_staff_anum_07_hours + sirt_staff_anum_08_hours + sirt_staff_anum_09_hours + sirt_staff_anum_10_hours)*Assistant_nurse_unit_manager)

sirt_post_staff = sirt_post_staff %>%
  mutate(Assistant_nurse_unit_manager_cost = (sirt_staff_anum_01_hours + sirt_staff_anum_02_hours + sirt_staff_anum_03_hours + sirt_staff_anum_04_hours + sirt_staff_anum_05_hours +
                                                sirt_staff_anum_06_hours + sirt_staff_anum_07_hours + sirt_staff_anum_08_hours + sirt_staff_anum_09_hours + sirt_staff_anum_10_hours)*Assistant_nurse_unit_manager)

sirt_pre_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Assistant_nurse_unit_manager_cost))



# Case_manager
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_case_m))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_case_m))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_case_m))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Case_manager_cost = (sirt_staff_case_m_01_hours + sirt_staff_case_m_02_hours + sirt_staff_case_m_03_hours + sirt_staff_case_m_04_hours + sirt_staff_case_m_05_hours +
                                                sirt_staff_case_m_06_hours + sirt_staff_case_m_07_hours + sirt_staff_case_m_08_hours + sirt_staff_case_m_09_hours + sirt_staff_case_m_10_hours)*Case_manager)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Case_manager_cost = (sirt_staff_case_m_01_hours + sirt_staff_case_m_02_hours + sirt_staff_case_m_03_hours + sirt_staff_case_m_04_hours + sirt_staff_case_m_05_hours +
                                                sirt_staff_case_m_06_hours + sirt_staff_case_m_07_hours + sirt_staff_case_m_08_hours + sirt_staff_case_m_09_hours + sirt_staff_case_m_10_hours)*Case_manager)

sirt_post_staff = sirt_post_staff %>%
  mutate(Case_manager_cost = (sirt_staff_case_m_01_hours + sirt_staff_case_m_02_hours + sirt_staff_case_m_03_hours + sirt_staff_case_m_04_hours + sirt_staff_case_m_05_hours +
                                                sirt_staff_case_m_06_hours + sirt_staff_case_m_07_hours + sirt_staff_case_m_08_hours + sirt_staff_case_m_09_hours + sirt_staff_case_m_10_hours)*Case_manager)

sirt_pre_staff  %>%
  summarise(sum = sum(Case_manager_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Case_manager_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Case_manager_cost))


# Nurse_unit_manager
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_nur_um))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_nur_um))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_nur_um))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Nurse_unit_manager_cost = (sirt_staff_nur_um_01_hours + sirt_staff_nur_um_02_hours + sirt_staff_nur_um_03_hours + sirt_staff_nur_um_04_hours + sirt_staff_nur_um_05_hours +
                                sirt_staff_nur_um_06_hours + sirt_staff_nur_um_07_hours + sirt_staff_nur_um_08_hours + sirt_staff_nur_um_09_hours + sirt_staff_nur_um_10_hours)*Nurse_unit_manager)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Nurse_unit_manager_cost = (sirt_staff_nur_um_01_hours + sirt_staff_nur_um_02_hours + sirt_staff_nur_um_03_hours + sirt_staff_nur_um_04_hours + sirt_staff_nur_um_05_hours +
                                sirt_staff_nur_um_06_hours + sirt_staff_nur_um_07_hours + sirt_staff_nur_um_08_hours + sirt_staff_nur_um_09_hours + sirt_staff_nur_um_10_hours)*Nurse_unit_manager)

sirt_post_staff = sirt_post_staff %>%
  mutate(Nurse_unit_manager_cost = (sirt_staff_nur_um_01_hours + sirt_staff_nur_um_02_hours + sirt_staff_nur_um_03_hours + sirt_staff_nur_um_04_hours + sirt_staff_nur_um_05_hours +
                                sirt_staff_nur_um_06_hours + sirt_staff_nur_um_07_hours + sirt_staff_nur_um_08_hours + sirt_staff_nur_um_09_hours + sirt_staff_nur_um_10_hours)*Nurse_unit_manager)

sirt_pre_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Nurse_unit_manager_cost))


# Intern_or_Resident
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_in_res))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_in_res))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_in_res))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Intern_or_Resident_cost = (sirt_staff_in_res_01_hours + sirt_staff_in_res_02_hours + sirt_staff_in_res_03_hours + sirt_staff_in_res_04_hours + sirt_staff_in_res_05_hours +
                                      sirt_staff_in_res_06_hours + sirt_staff_in_res_07_hours + sirt_staff_in_res_08_hours + sirt_staff_in_res_09_hours + sirt_staff_in_res_10_hours)*Intern_or_Resident)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Intern_or_Resident_cost = (sirt_staff_in_res_01_hours + sirt_staff_in_res_02_hours + sirt_staff_in_res_03_hours + sirt_staff_in_res_04_hours + sirt_staff_in_res_05_hours +
                                      sirt_staff_in_res_06_hours + sirt_staff_in_res_07_hours + sirt_staff_in_res_08_hours + sirt_staff_in_res_09_hours + sirt_staff_in_res_10_hours)*Intern_or_Resident)

sirt_post_staff = sirt_post_staff %>%
  mutate(Intern_or_Resident_cost = (sirt_staff_in_res_01_hours + sirt_staff_in_res_02_hours + sirt_staff_in_res_03_hours + sirt_staff_in_res_04_hours + sirt_staff_in_res_05_hours +
                                      sirt_staff_in_res_06_hours + sirt_staff_in_res_07_hours + sirt_staff_in_res_08_hours + sirt_staff_in_res_09_hours + sirt_staff_in_res_10_hours)*Intern_or_Resident)

sirt_pre_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Intern_or_Resident_cost))


# Registrar
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_registrar))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_registrar))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_registrar))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Registrar_cost = (sirt_staff_registrar_01_hours + sirt_staff_registrar_02_hours + sirt_staff_registrar_03_hours + sirt_staff_registrar_04_hours + sirt_staff_registrar_05_hours +
                                      sirt_staff_registrar_06_hours + sirt_staff_registrar_07_hours + sirt_staff_registrar_08_hours + sirt_staff_registrar_09_hours + sirt_staff_registrar_10_hours)*Registrar)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Registrar_cost = (sirt_staff_registrar_01_hours + sirt_staff_registrar_02_hours + sirt_staff_registrar_03_hours + sirt_staff_registrar_04_hours + sirt_staff_registrar_05_hours +
                                      sirt_staff_registrar_06_hours + sirt_staff_registrar_07_hours + sirt_staff_registrar_08_hours + sirt_staff_registrar_09_hours + sirt_staff_registrar_10_hours)*Registrar)

sirt_post_staff = sirt_post_staff %>%
  mutate(Registrar_cost = (sirt_staff_registrar_01_hours + sirt_staff_registrar_02_hours + sirt_staff_registrar_03_hours + sirt_staff_registrar_04_hours + sirt_staff_registrar_05_hours +
                                      sirt_staff_registrar_06_hours + sirt_staff_registrar_07_hours + sirt_staff_registrar_08_hours + sirt_staff_registrar_09_hours + sirt_staff_registrar_10_hours)*Registrar)

sirt_pre_staff  %>%
  summarise(sum = sum(Registrar_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Registrar_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Registrar_cost))



# Fellow
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_fellow))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_fellow))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_fellow))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Fellow_cost = (sirt_staff_fellow_01_hours + sirt_staff_fellow_02_hours + sirt_staff_fellow_03_hours + sirt_staff_fellow_04_hours + sirt_staff_fellow_05_hours +
                             sirt_staff_fellow_06_hours + sirt_staff_fellow_07_hours + sirt_staff_fellow_08_hours + sirt_staff_fellow_09_hours + sirt_staff_fellow_10_hours)*Fellow)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Fellow_cost = (sirt_staff_fellow_01_hours + sirt_staff_fellow_02_hours + sirt_staff_fellow_03_hours + sirt_staff_fellow_04_hours + sirt_staff_fellow_05_hours +
                             sirt_staff_fellow_06_hours + sirt_staff_fellow_07_hours + sirt_staff_fellow_08_hours + sirt_staff_fellow_09_hours + sirt_staff_fellow_10_hours)*Fellow)

sirt_post_staff = sirt_post_staff %>%
  mutate(Fellow_cost = (sirt_staff_fellow_01_hours + sirt_staff_fellow_02_hours + sirt_staff_fellow_03_hours + sirt_staff_fellow_04_hours + sirt_staff_fellow_05_hours +
                             sirt_staff_fellow_06_hours + sirt_staff_fellow_07_hours + sirt_staff_fellow_08_hours + sirt_staff_fellow_09_hours + sirt_staff_fellow_10_hours)*Fellow)

sirt_pre_staff  %>%
  summarise(sum = sum(Fellow_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Fellow_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Fellow_cost))


# Consultant
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_consultant))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_consultant))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_consultant))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Consultant_cost = (sirt_staff_consultant_01_hours + sirt_staff_consultant_02_hours + sirt_staff_consultant_03_hours + sirt_staff_consultant_04_hours + sirt_staff_consultant_05_hours +
                          sirt_staff_consultant_06_hours + sirt_staff_consultant_07_hours + sirt_staff_consultant_08_hours + sirt_staff_consultant_09_hours + sirt_staff_consultant_10_hours)*Consultant)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Consultant_cost = (sirt_staff_consultant_01_hours + sirt_staff_consultant_02_hours + sirt_staff_consultant_03_hours + sirt_staff_consultant_04_hours + sirt_staff_consultant_05_hours +
                          sirt_staff_consultant_06_hours + sirt_staff_consultant_07_hours + sirt_staff_consultant_08_hours + sirt_staff_consultant_09_hours + sirt_staff_consultant_10_hours)*Consultant)

sirt_post_staff = sirt_post_staff %>%
  mutate(Consultant_cost = (sirt_staff_consultant_01_hours + sirt_staff_consultant_02_hours + sirt_staff_consultant_03_hours + sirt_staff_consultant_04_hours + sirt_staff_consultant_05_hours +
                          sirt_staff_consultant_06_hours + sirt_staff_consultant_07_hours + sirt_staff_consultant_08_hours + sirt_staff_consultant_09_hours + sirt_staff_consultant_10_hours)*Consultant)

sirt_pre_staff  %>%
  summarise(sum = sum(Consultant_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Consultant_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Consultant_cost))



# Radiographer
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_radio))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_radio))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_radio))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Radiographer_cost = (sirt_staff_radio_01_hours + sirt_staff_radio_02_hours + sirt_staff_radio_03_hours + sirt_staff_radio_04_hours + sirt_staff_radio_05_hours +
                              sirt_staff_radio_06_hours + sirt_staff_radio_07_hours + sirt_staff_radio_08_hours + sirt_staff_radio_09_hours + sirt_staff_radio_10_hours)*Radiographer)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Radiographer_cost = (sirt_staff_radio_01_hours + sirt_staff_radio_02_hours + sirt_staff_radio_03_hours + sirt_staff_radio_04_hours + sirt_staff_radio_05_hours +
                              sirt_staff_radio_06_hours + sirt_staff_radio_07_hours + sirt_staff_radio_08_hours + sirt_staff_radio_09_hours + sirt_staff_radio_10_hours)*Radiographer)

sirt_post_staff = sirt_post_staff %>%
  mutate(Radiographer_cost = (sirt_staff_radio_01_hours + sirt_staff_radio_02_hours + sirt_staff_radio_03_hours + sirt_staff_radio_04_hours + sirt_staff_radio_05_hours +
                              sirt_staff_radio_06_hours + sirt_staff_radio_07_hours + sirt_staff_radio_08_hours + sirt_staff_radio_09_hours + sirt_staff_radio_10_hours)*Radiographer)

sirt_pre_staff  %>%
  summarise(sum = sum(Radiographer_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Radiographer_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Radiographer_cost))



# Ultrasonographer
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_ultra))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_ultra))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_ultra))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Ultrasonographer_cost = (sirt_staff_ultra_01_hours + sirt_staff_ultra_02_hours + sirt_staff_ultra_03_hours + sirt_staff_ultra_04_hours + sirt_staff_ultra_05_hours +
                                sirt_staff_ultra_06_hours + sirt_staff_ultra_07_hours + sirt_staff_ultra_08_hours + sirt_staff_ultra_09_hours + sirt_staff_ultra_10_hours)*Ultrasonographer)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Ultrasonographer_cost = (sirt_staff_ultra_01_hours + sirt_staff_ultra_02_hours + sirt_staff_ultra_03_hours + sirt_staff_ultra_04_hours + sirt_staff_ultra_05_hours +
                                sirt_staff_ultra_06_hours + sirt_staff_ultra_07_hours + sirt_staff_ultra_08_hours + sirt_staff_ultra_09_hours + sirt_staff_ultra_10_hours)*Ultrasonographer)

sirt_post_staff = sirt_post_staff %>%
  mutate(Ultrasonographer_cost = (sirt_staff_ultra_01_hours + sirt_staff_ultra_02_hours + sirt_staff_ultra_03_hours + sirt_staff_ultra_04_hours + sirt_staff_ultra_05_hours +
                                sirt_staff_ultra_06_hours + sirt_staff_ultra_07_hours + sirt_staff_ultra_08_hours + sirt_staff_ultra_09_hours + sirt_staff_ultra_10_hours)*Ultrasonographer)

sirt_pre_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Ultrasonographer_cost))



# Nuc_Med_Tech
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_nuc_mt))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_nuc_mt))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_nuc_mt))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Nuc_Med_Tech_cost = (sirt_staff_nuc_mt_01_hours + sirt_staff_nuc_mt_02_hours + sirt_staff_nuc_mt_03_hours + sirt_staff_nuc_mt_04_hours + sirt_staff_nuc_mt_05_hours +
                                    sirt_staff_nuc_mt_06_hours + sirt_staff_nuc_mt_07_hours + sirt_staff_nuc_mt_08_hours + sirt_staff_nuc_mt_09_hours + sirt_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Nuc_Med_Tech_cost = (sirt_staff_nuc_mt_01_hours + sirt_staff_nuc_mt_02_hours + sirt_staff_nuc_mt_03_hours + sirt_staff_nuc_mt_04_hours + sirt_staff_nuc_mt_05_hours +
                                    sirt_staff_nuc_mt_06_hours + sirt_staff_nuc_mt_07_hours + sirt_staff_nuc_mt_08_hours + sirt_staff_nuc_mt_09_hours + sirt_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

sirt_post_staff = sirt_post_staff %>%
  mutate(Nuc_Med_Tech_cost = (sirt_staff_nuc_mt_01_hours + sirt_staff_nuc_mt_02_hours + sirt_staff_nuc_mt_03_hours + sirt_staff_nuc_mt_04_hours + sirt_staff_nuc_mt_05_hours +
                                    sirt_staff_nuc_mt_06_hours + sirt_staff_nuc_mt_07_hours + sirt_staff_nuc_mt_08_hours + sirt_staff_nuc_mt_09_hours + sirt_staff_nuc_mt_10_hours)*Nuc_Med_Tech)

sirt_pre_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Nuc_Med_Tech_cost))


# NM_physicist
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_nm_ph))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_nm_ph))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_nm_ph))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(NM_physicist_cost = (sirt_staff_nm_ph_01_hours + sirt_staff_nm_ph_02_hours + sirt_staff_nm_ph_03_hours + sirt_staff_nm_ph_04_hours + sirt_staff_nm_ph_05_hours +
                                sirt_staff_nm_ph_06_hours + sirt_staff_nm_ph_07_hours + sirt_staff_nm_ph_08_hours + sirt_staff_nm_ph_09_hours + sirt_staff_nm_ph_10_hours)*NM_physicist)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(NM_physicist_cost = (sirt_staff_nm_ph_01_hours + sirt_staff_nm_ph_02_hours + sirt_staff_nm_ph_03_hours + sirt_staff_nm_ph_04_hours + sirt_staff_nm_ph_05_hours +
                                sirt_staff_nm_ph_06_hours + sirt_staff_nm_ph_07_hours + sirt_staff_nm_ph_08_hours + sirt_staff_nm_ph_09_hours + sirt_staff_nm_ph_10_hours)*NM_physicist)

sirt_post_staff = sirt_post_staff %>%
  mutate(NM_physicist_cost = (sirt_staff_nm_ph_01_hours + sirt_staff_nm_ph_02_hours + sirt_staff_nm_ph_03_hours + sirt_staff_nm_ph_04_hours + sirt_staff_nm_ph_05_hours +
                                sirt_staff_nm_ph_06_hours + sirt_staff_nm_ph_07_hours + sirt_staff_nm_ph_08_hours + sirt_staff_nm_ph_09_hours + sirt_staff_nm_ph_10_hours)*NM_physicist)

sirt_pre_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(NM_physicist_cost))
sirt_post_staff  %>%
  summarise(sum = sum(NM_physicist_cost))


# Labour_nuclear_medicine
sirt_pre_staff  %>%
  summarise(sum = sum(sirt_staff_mab_nm))
sirt_intra_staff  %>%
  summarise(sum = sum(sirt_staff_mab_nm))
sirt_post_staff  %>%
  summarise(sum = sum(sirt_staff_mab_nm))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(Labour_nuclear_medicine_cost = (sirt_staff_mab_nm_01_hours + sirt_staff_mab_nm_02_hours + sirt_staff_mab_nm_03_hours + sirt_staff_mab_nm_04_hours + sirt_staff_mab_nm_05_hours +
                                sirt_staff_mab_nm_06_hours + sirt_staff_mab_nm_07_hours + sirt_staff_mab_nm_08_hours + sirt_staff_mab_nm_09_hours + sirt_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

sirt_intra_staff = sirt_intra_staff %>%
  mutate(Labour_nuclear_medicine_cost = (sirt_staff_mab_nm_01_hours + sirt_staff_mab_nm_02_hours + sirt_staff_mab_nm_03_hours + sirt_staff_mab_nm_04_hours + sirt_staff_mab_nm_05_hours +
                                sirt_staff_mab_nm_06_hours + sirt_staff_mab_nm_07_hours + sirt_staff_mab_nm_08_hours + sirt_staff_mab_nm_09_hours + sirt_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

sirt_post_staff = sirt_post_staff %>%
  mutate(Labour_nuclear_medicine_cost = (sirt_staff_mab_nm_01_hours + sirt_staff_mab_nm_02_hours + sirt_staff_mab_nm_03_hours + sirt_staff_mab_nm_04_hours + sirt_staff_mab_nm_05_hours +
                                sirt_staff_mab_nm_06_hours + sirt_staff_mab_nm_07_hours + sirt_staff_mab_nm_08_hours + sirt_staff_mab_nm_09_hours + sirt_staff_mab_nm_10_hours)*Labour_nuclear_medicine)

sirt_pre_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
sirt_intra_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))
sirt_post_staff  %>%
  summarise(sum = sum(Labour_nuclear_medicine_cost))



## Final staff cost
sirt_pre_staff_final = sirt_pre_staff %>%
  select(1, 547: 563) %>%
  mutate(sirt_pre_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

sirt_pre_staff_final %>%
  summarise(sum = sum(sirt_pre_staff_final))

sirt_pre_staff_final_1 = sirt_pre_staff_final %>%
  select(1, sirt_pre_staff_final)



sirt_intra_staff_final = sirt_intra_staff %>%
  select(1, 547: 563) %>%
  mutate(sirt_intra_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

sirt_intra_staff_final %>%
  summarise(sum = sum(sirt_intra_staff_final))

sirt_intra_staff_final_1 = sirt_intra_staff_final %>%
  select(1, sirt_intra_staff_final)



sirt_post_staff_final = sirt_post_staff %>%
  select(1, 547: 563) %>%
  mutate(sirt_post_staff_final = Admin_labour_cost + Assistant_in_nursing_cost + Enrolled_nurse_cost + Registered_nurse_cost + Clinical_nurse_cost + Assistant_nurse_unit_manager_cost + Case_manager_cost + 
           Nurse_unit_manager_cost + Intern_or_Resident_cost + Registrar_cost + Fellow_cost + Consultant_cost + Radiographer_cost + Ultrasonographer_cost + Nuc_Med_Tech_cost + NM_physicist_cost + 
           Labour_nuclear_medicine_cost)

sirt_post_staff_final %>%
  summarise(sum = sum(sirt_post_staff_final))

sirt_post_staff_final_1 = sirt_post_staff_final %>%
  select(1, sirt_post_staff_final)



## -- Boot strapping -- ## sirt_pre_staff_final
set.seed(13579)
n = 9
B = 100000

Boot.pre_staff_cost.1 <- matrix(sample(sirt_pre_staff_final$sirt_pre_staff_final, size= B*n, 
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
as.data.frame(pre_staff_cost)


## -- Boot strapping -- ## sirt_intra_staff_final
set.seed(13579)
n = 9
B = 100000

Boot.intra_staff_cost.1 <- matrix(sample(sirt_intra_staff_final$sirt_intra_staff_final, size= B*n, 
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
as.data.frame(intra_staff_cost)


## -- Boot strapping -- ## sirt_post_staff_final
set.seed(13579)
n = 9
B = 100000

Boot.post_staff_cost.1 <- matrix(sample(sirt_post_staff_final$sirt_post_staff_final, size= B*n, 
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
as.data.frame(post_staff_cost)



sirt_staff_total = merge(x = sirt_pre_staff_final, y = sirt_intra_staff_final, by = "record_id", all = TRUE)
sirt_staff_total = merge(x = sirt_staff_total, y = sirt_post_staff_final, by = "record_id", all = TRUE)

sirt_staff_total[is.na(sirt_staff_total)]<-0

sirt_staff_total = sirt_staff_total %>%
  select(1, sirt_pre_staff_final, sirt_intra_staff_final, sirt_post_staff_final) %>%
  mutate(sirt_staff_total = sirt_pre_staff_final + sirt_intra_staff_final +sirt_post_staff_final)



sirt_staff_total %>%
  summarise(sum = sum(sirt_staff_total))

sirt_staff_total_1 = sirt_staff_total %>%
  select(1, 5)




## -- Boot strapping -- ## sirt_total_staff_final
set.seed(13579)
n = 9
B = 100000

Boot.total_staff_cost.1 <- matrix(sample(sirt_staff_total$sirt_staff_total, size= B*n, 
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
as.data.frame(total_staff_cost)




###########################################################
########### Total cost ####################################

## Total
Total_cost = merge(x = sirt_imaging_4, y = sirt_combine_ConMed, by = "record_id", all = TRUE)
Total_cost = merge(x = Total_cost, y = sirt_staff_total_1, by = "record_id", all = TRUE)

Total_cost = Total_cost %>%
  mutate(sirt_total_cost = sirt_imaging_cost + total_ConMed + sirt_staff_total)

Total_cost %>%
  summarise(sum = sum(sirt_total_cost))


write.csv(Total_cost, "SIRT_total_workup.csv")


## Pre
Total_pre_cost = merge(x = sirt_imaging_pre, y = sirt_pre_ConMed_3, by = "record_id", all = TRUE)
Total_pre_cost = merge(x = Total_pre_cost, y = sirt_pre_staff_final_1, by = "record_id", all = TRUE)


Total_pre_cost = Total_pre_cost %>%
  mutate(sirt_pre_total_cost = cost_pre_imaging + cost_pre_conMed + sirt_pre_staff_final)

Total_pre_cost %>%
  summarise(sum = sum(sirt_pre_total_cost))


## INtra
Total_intra_cost = merge(x = sirt_imaging_intra, y = sirt_intra_ConMed_3, by = "record_id", all = TRUE)
Total_intra_cost = merge(x = Total_intra_cost, y = sirt_intra_staff_final_1, by = "record_id", all = TRUE)


Total_intra_cost = Total_intra_cost %>%
  mutate(sirt_intra_total_cost = cost_intra_imaging + cost_intra_conMed + sirt_intra_staff_final)
 
Total_intra_cost %>%
  summarise(sum = sum(sirt_intra_total_cost))


## Post
Total_post_cost = merge(x = sirt_imaging_post, y = sirt_post_ConMed_3, by = "record_id", all = TRUE)
Total_post_cost = merge(x = Total_post_cost, y = sirt_post_staff_final_1, by = "record_id", all = TRUE)

Total_post_cost[is.na(Total_post_cost)]<-0

Total_post_cost = Total_post_cost %>%
  mutate(sirt_post_total_cost = cost_post_imaging + cost_post_conMed + sirt_post_staff_final)

Total_post_cost %>%
  summarise(sum = sum(sirt_post_total_cost))




###### Boot strap total cost
n = 9

Boot.sirt_total_cost <- matrix(sample(Total_cost$sirt_total_cost, size= B*n, 
                                        replace=TRUE), ncol=B, nrow=n)


dim(Boot.sirt_total_cost)

Boot.sirt_total_cost[1:5,1:5]

Boot.sirt_total_cost.Means_1 <- colMeans(Boot.sirt_total_cost)

length(Boot.sirt_total_cost.Means_1)

Boot.sirt_total_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.sirt_total_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.sirt_total_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.sirt_total_cost.Means_1, prob=0.975)


sirt_total_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(sirt_total_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(sirt_total_cost)



###### Boot strap pre cost
Boot.sirt_total_pre_cost <- matrix(sample(Total_pre_cost$sirt_pre_total_cost, size= B*n, 
                                      replace=TRUE), ncol=B, nrow=n)


dim(Boot.sirt_total_pre_cost)

Boot.sirt_total_pre_cost[1:5,1:5]

Boot.sirt_total_pre_cost.Means_1 <- colMeans(Boot.sirt_total_pre_cost)

length(Boot.sirt_total_pre_cost.Means_1)

Boot.sirt_total_pre_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.sirt_total_pre_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.sirt_total_pre_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.sirt_total_pre_cost.Means_1, prob=0.975)


sirt_pre_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(sirt_pre_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(sirt_pre_cost)



###### Boot strap intra cost
Boot.sirt_total_intra_cost <- matrix(sample(Total_intra_cost$sirt_intra_total_cost, size= B*n, 
                                          replace=TRUE), ncol=B, nrow=n)


dim(Boot.sirt_total_intra_cost)

Boot.sirt_total_intra_cost[1:5,1:5]

Boot.sirt_total_intra_cost.Means_1 <- colMeans(Boot.sirt_total_intra_cost)

length(Boot.sirt_total_intra_cost.Means_1)

Boot.sirt_total_intra_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.sirt_total_intra_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.sirt_total_intra_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.sirt_total_intra_cost.Means_1, prob=0.975)


sirt_intra_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(sirt_intra_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(sirt_intra_cost)



###### Boot strap post cost
Boot.sirt_total_post_cost <- matrix(sample(Total_post_cost$sirt_post_total_cost, size= B*n, 
                                            replace=TRUE), ncol=B, nrow=n)


dim(Boot.sirt_total_post_cost)

Boot.sirt_total_post_cost[1:5,1:5]

Boot.sirt_total_post_cost.Means_1 <- colMeans(Boot.sirt_total_post_cost)

length(Boot.sirt_total_post_cost.Means_1)

Boot.sirt_total_post_cost.Means_1[1:10]

post_cost_1_L = quantile(Boot.sirt_total_post_cost.Means_1, prob=0.025)
post_cost_1_M = quantile(Boot.sirt_total_post_cost.Means_1, prob=0.5)
post_cost_1_U = quantile(Boot.sirt_total_post_cost.Means_1, prob=0.975)


sirt_post_cost = matrix(
  c(-1, post_cost_1_L, post_cost_1_M, post_cost_1_U),
  ncol = 4,
  byrow = TRUE)

colnames(sirt_post_cost) = c("Year", "L_CI","Mean","U_CI")
as.data.frame(sirt_post_cost)





#################  To estimate the staff clinical team level cost - Pre ##########


######### Administrative_labour

sirt_pre_staff %>%
  summarise(max = max(sirt_staff_al)) # 3



#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_01_team = case_when(
    sirt_staff_al_01_team %in% 1 ~ "Oncology",
    sirt_staff_al_01_team %in% 2 ~ "Radiology",
    sirt_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_01_team = coalesce(sirt_staff_al_01_team, sirt_staff_al_01_other))
sirt_pre_staff$sirt_staff_al_01_team = as.factor(sirt_pre_staff$sirt_staff_al_01_team)

levels(sirt_pre_staff$sirt_staff_al_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)



#2
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_02_team = case_when(
    sirt_staff_al_02_team %in% 1 ~ "Oncology",
    sirt_staff_al_02_team %in% 2 ~ "Radiology",
    sirt_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_02_team = coalesce(sirt_staff_al_02_team, sirt_staff_al_02_other))
sirt_pre_staff$sirt_staff_al_02_team = as.factor(sirt_pre_staff$sirt_staff_al_02_team)

levels(sirt_pre_staff$sirt_staff_al_02_team)

sirt_pre_staff %>%
  filter(sirt_staff_al_02_team == "Admission" | sirt_staff_al_02_team == "Admissions" |sirt_staff_al_02_team == "Adimissions") %>%
  summarise(sum = sum(sirt_staff_al_02_hours),
            cost = sum*Administrative_labour)

sirt_pre_staff %>%
  filter(sirt_staff_al_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_al_02_hours),
            cost = sum*Administrative_labour)




#3
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_03_team = case_when(
    sirt_staff_al_03_team %in% 1 ~ "Oncology",
    sirt_staff_al_03_team %in% 2 ~ "Radiology",
    sirt_staff_al_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_03_team %in% 4 ~ "Anaesthesia"
  ))

sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_al_03_team = coalesce(sirt_staff_al_03_team, sirt_staff_al_03_other))
sirt_pre_staff$sirt_staff_al_03_team = as.factor(sirt_pre_staff$sirt_staff_al_03_team)

levels(sirt_pre_staff$sirt_staff_al_03_team)


sirt_pre_staff %>%
  filter(sirt_staff_al_03_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_al_03_hours),
            cost = sum*Administrative_labour)

sirt_pre_staff %>%
  filter(sirt_staff_al_03_team == "Infectious Diseases") %>%
  summarise(sum = sum(sirt_staff_al_03_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

sirt_pre_staff %>%
  summarise(max = max(sirt_staff_ass_n)) # 0


######## Enrolled_nurse
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_enr_n)) # 0


####### Registered_nurse
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_reg_n)) # 4


#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_01_team = case_when(
    sirt_staff_reg_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_01_team = coalesce(sirt_staff_reg_n_01_team, sirt_staff_reg_n_01_other))
sirt_pre_staff$sirt_staff_reg_n_01_team = as.factor(sirt_pre_staff$sirt_staff_reg_n_01_team)

levels(sirt_pre_staff$sirt_staff_reg_n_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_01_team == "Infectious Disease") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_01_team == "SCU") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)



#2
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_02_team = case_when(
    sirt_staff_reg_n_02_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_02_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_02_team = coalesce(sirt_staff_reg_n_02_team, sirt_staff_reg_n_02_other))
sirt_pre_staff$sirt_staff_reg_n_02_team = as.factor(sirt_pre_staff$sirt_staff_reg_n_02_team)

levels(sirt_pre_staff$sirt_staff_reg_n_02_team)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_02_team == "Infectious Disease") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)



#3
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_03_team = case_when(
    sirt_staff_reg_n_03_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_03_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_03_team = coalesce(sirt_staff_reg_n_03_team, sirt_staff_reg_n_03_other))
sirt_pre_staff$sirt_staff_reg_n_03_team = as.factor(sirt_pre_staff$sirt_staff_reg_n_03_team)

levels(sirt_pre_staff$sirt_staff_reg_n_03_team)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_03_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_03_team == "Infectious Disease") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



#4
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_04_team = case_when(
    sirt_staff_reg_n_04_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_04_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_reg_n_04_team = coalesce(sirt_staff_reg_n_04_team, sirt_staff_reg_n_04_other))
sirt_pre_staff$sirt_staff_reg_n_04_team = as.factor(sirt_pre_staff$sirt_staff_reg_n_04_team)

levels(sirt_pre_staff$sirt_staff_reg_n_04_team)

sirt_pre_staff %>%
  filter(sirt_staff_reg_n_04_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)




######### Clinical_nurse
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_cln_n)) # 2


#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_cln_n_01_team = case_when(
    sirt_staff_cln_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_cln_n_01_team = coalesce(sirt_staff_cln_n_01_team, sirt_staff_cln_n_01_other))
sirt_pre_staff$sirt_staff_cln_n_01_team = as.factor(sirt_pre_staff$sirt_staff_cln_n_01_team)

levels(sirt_pre_staff$sirt_staff_cln_n_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_cln_n_01_team == "Oncology") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Clinical_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_cln_n_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Clinical_nurse)

#2
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_cln_n_02_team = case_when(
    sirt_staff_cln_n_02_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_02_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_cln_n_02_team = coalesce(sirt_staff_cln_n_02_team, sirt_staff_cln_n_02_other))
sirt_pre_staff$sirt_staff_cln_n_02_team = as.factor(sirt_pre_staff$sirt_staff_cln_n_02_team)

levels(sirt_pre_staff$sirt_staff_cln_n_02_team)

sirt_pre_staff %>%
  filter(sirt_staff_cln_n_02_team == "Medical") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Clinical_nurse)

sirt_pre_staff %>%
  filter(sirt_staff_cln_n_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Clinical_nurse)




############ Assistant_nurse_unit_manager
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_anum)) # 0



############ Case_manager
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_case_m)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_case_m_01_team = case_when(
    sirt_staff_case_m_01_team %in% 1 ~ "Oncology",
    sirt_staff_case_m_01_team %in% 2 ~ "Radiology",
    sirt_staff_case_m_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_case_m_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_case_m_01_team = coalesce(sirt_staff_case_m_01_team, sirt_staff_case_m_01_other))
sirt_pre_staff$sirt_staff_case_m_01_team = as.factor(sirt_pre_staff$sirt_staff_case_m_01_team)

levels(sirt_pre_staff$sirt_staff_case_m_01_team)


sirt_pre_staff %>%
  filter(sirt_staff_case_m_01_team == "HPB" | sirt_staff_case_m_01_team == "HPB/HCC") %>%
  summarise(sum = sum(sirt_staff_case_m_01_hours),
            cost = sum*Case_manager)

sirt_pre_staff %>%
  filter(sirt_staff_case_m_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_case_m_01_hours),
            cost = sum*Case_manager)




############## Nurse_unit_manager	
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_nur_um)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nur_um_01_team = case_when(
    sirt_staff_nur_um_01_team %in% 1 ~ "Oncology",
    sirt_staff_nur_um_01_team %in% 2 ~ "Radiology",
    sirt_staff_nur_um_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nur_um_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nur_um_01_team = coalesce(sirt_staff_nur_um_01_team, sirt_staff_nur_um_01_other))
sirt_pre_staff$sirt_staff_nur_um_01_team = as.factor(sirt_pre_staff$sirt_staff_nur_um_01_team)

levels(sirt_pre_staff$sirt_staff_nur_um_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_nur_um_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_nur_um_01_hours),
            cost = sum*Nurse_unit_manager)



############## Intern_or_Resident
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_in_res)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_in_res_01_team = case_when(
    sirt_staff_in_res_01_team %in% 1 ~ "Oncology",
    sirt_staff_in_res_01_team %in% 2 ~ "Radiology",
    sirt_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_in_res_01_team = coalesce(sirt_staff_in_res_01_team, sirt_staff_in_res_01_other))
sirt_pre_staff$sirt_staff_in_res_01_team = as.factor(sirt_pre_staff$sirt_staff_in_res_01_team)

levels(sirt_pre_staff$sirt_staff_in_res_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_in_res_01_team == "GAST" | sirt_staff_in_res_01_team == "Gastro" | sirt_staff_in_res_01_team == "GASTRO" |
           sirt_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)


sirt_pre_staff %>%
  filter(sirt_staff_in_res_01_team == "Oncology") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_pre_staff %>%
  filter(sirt_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)



############# Registrar
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_registrar)) # 0

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_registrar_01_team = case_when(
    sirt_staff_registrar_01_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_01_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_registrar_01_team = coalesce(sirt_staff_registrar_01_team, sirt_staff_registrar_01_other))
sirt_pre_staff$sirt_staff_registrar_01_team = as.factor(sirt_pre_staff$sirt_staff_registrar_01_team)

levels(sirt_pre_staff$sirt_staff_registrar_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_registrar_01_team == "GAST" | sirt_staff_registrar_01_team == "Gastro" | sirt_staff_registrar_01_team == "GASTRO" |
           sirt_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)



############ Fellow
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_fellow)) # 0




################# Consultant
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_consultant)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_consultant_01_team = case_when(
    sirt_staff_consultant_01_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_01_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_consultant_01_team = coalesce(sirt_staff_consultant_01_team, sirt_staff_consultant_01_other))
sirt_pre_staff$sirt_staff_consultant_01_team = as.factor(sirt_pre_staff$sirt_staff_consultant_01_team)

levels(sirt_pre_staff$sirt_staff_consultant_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)






############### Radiographer
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_radio)) # 2

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_radio_01_team = case_when(
    sirt_staff_radio_01_team %in% 1 ~ "Oncology",
    sirt_staff_radio_01_team %in% 2 ~ "Radiology",
    sirt_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_radio_01_team = coalesce(sirt_staff_radio_01_team, sirt_staff_radio_01_other))
sirt_pre_staff$sirt_staff_radio_01_team = as.factor(sirt_pre_staff$sirt_staff_radio_01_team)

levels(sirt_pre_staff$sirt_staff_radio_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_radio_02_team = case_when(
    sirt_staff_radio_02_team %in% 1 ~ "Oncology",
    sirt_staff_radio_02_team %in% 2 ~ "Radiology",
    sirt_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_radio_02_team = coalesce(sirt_staff_radio_02_team, sirt_staff_radio_02_other))
sirt_pre_staff$sirt_staff_radio_02_team = as.factor(sirt_pre_staff$sirt_staff_radio_02_team)

levels(sirt_pre_staff$sirt_staff_radio_02_team)

sirt_pre_staff %>%
  filter(sirt_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_ultra)) # 0

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_ultra_01_team = case_when(
    sirt_staff_ultra_01_team %in% 1 ~ "Oncology",
    sirt_staff_ultra_01_team %in% 2 ~ "Radiology",
    sirt_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_ultra_01_team = coalesce(sirt_staff_ultra_01_team, sirt_staff_ultra_01_other))
sirt_pre_staff$sirt_staff_ultra_01_team = as.factor(sirt_pre_staff$sirt_staff_ultra_01_team)

levels(sirt_pre_staff$sirt_staff_ultra_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)






################ Nuc_Med_Tech
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_nuc_mt)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = case_when(
    sirt_staff_nuc_mt_01_team %in% 1 ~ "Oncology",
    sirt_staff_nuc_mt_01_team %in% 2 ~ "Radiology",
    sirt_staff_nuc_mt_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nuc_mt_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = coalesce(sirt_staff_nuc_mt_01_team, sirt_staff_nuc_mt_01_other))
sirt_pre_staff$sirt_staff_nuc_mt_01_team = as.factor(sirt_pre_staff$sirt_staff_nuc_mt_01_team)

levels(sirt_pre_staff$sirt_staff_nuc_mt_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_nuc_mt_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nuc_mt_01_hours),
            cost = sum*Nuc_Med_Tech)




###################### NM_physicist
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_nm_ph)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nm_ph_01_team = case_when(
    sirt_staff_nm_ph_01_team %in% 1 ~ "Oncology",
    sirt_staff_nm_ph_01_team %in% 2 ~ "Radiology",
    sirt_staff_nm_ph_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nm_ph_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_nm_ph_01_team = coalesce(sirt_staff_nm_ph_01_team, sirt_staff_nm_ph_01_other))
sirt_pre_staff$sirt_staff_nm_ph_01_team = as.factor(sirt_pre_staff$sirt_staff_nm_ph_01_team)

levels(sirt_pre_staff$sirt_staff_nm_ph_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_nm_ph_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nm_ph_01_hours),
            cost = sum*NM_physicist)



########################### Labour_nuclear_medicine
sirt_pre_staff %>%
  summarise(max = max(sirt_staff_mab_nm)) # 1

#1
sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_mab_nm_01_team = case_when(
    sirt_staff_mab_nm_01_team %in% 1 ~ "Oncology",
    sirt_staff_mab_nm_01_team %in% 2 ~ "Radiology",
    sirt_staff_mab_nm_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_mab_nm_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_pre_staff = sirt_pre_staff %>%
  mutate(sirt_staff_mab_nm_01_team = coalesce(sirt_staff_mab_nm_01_team, sirt_staff_mab_nm_01_other))
sirt_pre_staff$sirt_staff_mab_nm_01_team = as.factor(sirt_pre_staff$sirt_staff_mab_nm_01_team)

levels(sirt_pre_staff$sirt_staff_mab_nm_01_team)

sirt_pre_staff %>%
  filter(sirt_staff_mab_nm_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_mab_nm_01_hours),
            cost = sum*Labour_nuclear_medicine)




#######################################################  To estimate the staff clinical team level cost - intra ##########


######### Administrative_labour

sirt_intra_staff %>%
  summarise(max = max(sirt_staff_al)) # 0



#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_al_01_team = case_when(
    sirt_staff_al_01_team %in% 1 ~ "Oncology",
    sirt_staff_al_01_team %in% 2 ~ "Radiology",
    sirt_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_al_01_team = coalesce(sirt_staff_al_01_team, sirt_staff_al_01_other))
sirt_intra_staff$sirt_staff_al_01_team = as.factor(sirt_intra_staff$sirt_staff_al_01_team)

levels(sirt_intra_staff$sirt_staff_al_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)



#2
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_al_02_team = case_when(
    sirt_staff_al_02_team %in% 1 ~ "Oncology",
    sirt_staff_al_02_team %in% 2 ~ "Radiology",
    sirt_staff_al_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_02_team %in% 4 ~ "Anaesthesia"
  ))

sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_al_02_team = coalesce(sirt_staff_al_02_team, sirt_staff_al_02_other))
sirt_intra_staff$sirt_staff_al_02_team = as.factor(sirt_intra_staff$sirt_staff_al_02_team)

levels(sirt_intra_staff$sirt_staff_al_02_team)

sirt_intra_staff %>%
  filter(sirt_staff_al_02_team == "Admission") %>%
  summarise(sum = sum(sirt_staff_al_02_hours),
            cost = sum*Administrative_labour)





######### Assistant_in_nursing

sirt_intra_staff %>%
  summarise(max = max(sirt_staff_ass_n)) # 0


######## Enrolled_nurse
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_enr_n)) # 0

sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_enr_n_01_team = case_when(
    sirt_staff_enr_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_enr_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_enr_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_enr_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_enr_n_01_team = coalesce(sirt_staff_enr_n_01_team, sirt_staff_enr_n_01_other))
sirt_intra_staff$sirt_staff_enr_n_01_team = as.factor(sirt_intra_staff$sirt_staff_enr_n_01_team)

levels(sirt_intra_staff$sirt_staff_enr_n_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_enr_n_01_team == "Anaesthesia") %>%
  summarise(sum = sum(sirt_staff_enr_n_01_hours),
            cost = sum*Enrolled_nurse)

sirt_intra_staff %>%
  filter(sirt_staff_enr_n_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_enr_n_01_hours),
            cost = sum*Enrolled_nurse)



####### Registered_nurse
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_reg_n)) # 4


#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_01_team = case_when(
    sirt_staff_reg_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_01_team = coalesce(sirt_staff_reg_n_01_team, sirt_staff_reg_n_01_other))
sirt_intra_staff$sirt_staff_reg_n_01_team = as.factor(sirt_intra_staff$sirt_staff_reg_n_01_team)

levels(sirt_intra_staff$sirt_staff_reg_n_01_team)


sirt_intra_staff %>%
  filter(sirt_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)




#2
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_02_team = case_when(
    sirt_staff_reg_n_02_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_02_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_02_team = coalesce(sirt_staff_reg_n_02_team, sirt_staff_reg_n_02_other))
sirt_intra_staff$sirt_staff_reg_n_02_team = as.factor(sirt_intra_staff$sirt_staff_reg_n_02_team)

levels(sirt_intra_staff$sirt_staff_reg_n_02_team)


sirt_intra_staff %>%
  filter(sirt_staff_reg_n_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)


#3
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_03_team = case_when(
    sirt_staff_reg_n_03_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_03_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_03_team = coalesce(sirt_staff_reg_n_03_team, sirt_staff_reg_n_03_other))
sirt_intra_staff$sirt_staff_reg_n_03_team = as.factor(sirt_intra_staff$sirt_staff_reg_n_03_team)

levels(sirt_intra_staff$sirt_staff_reg_n_03_team)


sirt_intra_staff %>%
  filter(sirt_staff_reg_n_03_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)



#4
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_04_team = case_when(
    sirt_staff_reg_n_04_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_04_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_reg_n_04_team = coalesce(sirt_staff_reg_n_04_team, sirt_staff_reg_n_04_other))
sirt_intra_staff$sirt_staff_reg_n_04_team = as.factor(sirt_intra_staff$sirt_staff_reg_n_04_team)

levels(sirt_intra_staff$sirt_staff_reg_n_04_team)

sirt_intra_staff %>%
  filter(sirt_staff_reg_n_04_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)




######### Clinical_nurse
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_cln_n)) # 3


#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_01_team = case_when(
    sirt_staff_cln_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_01_team = coalesce(sirt_staff_cln_n_01_team, sirt_staff_cln_n_01_other))
sirt_intra_staff$sirt_staff_cln_n_01_team = as.factor(sirt_intra_staff$sirt_staff_cln_n_01_team)

levels(sirt_intra_staff$sirt_staff_cln_n_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_cln_n_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


#2
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_02_team = case_when(
    sirt_staff_cln_n_02_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_02_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_02_team = coalesce(sirt_staff_cln_n_02_team, sirt_staff_cln_n_02_other))
sirt_intra_staff$sirt_staff_cln_n_02_team = as.factor(sirt_intra_staff$sirt_staff_cln_n_02_team)

levels(sirt_intra_staff$sirt_staff_cln_n_02_team)

sirt_intra_staff %>%
  filter(sirt_staff_cln_n_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_cln_n_02_hours),
            cost = sum*Clinical_nurse)

#3
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_03_team = case_when(
    sirt_staff_cln_n_03_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_03_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_cln_n_03_team = coalesce(sirt_staff_cln_n_03_team, sirt_staff_cln_n_03_other))
sirt_intra_staff$sirt_staff_cln_n_03_team = as.factor(sirt_intra_staff$sirt_staff_cln_n_03_team)

levels(sirt_intra_staff$sirt_staff_cln_n_03_team)

sirt_intra_staff %>%
  filter(sirt_staff_cln_n_03_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_cln_n_03_hours),
            cost = sum*Clinical_nurse)



############ Assistant_nurse_unit_manager
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_anum)) # 0



############ Case_manager
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_case_m)) # 0


#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_case_m_01_team = case_when(
    sirt_staff_case_m_01_team %in% 1 ~ "Oncology",
    sirt_staff_case_m_01_team %in% 2 ~ "Radiology",
    sirt_staff_case_m_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_case_m_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_case_m_01_team = coalesce(sirt_staff_case_m_01_team, sirt_staff_case_m_01_other))
sirt_intra_staff$sirt_staff_case_m_01_team = as.factor(sirt_intra_staff$sirt_staff_case_m_01_team)

levels(sirt_intra_staff$sirt_staff_case_m_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_case_m_01_team == "HPB/HCC") %>%
  summarise(sum = sum(sirt_staff_case_m_01_hours),
            cost = sum*Clinical_nurse)





############## Nurse_unit_manager	
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_nur_um)) # 0



############## Intern_or_Resident
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_in_res)) # 0

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_in_res_01_team = case_when(
    sirt_staff_in_res_01_team %in% 1 ~ "Oncology",
    sirt_staff_in_res_01_team %in% 2 ~ "Radiology",
    sirt_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_in_res_01_team = coalesce(sirt_staff_in_res_01_team, sirt_staff_in_res_01_other))
sirt_intra_staff$sirt_staff_in_res_01_team = as.factor(sirt_intra_staff$sirt_staff_in_res_01_team)

levels(sirt_intra_staff$sirt_staff_in_res_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_in_res_01_team == "GAST" | sirt_staff_in_res_01_team == "Gastro" | sirt_staff_in_res_01_team == "GASTRO" |
           sirt_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)




############# Registrar
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_registrar)) # 1

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_registrar_01_team = case_when(
    sirt_staff_registrar_01_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_01_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_registrar_01_team = coalesce(sirt_staff_registrar_01_team, sirt_staff_registrar_01_other))
sirt_intra_staff$sirt_staff_registrar_01_team = as.factor(sirt_intra_staff$sirt_staff_registrar_01_team)

levels(sirt_intra_staff$sirt_staff_registrar_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_registrar_01_team == "GAST" | sirt_staff_registrar_01_team == "Gastro" | sirt_staff_registrar_01_team == "GASTRO" |
           sirt_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_intra_staff %>%
  filter(sirt_staff_registrar_01_team == "Anaesthesia") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_intra_staff %>%
  filter(sirt_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)




############ Fellow
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_fellow)) # 1

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_fellow_01_team = case_when(
    sirt_staff_fellow_01_team %in% 1 ~ "Oncology",
    sirt_staff_fellow_01_team %in% 2 ~ "Radiology",
    sirt_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_fellow_01_team = coalesce(sirt_staff_fellow_01_team, sirt_staff_fellow_01_other))
sirt_intra_staff$sirt_staff_fellow_01_team = as.factor(sirt_intra_staff$sirt_staff_fellow_01_team)

levels(sirt_intra_staff$sirt_staff_fellow_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_fellow_01_hours),
            cost = sum*Fellow)



################# Consultant
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_consultant)) # 2

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_consultant_01_team = case_when(
    sirt_staff_consultant_01_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_01_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_consultant_01_team = coalesce(sirt_staff_consultant_01_team, sirt_staff_consultant_01_other))
sirt_intra_staff$sirt_staff_consultant_01_team = as.factor(sirt_intra_staff$sirt_staff_consultant_01_team)

levels(sirt_intra_staff$sirt_staff_consultant_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_intra_staff %>%
  filter(sirt_staff_consultant_01_team == "Anaesthesia") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_consultant_02_team = case_when(
    sirt_staff_consultant_02_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_02_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_consultant_02_team = coalesce(sirt_staff_consultant_02_team, sirt_staff_consultant_02_other))
sirt_intra_staff$sirt_staff_consultant_02_team = as.factor(sirt_intra_staff$sirt_staff_consultant_02_team)

levels(sirt_intra_staff$sirt_staff_consultant_02_team)

sirt_intra_staff %>%
  filter(sirt_staff_consultant_02_team == "Anaesthesia") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)

sirt_intra_staff %>%
  filter(sirt_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)




############### Radiographer
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_radio)) # 2

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_radio_01_team = case_when(
    sirt_staff_radio_01_team %in% 1 ~ "Oncology",
    sirt_staff_radio_01_team %in% 2 ~ "Radiology",
    sirt_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_radio_01_team = coalesce(sirt_staff_radio_01_team, sirt_staff_radio_01_other))
sirt_intra_staff$sirt_staff_radio_01_team = as.factor(sirt_intra_staff$sirt_staff_radio_01_team)

levels(sirt_intra_staff$sirt_staff_radio_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_radio_02_team = case_when(
    sirt_staff_radio_02_team %in% 1 ~ "Oncology",
    sirt_staff_radio_02_team %in% 2 ~ "Radiology",
    sirt_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_radio_02_team = coalesce(sirt_staff_radio_02_team, sirt_staff_radio_02_other))
sirt_intra_staff$sirt_staff_radio_02_team = as.factor(sirt_intra_staff$sirt_staff_radio_02_team)

levels(sirt_intra_staff$sirt_staff_radio_02_team)

sirt_intra_staff %>%
  filter(sirt_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_02_hours),
            cost = sum*Radiographer)




############# Ultrasonographer
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_ultra)) # 0


#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_ultra_01_team = case_when(
    sirt_staff_ultra_01_team %in% 1 ~ "Oncology",
    sirt_staff_ultra_01_team %in% 2 ~ "Radiology",
    sirt_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_ultra_01_team = coalesce(sirt_staff_ultra_01_team, sirt_staff_ultra_01_other))
sirt_intra_staff$sirt_staff_ultra_01_team = as.factor(sirt_intra_staff$sirt_staff_ultra_01_team)

levels(sirt_intra_staff$sirt_staff_ultra_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)




################ Nuc_Med_Tech
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_nuc_mt)) # 1

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = case_when(
    sirt_staff_nuc_mt_01_team %in% 1 ~ "Oncology",
    sirt_staff_nuc_mt_01_team %in% 2 ~ "Radiology",
    sirt_staff_nuc_mt_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nuc_mt_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = coalesce(sirt_staff_nuc_mt_01_team, sirt_staff_nuc_mt_01_other))
sirt_intra_staff$sirt_staff_nuc_mt_01_team = as.factor(sirt_intra_staff$sirt_staff_nuc_mt_01_team)

levels(sirt_intra_staff$sirt_staff_nuc_mt_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_nuc_mt_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nuc_mt_01_hours),
            cost = sum*Nuc_Med_Tech)




###################### NM_physicist
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_nm_ph)) # 1

#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_nm_ph_01_team = case_when(
    sirt_staff_nm_ph_01_team %in% 1 ~ "Oncology",
    sirt_staff_nm_ph_01_team %in% 2 ~ "Radiology",
    sirt_staff_nm_ph_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nm_ph_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_nm_ph_01_team = coalesce(sirt_staff_nm_ph_01_team, sirt_staff_nm_ph_01_other))
sirt_intra_staff$sirt_staff_nm_ph_01_team = as.factor(sirt_intra_staff$sirt_staff_nm_ph_01_team)

levels(sirt_intra_staff$sirt_staff_nm_ph_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_nm_ph_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nm_ph_01_hours),
            cost = sum*NM_physicist)




##################### Labour_nuclear_medicine
sirt_intra_staff %>%
  summarise(max = max(sirt_staff_mab_nm)) # 0


#1
sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_mab_nm_01_team = case_when(
    sirt_staff_mab_nm_01_team %in% 1 ~ "Oncology",
    sirt_staff_mab_nm_01_team %in% 2 ~ "Radiology",
    sirt_staff_mab_nm_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_mab_nm_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_intra_staff = sirt_intra_staff %>%
  mutate(sirt_staff_mab_nm_01_team = coalesce(sirt_staff_mab_nm_01_team, sirt_staff_mab_nm_01_other))
sirt_intra_staff$sirt_staff_mab_nm_01_team = as.factor(sirt_intra_staff$sirt_staff_mab_nm_01_team)

levels(sirt_intra_staff$sirt_staff_mab_nm_01_team)

sirt_intra_staff %>%
  filter(sirt_staff_mab_nm_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_mab_nm_01_hours),
            cost = sum*Labour_nuclear_medicine)



#################  To estimate the staff clinical team level cost - Post ##########


######### Administrative_labour

sirt_post_staff %>%
  summarise(max = max(sirt_staff_al)) # 1



#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_al_01_team = case_when(
    sirt_staff_al_01_team %in% 1 ~ "Oncology",
    sirt_staff_al_01_team %in% 2 ~ "Radiology",
    sirt_staff_al_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_al_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_al_01_team = coalesce(sirt_staff_al_01_team, sirt_staff_al_01_other))
sirt_post_staff$sirt_staff_al_01_team = as.factor(sirt_post_staff$sirt_staff_al_01_team)

levels(sirt_post_staff$sirt_staff_al_01_team)

sirt_post_staff %>%
  filter(sirt_staff_al_01_team == "5D") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)

sirt_post_staff %>%
  filter(sirt_staff_al_01_team == "Infectious Diseases") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)

sirt_post_staff %>%
  filter(sirt_staff_al_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)

sirt_post_staff %>%
  filter(sirt_staff_al_01_team == "GASTRO" | sirt_staff_al_01_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)


sirt_post_staff %>%
  filter(sirt_staff_al_01_team == "surgical" | sirt_staff_al_01_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_al_01_hours),
            cost = sum*Administrative_labour)



######### Assistant_in_nursing

sirt_post_staff %>%
  summarise(max = max(sirt_staff_ass_n)) # 0


######## Enrolled_nurse
sirt_post_staff %>%
  summarise(max = max(sirt_staff_enr_n)) # 0






####### Registered_nurse
sirt_post_staff %>%
  summarise(max = max(sirt_staff_reg_n)) # 3


#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_01_team = case_when(
    sirt_staff_reg_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_01_team = coalesce(sirt_staff_reg_n_01_team, sirt_staff_reg_n_01_other))
sirt_post_staff$sirt_staff_reg_n_01_team = as.factor(sirt_post_staff$sirt_staff_reg_n_01_team)

levels(sirt_post_staff$sirt_staff_reg_n_01_team)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_01_team == "5D") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_01_team == "GAST" | sirt_staff_reg_n_01_team == "GASTRO" | sirt_staff_reg_n_01_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_01_team == "Infectious Diseases") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_01_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_01_hours),
            cost = sum*Registered_nurse)



#2
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_02_team = case_when(
    sirt_staff_reg_n_02_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_02_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_02_team = coalesce(sirt_staff_reg_n_02_team, sirt_staff_reg_n_02_other))
sirt_post_staff$sirt_staff_reg_n_02_team = as.factor(sirt_post_staff$sirt_staff_reg_n_02_team)

levels(sirt_post_staff$sirt_staff_reg_n_02_team)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_02_team == "5D") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_02_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_02_team == "Injectious Disease") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_02_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_02_hours),
            cost = sum*Registered_nurse)


#3
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_03_team = case_when(
    sirt_staff_reg_n_03_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_03_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_03_team = coalesce(sirt_staff_reg_n_03_team, sirt_staff_reg_n_03_other))
sirt_post_staff$sirt_staff_reg_n_03_team = as.factor(sirt_post_staff$sirt_staff_reg_n_03_team)

levels(sirt_post_staff$sirt_staff_reg_n_03_team)



sirt_post_staff %>%
  filter(sirt_staff_reg_n_03_team == "5D") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_03_team == "Infectious Disease") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_03_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_03_hours),
            cost = sum*Registered_nurse)




#4
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_04_team = case_when(
    sirt_staff_reg_n_04_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_04_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_04_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_04_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_04_team = coalesce(sirt_staff_reg_n_04_team, sirt_staff_reg_n_04_other))
sirt_post_staff$sirt_staff_reg_n_04_team = as.factor(sirt_post_staff$sirt_staff_reg_n_04_team)

levels(sirt_post_staff$sirt_staff_reg_n_04_team)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "2A") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "GAST" | sirt_staff_reg_n_04_team == "Gastro" | sirt_staff_reg_n_04_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "CCU") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_04_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_04_hours),
            cost = sum*Registered_nurse)






#5
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_05_team = case_when(
    sirt_staff_reg_n_05_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_05_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_05_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_05_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_05_team = coalesce(sirt_staff_reg_n_05_team, sirt_staff_reg_n_05_other))
sirt_post_staff$sirt_staff_reg_n_05_team = as.factor(sirt_post_staff$sirt_staff_reg_n_05_team)

levels(sirt_post_staff$sirt_staff_reg_n_05_team)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "2A") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "CARD") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_05_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_05_hours),
            cost = sum*Registered_nurse)





#6
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_06_team = case_when(
    sirt_staff_reg_n_06_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_06_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_06_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_06_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_06_team = coalesce(sirt_staff_reg_n_06_team, sirt_staff_reg_n_06_other))
sirt_post_staff$sirt_staff_reg_n_06_team = as.factor(sirt_post_staff$sirt_staff_reg_n_06_team)

levels(sirt_post_staff$sirt_staff_reg_n_06_team)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "2A") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "CARD") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_06_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_06_hours),
            cost = sum*Registered_nurse)



#7
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_07_team = case_when(
    sirt_staff_reg_n_07_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_07_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_07_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_07_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_07_team = coalesce(sirt_staff_reg_n_07_team, sirt_staff_reg_n_07_other))
sirt_post_staff$sirt_staff_reg_n_07_team = as.factor(sirt_post_staff$sirt_staff_reg_n_07_team)

levels(sirt_post_staff$sirt_staff_reg_n_07_team)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_07_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_07_team == "CARD") %>%
  summarise(sum = sum(sirt_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_07_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_07_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_reg_n_07_hours),
            cost = sum*Registered_nurse)




#8
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_08_team = case_when(
    sirt_staff_reg_n_08_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_08_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_08_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_08_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_08_team = coalesce(sirt_staff_reg_n_08_team, sirt_staff_reg_n_08_other))
sirt_post_staff$sirt_staff_reg_n_08_team = as.factor(sirt_post_staff$sirt_staff_reg_n_08_team)

levels(sirt_post_staff$sirt_staff_reg_n_08_team)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_08_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_08_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_08_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_08_hours),
            cost = sum*Registered_nurse)


#9
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_09_team = case_when(
    sirt_staff_reg_n_09_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_09_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_09_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_09_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_09_team = coalesce(sirt_staff_reg_n_09_team, sirt_staff_reg_n_09_other))
sirt_post_staff$sirt_staff_reg_n_09_team = as.factor(sirt_post_staff$sirt_staff_reg_n_09_team)

levels(sirt_post_staff$sirt_staff_reg_n_09_team)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_09_team == "2C") %>%
  summarise(sum = sum(sirt_staff_reg_n_09_hours),
            cost = sum*Registered_nurse)

sirt_post_staff %>%
  filter(sirt_staff_reg_n_09_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_09_hours),
            cost = sum*Registered_nurse)




#10
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_10_team = case_when(
    sirt_staff_reg_n_10_team %in% 1 ~ "Oncology",
    sirt_staff_reg_n_10_team %in% 2 ~ "Radiology",
    sirt_staff_reg_n_10_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_reg_n_10_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_reg_n_10_team = coalesce(sirt_staff_reg_n_10_team, sirt_staff_reg_n_10_other))
sirt_post_staff$sirt_staff_reg_n_10_team = as.factor(sirt_post_staff$sirt_staff_reg_n_10_team)

levels(sirt_post_staff$sirt_staff_reg_n_10_team)


sirt_post_staff %>%
  filter(sirt_staff_reg_n_10_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_reg_n_10_hours),
            cost = sum*Registered_nurse)






######### Clinical_nurse
sirt_post_staff %>%
  summarise(max = max(sirt_staff_cln_n)) # 0


#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_cln_n_01_team = case_when(
    sirt_staff_cln_n_01_team %in% 1 ~ "Oncology",
    sirt_staff_cln_n_01_team %in% 2 ~ "Radiology",
    sirt_staff_cln_n_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_cln_n_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_cln_n_01_team = coalesce(sirt_staff_cln_n_01_team, sirt_staff_cln_n_01_other))
sirt_post_staff$sirt_staff_cln_n_01_team = as.factor(sirt_post_staff$sirt_staff_cln_n_01_team)

levels(sirt_post_staff$sirt_staff_cln_n_01_team)

sirt_post_staff %>%
  filter(sirt_staff_cln_n_01_team == "5D") %>%
  summarise(sum = sum(sirt_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)


sirt_post_staff %>%
  filter(sirt_staff_cln_n_01_team == "HCC CNC") %>%
  summarise(sum = sum(sirt_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)

sirt_post_staff %>%
  filter(sirt_staff_cln_n_01_team == "PAH 11") %>%
  summarise(sum = sum(sirt_staff_cln_n_01_hours),
            cost = sum*Clinical_nurse)






############ Assistant_nurse_unit_manager
sirt_post_staff %>%
  summarise(max = max(sirt_staff_anum)) # 0



############ Case_manager
sirt_post_staff %>%
  summarise(max = max(sirt_staff_case_m)) # 0




############## Nurse_unit_manager	
sirt_post_staff %>%
  summarise(max = max(sirt_staff_nur_um)) # 0


#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nur_um_01_team = case_when(
    sirt_staff_nur_um_01_team %in% 1 ~ "Oncology",
    sirt_staff_nur_um_01_team %in% 2 ~ "Radiology",
    sirt_staff_nur_um_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nur_um_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nur_um_01_team = coalesce(sirt_staff_nur_um_01_team, sirt_staff_nur_um_01_other))
sirt_post_staff$sirt_staff_nur_um_01_team = as.factor(sirt_post_staff$sirt_staff_nur_um_01_team)

levels(sirt_post_staff$sirt_staff_nur_um_01_team)


sirt_post_staff %>%
  filter(sirt_staff_nur_um_01_team == "HPB") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Nurse_unit_manager)





############## Intern_or_Resident
sirt_post_staff %>%
  summarise(max = max(sirt_staff_in_res)) # 1

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_in_res_01_team = case_when(
    sirt_staff_in_res_01_team %in% 1 ~ "Oncology",
    sirt_staff_in_res_01_team %in% 2 ~ "Radiology",
    sirt_staff_in_res_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_in_res_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_in_res_01_team = coalesce(sirt_staff_in_res_01_team, sirt_staff_in_res_01_other))
sirt_post_staff$sirt_staff_in_res_01_team = as.factor(sirt_post_staff$sirt_staff_in_res_01_team)

levels(sirt_post_staff$sirt_staff_in_res_01_team)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "Oncology") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "GAST" | sirt_staff_in_res_01_team == "Gastro" | sirt_staff_in_res_01_team == "GASTRO" |
           sirt_staff_in_res_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "HPB") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "LIVR" ) %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "Radiology" ) %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "RESP" ) %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)

sirt_post_staff %>%
  filter(sirt_staff_in_res_01_team == "Surgery" | sirt_staff_in_res_01_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_in_res_01_hours),
            cost = sum*Intern_or_Resident)





############# Registrar
sirt_post_staff %>%
  summarise(max = max(sirt_staff_registrar)) # 1


#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_01_team = case_when(
    sirt_staff_registrar_01_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_01_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_01_team = coalesce(sirt_staff_registrar_01_team, sirt_staff_registrar_01_other))
sirt_post_staff$sirt_staff_registrar_01_team = as.factor(sirt_post_staff$sirt_staff_registrar_01_team)

levels(sirt_post_staff$sirt_staff_registrar_01_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "GAST" | sirt_staff_registrar_01_team == "Gastro" | sirt_staff_registrar_01_team == "GASTRO" |
           sirt_staff_registrar_01_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)


sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "HPB") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)


sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "LIVR") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "Medical") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_01_team == "UROL" | sirt_staff_registrar_01_team == "Urology") %>%
  summarise(sum = sum(sirt_staff_registrar_01_hours),
            cost = sum*Registrar)



#2
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_02_team = case_when(
    sirt_staff_registrar_02_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_02_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_02_team = coalesce(sirt_staff_registrar_02_team, sirt_staff_registrar_02_other))
sirt_post_staff$sirt_staff_registrar_02_team = as.factor(sirt_post_staff$sirt_staff_registrar_02_team)

levels(sirt_post_staff$sirt_staff_registrar_02_team)


sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "GAST" | sirt_staff_registrar_02_team == "Gastro" | sirt_staff_registrar_02_team == "GASTRO" |
           sirt_staff_registrar_02_team == "GAST") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)


sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "HPB") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)


sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "Ward call") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "Endocrine") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_02_team == "UROL" | sirt_staff_registrar_02_team == "Urology") %>%
  summarise(sum = sum(sirt_staff_registrar_02_hours),
            cost = sum*Registrar)




#3
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_03_team = case_when(
    sirt_staff_registrar_03_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_03_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_03_team = coalesce(sirt_staff_registrar_03_team, sirt_staff_registrar_03_other))
sirt_post_staff$sirt_staff_registrar_03_team = as.factor(sirt_post_staff$sirt_staff_registrar_03_team)

levels(sirt_post_staff$sirt_staff_registrar_03_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_03_team == "Medical") %>%
  summarise(sum = sum(sirt_staff_registrar_03_hours),
            cost = sum*Registrar)

sirt_post_staff %>%
  filter(sirt_staff_registrar_03_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_registrar_03_hours),
            cost = sum*Registrar)


sirt_post_staff %>%
  filter(sirt_staff_registrar_03_team == "Urology") %>%
  summarise(sum = sum(sirt_staff_registrar_03_hours),
            cost = sum*Registrar)


#4
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_04_team = case_when(
    sirt_staff_registrar_04_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_04_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_04_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_04_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_04_team = coalesce(sirt_staff_registrar_04_team, sirt_staff_registrar_04_other))
sirt_post_staff$sirt_staff_registrar_04_team = as.factor(sirt_post_staff$sirt_staff_registrar_04_team)

levels(sirt_post_staff$sirt_staff_registrar_04_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_04_team == "Radiology" ) %>%
  summarise(sum = sum(sirt_staff_registrar_04_hours),
            cost = sum*Registrar)



#5
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_05_team = case_when(
    sirt_staff_registrar_05_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_05_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_05_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_05_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_05_team = coalesce(sirt_staff_registrar_05_team, sirt_staff_registrar_05_other))
sirt_post_staff$sirt_staff_registrar_05_team = as.factor(sirt_post_staff$sirt_staff_registrar_05_team)

levels(sirt_post_staff$sirt_staff_registrar_05_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_05_team == "RESP" ) %>%
  summarise(sum = sum(sirt_staff_registrar_05_hours),
            cost = sum*Registrar)



#6
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_06_team = case_when(
    sirt_staff_registrar_06_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_06_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_06_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_06_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_06_team = coalesce(sirt_staff_registrar_06_team, sirt_staff_registrar_06_other))
sirt_post_staff$sirt_staff_registrar_06_team = as.factor(sirt_post_staff$sirt_staff_registrar_06_team)

levels(sirt_post_staff$sirt_staff_registrar_06_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_06_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_registrar_06_hours),
            cost = sum*Registrar)



#7
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_07_team = case_when(
    sirt_staff_registrar_07_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_07_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_07_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_07_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_07_team = coalesce(sirt_staff_registrar_07_team, sirt_staff_registrar_07_other))
sirt_post_staff$sirt_staff_registrar_07_team = as.factor(sirt_post_staff$sirt_staff_registrar_07_team)

levels(sirt_post_staff$sirt_staff_registrar_07_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_07_team == "Radiology" ) %>%
  summarise(sum = sum(sirt_staff_registrar_07_hours),
            cost = sum*Registrar)



#8
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_08_team = case_when(
    sirt_staff_registrar_08_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_08_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_08_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_08_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_08_team = coalesce(sirt_staff_registrar_08_team, sirt_staff_registrar_08_other))
sirt_post_staff$sirt_staff_registrar_08_team = as.factor(sirt_post_staff$sirt_staff_registrar_08_team)

levels(sirt_post_staff$sirt_staff_registrar_08_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_08_team == "RESP" ) %>%
  summarise(sum = sum(sirt_staff_registrar_08_hours),
            cost = sum*Registrar)



#9
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_09_team = case_when(
    sirt_staff_registrar_09_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_09_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_09_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_09_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_09_team = coalesce(sirt_staff_registrar_09_team, sirt_staff_registrar_09_other))
sirt_post_staff$sirt_staff_registrar_09_team = as.factor(sirt_post_staff$sirt_staff_registrar_09_team)

levels(sirt_post_staff$sirt_staff_registrar_09_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_09_team == "RESP" ) %>%
  summarise(sum = sum(sirt_staff_registrar_09_hours),
            cost = sum*Registrar)



#10
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_10_team = case_when(
    sirt_staff_registrar_10_team %in% 1 ~ "Oncology",
    sirt_staff_registrar_10_team %in% 2 ~ "Radiology",
    sirt_staff_registrar_10_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_registrar_10_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_registrar_10_team = coalesce(sirt_staff_registrar_10_team, sirt_staff_registrar_10_other))
sirt_post_staff$sirt_staff_registrar_10_team = as.factor(sirt_post_staff$sirt_staff_registrar_10_team)

levels(sirt_post_staff$sirt_staff_registrar_10_team)

sirt_post_staff %>%
  filter(sirt_staff_registrar_10_team == "Radiology" ) %>%
  summarise(sum = sum(sirt_staff_registrar_10_hours),
            cost = sum*Registrar)





############ Fellow
sirt_post_staff %>%
  summarise(max = max(sirt_staff_fellow)) # 1

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_fellow_01_team = case_when(
    sirt_staff_fellow_01_team %in% 1 ~ "Oncology",
    sirt_staff_fellow_01_team %in% 2 ~ "Radiology",
    sirt_staff_fellow_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_fellow_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_fellow_01_team = coalesce(sirt_staff_fellow_01_team, sirt_staff_fellow_01_other))
sirt_post_staff$sirt_staff_fellow_01_team = as.factor(sirt_post_staff$sirt_staff_fellow_01_team)

levels(sirt_post_staff$sirt_staff_fellow_01_team)

sirt_post_staff %>%
  filter(sirt_staff_fellow_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_fellow_01_hours),
            cost = sum*Fellow)

sirt_post_staff %>%
  filter(sirt_staff_fellow_01_team == "UROL") %>%
  summarise(sum = sum(sirt_staff_fellow_01_hours),
            cost = sum*Fellow)

sirt_post_staff %>%
  filter(sirt_staff_fellow_01_team == "GAST" | sirt_staff_fellow_01_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_fellow_01_hours),
            cost = sum*Fellow)





################# Consultant
sirt_post_staff %>%
  summarise(max = max(sirt_staff_consultant)) # 0

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_01_team = case_when(
    sirt_staff_consultant_01_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_01_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_01_team = coalesce(sirt_staff_consultant_01_team, sirt_staff_consultant_01_other))
sirt_post_staff$sirt_staff_consultant_01_team = as.factor(sirt_post_staff$sirt_staff_consultant_01_team)

levels(sirt_post_staff$sirt_staff_consultant_01_team)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "GAST" | sirt_staff_consultant_01_team == "Gastro") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "LIVR") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "Surgical") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_01_team == "UROL" | sirt_staff_consultant_01_team == "Urology") %>%
  summarise(sum = sum(sirt_staff_consultant_01_hours),
            cost = sum*Consultant)


#2
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_02_team = case_when(
    sirt_staff_consultant_02_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_02_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_02_team = coalesce(sirt_staff_consultant_02_team, sirt_staff_consultant_02_other))
sirt_post_staff$sirt_staff_consultant_02_team = as.factor(sirt_post_staff$sirt_staff_consultant_02_team)

levels(sirt_post_staff$sirt_staff_consultant_02_team)

sirt_post_staff %>%
  filter(sirt_staff_consultant_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_02_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_02_team == "Thoracic Medicine") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)

sirt_post_staff %>%
  filter(sirt_staff_consultant_02_team == "Urology") %>%
  summarise(sum = sum(sirt_staff_consultant_02_hours),
            cost = sum*Consultant)



#3
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_03_team = case_when(
    sirt_staff_consultant_03_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_03_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_03_team = coalesce(sirt_staff_consultant_03_team, sirt_staff_consultant_03_other))
sirt_post_staff$sirt_staff_consultant_03_team = as.factor(sirt_post_staff$sirt_staff_consultant_03_team)

levels(sirt_post_staff$sirt_staff_consultant_03_team)

sirt_post_staff %>%
  filter(sirt_staff_consultant_03_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_consultant_03_hours),
            cost = sum*Consultant)


#4
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_04_team = case_when(
    sirt_staff_consultant_04_team %in% 1 ~ "Oncology",
    sirt_staff_consultant_04_team %in% 2 ~ "Radiology",
    sirt_staff_consultant_04_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_consultant_04_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_consultant_04_team = coalesce(sirt_staff_consultant_04_team, sirt_staff_consultant_04_other))
sirt_post_staff$sirt_staff_consultant_04_team = as.factor(sirt_post_staff$sirt_staff_consultant_04_team)

levels(sirt_post_staff$sirt_staff_consultant_04_team)

sirt_post_staff %>%
  filter(sirt_staff_consultant_04_team == "RESP") %>%
  summarise(sum = sum(sirt_staff_consultant_04_hours),
            cost = sum*Consultant)




############### Radiographer
sirt_post_staff %>%
  summarise(max = max(sirt_staff_radio)) # 0

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_01_team = case_when(
    sirt_staff_radio_01_team %in% 1 ~ "Oncology",
    sirt_staff_radio_01_team %in% 2 ~ "Radiology",
    sirt_staff_radio_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_01_team = coalesce(sirt_staff_radio_01_team, sirt_staff_radio_01_other))
sirt_post_staff$sirt_staff_radio_01_team = as.factor(sirt_post_staff$sirt_staff_radio_01_team)

levels(sirt_post_staff$sirt_staff_radio_01_team)

sirt_post_staff %>%
  filter(sirt_staff_radio_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_01_hours),
            cost = sum*Radiographer)


#2
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_02_team = case_when(
    sirt_staff_radio_02_team %in% 1 ~ "Oncology",
    sirt_staff_radio_02_team %in% 2 ~ "Radiology",
    sirt_staff_radio_02_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_02_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_02_team = coalesce(sirt_staff_radio_02_team, sirt_staff_radio_02_other))
sirt_post_staff$sirt_staff_radio_02_team = as.factor(sirt_post_staff$sirt_staff_radio_02_team)

levels(sirt_post_staff$sirt_staff_radio_02_team)

sirt_post_staff %>%
  filter(sirt_staff_radio_02_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_02_hours),
            cost = sum*Radiographer)

#3
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_03_team = case_when(
    sirt_staff_radio_03_team %in% 3 ~ "Oncology",
    sirt_staff_radio_03_team %in% 2 ~ "Radiology",
    sirt_staff_radio_03_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_radio_03_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_radio_03_team = coalesce(sirt_staff_radio_03_team, sirt_staff_radio_03_other))
sirt_post_staff$sirt_staff_radio_03_team = as.factor(sirt_post_staff$sirt_staff_radio_03_team)

levels(sirt_post_staff$sirt_staff_radio_03_team)

sirt_post_staff %>%
  filter(sirt_staff_radio_03_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_radio_03_hours),
            cost = sum*Radiographer)





############# Ultrasonographer
sirt_post_staff %>%
  summarise(max = max(sirt_staff_ultra)) # 0

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_ultra_01_team = case_when(
    sirt_staff_ultra_01_team %in% 1 ~ "Oncology",
    sirt_staff_ultra_01_team %in% 2 ~ "Radiology",
    sirt_staff_ultra_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_ultra_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_ultra_01_team = coalesce(sirt_staff_ultra_01_team, sirt_staff_ultra_01_other))
sirt_post_staff$sirt_staff_ultra_01_team = as.factor(sirt_post_staff$sirt_staff_ultra_01_team)

levels(sirt_post_staff$sirt_staff_ultra_01_team)

sirt_post_staff %>%
  filter(sirt_staff_ultra_01_team == "Radiology") %>%
  summarise(sum = sum(sirt_staff_ultra_01_hours),
            cost = sum*Ultrasonographer)





################ Nuc_Med_Tech
sirt_post_staff %>%
  summarise(max = max(sirt_staff_nuc_mt)) # 1

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = case_when(
    sirt_staff_nuc_mt_01_team %in% 1 ~ "Oncology",
    sirt_staff_nuc_mt_01_team %in% 2 ~ "Radiology",
    sirt_staff_nuc_mt_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nuc_mt_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nuc_mt_01_team = coalesce(sirt_staff_nuc_mt_01_team, sirt_staff_nuc_mt_01_other))
sirt_post_staff$sirt_staff_nuc_mt_01_team = as.factor(sirt_post_staff$sirt_staff_nuc_mt_01_team)

levels(sirt_post_staff$sirt_staff_nuc_mt_01_team)

sirt_post_staff %>%
  filter(sirt_staff_nuc_mt_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nuc_mt_01_hours),
            cost = sum*Nuc_Med_Tech)



###################### NM_physicist
sirt_post_staff %>%
  summarise(max = max(sirt_staff_nm_ph)) # 1

#1
sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nm_ph_01_team = case_when(
    sirt_staff_nm_ph_01_team %in% 1 ~ "Oncology",
    sirt_staff_nm_ph_01_team %in% 2 ~ "Radiology",
    sirt_staff_nm_ph_01_team %in% 3 ~ "Nuclear medicine",
    sirt_staff_nm_ph_01_team %in% 4 ~ "Anaesthesia"
  ))


sirt_post_staff = sirt_post_staff %>%
  mutate(sirt_staff_nm_ph_01_team = coalesce(sirt_staff_nm_ph_01_team, sirt_staff_nm_ph_01_other))
sirt_post_staff$sirt_staff_nm_ph_01_team = as.factor(sirt_post_staff$sirt_staff_nm_ph_01_team)

levels(sirt_post_staff$sirt_staff_nm_ph_01_team)

sirt_post_staff %>%
  filter(sirt_staff_nm_ph_01_team == "Nuclear medicine") %>%
  summarise(sum = sum(sirt_staff_nm_ph_01_hours),
            cost = sum*NM_physicist)





########################### Labour_nuclear_medicine
sirt_post_staff %>%
  summarise(max = max(sirt_staff_mab_nm)) # 0



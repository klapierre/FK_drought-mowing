################################################################################
##  EEA_fluorescence.R: Analyses examining fluorescence extracellular enzyme activity for Fort Keogh drought x mowing experiment
##
##  Author: Kimberly Komatsu
##  Date created: September 10, 2020
################################################################################

library(openxlsx)
library(tidyverse)


#kim's laptop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\misc co-author papers\\Fort Keogh_drought x mowing\\EEA data')


###read in data
#read in treatment data
trt  <- read.xlsx('SIM plots trt.xlsx')


#read in plate set-up data
plate <- read.xlsx('Drought x Mowing EEA plate setup .xlsx')%>% #reads in the data
  select(processing_date, site, sample_year, sample_month, plot, wet_weight, EEA_weight, plate_num, plate_position, envelope_weight)%>% #selects just the columns we want by name
  full_join(read.xlsx('Drought x Mowing Soil Dry Weights.xlsx'))%>% #import and join on the dry soil weight data
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))%>% #fixes the date to be in yyyy-mm-dd format
  mutate(EEA_dry=(EEA_weight*((dry_weight-envelope_weight)/wet_weight)))%>% #calculate EEA dry weight based on wet to dry conversion for soil mositure
  select(-EEA_weight, -dry_weight, -envelope_weight, -wet_weight)

soilWeight <- plate%>% #creates a dataframe with soil weights for each sample in columns associated with each plate, rather than long-form
  select(processing_date, plate_num, plate_position, EEA_dry)%>%
  mutate(plate_position2=paste('soil_weight',plate_position, sep='_'))%>% #makes a new column that we will use to spread the soil weights for each plate into columns to calculate enzyme activities
  select(-plate_position)%>%
  spread(key=plate_position2, value=EEA_dry)

time <- read.xlsx('Drought x Mowing EEA plate time.xlsx')%>% #creates a dataframe related to the length of time each assay ran
  select(processing_date, plate_num, enzyme, time_hr)%>%
  filter(!(enzyme %in% c('PPO', 'PER')))%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))

fluor <- read.xlsx('Drought x Mowing EEA Fluorescence Data.xlsx')%>% #read in fluorescence data
  select(-notes)%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))%>%
  left_join(time)%>% #join time data
  left_join(soilWeight)%>% #join weight data
  mutate( #calculate enzyme activites for each sample (1-3) within each plate
    #activity 1
    quench1=((quench_control_1-soil_control_1)/standard_control), 
    emmission1=(standard_control/0.5),
    net_fluor1=(((soil_assay_1-soil_control_1)/quench1)-substrate_control),
    activity1=((net_fluor1*125)/(emmission1*0.2*time_hr*soil_weight_1)),
    #activity 2
    quench2=((quench_control_2-soil_control_2)/standard_control), 
    emmission2=(standard_control/0.5),
    net_fluor2=(((soil_assay_2-soil_control_2)/quench2)-substrate_control),
    activity2=((net_fluor2*125)/(emmission2*0.2*time_hr*soil_weight_2)),
    #activity 3
    quench3=((quench_control_3-soil_control_3)/standard_control), 
    emmission3=(standard_control/0.5),
    net_fluor3=(((soil_assay_3-soil_control_3)/quench3)-substrate_control),
    activity3=((net_fluor3*125)/(emmission3*0.2*time_hr*soil_weight_3))
    )%>%
  select(processing_date, enzyme, plate_num, well, activity1, activity2, activity3)%>%
  gather(key='activity_position', value='activity', activity1, activity2, activity3)%>% #gather into long-form
  separate(col=activity_position, into=c('drop', 'plate_position'), sep='y')%>%
  mutate(plate_position=as.numeric(plate_position))%>%
  select(-drop)%>%
  left_join(plate)%>%
  filter(!(is.na(plot)))%>% #filter samples that were not from this experiment
  filter(activity>0)%>% #remove samples where activity was below detection limit (24 observations)
  mutate(flag=ifelse(enzyme=='AP'&activity>1100, 1,
                     ifelse(enzyme=='BG'&activity>350, 1,
                            ifelse(enzyme=='BX'&activity>60, 1,
                                   ifelse(enzyme=='CB'&activity>80, 1,
                                          ifelse(enzyme=='LAP'&activity>50, 1,
                                                 ifelse(enzyme=='NAG'&activity>200, 1, 0)))))))%>%
  filter(flag==0)%>% #filter out wells that were outliers in activity levels (drops 17 observations)
  group_by(site, sample_year, sample_month, plot, enzyme)%>%
  summarise(activity_mean=mean(activity), length=length(activity))%>% #calculate mean activity levels across all technical replicates for each plot
  ungroup() #check length to ensure each plot has at least 5 technical reps (3 plot*enzyme combos have 5 reps, 4 have 6, 24 have 7, remaining 328 have all 8 tech reps)

ggplot(data=fluor, aes(x=activity_mean)) +
  geom_histogram() +
  facet_wrap(~enzyme, scales='free')


fluorTrt <- fluor%>%
  left_join(trt)%>%
  mutate(seas=as.factor(seas), mow=as.factor(mow), precip=as.factor(precip))

rm(list = c('fluor', 'plate', 'soilWeight', 'time', 'trt'))

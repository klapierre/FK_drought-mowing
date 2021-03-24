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
  filter(enzyme %in% c('PPO', 'PER'))%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))

abs <- read.xlsx('Drought x Mowing EEA Absorbance Data.xlsx')%>% #read in absorbance data
  select(-X17)%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))%>%
  left_join(time)%>% #join time data
  left_join(soilWeight)%>% #join weight data
  mutate(substrate_control1=ifelse(substrate_control1>0.1, NA, substrate_control1),
         substrate_control2=ifelse(substrate_control2>0.1, NA, substrate_control2))%>%
  group_by(processing_date, enzyme, plate_num, time_hr, soil_weight_1, soil_weight_2, soil_weight_3)%>%
  summarise_at(vars(substrate_control1:soil_assay_b_3), mean, na.rm=T)%>%
  ungroup()%>%
  mutate( #calculate enzyme activites for each sample (1-3) within each plate
    #activity 1
    NAU1=(mean(c(soil_assay_a_1, soil_assay_b_1)) - soil_control_1 - mean(c(substrate_control1, substrate_control2))), 
    activity1=(NAU1/(7.9*time_hr*0.2*soil_weight_1)),
    #activity 2
    NAU2=(mean(c(soil_assay_a_2, soil_assay_b_2)) - soil_control_2 - mean(c(substrate_control1, substrate_control2))), 
    activity2=(NAU2/(7.9*time_hr*0.2*soil_weight_2)),
    #activity 3
    NAU3=(mean(c(soil_assay_a_3, soil_assay_b_3)) - soil_control_3 - mean(c(substrate_control1, substrate_control2))), 
    activity3=(NAU3/(7.9*time_hr*0.2*soil_weight_3))
    )%>%
  select(processing_date, enzyme, plate_num, activity1, activity2, activity3)%>%
  gather(key='activity_position', value='activity', activity1, activity2, activity3)%>% #gather into long-form
  separate(col=activity_position, into=c('drop', 'plate_position'), sep='y')%>%
  mutate(plate_position=as.numeric(plate_position))%>%
  select(-drop)%>%
  left_join(plate)%>%
  filter(!(is.na(activity))) #filter samples that were not from this experiment

###many values negative (wells with soil and subtrate lighter than wells with soil alone) -- oxidase enzymes not highly active in these soils?

ggplot(data=abs, aes(x=activity)) +
  geom_histogram() +
  facet_wrap(~enzyme, scales='free')


absTrt <- abs%>%
  left_join(trt)%>%
  mutate(seas=as.factor(seas), mow=as.factor(mow), precip=as.factor(precip))

rm(list = setdiff(ls(), "absTrt"))

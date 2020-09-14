################################################################################
##  EEA_fluorescence.R: Analyses examining fluorescence extracellular enzyme activity for Fort Keogh drought x mowing experiment
##
##  Author: Kimberly Komatsu
##  Date created: September 10, 2020
################################################################################

library(openxlsx)
library(performance)
library(tidyverse)


#kim's laptop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\misc co-author papers\\Fort Keogh_drought x mowing\\EEA data')


###set the theme of graphs
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_text(size=20), legend.text=element_text(size=20))


###read in data
#read in treatment data
trt  <- read.xlsx('SIM plots trt.xlsx')

############# SARAH: stop here

#read in plate set-up data
plate <- read.xlsx('Drought x Mowing EEA plate setup .xlsx')%>% #reads in the data
  select(processing_date, site, sample_year, sample_month, plot, wet_weight, EEA_weight, plate_num, plate_position, envelope_weight)%>% #selects just the columns we want by name
  full_join(read.xlsx('Drought x Mowing Soil Dry Weights.xlsx'))%>% #import and join on the dry soil weight data -- be sure to have the same column names (including capitalization) between files!
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))%>% #fixes the date to be in yyyy-mm-dd format
  mutate(EEA_dry=(EEA_weight*((dry_weight-envelope_weight)/wet_weight)))%>% #calculate EEA dry weight based on wet to dry conversion for soil mositure
  select(-EEA_weight, -dry_weight, -envelope_weight, -wet_weight)%>% #note that plots 33 and 35 don't have a dry weight associated with them, and 3 and 55 have two weights associated with them. need to reweigh these
  filter(!(plot %in% c(3, 55, 33, 35))) #get rid of the ones that have messed up dry soil weights for now

soilWeight <- plate%>%
  select(processing_date, plate_num, plate_position, EEA_dry)%>%
  mutate(plate_position2=paste('soil_weight',plate_position, sep='_'))%>% #makes a new column that we will use to spread the soil weights for each plate into columns to calculate enzyme activities
  select(-plate_position)%>%
  spread(key=plate_position2, value=EEA_dry)

time <- read.xlsx('Drought x Mowing EEA plate time.xlsx')%>%
  select(processing_date, plate_num, enzyme, time_hr)%>%
  filter(!(enzyme %in% c('PPO', 'PER')))%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))

fluor <- read.xlsx('Drought x Mowing EEA Fluorescence Data.xlsx')%>%
  select(-notes)%>%
  mutate(processing_date=as.Date(processing_date, origin='1899-12-30'))%>%
  left_join(time)%>%
  left_join(soilWeight)%>% #note that there are some missing soil weights beyond the 4 we dropped. is this because some plates have columns that were GxD and not drought x mowing?
  mutate(
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
  gather(key='activity_position', value='activity', activity1, activity2, activity3)%>%
  separate(col=activity_position, into=c('drop', 'plate_position'), sep='y')%>%
  mutate(plate_position=as.numeric(plate_position))%>%
  select(-drop)%>%
  left_join(plate)%>%
  filter(!(is.na(activity)), activity>0, activity<2000)%>%
  group_by(site, sample_year, sample_month, plot, enzyme)%>%
  summarise(activity_mean=mean(activity))%>%
  ungroup()

ggplot(data=fluor, aes(x=activity_mean)) +
  geom_histogram() +
  facet_wrap(~enzyme, scales='free')


# #dropping outliers
# fluorPlateOutliers <- fluor%>%
#   group_by(processing_date, enzyme, plate_num, plate_position)%>%
#   summarise(plate_mean=mean(activity), plate_sd=sd(activity))%>%
#   ungroup()
# 
# fluorOverallOutliers <- fluor%>%
#   group_by(enzyme)%>%
#   summarise(overall_mean=mean(activity), overall_sd=sd(activity))%>%
#   ungroup()
  


############# SARAH: start here


fluorTrt <- fluor%>%
  left_join(trt)

str(fluorTrt) #tells you what R thinks each column is

ggplot(data=fluorTrt, aes(x=as.factor(precip), y=activity_mean, color=interaction(mow,seas))) +
  geom_boxplot() +
  xlab('Precipitation (%)') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  scale_color_brewer(palette='Paired', name='Mowing') +
  facet_wrap(~enzyme, scales='free') #sarah, no need to include facet_wrap statement


#the plots below are not a great way to look at the data because they look at each trt individually

ggplot(data=fluorTrt, aes(x=as.factor(precip), y=activity_mean)) +
  geom_boxplot() +
  xlab('Precipitation (%)') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  facet_wrap(~enzyme, scales='free') #sarah, no need to include facet_wrap statement

ggplot(data=fluorTrt, aes(x=as.factor(mow), y=activity_mean)) +
  geom_boxplot() +
  xlab('Mowing Height (cm)') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  facet_wrap(~enzyme, scales='free') #sarah, no need to include facet_wrap statement

ggplot(data=fluorTrt, aes(x=as.factor(seas), y=activity_mean)) +
  geom_boxplot() +
  xlab('Mowing Season') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  facet_wrap(~enzyme, scales='free') #sarah, no need to include facet_wrap statement



###running ANOVAs -- sarah, you'll just have to run one ANOVA model for ph and you don't have to do the subset step for the data
apANOVA <- aov(activity_mean ~ as.factor(precip)*as.factor(mow)*seas, data=subset(fluorTrt, enzyme=='AP'))
summary(apANOVA)

summary(bgANOVA <- aov(activity_mean ~ precip*mow*seas, data=subset(fluorTrt, enzyme=='BG')))

summary(bxANOVA <- aov(activity_mean ~ precip*mow*seas, data=subset(fluorTrt, enzyme=='BX')))
summary(bxANOVA <- aov(activity_mean ~ as.factor(mow)*seas, data=subset(fluorTrt, enzyme=='BX'&precip==100)))

summary(cbANOVA <- aov(activity_mean ~ precip*mow*seas, data=subset(fluorTrt, enzyme=='CB')))

summary(lapANOVA <- aov(activity_mean ~ precip*mow*seas, data=subset(fluorTrt, enzyme=='LAP')))

summary(nagANOVA <- aov(activity_mean ~ precip*mow*seas, data=subset(fluorTrt, enzyme=='NAG')))

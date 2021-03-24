################################################################################
##  EEA_analysis.R: Examining effects of mowing and drought on extracellular enzyme activitites.
##
##  Author: Kimberly Komatsu
##  Date created: March 23, 2021
################################################################################

library(performance)
library(tidyverse)

#kim's laptop
setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\misc co-author papers\\Fort Keogh_drought x mowing\\EEA data')


###functions
#function to get means and standard errors for easy bar graphs
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  


###set the theme of graphs
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_text(size=20), legend.text=element_text(size=20))


###source data
source('C:\\Users\\lapie\\Desktop\\R files laptop\\FK_drought-mowing\\EEA_fluorescence.R')
source('C:\\Users\\lapie\\Desktop\\R files laptop\\FK_drought-mowing\\EEA_absorbance.R')


###combining data
activityTrt <- absTrt%>%
  select(site, sample_year, sample_month, plot, enzyme, activity, seas, mow, precip)%>%
  rbind(fluorTrt)

# write.csv(activityTrt, 'Drought x Mowing_EEA_final.csv', row.names=F)

###running ANOVAs
summary(apANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='AP'))) #no significant effects
check_model(apANOVA)

summary(bgANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='BG'))) #significant effect of season of mowing: mowing on October leads to higher activity than mowing in June
check_model(bgANOVA)

summary(bxANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='BX'))) #no significant effects
check_model(bxANOVA)

summary(cbANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='CB'))) #no significant effects
check_model(cbANOVA)

summary(lapANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='LAP'))) #no significant effects
check_model(lapANOVA)

summary(nagANOVA <- aov(activity ~ precip*mow*seas, data=subset(activityTrt, enzyme=='NAG'))) #significant effect of season of mowing: mowing on October leads to higher activity than mowing in June
check_model(nagANOVA)


#figures
ggplot(data=subset(activityTrt, !(enzyme %in% c('PER', 'PPO'))), aes(x=precip, y=activity, color=interaction(mow,seas))) +
  geom_boxplot() +
  xlab('Precipitation (%)') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  scale_color_brewer(palette='Paired', name='Mowing') +
  facet_wrap(~enzyme, scales='free')

ggplot(data=barGraphStats(data=subset(activityTrt, !(enzyme %in% c('PER', 'PPO'))), variable="activity", byFactorNames=c("seas", "enzyme")), aes(x=seas, y=mean)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  xlab('Mowing Season') + ylab('Extracellular Enzyme Activity (nmol/hr/g)') +
  facet_wrap(~enzyme, scales='free')

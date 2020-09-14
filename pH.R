################################################################################
##  pH.R: Analyses examining soil pH for Fort Keogh drought x mowing experiment
##
##  Author: Kimberly Komatsu
##  Date created: September 10, 2020
################################################################################

library(openxlsx)
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
             legend.title=element_blank(), legend.text=element_text(size=20))


###read in data
#read in pH data
pH <- read.xlsx('Drought x Mowing pH data.xlsx')%>%
  left_join(read.xlsx('SIM plots trt.xlsx'))

colnames(pH) #determine column names


###make graphs
#histogram to check normality
ggplot(data=pH, aes(x=pH)) +
  geom_histogram()

#dot plot
ggplot(data=pH, aes(x=precip, y=pH, color=interaction(seas, mow))) +
  geom_point()

#bar graph
ggplot(data=barGraphStats(data=pH, variable="pH", byFactorNames=c("seas", "mow", "precip")), aes(x=as.factor(precip), y=mean, fill=interaction(seas, mow))) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
  scale_fill_brewer(palette='Paired', name='Mowing') +
  xlab('Precipitation Difference (%)') + ylab('Soil pH') +
  theme(legend.title=element_text(size=20))



###statistical test
pHmodel <- aov(pH ~ precip*seas*mow, data=pH) #ANOVA: check with Kurt, but probably the mowing date should be nested within the mowing treatment?
summary(pHmodel) #significant effect of precip
check_model(pHmodel)
pairwise.t.test(pH$pH, pH$precip, p.adj="bonf")

## Bodo Winter
## October 2, 2015
## Stadtlander Etymology investigation


##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## The column 'MainYear' is the age of the adjective itself
## The column 'EtymonYear' is the age of the etymon

## Libraries:

library(dplyr)

## Options:

options(stringsAsFactors = F)

## Load in data:

setwd('/Users/teeniematlock/Desktop/sense_phd/data/')
stadt <- read.csv('stadt_rough_hard_words_oed_watkins_2000.csv')

## Get rid of that on NA ('smooshy' was not found in the OED):

stadt <- stadt[!is.na(stadt$MainYear),]

## Get rid of extra info for MainYear:

stadt$MainYear <- sapply(strsplit(stadt$MainYear, split = '/'), FUN = function(x) x[1])
stadt$MainYear <- gsub('\\?|[a-z]', '', stadt$MainYear)

## Fix these in the Etymon Year column; set '13..' to '1300':

stadt[which(stadt$EtymonYear == '13..'),]$EtymonYear <- '1300'
stadt[65,]$EtymonYear <- '1300'
stadt[stadt$EtymonYear == '14\xc4',]$EtymonYear <- '1400'

## Get rid of extra info for EtymonYear:

stadt$EtymonYear <- sapply(strsplit(stadt$EtymonYear, split = '/'),
	FUN = function(x) x[1])
stadt$EtymonYear <- gsub('\\?|[a-z]', '', stadt$EtymonYear)

## Set earliest age for OE:

stadt[grep('OE', stadt$MainYear), ]$MainYear <- '700'
stadt[grep('OE', stadt$EtymonYear), ]$EtymonYear <- '700'

## There are those that have "-" ... take the mean of those two years:

splitted <- strsplit(stadt[grep('\\-', stadt$EtymonYear), ]$EtymonYear, split = '-')
splitted <- sapply(splitted, FUN = function (x) (as.numeric(x[1]) + as.numeric(x[2])) / 2)
stadt[grep('\\-', stadt$EtymonYear), ]$EtymonYear <- splitted

## Make into numeric:

stadt$MainYear <- as.numeric(stadt$MainYear)
stadt$EtymonYear <- as.numeric(stadt$EtymonYear)

## Combine columns with those specific for adjective and if there's an earlier one, the root age:

stadt$TotalEtymYear <- stadt$MainYear
stadt[which(stadt$EtymonYear < stadt$MainYear), ]$TotalEtymYear <- stadt[which(stadt$EtymonYear < stadt$MainYear), ]$EtymonYear



##------------------------------------------------------------------
## Create /r/ variables:
##------------------------------------------------------------------

## Has_r variable:

stadt$Initial_r <- 'no'
stadt[grep('^r', stadt$Word),]$Initial_r <- 'yes'

stadt$InitialCluster_r <- 'no'
stadt[grep('^[a-z]r', stadt$Word),]$InitialCluster_r <- 'yes'

stadt$Has_r_Initial <- 'no'
stadt[stadt$Initial_r == 'yes' | stadt$InitialCluster_r == 'yes',]$Has_r_Initial <- 'yes'

stadt$Has_r <- 'no'
stadt[grep('r', stadt$Word),]$Has_r <- 'yes'

stadt$Has_r_end <- 'no'
stadt[grep('r$', stadt$Word),]$Has_r_end <- 'yes'

stadt$Has_r_elsewhere <- 'no'
stadt[stadt$Has_r == 'yes' & stadt$InitialCluster_r == 'no' & stadt$Initial_r == 'no' & stadt$Has_r_end == 'no',]$Has_r_elsewhere = 'yes'


##------------------------------------------------------------------
## Create median split variables:
##------------------------------------------------------------------

## For treating this as a classification problem, make roughness and hardness median splits:

stadt$RoughCat <- NA
stadt[which(stadt$RoughnessMean < median(stadt$RoughnessMean, na.rm = T)),]$RoughCat <- 'low'
stadt[which(stadt$RoughnessMean > median(stadt$RoughnessMean, na.rm = T)),]$RoughCat <- 'high'

stadt$HardCat <- NA
stadt[which(stadt$HardnessMean < median(stadt$HardnessMean, na.rm = T)),]$HardCat <- 'low'
stadt[which(stadt$HardnessMean > median(stadt$HardnessMean, na.rm = T)),]$HardCat <- 'high'


##------------------------------------------------------------------
## Create 'consistent addition' variable:
##------------------------------------------------------------------

## For hardness:

hard_R <- stadt$HardCat == 'high' & stadt$Has_r == 'yes'
soft_noR <- stadt$HardCat == 'low' & stadt$Has_r != 'no'
hard_noR <- stadt$HardCat == 'high' & stadt$Has_r == 'no'
soft_R <- stadt$HardCat == 'low' & stadt$Has_r != 'yes'

stadt$HardConsistentAddition <- NA
stadt[which(hard_R | soft_noR),]$HardConsistentAddition <- 'consistent'
stadt[which(hard_noR | soft_R),]$HardConsistentAddition <- 'inconsistent'

## For roughness:

rough_R <- stadt$RoughCat == 'high' & stadt$Has_r == 'yes'
smooth_noR <- stadt$RoughCat == 'low' & stadt$Has_r != 'no'
rough_noR <- stadt$RoughCat == 'high' & stadt$Has_r == 'no'
smooth_R <- stadt$RoughCat == 'low' & stadt$Has_r != 'yes'

stadt$RoughConsistentAddition <- NA
stadt[which(rough_R | smooth_noR),]$RoughConsistentAddition <- 'consistent'
stadt[which(rough_noR | smooth_R),]$RoughConsistentAddition <- 'inconsistent'

## Arrange by year:

stadt <- arrange(stadt, TotalEtymYear)



## Check consistent additions over time:

quartz('', 8, 6)
plot(stadt$TotalEtymYear, jitter(as.numeric(as.factor(stadt$RoughConsistentAddition)), 0.5), pch = 19)
summary(glm(as.factor(RoughConsistentAddition) ~ TotalEtymYear, stadt, family = 'binomial'))



##------------------------------------------------------------------
## Look at data:
##------------------------------------------------------------------

df <- stadt[stadt$TotalEtymYear > 1900,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1800 & stadt$TotalEtymYear < 1900,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1700 & stadt$TotalEtymYear < 1800,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1600 & stadt$TotalEtymYear < 1700,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1500 & stadt$TotalEtymYear < 1600,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1400 & stadt$TotalEtymYear < 1500,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1300 & stadt$TotalEtymYear < 1400,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 1200 & stadt$TotalEtymYear < 1300,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear >= 1000 & stadt$TotalEtymYear < 1200,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 900 & stadt$TotalEtymYear < 1000,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 800 & stadt$TotalEtymYear < 900,]
table(df$Has_r, df$HardCat)

df <- stadt[stadt$TotalEtymYear > 700 & stadt$TotalEtymYear < 800,]
table(df$Has_r, df$HardCat)

all_years <- unique(stadt$TotalEtymYear)

hard_all_props <- numeric(length(all_years))
rough_all_props <- numeric(length(all_years))

for (i in 1:length(all_years)) {
	this_year <- all_years[i]
	this_subset <- stadt[stadt$TotalEtymYear <= this_year,]
	hard_table <- table(this_subset$Has_r, this_subset$HardCat)
	rough_table <- table(this_subset$Has_r, this_subset$RoughCat)
	hard_all_props[i] <- (1 - sum(diag(hard_table)) / sum(hard_table))
	rough_all_props[i] <- (1 - sum(diag(rough_table)) / sum(rough_table))
	}


## Historical trend tlot for proportion of modality matches over time:

setup_plots(N = 1)
emptyplot(xlim = c(700, 2000), ylim = c(0.2, 1.0))
axis(side = 1, cex.axis = 1.25, lwd.ticks = 2, font = 2, at = seq(700, 2100, 175))
mtext(text = 'Year', side = 1, line = 3, font = 2, cex = 1.7)
left_axis(text = 'Proportion of matches', at = seq(0.2, 1.0, 0.2), type = 1)
points(all_years, rough_all_props, type = 'l', lwd = 2)
points(all_years, rough_all_props, lwd = 1, pch = 19, cex = 1.2, col = rgb(0, 0, 0, 0.4))
segments(x0 = all_years, y0 = 0.2, y1 = 0.22)




##------------------------------------------------------------------
## Check Indo-European:
##------------------------------------------------------------------

## Indo-European root /r/ variable:

PIE <- stadt[!is.na(stadt$IE_root_watkins_2000),]
PIE$PIE_r <- 'no_r'
PIE[grep('r', PIE$IE_root_watkins_2000),]$PIE_r <- 'r'

## Check:

table(PIE$RoughCat, PIE$PIE_r)
table(PIE$HardCat, PIE$PIE_r)

chisq.test(table(PIE$RoughCat, PIE$PIE_r))


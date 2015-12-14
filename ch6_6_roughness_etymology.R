## Bodo Winter
## October 2, 2015
## Analysis for Ch. 6.6, 'What explains the association between roughness and /r/?'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## The column 'MainYear' is the age of the adjective itself
## The column 'EtymonYear' is the age of the etymon

## Libraries:

library(dplyr)

## Options:

options(stringsAsFactors = F)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis/'

## Load in plotting functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))

## Load in data:

setwd(file.path(mainPath, 'data'))
stadt <- read.csv('stadtlander_roughness_OED_watkins_2000.csv')

## Get rid of that on NA ('smooshy' was not found in the OED):

stadt <- stadt[!is.na(stadt$MainYear),]

## Get rid of extra info for MainYear:

stadt$MainYear <- sapply(strsplit(stadt$MainYear, split = '/'), FUN = function(x) x[1])
stadt$MainYear <- gsub('\\?|[a-z]', '', stadt$MainYear)

## Fix these in the Etymon Year column; set '13..' to '1300':

stadt[which(stadt$EtymonYear == '13..'),]$EtymonYear <- '1300'

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

## Has_r variable:

stadt$Has_r <- 'no'
stadt[grep('r', stadt$Word),]$Has_r <- 'yes'


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

## For roughness:

rough_R <- stadt$RoughCat == 'high' & stadt$Has_r == 'yes'
smooth_noR <- stadt$RoughCat == 'low' & stadt$Has_r != 'no'
rough_noR <- stadt$RoughCat == 'high' & stadt$Has_r == 'no'
smooth_R <- stadt$RoughCat == 'low' & stadt$Has_r != 'yes'

stadt$RoughConsistentAddition <- NA
stadt[which(rough_R | smooth_noR),]$RoughConsistentAddition <- 'consistent'
stadt[which(rough_noR | smooth_R),]$RoughConsistentAddition <- 'inconsistent'

## For hardness:

hard_R <- stadt$HardCat == 'high' & stadt$Has_r == 'yes'
soft_noR <- stadt$HardCat == 'low' & stadt$Has_r != 'no'
hard_noR <- stadt$HardCat == 'high' & stadt$Has_r == 'no'
soft_R <- stadt$HardCat == 'low' & stadt$Has_r != 'yes'

stadt$HardConsistentAddition <- NA
stadt[which(hard_R | soft_noR),]$HardConsistentAddition <- 'consistent'
stadt[which(hard_noR | soft_R),]$HardConsistentAddition <- 'inconsistent'

## Arrange by year:

stadt <- arrange(stadt, TotalEtymYear)



##------------------------------------------------------------------
## Look at temporal trend:
##------------------------------------------------------------------

## Create empty vectors to store proportions:

all_years <- unique(stadt$TotalEtymYear)
hard_all_props <- numeric(length(all_years))
rough_all_props <- numeric(length(all_years))

## Loop through and add proportions:

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

## Chi-squar test of PIE data:

chisq.test(table(PIE$RoughCat, PIE$PIE_r))




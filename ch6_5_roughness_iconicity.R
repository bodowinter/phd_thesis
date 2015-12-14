## Bodo Winter
## September 25, 2015
## Analysis for Ch. 6.5, 'Sound structure reflects tactile properties'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Set options:

options(stringsAsFactors = F)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis/'

## Load in plotting functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))

## Load in libraries:

library(dplyr)
library(party)
library(diptest)
library(stringr)

## Load in data:

setwd(file.path(mainPath, 'data'))
stadt <- read.csv('stadtlander_murdoch_2000.csv')

## Load in data:

setwd(file.path(mainPath, 'data'))
ELP <- read.csv('ELP_pronunciations.csv')
stadt <- read.csv('stadtlander_murdoch_2000.csv')

## Merge ELP pronunciations into stadtlander dataset:

stadt$Pron <- ELP[match(stadt$Word, ELP$Word), ]$Pron



##------------------------------------------------------------------
## Create median split variables:
##------------------------------------------------------------------
## Generally I disprefer median splis; but the interpretation of the random forest
## algorithm is greatly facilitated when treating roughness/hardness as a 
## classification problem rather than a regression problem

## To test legitimacy of median split:

dip.test(stadt$Roughness)
dip.test(stadt$Hardness)

## For treating this as a classification problem, make roughness and hardness median splits:

stadt$RoughCat <- NA
stadt[which(stadt$Roughness < median(stadt$Roughness, na.rm = T)),]$RoughCat <- 'low'
stadt[which(stadt$Roughness > median(stadt$Roughness, na.rm = T)),]$RoughCat <- 'high'

stadt$HardCat <- NA
stadt[which(stadt$Hardness < median(stadt$Hardness, na.rm = T)),]$HardCat <- 'low'
stadt[which(stadt$Hardness > median(stadt$Hardness, na.rm = T)),]$HardCat <- 'high'



##------------------------------------------------------------------
## Add additional pronunciations by hand:
##------------------------------------------------------------------

## Where possible, the root was identified in the pronunciations
## In addition, MacMillan and Webster were consulted for this
## No strict syllabification has been undertaken (not relevant)

stadt[stadt$Word == 'callused',]$Pron <- 'k"al@st'
stadt[stadt$Word == 'craterous',]$Pron <- 'kr"e.4@`.r@s'
stadt[stadt$Word == 'cushiony',]$Pron <- 'k"US.n=i'
stadt[stadt$Word == 'fiberglass',]$Pron <- 'f"aI.b@r.gl"as'
stadt[stadt$Word == 'formica',]$Pron <- 'f"or.maIk@'
stadt[stadt$Word == 'goopy',]$Pron <- 'g"u.pi'
stadt[stadt$Word == 'grainy',]$Pron <- 'gr"en.i'
stadt[stadt$Word == 'holey',]$Pron <- 'h"oli'
stadt[stadt$Word == 'nonbreakable',]$Pron <- 'n"Vn.br"ek.@.bl='
stadt[stadt$Word == 'pointy',]$Pron <- 'p"OInt.i'
stadt[stadt$Word == 'scrunchy',]$Pron <- 'skr"VntS.i'
stadt[stadt$Word == 'smooshy',]$Pron <- 'sm"VS.i'
stadt[stadt$Word == 'squeezable',]$Pron <- 'skw"iz.@.bl='
stadt[stadt$Word == 'squishy',]$Pron <- 'skw"IS.i'
stadt[stadt$Word == 'wafflish',]$Pron <- 'w"A.flIS'



##------------------------------------------------------------------
## Create a second pronunciation column to make the inter-medial dentals either voiced or voiceless:
##------------------------------------------------------------------

## Duplicate pronunciation variable:

stadt$Pron2 <- stadt$Pron 

## Replace pronunciations:

stadt[stadt$Pron2 == 'b"i4.@d',]$Pron2 <- 'b"id.@d'
stadt[stadt$Pron2 == 'br"I.4l=',]$Pron2 <- 'br"I.tl='
stadt[stadt$Pron2 == 'k"o4.@d',]$Pron2 <- 'k"ot.@d'
stadt[stadt$Pron2 == 'k"Vm.f@`4.@.bl=',]$Pron2 <- 'k"Vm.f@`t.@.bl='
stadt[stadt$Pron2 == 'k@`.r"o4.@d',]$Pron2 <- 'k@`.r"od.@d'
stadt[stadt$Pron2 == 'k"V.4l=.i',]$Pron2 <- 'k"V.dl=.i'
stadt[stadt$Pron2 == 'd"En4.@d',]$Pron2 <- 'd"Ent.@d'
stadt[stadt$Pron2 == 'dIs.dZ"OIn4.@d',]$Pron2 <- 'dIs.dZ"OInt.@d'
stadt[stadt$Pron2 == 'dZ@.l"a.4@.n@s',]$Pron2 <- 'dZ@.l"a.t@.n@s'
stadt[stadt$Pron2 == 'l"u.br@.k%e4.@d',]$Pron2 <- 'l"u.br@.k%et.@d'
stadt[stadt$Pron2 == 'm"a4.@d',]$Pron2 <- 'm"at.@d'
stadt[stadt$Pron2 == 'p"en4.@d',]$Pron2 <- 'p"ent.@d'
stadt[stadt$Pron2 == 'p"3`.f@`.r%e4.@d',]$Pron2 <- 'p"3`.f@`.r%et.@d'
stadt[stadt$Pron2 == 's"an4.i',]$Pron2 <- 's"and.i'
stadt[stadt$Pron2 == 'v"El.v@4.i',]$Pron2 <- 'v"El.v@t.i'
stadt[stadt$Pron2 == 'sI.r"e4.@d',]$Pron2 <- 'sI.r"et.@d'
stadt[stadt$Pron2 == 't"En.4@`',]$Pron2 <- 't"En.d@`'
stadt[stadt$Pron2 == 'n"I4.@d',]$Pron2 <- 'n"It.@d'
stadt[stadt$Pron2 == 'kr"e.4@`.r@s',]$Pron2 <- 'kr"e.t@`.r@s'



##------------------------------------------------------------------
## Extract phonemic features: single phonemes
##------------------------------------------------------------------

## Function for getting matrix of consonants:

get_consonants <- function(string) {
	all_res <- c()
	patterns <- c('T', 'D', 'f', 'v', 'p', 't[^S]|t$',
		'k', 'b', 'd[^Z]|d$', 'g', 'S', 'Z|dZ', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')
		# Z|dZ are together since they are not differentiated for the non-words either
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- c('T', 'D', 'f', 'v', 'p', 't',
		'k', 'b', 'd', 'g', 'S', 'Z', 's', 'z', 'r', 'l',
		'm', 'n', 'N', 'w', 'j', 'h', 'tS')

	return(all_res)
	}

## Function for getting matrix of vowels:

get_vowels <- function(string) {
	all_res <- c()
	patterns <- c('@', 'e', 'I', 'u', 'O', 'A',
		'i', 'aI', 'V', 'a', 'o', 'E', 'U', '3', 'OI')
	
	for (i in 1:length(patterns)) {
		all_res <- cbind(all_res,
			str_count(string, patterns[i]))
		}

	colnames(all_res) <- c('AT', 'e', 'I', 'u', 'O', 'A',
		'i', 'aI', 'V', 'a', 'o', 'E', 'U', 'ET', 'OI')

	return(all_res)	
	}

## Extract 'em:

stadt <- cbind(stadt,
	get_consonants(stadt$Pron2),
	get_vowels(stadt$Pron2))



##------------------------------------------------------------------
## Extract subsets without NA's and transform variables:
##------------------------------------------------------------------

## Split up:

rough <- stadt[!is.na(stadt$Roughness),]
hard <- stadt[!is.na(stadt$Hardness),]

## 'blunt' is exactly at median roughness - it's safe to exclude this (rather than coding it either or):

rough <- rough[!is.na(rough$RoughCat),]

## 'hairy' and 'canvas' are exactly at median roughness:

hard <- hard[!(hard$Word %in% c('canvas','hairy')),]

## Make into factors:

rough$RoughCat <- as.factor(rough$RoughCat)
hard$HardCat <- as.factor(hard$HardCat)



##------------------------------------------------------------------
## Random forests preparation: Predictor formulas and helper function:
##------------------------------------------------------------------

## Construct formula pieces with predictors:

phoneme_predictors <- paste(colnames(dplyr::select(stadt, T:OI)),
	collapse = ' + ')

## Construct formulas for roughness:

rough_phoneme_formula <- as.formula(paste('RoughCat', phoneme_predictors, sep = ' ~ '))

## Construct formulas for hardness:

hard_phoneme_formula <- as.formula(paste('HardCat', phoneme_predictors, sep = ' ~ '))

## Function for extracting predictions out of forest:

rf_pred <- function(myforest) {
	mypreds <- treeresponse(myforest)
	mypreds <- sapply(mypreds, FUN = function(x) x[1])
	mypreds[mypreds > 0.5] <- c('high')
	mypreds[mypreds <= 0.5] <- c('low')
	return(mypreds)
	}



##------------------------------------------------------------------
## For exploration, plot conditional inference tree:
##------------------------------------------------------------------

## Roughness:

rough_ctree_phoneme <- ctree(rough_phoneme_formula, rough)

## Make plots of this:

plot(rough_ctree_phoneme)

## Hardness:

hard_ctree_phoneme <- ctree(hard_phoneme_formula, hard)

## Make plots of this:

plot(hard_ctree_phoneme)



##------------------------------------------------------------------
## Random forests:
##------------------------------------------------------------------

## Forest settings:

data.controls <- cforest_unbiased(ntree = 3000, mtry = 10)	# 10 coz 100 variables

## Build the forest:

set.seed(42)
rough_forest_phoneme <- cforest(rough_phoneme_formula,
	data = rough, controls = data.controls)
hard_forest_phoneme <- cforest(hard_phoneme_formula,
	data = hard, controls = data.controls)

## Get variable importances:

rough_varimp_phoneme <- varimp(rough_forest_phoneme, conditional = T)
hard_varimp_phoneme <- varimp(hard_forest_phoneme, conditional = T)

## Retrieve predictions:

rough_res_phoneme <- table(rough$RoughCat, rf_pred(rough_forest_phoneme))
hard_res_phoneme <- table(hard$HardCat, rf_pred(hard_forest_phoneme))

## Get accuracy scores:

sum(diag(rough_res_phoneme)) / nrow(rough)
sum(diag(hard_res_phoneme)) / nrow(hard)



##------------------------------------------------------------------
## Plot of random forest variable importances:
##------------------------------------------------------------------

## Make a nice variable importance plot of just the phonemes:

rough_sorted_varimps <- sort(rough_varimp_phoneme, decreasing = F)
rough_IPA_labels <- names(rough_sorted_varimps)
rough_IPA_labels[rough_IPA_labels == 'I'] <- 'ɪ'
rough_IPA_labels[rough_IPA_labels == 'i'] <- 'iː'
rough_IPA_labels[rough_IPA_labels == 'E'] <- 'eː'
rough_IPA_labels[rough_IPA_labels == 'V'] <- 'ʌ'
rough_IPA_labels[rough_IPA_labels == 'a'] <- 'æ'
rough_IPA_labels[rough_IPA_labels == 'u'] <- 'uː'
rough_IPA_labels[rough_IPA_labels == 'aI'] <- 'aɪ'

## Select first 10:

rough_IPA_labels <- rev(rev(rough_IPA_labels)[1:9])
rough_sorted_varimps <- rev(rev(rough_sorted_varimps)[1:9])

## Make a nice variable importance plot of just the phonemes:

hard_sorted_varimps <- sort(hard_varimp_phoneme, decreasing = F)
hard_IPA_labels <- names(hard_sorted_varimps)
hard_IPA_labels[hard_IPA_labels == 'I'] <- 'ɪ'
hard_IPA_labels[hard_IPA_labels == 'OI'] <- 'ɔɪ'
hard_IPA_labels[hard_IPA_labels == 'S'] <- 'ʃ'
hard_IPA_labels[hard_IPA_labels == 'A'] <- 'ɑː'
hard_IPA_labels[hard_IPA_labels == 'i'] <- 'iː'
hard_IPA_labels[hard_IPA_labels == 'E'] <- 'eː'
hard_IPA_labels[hard_IPA_labels == 'V'] <- 'ʌ'
hard_IPA_labels[hard_IPA_labels == 'a'] <- 'æ'
hard_IPA_labels[hard_IPA_labels == 'u'] <- 'uː'
hard_IPA_labels[hard_IPA_labels == 'aI'] <- 'aɪ'

## Select first 10:

hard_IPA_labels <- rev(rev(hard_IPA_labels)[1:9])
hard_sorted_varimps <- rev(rev(hard_sorted_varimps)[1:9])

## Roughness / hardness double plot:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
# Plot 1:
emptyplot(xlim = c(-0.01, 0.12), ylim = c(0.5, 9.5))
axis(side = 2, at = 1:9, labels = rough_IPA_labels, las = 2, font = 2, cex.axis = 1.35, lwd = 3)
axis(side = 1, at = seq(0, 0.12, 0.02), cex.axis = 1.25, lwd = 3, font = 2)
abline(v = 0, lty = 2)
abline(h = 1:9, lty = 1, col = 'darkgrey')
points(rough_sorted_varimps, 1:9, pch = 19)
mtext(side = 2, text = 'Phonemes', line = 3.6, font = 2, cex = 1.8)
mtext(side = 1, text = 'Relative Importance',
	line = 3, font = 2, cex = 1.5)
mtext(side = 3, text = 'Roughness', line = 1, font = 2, cex = 1.8)
box(lwd = 3)
# Plot 2:
emptyplot(xlim = c(-0.01, 0.12), ylim = c(0.5, 9.5))
axis(side = 4, at = 1:9, labels = hard_IPA_labels, las = 2, font = 2, cex.axis = 1.35, lwd = 3)
axis(side = 1, at = seq(0, 0.12, 0.02), cex.axis = 1.25, lwd = 3, font = 2)
abline(v = 0, lty = 2)
abline(h = 1:9, lty = 1, col = 'darkgrey')
points(hard_sorted_varimps, 1:9, pch = 19)
mtext(side = 1, text = 'Relative Importance',
	line = 3, font = 2, cex = 1.5)
mtext(side = 3, text = 'Hardness', line = 1, font = 2, cex = 1.8)
box(lwd = 3)



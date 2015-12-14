## Bodo Winter
## November 27, 2015
## Analysis for Ch. 5.2, 'Words for roughness/hardness'

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

## Load in data:

setwd(file.path(mainPath, 'data'))
stadt <- read.csv('stadtlander_murdoch_2000.csv')



##------------------------------------------------------------------
## Correlating roughness and hardness:
##------------------------------------------------------------------

## Take a subset for which both norms exist:

both <- stadt[!is.na(stadt$Roughness) & !is.na(stadt$Hardness),]
nrow(both)	# 59

## Correlate:

summary(lm(Roughness ~ Hardness, both))	# R^2 = 63%!
with(both, cor.test(Roughness, Hardness))	# r = 0.79



##------------------------------------------------------------------
## Load in and pre-process valence norms:
##------------------------------------------------------------------

## Load in other datasets used in this particular analysis:

aff <- read.csv('warriner_2013_affective_norms.csv')

## Compute absolute valence score:

aff <- mutate(aff, AbsV = abs(Val - mean(Val)))

## Merge valence into stadt:

stadt$Val <- aff[match(stadt$Word, aff$Word),]$Val
stadt$AbsV <- aff[match(stadt$Word, aff$Word),]$AbsV

## Load in Senti Wordnet:

sent <- read.csv('sentiwordnet_3.0.csv')

## Loop through each Lynott and Connell (2009) term and retrieve the corresponding senti wordnet entries:

stadt$PosScore <- NA
stadt$NegScore <- NA
for (i in 1:nrow(stadt)) {
	this_word <- stadt[i,]$Word
	this_regex <- paste(paste0(this_word, '$'), paste0(this_word, ' '), sep = '|')
	these_sent <- grep(this_regex, sent$SynsetTerms)
	if (length(these_sent) > 0) {
		
		means <- colMeans(sent[these_sent,c('PosScore', 'NegScore')])
		stadt[i,]$PosScore <- means[1]
		stadt[i,]$NegScore <- means[2]
		
		}

	if (i %% 100 == 0) {
		cat(paste0(i, '\n'))
		}
	}

## Create valence difference score:

stadt <- mutate(stadt, PosDiff = PosScore - NegScore)

## Load in NRC Hashtag data:

hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Colnames:

hash <- rename(hash, Word = V1, SentimentScore = V2, NumPos = V3, NumNeg = V4)

## Create absolute valence score:

hash <- mutate(hash, AbsSent = abs(SentimentScore - mean(SentimentScore, na.rm = T)))

## Merge:

stadt$Sent <- hash[match(stadt$Word, hash$Word), ]$SentimentScore
stadt$AbsSent <- hash[match(stadt$Word, hash$Word), ]$AbsSent

## Check overlap:

sum(!is.na(stadt$Val))
sum(!is.na(stadt$Val)) / nrow(stadt)

sum(!is.na(stadt$PosScore))
sum(!is.na(stadt$PosScore)) / nrow(stadt)

sum(!is.na(stadt$Sent))
sum(!is.na(stadt$Sent)) / nrow(stadt)



##------------------------------------------------------------------
## Analysis of valence:
##------------------------------------------------------------------

## Stadtlander without slimy:

noslime <- stadt[stadt$Word != 'slimy',]

## Correlate warriner valence with roughness and hardness:

summary(warr.rough <- lm(Val ~ Roughness, stadt))
summary(warr.hard <- lm(Val ~ Hardness, stadt))

## Correlate senti valence with roughness and hardness:

summary(senti.rough <- lm(PosDiff ~ Roughness, stadt))
summary(senti.hard <- lm(PosDiff ~ Hardness, stadt))

## Correlate senti valence with roughness and hardness:

summary(hash.rough <- lm(Sent ~ Roughness, stadt))
summary(hash.hard <- lm(Sent ~ Hardness, stadt))

## Make a plot of the Warriner data:

rough_newdata <- data.frame(Roughness = seq(-7, 7, 0.01))
rough_newdata$fit <- predict(warr.rough, rough_newdata)
rough_newdata$UB <- rough_newdata$fit + 1.96 * predict(warr.rough, rough_newdata, se.fit = T)$se.fit
rough_newdata$LB <- rough_newdata$fit - 1.96 * predict(warr.rough, rough_newdata, se.fit = T)$se.fit

hard_newdata <- data.frame(Hardness = seq(-7, 7, 0.01))
hard_newdata$fit <- predict(warr.hard, hard_newdata)
hard_newdata$UB <- hard_newdata$fit + 1.96 * predict(warr.hard, hard_newdata, se.fit = T)$se.fit
hard_newdata$LB <- hard_newdata$fit - 1.96 * predict(warr.hard, hard_newdata, se.fit = T)$se.fit

## Make a plot of this:

setup_plots(N = 2)
# plot 1
emptyplot(xlim = c(-7, 7), ylim = c(0, 9), AB = '(a)')
left_axis(text = 'Valence', at = seq(0, 9, 1), type = 2)
lower_axis(style = 'continuous', at = seq(-7, 7, 3.5),
	lab = 'Roughness Ratings', type = 1, labels = c('-7', '-3.5', '0', '+3.5', '+7'))
polygon(c(rough_newdata$Roughness, rev(rough_newdata$Roughness)),
	c(rough_newdata$UB, rev(rough_newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(rough_newdata$Roughness, rough_newdata$fit, lwd = 2, type = 'l')
points(stadt$Roughness, stadt$Val, pch = 19,
	cex = 0.7, col = rgb(0, 0, 0, 0.9))
# plot 2
emptyplot(xlim = c(-7, 7), ylim = c(0, 9), AB = '(b)')
lower_axis(style = 'continuous', at = seq(-7, 7, 3.5),
	lab = 'Hardness Ratings', type = 1, labels = c('-7', '-3.5', '0', '+3.5', '+7'))
polygon(c(hard_newdata$Hardness, rev(hard_newdata$Hardness)),
	c(hard_newdata$UB, rev(hard_newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(hard_newdata$Hardness, hard_newdata$fit, lwd = 2, type = 'l')
points(stadt$Hardness, stadt$Val, pch = 19,
	cex = 0.7, col = rgb(0, 0, 0, 0.9))



##------------------------------------------------------------------
## Computing context valence:
##------------------------------------------------------------------

## Load in COCA:

stadt_COCA <- read.csv('stadtlander_murdoch_2000_COCA_context.csv')

## Merge Warriner valence into the COCA data frame:

stadt_COCA$Val <- aff[match(adj$Noun, aff$Word),]$Val

## Merge Twitter Emotion Corpus data into it:

stadt_COCA$Sent <- hash[match(adj$Noun, hash$Word), ]$SentimentScore

## Loop through each noun and retrieve the corresponding senti wordnet entries:

all_nouns <- unique(stadt_COCA$Noun)
all_nouns <- data.frame(Noun = all_nouns)
all_nouns$PosScore <- NA
all_nouns$NegScore <- NA
for (i in 1:nrow(all_nouns)) {
	this_word <- all_nouns[i,]$Noun
	this_regex <- paste(paste0(this_word, '$'), paste0(this_word, ' '), sep = '|')
	these_sent <- grep(this_regex, sent$SynsetTerms)

	if (length(these_sent) > 0) {
		
		means <- colMeans(sent[these_sent,c('PosScore', 'NegScore')])
		all_nouns[i,]$PosScore <- means[1]
		all_nouns[i,]$NegScore <- means[2]
		
		}

	if (i %% 100 == 0) {
		cat(paste0(i, '\n'))
		}
	}

## Difference score:

all_nouns <- mutate(all_nouns, PosDiff = PosScore - NegScore)

## Add to adj dataframe:

stadt_COCA$PosDiff <- all_nouns[match(adj$Noun, all_nouns$Noun), ]$PosDiff

## Compute weights:

stadt_freqs <- aggregate(Freq ~ Word, stadt_COCA, sum)
stadt_COCA$AdjFreq <- stadt_freqs[match(stadt_COCA$Word, stadt_freqs$Word),]$Freq
stadt_COCA$Weight <- stadt_COCA$Freq / stadt_COCA$AdjFreq

## Take frequency-weighted averages:

stadt_agr <- summarise(group_by(stadt_COCA, Word),
	Val = weighted.mean(Val, na.rm = T, w = Weight),
	Sent = mean(Sent, na.rm = T, w = Weight),
	PosDiff = weighted.mean(PosDiff, na.rm = T, w = Weight))

## Merge roughness and hardness into this:

stadt_agr$Roughness <- stadt[match(stadt_agr$Word, stadt$Word), ]$Roughness
stadt_agr$Hardness <- stadt[match(stadt_agr$Word, stadt$Word), ]$Hardness



##------------------------------------------------------------------
## Analysis of context valence by roughness:
##------------------------------------------------------------------

## Load in SUBTLEX POS tags:

SUBTL <- read.csv('SUBTLEX_US.csv')

## Merge Dominant POS into stadt_agr:

stadt_agr$POS <- SUBTL[match(stadt_agr$Word, SUBTL$Word),]$Dom_PoS_SUBTLEX

## Reduce to those that are adjectives (since we are looking at adj-noun pairs):

stadt_red <- filter(stadt_agr, POS == 'Adjective')

## Correlate warriner valence with roughness and hardness:

summary(warr.rough <- lm(Val ~ Roughness, stadt_red))
summary(warr.hard <- lm(Val ~ Hardness, stadt_red))

## Correlate senti valence with roughness and hardness:

summary(senti.rough <- lm(PosDiff ~ Roughness, stadt_red))
summary(senti.hard <- lm(PosDiff ~ Hardness, stadt_red))

## Correlate senti valence with roughness and hardness:

summary(hash.rough <- lm(Sent ~ Roughness, stadt_red))
summary(hash.hard <- lm(Sent ~ Hardness, stadt_red))

## Make a plot of the Warriner data:

rough_newdata <- data.frame(Roughness = seq(-7, 7, 0.01))
rough_newdata$fit <- predict(hash.rough, rough_newdata)
rough_newdata$UB <- rough_newdata$fit + 1.96 * predict(warr.rough, rough_newdata, se.fit = T)$se.fit
rough_newdata$LB <- rough_newdata$fit - 1.96 * predict(warr.rough, rough_newdata, se.fit = T)$se.fit

hard_newdata <- data.frame(Hardness = seq(-7, 7, 0.01))
hard_newdata$fit <- predict(hash.hard, hard_newdata)
hard_newdata$UB <- hard_newdata$fit + 1.96 * predict(warr.hard, hard_newdata, se.fit = T)$se.fit
hard_newdata$LB <- hard_newdata$fit - 1.96 * predict(warr.hard, hard_newdata, se.fit = T)$se.fit

## Make a plot of this:

setup_plots(N = 2)
# plot 1
emptyplot(xlim = c(-7, 7), ylim = c(-0.5, 1), AB = '(a)')
left_axis(text = 'Context Valence', at = seq(-0.5, 1.5, 0.5), type = 1)
lower_axis(style = 'continuous', at = seq(-7, 7, 3.5),
	lab = 'Roughness Ratings', type = 1, labels = c('-7', '-3.5', '0', '+3.5', '+7'))
polygon(c(rough_newdata$Roughness, rev(rough_newdata$Roughness)),
	c(rough_newdata$UB, rev(rough_newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(rough_newdata$Roughness, rough_newdata$fit, lwd = 2, type = 'l')
points(stadt_red$Roughness, stadt_red$Sent, pch = 19,
	cex = 0.7, col = rgb(0, 0, 0, 0.9))
# plot 2
emptyplot(xlim = c(-7, 7), ylim = c(-0.5, 1), AB = '(b)')
lower_axis(style = 'continuous', at = seq(-7, 7, 3.5),
	lab = 'Hardness Ratings', type = 1, labels = c('-7', '-3.5', '0', '+3.5', '+7'))
polygon(c(hard_newdata$Hardness, rev(hard_newdata$Hardness)),
	c(hard_newdata$UB, rev(hard_newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(hard_newdata$Hardness, hard_newdata$fit, lwd = 2, type = 'l')
points(stadt_red$Hardness, stadt_red$Sent, pch = 19,
	cex = 0.7, col = rgb(0, 0, 0, 0.9))




##------------------------------------------------------------------
## Analysis of context valence, including sense counts:
##------------------------------------------------------------------

## Load in sense counts:

setwd(file.path(mainPath, 'data'))
senses <- read.csv('wordnet_macmillan_senses.csv')
stadt_agr <- cbind(stadt_agr, senses[match(stadt_agr$Word, senses$Word), c('WordNet', 'MacMillan')])

## Create absolute roughness and hardness measures:

stadt_agr$AbsRoughness <- abs(stadt_agr$Roughness)
stadt_agr$AbsHardness <- abs(stadt_agr$Hardness)

## First of all, check absolute valence as a function of sense score:

library(MASS)
summary(wn.rough <- glm.nb(WordNet ~ AbsRoughness, stadt_agr[stadt_agr$Word != 'flat',]))
summary(wn.hard <- glm.nb(WordNet ~ AbsHardness, stadt_agr[stadt_agr$Word != 'clean',]))

summary(mac.rough <- glm.nb(MacMillan ~ AbsRoughness, stadt_agr[stadt_agr$Word != 'flat',]))
summary(mac.hard <- glm.nb(MacMillan ~ AbsHardness, stadt_agr[stadt_agr$Word != 'clean',]))

## Perform likelihood ratio tests:

anova(wn.rough)
anova(wn.hard)

anova(mac.rough)
anova(mac.hard)

## Get predictions for plot:

wn.rough.pred <- as.data.frame(predict.glm(wn.rough,
	newdata = data.frame(AbsRoughness = seq(0, 7, 0.1)),
	se.fit = T, type = 'response')[1:2])
wn.hard.pred <- as.data.frame(predict.glm(wn.hard,
	newdata = data.frame(AbsHardness = seq(0, 7, 0.1)),
	se.fit = T, type = 'response')[1:2])

## Add upper and lower confidence bands:

wn.rough.pred$UB <- wn.rough.pred$fit + 1.96 * wn.rough.pred$se.fit
wn.rough.pred$LB <- wn.rough.pred$fit - 1.96 * wn.rough.pred$se.fit

wn.hard.pred$UB <- wn.hard.pred$fit + 1.96 * wn.hard.pred$se.fit
wn.hard.pred$LB <- wn.hard.pred$fit - 1.96 * wn.hard.pred$se.fit

## Define points to show text for:

hardness_points <- c('hard', 'soft', 'solid', 'tender', 'sharp', 'tough', 'stiff', 'crisp', 'brittle')
roughness_points <- c('rough', 'smooth', 'firm', 'broken', 'slick', 'fine', 'crisp', 'blunt', 'woolly')
hard_words <- stadt_agr[stadt_agr$Word %in% hardness_points,]
rough_words <- stadt_agr[stadt_agr$Word %in% roughness_points,]

## Make a plot of this:

lower_factor = 1
setup_plots(N = 2)
emptyplot(xlim = c(-0.5, 7.5), ylim = c(0, 22), AB = '(a)')
points(seq(0, 7, 0.1), wn.rough.pred$fit, type = 'l', lwd = 3)
polygon(x = c(seq(0, 7, 0.1), rev(seq(0, 7, 0.1))),
	y = c(wn.rough.pred$UB, rev(wn.rough.pred$LB)), col = rgb(0, 0, 0, 0.4), border = NA)
points(stadt_agr$AbsRoughness, stadt_agr$WordNet, pch = 19, cex = 1.05, col = rgb(0, 0, 0, 0.65))
text(rough_words$AbsRoughness, rough_words$WordNet - lower_factor, labels = rough_words$Word, font = 2)
left_axis(text = 'Dictionary Meanings', at = seq(0, 20, 5), type = 2)
lower_axis(style = 'continuous', at = seq(0, 7, 1), lab = '', type = 1)
mtext(side = 1, text = 'Absolute Roughness', line = 3, font = 2, cex = 1.5)
emptyplot(xlim = c(-0.5, 7.5), ylim = c(0, 22), AB = '(b)')
polygon(x = c(seq(0, 7, 0.1), rev(seq(0, 7, 0.1))),
	y = c(wn.hard.pred$UB, rev(wn.hard.pred$LB)), col = rgb(0, 0, 0, 0.4), border = NA)
text(hard_words$AbsHardness, hard_words$WordNet - lower_factor, labels = hard_words$Word, font = 2)
points(seq(0, 7, 0.1), wn.hard.pred$fit, type = 'l', lwd = 3)
points(stadt_agr$AbsHardness, stadt_agr$WordNet, pch = 19, cex = 1.05, col = rgb(0, 0, 0, 0.65))
axis(side = 1, cex.axis = 1.25, lwd.ticks = 2, font = 2, at = seq(0, 7, 1), labels = T)
lower_axis(style = 'continuous', at = seq(0, 7, 1), lab = '', type = 1)
mtext(side = 1, text = 'Absolute Hardness', line = 3, font = 2, cex = 1.5)
# for the hardness plot, the word 'clean' is not shown; for the roughness plot, the word 'flat' is not shown




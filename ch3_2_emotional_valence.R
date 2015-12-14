## Bodo Winter
## September 17, 2015
## Analysis for Ch. 3.2, 'Characterizing the odor and taste lexicon'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis/'

## Load in plotting functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))
source(file.path(mainPath, 'functions/model_prediction_functions.R'))

## Load in modality norms:

setwd(file.path(mainPath, 'data'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2015_verb_norms.csv')

## Order factors for plotting:

modalities <- c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')
l$DominantModality <- factor(l$DominantModality, levels = modalities)
n$DominantModality <- factor(n$DominantModality, levels = modalities)
v$DominantModality <- factor(v$DominantModality, levels = modalities)

## Extract random subset from the verbs:

v <- filter(v, RandomSet == 'yes')

## Combine all:

xall <- rbind(dplyr::select(l, Word, DominantModality),
	dplyr::select(n, Word, DominantModality),
	dplyr::select(v, Word, DominantModality))



##------------------------------------------------------------------
## Load in Warriner et al. (2013) norms:
##------------------------------------------------------------------

## Load in other datasets used in this particular analysis:

aff <- read.csv('warriner_2013_affective_norms.csv')

## Compute absolute valence score:

aff <- mutate(aff, AbsV = abs(Val - mean(Val)))

## Merge valence into xall:

xall$Valence <- aff[match(xall$Word, aff$Word),]$Val
xall$AbsV <- aff[match(xall$Word, aff$Word),]$AbsV

## Check overlap:

sum(!is.na(xall$Valence)) / nrow(xall)

## Merge POS into there:

xall$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)),
	rep('verb', nrow(v)))



##------------------------------------------------------------------
## NRC Hashtag Emotion lexicon:
##------------------------------------------------------------------

## Load in NRC Hashtag data:

hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Colnames:

hash <- rename(hash, Word = V1, SentimentScore = V2, NumPos = V3, NumNeg = V4)

## Create absolute valence score:

hash <- mutate(hash, AbsSent = abs(SentimentScore - mean(SentimentScore, na.rm = T)))

## Merge:

xall$Sent <- hash[match(xall$Word, hash$Word), ]$SentimentScore
xall$AbsSent <- hash[match(xall$Word, hash$Word), ]$AbsSent

## Check overlap:

sum(!is.na(xall$Sent)) / nrow(xall)	# 85%



##------------------------------------------------------------------
## Senti Wordnet analysis & Lynott & Connell (2009):
##------------------------------------------------------------------

## Load in Senti Wordnet:

sent <- read.csv('sentiwordnet_3.0.csv')

## Loop through each Lynott and Connell (2009) term and retrieve the corresponding senti wordnet entries:

xall$PosScore <- NA
xall$NegScore <- NA
xall$PosScoreSD <- NA
xall$NegScoreSD <- NA
for (i in 1:nrow(xall)) {
	this_word <- xall[i,]$Word
	this_regex <- paste(paste0(this_word, '$'), paste0(this_word, ' '), sep = '|')
	these_sent <- grep(this_regex, sent$SynsetTerms)
	if (length(these_sent) > 0) {
		
		means <- colMeans(sent[these_sent,c('PosScore', 'NegScore')])
		sds <- apply(sent[these_sent,c('PosScore', 'NegScore')], 2, sd)
		xall[i,]$PosScore <- means[1]
		xall[i,]$NegScore <- means[2]

		}

	if (i %% 100 == 0) {
		cat(paste0(i, '\n'))
		}
	}

## Check overlap:

sum(is.na(xall$PosScore))
1 - (sum(is.na(xall$PosScore)) / nrow(l))	# 76%

## Compute valence measures:

xall <- mutate(xall, PosDiff = PosScore - NegScore)
xall$ValMax <- apply(xall[,c('PosScore', 'NegScore')], 1, max)



##------------------------------------------------------------------
## Analysis of Warriner et al. (2013) data:
##------------------------------------------------------------------

## Valence:

summary(val.mdl <- lm(Valence ~ DominantModality, xall))
anova(val.mdl)

## Absolute valence:

summary(abs.mdl <- lm(AbsV ~ DominantModality, xall))
anova(abs.mdl)

## Post-hoc tests:

t.test(Valence ~ DominantModality, filter(xall, DominantModality %in% c('Gustatory', 'Olfactory')),
	var.equal = T)

## Extract predictions:

val.pred <- my.predict.lm(val.mdl)
absv.pred <- my.predict.lm(abs.mdl)

## Make a plot of this:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
## Plot 1:
emptyplot(xlim = c(0.5, 5.5), ylim = c(3.5, 6.5), AB = '(a)')
draw_preds(val.pred)
lower_axis(N = xall$DominantModality, type = 2)
top_labels(first_text = 'Warriner et al. (2013)', second_text = '', type = 2)
left_axis(text = 'Valence', at = seq(3.5, 6.5, 1), type = 1)
## Plot 2:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0.8, 2.2), AB = '(b)')
draw_preds(absv.pred)
lower_axis(N = xall$DominantModality, type = 2)
mtext(side = 4, text = 'Absolute Valence', line = 3.5, font = 2, cex = 2)
axis(side = 4, at = seq(0.8, 2.2, 0.4), lwd = 2, font = 2, cex.axis = 1.5, las = 2)
top_labels(first_text = 'Warriner et al. (2013)', second_text = '', type = 2)



##------------------------------------------------------------------
## Analysis of Twitter Emotion Corpus data:
##------------------------------------------------------------------

## Analysis of sentiment score:

summary(xmdl.val <- lm(Sent ~ DominantModality, xall))
anova(xmdl.val)

## Analysis of absolute sentiment score:

summary(xmdl.abs <- lm(AbsSent ~ DominantModality, xall))
anova(xmdl.abs)

## Get predictions:

val.pred <- my.predict.lm(xmdl.val)
abs.pred <- my.predict.lm(xmdl.abs)

## Make a plot of this:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
## Plot 1:
emptyplot(xlim = c(0.5, 5.5), ylim = c(-0.8, 1.0), AB = '(a)')
draw_preds(val.pred)
lower_axis(N = xall$DominantModality, type = 2)
left_axis(text = 'Valence', at = seq(-0.8, 1, 0.4), type = 1)
top_labels(first_text = 'TEC lexicon', second_text = '', type = 2)
## Plot 2:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 1.5), AB = '(b)')
draw_preds(abs.pred)
lower_axis(N = xall$DominantModality, type = 2)
mtext(side = 4, text = 'Absolute Valence', line = 4.2, font = 2, cex = 2)
axis(side = 4, at = seq(0, 1.5, 0.25), lwd = 2, font = 2, cex.axis = 1.5, las = 2)
top_labels(first_text = 'TEC lexicon', second_text = '', type = 2)

## Post-hoc test:

t.test(SentimentScore ~ DominantModality, filter(xall, DominantModality %in% c('Gustatory', 'Olfactory')),
	var.equal = T)



##------------------------------------------------------------------
## Analysis of SentiWordNet 3.0 data:
##------------------------------------------------------------------
## (only a subset of the results is reported in the body of the text: PosDiff and ValMax)

## Make models:

summary(xmdl.posdiff <- lm(PosDiff ~ DominantModality, xall))
summary(xmdl.max <- lm(ValMax ~ DominantModality, xall))

## Perform likelihood ratio tests against null model:

anova(xmdl.posdiff)
anova(xmdl.max)

## Get predictions:

max.pred <- my.predict.lm(xmdl.max)

## Make a plot of valmax:

setup_plots(N = 1)
## Plot 1:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 0.4))
draw_preds(max.pred)
lower_axis(N = xall$DominantModality, type = 2)
# top_labels(first_text = '', second_text = '', type = 1)
left_axis(text = 'Max SentiScore', at = seq(0, 0.4, 0.1), type = 1)
top_labels(first_text = 'SentiWordNet 3.0', second_text = '', type = 2)

## Post-hoc comparison:

t.test(PosDiff ~ DominantModality, filter(xall, DominantModality %in% c('Gustatory', 'Olfactory')),
	var.equal = T)



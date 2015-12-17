## Bodo Winter
## October 19, 2015
## Analysis for Ch. 4.4, 'Taste and smell words are more emotionally variable'


##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)
library(reshape2)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis'

## Load in plotting functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))
source(file.path(mainPath, 'functions/model_prediction_functions.R'))

## Load in modality norms:

setwd(file.path(mainPath, 'data'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2015_verb_norms.csv')

## Random subset of verbs:

v <- filter(v, RandomSet == 'yes')

## Load in data:

adj <- read.csv('COCA_adj_noun.csv')

## Get rid of unwanted labels (e.g., upper case nouns):

adj$Noun <- tolower(adj$Noun)
adj <- aggregate(Freq ~ Noun * Word, adj, sum)
adj <- dplyr::select(adj, Word, Noun, Freq)

## Define vector with order of modalities:

modalities <- c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')



##------------------------------------------------------------------
## Load in valence norms:
##------------------------------------------------------------------

## Load in Warriner:

aff <- read.csv('warriner_2013_affective_norms.csv')

## Load in NRC Hashtag data:

hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Colnames:

hash <- rename(hash, Word = V1, Sent = V2, NumPos = V3, NumNeg = V4)

## Load in Senti Wordnet:

sent <- read.csv('sentiwordnet_3.0.csv')



##------------------------------------------------------------------
## Preprocessing COCA adj-noun pairs:
##------------------------------------------------------------------

## Merge dominant modality into there:

adj$DominantModality <- l[match(adj$Word, l$Word),]$DominantModality

## Merge valence into adj:

adj$Valence <- aff[match(adj$Noun, aff$Word),]$Val

## Merge:

adj$Sent <- hash[match(adj$Noun, hash$Word), ]$Sent
adj$AbsSent <- hash[match(adj$Noun, hash$Word), ]$AbsSent

# Define empty dataset with noun slots:

all_nouns <- unique(adj$Noun)
all_nouns <- data.frame(Noun = all_nouns)
all_nouns$PosScore <- NA
all_nouns$NegScore <- NA

## Clean tags that create problems for regular expressions (and won't be matachable anyway):

not_these <- grep('\\(', all_nouns$Noun)
all_nouns <- all_nouns[-not_these,]
not_these <- grep('\\*', all_nouns$Noun)
all_nouns <- all_nouns[-not_these,]

## Get SentiWordNet data for each noun:

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

## Valence difference score (in analogy to 'Val' and 'Sent' for the other data):

all_nouns <- mutate(all_nouns, PosDiff = PosScore - NegScore)

## Add to adj dataframe:

adj$PosDiff <- all_nouns[match(adj$Noun, all_nouns$Noun), ]$PosDiff

## Compute standard deviations:

xagr <- summarise(group_by(adj, Word),
	ValenceSD = sd(Valence, na.rm = T),
	SentSD = sd(Sent, na.rm = T),
	PosDiffSD = sd(PosDiff, na.rm = T))
xagr$DominantModality <- l[match(xagr$Word, l$Word),]$DominantModality
xagr <- xagr[xagr$Word %in% l$Word,]



##------------------------------------------------------------------
## Analysis of context variability:
##------------------------------------------------------------------

## Re-order:

xagr$DominantModality <- factor(xagr$DominantModality, levels = modalities)

## Perform tests, Warriner:

summary(war.sd <- lm(ValenceSD ~ DominantModality, xagr))
anova(war.sd)

## Perform tests, Twitter Emotion Corpus:

summary(TEC.sd <- lm(SentSD ~ DominantModality, xagr))
anova(TEC.sd)

## Perform tests, Senti WordNet 3.0:

summary(senti.sd <- lm(PosDiffSD ~ DominantModality, xagr))
anova(senti.sd)

## Get predictions for Warriner and TEC (which I plot), Warriner predictions:

war.sd.preds <- my.predict.lm(war.sd)

## TEC prdictions:

TEC.sd.pred <- my.predict.lm(TEC.sd)

## Make a plot of this, Warriner data left, TEC data right:

l$DominantModality <- factor(l$DominantModality, levels = modalities)
quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
## Plot 1:
emptyplot(xlim = c(0.5, 5.5), ylim = c(1, 1.5), AB = '(a)')
draw_preds(war.sd.preds)
lower_axis(N = l$DominantModality, type = 2)
left_axis(text = 'Valence SD', at = seq(1, 1.5, 0.15), type = 1)
top_labels(first_text = 'Warriner et al. (2013)', second_text = '', type = 2)
## Plot 2:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0.7, 1.1), AB = '(b)')
draw_preds(TEC.sd.pred)
lower_axis(N = l$DominantModality, type = 2)
mtext(side = 4, text = 'Valence SD', line = 4.2, font = 2, cex = 2)
axis(side = 4, at = seq(0.7, 1.1, 0.1), lwd = 2, font = 2, cex.axis = 1.5, las = 2)
top_labels(first_text = 'Mohammad (2012)', second_text = '', type = 2)



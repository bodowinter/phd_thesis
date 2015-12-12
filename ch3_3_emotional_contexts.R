## Bodo Winter
## October 19, 2015
## Analysis for Ch. 3.3, 'Taste and smell words in context'

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

## Combine all:

xall <- rbind(dplyr::select(l, Word, DominantModality),
	dplyr::select(n, Word, DominantModality),
	dplyr::select(v, Word, DominantModality))

## Load in data:

adj <- read.csv('COCA_adj_noun.csv')

## Get rid of unwanted labels (e.g., uppercase nouns):

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
aff <- mutate(aff, AbsV = abs(Val - mean(Val)))	# absolute valence

## Load in NRC Hashtag data:

hash <- read.csv('NRC_hashtag_unigrams-pmilexicon.txt', sep = '\t', header = F)

## Colnames:

hash <- rename(hash, Word = V1, Sent = V2, NumPos = V3, NumNeg = V4)

## Create absolute valence score:

hash <- mutate(hash, AbsSent = abs(Sent - mean(Sent, na.rm = T)))

## Load in Senti Wordnet:

sent <- read.csv('sentiwordnet_3.0.csv')



##------------------------------------------------------------------
## Objectivity vs. subjectivity:
##------------------------------------------------------------------

## Imdb movie review dataset classified according to subjective versus objective:

subj <- readLines('pang_lee_2004_subjective.txt')
obj <- readLines('pang_lee_2004_objective.txt')

## Process this:

subj <- strsplit(subj, split = ' ')
obj <- strsplit(obj, split = ' ')

## Make a table:

subj_df <- data.frame(Objectivity = rep('subj', 5000))
obj_df <- data.frame(Objectivity = rep('obj', 5000))

## Dataframe:

M <- as.data.frame(matrix(numeric(5*5000), nrow = 5000))
colnames(M) <- modalities
subj_df <- cbind(subj_df, M)
obj_df <- cbind(obj_df, M)

## Dataframes with words per modality:

Visual <- unique(xall[xall$DominantModality == 'Visual',]$Word)
Haptic <- unique(xall[xall$DominantModality == 'Haptic',]$Word)
Auditory <- unique(xall[xall$DominantModality == 'Auditory',]$Word)
Gustatory <- unique(xall[xall$DominantModality == 'Gustatory',]$Word)
Olfactory <- unique(xall[xall$DominantModality == 'Olfactory',]$Word)

## Loop through subj and put it into df:

for (i in 1:5000) {
	this_vector <- subj[[i]]
	
	for (j in 1:5) {
		this_modality <- modalities[j]
		subj_df[i, this_modality] <- sum(this_vector %in% get(this_modality))
		}
	
	}

## Do the same for obj:

for (i in 1:5000) {
	this_vector <- obj[[i]]
	
	for (j in 1:5) {
		this_modality <- modalities[j]
		obj_df[i, this_modality] <- sum(this_vector %in% get(this_modality))
		}
	
	}

## Combine:

subj <- rbind(subj_df, obj_df)

## Make negative binomial models for this:

library(MASS)
summary(vis.mdl <- glm.nb(Visual ~ Objectivity, subj))
summary(hap.mdl <- glm.nb(Haptic ~ Objectivity, subj))
summary(aud.mdl <- glm.nb(Auditory ~ Objectivity, subj))
summary(gus.mdl <- glm.nb(Gustatory ~ Objectivity, subj))
summary(olf.mdl <- glm.nb(Olfactory ~ Objectivity, subj))

## Make null models for comparison:

vis.null <- glm.nb(Visual ~ 1, subj)
hap.null <- glm.nb(Haptic ~ 1, subj)
aud.null <- glm.nb(Auditory ~ 1, subj)
gus.null <- glm.nb(Gustatory ~ 1, subj)
olf.null <- glm.nb(Olfactory ~ 1, subj)
anova(vis.null, vis.mdl, test = 'Chisq')
anova(hap.null, hap.mdl, test = 'Chisq')
anova(aud.null, aud.mdl, test = 'Chisq')
anova(gus.null, gus.mdl, test = 'Chisq')
anova(olf.null, olf.mdl, test = 'Chisq')

## Make predictions table that contains the slopes:

xpred <- data.frame(DominantModality = modalities)
xpred$fit <- c(coef(vis.mdl)[2], coef(hap.mdl)[2], coef(aud.mdl)[2], coef(gus.mdl)[2], coef(olf.mdl)[2])
xpred$se.fit <- c(summary(vis.mdl)$coefficients[2,2],
	summary(hap.mdl)$coefficients[2,2],
	summary(aud.mdl)$coefficients[2,2],
	summary(gus.mdl)$coefficients[2,2],
	summary(olf.mdl)$coefficients[2,2])
xpred$UB <- xpred$fit + 1.96 * xpred$se.fit
xpred$LB <- xpred$fit - 1.96 * xpred$se.fit

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0.5, 5.5), ylim = c(-0.5, 2))
abline(h = 0, lty = 2)
draw_preds(xpred)
lower_axis(N = '', type = 2)
top_labels(first_text = 'Subjectivity vs. Objectivity', second_text = '', type = 2)
left_axis(text = 'Log Slope', at = seq(-0.5, 2, 0.5), type = 1)



##------------------------------------------------------------------
## Preprocess COCA adj-noun pairs:
##------------------------------------------------------------------

## Merge dominant modality into there:

adj$DominantModality <- l[match(adj$Word, l$Word),]$DominantModality

## For creating weights, create frequencies:

adj_freqs <- aggregate(Freq ~ Word, adj, sum)

## For creating weights, merge the frequencies into the table:

adj$AdjFreq <- adj_freqs[match(adj$Word, adj_freqs$Word), ]$Freq

## Create weights:

adj <- mutate(adj, Weight = Freq / AdjFreq)

## Merge valence into adj:

adj$Valence <- aff[match(adj$Noun, aff$Word),]$Val
adj$AbsV <- aff[match(adj$Noun, aff$Word),]$AbsV

## Merge:

adj$Sent <- hash[match(adj$Noun, hash$Word), ]$Sent
adj$AbsSent <- hash[match(adj$Noun, hash$Word), ]$AbsSent

# Define empty dataset with slots for all nouns that co-occur with adjectives:

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
	these_sent <- grep(this_word, sent$SynsetTerms)
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

## Overall valence from SentiWordNet 3.0 (corresponds to  'absolute valence' of the other data):

all_nouns$ValMax <- apply(all_nouns[,c('PosScore', 'NegScore')], 1, max)

## Add to adj dataframe:

adj <- cbind(adj, all_nouns[match(adj$Noun, all_nouns$Noun),c('PosScore','NegScore','ValMax', 'PosDiff')])

## Take averages:

xagr <- summarise(group_by(adj, Word),
	Valence = mean(Valence, na.rm = T, w = Weight),
	AbsV = mean(AbsV, na.rm = T, w = Weight),
	Sent = mean(Sent, na.rm = T, w = Weight),
	AbsSent = mean(AbsSent, na.rm = T, w = Weight),
	PosDiff = mean(PosDiff, na.rm = T, w = Weight),
	PosScore = mean(PosScore, na.rm = T, w = Weight),
	NegScore = mean(NegScore, na.rm = T, w = Weight),
	ValMax = mean(ValMax, na.rm = T, w = Weight))
xagr$DominantModality <- l[match(xagr$Word, l$Word),]$DominantModality
xagr <- xagr[xagr$Word %in% l$Word,]



##------------------------------------------------------------------
## Analysis of context valence:
##------------------------------------------------------------------

## Re-order:

xagr$DominantModality <- factor(xagr$DominantModality, levels = modalities)

## Perform tests, Warriner:

summary(war.val <- lm(Valence ~ DominantModality, xagr))
anova(war.val)

summary(war.abs <- lm(AbsV ~ DominantModality, xagr))
anova(war.abs)

## Perform tests, Twitter Emotion Corpus:

summary(TEC.val <- lm(Sent ~ DominantModality, xagr))
anova(TEC.val)

summary(TEC.abs <- lm(AbsSent ~ DominantModality, xagr))
anova(TEC.abs)

## Perform tests, Senti WordNet 3.0:

summary(senti.posdiff <- lm(PosDiff ~ DominantModality, xagr))
anova(senti.posdiff)

summary(senti.max <- lm(ValMax ~ DominantModality, xagr))
anova(senti.max)

## Post-hoc tests:

t.test(Valence ~ DominantModality,
	filter(xagr, DominantModality %in% c('Gustatory', 'Olfactory')), var.equal = T)

t.test(Sent ~ DominantModality,
	filter(xagr, DominantModality %in% c('Gustatory', 'Olfactory')), var.equal = T)

t.test(PosDiff ~ DominantModality,
	filter(xagr, DominantModality %in% c('Gustatory', 'Olfactory')), var.equal = T)

## Make a plot with predictions:

war.abs.pred <- my.predict.lm(war.abs)

## Get predictions:

TEC.abs.pred <- my.predict.lm(TEC.abs)

## Combination of two plots:
## Both absolut valence, Warriner left, SentiWordNet right:

quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.25), mai = c(0, 0.25, 0, 0))
## Plot 1:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0.8, 1.4), AB = '(a)')
draw_preds(war.abs.pred)
lower_axis(N = xagr$DominantModality, type = 2)
left_axis(text = 'Absolute Valence', at = seq(0.8, 1.8, 0.2), type = 1)
top_labels(first_text = 'Warriner et al. (2013)', second_text = '', type = 2)
## Plot 2:
emptyplot(xlim = c(0.5, 5.5), ylim = c(0.4, 1), AB = '(b)')
draw_preds(TEC.abs.pred)
lower_axis(N = xagr$DominantModality, type = 2)
top_labels(first_text = 'Mohammad (2012)', second_text = '', type = 2)
mtext(side = 4, text = 'Absolute Valence', line = 3.5, font = 2, cex = 2)
axis(side = 4, at = seq(0.4, 1, 0.2), lwd = 2, font = 2, cex.axis = 1.5, las = 2)



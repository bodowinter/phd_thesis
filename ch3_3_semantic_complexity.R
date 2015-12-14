## Bodo Winter
## Created: September 20, 2015
## Analysis for Ch. 3.3, 'Differences in semantic complexity'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd'

## Load in data:

setwd(file.path(mainPath, 'data'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2015_verb_norms.csv')

## Load in MacMillan and WordNet sense counts:

senses <- read.csv('wordnet_macmillan_senses.csv')

## Create random subsets:

v <- filter(v, RandomSet == 'yes')

## Load in libraries:

library(MASS)		# for glm.nb
library(pscl)		# for oDtest
library(dplyr)

## Load in plotting functions:

source(file.path(mainPath, 'scripts/model_prediction_functions.R'))



##------------------------------------------------------------------
## Analysis of semantic complexity:
##------------------------------------------------------------------

## Merge datasets:

xall <- rbind(dplyr::select(l, Word, DominantModality, ModalityExclusivity, VisualStrengthMean:OlfactoryStrengthMean),
	dplyr::select(n, Word, DominantModality, ModalityExclusivity, VisualStrengthMean:OlfactoryStrengthMean),
	dplyr::select(v, Word, DominantModality, ModalityExclusivity, VisualStrengthMean:OlfactoryStrengthMean))

## Get rid of duplicated:

xall <- xall[!duplicated(xall$Word),]

## Add POS column:

xall$POS <- c(rep('adj',nrow(l)), rep('noun',nrow(n)), rep('verb',nrow(xall)-823))
xall <- cbind(xall, senses[match(xall$Word, senses$Word), c('WordNet', 'MacMillan')])

## Construct models:

summary(xmdl.wn <- glm.nb(WordNet ~ DominantModality + POS, xall))
summary(xmdl.mc <- glm.nb(MacMillan ~ DominantModality + POS, xall))

## Do likelihood ratio tests of models:

summary(xmdl.wn2 <- glm.nb(WordNet ~ 1 + POS, xall))
summary(xmdl.mc2 <- glm.nb(MacMillan ~ 1 + POS, xall))
anova(xmdl.wn2, xmdl.wn, test = 'Chisq')
anova(xmdl.mc2, xmdl.mc, test = 'Chisq')

## Test whether negative binomial model is needed:

odTest(xmdl.wn)
odTest(xmdl.mc)

## Make marginal models for plot:

wn.plot <- glm.nb(WordNet ~ DominantModality, xall)
mc.plot <- glm.nb(MacMillan ~ DominantModality, xall)
wn.df <- my.predict.lm(wn.plot, link = 'log')
mc.df <- my.predict.lm(mc.plot, link = 'log')

## Order the prediction dataframes:

modality_order <- c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')
wn.df$DominantModality <- factor(wn.df$DominantModality,
	levels = modality_order)
mc.df$DominantModality <- factor(mc.df$DominantModality,
	levels = modality_order)	
wn.df <- wn.df[order(wn.df$DominantModality),]
mc.df <- mc.df[order(mc.df$DominantModality),]

## Make a plot of this:

setup_plots(N = 2)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 11), AB = '(a)', yfactor = 0.05)
draw_preds(wn.df)
lower_axis(type = 2, N = factor(xall$DominantModality, levels = modality_order))
left_axis(at = seq(0, 10, 2), text = 'Dictionary meanings', type = 2)
top_labels(first_text = 'WordNet', second_text = '')
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 11), AB = '(b)', yfactor = 0.05)
draw_preds(mc.df)
lower_axis(type = 2, N = factor(xall$DominantModality, levels = modality_order))
top_labels(first_text = 'MacMillan', second_text = '')




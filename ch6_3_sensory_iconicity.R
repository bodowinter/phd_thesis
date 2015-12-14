## Bodo Winter
## September 17, 2015
## Analysis for Ch. 6.3, '6.4. Testing the iconicity of sensory words'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Set options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis'

## Load in plotting and model prediction functions:

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

## Reduce verbs to random SUBTLset:

v <- filter(v, RandomSet == 'yes')

## Load in iconicity, sensory experience ratings and SUBTLTLEX POS data:

icon <- read.csv('iconicity_ratings.csv')
SER <- read.csv('juhasz_yap_2013_SER.csv')
SUBTL <- read.csv('SUBTLEX_US.csv')

## Merge sensory experince ratings into there:

icon$SER <- SER[match(icon$Word, SER$Word),]$SER

## How many overlap?

sum(!is.na(icon$SER))	# 1,780 words

## Rename part of speech column:

SUBTL <- rename(SUBTL, POS = Dom_PoS_SUBTLEX)

## With the part-of-speech tags of SUBTLTLEX, merge grammatical items:

gram_items <- c('Article', 'Conjunction', 'Determiner', 'Number', 'Preposition', 'Pronoun',
	'Not', 'Ex', '#N/A', 'To')
SUBTL[SUBTL$POS %in% gram_items,]$POS <- 'Grammatical'

## Merge interjections and unclassified to just 'interjection':

iconic_items <- c('Unclassified', 'Interjection')
SUBTL[SUBTL$POS %in% iconic_items,]$POS <- 'Interjection'

## Code the POS of the following words (which occur in the iconicity data frame) by hand:

SUBTL[SUBTL$Word == 'brown',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'kitty',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'walker',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'grr',]$POS <- 'Interjection'
SUBTL[SUBTL$Word == 'ash',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'beery',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'bray',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'char',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'curly',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'frost',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'god',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'grace',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'herby',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'jag',]$POS <- 'Noun'	# according to Macmillan
SUBTL[SUBTL$Word == 'jammy',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'march',]$POS <- 'Noun'
SUBTL[SUBTL$Word == 'pat',]$POS <- 'Verb'	# according to Macmillan
SUBTL[SUBTL$Word == 'rusty',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'shaggy',]$POS <- 'Adjective'
SUBTL[SUBTL$Word == 'soundless',]$POS <- 'Adjective'

## Code the POS of the following words (which occur in the SER data frame) by hand:

nouns <- c('abbot', 'alto', 'arbor', 'arc', 'aspen', 'barb', 'basil', 'beck',
	'belle', 'bill', 'birch', 'bond', 'boulder', 'bud', 'burl', 'cam', 'cape',
	'carol', 'celeste', 'cheddar', 'cliff', 'cob', 'colt', 'coop', 'crick',
	'daisy', 'dale', 'dean', 'dell', 'dill', 'ensign', 'eve', 'fife',
	'finch', 'flax', 'flint', 'ford', 'fort', 'gable', 'gale', 'garland',
	'gene', 'gill', 'glen', 'gob', 'gong', 'gore', 'grant', 'hank', 'hart',
	'hawk', 'heath', 'helm', 'hutch', 'iris', 'jack', 'jasper', 'jay',
	'jersey', 'knight', 'lily', 'ma', 'mace', 'mandrake', 'mark', 'marsh',
	'mason', 'metro', 'mike', 'moll', 'moss', 'nick', 'pa', 'paddy', 'peacock',
	'pearl', 'peg', 'pelt', 'pip', 'poppy', 'prism', 'quill', 'ray', 'reed',
	'regent', 'reuben', 'robin', 'ruby', 'saint', 'sheen', 'shogun',
	'shore', 'sparrow', 'steed', 'stein', 'stork', 'swan', 'thorn',
	'trill', 'tulip', 'van', 'velcro', 'venus', 'ware', 'watt',
	'welch', 'wick', 'wren', 'yam')

SUBTL[SUBTL$Word %in% nouns, ]$POS <- 'Noun'

## Add verb tags:

verbs <- c('dodge', 'don', 'drew', 'fester', 'josh', 'leach',
	'leer', 'lynch', 'parry', 'peck', 'pierce', 'retch', 'revere',
	'rue', 'stoke', 'trek')

SUBTL[SUBTL$Word %in% verbs, ]$POS <- 'Verb'

## Add adjective tags:
	
adjectives <- c('butch', 'frank', 'haggard', 'hale', 'hardy', 'hazel',
	'ill', 'stark')

SUBTL[SUBTL$Word %in% adjectives, ]$POS <- 'Adjective'

## Exclude tags that are not in MacMillan (no POS information):

not_in_macmillan <- c('bunt', 'bosh', 'druthers', 'fritz', 'leary', 'lilly',
	'milch', 'tong')

SUBTL <- SUBTL[!(SUBTL$Word %in% not_in_macmillan), ]

## Exclude since it is listed as a name in MacMillan:

SUBTL <- SUBTL[SUBTL$Word != 'bob',]



##------------------------------------------------------------------
## One sample t-test against zero:
##------------------------------------------------------------------

t.test(icon$Iconicity, mu = 0)




##------------------------------------------------------------------
## Look at parts of speech for the iconicity ratings:
##------------------------------------------------------------------

## Merge POS into the iconicity dataset:

icon$POS <- SUBTL[match(icon$Word, SUBTL$Word),]$POS

## Look at means:

icon_agr <- aggregate(Iconicity ~ POS, icon, mean)

## For ease of reporting:

(mutate(icon_agr, Iconicity = round(Iconicity, 2)) %>% arrange(desc(Iconicity)) -> icon_agr)

## Test for significant differences between lexical categories:

summary(icon_POS <- lm(Iconicity ~ POS, icon))
anova(icon_POS)



##------------------------------------------------------------------
## Show overall distribution of iconicity ratings:
##------------------------------------------------------------------

## For plotting reduce icon_agr to adjectives, verbs and nouns:

icon_agr <- icon_agr[icon_agr$POS %in% c('Verb', 'Adjective', 'Noun', 'Grammatical', 'Interjection'),]

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(-2.5, 5), ylim = c(0, 0.5), yaxs = 'i')
plot_density(icon$Iconicity, powerpoint = F)
left_axis(text = 'Density', at = seq(0, 0.5, 0.1), type = 1)
lower_axis(style = 'continuous', lab = 'Iconicity Ratings', at = seq(-2.5, 5, 2.5), type = 1)
axis(side = 1, at = icon_agr$Iconicity, lwd.ticks = 2, font = 2, tcl = +0.65 , labels = F)
text(x = icon_agr$Iconicity, y = 0.03, labels = c('I', 'V', 'A', 'N', 'G'), font = 2, cex = 1.15)
abline(v = 0, lty = 2, lwd = 2)
box(lwd = 2)



##------------------------------------------------------------------
## Analysis of SER ratings, simple:
##------------------------------------------------------------------

## Regress iconicity onto SER:

summary(xmdl <- lm(Iconicity ~ SER, icon))
anova(xmdl)

## Perform a correlation:

with(icon, cor.test(SER, Iconicity))

## Make predictions for plot:

newdata <- data.frame(SER = seq(1, 7, 0.01))
newdata$fit <- predict(xmdl, newdata)
newdata$UB <- newdata$fit + 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit
newdata$LB <- newdata$fit - 1.96 * predict(xmdl, newdata, se.fit = T)$se.fit

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0.5, 7.5), ylim = c(-2.5, 5))
left_axis(text = 'Iconicity Ratings', at = seq(-5, 5, 2.5), type = 1)
lower_axis(style = 'continuous', at = 1:7, lab = '', type = 1)
mtext(side = 1, text = 'Sensory Experience Ratings',
	line = 3.5, font = 2, cex = 2)
polygon(c(newdata$SER, rev(newdata$SER)),
	c(newdata$UB, rev(newdata$LB)), col = rgb(0, 0, 0, 0.4), border = F)
points(newdata$SER, newdata$fit, lwd = 2, type = 'l')
points(icon$SER, icon$Iconicity, pch = 19,
	cex = 0.7, col = rgb(0, 0, 0, 0.4))




##------------------------------------------------------------------
## Analysis of SER ratings, with frequency and POS:
##------------------------------------------------------------------

## Merge the SUBTLTLEX frequencies into icon:

icon$Freq <- SUBTL[match(icon$Word, SUBTL$Word), ]$Lg10WF

## Merge frequency into there:

icon$Freq <- SUBTL[match(icon$Word, SUBTL$Word),]$FREQcount

## Analyze this:

summary(SER_mdl <- lm(Iconicity ~ SER + log10(Freq) + POS, icon))
anova(SER_mdl)



##------------------------------------------------------------------
## Analysis of by-modality differences:
##------------------------------------------------------------------

## Take aggregate:

xagr <- rbind(dplyr::select(l, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(n, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(v, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity))

## Add POS column:

xagr$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)), rep('verb', nrow(v)))

## Add iconicity to this:

xagr$Iconicity  <- icon[match(xagr$Word, icon$Word),]$Iconicity

## Make a model of this:

summary(xmdl <- lm(Iconicity ~ DominantModality, xagr))
summary(xmdl.pos <- lm(Iconicity ~ DominantModality + POS, xagr))	# not reported in main text
anova(xmdl)
anova(xmdl.pos)		# not reported in main text

## Make predictions:

mypreds <- my.predict.lm(xmdl)

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 2))
draw_preds(mypreds)
lower_axis(style = 'modality', lab = '', N = xagr$DominantModality, type = 2)
left_axis(text = 'Iconicity', at = seq(0, 2, 0.5), type = 1)

## Separate by-modality analysis for different parts of speech:

summary(xmdl.v <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'verb')))
anova(xmdl.v)
summary(xmdl.n <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'noun')))
anova(xmdl.n)
summary(xmdl.adj <- lm(Iconicity ~ DominantModality, subset(xagr, POS == 'adj')))
anova(xmdl.adj)



##------------------------------------------------------------------
## Analysis of whether sound association explains tactile iconicity:
##------------------------------------------------------------------

## Adjectives only:

library(lavaan)
myFormula <- '# direct effect:
	Iconicity ~ c * HapticStrengthMean
	# mediator:
	AuditoryStrengthMean ~ a * HapticStrengthMean
	Iconicity ~ b * AuditoryStrengthMean
	# indirect effect (a * b):
	ab := a * b
	# total effect:
	total := c + (a * b)'

## Path analysis for only haptic adjectives:

hap_adjs <- xagr[xagr$POS == 'adj' & xagr$DominantModality == 'Haptic',]
hap_adjs <- hap_adjs[!is.na(hap_adjs$Iconicity),]
hap_adjs.fit <- sem(myFormula, data = hap_adjs, se = 'boot')		# haptic subset
summary(hap_adjs.fit)

## Path analysis for adjectives (reported in chapter):

lred <- xagr[xagr$POS == 'adj',]
lred <- lred[!is.na(lred$Iconicity),]
lred.fit <- sem(myFormula, data = lred, se = 'boot')
summary(lred.fit)

## Path analysis for all data (not reported in main text):

xagr.red <- xagr[!is.na(xagr$Iconicity),]
xagr.fit <- sem(myFormula, data = xagr.red, se = 'boot')
summary(xagr.fit)



##------------------------------------------------------------------
## Phonestheme analysis:
##------------------------------------------------------------------

## Load in phonesthemes (hand-coded based on Hutchins, 1998 Appendix A):

library(xlsx)
phon <- read.xlsx('phonesthemes_hutchins_1998.xlsx', 1)

## Process the phonesthemes:

phonsplit <- strsplit(phon$Phonaestheme, ';')
nphon <- numeric(length(phonsplit))
for (i in 1:length(phonsplit)) {
	if (!is.na(phonsplit[[i]])) {
		nphon[i] <- length(phonsplit[[i]])
		}
	}
phon$NPhonestheme <- nphon

## Separate final and initial phonesthemes:

phon$NInitialPhonestheme <- numeric(nrow(phon))
phon$NFinalPhonestheme <- numeric(nrow(phon))
for (i in 1:length(phonsplit)) {
	phon[i,]$NInitialPhonestheme <- length(grep('[a-z]+-', phonsplit[[i]]))
	phon[i,]$NFinalPhonestheme <- length(grep('-[a-z]+', phonsplit[[i]]))
	}

## Put this into Lynott & Connell (2009):

l <- cbind(l, phon[match(l$Word, phon$Word),-1])
l$Iconicity <- icon[match(l$Word, icon$Word),]$Iconicity

## Make a table:

xtab <- table(l$DominantModality, ifelse(l$NPhonestheme == 0, 0, 1))
props <- round(xtab[,2] / rowSums(xtab[,1:2]),2)



##------------------------------------------------------------------
## OED analysis:
##------------------------------------------------------------------

## Load in etymologies:

OED <- read.csv('all_words_OED_etymology.csv')

## Match:

l <- cbind(l, OED[match(l$Word, OED$Word),-1])

## Which are not in OED?

l[is.na(l$Year),]$Word		# 'coconutty'

## Get rid of 'coconutty' for this analysis:

l <- l[l$Word != 'coconutty',]

## Fix some origin POS:

l[l$OED_OriginPOS == 'noun/adj same form',]$OED_OriginPOS <- 'adj/noun'
l[l$OED_OriginPOS == 'noun or verb',]$OED_OriginPOS <- 'noun/verb'

## Suspected to be imitative:

l$Imitative <- 'not'
l[l$Origin %in% c('possibly imitative', 'Germanic/probably imitative', 'Germanic, perhaps ultimatively imitative', 'probably imitative'),]$Imitative <- 'possibly'
l[l$Origin %in% 'imitative',]$Imitative <- 'yes'
l[l$Origin %in% c('unclear', 'origin unknown', 'unclear, probably Scandinavian'),]$Imitative <- 'unclear'

## Make a table:

(xtab <- table(l$DominantModality, l$Imitative))
arrange(aggregate(Iconicity ~ Imitative, l, mean), desc(Iconicity))



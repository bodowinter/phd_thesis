## Bodo Winter
## September 17, 2015; New approach October 15, 2015
## Analysis for Ch. 2.5, 'Word frequency asymmetries'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Options:

options(stringsAsFactors = F)

## Load in packages:

library(dplyr)
library(reshape2)
library(lme4)
library(MASS)
library(pscl)
library(glmmADMB)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis/'

## Load in functions:

source(file.path(mainPath, 'functions/model_prediction_functions.R'))
source(file.path(mainPath, 'functions/plotting_functions.R'))

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

## Load in other datasets used in this particular analysis:

verbs <- read.csv('COCA_verb_frequencies.csv')
nouns <- read.csv('COCA_noun_frequencies.csv')
adjs <- read.csv('COCA_adj_frequencies.csv')

## Merge COCA frequency data into l, d and n:

l <- cbind(l, adjs[match(l$Word, adjs$Word), -1])
n <- cbind(n, nouns[match(n$Word, nouns$Word), -1])
v <- cbind(v, verbs[match(v$Word, verbs$Word), -1])

## For how many was there no frequency data?

sum(is.na(l$Freq))
sum(is.na(n$Freq))
sum(is.na(v$Freq))

## Make those that are NA into 0 (since it is not attested):

l[is.na(l$Freq), c('Freq', 'CD')] <- 0
n[is.na(n$Freq), c('Freq', 'CD')] <- 0
v[is.na(v$Freq), c('Freq', 'CD')] <- 0

## Create a data frame from the random subset:

vrand <- filter(v, RandomSet == 'yes')


##------------------------------------------------------------------
## Simple average frequency analysis:
##------------------------------------------------------------------

## Take aggregate:

xagr <- rbind(dplyr::select(l, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD),
	dplyr::select(n, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD),
	dplyr::select(v, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD))
xagr.rand <- rbind(dplyr::select(l, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD),
	dplyr::select(n, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD),
	dplyr::select(vrand, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity, Freq:CD))

## Add POS column to both datasets:

xagr$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)), rep('verb', nrow(v)))
xagr.rand$POS <- c(rep('adj', nrow(l)), rep('noun', nrow(n)), rep('verb', nrow(vrand)))

## Make a model of just the random subset data with just modality:

summary(xmdl.rand <- glm.nb(Freq ~ DominantModality, xagr.rand))
summary(xmdl.rand.null <- glm.nb(Freq ~ 1, xagr.rand))

## Likelihood ratio test:

anova(xmdl.rand.null, xmdl.rand, test = 'Chisq')

## Test whether negative binomial model is needed:

odTest(xmdl.rand)

## To test interaction, full set is needed:

summary(xmdl.nb <- glm.nb(Freq ~ DominantModality * POS, xagr))
summary(xmdl.noint <- glm.nb(Freq ~ DominantModality + POS, xagr))
summary(xmdl.nofreq <- glm.nb(Freq ~ 1 + POS, xagr))

## Likelihood ratio tests:

anova(xmdl.noint, xmdl.nb, test = 'Chisq')
anova(xmdl.nofreq, xmdl.noint, test = 'Chisq')

## Extract predictions:

xmdl.df <- my.predict.lm(xmdl.rand, link = 'log')

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 18000))
draw_preds(xmdl.df)
lower_axis(type = 2, N = xagr.rand$DominantModality)
left_axis(at = seq(0, 18000, 2000),
	text = 'Predicted word frequency', type = 1,
	labels = paste0(seq(0, 18, 2), 'k'))



##------------------------------------------------------------------
## Simple cumulative frequencies:
##------------------------------------------------------------------

## Take means:

lfreqs <- aggregate(Freq ~ DominantModality, l, sum)[,-1]
nfreqs <- aggregate(Freq ~ DominantModality, n, sum)[,-1]
vfreqs <- aggregate(Freq ~ DominantModality, vrand, sum)[,-1]
vfreqs <- c(vfreqs, 0)

## Make a table out of this:

allfreqs <- c(lfreqs, nfreqs, vfreqs)
allfreqs <- round(allfreqs, -3) / 1000
allfreqs <- paste0(allfreqs, 'k')
M <- matrix(allfreqs, nrow = 3, byrow = T)

## Make the table presentable:

rownames(M) <- c('Adjectives', 'Nouns', 'Verbs')
colnames(M) <- c('Vision', 'Hearing', 'Touch', 'Taste', 'Smell')

## Calculate random draw frequency:

allfreqs <- c(lfreqs, nfreqs, vfreqs)
M <- matrix(allfreqs, nrow = 3, byrow = T)
Msums <- colSums(M)
round(Msums / sum(Msums), 2)



##------------------------------------------------------------------
## Stability Check #1: Dialect variation
##------------------------------------------------------------------

## Load in additional datasets needed:

ELP <- read.csv('ELP_frequency.csv')
BLP <- read.csv('BLP_frequency.csv')
SUB <- read.csv('SUBTLEX_US.csv')
MRC <- read.csv('MRC.csv')

## Make all into long format:

lsub <- dplyr::select(l, Word, DominantModality)
nsub <- dplyr::select(n, Word, DominantModality)
vsub <- dplyr::select(v, Word, DominantModality)

## Put them below each other:

xall <- rbind(lsub, nsub, vsub)

## Merge US frequencies into this:

xall$SUB <- SUB[match(xall$Word, sub$Word),]$FREQcount
xall$KF <- ELP[match(xall$Word, ELP$Word),]$Freq_KF
xall$HAL <- ELP[match(xall$Word, ELP$Word),]$Feq_HAL
xall$TL <- MRC[match(xall$Word, MRC$Word),]$T.L.FREQ
xall$Brown <- MRC[match(xall$Word, MRC$Word),]$BROWN.FREQ
xall$COCA <- adjs[match(xall$Word, adjs$Word),]$Freq

## Merge British frequencies into this:

xall$BritSUB <- BLP[match(xall$Word, BLP$Word),]$SUBTLEX.FREQ
xall$BNC <- BLP[match(xall$Word, BLP$Word),]$BNC.FREQ
xall$CELEX <- BLP[match(xall$Word, BLP$Word),]$Celex.FREQ

## Make into long format:

llong <- melt(xall, id.vars = c('Word', 'DominantModality'))
llong <- rename(llong, Corpus = variable, Freq = value)

## Add AE vs. BE information:

british_corpora <- c('BritSUB', 'BNC', 'CELEX')
llong$Dialect <- 'AE'
llong[llong$Corpus %in% british_corpora, ]$Dialect <- 'BE'

## Fill NA's with 0's:

llong[is.na(llong$Freq),]$Freq <- 0

## Make a mixed negative binomial model out of this:

xmdl.dia.nb <- glmmadmb(Freq ~ Dialect * DominantModality + (1|Corpus), family = 'nbinom', data = llong)
xmdl.dia.nb.noint <- glmmadmb(Freq ~ Dialect + DominantModality + (1|Corpus), family = 'nbinom', data = llong)
xmdl.dia.nb.noMod <- glmmadmb(Freq ~ Dialect + 1 + (1|Corpus), family = 'nbinom', data = llong)

## Likelihood ratio test of interaction:

this_df <- length(coef(xmdl.dia.nb)) - length(coef(xmdl.dia.nb.noint))
LRT <- diff(c(2 * logLik(xmdl.dia.nb), 2 * logLik(xmdl.dia.nb.noint)))
1 - pchisq(abs(LRT), df = this_df)		# p-value

## Likelihood ratio test of main effect:

LRT <- diff(c(2 * logLik(xmdl.dia.nb.noint), 2 * logLik(xmdl.dia.nb.noMod)))
1 - pchisq(abs(LRT), df = 4)		# p-value

## Simple plot model:

xmdl.dia.plot.all <- glmmadmb(Freq ~ DominantModality + (1|Corpus), family = 'nbinom', data = llong)

## Extract data:

newdata <- data.frame(DominantModality = modalities, Freq = rep(0, 5))
mm <- model.matrix(terms(xmdl.dia.plot.all), newdata)
newdata$Freq <- predict(xmdl.dia.plot.all, newdata, re.form = NA)
newdata$SE <- diag(mm %*% tcrossprod(vcov(xmdl.dia.plot.all), mm))

## For some reasons the labels are changed (I hand-checked this):

newdata$DominantModality <- c('Olfactory', 'Auditory', 'Visual', 'Haptic', 'Gustatory')
rownames(newdata) <- c('Olfactory', 'Auditory', 'Visual', 'Haptic', 'Gustatory')
newdata <- newdata[modalities,]

## Exponentiate:

newdata$UB <- exp(newdata$Freq + 1.96 * newdata$SE)
newdata$LB <- exp(newdata$Freq - 1.96 * newdata$SE)
newdata$Freq <- exp(newdata$Freq)
newdata <- rename(newdata, fit = Freq)

## Make a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 2500))
draw_preds(newdata)
lower_axis(type = 2, style = 'modality', N = xall$DominantModality)
left_axis(at = seq(0, 2500, 500),
	text = '', type = 3)
mtext(side = 2, line = 4.5, 'Predicted frequency', cex = 1.85, font = 2)



##------------------------------------------------------------------
## Stability Check #2: Diachronic variation (Google NGram)
##------------------------------------------------------------------

## Load in Google data:

goo <- read.csv('google_ngram.csv')

## Add dominant modality information:

goo$Modality <- l[match(goo$Word, l$Word),]$DominantModality

## Compute averages:

goo_agr <- summarise(group_by(goo, Year, Modality),
	FreqMean = mean(Frequency), FreqSD = sd(Frequency))

## Create a plot of this:

setup_plots(N = 1)
emptyplot(xlim = c(1700, 2060), ylim = c(0, 0.00004))
for (i in 1:5) {
	this_modality <- unique(goo_agr$Modality)[i]
	with(goo_agr[goo_agr$Modality == this_modality,],
		points(Year, FreqMean, type = 'l', lwd = 2, col = 1)	# change for color
		)
	}
axis(side = 1, at = seq(1700, 2000, 100), lwd = 2, font = 2, cex.axis = 1.5)
axis(side = 2, at = seq(0, 0.00004, length.out = 5),
	labels = paste(0:4, 'e-05', sep = ''),
	lwd = 2, font = 2, cex.axis = 1.25, las = 2)
mtext(side = 1, text = 'Year',
	line = 3, font = 2, cex = 1.9)
mtext(side = 2, text = 'Relative frequency', line = 4.6, font = 2, cex = 1.8)
axis(side = 1, at = 1:7, lwd = 2, font = 2, cex.axis = 1.5)
text(x = 1900, y = 0.0000375, labels = 'Visual', font = 2, cex = 1.5)
text(x = 1900, y = 0.0000165, labels = 'Tactile', font = 2, cex = 1.5)
text(x = 1900, y = 0.0000085, labels = 'Olfactory', font = 2, cex = 1.5)
text(x = 2040, y = 0.000004, labels = 'Auditory', font = 2, cex = 1.15)
text(x = 2040, y = 0.000002, labels = 'Gustatory', font = 2, cex = 1.15)





##------------------------------------------------------------------
## Contextual diversity with COCA 2grams:
##------------------------------------------------------------------

## Model COCA 2grams:

summary(xmdl.cd <- glm.nb(CD ~ DominantModality + POS, xagr.rand))
summary(xmdl.nocd <- glm.nb(CD ~ 1 + POS, xagr.rand))
anova(xmdl.cd, xmdl.nocd, test = 'Chisq')

## Test whether negative binomial model is needed:

odTest(xmdl.nb)

## Make a marginal model for plot:

xmdl.plot <- glm.nb(CD ~ DominantModality, xagr.rand)

## Extract predictions:

xmdl.df <- my.predict.lm(xmdl.plot, link = 'log')



##------------------------------------------------------------------
## Contextual diversity with SUBTLEX movies:
##------------------------------------------------------------------

## Merge SUBTLEX into xagr.rand:

xagr.rand$SubCD <- SUB[match(xagr.rand$Word, SUB$Word),]$CDcount

## Model SUBTLEX movie couns:

summary(xmdl.sub <- glm.nb(SubCD ~ DominantModality + POS, xagr.rand))
summary(xmdl.sub.nocd <- glm.nb(CD ~ 1 + POS, xagr.rand))
anova(xmdl.sub.nocd, xmdl.sub, test = 'Chisq')

## Make a marginal model for plot:

xmdl.plotSUB <- glm.nb(SubCD ~ DominantModality, xagr.rand)

## Extract predictions:

xmdl.dfSUB <- my.predict.lm(xmdl.plotSUB, link = 'log')



##------------------------------------------------------------------
## Plot COCA and SUBTLEX contextual diversity (not shown in thesis):
##------------------------------------------------------------------

setup_plots(N = 2)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 1750), AB = '(a)')
draw_preds(xmdl.df)
lower_axis(type = 2, N = xagr.rand$DominantModality)
top_labels(first_text = 'COCA 2grams', second_text = '')
left_axis(at = seq(0, 1750, 250),
	text = '', type = 1)
mtext(side = 2, line = 4.5, 'Predicted contextual diversity', cex = 1.85, font = 2)
emptyplot(xlim = c(0.5, 5.5), ylim = c(0, 1750), AB = '(b)')
draw_preds(xmdl.dfSUB)
lower_axis(type = 2, N = xagr.rand$DominantModality)
top_labels(first_text = 'SUBTLEX subtitle corpus', second_text = '')




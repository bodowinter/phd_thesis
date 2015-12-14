## Bodo Winter
## Created: September 20, 2015
## Analysis for Ch. 2.3, 'Using Lexical differentiation and specialization of the senses'

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

## Load in plotting functions:

source(file.path(mainPath, 'scripts/plotting_functions.R'))

## Load in libraries:

library(lme4)
library(reshape2)
library(diptest)
library(dplyr)

## Take random subset:

v <- filter(v, RandomSet == 'yes')


##------------------------------------------------------------------
## Table of means perceptual strength ratings (not reported):
##------------------------------------------------------------------

## Aggregate means:

M <- c(colMeans(dplyr::select(l, VisualStrengthMean:OlfactoryStrengthMean)),
	colMeans(dplyr::select(n, VisualStrengthMean:OlfactoryStrengthMean)),
     colMeans(dplyr::select(vrand, VisualStrengthMean:OlfactoryStrengthMean)))
M <- matrix(M, nrow = 3, byrow = T)

## Overall average:

M <- rbind(M, colMeans(M))

## Make the table presentable for externalization:

M <- round(M, 2)
rownames(M) <- c('Adjectives', 'Nouns', 'Verb', 'Average')
colnames(M) <- c('Vision', 'Hearing', 'Touch', 'Taste', 'Smell')



##------------------------------------------------------------------
## Check counts and do analysis of it:
##------------------------------------------------------------------

## How many per modality?:

counts <- rbind(table(l$DominantModality),
	table(n$DominantModality))

## Create table based on random set variable:

v$DominantModality <- factor(v$DominantModality,
     levels = c('Auditory','Gustatory','Haptic','Olfactory','Visual'))
xtab <- table(v[v$RandomSet == 'yes',]$DominantModality)

## Merge this with the counts table:

counts <- rbind(counts, xtab)

## Fix rownames:

rownames(counts) <- c('lyn','nn','vb')

## Re-order modalities:

counts <- counts[,c('Visual', 'Haptic', 'Auditory', 'Gustatory', 'Olfactory')]

## Rename:

colnames(counts) <- c('Vision', 'Hearing', 'Touch', 'Taste', 'Smell')

## Proportions:

apply(counts, 1, FUN = function(x) x / sum(x))

## Do a chi-square test of each:

helper_function <- function(x) {
	xchisq <- chisq.test(x)
	paste(xchisq$parameter, round(xchisq$statistic, 2), round(xchisq$p.value, 5), collapse = ' , ')
	}
apply(counts, 1, helper_function)

## Append these chi-square tests to external table:

counts_ext <- cbind(counts, apply(counts, 1, helper_function))
rownames(counts_ext) <- c('Adjectives', 'Nouns', 'Verbs')



##------------------------------------------------------------------
## Plot of adjective distributions:
##------------------------------------------------------------------

## Make a plot, only for Lynott and Connell (2009):

text_height_factor <- 0.005
setup_plots(5)
emptyplot(xlim = c(-0.5, 5.5), ylim = c(0, 0.6), yaxs = 'i',
	AB = '(a)', yfactor = 0.07)	# plot 1
mtext(side = 3, line = 1, text = 'Visual Strength', cex = 1.5, font = 2)
plot_density(l$VisualStrengthMean)
axis(side = 1, at = 0:5, labels = 0:5, lwd.ticks = 2, font = 2, cex.axis = 1.75)
axis(side = 2, at = seq(0, 0.6, 0.2), las = 2, cex.axis = 1.5, font = 2, lwd.ticks = 2 )
mtext(side = 2, text = "Density", line = 3.75, font = 2, cex = 1.5)
box(lwd = 2)
## Add examples:
segments(x0 = 4.5, x1 = 3.7, y0 = 0.4, y1 = 0.47, lwd = 2)
text(x = 3.7, y = 0.49 + text_height_factor, labels = 'chubby', font = 2, cex = 1.35)
segments(x0 = 4.3, x1 = 3.2, y0 = 0.3, y1 = 0.34, lwd = 2)
text(x = 3, y = 0.36 + text_height_factor, labels = 'yellow', font = 2, cex = 1.35)

emptyplot(xlim = c(-0.5, 5.5), ylim = c(0, 0.6), yaxs = 'i',
	AB = '(b)', yfactor = 0.07)	# plot 2
mtext(side = 3, line = 1, text = 'Tactile Strength', cex = 1.5, font = 2)
plot_density(l$HapticStrengthMean)
axis(side = 1, at = 0:5, labels = 0:5, lwd.ticks = 2, font = 2, cex.axis = 1.75)
box(lwd = 2)
## Add examples:
segments(x0 = 3.8-0.1, x1 = 3.2-0.2, y0 = 0.05+0.06, y1 = 0.15+0.1, lwd = 2)
text(x = 3.2-0.1, y = 0.27 + text_height_factor, labels = 'scratchy', font = 2, cex = 1.35)
segments(x0 = 4.3-0.2, x1 = 4.9-0.2, y0 = 0.15, y1 = 0.27+0.05, lwd = 2)
text(x = 4.9-0.2, y = 0.34 + text_height_factor, labels = 'weightless', font = 2, cex = 1.35)

emptyplot(xlim = c(-0.5, 5.5), ylim = c(0, 0.6), yaxs = 'i',
	AB = '(c)', yfactor = 0.07)	# plot 3
mtext(side = 3, line = 1, text = 'Auditory Strength', cex = 1.5, font = 2)
plot_density(l$AuditoryStrengthMean)
axis(side = 1, at = 0:5, labels = 0:5, lwd.ticks = 2, font = 2, cex.axis = 1.75)
box(lwd = 2)
## Add examples:
segments(x0 = 4.8, x1 = 4.2, y0 = 0.07, y1 = 0.24, lwd = 2)
text(x = 4.2, y = 0.26 + text_height_factor, labels = 'quiet', font = 2, cex = 1.35)
segments(x0 = 4.5, x1 = 3.3, y0 = 0.09, y1 = 0.15, lwd = 2)
text(x = 3.3, y = 0.17 + text_height_factor, labels = 'mumbling', font = 2, cex = 1.35)

emptyplot(xlim = c(-0.5, 5.5), ylim = c(0, 0.6), yaxs = 'i',
	AB = '(d)', yfactor = 0.07)	# plot 4
mtext(side = 3, line = 1, text = 'Gustatory Strength', cex = 1.5, font = 2)
plot_density(l$GustatoryStrengthMean)
axis(side = 1, at = 0:5, labels = 0:5, lwd.ticks = 2, font = 2, cex.axis = 1.75)
axis(side = 2, at = seq(0, 0.6, 0.2), las = 2, cex.axis = 1.5, font = 2, lwd.ticks = 2 )
mtext(side = 2, text = "Density", line = 3.75, font = 2, cex = 1.5)
box(lwd = 2)
## Add examples:
segments(x0 = 3.8+0.1, x1 = 3.2+0.1, y0 = 0.02, y1 = 0.15-0.03, lwd = 2)
text(x = 3.2, y = 0.14 + text_height_factor, labels = 'fresh', font = 2, cex = 1.35)
segments(x0 = 4.3, x1 = 4.9, y0 = 0.05, y1 = 0.25-0.02, lwd = 2)
text(x = 4.9, y = 0.25 + text_height_factor, labels = 'tasteless', font = 2, cex = 1.35)

emptyplot(xlim = c(-0.5, 5.5), ylim = c(0, 0.6), yaxs = 'i',
	AB = '(e)', yfactor = 0.07)	# plot 5
mtext(side = 3, line = 1, text = 'Olfactory Strength', cex = 1.5, font = 2)
plot_density(l$OlfactoryStrengthMean)
axis(side = 1, at = 0:5, labels = 0:5, lwd.ticks = 2, font = 2, cex.axis = 1.75)
box(lwd = 2)
## Add examples:
segments(x0 = 3.8, x1 = 3.2, y0 = 0.05+0.02, y1 = 0.15+0.02, lwd = 2)
text(x = 3.2, y = 0.19 + text_height_factor, labels = 'sweaty', font = 2, cex = 1.35)
segments(x0 = 4.3-0.1, x1 = 4.9-0.1, y0 = 0.05, y1 = 0.25, lwd = 2)
text(x = 4.9-0.1, y = 0.27 + text_height_factor, labels = 'musky', font = 2, cex = 1.35)



##------------------------------------------------------------------
## LMER Analysis of mean perceptual strength ratings:
##------------------------------------------------------------------

## Define vector with relevant column names and ID variables:

column_names <- grep('StrengthMean', colnames(l), value = T)
id_vars <- c('Word', 'DominantModality')

## Reshape modality strengths:

melt(l[, c(column_names, id_vars)], id.vars = id_vars) %>%
	rename(WhichModality = variable, Strength = value) -> llong
melt(n[, c(column_names, id_vars)], id.vars = id_vars) %>%
	rename(WhichModality = variable, Strength = value) -> nlong
melt(vrand[, c(column_names, id_vars)], id.vars = id_vars) %>%
     rename(WhichModality = variable, Strength = value) -> vlong

## Clean the content of the WhichModality column:

llong <- mutate(llong, WhichModality = gsub('Mean', '', WhichModality))
nlong <- mutate(nlong, WhichModality = gsub('Mean', '', WhichModality))
vlong <- mutate(vlong, WhichModality = gsub('Mean', '', WhichModality))

## Alltogether:

xall <- rbind(llong, nlong, vlong)
xall$POS <- c(rep('adj',nrow(llong)), rep('noun',nrow(nlong)), rep('verb',nrow(vlong)))

## Overall model:

xall.mdl <- lmer(Strength ~ POS * WhichModality + (1+WhichModality|Word), xall, REML = F)
xall.noint <- lmer(Strength ~ POS + WhichModality + (1+WhichModality|Word), xall, REML = F)
xall.noPOS <- lmer(Strength ~ WhichModality + (1+WhichModality|Word), xall, REML = F)	# warning
xall.noMod <- lmer(Strength ~ POS + (1+WhichModality|Word), xall, REML = F)

## Likelihood ratio tests:

anova(xall.noint, xall.mdl, test = 'Chisq')		# test of modality * POS interaction
anova(xall.noMod, xall.noint, test = 'Chisq')	# test of modality
anova(xall.noPOS, xall.noint, test = 'Chisq')	# test of POS



##------------------------------------------------------------------
## Bimodality analysis:
##------------------------------------------------------------------

## Setup a table to be filled with bimodality values:

M <- matrix(rep(NA, 15), nrow = 3)
rownames(M) <- c('l', 'n', 'v')	# dataset names
colnames(M) <- column_names
M_pvals <- M		# copy for p-values

## Loop through this:

for (i in 1:3) {
	for (j in 1:5) {
		
		dip_res <- dip.test(get(rownames(M)[i])[, colnames(M)[j]])
		
		M[i, j] <- dip_res$statistic
		M_pvals[i, j] <- dip_res$p.value
		
		}
	}

## Overall average:

M <- rbind(M, colMeans(M))

## Make table presentable for externalization:

M <- round(M * 100, 2)	# multiply by 100 for ease of interpretation
rownames(M) <- c('Adjectives', 'Nouns', 'Verbs', 'Average')
colnames(M) <- c('Vision', 'Hearing', 'Touch', 'Taste', 'Smell')



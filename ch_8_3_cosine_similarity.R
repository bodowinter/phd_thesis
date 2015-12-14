## Bodo Winter
## September 18, 2015; November 11, 2015
## Relationships between sensory modalities

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Options:

options(stringsAsFactors = F)

## Load in libraries:

library(dplyr)

## Define path to parent directory:

mainPath <- '/Users/teeniematlock/Desktop/sense_phd/analysis'

## Load in plotting and prediction functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))
source(file.path(mainPath, 'functions/model_prediction_functions.R'))

## Load in COCA data:

setwd(file.path(mainPath, 'data/'))
adj <- read.csv('COCA_adj_noun.csv')

## Load in modality norms:

l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')

## Add adjective perceptual strengths:

modalities <- c('VisualStrengthMean', 'HapticStrengthMean', 'AuditoryStrengthMean',
	'GustatoryStrengthMean', 'OlfactoryStrengthMean')
adj_norms <- l[match(adj$Word, l$Word), c(modalities, 'DominantModality', 'ModalityExclusivity')]
colnames(adj_norms) <- paste0('Adj',colnames(adj_norms))
adj <- cbind(adj, adj_norms)
rm(adj_norms)

## Add noun perceptual strengths:

noun_norms <- n[match(adj$Noun, n$Word), c(modalities, 'DominantModality', 'ModalityExclusivity')]
colnames(noun_norms) <- paste0('Noun',colnames(noun_norms))
adj <- cbind(adj, noun_norms)
rm(noun_norms)

## Change rownames back to normal:

rownames(adj) <- 1:nrow(adj)

## Compute log frequency:

adj <- mutate(adj, LogFreq = log10(Freq))

## Load in Warriner dataset:

aff <- read.csv('warriner_2013_affective_norms.csv')

## Create absolute valnce norms:

aff <- mutate(aff, AbsV = abs(Val - mean(Val)))

## Merge affect into 'adj':

adj$AdjAbsV <- aff[match(adj$Word, aff$Word),]$AbsV



##------------------------------------------------------------------
## Compute modality fit between adj-noun pairs (cosine similarity):
##------------------------------------------------------------------

## Cosine similarity function:

cosine_sim <- function(x, y) {
	numerator <- x %*% y
	denominator <- sqrt(x %*% x * y %*% y)
	return(as.vector(numerator/denominator))
	}

## Create a subset for which both adj and noun norms exist:

mcos <- filter(adj, !is.na(NounVisualStrengthMean))

## Create empty cosine column to fill with modality fit:

mcos$Cosine <- NA

## Loop through and compute the cosine of the adj and the noun modality strenths:

for (i in 1:nrow(mcos)) {
	A <- unlist(mcos[i,grep('Noun(.)+StrengthMean', colnames(adj))])
	B <- unlist(mcos[i,grep('Adj(.)+StrengthMean', colnames(adj))])
	mcos[i,]$Cosine <- cosine_sim(A, B)
	if (i %% 1000 == 0) {cat(paste(i, '\n'))}
	}

## Make a plot of the cosine distribution:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(0, 5), yaxs = 'i')
plot_density(mcos$Cosine, powerpoint = F, mean = F)
left_axis(text = 'Density', at = seq(0, 5, 1), type = 1)
mtext(side = 3, line = 1, text = 'Modality Compatibility', cex = 1.5, font = 2)
lower_axis(style = 'continuous', lab = 'Cosine Similarity', at = seq(0, 1, 0.25), type = 1)
box(lwd = 2)
segments(x0 = 0.95, x1 = 0.65, y0 = 2, y1 = 3, lwd = 3)
text(x = 0.65, y = 3.15, labels = 'abrasive contact', font = 2, cex = 1.35)
segments(x0 = 0.12, x1 = 0.25, y0 = 0.1, y1 = 1, lwd = 3)
text(x = 0.25, y = 1.15, labels = 'fragrant music', font = 2, cex = 1.35)



##------------------------------------------------------------------
## Frequency analysis and plots (not discussed in body of the text):
##------------------------------------------------------------------

## Correlate this WarrValDiff with frequency:

summary(mod.match.mdl <- lm(LogFreq ~ Cosine, mcos))

## Conduct test with heteroskedasticity corrected standard errors:

library(lmtest)
library(sandwich)
coeftest(mod.match.mdl, vcov = vcovHC)
waldtest(mod.match.mdl, vcov = vcovHC)

## The plot:

setup_plots(N = 1)
emptyplot(xlim = c(0, 1), ylim = c(0, 5), AB = '')	# modality match
points(mcos$Cosine, mcos$LogFreq,
	pch = 19, cex = 0.8, col = rgb(0,0,0,0.4))
left_axis(text = 'Log Frequency', at = seq(0, 5, 1), type = 1)
mtext(side = 3, line = 1, text = 'Modality Match', cex = 1.5, font = 2)
lower_axis(style = 'continuous', lab = 'Cosine Similarity', type = 1,
	at = seq(0, 1, 0.25))



##------------------------------------------------------------------
## Compute baseline cosine values through random pairing:
##------------------------------------------------------------------

## Create a baseline cosine value (random pairing):

random_cosines <- numeric(10000)
set.seed(42)
for (i in 1:10000) {
	this_adj_row <- sample(1:nrow(mcos), 1)
	this_adj <- mcos[this_adj_row, ]$Word
	these_noun_collocates <- unique(mcos[mcos$Word == this_adj,]$Noun)
	possible_noun_choices <- (1:nrow(mcos))[!(mcos$Noun %in% these_noun_collocates)]
	this_noun <- sample(possible_noun_choices, 1)
	A <- unlist(mcos[this_noun, grep('Noun(.)+StrengthMean', colnames(mcos))])
	B <- unlist(mcos[this_adj_row, grep('Adj(.)+StrengthMean', colnames(mcos))])
	random_cosines[i] <- cosine_sim(A, B)
	if (i %% 1000 == 0) {cat(paste(i, '\n'))}	
	}

## Average:

mean(random_cosines)

## Test random cosines against real cosines:

wilcox.test(random_cosines, mcos$Cosine, paired = F)



##------------------------------------------------------------------
## See whether the cosine is related to affect and iconicity:
##------------------------------------------------------------------

## Take affect subset and reduce to those where there are also cosines:

comb <- warr[!is.na(warr$Cosine),]
nrow(comb)		# 11,971

## Correlate this WarrValDiff with frequency:

summary(cos.absadj.mdl <- lm(AdjAbsV ~ Cosine, comb))

## Conduct test with heteroskedasticity corrected standard errors:

coeftest(cos.absadj.mdl, vcov = vcovHC)
waldtest(cos.absadj.mdl, vcov = vcovHC)

## Add iconicity:

icon <- read.csv('iconicity_ratings.csv')
mcos$Iconicity <- icon[match(mcos$Word, icon$Word),]$Iconicity

## Make a model:

summary(cos.icon <- lm(Iconicity ~ Cosine, mcos))

## Conduct test with heteroskedasticity corrected standard errors:

coeftest(cos.icon, vcov = vcovHC)
waldtest(cos.icon, vcov = vcovHC)

## Make predictions for plot, absolute valence and iconicity:

newdata <- data.frame(Cosine = seq(0, 1, 0.01))
newaff <- newdata
newicon <- newdata

newaff$fit <- predict(cos.absadj.mdl, newaff)
newaff$UB <- newaff$fit + 1.96 * predict(cos.absadj.mdl, newaff, se.fit = T)$se.fit
newaff$LB <- newaff$fit - 1.96 * predict(cos.absadj.mdl, newaff, se.fit = T)$se.fit

newicon$fit <- predict(cos.icon, newicon)
newicon$UB <- newicon$fit + 1.96 * predict(cos.absadj.mdl, newicon, se.fit = T)$se.fit
newicon$LB <- newicon$fit - 1.96 * predict(cos.absadj.mdl, newicon, se.fit = T)$se.fit

## Make a plot of modality match versus valence match:

yfactor <- 0.03
quartz('', 12, 5)
par(mfrow = c(1, 2), omi = c(1, 1.1, 0.85, 1.5), mai = c(0, 0.25, 0, 0.25))
emptyplot(xlim = c(0, 1), ylim = c(-0.5, 5), AB = '(a)')
points(comb$Cosine, comb$AdjAbsV,
	pch = 19, cex = 0.8, col = rgb(0,0,0,0.25))
polygon(x = c(0, 1, 1, 0),
	y = c(newaff$fit[1] - yfactor, newaff$fit[101] - yfactor,
		newaff$fit[101] + yfactor, newaff$fit[1] + yfactor),
	col = 'white', border = 'black')
axis(side = 2, at = seq(0, 5, 1), lwd = 2, font = 2, cex.axis = 1.5, las = 2)
mtext(side = 2, text = 'Absolute Valence (Adj)', line = 3.5, font = 2, cex = 2)
lower_axis(style = 'continuous', lab = 'Cosine Similarity', type = 1,
	at = seq(0, 1, 0.25))

## Make a plot of modality match versus absolute valence:

yfactor <- 0.05
emptyplot(xlim = c(0, 1), ylim = c(-3, 6), AB = '(b)')
points(mcos$Cosine, mcos$Iconicity,
	pch = 19, cex = 0.8, col = rgb(0,0,0,0.25))
polygon(x = c(0, 1, 1, 0),
	y = c(newicon$fit[1] - yfactor, newicon$fit[101] - yfactor,
		newicon$fit[101] + yfactor, newicon$fit[1] + yfactor),
	col = 'white', border = 'black')
mtext(side = 4, text = 'Iconicity', line = 4.2, font = 2, cex = 2)
axis(side = 4, at = seq(-3, 6, 1.5), lwd = 2, font = 2, cex.axis = 1.5, las = 2)
lower_axis(style = 'continuous', lab = 'Cosine Similarity', type = 1,
	at = seq(0, 1, 0.25))





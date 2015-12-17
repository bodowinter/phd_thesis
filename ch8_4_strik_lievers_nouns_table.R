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

## Load in plotting functions:

source(file.path(mainPath, 'functions/plotting_functions.R'))

## Load in data:

setwd(file.path(mainPath, 'data/'))
l <- read.csv('lynott_connell_2009_adj_norms.csv')
strik_nouns <- read.csv('strik_lievers_2015_nouns_with_freq.csv')	# strik noun with modality

## Load in COCA and add adjective frequency:

COCA <- read.csv('COCA_adj_noun.csv')			# overall adjective dataset
adj_freq <- aggregate(Freq ~ Word, COCA, sum)
l$Freq <- adj_freq[match(l$Word, adj_freq$Word),]$Freq

## Append adjective and strik:

strik <- COCA
strik$Modality <- strik_nouns[match(strik$Noun, strik_nouns$Word),]$Modality

## Reduce to those that are not NA:

nrow(strik)	# 149,387
strik <- filter(strik, !is.na(Modality))
nrow(strik)	# 4,685

## Merge adjective modalities into strik dataset:

strik$AdjModality <- l[match(strik$Word, l$Word),]$DominantModality

## Create a same vs. different variable for Strik:

strik$Same <- ifelse(strik$Modality == strik$AdjModality, 'yes', 'no')

## Add dimensionality information and get rid of dimension words:

sem <- read.csv('lynott_connell_2009_semantic_codings.csv')
strik$Dimension <- sem[match(strik$Word, sem$Word), ]$Dimension

## Get rid of dimension words:

nrow(strik)	# 4,685
strik <- filter(strik, Dimension == 'no')
nrow(strik)	# 3,860

## Get rid of dimension words in the Lynott dataset (from which the simulation will be drawn):

l$Dimension <- sem[match(l$Word, sem$Word), ]$Dimension
l <- filter(l, Dimension == 'no')

## Get rid of instruments in both datasets, strik dataset:

nrow(strik)	# 3,869
strik <- strik[!(strik$Noun %in% strik_nouns[strik_nouns$Instrument == 'yes',]$Word),]
nrow(strik)	# 3,713

## Get rid of instruments in both datasets, strik noun dataset (later for loop):

nrow(strik_nouns)	# 219
strik_nouns <- filter(strik_nouns, Instrument == 'no')
nrow(strik_nouns)	# 193

## Check some examples:

head(arrange(strik[strik$Same == 'no',], desc(Freq)), 20)
tail(arrange(strik[strik$Same == 'no',], desc(Freq)), 20)



##------------------------------------------------------------------
## Create frequency Ullman-style table with token frequencies:
##------------------------------------------------------------------

## Summarize token frequencies:

strik_tokens <- aggregate(Freq ~ Modality * AdjModality, strik, sum)

## Create an empty table:

stab <- matrix(numeric(25), nrow = 5)
rownames(stab) <- c('Haptic', 'Gustatory', 'Olfactory', 'Auditory', 'Visual')
colnames(stab) <- rownames(stab)

## Loop through the tables and add token frequencies:

for (i in 1:nrow(stab)) {
	for (j in 1:nrow(stab)) {
			this_row <- strik_tokens$AdjModality == rownames(stab)[i]

			strik_row <- this_row & strik_tokens$Modality == colnames(stab)[j]
			
			stab[i,j] <- strik_tokens[strik_row,]$Freq
		}
	}

## Look at it:

stab
rowSums(stab)
colSums(stab)
sum(stab)

## Without diagonal:

nodiag <- stab
diag(nodiag) <- 0
rowSums(nodiag)
colSums(nodiag)

## How many same modality cases?

sum(diag(stab)) / sum(stab)



##------------------------------------------------------------------
## Looking at unique adjective-noun pair (types) rather than tokens:
##------------------------------------------------------------------
## (not reported in main body of the texT)

## Create table:

ptab <- table(strik$AdjModality, strik$Modality)
ptab <- ptab[rownames(stab),rownames(stab)]

## How many unique words are mapped?

utab <- matrix(numeric(25), nrow = 5)
rownames(utab) <- c('Haptic', 'Gustatory', 'Olfactory', 'Auditory', 'Visual')
colnames(utab) <- rownames(utab)

## Loop through the tables and add token frequencies:

for (i in 1:nrow(utab)) {
	for (j in 1:nrow(utab)) {
			this_row <- strik$AdjModality == rownames(utab)[i]

			strik_row <- this_row & strik$Modality == colnames(utab)[j]
			
			these_adjs <- unique(strik[strik_row,]$Word)
			N_adj <- length(these_adjs)
			
			utab[i,j] <- N_adj
		}
	}
utab

## How many adjectives are mapped in percentages?

## Create an empty table:

mapped <- matrix(numeric(25), nrow = 5)
rownames(mapped) <- c('Haptic', 'Gustatory', 'Olfactory', 'Auditory', 'Visual')
colnames(mapped) <- rownames(mapped)

## Loop through the tables and add token frequencies:

for (i in 1:nrow(mapped)) {
	for (j in 1:nrow(mapped)) {
			this_row <- strik$AdjModality == rownames(mapped)[i]

			strik_row <- this_row & strik$Modality == colnames(mapped)[j]
			
			these_adjs <- unique(strik[strik_row,]$Word)
			N_adj <- length(these_adjs)
			N_full <- nrow(l[l$DominantModality == rownames(mapped)[i],])
			
			mapped[i,j] <- N_adj / N_full
		}
	}
round(mapped, 2)

## Get source and target means:

round(rowMeans(mapped), 2)
round(colMeans(mapped), 2)

## Get rid of diagonal to get means:

mapnodiag <- mapped
diag(mapnodiag) <- 0

round(rowMeans(mapnodiag), 2)
round(colMeans(mapnodiag), 2)



##------------------------------------------------------------------
## Preprocessing for logistic regression analysis:
##------------------------------------------------------------------

## Which adjectives are increased in frequency, first, get strik counts:

sfreq <- aggregate(Freq ~ Word, strik[strik$Same == 'no',], sum)

## Create a mapped column:

sfreq$Mapped <- 'yes'

## Create data frame of those that were not mapped:

notmapped <- data.frame(Word = setdiff(l$Word, sfreq$Word))
notmapped$Freq <- 0
notmapped$Mapped <- 'no'

## Merge:

sfreq <- rbind(sfreq, notmapped)

## Then, get full counts:

sfreq$BaseFreq <- adj_freq[match(sfreq$Word, adj_freq$Word),]$Freq

## Exclude those that are never attested:

sfreq <- filter(sfreq, !is.na(sfreq$BaseFreq))

## Ratio and log ratio:

sfreq <- mutate(sfreq, 
	Ratio = Freq / BaseFreq)

## Order:

sfreq <- arrange(sfreq, desc(Ratio))
	
## Add information:

sfreq$Modality <- l[match(sfreq$Word, l$Word),]$DominantModality
sfreq$Exclusivity <- l[match(sfreq$Word, l$Word),]$ModalityExclusivity

## Add valence information:

val <- read.csv('adjective_context_valence.csv')
sfreq$AbsV <- val[match(sfreq$Word, val$Word),]$AbsV
sfreq$AbsSent <- val[match(sfreq$Word, val$Word),]$AbsSent
sfreq$ValMax <- val[match(sfreq$Word, val$Word),]$ValMax

## Add iconicity:

icon <- read.csv('iconicity_ratings.csv')
sfreq$Iconicity <- icon[match(sfreq$Word, icon$Word),]$Iconicity

## Add semantic information:

sfreq <- cbind(sfreq, sem[match(sfreq$Word, sem$Word),-1])



##------------------------------------------------------------------
## Logistic regression analysis:
##------------------------------------------------------------------

## For logistic regression model, need to:

sfreq <- mutate(sfreq, Mapped = as.factor(Mapped))

## How does frequency affect this?

summary(xmdl.freq <- glm(Mapped ~ log10(BaseFreq), sfreq, family = 'binomial'))

## Are those words mapped overall more or less valenced?

summary(xmdl.absv <- glm(Mapped ~ AbsV + log10(BaseFreq), sfreq, family = 'binomial'))
summary(glm(Mapped ~ AbsSent + log10(BaseFreq), sfreq, family = 'binomial'))
summary(glm(Mapped ~ ValMax + log10(BaseFreq), sfreq, family = 'binomial'))

## Are those words mapped overall more or less multimodal?

summary(xmdl.excl <- glm(Mapped ~ Exclusivity + log10(BaseFreq), sfreq, family = 'binomial'))

## Are those words mapped overall more or less iconic?

summary(xmdl.icon <- glm(Mapped ~ Iconicity + log10(BaseFreq), sfreq, family = 'binomial'))

## Get predictions of affect:

summary(xmdl.affect <- glm(Mapped ~ AbsV, sfreq, family = 'binomial'))
xpred <- data.frame(AbsV = seq(0, 2, 0.01))
xpred <- cbind(xpred,
	as.data.frame(predict(xmdl.affect, newdata = xpred, type = 'link', se.fit = T)[1:2]))
xpred$UB <- plogis(xpred$fit + 1.96 * xpred$se.fit)
xpred$LB <- plogis(xpred$fit - 1.96 * xpred$se.fit)
xpred$fit <- plogis(xpred$fit)

## Get predictions of iconicity:

summary(xmdl.icon <- glm(Mapped ~ Iconicity, sfreq, family = 'binomial'))
xpred.icon <- data.frame(Iconicity = seq(-2.5, 4.5, 0.01))
xpred.icon <- cbind(xpred.icon,
	as.data.frame(predict(xmdl.icon, newdata = xpred.icon, type = 'link', se.fit = T)[1:2]))
xpred.icon$UB <- plogis(xpred.icon$fit + 1.96 * xpred.icon$se.fit)
xpred.icon$LB <- plogis(xpred.icon$fit - 1.96 * xpred.icon$se.fit)
xpred.icon$fit <- plogis(xpred.icon$fit)

## Plot affect and iconicity together:

set.seed(42)
quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(1, 1.35, 0.85, 0.25), mai = c(0, 0.25, 0, 0))
emptyplot(xlim = c(0.5, 1.5), ylim = c(-0.1, 1.1))
points(sfreq$AbsV, jitter(as.numeric(sfreq$Mapped) - 1, 0.2), col = rgb(0, 0, 0, 0.4), pch = 19)
polygon(x = c(xpred$AbsV, rev(xpred$AbsV)),
	y = c(xpred$UB, rev(xpred$LB)), border = F, col = rgb(0, 0, 0, 0.4))
points(xpred$AbsV, xpred$fit, lwd = 3, type = 'l')
abline(h = c(0, 1), lwd = 2, lty = 2)
axis(side = 2, at = c(0, 1), las = 2, font = 2, labels = c('Not mapped', 'Mapped'), cex.axis = 1.25)
axis(side = 1, cex.axis = 1.25, lwd.ticks = 2, font = 2, at = seq(0, 2, 0.5))
mtext('Absolute Valence', side = 1, line = 3, font = 2, cex = 1.6)
emptyplot(xlim = c(-2.5, 5), ylim = c(-0.1, 1.1))
points(sfreq$Iconicity, jitter(as.numeric(sfreq$Mapped) - 1, 0.2), col = rgb(0, 0, 0, 0.4), pch = 19)
polygon(x = c(xpred.icon$Iconicity, rev(xpred.icon$Iconicity)),
	y = c(xpred.icon$UB, rev(xpred.icon$LB)), border = F, col = rgb(0, 0, 0, 0.4))
points(xpred.icon$Iconicity, xpred.icon$fit, lwd = 3, type = 'l')
abline(h = c(0, 1), lwd = 2, lty = 2)
axis(side = 1, cex.axis = 1.25, lwd.ticks = 2, font = 2, at = seq(-2.5, 5, 2.5))
mtext('Iconicity', side = 1, line = 3, font = 2, cex = 1.6)


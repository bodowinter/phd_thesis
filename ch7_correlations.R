## Bodo Winter
## September 18, 2015
## Analysis for Ch. 7.2, 'Modality correlations in adjective-noun pairs'

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
COCA <- read.csv('COCA_adj_noun.csv')

## Load in modality norms:

l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2015_verb_norms.csv')

## Take only the random subset of the verbs:

v <- filter(v, RandomSet == 'yes')

## Add adjective perceptual strengths:

modalities <- c('VisualStrengthMean', 'HapticStrengthMean', 'AuditoryStrengthMean',
	'GustatoryStrengthMean', 'OlfactoryStrengthMean')
adj_norms <- l[match(COCA$Word, l$Word), c(modalities, 'DominantModality', 'ModalityExclusivity')]
colnames(adj_norms) <- paste0('Adj', colnames(adj_norms))
COCA <- cbind(COCA, adj_norms)
rm(adj_norms)

## Add noun perceptual strengths:

noun_norms <- n[match(COCA$Noun, n$Word), c(modalities, 'DominantModality', 'ModalityExclusivity')]
colnames(noun_norms) <- paste0('Noun', colnames(noun_norms))
COCA <- cbind(COCA, noun_norms)
rm(noun_norms)

## Change rownames back to normal:

rownames(COCA) <- 1:nrow(COCA)

## Compute log frequency:

COCA <- mutate(COCA, LogFreq = log10(Freq))



##------------------------------------------------------------------
## Correlation matrices for adjective-noun pairs:
##------------------------------------------------------------------

## Take only those for which both perceptual strength values exist:

adj_red <- filter(COCA, !is.na(NounVisualStrengthMean))

## For creating weights, create frequencies:

adj_freqs <- aggregate(Freq ~ Word, adj_red, sum)

## For creating weights, merge the frequencies into the table:

adj_red$AdjFreq <- adj_freqs[match(adj_red$Word, adj_freqs$Word), ]$Freq

## Create weights:

adj_red <- mutate(adj_red, Weight = Freq / AdjFreq)

## Get the reducd dataset with averages (weighted mean):

group_by(adj_red, Word) %>% summarise(Freq = sum(Freq),
	AdjVisualStrengthMean = mean(AdjVisualStrengthMean),
	AdjAuditoryStrengthMean = mean(AdjAuditoryStrengthMean),
	AdjHapticStrengthMean = mean(AdjHapticStrengthMean),
	AdjGustatoryStrengthMean = mean(AdjGustatoryStrengthMean),
	AdjOlfactoryStrengthMean = mean(AdjOlfactoryStrengthMean),
	NounVisualStrengthMean = weighted.mean(NounVisualStrengthMean, w = Weight),
	NounAuditoryStrengthMean = weighted.mean(NounAuditoryStrengthMean, w = Weight),
	NounHapticStrengthMean = weighted.mean(NounHapticStrengthMean, w = Weight),
	NounGustatoryStrengthMean = weighted.mean(NounGustatoryStrengthMean, w = Weight),
	NounOlfactoryStrengthMean = weighted.mean(NounOlfactoryStrengthMean, w = Weight)) -> adj_agr

## Set up matrix for first-order correlations:

noun_cors <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors) <- paste0('Adj', modalities)
colnames(noun_cors) <- paste0('Noun', modalities)

## Set up matrix for p-values:

noun_cors_pvals <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(noun_cors_pvals) <- rownames(noun_cors)
colnames(noun_cors_pvals) <- colnames(noun_cors)

## Loop through matrix to fill each cell with first order correlations:

for (i in 1:5) {
	adjective_modality <- rownames(noun_cors)[i]
	
	for (j in 1:5) {
		noun_modality <- colnames(noun_cors)[j]
		
		cor.temp <- cor.test(unlist(adj_agr[,adjective_modality]), unlist(adj_agr[,noun_modality]))
		
		noun_cors[i, j] <- cor.temp$estimate	# correlation coefficients
		noun_cors_pvals[i, j] <- cor.temp$p.value		# p-values
		
		}
	}

## Multiple comparisons corrected p-values:

round(noun_cors, 2)
noun_cors_pvals < (0.05/25)



##------------------------------------------------------------------
## Intra-word correlations:
##------------------------------------------------------------------
## In analogy to Louwerse & Connell (2011), not reported in text

## Combine all:

xagr <- rbind(dplyr::select(l, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(n, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity),
	dplyr::select(v, Word, DominantModality, VisualStrengthMean:OlfactoryStrengthMean,
	ModalityExclusivity))

## Extract only the strength values to be correlated:

xagr_cors_data <- dplyr::select(xagr, VisualStrengthMean:OlfactoryStrengthMean)

## Set up matrix for first-order correlations:

xagr_cors <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(xagr_cors) <- colnames(xagr_cors_data)
colnames(xagr_cors) <- colnames(xagr_cors_data)

## Set up matrix for p-values:

xagr_cors_pvals <- matrix(rep(NA, 5 * 5), nrow = 5)
rownames(xagr_cors_pvals) <- colnames(xagr_cors_data)
colnames(xagr_cors_pvals) <- colnames(xagr_cors_data)

## Loop through matrix to fill each cell with first order correlations:

for (i in 1:5) {
	row_modality <- rownames(xagr_cors)[i]
	
	for (j in 1:5) {
		column_modality <- colnames(xagr_cors)[j]
		
		cor.temp <- cor.test(xagr_cors_data[,row_modality], xagr_cors_data[,column_modality])
		
		xagr_cors[i, j] <- cor.temp$estimate			# correlation coefficients
		xagr_cors_pvals[i, j] <- cor.temp$p.value		# p-values
		
		}
	}

## Multiple comparisons corrected p-values:

round(noun_cors, 2)
xagr_cors_pvals < (0.05/20)




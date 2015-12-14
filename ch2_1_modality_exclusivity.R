## Bodo Winter
## Created: September 20, 2015
## Analysis for Ch. 2.1, 'Using modality norms to characterize the senses'

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in data:

setwd('/Users/teeniematlock/Desktop/sense_phd/data')
l <- read.csv('lynott_connell_2009_adj_norms.csv')
n <- read.csv('lynott_connell_2013_noun_norms.csv')
v <- read.csv('winter_2015_verb_norms.csv')

## Load in library:

library(dplyr)

## Create random and non-random subsets:

vrand <- filter(v, RandomSet == 'yes')
vsel <- filter(v, RandomSet == 'no')



##------------------------------------------------------------------
## Ch. 2.1. Multimodality differences between verbs, nouns and adjs
##------------------------------------------------------------------

## Exclusivities of different verb sets:

mean(v$ModalityExclusivity)
mean(vrand$ModalityExclusivity)
mean(vsel$ModalityExclusivity)

## Wilcoxon test of random versus selected:

wilcox.test(vsel$ModalityExclusivity, vrand$ModalityExclusivity, paired = F)

## Wilcoxon test:

wilcox.test(l$ModalityExclusivity, n$ModalityExclusivity, paired = F)
wilcox.test(l$ModalityExclusivity, v$ModalityExclusivity, paired = F)
wilcox.test(n$ModalityExclusivity, v$ModalityExclusivity, paired = F)




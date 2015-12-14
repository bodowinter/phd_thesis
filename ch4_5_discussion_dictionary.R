## Bodo Winter
## Dec 12, 2015
## Supplementary analysis for Ch. 3.5, 'Discussion'

##------------------------------------------------------------------
## Test of whether dictionary meanings specify pleasantness (MacMillan):
##------------------------------------------------------------------

## Load in dictionary data:

setwd('/Users/teeniematlock/Desktop/sense_phd/analysis/data/')
dic <- read.csv('macmillan_taste_smell_words.csv')

## Load in other data:

l <- read.csv('lynott_connell_2009_adj_norms.csv')
aff <- read.csv('warriner_2013_affective_norms.csv')

## Merge with Lynott:

dic$DominantModality <- l[match(dic$Word, l$Word),]$DominantModality

## Check for association:

table(dic$Dictionary, dic$DominantModality)

## Add valence:

dic$Val <- aff[match(dic$Word, aff$Word),]$Val
aff <- mutate(aff, AbsV = abs(Val - mean(Val)))
dic$AbsV <- aff[match(dic$Word, aff$Word),]$AbsV

## Look at valence:

aggregate(Val ~ Dictionary, dic, mean)
aggregate(AbsV ~ Dictionary, dic, mean)

## Arrange by valence:

arrange(dic, desc(Val))





## Bodo Winter
## Dec 12, 2015
## Supplementary analysis for Ch. 3.5, 'Discussion'

##------------------------------------------------------------------
## Test of whether dictionary meanings specify pleasantness (MacMillan):
##------------------------------------------------------------------

## Load in dictionary data:

dic <- read.csv('taste_and_smell_words_only_for_dictionary_evaluative_meaning_bw.csv')

## Merge with Lynott:

dic$DominantModality <- l[match(dic$Word, l$Word),]$DominantModality

## Test for association:

table(dic$Dictionary, dic$DominantModality)[-2,4:5]
chisq.test(table(dic$Dictionary, dic$DominantModality)[-2,4:5])

## Add valence:

dic$Val <- aff[match(dic$Word, aff$Word),]$V.Mean.Sum
dic$AbsV <- aff[match(dic$Word, aff$Word),]$AbsV

## Look at valence:

aggregate(Val ~ Dictionary, dic, mean)
aggregate(AbsV ~ Dictionary, dic, mean)

## Arrange by valence:

arrange(dic, desc(Val))





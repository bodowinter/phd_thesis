## Bodo Winter
## Created: December 11, 2015
## Installing all packages needed for running the thesis scripts

install.packages('dplyr')

install.packages('reshape')

install.packages('MASS')

install.packages('pscl')

install.packages('party')

install.packages('diptest')

install.packages('lme4')

install.packages('glmmADMB', 
    repos = c('http://glmmadmb.r-forge.r-project.org/repos',
            getOption('repos')),
    type = 'source')


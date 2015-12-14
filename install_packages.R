## Bodo Winter
## Created: December 11, 2015
## Installing all packages needed for running the thesis scripts

install.packages('dplyr')

install.packages('reshape2')

install.packages('MASS')

install.packages('pscl')

install.packages('party')

install.packages('diptest')

install.packages('lme4')

install.packages('sandwich')

install.packages('lmtest')

install.packages('lavaan')

install.packages('xlsx')

install.packages('glmmADMB', 
    repos = c('http://glmmadmb.r-forge.r-project.org/repos',
            getOption('repos')),
    type = 'source')


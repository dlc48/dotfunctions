# cd /Users/Shared/code/r/source/git_dotfunctions/
# R --vanilla

rm(list=ls())


# initial description file: ONLY FIRST TIME
# devtools::create("BATS")


# RUN THIS AFTER AN UPDATE 
devtools::document() 


############################################
# github installation:

# PUSH and then use
devtools::install_github("dlc48/dotfunctions", auth_token = "XXXX")
# where "auth_token" is generated from github.com
# -> dlc48 / settings / Developer settings / Personal access token / 
#    Tokens (classic) / generate new token 


############################################
# local installation:
devtools::build(binary=FALSE)

# install
    # within R
    devtools::install()
    # outside R
    R CMD INSTALL dotfunctions_1.0.0.tar.gz




# build: NO NEED IF INSTALLED FROM GITHUB
# devtools::build(binary=FALSE)


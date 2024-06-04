# cd /Users/Shared/code/r/packages/git_dotfunctions
# R --vanilla

rm(list=ls())
system("find . -name '.DS_Store' -delete")


# initial description file: ONLY FIRST TIME
# devtools::create("BATS")


# RUN THIS AFTER AN UPDATE 
devtools::document() 



############################################
# local install (temporary files for test): 
devtools::load_all()


############################################
# local installation:
devtools::build(binary=FALSE)

# install
    # within R
    devtools::install()
    # outside R
    R CMD INSTALL dotfunctions_1.0.0.tar.gz



############################################
# github installation:

# PUSH and then use for private repo:
devtools::install_github("dlc48/dotfunctions", auth_token = "XXXX")
# where "auth_token" is generated from github.com
# -> dlc48 / settings / Developer settings / Personal access token / 
#    Tokens (classic) / generate new token 

# PUSH and then use for public repo:
devtools::install_github("dlc48/dotfunctions")


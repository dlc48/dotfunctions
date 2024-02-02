# cd /Users/Shared/code/r/source/git_dotfunctions/
# R --vanilla

rm(list=ls())


# initial description file: ONLY FIRST TIME
# devtools::create("BATS")


# 
devtools::document() 

# build: NO NEED IF INSTALLED FROM GITHUB
# devtools::build(binary=FALSE)

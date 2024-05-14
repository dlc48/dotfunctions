#' @name .ip
#' @title shorcut 
#' @description function allowing to install and load CRAN packages 
#' @param pkg string indicating the name of the package to install 
#' @param mirror source mirror: 1 for ETH, any other number for Imperial
#' @param dependencies logical for dependencies
#' @param quiet logical for messages
#' @export
.ip = function(pkg,mirror=1,dependencies=TRUE,quiet=FALSE){
    if(mirror==1){
        install.packages(pkg,repos="https://stat.ethz.ch/CRAN/",dependencies=dependencies,quiet=quiet)
    }else{
        install.packages(pkg,repos="https://cran.ma.imperial.ac.uk/",dependencies=dependencies,quiet=quiet)
    }
    print(pkg)
    write(pkg,file="/Users/Shared/code/r/packages/installed/cran.txt",ncolumns=1,append=TRUE)
    library(pkg,character.only=TRUE)
    }

#' @name .bc
#' @title shorcut 
#' @description function allowing to install and load bioconductor packages 
#' @param ... string indicating the name of the package to install 
#' @param ask logical indicating whether to prompt user before installed packages are updated (argument of BiocManager::install)
#' @param dependencies logical for dependencies
#' @param quiet logical for messages
#' @export
.bc = function(...,ask=TRUE){
    BiocManager::install(...,ask = ask)
    write(...,file="/Users/Shared/code/r/packages/installed/bioconductor.txt",ncolumns=1,append=TRUE)
    library(...,quietly=FALSE)
}

#' @name .ip
#' @title shorcut 
#' @description function allowing to install and load CRAN packages 
#' @param pkg string indicating the name of the package to install 
#' @param mirror source mirror: 1 for ETH, any other number for Imperial
#' @param dependencies logical for dependencies
#' @param quiet logical for messages
#' @param path if not NULL (default), path to a text file to which the name of the installed package will be added
#' @export
.ip = function(pkg, mirror=1, dependencies=TRUE, quiet=FALSE, path=NULL){
    if(mirror==1){
        install.packages(pkg,repos="https://stat.ethz.ch/CRAN/",dependencies=dependencies,quiet=quiet)
    }else{
        install.packages(pkg,repos="https://cran.ma.imperial.ac.uk/",dependencies=dependencies,quiet=quiet)
    }
    print(pkg)
    if(!is.null(path)){
        write(pkg,file=path,ncolumns=1,append=TRUE)
    }   
    library(pkg,character.only=TRUE)
    }

#' @name .bc
#' @title shorcut 
#' @description function allowing to install and load bioconductor packages 
#' @param ... string indicating the name of the package to install 
#' @param ask logical indicating whether to prompt user before installed packages are updated (argument of BiocManager::install)
#' @param path if not NULL (default), path to a text file to which the name of the installed package will be added    
#' @export
.bc = function(..., ask=TRUE, path=NULL){
    BiocManager::install(...,ask = ask)
    if(!is.null(path)){
        write(...,file=path,ncolumns=1,append=TRUE)
    }
    library(...,quietly=FALSE)
}

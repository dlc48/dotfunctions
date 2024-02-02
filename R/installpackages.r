# check the script /scripts/InstallPackagesAfterRInstall.rs to re-install all packages

#' .ip
#'
#' note: files are stored under ">.libPaths"
#' @param pkg name of a package
#' @param mirror source mirror
#' @param dependencies dependencies
#' @param quiet suppress messages
#' @export
#'
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

#' .bc
#'
#' install bioconductor packages
#' @param ... package
#' @param ask ask
#' @export
.bc = function(...,ask=TRUE){

    BiocManager::install(...,ask = ask)
    write(...,file="/Users/Shared/code/r/packages/installed/bioconductor.txt",ncolumns=1,append=TRUE)
    library(...,quietly=FALSE)

    if(FALSE){
# CRAN
cran = unique(read.csv("/Users/Shared/code/r/packages/installed/cran.txt",header=FALSE)[,1])
install.packages(cran)
# BIOCOND
biocond = unique(read.csv("/Users/Shared/code/r/packages/installed/bioconductor.txt",header=FALSE)[,1])
BiocManager::install(biocond)

    }
}

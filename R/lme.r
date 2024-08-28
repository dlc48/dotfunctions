#' @name .lmecov
#' @title lme fit random effect covariance matrix
#' @description extract the estimated random effect covariance matrix of a linear mixed model fitted by means of nlme::lme(). 
#' @param fit a fit of class "lme"
#' @param residual.variance a logical indicating if the residual variance should be included or not. By default, residual.variance=TRUE so that the residual variance is included and the ouput is of size (q x q), where q denotes the number of random effects plus 1 (for the residual variance).
#' @returns the covariance matrix (of class matrix)
#' @export
#' @examples
#' \dontrun{
#' # library(nlme)
#' fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
#' fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
#' .lmecov(fm1)
#' .lmecov(fm2)
#' }
.lmecov = function(fit,residual.variance = TRUE){
    # check class
    if(!inherits(fit,"lme")){.w();stop()}
    # 
    tmp  = VarCorr(fit)
    out  = diag(.an(tmp[,"Variance"]))
    # if correlation
    if(any(colnames(tmp)=="Corr")){
        p    = ncol(out)
        corr = rbind(cbind(matrix(.an(tmp[-p,-(1:2)]),nrow=p-1),NA,NA),NA)
        for(i in 1:(p-1)){
            for(j in i:(p-1)){
                if(j!=i){
                    out[i,j] = out[j,i] = sqrt(out[i,i])*sqrt(out[j,j])*corr[j,i]
                }
            }
        }
        # check: 
        # cov2cor(out) - corr
    }
    # output
    if(residual.variance){out}else{out[-p,-p,drop=FALSE]}
}



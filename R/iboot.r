#' @name .iboot
#' @title IB estimator 
#' @description IB estimator 
#' @param data a dataset, typically of class data.frame, with observations in rows and relevant information (outcomes, predictors, grouping factors, ...) in columns.
#' @param est the (possibly biased initial) estimator (self-defined) function. Its input should take '`data`' as first argument and, optionally, be followed by elements indicated under argument '`est.control`'. The output should be a named vector of target parameter estimates.
#' @param est.control An optional list of parameters for the estimator function indicated under '`est`'.
#' @param gen a (self-defined) data generating function. Its input should take '`seed`' and '`par`' as first and second arguments and be followed, optionally, by elements indicated under argument '`gen.control`'. The output should be a dataset 'similar' to the one indicated under argument '`data`' and could be used as input in the function specified under '`est`'. '`par`' should be a vector of parameters of same length and names as the output of the function '`est`'.
#' @param gen.control An optional list of parameters for the estimator function indicated under '`gen`'. 
#' @param conv a (self-defined) function assessing convergence based on the estimates obtained on iterations 1 to k (where k denotes the iteration at which convergence is assessed). Its output should be logical. Its first argument should be the (k x p) matrix of estimates, and could be followed, optionally, by elements indicated under argument '`conv.control`'. Check [.iboot.conv.relative] (default) and [.iboot.conv.absolute] for examples. In complex models, (co)variance parameters are typically more variable than others and may not systematically be estimated in all cases (due to convergence issues, for example), so that a convergence function specific to the problem at hand may be more suitable that the suggested ones. 
#' @param conv.control An optional list of parameters for the convergence function indicated under '`conv`'. Default is a list with argument 'tol' = 1e-3, where 'tol' denotes the tolerance value used in [.iboot.conv.relative] (default).
#' @param convK a (self-defined) function assessing convergence based on the estimates obtained on iterations 1 to K (where K denotes the maximum number of iterations). Its output should be logical. Its first argument should be the (K x p) matrix of estimates, and could be followed, optionally, by elements indicated under argument '`conv.control`'. In complex models and/or in models considering discrete responses, the same response vector can sometimes be obtained via different sets of parameters, often leading the implicit boostrap to go from a such a set to the next one between iterations, thus justifying a different assessment of convergence in such cases. Check [.iboot.conv.lm] (default) for an example.
#' @param convK.control An optional list of parameters for the convergence function indicated under '`convK`'. Default is a list with argument 'tol' = 1e-3, where 'tol' denotes the tolerance value used in [.iboot.conv.relative] (default).
#' @param B either a scalar indicating the number of IB samples which would be generated with seed 1:B or a list of seeds to be used to generate the IB samples. Default is B = 1999.
#' @param B1 the first seed, i.e., seed number 1, where the total number of (potential) seeds equals B x R. IMPORTANT: To avoid seed overlap, B1 is internally multiplied by '10^nchar(BR)'. Default = 0.
#' @param R the number of alternative seeds to consider assuming the seed of interest does not lead to estimates (no convergence). Default is R=49 (so that 50 seeds per task are considered in total). 
#' @param K the maximum number of iterations to seek convergence for a given bootrap sample. If K=NULL (default), K is set to ceiling(10*log(N)) where N denotes the number of rows of the dataset provided in argument 'data'. 
#' @param parallel a logical indicating if parallel computation should be used. If parallel = TRUE (default), package parallel with forking (ie, not possible for Windows at the moment). 
#' @param n.cores number of cores for parallel computation. Default to 9.
#' @param print an integer of value 0 (nothing), 1 (prints B and R) or 2 (prints B, R and K) indicating how progress should be printed (useful for debugging). Default is print = 0. 
#' @returns The function [.iboot] returns an S3 object of class 'iboot' with available print/summary/plot functions. 
#' @export
.iboot = function(  data, 
                    est   = NULL, est.control = NULL,
                    gen   = NULL, gen.control = NULL,
                    conv  = .iboot.conv.relative, conv.control = list(tol=1e-3),
                    convK = .iboot.conv.lm, convK.control = list(alpha=0.01),
                    B = 1999, B1 = 0, R = 49, K = NULL, 
                    parallel = TRUE, n.cores = 9, print = 0){
    # test
    # if(FALSE){
    #     data = data 
    #     est = est2.fun; est.control=list(mu.formula=mu.formula1,sigma.formula=sigma.formula);
    #     gen = sim.fun; gen.control=list(empty=empty,X.mu=X.mu,X.sigma=X.sigma,cutoff=cutoff);
    #     conv  = .iboot.conv.relative; conv.control = list(tol=1e-4, min.iter=25, lag=c(1,5));
    #     convK = .iboot.conv.lm; convK.control = list(alpha=0.05, frac=1/4); 
    #     B = 1999; B1 = id.seed$value[sw]; K=100; R=50; n.cores=10;
    #     parallel = TRUE; print=2          
    # }

    mc = match.call() # mc = NULL
    ##
    ## minor checks
    ##
    N  = nrow(data)
    if(is.null(K)|!is.numeric(K)){K = ceiling(10*log(N))}
    if(!is.function(est)){stop("'est' must be a function")}
    if(!is.function(gen)){stop("'gen' must be a function")}
    if(length(B)==1&is.numeric(B)){vect.seed = 1:B
    }else{if(length(B)>1&all(is.numeric(B))){vect.seed = B
    }else{stop("check input of 'B'")}}
    n.seed = length(vect.seed)
    if(Sys.info()[[1]]=="Windows"&parallel){
        parallel = FALSE
        warning("no parallel option for windows at the moment")
    }
    B1 = B1*10^(nchar(.ac(B*R)))
    print = .an(print) # backward compatibility 

    ##
    ## pi0: queen
    ##
    pi0 = try(R.utils::doCall(est,
              args = c(plyr::.(data=data),est.control)),silent=TRUE)
    if(class(pi0)[1]=="try-error"){
        stop("no initial estimates on 'data'")
    }
    n.par  = length(pi0)

    ##
    ## pi^star: king
    ##
    if(parallel){
        mx.hat.Bp = parallel::mclapply(vect.seed,.iboot_seed,par=pi0,
            est=est,est.control=est.control,
            gen=gen,gen.control=gen.control,
            conv=conv,conv.control=conv.control,
            convK=convK,convK.control=convK.control,
            n.seed=n.seed,B1=B1,R=R,K=K,
            plot=FALSE,print=print,mc.set.seed = FALSE,mc.cores=n.cores)
        mx.hat.Bp = matrix(unlist(mx.hat.Bp),byrow=TRUE,
                           nrow=length(mx.hat.Bp),ncol=n.par,
                           dimnames=list(vect.seed,names(pi0)))
    }else{
        mx.hat.Bp = matrix(NA,nrow=n.seed,ncol=n.par,
                           dimnames=list(vect.seed,names(pi0)))
        for(sw in 1:n.seed){# sw=0; sw=sw+1
            mx.hat.Bp[sw,] = .iboot_seed(vect.seed[sw],par=pi0,
                      est=est,est.control=est.control,
                      gen=gen,gen.control=gen.control,
                      conv=conv,conv.control=conv.control,
                      convK=convK,convK.control=convK.control,
                      n.seed=n.seed,B1=B1,R=R,K=K,
                      plot=FALSE,print=print)
        }
    }
    ##
    ## out: prince
    ##
    out = list(initial = pi0, boot = mx.hat.Bp, 
               #final = apply(mx.hat.Bp,2,median,na.rm=TRUE),
               #ci    = apply(mx.hat.Bp,2,quantile,probs=c(.025,.975),na.rm=TRUE),
               call  = mc)
    class(out) = "iboot"
    out
}


#' @name .iboot_seed
#' @title implicit bootrap sample related estimates
#' @description internal function 
#' @param seed the seed related to the bth Monte Carlo sample of interest. If data generation and estimation related to this seed does not lead to convergence, 50 other seeds are used. This list of seeds is defined as seq(B1+seed,B1+seed+n.seed*R,n.seed) where n.seed is defined further below.
#' @param par the vector of parameters as estimated by 'est' on the original dataset.
#' @param est refer to [.iboot] for details.
#' @param est.control refer to [.iboot] for details.
#' @param gen refer to [.iboot] for details.
#' @param gen.control refer to [.iboot] for details.
#' @param conv refer to [.iboot] for details.
#' @param conv.control refer to [.iboot] for details.
#' @param convK refer to [.iboot] for details.
#' @param convK.control refer to [.iboot] for details.
#' @param n.seed number of IB samples.
#' @param B1 the first seed of the sequence of interest.
#' @param R refer to [.iboot] for details.
#' @param K refer to [.iboot] for details.
#' @param plot only for check. Default to FALSE. 
#' @param print refer to [.iboot] for details.  
#' @returns The function [.iboot_seed] returns the vector of implicit bootrap sample related estimates.
.iboot_seed = function(seed,par,
                      est=est,est.control=est.control,
                      gen=gen,gen.control=gen.control,
                      conv=conv,conv.control=conv.control,
                      convK=convK,convK.control=convK.control,
                      n.seed=n.seed,B1=B1, R=R,K=K,
                      plot=FALSE,print=print){
    # par=pi0; plot=TRUE; print=2; seed=vect.seed[sw]
    out        = rep(NA,length(par))
    names(out) = names(par)
    inner.seed = seq(B1+seed,B1+seed+n.seed*R,n.seed)
    seedw      = 0
    converged  = FALSE
    while(!converged & seedw<length(inner.seed)){
        # initialise
        seedw     = seedw+1
        if(print>0){
            cat("\nstart seed",.ac(inner.seed[seedw]),"(",seedw,"/",length(inner.seed),")\n")
        }
        continue  = TRUE 
        iter      = 1
        mx.hat.kp = matrix(nrow=K+1,ncol=length(par),dimnames=list(1:(K+1),names(par)))
        mx.hat.kp[1,] = par
        # data.km1  = data$y  
        if(plot){
            .ep(xlim=c(1,K),ylim=range(par)+c(-1,1)*(max(par)-min(par))*.25)
            axis(1)
            axis(2)
            }
        # loop
        while(continue & iter<K){
            iter = iter+1
            # generate data
            data.iter = try(R.utils::doCall(gen,
                            args = c(plyr::.(seed=as.character(inner.seed[seedw]), par=mx.hat.kp[iter-1,]),
                            gen.control)),silent=TRUE)
            if(class(data.iter)[1]=="try-error"){
                if(print==2){cat("x")}
                continue = FALSE 
            }else{
            # Compute pi^*
            pistar = try(R.utils::doCall(est,
                         args = c(plyr::.(data=data.iter),est.control)),silent=TRUE)
            if(class(pistar)[1]=="try-error"){
                if(print==2){cat("x")}
                continue = FALSE 
            }else{
                if(all(is.na(pistar))){
                    continue = FALSE 
                }else{
                # Update theta^{(k)}
                mx.hat.kp[iter,] = mx.hat.kp[iter-1,] + par  - pistar 
                if(plot){
                    points(rep(iter,length(par)),unlist(mx.hat.kp[iter,]),
                           col=rainbow(length(par)))
                }                       
                # check convergence     
                arg.conv = c(plyr::.(estimates=mx.hat.kp[1:iter,]),conv.control)     
                names(arg.conv)[1] = names(formals(conv))[1]
                converged = try(R.utils::doCall(conv,args = arg.conv),silent=TRUE)
                if(!is.logical(converged)|is.na(converged)){
                    if(print==2){
                        .w(paste0("non logical 'conv' output at iteration ",
                           iter," of seed ",inner.seed[seedw]))
                    }
                    converged = FALSE
                }else{if(converged){
                    if(print==2){cat("o")}
                    continue  = FALSE
                    out = c(mx.hat.kp[iter,])                
                    }
                }
                # check if conv at last iter
                if(iter==K&!converged){
                    if(print==2){cat("|")}
                    arg.conv = c(plyr::.(estimates=mx.hat.kp[1:iter,]),convK.control)    
                    names(arg.conv)[1] = names(formals(convK))[1]
                    converged = try(R.utils::doCall(convK,args = arg.conv),silent=TRUE)
                    if(!is.logical(converged)|is.na(converged)){
                        if(print==2){
                            .w(paste0("non logical 'convK' output at iteration ",
                               iter," of seed ",inner.seed[seedw]))
                        }
                        converged = FALSE
                    }else{if(converged){
                        if(print==2){cat("o")}
                        continue  = FALSE
                        out = c(mx.hat.kp[iter,])
                        }
                    }
                }
                # continue
                if(print==2&!converged){cat(".")}   
                }}
            }# end if not try-error 
        }# end while-iter
    }# end while-seed
    out
}


#' @name .iboot.conv.absolute
#' @title IB convergence assessment 
#' @description Assessment of IB convergence by comparing estimates at iteration k to estimates at iteration k-1 in absolute terms and checking if the sum is smaller than the tolerence value.
#' @param estimates the (k x p) matrix of estimates, where K and p respecively denotes the maximum number of iterations and total number of parameters.
#' @param tol a convergence relative tolerance value. Default is tol = 1e-3. 
#' @param lag a vector of (positive) scalars indicating to which previous iterations the current estimates would be compared to. Indicate lag=1 for a comparison with estimates at the previous iteration (default to 1).
#' @param min.iter a scalar, the minimum number of iterations required to assess convergence (default to 10). 
#' @returns a logical
#' @export
.iboot.conv.absolute = function(estimates,tol,lag=1,min.iter=10){
    min.iter = max(min.iter,max(lag)+1)
    k = nrow(estimates)
    if(k>=min.iter){
        n.lag = length(lag)
        converged = rep(NA,n.lag)
        for(lw in 1:n.lag){
            converged[lw] = sum(abs(estimates[k,]-estimates[k-1,])) 
        }
        converged = all(converged < tol)
    }else{
        converged = FALSE
    } 
    converged
}




#' @name .iboot.conv.relative
#' @title IB convergence assessment 
#' @description Assessment of IB convergence by comparing estimates at iteration k to estimates at iteration k-1 in relative terms and checking if the sum of the squared relative difference is smaller than the tolerence value.
#' @param estimates the (k x p) matrix of estimates, where K and p respecively denotes the maximum number of iterations and total number of parameters.
#' @param tol a convergence relative tolerance value. Default is tol = 1e-3. 
#' @param lag a vector of (positive) scalars indicating to which previous iterations the current estimates would be compared to. Indicate lag=1 for a comparison with estimates at the previous iteration (default to 1).
#' @param min.iter a scalar, the minimum number of iterations required to assess convergence (default to 10). 
#' @returns a logical
#' @export
.iboot.conv.relative = function(estimates,tol,lag=1,min.iter=10){
    min.iter = max(min.iter,max(lag)+1)
    k = nrow(estimates)
    if(k>=min.iter){
        n.lag = length(lag)
        converged = rep(NA,n.lag)
        for(lw in 1:n.lag){
            converged[lw] = sum((estimates[k,]/estimates[k-lag[lw],]-1)^2) 
        }
        converged = all(converged < tol)
    }else{
        converged = FALSE
    } 
    converged
}


#' @name .iboot.conv.lm
#' @title IB final convergence assessment 
#' @description Linear-model based assessment of IB convergence. In complex models and/or in models considering discrete responses, the same response vector can sometimes be obtained via different sets of parameters, often leading the implicit boostrap to go from a such a set to the next one between iterations. In such cases, fitting a linear model on parameter estimate vectors leads to slopes close to 0. It is what this function tests, typically when iteration K is reached, i.e, when k=K.
#' @param estimates the (k x p) matrix of estimates, where k and p respecively denotes the number of iterations and total number of parameters.
#' @param alpha the p-value cut-off value used to assess significance of the slope of each target parameter.
#' @param frac the fraction of K to use to define assessment after K iterations (1/2 by default).
#' @returns a logical
#' @export
.iboot.conv.lm = function(estimates,alpha,frac=1/2){
    K    = nrow(estimates)
    pval = apply(estimates[floor(K*(1-frac)):K,],2,function(x){
           coef(summary(lm(x~I(1:length(x)))))[2,4]})
    if(any(is.na(pval))){
        posw = which(is.na(pval))
        pval[posw] = apply(estimates[(K/2):K,posw,drop=FALSE],2,function(x){
                     coef(summary(lm(x~I(1:length(x)))))[2,1]})==0
    }
    all(pval>alpha) 
}


#' @name print.iboot
#' @title print function 
#' @description print function for objects of class 'iboot' 
#' @param x object of class 'iboot'.
#' @param ... additional arguments to be passed nowhere.
#' @returns print.
#' @export
print.iboot = function(x, ...){
    cat("\nIB estimator\n")
    cat("(",nrow(x$boot)," samples)\n\n",sep="")
    cat("initial:\n")
    print(x$initial)
    #cat("final (median):\n")
    #print(x$final) 
    cat("\ncall:\n")       
    print(x$call)
}





#' @name summary.iboot
#' @title summary function 
#' @description summary function for objects of class 'iboot' 
#' @param object object of class 'iboot'
#' @param point a function defining the point estimate for a target parameter. Its first argument must be the vector IB estimates of length B and could be followed, optionally, by elements indicated under argument '`point.control`'. Default is the median function.
#' @param point.control An optional list of parameters for the point estimate function indicated under '`point`'. Default is `na.rm = FALSE`.
#' @param interval a function defining the interval estimates for a target parameter. Its first argument must be the vector IB estimates of length B and could be followed, optionally, by elements indicated under argument '`point.control`'. The output should be a named vector (typically of length 2 but longer if wished) of estimates. Default is the quantile function leading to percentile confidence intervals. Other choices may lead to better coverages when B is small to moderate. 
#' @param interval.control An optional list of parameters for the interval estimate function indicated under '`interval`'. Defaults are `probs = c(0.025,.975)`, leading to confidence intervals at the 0.95 levels, and `na.rm = FALSE`.
#' @param ... additional arguments to be passed nowhere.
#' @returns an object of class 'iboot.summary' with available print function. The object is a list with elements
#' \itemize{
#'   \item initial - The vector of initial target parameter estimates.
#'   \item boot - The (B x p) matrix of IB estimates.
#'   \item final - The vector of final target parameter estimates (as estimated by the function `point` in summary).
#'   \item ci - The (p x 2) matrix of confidence intervals)..
#' } 
#' @export
summary.iboot = function(object, 
                         point = median, point.control = list(na.rm=FALSE),
                         interval = quantile, interval.control = list(prob=c(0.025,.975),na.rm=FALSE), ...){
    # test
    if(FALSE){
       object = iboot_seed[[sw]]
       point = median; point.control = list(na.rm=FALSE)
       interval = quantile; interval.control = list(probs=c(0.025,.975),na.rm=FALSE)
    }
    # output
    n.p = ncol(object$boot)
    # point estimates
    object$final = rep(NA,n.p)
    names(object$final) = names(object$initial)
    for(pw in 1:n.p){# pw=1
        arg.point = c(plyr::.(boot=object$boot[,pw]),point.control)              
        names(arg.point)[1] = names(formals(point))[1]
        object$final[pw] = do.call(point,args = arg.point)
        }
    # interval estimates    
    pw=1
    arg.interval = c(plyr::.(boot=object$boot[,pw]),interval.control) 
    names(arg.interval)[1] = names(formals(interval))[1]
    tmp = do.call(interval,args = arg.interval)
    ci = matrix(NA,nrow=n.p,ncol=length(tmp),dimnames=list(names(object$initial),names(tmp)))
    ci[pw,] = tmp
    for(pw in 2:n.p){# pw=2
        arg.interval = c(plyr::.(boot=object$boot[,pw]),interval.control) 
        names(arg.interval)[1] = names(formals(interval))[1]
        ci[pw,] = do.call(interval,args = arg.interval)
    }
    object$ci = ci
    # print    
    class(object) = "summary.iboot"
    object
}


#' @name print.summary.iboot
#' @title print function 
#' @description print function for objects of class 'summary.iboot' 
#' @param x object of class 'summary.iboot'.
#' @param ... additional arguments to be passed to print.
#' @returns print.
#' @export
print.summary.iboot = function(x, ...){
    cat("\n-------------------------------\n")
    cat(" IB estimation \n")
    cat(" ",nrow(x$boot)," samples",sep="")
    if(any(is.na(x$boot))){
    cat(" (",sum(apply(is.na(x$boot),1,any))," with NAs)\n",sep="")
    }else{cat("\n")}
    cat("-------------------------------\n\n")
    # estimates
    out = cbind(initial=x$initial, final=x$final, x$ci)
    cat("Estimates:\n")
    print(out, ...)    
    cat("\n-------------------------------\n\n")

}








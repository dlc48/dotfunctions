#' @name .ep
#' @title empty plot function
#' @description generate an empty plot (no axes, no axe labels, no box, no points)
#' @param xlim  xlim
#' @param ylim  ylim
#' @param ...  ...
#' @export
#' @examples
#' \dontrun{
#' .ep()
#' }
.ep = function(xlim=c(0,1),ylim=c(0,1),...) {
    plot(1,1,pch="",axes=FALSE,xlab="",ylab="",main="",
         ylim=ylim,xlim=xlim,...)
    }


#' @name .color
#' @title colour shortcut function
#' @description create a function generating the colours or interest
#' @returns a function generating the colours of interest
#' @export
#' @examples
#' \dontrun{
#' pal.fun = .color()
#' }
.color = function(){colorspace::choose_palette()}


#' @name .circle
#' @title circle function
#' @description plot a circle based on coordinates of centre and radius. Note that circles may look like ellipses if the aspect ratio (argument 'asp' of plot) is different from 1
#' @param x x is a vector defined as follows: first element =  centre x-coordinate, second element =  centre y-coordinate, third element = radius.
#' @param plot a logical. use TRUE to plot or FALSE to save dataset of x and y coordinates of 200 points
#' @param ... ...
#' @export
#' @examples
#' \dontrun{
#' .ep(asp=1)
#' .circle(c(.5,.5,.25))
#' }
.circle = function(x,plot=TRUE,...){
    theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
    out   = data.frame(x = as.numeric(x[3]) * cos(theta) + as.numeric(x[1]),
                       y = as.numeric(x[3]) * sin(theta) + as.numeric(x[2]))
    if(plot){
        lines(x = out$x, out$y, ...)
    }else{
        out
    }
}


#' @name .ellipse
#' @title bivariate Gaussian ellipse function
#' @description plot an ellipse based on coordinates of centre, covariance matrix and quantile of the bivariate gaussian. Note that visualisation may be affected by the aspect ratio of the plot (argument 'asp' of plot) 
#' @param mu vector of length 2 indicating the coordinates of the centre of the ellipse. (0,0) by default
#' @param sigma 2-by-2 covariance matrix (with identity as default)
#' @param quantile quantile of the bivariate Gaussian to display
#' @param plot a logical. use TRUE to plot or FALSE to save dataset of x and y coordinates of 200 points
#' @param ... ...
#' @export
#' @examples
#' \dontrun{
#' .ep(asp=1)
#' data = mvtnorm::rmvnorm(500,c(-1,2),sigma=matrix(c(1,.5,.5,2),ncol=2))
#' plot(data[,1],data[,2])
#' .ellipse(apply(data,2,mean),var(data),col="red",lty=2)
#' .ellipse(c(-1,2),matrix(c(1,.5,.5,2),ncol=2),col="blue",lty=1)
#' }
.ellipse = function(mu=c(0,0),sigma=diag(2),quantile=.95,plot=TRUE,...){
    points    = c(0:180,0)*pi/90
    sd        = sqrt(diag(sigma))
    corr      = cov2cor(sigma)[1,2]
    chi       = sqrt(qchisq(quantile,2))
    coord_cor = list(x=chi*cos(points+acos(corr)/2),
                     y=chi*cos(points-acos(corr)/2))
    out       =  data.frame(x=coord_cor$x*sd[1]+mu[1],
                            y=coord_cor$y*sd[2]+mu[2]) 
    if(plot){
        lines(x = out$x, out$y, ...)
    }else{
        out
    }
}




#' @name .axis
#' @title axis function
#' @description create fancy plot axes (typically based on an id file).
#' @param data a file ('id.x'-like) with a column 'pos' (position on the xaxis), typically 1 to nrow for a split at the full file level
#' @param split a factor (typically year/month, school/class, aso)
#' @param side 3 = over, 1 = below
#' @param at.y if non-null, label are displayed in the plot on an horizontal line
#'  at 'at.y' on the y-axis
#' @param col.abline colour of the vertical boundaries in the argument 'col' of \link[graphics]{abline}
#' @param col.label colour of the label 
#' @param col.axis colour of the x-axis if add.axis == TRUE 
#' @param padj padj
#' @param hadj hadj
#' @param cex cex
#' @param add.abline TRUE for vertical boundaries
#' @param add.label TRUE for label
#' @param add.axis TRUE for x-axis
#' @param label label
#' @param ... other useful arguments: cex for the cex of labels in axis() and text(), lty for the lty in abline(), padj or hadj for the adjustment of the location of the label in axis()
#' @export
.axis = function(data, split, side = 3, at.y = NULL,
                 col.abline = "gray", col.label = "blue", col.axis="blue",
                 padj = NA, hadj = NA, cex = 1,
                 add.abline = TRUE, add.label = TRUE, add.axis = FALSE,
                 label = NULL, ...){
    data_s = split(data,as.numeric(data[,split]))
    id     = format(unlist(lapply(data_s,function(x)unique(as.character(x[,split])))))
    mid    = unlist(lapply(data_s,function(x)mean(x$pos)))
    high   = apply(rbind(unlist(lapply(data_s,function(x)max(x$pos)))[-length(data_s)],
                         unlist(lapply(data_s,function(x)min(x$pos)))[-1]),2,mean)
    jump   = mean(high[-1]-high[-length(high)])
    limits = c(min(high)-jump,high,jump+max(high))
    # abline:
    if(add.abline){
        abline(v=high,col=col.abline,...)
        }
    # names:
    if(add.label){
        if(is.null(label)){id_level = id}else{id_level = label}
        if(is.null(at.y)){
            axis(side, at = mid, labels = id_level, tick = FALSE, col.axis = col.label,
                 padj = padj, hadj = hadj, cex.axis = cex, ...)
        }else{
            text(mid, rep(at.y,length(mid)), labels = id_level, col = col.label, cex = cex, ...)
            }
        }
    # tick
    if(add.axis){
        axis(side, at = limits, labels = rep("",length(limits)), tick = TRUE, col = col.axis, ...)
        }
    }



#' @name .polygon
#' @title polygon 'shaded' area
#' @description add a coloured area defined by the function polygon to an existing plot. Ideal to display confidence bounds.
#' @param x vector of the x axis
#' @param y1 vector of values of f(x) for the first bound (i.e., upper or lower). It should be a vector of the same length as x.
#' @param y2 value(s) of f(x) for the second bound. It can either be a vector of the same length as y1 or a scalar of class numeric (default y2 = 0).  
#' @param col the colour to be used for the surface
#' @param border the colour to be used for the border (default border=NA, i.e., no border)
#' @param ... other parameter sent to \link[graphics]{polygon}
#' @export
#' @examples
#' \dontrun{
#' x   = seq(-4,4,length=1000)
#' f.x = dnorm(x)
#' .ep(xlim=range(x),ylim=range(f.x))
#' axis(1, pos=0)
#' axis(2)
#' lines(x,f.x)  
#' posw = x>-1
#' .polygon(x[posw],y1=f.x[posw])
#' }
.polygon = function(x,y1,y2=0,col=gray(.75),border=NA,...){
    # minor checks
    if(length(y2)==1 & inherits(y2,"numeric")){
        y2 = rep(y2,length(y1))
    }
    if((length(x)!=length(y1)) | (length(x)!=length(y1))){
        .w("inconsistent 'x', 'y1' and 'y2' vector lengths"); stop()
    }
    #
    xx = c(x,x[length(x):1])
    yy = c(y1,y2[length(y2):1])
    polygon(xx,yy, col=col, border=border, ...)
}


#' @name .isna
#' @title plot missing pattern
#' @description plot missing pattern
#' @param data a dataset with missings
#' @param col a vector of colours, the first one for the non-missing information, the second one for the missing one
#' @param transpose a logical indicating if the dataset should be transposed.  
#' @param order.row a logical indicating if the rows of the dataset should be ordered according to a distance functions,
#' @param order.col a logical indicating if the columns of the dataset should be ordered according to a distance functions,
#' @param cex.axis a numerical vector of length 2 indicating the font size on the ast and second axes,
#' @param hadj a numerical scalar indicating the hadj for the sample size onformation on axis 2, 
#' @export
#' @examples
#' \dontrun{
#' x   = matrix(sample(c(NA,1),100,replace=TRUE),ncol=4,dimnames=list(1:25,LETTERS[1:4]))
#' .isna(x)
#' }
.isna = function(data,col  = c("light gray","red"),transpose=FALSE,
                 order.row = TRUE, order.col = TRUE,
                 cex.axis = c(1,.4),hadj = 2.5){
    # data = id.patient0; col=c("light gray","red"); transpose = FALSE; order.row = TRUE; order.col = TRUE;

    # par(mfrow=c(1,1),mar=c(2,8,1.5,8))
    if(transpose){data=t(data)}
    data1 = is.na(data)    
    roww = if(order.row){
                hclust(dist(data1, method = "binary"))$order
            }else{1:nrow(data)}
    colw = if(order.col){
                hclust(dist(t(data1), method = "binary"))$order
            }else{1:ncol(data)}
    data1  <- data1[rank(roww),rank(colw)]
    data   <- data[rank(roww),rank(colw)]

    image(data1[1:nrow(data1),ncol(data1):1],col=col,axes=FALSE)
    axis(1,c(0,1),c(1,nrow(data)),las=1,cex.axis= cex.axis[1])
    axis(4,seq(0,1,length=ncol(data)),
         colnames(data)[ncol(data):1],las=2,cex.axis=cex.axis[2])
    axis(2,seq (0,1,length=ncol(data)),
         paste0(format(round(apply(!is.na(data),2,mean)*100,2)),"%")[ncol(data):1],
         las=2,cex.axis=cex.axis[2])
    axis(2,seq(0,1,length=ncol(data)),
         paste0("n = ",.fill(apply(!is.na(data),2,sum)))[ncol(data):1],
         las=2,cex.axis=cex.axis[2],tick=FALSE,hadj=hadj)
}


#' @name .violin
#' @title Violin plot 
#' @description Adds violin density estimates to a plot (typically created by [.ep]). 
#' @param data A vector or matrix of numerical values.
#' @param at A scalar if `data` is a vector, or a vector of scalars if `data` if a matrix. When `at=NULL` (default), value `1` and `1:p` are respectively used if data is a vector or a matrix, where `p` denote the number of columns of the matrix.
#' @param h A numeric corresponding to the smoothing parameter. Default to 1. When `h=NULL`, the smoothing value is optimised in the \link[sm]{sm.density} function (Good luck with that).
#' @param violin A \link[base]{logical} (with default set to `TRUE`) indicating if the violin plot should be drawn, or a list with names `col`, `border`, and `lwd`, respectively indicating i/ the background colour, ii/ the colour of the border, and iii/ the tickness of the border. The input `violin = TRUE` is equivalent to `violin = list(col="#8B897040", border=gray(0),lwd=1)`. 
#' @param boxplot A \link[base]{logical} (with default set to `TRUE`) indicating if a boxplot should be drawn, or a list with names `col`, `border`, `width` and `lwd`, respectively indicating i/ the colour of the boxplot, ii/ the colour of the border of the boxplot, iii/ the boxplot width, and iv/ the tickness of the boxplot border. The input `boxplot = TRUE` is equivalent to `boxplot = list(col="#8B897040", border=gray(.2),width =.05,lwd=1)`. 
#' @param horizontal A \link[base]{logical} (with default set to `FALSE`) indicating if the plot should be horizontal or vertical. 
#' @param points A \link[base]{logical} (with default set to `TRUE`) indicating if individual points should be drawn, or a list with names `col`, `pch`, `cex`, respectively indicating i/ the colour(s), ii/ the symbol(s) , iii/ the expension factor(s) of the points, and iv/ the level of jittering for the points on the y-axis. The input `points = TRUE` is equivalent to `points = list(col="#2E008B", pch=16, cex=1, jitter=0)`. `jitter=0.25` add a uniform noise with distribution `U[-0.25,0.25]` to the data (might be useful for counts).
#' @param first A character indicating if the `points` or the violin plot should be plotted first. Default to `points`.
#' @seealso \link[sm]{sm.density} 
#' @export
#' @examples
#' \dontrun{
#'   data = data.frame(norm=rnorm(500,rep(c(0,5),each=250),1),
#'                    unif=stats::runif(500,-2,6))    
#'  .ep(c(.5,2.5), range(data))
#'   axis(1, at  = 1:ncol(data), colnames(data))
#'   axis(2, las = 2)
#'  .violin(data)
#' } 
.violin = function(data, at=NULL, h=1, violin=TRUE, boxplot=TRUE, horizontal=FALSE,
                   points=TRUE, first="points"){
    # first
    if(is.na(match(first,c("violin","points")))){
        stop(.w("'first' should either be set to'violin' or 'points'"))
    }
    # p
    n.p = ifelse(inherits(data,"data.frame")|inherits(data,"matrix"),ncol(data), 1)
    if(n.p==1){ data=matrix(data,ncol=1)}
    if(!is.null(at)){
        if(length(at)!=n.p){
            stop(.w("length of 'at' should match number of columns of 'data'"))        
        }
    }else{
        at = 1:n.p
    }
    # legend
    if(is.list(violin)){# violin = list(col="green", cex=1.25)
        violin.col    = ifelse(!is.null(violin[["col"]]),violin[["col"]],"#8B897040")        
        violin.border = ifelse(!is.null(violin[["border"]]),violin[["border"]],gray(0))    
        violin.lwd    = ifelse(!is.null(violin[["lwd"]]),violin[["lwd"]],1)    
        violin        = TRUE
    }else{
        if(!is.logical(violin)){
            stop("'violin' should be a list or a logical")
        }else{
            if(violin){
                violin.col    = "#8B897040"      
                violin.border = gray(0)
                violin.lwd    = 1
            }
        }
    }
    # boxplot
    if(is.list(boxplot)){# boxplot = list(col="green", cex=1.25)
        boxplot.col    = ifelse(!is.null(boxplot[["col"]]),boxplot[["col"]],"#FFD70070")        
        boxplot.border = ifelse(!is.null(boxplot[["border"]]),boxplot[["border"]],gray(.2))    
        boxplot.width  = ifelse(!is.null(boxplot[["width"]]),boxplot[["width"]],.05)
        boxplot.lwd    = ifelse(!is.null(boxplot[["lwd"]]),boxplot[["lwd"]],1)    
        boxplot        = TRUE
    }else{
        if(!is.logical(boxplot)){
            stop("'boxplot' should be a list or a logical")
        }else{
            if(boxplot){
                boxplot.col    = "#FFD70070"      
                boxplot.border = gray(.2)
                boxplot.width  = .05
                boxplot.lwd    = 1
            }
        }
    }
    # points
    if(is.list(points)){# points = list(col="green", cex=1.25)
        points.col    = ifelse(!is.null(points[["col"]]),points[["col"]],"#2E008B90")        
        points.pch    = ifelse(!is.null(points[["pch"]]),points[["pch"]],16)    
        points.cex    = ifelse(!is.null(points[["cex"]]),points[["cex"]],1)
        points.jitter = ifelse(!is.null(points[["jitter"]]),points[["jitter"]],0)    
        points        = TRUE
    }else{
        if(!is.logical(points)){
            stop("'points' should be a list or a logical")
        }else{
            if(points){
                points.col    = "#2E008B90"    
                points.pch    = 16
                points.cex    = 1
                points.jitter = 0
            }else{
                points.jitter = 0
            }
        }
    }
    # plot
    for(pw in 1:n.p){# pw=1
        violin.density = .violin.density(data[,pw], h=h)
        #
        epsilon.x = stats::rnorm(violin.density$n,rep(0,violin.density$n),
                        sqrt(violin.density$pw.freq/
                        max(violin.density$pw.freq))*.15)        
        epsilon.y = if(points.jitter==0){
                        0
                    }else{
                        stats::runif(violin.density$n,-points.jitter,points.jitter)
                    }
        #
        if(first=="points"&points){
            xw = rep(at[pw],violin.density$n)+epsilon.x
            yw = data[,pw]+epsilon.y
            if(!horizontal){
                points(xw, yw,col=points.col,pch=points.pch,cex=points.cex)
            }else{
                points(yw, xw,col=points.col,pch=points.pch,cex=points.cex)             
            }
        }
        # violin
        xx = c(-violin.density$sm.density, 
                violin.density$sm.density[length(violin.density$sm.density):1])
        yy = c(violin.density$sm.points, 
               violin.density$sm.points[length(violin.density$sm.points):1])
        if(!horizontal){
            graphics::polygon(xx+at[pw], yy, col=violin.col, border=violin.border, 
                        lwd=violin.lwd)
        }else{
            graphics::polygon(yy, xx+at[pw], col=violin.col, border=violin.border, 
                        lwd=violin.lwd)
        }
        # boxplot
        if(boxplot){
            if(!horizontal){        
                graphics::segments(at[pw],violin.density$quantile[1],
                         at[pw],violin.density$quantile[2],
                         col=boxplot.border, lwd=boxplot.lwd)
                graphics::segments(at[pw],violin.density$quantile[4],
                         at[pw],violin.density$quantile[5],
                         col=boxplot.border, lwd=boxplot.lwd)
                graphics::rect(at[pw]-boxplot.width, violin.density$quantile[2], 
                     at[pw]+boxplot.width, violin.density$quantile[4], 
                     col=boxplot.col, border=boxplot.border, lwd=boxplot.lwd)
                graphics::segments(at[pw]-boxplot.width,violin.density$quantile[3],
                         at[pw]+boxplot.width,violin.density$quantile[3],
                         col=boxplot.border, lwd=boxplot.lwd)
            }else{
                graphics::segments(violin.density$quantile[1],at[pw],
                         violin.density$quantile[2],at[pw],
                         col=boxplot.border, lwd=boxplot.lwd)
                graphics::segments(violin.density$quantile[4],at[pw],
                         violin.density$quantile[5],at[pw],
                         col=boxplot.border, lwd=boxplot.lwd)
                graphics::rect(violin.density$quantile[2], at[pw]-boxplot.width, 
                     violin.density$quantile[4], at[pw]+boxplot.width, 
                     col=boxplot.col, border=boxplot.border, lwd=boxplot.lwd)
                graphics::segments(violin.density$quantile[3],at[pw]-boxplot.width,
                         violin.density$quantile[3],at[pw]+boxplot.width,
                         col=boxplot.border, lwd=boxplot.lwd)            
            }
        }
        # points
        if(first=="violin"&points){
            xw = rep(at[pw],violin.density$n)+epsilon.x
            yw = data[,pw]+epsilon.y
            if(!horizontal){
                points(xw, yw,col=points.col,pch=points.pch,cex=points.cex)
            }else{
                points(yw, xw,col=points.col,pch=points.pch,cex=points.cex)             
            }
        }
    } 
}




#' @name .violin.density
#' @title Violin plot density estimates
#' @description Extracts density estimates for a violin plot. 
#' @param data data vector.
#' @param h a scalar corresponding to the smoothing parameter.
#' @returns a list of coordinates and corresponding density values.
#' @seealso \link[sm]{sm.density} 
.violin.density = function(data, h=NULL){
    data      = data[!is.na(data)]
    quantilew = quantile(data, prob=c(0,.25,.5,.75,1))
    iqrw      = quantilew[4] - quantilew[2]
    upper     = min(quantilew[4] + 1.5 * iqrw, quantilew[5])
    lower     = max(quantilew[2] - 1.5 * iqrw, quantilew[1])
    rangew    = c(min(lower, quantilew[1]), max(upper, quantilew[5]))
    argw      = list(display="none",h=h)
    # sm
    if(is.null(h)){
        smoothw   = do.call(sm::sm.density, c(list(data, xlim = rangew, display="none")))
    }else{
        smoothw   = do.call(sm::sm.density, c(list(data, xlim = rangew, display="none", h=h)))
    }
    # block
    n.lim   = min(100,length(unique(data)))
    lim     = seq(quantilew[1],quantilew[5],length=n.lim)
    pw.data = rep(n.lim, length(data))
    for(lw in n.lim:1){
        pw.data[data<=lim[lw]] = lw
    }
    pw.freq = tabulate(pw.data)/length(data)
    # 
    list(sm.points  = smoothw$eval.points,
         sm.density = smoothw$estimate * 0.4/max(smoothw$estimate),
         quantile   = c(lower,quantilew[2:4],upper), range=rangew, mean=mean(data),
         data       = data, n=length(data), pw.freq = pw.freq[pw.data])
}



#' @name .hist
#' @title Non-paramteric density plots 
#' @description Adds non-paramteric density plots to a plot (typically created by [.ep]). 
#' @param data A vector or matrix of numerical values.
#' @param h A numeric (data is a vector) or vector of numerical values (data is a matrix) corresponding to the smoothing parameter to use. Default to 1. When `h=NULL`, the smoothing value is optimised in the \link[sm]{sm.density} function (Good luck with that).
#' @param col A character (data is a vector) or vector of characters (data is a matrix) corresponding to the colours to use. Default set to `NULL` in which case \link[grDevices]{rainbow} is used with 50% transluency.
#' @param border A character (data is a vector) or vector of characters (data is a matrix) corresponding to the colours to use. Default set to `NULL` in which case \link[grDevices]{rainbow} is used without transluency.
#' @param lwd A numeric (data is a vector) or vector of numerical values (data is a matrix) corresponding to the border thickness to use. Default to 1.
#' @param points A \link[base]{logical} (with default set to `TRUE`) indicating if individual points should be drawn, or a list with names `col`, `pch`, `cex`, respectively indicating i/ the colour(s), ii/ the symbol(s) , iii/ the expension factor(s) of the points, and iv/ the level of jittering for the points on the x-axis. The input `points = TRUE` is equivalent to `points = list(col=NULL, pch=16, cex=1, jitter=0)`. `jitter=0.25` add a uniform noise with distribution `U[-0.25,0.25]` to the data (might be useful for counts). When `col=NULL` (the default), the same colour as for the non-parametric density plot is used.
#' @param first A character indicating if the `points` or the non-paramteric density plot should be plotted first. Default to `points`.
#' @seealso \link[grDevices]{rainbow}, \link[sm]{sm.density} 
#' @export
#' @examples
#' \dontrun{
#'   data = data.frame(norm=rnorm(500,rep(c(0,5),each=250),1),
#'                    unif=stats::runif(500,-2,6))    
#'  .ep(range(data), c(0, 0.75))
#'   axis(1)
#'   axis(2, las = 2)
#'  .hist(data)
#' } 
.hist = function(data, h=1, col=NULL, border=NULL, 
                 lwd=1, points=TRUE, first="points"){
    # first
    if(is.na(match(first,c("hist","points")))){
        stop(.w("'first' should either be set to'hist' or 'points'"))
    }
    # p
    n.p = ifelse(inherits(data,"data.frame")|inherits(data,"matrix"),ncol(data), 1)
    if(n.p==1){ data=matrix(data,ncol=1)}
    # col
    if(is.null(col)){
        col = .p(rainbow(n.p),50)
    }else{
        if(length(col)==1){
            col = rep(col,n.p)
        }else{
            if(length(col)!=n.p){stop(.w("check your 'col' input"))}
        }
    }   
    # border
    if(is.null(border)){
        border = rainbow(n.p)
    }else{
        if(length(border)==1){
            border = rep(border,n.p)
        }else{
            if(length(border)!=n.p){stop(.w("check your 'border' input"))}
        }
    }   
    # lwd
    if(length(lwd)==1){
        lwd = rep(lwd,n.p)
    }else{
        if(length(lwd)!=n.p){stop(.w("check your 'lwd' input"))}
    }    
    # points
    if(is.list(points)){# points = list(col="green", cex=1.25)
        points.col    = if(!is.null(points[["col"]])){
                            if(length(points[["col"]])==1){
                                rep(points[["col"]],n.p)
                            }else{
                                if(length(points[["col"]])!=n.p){
                                    .w("check length of 'points$col'")
                                }
                                points[["col"]]
                            }
                        }else{
                            col
                        }
        points.pch    = if(!is.null(points[["pch"]])){
                            if(length(points[["pch"]])==1){
                                rep(points[["pch"]],n.p)
                            }else{
                                if(length(points[["pch"]])!=n.p){
                                    .w("check length of 'points$pch'")
                                }
                                points[["pch"]]
                            }
                        }else{
                            rep(16, n.p)
                        }
        points.cex    = if(!is.null(points[["cex"]])){
                            if(length(points[["cex"]])==1){
                                rep(points[["cex"]],n.p)
                            }else{
                                if(length(points[["cex"]])!=n.p){
                                    .w("check length of 'points$cex'")
                                }
                                points[["cex"]]
                            }
                        }else{
                            rep(1, n.p)
                        }
        points.jitter = if(!is.null(points[["jitter"]])){
                            if(length(points[["jitter"]])==1){
                                rep(points[["jitter"]],n.p)
                            }else{
                                if(length(points[["jitter"]])!=n.p){
                                    .w("check length of 'points$jitter'")
                                }
                                points[["jitter"]]
                            }
                        }else{
                            rep(0, n.p)
                        }
        points        = TRUE
    }else{
        if(!is.logical(points)){
            stop("'points' should be a list or a logical")
        }else{
            if(points){
                points.col    = col   
                points.pch    = rep(16,n.p)
                points.cex    = rep(1,n.p)
                points.jitter = rep(0,n.p)
            }
        }
    }
    # h
    if(length(h)==1){
        h = rep(h,n.p)
    }
    if(any(h<0)|any(is.na(h))){
        stop(.w("check your 'h' input"))
    }
    # plot
    for(pw in 1:n.p){# pw=1
        hist.density = .hist.density(data[,pw], h=h[pw])
        #
        epsilon.y = abs(stats::rnorm(hist.density$n,rep(0,hist.density$n),
                        sqrt(hist.density$pw.freq/
                        max(hist.density$pw.freq))*.3))        
        epsilon.x = if(points.jitter[pw]==0){
                        0
                    }else{
                        stats::runif(hist.density$n,-points.jitter,points.jitter)
                    }
        #
        if(first=="points"){
            points(data[,pw]+epsilon.x,
                   epsilon.y,
                   col=points.col[pw],pch=points.pch[pw],cex=points.cex[pw])
        }
        # violin
        .polygon(hist.density$sm.points, hist.density$sm.density, 
                 col=col[pw], border=border[pw], lwd=lwd[pw])
        # points
        if(first=="hist"){
            points(data[,pw]+epsilon.x,
                   epsilon.y,
                   col=points.col[pw],pch=points.pch[pw],cex=points.cex[pw])
        }
    } 


}


#' @name .hist.density
#' @title Histogram plot density estimates
#' @description Extracts density estimates for a violin plot. 
#' @param data data vector.
#' @param h a scalar corresponding to the smoothing parameter.
#' @returns a list of coordinates and corresponding density values.
#' @seealso \link[sm]{sm.density} 
.hist.density = function(data, h=NULL){
    data      = data[!is.na(data)]
    quantilew = quantile(data, prob=c(0,.25,.5,.75,1))
    iqrw      = quantilew[4] - quantilew[2]
    upper     = min(quantilew[4] + 1.5 * iqrw, quantilew[5])
    lower     = max(quantilew[2] - 1.5 * iqrw, quantilew[1])
    rangew    = c(min(lower, quantilew[1]), max(upper, quantilew[5]))
    argw      = list(display="none",h=h)
    # sm
    if(is.null(h)){
        smoothw   = do.call(sm::sm.density, c(list(data, display="none")))
    }else{
        smoothw   = do.call(sm::sm.density, c(list(data, display="none", h=h)))
    }
    # block
    n.lim   = min(100,length(unique(data)))
    lim     = seq(quantilew[1],quantilew[5],length=n.lim)
    pw.data = rep(n.lim, length(data))
    for(lw in n.lim:1){
        pw.data[data<=lim[lw]] = lw
    }
    pw.freq = tabulate(pw.data)/length(data)
    # 
    list(sm.points  = smoothw$eval.points,
         sm.density = smoothw$estimate,
         quantile   = c(lower,quantilew[2:4],upper), range=rangew, mean=mean(data),
         data       = data, n=length(data), pw.freq = pw.freq[pw.data])
}




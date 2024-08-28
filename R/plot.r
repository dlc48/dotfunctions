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



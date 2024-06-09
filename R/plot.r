#' @name .ep
#' @title empty plot function
#' @description generate an empty plot (no axes, no axe labels, no box, no points)
#' @param xlim  xlim
#' @param ylim  ylim
#' @param ...  ...
#' @export
#' @examples
#' \donttest{
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
#' \donttest{
#' pal.fun = .color()
#' }
.color = function(){colorspace::choose_palette()}


#' @name .circle
#' @title circle function
#' @description plot a circle based on coordinates of centre and radius. Note that circles may look like ellipses if the aspect ratio (argument 'asp' of plot) is different from 1
#' @param x x is a vector defined as follows x[1] =  centre x-coordinate, x[2] =  centre y-coordinate, x[3] = radius.
#' @param plot a logical. use TRUE to plot or FALSE to save dataset of x and y coordinates of 200 points
#' @param ... ...
#' @export
#' @examples
#' \donttest{
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


#' @name .axis
#' @title axis function
#' @description create fancy plot axes (typically based on an id file).
#' @param data a file ('id.x'-like) with a column 'pos' (position on the xaxis), typically 1 to nrow for a split at the full file level
#' @param split a factor (typically year/month, school/class, aso)
#' @param side 3 = over, 1 = below
#' @param at.y if non-null, label are displayed in the plot on an horizontal line
#'  at 'at.y' on the y-axis
#' @param col.abline colour of the vertical boundaries in abline() [col]
#' @param col.label colour of the label in axis() [col.axis]
#' @param col.axis colour of the x-axis if add.axis == TRUE in axis [col.axis]
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





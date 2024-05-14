#' .ep
#'
#' defines an empty plot (base R)
#' @param xlim  xlim
#' @param ylim  ylim
#' @export

.ep = function(xlim=c(0,1),ylim=c(0,1)) {
    plot(1,1,pch="",axes=FALSE,xlab="",ylab="",main="",
         ylim=ylim,xlim=xlim)
    }



################################################################################
#' .col
#'
#'first define a function generating the colors you want
#'pal.fun = .col() # pick what is suitable (n doesn't matter)
#' @export

.col = function() {colorspace::choose_palette()}



#' .circle
#'
#' @param x x is a vector of:
#' x[1]: x-coordinate
#' x[2]: y-coordinate
#' x[3]: radius
#' @param plot plot
#' @param ... ...
#' @export

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



#' .axis
#'
#' Helper for axis
#'
#'     OTHER useful arguments [sometimes passed through '...']
#'     cex: cex of labels in axis() and text() [cex.axis]
#'     lty: lty in abline()
#'     padj (hadj): adjustment of the location of the label in axis()
#'     orientation of the label in axis()
#'     orientation of the label in text() if at.y != NULL
#'     y-position of the x-axis in axis()
#' @param data a file ('id.t'-like] with a column 'pos' (position on the xaxis),
#' typically 1 to nrow for a split at the full file level
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
#' @param ... ...
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





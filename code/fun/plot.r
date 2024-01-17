
    
################################################################################
.ep = function(xlim=c(0,1),ylim=c(0,1)) {
    plot(1,1,pch="",axes=FALSE,xlab="",ylab="",main="",
         ylim=ylim,xlim=xlim)
    }
    ## first define a function generating the colors you want
    # > pal.fun = .col() # pick what is suitable (n doesn't matter)
    ## then use it:
    # > barplot(rep(1,25),col=pal.fun(25))
    
  
################################################################################
.col = function() {colorspace::choose_palette()}
    ## first define a function generating the colors you want
    # > pal.fun = .col() # pick what is suitable (n doesn't matter)
    ## then use it:
    # > barplot(rep(1,25),col=pal.fun(25))

###############################################################################
.circle = function(x,plot=TRUE,...){
    ## x is a vector of
    # x[1]: x-coordinate
    # x[2]: y-coordinate
    # x[3]: radius
    theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle
    out   = data.frame(x = as.numeric(x[3]) * cos(theta) + as.numeric(x[1]),
                       y = as.numeric(x[3]) * sin(theta) + as.numeric(x[2]))
    if(plot){
        lines(x = out$x, out$y, ...)
    }else{
        out
    }
}


################################################################################
.axis = function(data, split, side = 3, at.y = NULL, 
                 col.abline = "gray", col.label = "blue", col.axis="blue",
                 padj = NA, hadj = NA, cex = 1,
                 add.abline = TRUE, add.label = TRUE, add.axis = FALSE,
                 label = NULL, ...){
    # MAIN ARGUMENTS:
    # - data: a file ('id.t'-like] with a column 'pos' (position on the xaxis),
    #       typically 1 to nrow for a split at the full file level
    # - split: a factor (typically year/month, school/class, aso)
    # - side: 3 = over, 1 = below
    # - at.y: if non-null, label are displayed in the plot on an horizontal line
    #         at 'at.y' on the y-axis
    # COL: col may refer to many thinks so that a general 'col' would not work: 
    #   - col.abline: colour of the vertical boundaries in abline() [col]
    #   - col.label: colour of the label in axis() [col.axis]
    #   - col.axis: colour of the x-axis if add.axis == TRUE in axis [col.axis]
    # ADD: allows to select what to do
    #   - add.abline == TRUE for vertical boundaries 
    #   - add.label == TRUE for label
    #   - add.axis == TRUE for x-axis
    # OTHER useful arguments [sometimes passed through '...']
    # - cex: cex of labels in axis() and text() [cex.axis]
    # - lty: lty in abline()
    # - padj (hadj): adjustment of the location of the label in axis()
    # - las: orientation of the label in axis() 
    # - srt: orientation of the label in text() if at.y != NULL 
    # - pos: y-position of the x-axis in axis()

    # test
    #data=split(id.pos,id.pos$af)[[2]]; split="orga"; side = 1; at.y = NULL; 
    #col.abline = "gray"; col.label = "blue"; col.axis="blue";
    #padj = NA; hadj = NA; cex = 1;
    #add.abline = TRUE; add.label = TRUE; add.axis = FALSE;    

    # information:
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


    
    
#rm(list=ls())
#load("/Users/mediapulse/prog/rc-current/pin/OUT")

#id.t$month = factor(.an(substr(id.t$iso,5,6)),levels=1:12,labels=substr(month.abb,1,3))
#id.t$quarter = factor(rep(1:4,each=3)[.an(id.t$month)],levels=1:4,labels=.p(c("1st","2nd","3rd","4th")," quarter"))
#id.t$week = factor(c(rep(1,4),rep(2:1000,each=7))[1:n.t])
#id.t$year = factor(.an(substr(id.t$iso,1,4)))
#id.t$y = rnorm(n.t)
#id.t = id.t[1:2800,]
#n.t = nrow(id.t)

#plot(id.t$pos,id.t$y,axes=FALSE,pch="")
#lapply(split(id.t,id.t$year),function(x).axis(x,"month",side=1,cex.axis=.25,hadj=-5,col.label="red",las=2,lty=3,col.abline="light gray"))  
#lapply(split(id.t,id.t$year),function(x).axis(x,"quarter",side=3,cex.axis=.25,hadj=1,col.label="red",las=2,lty=2,col.abline="gray"))  
#.axis(id.t,"year",side=1,col.abline="brown")
#.axis(id.t,"year",side=3,col.abline="brown")
#points(id.t$pos,id.t$y)



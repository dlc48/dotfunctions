#' .sectotime
#'
#' @param n.sec seconds
#' @export

.sectotime = function(n.sec){
    .sectotime0 = function(n.sec){
        hms=as.character(c(floor(n.sec/60/60),floor(n.sec/60-60*floor(n.sec/60/60)),
          floor(n.sec-60*60*floor(n.sec/60/60)-60*floor(n.sec/60-60*floor(n.sec/60/60)))))
        paste(sapply(hms,function(x)if(nchar(x)==1){paste("0",x,sep="")}else{x}),collapse=":")
        }
    sapply(n.sec,.sectotime0)
    }

#' .timetosec
#'
#' @param x input
#' @export
.timetosec=function(x){
    hh = floor(x/10000)
    mm = floor((x-hh*10000)/100)
    ss = x-hh*10000-mm*100
    hh*3600+mm*60+ss
    }
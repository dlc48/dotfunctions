#' @name .sectotime
#' @title time function
#' @description defines the time (hh:mm:ss) corresponding to each element of a vector of seconds
#' @param n.sec vector of integers (ranging from 0 to 86400, the number of seconds in a day). 
#' @returns output is a string vector of format 'hh:mm:ss'.
#' @export
#' @examples
#' .sectotime(c(1000,10000))
.sectotime = function(n.sec){
    .sectotime0 = function(n.sec){
        hms=as.character(c(floor(n.sec/60/60),floor(n.sec/60-60*floor(n.sec/60/60)),
          floor(n.sec-60*60*floor(n.sec/60/60)-60*floor(n.sec/60-60*floor(n.sec/60/60)))))
        paste(sapply(hms,function(x)if(nchar(x)==1){paste("0",x,sep="")}else{x}),collapse=":")
        }
    sapply(n.sec,.sectotime0)
    }

#' @name .timetosec
#' @title second function
#' @description defines the second (integer ranging from 0 to 86400, the number of seconds in a day) corresponding to a vector of integers of format 'hhmmss' (where 024640 or 24640 correspond to '02:46:40')
#' @param x string vector of integers 'hhmmss'.
#' @returns output is a vector of integers.
#' @export
#' @examples
#' .timetosec(c(001640,24640))
.timetosec=function(x){
    .timetosec0=function(x){
        hh = floor(x/10000)
        mm = floor((x-hh*10000)/100)
        ss = x-hh*10000-mm*100
        hh*3600+mm*60+ss
        }
    sapply(x,.timetosec0)        
    }


#' @name .date
#' @title iso date function
#' @description indicate the date of the day (the function is used) in iso format (typically used to name output files).    
#' @param x whatever (it will be ignored)
#' @export
#' @examples
#' .date()    
 .date = function(x=NULL){format(Sys.time(), "%Y%m%d")}

#' @name .ad
#' @title shortcut
#' @description shortcut for as.Date...
#' @param x date input 
#' @param ... other input sent to as.Date()
#' @export
#' @examples
#' .ad(.date(),format="%Y%m%d")   
.ad = function(x,...){as.Date(x,...)}

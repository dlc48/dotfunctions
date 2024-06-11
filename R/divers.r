#' @name .h
#' @title print function for arrays
#' @description prints the first dimensions of an array (up to size 3), matrix or data frame. Less busy people may consider the function head().
#' @param x R object to 
#' @param size size of the output
#'
#' @export
#' @examples
#' dimw = c(10,20,30) 
#' arw  = array(1:prod(dimw),dimw)
#' .h(arw) 
#' .h(.adf(arw[,,1]))
.h=function(x,size=c(10,10,2)){
   dimx  = dim(x)
   size  = size[1:min(length(dimx),length(size))]
   for(sw in 1:length(size)){
       size[sw] = min(dimx[sw],size[sw])
   }
   # matrix
   if(length(size)==2){
      x[1:size[1],1:size[2]]
   # array
   }else{
      x[1:size[1],1:size[2],1:size[3]]
   }
}

#' @name .eval
#' @title evaluation function
#' @description evaluate an R expression based on an input of class 'character'. 
#' @param expr expression
#' @param ... other parameters
#' @returns returns the output of the evaluation, whatever it is...  
#' @export
#' @examples
#' .eval('c(1,2)')
.eval=function(expr,...){eval(parse(text = noquote(expr)),...)}

#' @name .nf
#' @title factor exterminator
#' @description change columns of class 'factor' of a data frame into columns of class 'character'. 'nf' stands for 'No (bloody) factors'. Since read.csv() (and equivalent functions) changes the default of the argument 'stringsAsFactors' to FALSE, .nf() is less useful.
#'
#' @param file a dataframe 
#' @returns returns a data frame of same size as input but without factors. 
#' @export
#' @examples
#' sapply(.nf(data.frame(y=rnorm(10),x=factor(rep(0:1,each=5)))),class)
.nf=function(file){
	temp=file
	vect.colw=seq(1,dim(temp)[2])[sapply(temp,class)=="factor"]
	if(length(vect.colw)>0){
	for(colw in 1:length(vect.colw)){temp[,vect.colw[colw]]=as.character(temp[,vect.colw[colw]])}
	}
	temp
	}

#' @name .anac
#' @title shortcut
#' @description shortcut for as.numeric used on the output of as.character used on a vector.
#' @param vector input vector
#' @returns returns a vector of class numeric.     
#' @export
#' @examples
#' .anac(table(rpois(100,2)))
.anac=function(vector){as.numeric(as.character(vector))}


#' @name .adf
#' @title shortcut
#' @description change class of a matrix to 'data.frame'. Less busy people may consider the function as.data.frame().
#' @param matrix input matrix
#' @param colnames colnames
#' @param rownames rownames
#' @returns a data frame.
#' @export
#' @examples
#' .adf(matrix(1:10,ncol=2),colnames=c("a","b")) 
.adf=function(matrix,colnames=NULL,rownames=NULL){
    dfw = as.data.frame(matrix)
    if(!is.null(colnames)){colnames(dfw) = colnames}
    if(!is.null(rownames)){rownames(dfw) = rownames}
    dfw
    }


#' @name .adfm
#' @title shortcut
#' @description convert a vector to a data.frame (of possibly different columns).
#' @param ... R object
#' @returns a data frame.    
#' @export
#' @examples
#' .adfm(1:10,ncol=2)
.adfm=function(...){as.data.frame(matrix(...))}


#' @name .ac
#' @title shortcut
#' @description shortcut of as.character()
#' @param ... R object
#' @returns a vector of characters.    
#' @export
#' @examples
#' .ac(1:2)
.ac=function(...){as.character(...)}


#' @name .an
#' @title shortcut
#' @description shortcut of as.numeric(). 
#' @param ... R object
#' @param warning suppress warnings
#' @returns a vector of numerical values.     
#' @export
#' @examples
#' .an("2")
.an=function(...,warning=FALSE){
    if(warning){as.numeric(...)
    }else{suppressWarnings(as.numeric(...))}
    }

#' @name .annt
#' @title shortcut
#' @description as.numeric of the names of table applied to a vector 
#' @param vector vector object
#' @returns a vector of numerical values.        
#' @export
#' @examples
#' .an("2")    
.annt=function(vector){as.numeric(names(table((vector))))}

#' @name .alrna
#' @title shortcut
#' @description creates a list of (possibly named) elements
#' @param integer integer indicating the length of the list
#' @param names name of each element of the list (default is NULL)
#' @returns an object of class 'list'        
#' @export
#' @examples
#' .alrna(10,LETTERS[1:10])
.alrna=function(integer,names=NULL){
	out=as.list(rep(NA,integer))
	if(!is.null(names)){names(out)=names}
	out
	}

#' @name .p
#' @title shortcut
#' @description lazy shortcut for paste0
#' @param ... objects
#' @param sep a character string to separate the terms
#' @export
#' @examples
#' .p(LETTERS[1:10],collapse="+")
.p=function(...,sep=""){paste(...,sep=sep)}


#' @name .cat
#' @title for loop iteration print
#' @description cat a dot or an iteration number at each iteration of a for loop. Idea is to see progress.
#' @param i iteration number
#' @param n.loop number of loop
#' @param n.dotline number dot per line
#' @param msg message text
#' @export
#' @examples
#' for(i in 1:200){.cat(i,200,10)}
.cat=function(i,n.loop,n.dotline=50,msg=NULL){
              if(any(seq(0,n.loop,n.dotline)==i)){cat(i,msg,date(),"\n")}else{cat(".")}
              }

#' @name .isin
#' @title 'is in' function 
#' @description defines if an interval (of two values) contains a value of interest. Most often used to check if a confidence interval contains the true value
#' @param ci (confidence) interval
#' @param theta value of interest
#' @returns a logical (TRUE or FALSE)                      
#' @export
#' @examples
#' .isin(t.test(rnorm(20),rnorm(20))$conf.int,0)
.isin=function(ci,theta){if(!any(is.na(ci))){if(theta>=ci[1]&theta<=ci[2]){T}else{F}}else{NA}}

#' @name .o
#' @title shortcut
#' @description shuts down all open graphics devices
#' @export
#' @examples
#' \dontrun{
#' .o()
#' }
.o=function(){graphics.off()}

#' @name .expit
#' @title shortcut
#' @description defines the expit function on a vector of numerical values. typically used to define the probabilities of success related to a logistic regression.
#' @param x vector of rational numbers 
#' @returns a vector of rational numbers belonging to (0,1)               
#' @export
#' @examples
#' .expit(0)
.expit = function(x){exp(x)/(1+exp(x))}

#' @name .logit
#' @title shortcut
#' @description defines the .logit function on a vector of numerical values belonging to (0,1). typically used with probabilities related to a linear regression.
#' @param p vector of rational numbers belonging to (0,1)
#' @returns a vector of rational numbers          
#' @export
#' @examples
#' .logit(0.5)
.logit = function(p){log(p/(1-p))}

#' @name .trunc
#' @title shortcut
#' @description removes the empty spaces coming after the last character for each element of a vector of strings and thus transforms, for example, "abc def gh     " into "abc def gh". 
#' @param x input vector of strings
#' @returns a vector of strings         
#' @export
#' @examples
#' .trunc("abc def gh     ")
.trunc = function(x){
    .trunc0 = function(x){
        if(is.character(x)){
            if(!is.na(x)){
                pos = max(which(is.na(match(1:nchar(x),gregexpr(" ",x)[[1]]))))
                substr(x,1,pos)
            }else{
                NA
            }
        }else{
            NA
        }
        }
    sapply(x,.trunc0)
    }

#' @name .fill
#' @title shortcut
#' @description for printing purposes, add characters of choice (0s or empty spaces, for example) to a vector of string. typically used to ensure ids have the same number of characters
#' @param x input vector of strings
#' @param npos number of position to add, default to the max of the input vector
#' @param with character to add (default is an empty space)
#' @param front a logical indicating if the chracter is to add at the front (default) 
#' @returns a vector of strings (with added characters)       
#' @export
#' @examples
#' .fill(1:20,with="0")
.fill = function(x,npos=NULL,with=" ",front=TRUE){
    # x=1; npos=3; with=0;front=TRUE
    .fill0 = function(x,npos=2,with=" ",front=TRUE){
        # x=1; npos=3; with=0;front=TRUE
        x = .ac(x)
        if(nchar(x)>npos){
            .w("nchar(x)>pos banane")
            .ac(x)
        }else{
        if(nchar(x)==npos){
            .ac(x)
        }else{
            .p(if(front){.p(rep(with,npos-nchar(x)),collapse="")},
               .ac(x),
               if(!front){.p(rep(with,npos-nchar(x)),collapse="")},
               collapse="")
        }}
        }
    #
    sapply(x,.fill0,npos=ifelse(is.null(npos),max(nchar(x)),npos),with=with,front=front)
    }



####### head
#' .h
#'
#' @param x R object
#' @param size size of teh output
#'
#' @export
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


#' .eval
#'
#' evaluate "character"-Rcommand
#' @param expr expression
#' @param ... other parameters
#' @export

.eval=function(expr,...){eval(parse(text = noquote(expr)),...)}




#' .nf
#'
#' the function ".nf" for "No Factor" transform "factor-vector
#' of dataframe in "character" (when importing a dataset with read,
#' more elegant to use stringsAsFactors=FALSE)
#'
#' @param file a dataframe or a matrix
#' @export

.nf=function(file){
	temp=file
	vect.colw=seq(1,dim(temp)[2])[sapply(temp,class)=="factor"]
	if(length(vect.colw)>0){
	for(colw in 1:length(vect.colw)){temp[,vect.colw[colw]]=as.character(temp[,vect.colw[colw]])}
	}
	temp
	}

#' .adf
#'
#' as.numeric of as.character
#' @param vector input vector
#' @export
.anac=function(vector){as.numeric(as.character(vector))}


#' .adf
#'
#' as.data.frame
#' @param matrix input matrix
#' @param colnames colnames
#' @param rownames rownames
#' @export

.adf=function(matrix,colnames=NULL,rownames=NULL){
    dfw = as.data.frame(matrix)
    if(!is.null(colnames)){colnames(dfw) = colnames}
    if(!is.null(rownames)){rownames(dfw) = rownames}
    dfw
    }


#' .adfm
#'
#' matrix as.data.frame
#' @param ... R object
#' @export

.adfm=function(...){as.data.frame(matrix(...))}

#' .ac
#'
#' as.character
#' @param ... R object
#' @export
.ac=function(...){as.character(...)}


#' .an
#'
#' as.numeric
#' @param ... R object
#' @param warning suppress warnings
#' @export
.an=function(...,warning=FALSE){
    if(warning){as.numeric(...)
    }else{suppressWarnings(as.numeric(...))}
    }

####### as.numeric of names of table
#' .annt
#'
#' as.numeric of names of table
#' @param vector vector object
#' @export

.annt=function(vector){as.numeric(names(table((vector))))}


#' .alrna
#'
#' as.list rep na
#' @param integer number of rep
#' @param names default is NULL
#' @export

.alrna=function(integer,names=NULL){
	out=as.list(rep(NA,integer))
	if(!is.null(names)){names(out)=names}
	out
	}

#' .p
#'
#' paste with default of sep=""
#' @param ... objects
#' @param sep a character string to separate the terms
#' @export

.p=function(...,sep=""){paste(...,sep=sep)}


#' .cat
#'
#' cat a dot or iteration number in a loop
#' @param i iteration number
#' @param n.loop number of loop
#' @param n.dotline number dot per line
#' @param msg message text
#' @export

.cat=function(i,n.loop,n.dotline=50,msg=NULL){
              if(any(seq(0,n.loop,n.dotline)==i)){cat(i,msg,date(),"\n")}else{cat(".")}
              }

#' .isin
#'
#' confidence intervals: does theta belong to ci ?
#' @param ci  confidence intervals
#' @param theta theta
#' @export

.isin=function(ci,theta){if(!any(is.na(ci))){if(theta>=ci[1]&theta<=ci[2]){T}else{F}}else{NA}}



#' .o
#'
#' kill all graphics
#' @export
.o=function(){graphics.off()}


#########
#' .expit
#'
#' expit function
#' @param x input value
#' @export
.expit = function(x){exp(x)/(1+exp(x))}



#' .logit
#'
#' .logit function
#' @param p input value
#' @export

.logit = function(p){log(p/(1-p))}


#' .trunc
#'
#' Trunc function (useful for names)
#' transform "abc def gh     " to "abc def gh"
#' @param x input string
#' @export

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


#' .idf
#'
#' id -> function
#' @param input input id
#' @param name name
#' @export

.idf = function(input,name=NULL){
    # warnings
    w1 = ifelse(class(input)=="table",
                length(unique(names(input)))!=length(input),
                length(unique(input))!=length(input))
    w2 = is.null(name)
    if(w1|w2){
        if(w1){.w("duplicted id not allowed")}
        if(w2){.w("name can't be empty")}
    }else{
        # elements
        n = length(input)
        if(class(input)=="table"){
            id = names(input)
            if(all(!is.na(.an(id)))){id = .ac(id[order(.an(id))])}
        }else{id = input}
        id = data.frame(pos=1:n,id=.ac(id),stringsAsFactors=FALSE,
                        row.names=id)
        if(class(input)=="table"){
            id$n = unlist(c(input))
            if(!any(is.na(suppressWarnings(.an(names(input),warning=FALSE))))){id$value = .an(names(input),warning=FALSE)}
        }else{
            if(!any(is.na(suppressWarnings(.an(input,warning=FALSE))))){id$value = .an(input,warning=FALSE)}
            }
        # save
        assign(.p("n.",name),n,pos=.GlobalEnv)
        assign(.p("id.",name),id,pos=.GlobalEnv)
        }
    }


#' .ide
#'
#' id -> element
#' @param x input
#' @export

.ide = function(x){
    c(apropos("^id[.]"),apropos("^n[.]"))
    }



#' .date
#' @param x input
#' @export
 .date = function(x){format(Sys.time(), "%Y%m%d")}


#' .ad
#' as.Date
#' @param x input
#' @export

.ad = function(x){as.Date(x)}


#' .fill
#' for title, add stuff
#' @param x input R object
#' @param npos npos
#' @param with with
#' @param front front

#' @export

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


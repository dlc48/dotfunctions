

####### evaluate "character"-Rcommand 
.eval=function(expr,...){eval(parse(text = noquote(expr)),...)}

####### the function ".nf" for "No Factor" transform "factor-vector 
#       of dataframe in "character" (when importing a dataset with read, 
#       more elegant to use stringsAsFactors=FALSE)
.nf=function(file){
	# file = a dataframe or a matrix 
	temp=file
	vect.colw=seq(1,dim(temp)[2])[sapply(temp,class)=="factor"]
	if(length(vect.colw)>0){
	for(colw in 1:length(vect.colw)){temp[,vect.colw[colw]]=as.character(temp[,vect.colw[colw]])}
	}
	temp
	}

####### as.numeric of as.character
.anac=function(vector){as.numeric(as.character(vector))}

####### as.data.frame
.adf=function(matrix,colnames=NULL,rownames=NULL){
    dfw = as.data.frame(matrix)
    if(!is.null(colnames)){colnames(dfw) = colnames}
    if(!is.null(rownames)){rownames(dfw) = rownames}    
    dfw
    }

####### as.data.frame
.adfm=function(...){as.data.frame(matrix(...))}

####### as.character
.ac=function(...){as.character(...)}

####### as.numeric
.an=function(...,warning=TRUE){
    if(warning){as.numeric(...)
    }else{suppressWarnings(as.numeric(...))}
    }

####### as.numeric of names of table
.annt=function(vector){as.numeric(names(table((vector))))}

####### as.list rep na
.alrna=function(integer,names=NULL){
	out=as.list(rep(NA,integer))
	if(!is.null(names)){names(out)=names}
	out
	}

####### paste with default of sep=""
.p=function(...,sep=""){paste(...,sep=sep)}

####### cat a dot or iteration number in a loop
.cat=function(i,n.loop,n.dotline=50,msg=NULL){
              # i: iteration number
              # n.loop: number of loop  
              # n.dotline: number dot per line
              if(any(seq(0,n.loop,n.dotline)==i)){cat(i,msg,date(),"\n")}else{cat(".")}
              }

######## confidence intervals: does theta belong to ci ?
.isin=function(ci,theta){if(!any(is.na(ci))){if(theta>=ci[1]&theta<=ci[2]){T}else{F}}else{NA}} 
    # .isin=function(ci,theta){if(!any(is.na(ci))){if(theta>ci[1]&theta<ci[2]){T}else{F}}else{NA}} # version until 20160313
    ## Gillian's version
    #    true = 0
    #    low = -.15
    #    high = -.30
    #    round(0.5*sign(high-true)+0.5*sign(true-low))

################################################################################
.o=function(){graphics.off()}
    # kill all graphics


######### expit function
.expit = function(x){exp(x)/(1+exp(x))}

######### .logit function
.logit = function(p){log(p/(1-p))}

######### trunc function (useful for names)
.trunc = function(x){
    # transform "abc def gh     " to "abc def gh" 
    # x = "abc def gh     "
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
    #
    sapply(x,.trunc0) 
    } 

######## id -> function
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
            if(!any(is.na(.an(names(input),warning=FALSE)))){id$value = .an(names(input),warning=FALSE)}
        }else{
            if(!any(is.na(.an(input,warning=FALSE)))){id$value = .an(input,warning=FALSE)}
            }
        # save
        assign(.p("n.",name),n,pos=.GlobalEnv)
        assign(.p("id.",name),id,pos=.GlobalEnv)  
        }
    }

######## id -> element
.ide = function(x){
    c(apropos("^id[.]"),apropos("^n[.]"))    
    }

    
######## 
.date = function(x)format(Sys.time(), "%Y%m%d")

######## for title, add stuff
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


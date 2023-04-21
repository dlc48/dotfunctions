

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
.an=function(...){as.numeric(...)}

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
    # kill all gfaphics

######## cbind.files.fun used in radio and tv-research 
.cbf=function(list.files,path=F,name.loaded.file="mx.g.zt"){
	# - list.files: list of files to bind
	# - path=T: on doit loader un fichier
	# - path=F: le fichier est dans .GlobalEnv
	# - name.loaded.file = nom du fichier loadé
	n.files=length(list.files)
	out=NULL
	for(filew in 1:n.files){
		if(path){
		load(list.files[filew])
		if(filew>1&name.loaded.file=="mx.k.zt"){mx.k.zt=mx.k.zt+prod(dim(out))}
		out=cbind(out,get(name.loaded.file))
		}else{
		out=cbind(out,get(list.files[filew]))
		}# end else
		}# end filew
	out=out[,order(as.numeric(colnames(out)))]
	out
	}

######### expit function
.expit = function(x){exp(x)/(1+exp(x))}
         
            
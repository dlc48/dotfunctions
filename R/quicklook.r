#' @name .ql
#' @title quick look function 
#' @description generates a 'quick look' of R objects of different classes. Currently handles matrices, data frames, lists and vectors. Typically prints a subset of the object (first rows and columns for matrices, for example) and some relevant summaries (like the class, number of 0s and NAs, for each column of a data frame, for example)
#' @param obj R object
#' @param dim1 max number of rows to display
#' @param dim2 max number of columns to display
#' @param dim3 max number of dimensions to display for array
#' @param length.vector max length of a vector
#' @param length.list max length of a list
#' @param digit rounding
#' @param inf.col "all" (default), "sample","none"
#' @param inf.list "all" (default), "sample","none"
#' @param full by default is TRUE
#' @export
#' @examples
#' .ql(.adfm(rnorm(100),ncol=4))
.ql=function(obj,dim1=10,dim2=6,dim3=3,length.vector=24,length.list=5,
             digit=3,inf.col="all",inf.list="sample",full=TRUE){
    # obj = matrix(1:10,ncol=2); dim1=10;dim2=6;dim3=3;length.vector=24;length.list=5; digit=3;inf.col="all";inf.list="sample";full=TRUE

    nameobj = deparse(match.call()[[2]])
    cl      = class(obj)
    if(inherits(obj,"tbl")){obj = .adf(obj)}
    # max dim and length for vectors, matrices, dataframes, arrays and lists
    dim1b=round(dim1/2,0);dim1=2*dim1b
    dim2b=round(dim2/2,0);dim2=2*dim2b
    dim3b=round(dim3/2,0);dim3=2*dim3b
    length.vector2=round(length.vector/2,0);length.vector=2*length.vector2
    length.list2=round(length.list/2,0);length.list=2*length.list2
    cat("\n",paste(rep("-", .Options$width),collapse=""),"\n",sep="")
    ###
    ### MATRIX OR DATAFRAME
    ###
    if(inherits(obj,c("matrix","data.frame"))){
        # info
        # if all numeric...: check for inf and percentage of negative vs positive values.
        dimobj=dim(obj)
        pdo=prod(dimobj)
        sumnaw=sum(is.na(obj))
        #colclass=apply(obj,2,class)                                # 20160309
        colclass      = sapply(obj,class)                           # 20160309
        colclass.name = unlist(lapply(colclass,.p,collapse="-"))    # 20160309
        colclass      = unlist(lapply(colclass,function(x)x[1]))    # 20160309
        if(all(colclass=="numeric")|
           all(colclass=="integer")|
           all(colclass=="double")){
           infw=apply(obj,2,function(x)sum(is.infinite(x)))
           negposw=format(c(sum(obj<0,na.rm=T),sum(obj>0,na.rm=T))/pdo*100,digit=digit)
           }
        ### SAMPLE of OBJ
        # limit rows
        if(dimobj[1]>dim1){
        obj2=as.data.frame(matrix(NA,nrow=dim1+1,ncol=dimobj[2]))
        obj2[-(dim1b+1),]=format(obj[c(1:dim1b,(dimobj[1]-dim1b+1):dimobj[1]),],digit=digit)
        if(!is.null(dimnames(obj)[[1]])){
            dimnames(obj2)[[1]][dim1b+1]=" "
            dimnames(obj2)[[1]][-(dim1b+1)]=dimnames(obj)[[1]][c(1:dim1b,(dimobj[1]-dim1b+1):dimobj[1])]
            }else{
            dimnames(obj2)[[1]][-(dim1b+1)]=as.character(c(1:dim1b,(dimobj[1]-dim1b+1):dimobj[1]))
            }
        dimnames(obj2)[[1]][(dim1b+1)]=obj2[dim1b+1,]="..."
        }else{obj2=obj}
        # limit cols
        if(dimobj[2]>dim2){
        obj2=obj2[,c(1:dim2b,(dimobj[2]-dim2b):dimobj[2])]
        if(!is.null(dimnames(obj)[[2]])){
            dimnames(obj2)[[2]][-(dim2b+1)]=dimnames(obj)[[2]][c(1:dim2b,(dimobj[2]-dim2b+1):dimobj[2])]
            }else{
            dimnames(obj2)[[2]][-(dim2b+1)]=as.character(c(1:dim2b,(dimobj[2]-dim2b+1):dimobj[2]))
            }
        dimnames(obj2)[[2]][(dim2b+1)]=obj2[,dim2b+1]="..."
        }else{
        if(!is.null(dimnames(obj)[[2]])){
            dimnames(obj2)[[2]]=dimnames(obj)[[2]]
            }else{
            dimnames(obj2)[[2]]=as.character(1:dimobj[2])
            }
        }
        #### INFO ABOUT COLUMNS OF OBJ
        if(inf.col!="none"){
            if(inf.col=="all"){colw=1:dimobj[2]}
            if(inf.col=="sample"){
                if(dimobj[2]>dim2){colw=c(1:dim2b,(dimobj[2]-dim2b+1):dimobj[2])
                }else{colw=1:dimobj[2]}
                }
            matx.inf.col=as.data.frame(matrix(NA,nrow=length(colw),ncol=8))
            dimnames(matx.inf.col)[[2]]=c("col","id","class","NA","0","table","min","max")
            for(i in 1:length(colw)){
                dataw = obj[,colw[i]]
                clw   = colclass[i] # class(dataw) # 20160309
                if(!is.null(dimnames(obj)[[2]])){nw=dimnames(obj)[[2]][(colw[i])]
                    }else{nw="-"}
                if(clw=="numeric"|clw=="integer"|clw=="double"|clw=="logical"){
                    rangew=format(range(as.numeric(dataw),na.rm=T),digit=digit)
                    }else{if(clw=="POSIXlt"){
                    rangew=format(range(dataw))
                    }else{
                    rangew=c(".",".")}
                    }
                matx.inf.col[i,]=c(colw[i],nw,colclass.name[i],
                                  sum(is.na(dataw)),sum(dataw==0,na.rm=T),
                                  if(clw!="POSIXlt"){length(table(dataw))
                                  }else{length(table(format(dataw)))},
                                  rangew)
                }
            }
        ### print
        cat("\nQUICK LOOK on ",.p(cl,collapse="/")," '",nameobj,"'\n\n",sep="")
        cat("-> dim    : ",dimobj[1]," rows, ",dimobj[2]," columns\n",sep="")
        cat("-> NA     : ",sumnaw," / ",format(sumnaw/pdo*100,digit=digit),
            "% total (",sum(apply(is.na(obj),2,sum)>0)," columns)\n",sep="")
        cat("-> 0      : ",sum(obj==0,na.rm=T)," total (",sum(apply(obj==0,2,sum,na.rm=T)>0),
            " columns)\n",sep="")
        if(all(colclass=="numeric")|
           all(colclass=="integer")|
           all(colclass=="double")){
            cat("-> Inf    : ",sum(infw)," total (",sum(infw>0)," columns)\n",sep="")
            cat("-> -/+    : ",negposw[1],"% / ",negposw[2],"%\n",sep="")
            }
        cat("\nSAMPLE of ",.p(cl,collapse="/"),"\n\n",sep="")
        print(obj2)
        cat("\n")
        if(inf.col!="none"){
            cat("COLUMNS of ",.p(cl,collapse="/"),"\n\n",sep="")
            print(matx.inf.col)
            cat("\n")
            }
    }
    ###
    ### Arrays
    ###
    #if(cl=="array"){
    #    for(dw in 1:dim3){
    #        obja = obj[,,3]
    #        }
    #    }

    ###
    ### VECTORS AND SCALARS
    ###
    if(inherits(obj,c("numeric","character","integer","double","logical"))){
        # info
        length.vectorw=length(obj)
        # if length.vector = 1
        if(length.vectorw==1){
            if(inherits(obj,c("character"))){
                cat("\nQUICK LOOK on '",nameobj,"'\n\n",sep="")
                cat("-> class  : ",.p(cl,collapse="/"),"\n",sep="")
                cat("-> length : ",length.vectorw,"\n",sep="")
                cat("\nPRINT \n\n")
                print(obj)
                cat("\n")
            }else{
                cat("\nQUICK LOOK on scalar '",nameobj,"'\n\n",sep="")
                cat("-> class  : ",.p(cl,collapse="/"),"\n",sep="")
                cat("-> length : ",length.vectorw,"\n",sep="")
                cat("\nPRINT \n\n")
                print(obj)
                cat("\n")
            }
            }
        # if length.vector>1
        if(length.vectorw>1){
            statw=sum(is.na(obj))
            if(!inherits(obj,c("character"))){
                statw=c(statw,sum(obj==0,na.rm=T),# 0
                    sum(is.infinite(obj)),# Inf
                    c(sum(obj<0,na.rm=T),sum(obj>0,na.rm=T)),# -/+
                    range(obj))
                }
            pc.statw=format(statw/length.vectorw*100,digit=digit)
            statw=format(statw,digit=digit)
            # print
            cat("\nQUICK LOOK on vector '",nameobj,"'\n\n",sep="")
            cat("-> class  : ",.p(cl,collapse="/"),"\n",sep="")
            cat("-> length : ",length.vectorw,"\n",sep="")
            cat("-> NA     : ",statw[1]," / ",pc.statw[1],"%\n",sep="")
            if(!inherits(obj,c("character"))){
                cat("-> 0      : ",statw[2]," / ",pc.statw[2],"%\n",sep="")
                cat("-> Inf    : ",statw[3]," / ",pc.statw[3],"%\n",sep="")
                cat("-> -/+    : ",pc.statw[4],"% / ",pc.statw[5],"%\n",sep="")
                cat("-> min    : ",statw[6],"\n",sep="")
                cat("-> max    : ",statw[7],"\n",sep="")
            }
            cat("\nSAMPLE of vector\n\n")
            if(length.vectorw>length.vector){
                dataw=format(obj[c(1:length.vector2,(length.vectorw-length.vector2+1):length.vectorw)],digit=digit)
                cat(dataw[1:length.vector2],"\n")
                cat(" ...\n")
                cat(dataw[(length.vector2+1):length.vector],"\n")
                }else{
                print(obj)
                }
            cat("\n")
        }
    }
    ###
    ### LISTS
    ###
    if(inherits(obj,c("list","pairlist"))){
        # info
        length.listw=length(obj)
        name.list=names(obj)
        null.name.list=is.null(name.list)
        cl.list=lapply(obj,class)
        t.cl.list=table(unlist(cl.list))
        nt.cl.list=names(t.cl.list)
        # print:
        cat("\nQUICK LOOK on list '",nameobj,"'\n\n",sep="")
        cat("-> length : ",length.listw,"\n",sep="")
        # names
        if(null.name.list){
        cat("-> names  : F\n",sep="")}else{
        cat("-> names  : T (first and last: '",
            name.list[1],"', '",name.list[length.listw],"')\n",sep="")
        }
        # class
        if(length(nt.cl.list)==1){
        cat("-> class  : all ",nt.cl.list,"\n",sep="")}else{
        cat("-> class  : ")
        print(t.cl.list)
        cat("\n")
        }
        # sample
        cat("\nSAMPLE of list\n\n")
        if(length.listw>length.list){
            list.objw=obj[c(1:length.list2,(length.list-length.list2+1):length.list)]
            print(list.objw[1:length.list2])
            if(null.name.list){cat("\n[[...]]\n ...\n\n")}else{
                cat("\n$'...'\n ...\n\n")
                }
            print(obj[(length.list2+1):length.list2])
        }else{
        print(obj)
        }
        }
    ###
    ### FUNCTION
    ###
    if(inherits(obj,c("function"))){
        # info
        formalsw=formals(as.character(nameobj))
        n.formalsw=length(formalsw)
        matx.args=as.data.frame(matrix(NA,nrow=n.formalsw,ncol=2))
        dimnames(matx.args)[[2]]=c("id","default")
        matx.args[,1]=names(formalsw)
        for(i in 1:n.formalsw){matx.args[i,2]=
            ifelse(is.null(formalsw[[i]]),".",
                   ifelse(as.character(formalsw[[i]])=="",".",as.character(formalsw[[i]]))
                   )
            }
        # print
        cat("\nQUICK LOOK on ",cl," '",nameobj,"'\n\n",sep="")
        cat("-> arguments :\n",sep="")
        print(matx.args)
        cat("\n")
        }
    ### end
    cat("\n",paste(rep("-", .Options$width),collapse=""),"\n\n",sep="")
    }



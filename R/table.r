#' @name .sig
#' @title sig function 
#' @description star system for p-values.
#' @param pval vector of p-values
#' @returns output is a vector of class 'character'.
#' @export
#' @examples
#' .sig(c(0,0.01,0.05,0.1,1))
.sig = function(pval){
    out = rep("",length(pval))
    out[pval<=  .1 & !is.na(pval)] = "."
    out[pval<= .05 & !is.na(pval)] = "*"
    out[pval<= .01 & !is.na(pval)] = "**"
    out[pval<=.001 & !is.na(pval)] = "***"
    out
    }

#' @name .pval
#' @title p-val function 
#' @description print function for p-values, change 1e-299 into <0.0001. 
#' @param pval vector of p-values
#' @param digit number of digits for rounding rule
#' @returns output is a vector of class 'character' with a fixed number of postions after the comma.
#' @export
#' @examples
#' .pval(c(0,0.01,0.05,0.1,1))    
.pval = function(pval,digit=4){
    out = format(c(.an(.p("0.",.p(rep(1,digit),collapse=""))),round(pval,digits=digit)))[-1]
    out[is.na(pval)] = ""
    out[!is.na(match(out,c("0.00000","0.0000","0.000","0.00","0.0","0")))] = .p("<0.",.p(rep(0,digit-1),collapse=""),"1")
    out
    }

#' @name .tex
#' @title tex function 
#' @description defines the centre part of the tex code corresponding to a Table (and typically called, in TeX, via the input function). the input is a matrix with row and column names.
#' @param input the table, including rownames and colnames
#' @param file the name and path of the tex file
#' @param cmidrule a matrix of same size as input with non-NA entries
#'  for position with a cmidrule. Example: c(NA,1,1,2,2) for a given row
#' @param col.row colour of each row
#' @param col.col colour of each column
#' @param pos.textbfrow position of the rows with bold names
#' @param pos.textbfcol position of the columns with bold names
#' @param pos.centeredrow position of the columns which should be centered in row
#' @param pos.centeredcol position of the rows which should be centered in column
#' @export
.tex = function(input,file=NULL,cmidrule=NULL,
                col.row=NULL,col.col=NULL,
                pos.textbfrow=NULL,pos.textbfcol=NULL,
                pos.centeredrow=NULL,pos.centeredcol=NULL){

    # size
    n = nrow(input)
    p = ncol(input)
    input[input==""] = NA
    input[input==" "] = NA
    input[input=="  "] = NA
    # checks
    if(!is.null(cmidrule)){
        if(!all(dim(input)==dim(cmidrule))){.w("cmidrule matrix's dimension not equal to input")}
    }
    if(!is.null(col.row)){
        if(length(col.row)!=nrow(input)){.w("col.row length not compatible with input size")}
    }
    if(!is.null(col.col)){
        if(length(col.col)!=ncol(input)){.w("col.col length not compatible with input size")}
    }
    # adapt input to cex/col
    if(!is.null(col.row)){
        for(i in 1:n){
            if(!is.na(col.row[i])){
                posw = which(!is.na(input[i,]))
                input[i,posw] = .p("{\\color{",col.row[i],"}",input[i,posw],"}")
                }
            }
    }
    if(!is.null(col.col)){
        for(i in 1:p){
            if(!is.na(col.col[i])){
                posw = which(!is.na(input[,i]))
                input[posw,i] = .p("{\\color{",col.col[i],"}",input[posw,i],"}")
                }
            }
    }
    if(!is.null(pos.textbfrow)){
        for(i in 1:length(pos.textbfrow)){
            posw = which(!is.na(input[pos.textbfrow[i],]))
            input[pos.textbfrow[i],posw] = .p("\\textbf{",input[pos.textbfrow[i],posw],"}")
            }
    }
    if(!is.null(pos.textbfcol)){
        for(i in 1:length(pos.textbfcol)){
            posw = which(!is.na(input[,pos.textbfcol[i]]))
            input[posw,pos.textbfrow[i]] = .p("\\textbf{",input[posw,pos.textbfrow[i]],"}")
            }
    }
    # print
    for(i in 1:nrow(input)){# i=1
        #cat(i,"\t")
        inputw = input[i,]
        deletedcol = NULL
        # cmidrule
        if(!is.null(cmidrule)){
            cmidrulew = cmidrule[i,]
            if(!all(is.na(cmidrulew))){
                pos_rule = split(1:p,cmidrulew)
                for(rulew in 1:length(pos_rule)){
                    write(.p("\\cmidrule(r){",min(pos_rule[[rulew]]),"-",max(pos_rule[[rulew]]),"}"),file=file,append=TRUE)
                    }
                }
            }
        # centering of the elements of a row by column
        #cat("row\t")
        if(!is.null(pos.centeredrow)){
            if(any(pos.centeredrow==i)){
            # prepare (initial NA's are special cases)
            cumsumw = cumsum(!is.na(inputw))
            if(any(cumsumw==0)){
                sum0 = sum(cumsumw==0)
                cumsumw[1:sum0] = 1:sum0
                cumsumw[-c(1:sum0)] = cumsumw[-c(1:sum0)]+sum0
                }
            # do
                #deletedcol = unlist(sapply(split(1:p,cumsumw),function(x){x[-1]}))
            elements = split(unlist(inputw),cumsumw)
            inputw = sapply(elements,function(x){
                if(!is.na(x[1])){.p("\\multicolumn{",length(x),"}{c}{",x[1],"}")}else{NA}
                })
            }
        }
        # centering of the elements of a column by row
        #cat("col\t")
        if(!is.null(pos.centeredcol)){
            for(j in 1:length(pos.centeredcol)){# j=1
                posw = pos.centeredcol[j]
                # only if position non-na...
                if(!is.na(input[i,posw])){
                    # have to define how deep this goes: prepare
                    cumsumw = cumsum(!is.na(input[,posw]))
                    if(any(cumsumw==0)){
                        sum0 = sum(cumsumw==0)
                        cumsumw[1:sum0] = 1:sum0
                        cumsumw[-c(1:sum0)] = cumsumw[-c(1:sum0)]+sum0
                        }
                    # do
                    inputw[posw] = .p("\\multirow{",sum(cumsumw==cumsumw[i]),"}*{",input[i,posw],"}")
                    }
                }
            }

        # print
        #cat("print\t")
        inputw = sapply(inputw,function(x)gsub("&", "\\\\&", x))
        textw = .p(.p(sapply(.ac(inputw),function(x)if(is.na(x)){" "}else{x}),
                   collapse=" & "), " \\tabularnewline",collapse=" ")
        textw = gsub("_", "\\\\_", textw)
        textw = gsub("-", "$-$", textw)
        textw = gsub("<", "$<$", textw)
        textw = gsub(">", "$>$", textw)
        textw = gsub("%", "\\\\%", textw)
        write(textw,file=file,append=ifelse(i==1,FALSE,TRUE))
        #cat("\n")
        }
    }





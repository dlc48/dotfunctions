#' @name .idf
#' @title id function 
#' @description creates, in .GlobalEnv (!), an 'id' data frame based on a vector (of strings or numerical values) or on the output of the function table() applied to a vector as well as an object indicating the numbers of rows of the data frame. If the name is 'X', the id data.frame will be named 'id.X' and the size scalar 'n.X'. This function will replace any objects in .GlobalEnv having the same target name if permission is granted (see argument 'ask').
#' @param input input id
#' @param name name
#' @param ask a logical. When ask equals TRUE (default), the function checks if an r object of .GlobalEnv is already named 'id.X' or 'n.X' (where X is the name specified in the previous argument) exists and, if it does, ask for the authorisation to overwrite them. The question is not asked when options()$dotfunctions_dontask == TRUE, where options()$dotfunctions_dontask can be set to TRUE (or FALSE) by running options(dotfunctions_dontask = TRUE) in .First, for example, and is set to TRUE in an R session if the user once answers 'always' to the question.
#' @param fill when the length of numerical-based ids are different ("1", "100"), fill allow to 'fill' the blank so that they have the same length ("001","100"). Default to backward comparible `fill = NUll` (no action taken, different sizes accepted).
#' @returns a data frame with columns 'pos' (scalar vector going from 1 to the number of rows), 'id' (character vector ordered by numerical value whe the input is numerical) and possibly 'n' (numerical vector) when the input is numerical or the output of the function table(). the returned object is of class 'data.frame' and '.idf'
#' @export
#' @examples
#' \dontrun{
#' .idf(table(rpois(10,3)),"X")
#' print(id.X)
#' print(n.X)
#' }
.idf = function(input, name, ask=TRUE,fill=NULL){
    # warnings
    w1 = ifelse(inherits(input,"table"),
                length(unique(names(input)))!=length(input),
                length(unique(input))!=length(input))
    w2 = is.null(name)
    if(w1|w2){
        if(w1){.w("duplicted id not allowed")}
        if(w2){.w("name can't be empty")}
    }else{
        # elements
        n = length(input)
        if(inherits(input,"table")){
            id = names(input)
            if(all(!is.na(.an(id)))){id = .ac(id[order(.an(id))])}
        }else{id = input}
        id = data.frame(pos=1:n,id=.ac(id),stringsAsFactors=FALSE,
                        row.names=id)
        if(inherits(input,"table")){
            id$n = unlist(c(input))
            if(!any(is.na(suppressWarnings(.an(names(input),warning=FALSE))))){id$value = .an(names(input),warning=FALSE)}
        }else{
            if(!any(is.na(suppressWarnings(.an(input,warning=FALSE))))){
                id$value = .an(input,warning=FALSE)
                id = id[order(id$value),]
                id$pos = 1:n
            }
        }
        if(any(colnames(id)=="value")&!is.null(fill)){
            if(nchar(fill)==1){
            id$id = rownames(id) = gsub(" ",fill,format(id$value))
            }
        }    
        # ask
        if(!is.null(options()$dotfunctions_dontask)){
            if(options()$dotfunctions_dontask){ask=FALSE}
        }                    
        if(ask){ 
            # n.XX             
            if(exists(.p("n.",name),envir=.GlobalEnv)){
                .w(.p("An object named 'n.",name,
                   "' already exists in .GlobalEnv."),immediate.=TRUE)
                answer = readline("\tOverwrite it? yes/no/always:\t")
                if(answer=="always"){options(dotfunctions_dontask = TRUE)}
                if((answer=="no"|answer=="n"|answer=="")&(answer!="yes"|answer!="y")){
                    stop("\nYou refused overwriting (fair!): pick another name and start over") 
               }
            }
            # id.XX
            cond1 = is.null(options()$dotfunctions_dontask)
            cond2 = ifelse(!cond1, options()$dotfunctions_dontask, FALSE)
            if(cond1 | !cond2){
                # answered 'always' before in the same R session
                if(exists(.p("id.",name),envir=.GlobalEnv)){
                    .w(.p("An object named 'id.",name,
                       "' already exists in .GlobalEnv."),immediate.=TRUE)
                    answer = readline("\tOverwrite it? yes/no/always:\t")
                    if(answer=="always"){options(dotfunctions_dontask = TRUE)}
                    if((answer=="no"|answer=="n"|answer=="")&(answer!="yes"|answer!="y")){
                        stop("\nYou refused overwriting (fair!): pick another name and start over") 
                    }
                }
            }
        }
        # assign 
        class(id) = c("data.frame",".idf")
        assign(.p("n.",name),n,pos=.GlobalEnv)
        assign(.p("id.",name),id,pos=.GlobalEnv)
    }
}




#' @name .ide
#' @title list of existing id elements 
#' @description identify all existing id objects (typically created by means of .idf in an environment (typically .GlobalEnv). 
#' @returns output is a string vector.
#' @export
#' @examples    
#' \dontrun{    
#' .ide()
#' }    
.ide = function(){
    c(apropos("^id[.]"),apropos("^n[.]"))
}


#' @name .idc
#' @title  combines id objects
#' @description creates an id object based on all combinations of levels of id objects of interest (created by means of .idf)
#' @param name name (by default 'comb' for 'combination')
#' @param ... list of id objects
#' @param sep the separator to be used for the id column of the resulting object
#' @param fill if TRUE, all elements of the id column have the same length
#' @param ask a logical. When ask equals TRUE (default), the function checks if an r object of .GlobalEnv is already named 'id.X' or 'n.X' (where X is the name specified in the previous argument) exists and, if it does, ask for the authorisation to overwrite them. The question is not asked when options()$dotfunctions_dontask == TRUE, where options()$dotfunctions_dontask can be set to TRUE (or FALSE) by running options(dotfunctions_dontask = TRUE) in .First, for example, and is set to TRUE in an R session if the user once answers 'always' to the question.
#' @returns a data frame with columns 'pos' (scalar vector going from 1 to the number of rows), 'id' (character vector ordered by numerical value whe the input is numerical) and as many columns as there were (unique) id objects to combine, each of them being identifiable via the id extension name (i.e, "x" for id.x, for example) . the returned object is of class 'data.frame' and '.idf'
#' @export
#' @examples    
#' \dontrun{
#' .idf(table(rpois(10,3)),"X")
#' .idf(c("a","b"),"Y")
#' .idc("cYX",id.Y,id.X,sep="&")
#' print(id.cYX)
#' print(n.cYX)
#' }
.idc = function(name="comb",...,sep="|",fill=TRUE,ask=TRUE){
    # select only unique .idf oebject
    dots = rlang::dots_list(...,.named=TRUE)
    dots = dots[unique(names(dots))]
    dots = dots[sapply(dots,inherits,".idf")]
    if(length(dots)==0){
        .w('no idf objects to combine')
        stop()
    }else{if(length(dots)==1){
        .w('only one (unique) idf object provided: nothing to combine')
        stop()
    }else{
        levels_dots = lapply(dots, function(x){x$id})
        names_dots  = sapply(names(dots), function(x){substr(x,4,nchar(x))})        
        if(any(names_dots==name)){
            .w("chosen 'name' matches the name of one of the id objects to combine")
            stop()      
        }
        n.dots      = sapply(dots,nrow)
        n           = prod(n.dots)
        id          = data.frame(pos=1:n, id=NA)
        for(dw in 1:length(dots)){            
            add = if(dw==1){
                      rep(levels_dots[[1]],each=prod(n.dots[-1]))
                  }else{if(dw==length(dots)){
                      rep(levels_dots[[length(dots)]],prod(n.dots[-length(dots)]))
                  }else{
                      rep(rep(levels_dots[[dw]],each=prod(n.dots[-c(1:dw)])),
                          prod(n.dots[-c(dw:length(dots))]))
                  }}
            id  = cbind(id,factor(add,levels=levels_dots[[dw]]))
            colnames(id)[ncol(id)] = names_dots[dw]   
            }
        # id and rownames
        if(fill){tmp = apply(id[,names_dots],2,.fill,with=" ")
        }else{   tmp = id[,names_dots]}
        id$id = rownames(id) = apply(tmp,1,paste0,collapse=sep)
    }}
    # ask
    if(!is.null(options()$dotfunctions_dontask)){
        if(options()$dotfunctions_dontask){ask=FALSE}
    }                    
    if(ask){ 
        # n.XX             
        if(exists(.p("n.",name),envir=.GlobalEnv)){
            .w(.p("An object named 'n.",name,
               "' already exists in .GlobalEnv."),immediate.=TRUE)
            answer = readline("\tOverwrite it? yes/no/always:\t")
            if(answer=="always"){options(dotfunctions_dontask = TRUE)}
            if((answer=="no"|answer=="n"|answer=="")&(answer!="yes"|answer!="y")){
                stop("\nYou refused overwriting (fair!): pick another name and start over") 
           }
        }
        # id.XX
        cond1 = is.null(options()$dotfunctions_dontask)
        cond2 = ifelse(!cond1, options()$dotfunctions_dontask, FALSE)
        if(cond1 | !cond2){
            # answered 'always' before in the same R session
            if(exists(.p("id.",name),envir=.GlobalEnv)){
                .w(.p("An object named 'id.",name,
                   "' already exists in .GlobalEnv."),immediate.=TRUE)
                answer = readline("\tOverwrite it? yes/no/always:\t")
                if(answer=="always"){options(dotfunctions_dontask = TRUE)}
                if((answer=="no"|answer=="n"|answer=="")&(answer!="yes"|answer!="y")){
                    stop("\nYou refused overwriting (fair!): pick another name and start over") 
                }
            }
        }
    }
    # assign 
    class(id) = c("data.frame",".idf")
    assign(.p("n.",name),n,pos=.GlobalEnv)
    assign(.p("id.",name),id,pos=.GlobalEnv)
}
#.idc("rho",id.hyp,id.rho)


#' @name .ar
#' @title id files based arrays
#' @description creates an array with dim(ensions) and dimnames coming from id files, typically generated via .idf().
#' @param id1 first id file
#' @param id2 second id file
#' @param id3 third id file
#' @param id4 fourth id file
#' @param id5 fifth id file
#' @param value content of the array, NA by default  
#' @returns an array (or a matrix when only 2 id files are provided)
#' @export
#' @examples     
#' \dontrun{       
#' .idf(table(rpois(10,3)),"X")
#' .idf(LETTERS[1:6],"Y")   
#' .ar(id.X,id.Y)    
#' }   
.ar = function(id1,id2,id3=NULL,id4=NULL,id5=NULL,value=NA){
    listw = list(id1,id2,id3,id4,id5)
    listw = listw[!sapply(listw,is.null)]
    # minimal checks
    if(length(listw)<2){.w("provide at least 2 id.XX files");stop()}
    if(any(sapply(listw,function(x)length(unique(x$id)))!=
       sapply(listw,nrow))){.w("invalid id.XX files");stop()}
    # define array
    array(value,dim=sapply(listw,nrow),
          dimnames=lapply(listw,function(x)x$id))
}




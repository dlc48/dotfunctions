#' @name .idf
#' @title id function 
#' @description creates, in .GlobalEnv (!), an 'id' data frame based on a vector (of strings or numerical values) or on the output of the function table() applied to a vector as well as an object indicating the numbers of rows of the data frame. If the name is 'X', the id data.frame will be named 'id.X' and the size scalar 'n.X'. This function will replace any objects in .GlobalEnv having the same target name (!).
#' @param input input id
#' @param name name
#' @returns a data frame with columns 'pos' (scalar vector going from 1 to the number of rows), 'id' (character vector ordered by numerical value whe the input is numerical) and possibly 'n' (numerical vector) when the input is numerical or the output of the function table().
#' @export
#' @examples
#' \dontrun{
#' .idf(table(rpois(10,3)),"X")
#' print(id.X)
#' print(n.X)
#' }
.idf = function(input,name){
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


#' @name .ide
#' @title id elements 
#' @description identify all existing id objects (typically created by means of .idf in an environment (typically .GlobalEnv)
#' @returns output is a string vector.
#' @export
#' @examples    
#' \dontrun{    
#' .ide()
#' }    
.ide = function(){
    c(apropos("^id[.]"),apropos("^n[.]"))
    }

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




#' @name .idf
#' @title id function 
#' @description creates, in .GlobalEnv (!), an 'id' data frame based on a vector (of strings or numerical values) or on the output of the function table() applied to a vector as well as an object indicating the numbers of rows of the data frame. If the name is 'X', the id data.frame will be named 'id.X' and the size scalar 'n.X'. This function will replace any objects in .GlobalEnv having the same target name (!).
#' @param input input id
#' @param name name
#' @returns a data frame with columns 'pos' (scalar vector going from 1 to the number of rows), 'id' (character vector ordered by numerical value whe the input is numerical) and possibly 'n' (numerical vector) when the input is numerical or the output of the function table().
#' @export
#' @examples
#' .idf(table(rpois(10,3)),"X")
#' print(id.X)
#' print(n.X)
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
#' .ide()
.ide = function(){
    c(apropos("^id[.]"),apropos("^n[.]"))
    }

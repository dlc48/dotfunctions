#' @name .xlss
#' @title shorcut 
#' @description print the names of the sheets of an excel file. Output is a vector of class 'character'.
#' @param file input Excel file
#' @export
.xlss = function(file){
    readxl::excel_sheets(file)
    }

#' @name .xls
#' @title shorcut 
#' @description import xls data. Output is of class 'data.frame'. The workhorse is 'readxl::read_excel' with 'guess' based on a large number of entries (not the default to avoid problems).
#' @param file input Excel file
#' @param sheet sheet
#' @param skip skip rows
#' @param col_names column names
#' @param col_types columnt types
#' @param ... ...
#' @returns object of class 'data.frame' 
#' @export
.xls = function(file,sheet=NULL,skip=0,col_names=TRUE,col_types="guess",...){
    if(is.null(sheet)){
        sheet = .xlss(file)[1]
    }else{
        if(is.na(match(sheet,.xlss(file)))){
            .w("sheet name doesn't exist in file")
        }
    }
    n_max = nrow(readxl::read_excel(file, sheet=sheet))
    .adf(readxl::read_excel(file, sheet=sheet,
         col_types=col_types,skip=skip,col_names=col_names,guess_max = min(1e+5, n_max),...))
    }


#' Title
#'
#' Read excel sheet names
#' @param file input Excel file
#' @export
.xlss = function(file){
    readxl::excel_sheets(file)
    }

#' .xls
#'
#'Import xls data
#' @param file input Excel file
#' @param sheet sheet
#' @param skip skip rows
#' @param col_names column names
#' @param col_types columnt types
#' @param ... ...
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

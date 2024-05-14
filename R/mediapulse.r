#' @name .cbf
#' @title cbind files function
#' @description function used in radio and tv-research to combine files
#' @param list.files list of files to cbind
#' @param path indicate path=TTRUE to load a file and path=FALSE if files are in GlobalEnv
#' @param name.loaded.file name of the loaded files (typically 'mx.x.zt', for example)
#' @returns combined matrix or data frame.
#' @export
.cbf=function(list.files,path=F,name.loaded.file="mx.g.zt"){
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







    
######## cbind.files.fun used in radio and tv-research 
.cbf=function(list.files,path=F,name.loaded.file="mx.g.zt"){
	# - list.files: list of files to bind
	# - path=T: on doit loader un fichier
	# - path=F: le fichier est dans .GlobalEnv
	# - name.loaded.file = name of the loaded file
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





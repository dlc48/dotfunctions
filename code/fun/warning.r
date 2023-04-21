.w=function(x=NULL){
	msg=paste(
	#"\n",
	"\t_________\n",
	"\t|     |\n",
	"\t|     o\n",
	"\t|    /|\\ \n",
	"\t|    / \\ \n",
	"\t|\n",
	"\t|\n",
	"\t-\n\n",sep="")
	warning(.p(msg,if(!is.null(x)).p("\t",x,"\n")),call.=FALSE)
	#if(!is.null(x)){
	#cat("\n---->\t")
	#cat(x)
	#cat("\n\n")
	#}
	}


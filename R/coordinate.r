#' @name .coord
#' @title coordinate function 
#' @description gives the matrix or array position corresponding to the coordinates of interest. Allows fast filling of arrays (via pointers). 
#' @param n.dk vector of integers indicating the coordinates of interest of a matrix or array. The vector length should match the dimension of the object of interest.
#' @param n.tk vector of integers indicating the dimension of the array of interest, typically dim(obj), where obj is the object of interest.
#' @returns output is a (numeric) scalar.  
#' @export
#' @examples
#' dimw = c(10,20,30,4) 
#' arw  = array(1:prod(dimw),dimw)
#' arw[.coord(c(10,20,15,3),dim(arw))] == .coord(c(10,20,15,3),dim(arw))
.coord=function(n.dk,n.tk){
	n.k=length(n.tk)
	if(n.k!=length(n.dk)){stop("n.dk and n.tk have different lengths")}
	for(k in 1:n.k){if(n.dk[k]>n.tk[k]){stop(cat("in dim", k, "n.dk > n.tk  :"))}}
	# calc
	temp=rep(n.dk[1],n.k)
	for(k in 1:(n.k-1)){temp[k]=prod(n.tk[1:(n.k-k)])*(n.dk[(n.k-k+1)]-1)}
	sum(temp)
	}


#' @name .coordr
#' @title reverse coordinate function 
#' @description reverse of the .coord() function: gives coordinates corresponding to a position of interest. 
#' @param kd position of interest
#' @param n.tk vector of integers indicating the dimension of the array of interest, typically dim(obj), where obj is the object of interest.
#' @returns output is a vector of scalars of length matching the dimension of the array. 
#' @export
#' @examples
#' dimw = c(10,20,30,4) 
#' arw  = array(1:prod(dimw),dimw)
#' pos  = 768
#' n.dk = .coordr(pos,dim(arw))	
#' arw[n.dk[1],n.dk[2],n.dk[3],n.dk[4]] == pos
.coordr=function(kd,n.tk){
	n.k=length(n.tk)
	n.dk=rep(NA,n.k-1)
	krd=kd
	# calc
	for(kw in n.k:2){
		n.dk[kw-1]=ceiling(krd/prod(n.tk[1:(kw-1)]))
		krd=krd-(n.dk[kw-1]-1)*prod(n.tk[1:(kw-1)])
		}
	# output
	out=c(krd,n.dk)
	out
	}





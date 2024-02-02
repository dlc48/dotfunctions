

###################################################################################
# for k >= 2
###################################################################################

#' .coord
#'
#' @param n.dk avec d pour desire et k = 1...n.k
#' @param n.tk avec t pour total  et k = 1...n.k
#'
#' @return sum(temp)
#' @export

.coord=function(n.dk,n.tk){
	# n.dk = vecteur des coordonnees desirees   (n.dk avec d pour desire et k = 1...n.k )
	# n.tk = vecteur des dim de l-array de base (n.tk avec t pour total  et k = 1...n.k )
	# n.k = nbre total de dim et k une dim quelquonque
	n.k=length(n.tk)
	if(n.k!=length(n.dk)){stop("coord and dimarray are different length")}
	for(k in 1:n.k){if(n.dk[k]>n.tk[k]){stop(cat("in dim", k, "n.dk > n.tk  :"))}}
	# calc
	temp=rep(n.dk[1],n.k)
	for(k in 1:(n.k-1)){temp[k]=prod(n.tk[1:(n.k-k)])*(n.dk[(n.k-k+1)]-1)}
	sum(temp)
	}



## tests:
#dimw=c(10,20,30,4)
#arry=array(1:prod(dimw),dimw)
#arry[coordkdim.fun(c(10,20,15,3),dimw)];coordkdim.fun(c(10,20,15,3),dimw)

#n.tk=c(5,3,2)
#arry=array(1:prod(n.tk),n.tk)


#' .coordr
#'
#' @param kd position dans k dim desiree   (1 < kd < prod(n.tk))
#' @param n.tk vecteur des dim de l-array de base (n.tk avec t pour total  et k = 1...n.k )
#'
#' @return out
#' @export

.coordr=function(kd,n.tk){
	# kd = position dans k dim desiree   (1 < kd < prod(n.tk))
	# n.tk = vecteur des dim de l-array de base (n.tk avec t pour total  et k = 1...n.k )
	# n.k = nbre total de dim et k une dim quelquonque
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

## tests:
#n.tk=c(5,4,3,2)
#arry=array(1:prod(n.tk),n.tk)
#kd=arry[4,3,2,2];kd;coordkdim.rfun(kd,n.tk)




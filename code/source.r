
if(.id=='rl') path.dotfunctions <- paste0(.path_git,'git_dotfunctions/')

source.files = dir(paste0(path.dotfunctions,"code/fun"),full.names=TRUE)
source.files = source.files[sapply(source.files, function(x) substr(x,nchar(x)-1, nchar(x)) == ".r")]
for(f in 1:length(source.files)) {
    source(source.files[f])
    }
rm(source.files,path.dotfunctions,f)    

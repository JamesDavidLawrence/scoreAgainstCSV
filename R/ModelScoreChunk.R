scoreAgainstCSV <- function(model,fin,fout,inside=TRUE,predNames=paste0(substitute(model),"Preds"),
                            chunkSize=1000L,verbose=TRUE,checkNames = TRUE, colClasses = NA,...){
	if(is(fin,"connection")){
		if(isOpen(fin,"w") || !isOpen(fin,"r")) stop("'fin' must be open for reading and not open for writing")
	}
	if(is(fout,"connection")){
		if(!isOpen(fout,"w")) stop("'fout' must be open for writing")
	}
	if(is.character(fin)){
		fin <- file(fin,open="r")
		on.exit(close(fin),add=TRUE)
	}
	if(is.character(fout)){
		fout <- file(fout,open="w")
		on.exit(close(fout),add=TRUE)
	}
	UseMethod("scoreAgainstCSV")
}

scoreAgainstCSVS4Generic <- function(model,fin,fout,inside=TRUE,predNames=paste0(substitute(model),"Preds"),
                                     chunkSize=1000L,verbose=TRUE,checkNames = TRUE, colClasses = NA,...){
	if(is(fin,"connection")){
		if(isOpen(fin,"w") || !isOpen(fin,"r")) stop("'fin' must be open for reading and not open for writing")
	}
	if(is(fout,"connection")){
		if(!isOpen(fout,"w")) stop("'fout' must be open for writing")
	}
	if(is.character(fin)){
		fin <- file(fin,open="r")
		on.exit(close(fin),add=TRUE)
	}
	if(is.character(fout)){
		fout <- file(fout,open="w")
		on.exit(close(fout),add=TRUE)
	}
	standardGeneric("scoreAgainstCSV")
}

scoreAgainstCSV.default <- function(model,fin,fout,inside=TRUE,predNames=paste0(substitute(model),"Preds"),
                                    chunkSize=1000L,verbose=TRUE,checkNames = TRUE, colClasses = NA,...){
	df.in <- try(read.csv(fin,stringsAsFactors=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
	if(inherits(df.in,"try-error")) return(invisible(NULL))
	df.names <- colnames(df.in)
	df.out <- cbind(df.in,predict(model,df.in,...))
	names(df.out) <- c(df.names,predNames)
	write.csv(df.out,fout,row.names=FALSE)
	n <- nrow(df.out)
	if(verbose) cat("scored",n,"rows\n")
	while(TRUE){
		df.in <- try(read.csv(fin,stringsAsFactors=FALSE,header=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
		if(inherits(df.in,"try-error")) return(invisible(NULL))
		names(df.in) <- df.names
		df.out <- cbind(df.in,predict(model,df.in,...))
		names(df.out) <- c(df.names,predNames)
		names(df.out) <- NULL
		write.csv(df.out[,],fout,row.names=FALSE)
		n <- n + nrow(df.out)
		if(verbose) cat("scored",n,"rows\n")		
	}
}

scoreAgainstCSV.expression <- function(model,fin,fout,inside=TRUE,
                                       predNames=paste0(substitute(model),"Preds"),
                                       chunkSize=1000L,verbose=TRUE,
                                       checkNames = TRUE, colClasses = NA,...){
	df.in <- try(read.csv(fin,stringsAsFactors=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
	if(inherits(df.in,"try-error")) return(invisible(NULL))
	df.names <- colnames(df.in)
	if(inside) df.out <- within(df.in,eval(model))
	if(!inside) eval(model)
	write.csv(df.out,fout,row.names=FALSE)
	n <- nrow(df.out)
	if(verbose) cat("scored",n,"rows\n")
	while(TRUE){
		df.in <- try(read.csv(fin,stringsAsFactors=FALSE,header=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
		if(inherits(df.in,"try-error")) return(invisible(NULL))
		names(df.in) <- df.names
		if(inside) df.out <- within(df.in,eval(model))
		if(!inside) eval(model)
		names(df.out) <- NULL
		write.csv(df.out[,],fout,row.names=FALSE)
		n <- n + nrow(df.out)
		if(verbose) cat("scored",n,"rows\n")		
	}
}

scoreAgainstCSV.gbm <- function(model,fin,fout,inside=TRUE,predNames=paste0(substitute(model),"Preds"),chunkSize=1000L,verbose=TRUE,
                                checkNames = TRUE, colClasses = NA,...){
	df.in <- try(read.csv(fin,stringsAsFactors=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
	if(inherits(df.in,"try-error")) return(invisible(NULL))
	for (v in names(df.in)){
		j <- match(v,model$var.names)
		vl <- model$var.levels[[j]]
		if(class(model$var.levels[[j]])=="character") df.in[[v]] <- factor(df.in[[v]],levels=vl) else df.in[[v]] <- as.numeric(df.in[[v]])
	}
	df.names <- colnames(df.in)
	df.out <- cbind(df.in,predict(model,df.in,...))
	names(df.out) <- c(df.names,predNames)
	write.csv(df.out,fout,row.names=FALSE)
	n <- nrow(df.out)
	if(verbose) cat("scored",n,"rows\n")
	while(TRUE){
		df.in <- try(read.csv(fin,stringsAsFactors=FALSE,header=FALSE,nrow=chunkSize,check.names=checkNames,colClasses=colClasses),silent=TRUE)
		if(inherits(df.in,"try-error")) return(invisible(NULL))
		names(df.in) <- df.names
		for (v in names(df.in)){
			j <- match(v,model$var.names)
			vl <- model$var.levels[[j]]
			if(class(model$var.levels[[j]])=="character") df.in[[v]] <- factor(df.in[[v]],levels=vl) else df.in[[v]] <- as.numeric(df.in[[v]])
		}
		df.out <- cbind(df.in,predict(model,df.in,...))
		names(df.out) <- c(df.names,predNames)
		names(df.out) <- NULL
		write.csv(df.out[,],fout,row.names=FALSE)
		n <- n + nrow(df.out)
		if(verbose) cat("scored",n,"rows\n")		
	}
}

## S4 variants
## can't be used without calling setOldClass("gbm")

setOldClass("gbm")
setGeneric("scoreAgainstCSV",signature=c("model"),def=scoreAgainstCSVS4Generic)
setMethod("scoreAgainstCSV",signature=signature(model="ANY"),scoreAgainstCSV.default)
setMethod("scoreAgainstCSV",signature=signature(model="expression"),scoreAgainstCSV.expression)
setMethod("scoreAgainstCSV",signature=signature(model="gbm"),scoreAgainstCSV.gbm)

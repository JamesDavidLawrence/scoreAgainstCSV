\name{scoreAgainstCSV}
\alias{scoreAgainstCSV}
\alias{scoreAgainstCSV.gbm}
\alias{scoreAgainstCSV.expression}
\alias{scoreAgainstCSV.gbm}
\alias{scoreAgainstCSV-methods}
\alias{scoreAgainstCSV,ANY-method}
\alias{scoreAgainstCSV,gbm-method}
\alias{scoreAgainstCSV,expression-method}
\title{
Score a model against a CSV file in chunks
}
\description{
This function scores a model against a CSV file, but doing it in chunks of a specified size, rather than all at once. This means that the memory consumption will be much lower and allows for scoring of data sets too large to fit into memory.
}
\usage{
scoreAgainstCSV(model, fin, fout, inside = TRUE, predNames = paste0(substitute(model), "Preds"), chunkSize = 1000L, verbose = TRUE,checkNames = TRUE, colClasses = NA, ...)
\S4method{scoreAgainstCSV}{ANY}(model, fin, fout, inside = TRUE, predNames = paste0(substitute(model), "Preds"), chunkSize = 1000L, verbose = TRUE,checkNames = TRUE, colClasses = NA, ...)
\S4method{scoreAgainstCSV}{gbm}(model, fin, fout, inside = TRUE, predNames = paste0(substitute(model), "Preds"), chunkSize = 1000L, verbose = TRUE,checkNames = TRUE, colClasses = NA, ...)
\S4method{scoreAgainstCSV}{expression}(model, fin, fout, inside = TRUE, predNames = paste0(substitute(model), "Preds"), chunkSize = 1000L, verbose = TRUE,checkNames = TRUE, colClasses = NA, ...)
}

\arguments{
  \item{model}{
The model object to score against. Different methods will be dispatched depending on the class of \code{model}.
}
  \item{fin}{
The input file. Can either be a quoted filename, or a connection which is open for reading but not for writing.
}
  \item{fout}{
The output file. Can either be a quoted filename, or a connection which is open for writing.
}
  \item{inside}{
Logical. Evaluate expressions "inside" the input data frame (using \code{\link{within}}), or in the environment of the function? Only used if \code{model} is of class "expression" or a subclass.
}
  \item{predNames}{
The name(s) to give to the new column(s) of predictions. Defaults to the name of the model object, with "Preds" on the end. Note that \code{predNames} must be specified explicitly if \code{scoreAgainstCSV} is applied to an \code{EMBglm} object model.
}
  \item{chunkSize}{
Number of rows per chunk. More will mean more memory usage, but may be faster in some situations and reduces computation overhead.
}
  \item{verbose}{
Logical. If TRUE, a message is printed after each chunk scored.
}

  \item{checkNames}{
Logical. Passed on to \code{check.names} argument in \code{read.csv} when reading in files.
}
  \item{colClasses}{
Logical. Passed on to \code{colClasses} argument in \code{read.csv} when reading in files.  It may be necessary to set this to `character` to avoid issues with reading in numeric (or mostly numeric) data which contain leading zeroes.
}
  \item{\dots}{
Any further arguments to pass to code{predict}.
}
}
\details{
\code{scoreAgainstCSV} is a (both S3 and S4) generic function, with specific methods defined for \code{model} of class \code{gbm} and \code{expression}. The \code{gbm} package is only required if that particular method is used.\cr
If \code{model} is an expression, then this can be used to provide some SAS-data-step-like functionality without taking up a great deal of memory space.\cr
The default model will attempt to make use of the methods of \code{predict}, but there is no guarantee it will work for a particular class of model. In particular you may run into issues of factor mis-coding, which you can solve by adding a block of code similar to that found in the \code{.gbm} method.
}
\value{
NULL, invisibly.
}
\author{
James Lawrence
}
\note{
Error messages from \code{read.csv} are suppressed, so you might like to check your file is readable before using this function. Other errors (such as in \code{predict}) will report normally. \cr


}
\seealso{
\code{\link{predict}}, \code{\link{predict.EMBglm}} 
}
\examples{
## simulate some data
df1 <- data.frame(x1=factor(sample(letters,1000,replace=TRUE)),x2=factor(sample(letters,1000,replace=TRUE)),x3=rnorm(1000))
df1$y <- with(df1,rnorm(1000) + as.integer(x1)/26 + (as.integer(x2)-as.integer(x1))/2000 + x3^2)
## fit a GBM and a GLM
require(gbm)
gbm1 <- gbm(y ~ ., data=df1, n.trees=500L,shrinkage=0.01,interaction.depth=4L,verbose=TRUE)
glm1 <- glm(y ~ ., data=df1)
## write the data to a file
write.csv(df1,file="testCSV.csv",row.names=FALSE)
## now score the GBM and GLM
## big chunk (1000 rows)
scoreAgainstCSV(gbm1,fin="testCSV.csv",fout="testCSV2.csv",n.trees=500L)
## smaller chunks (10 rows)
scoreAgainstCSV(gbm1,fin="testCSV.csv",fout="testCSV3.csv",chunkSize=10L,n.trees=500L)
scoreAgainstCSV(glm1,fin="testCSV.csv",fout="testCSV4.csv",chunkSize=10L)
## check the results are the same regardless of chunk size
identical(read.csv("testCSV2.csv"),read.csv("testCSV3.csv"))
## clean up
unlink("testCSV.csv")
unlink("testCSV2.csv")
unlink("testCSV3.csv")
unlink("testCSV4.csv")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

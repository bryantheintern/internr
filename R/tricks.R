#'Make a Big File out of a bunch of tiny ones.
#'Take a bunch of files of the same format(CSV) from one folder and combine (rbind) them
#' into one dataframe.
#' @param location Select folder location where the files are. Make sure those files are the only ones in the folder
#' header Do the files have a header? Defaulted to TRUE
#' @keywords files
#' @export
#' @examples
#' bigfile()
#' 


bigfile <- function(location,header=TRUE){
s<-list.files(path=location,pattern="csv",full.names=TRUE)

t<-lapply(s,read.csv)

u<-do.call(rbind,t)
}

#'Generate Pairs plot, except instead of giving a scatterplot above and below the main diagonal,
#'calculate the correlation for each pair and display the correlation either above or below
#'the main diagonal.
#'@param x dataframe containing variables you want to compare
#'upper logical defaulted to FALSE. If TRUE, the correlations will be shown in the upper half instead of the lower
#'@keywords plot
#'@export
#'@examples
#'coolpairs()

coolpairs <- function(x,upper=FALSE){
	panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
	{
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(0, 1, 0, 1))
		r = (cor(x, y))
		txt <- format(c(r, 0.123456789), digits=digits)[1]
		txt <- paste(prefix, txt, sep="")
		if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
		text(0.5, 0.5, txt, cex = cex * abs(r))
	}
if(upper == FALSE) pairs(x, lower.panel=panel.cor)
else pairs(x,upper.panel=panel.cor)
}


#'Not In
#'
#'This function serves the opposite function of %in% since R can't figure out !%in%
#'Found on Stack Overflow. Has the same syntax as %in% (See ?match)
#'@param x the thing you want to filter out
#'table where you want to filter it out of
#'@keywords filter
#'@export
#'@examples
#'%not in%


'%not in%' <- function (x,table) is.na(match(x,table,nomatch=NA_integer_))




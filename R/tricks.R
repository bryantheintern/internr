#'Directory
#'@keywords directory
#'@export
#'@examples
#'directory()
#'

directory<- function(){
	cat("
			big.csv:  Read in a folder of .CSVs and
			rbind them into one big data frame.
			---------------------------------------
			cor.pairs:  Make a pairs plot but with
			the corresponding correlations shown.
			---------------------------------------
			%not in%:  Opposite of %in%. Test if
			something is in something else or not.
			---------------------------------------
			mode:  Find the number of the highest
			frequency.
			---------------------------------------
			big.xlsx:  Same as big.csv but for .xlsx
			files. Also gives option to write
			resulting data frame into a .CSV.
			---------------------------------------
			noNA:  Remove NAs from a data frame,
			either by replacing them with '0's or
			by deleting rows.
			---------------------------------------
			what:  List of code chunks that couldn't
			easily be converted into one function or
			were already one function.
			---------------------------------------
			keyboard:  RStudio Keyboard shortcuts
			that may be useful.
	")
}

#'Make a Big File out of a bunch of tiny ones.
#'Take a bunch of files of the same format(CSV) from one folder and combine (rbind) them
#' into one dataframe.
#' @param location Select folder location where the files are. Make sure those files are the only ones in the folder
#' @param header Do the files have a header? Defaulted to TRUE
#' @param stringsAsFactors Do you want the strings converted to factors? Defaulted to FALSE
#' @keywords files
#' @export
#' @examples
#' big.csv("S:\\Key Retailing\\Active Projects and Teams\\Order Evolution\\Intern\\Bryan\\Movement Data")
#'


big.csv <- function(location,header=TRUE,stringsAsFactors=FALSE){
	s<-list.files(path=location,pattern="csv",full.names=TRUE)
	if (stringsAsFactors==FALSE){
		t<-lapply(s,read.csv,stringsAsFactors=FALSE)
	}
	else{lapply(s,read.csv,stringsAsFactors=TRUE)}
	u<-do.call(rbind,t)
}

#'Generate Pairs plot, except instead of giving a scatterplot above and below the main diagonal,
#'calculate the correlation for each pair and display the correlation either above or below
#'the main diagonal.
#'@param x dataframe containing variables you want to compare
#'@param upper logical defaulted to FALSE. If TRUE, the correlations will be shown in the upper half instead of the lower
#'@keywords plot
#'@export
#'@examples
#'coolpairs(mtcars[1:4])

cor.pairs <- function(x,upper=FALSE){
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
#'@param table where you want to filter it out of
#'@keywords filter
#'@export
#'@examples
#'5 %not in% c(1,2,3,4,5)
#'5 %not in% c(1,2,3,4)


'%not in%' <- function (x,table) is.na(match(x,table,nomatch=NA_integer_))


#'Mode function: find the mode of a set, functions just like mean or sum functions
#'@param x the list of things you want to find the mode of.
#'@keywords stats
#'@export
#'@examples
#'
#'mode(c(1,4,2,5,7,6,45,8,9,8,6,5,4,6,67,7,4,2,4,65,6))

mode <- function(x){
	as.numeric(names(sort(-table(x))) [1])
}


#'Read in a bunch of .xlsx files and spit out a csv
#'@param inpath Where is the folder you want to pull the files from? Make sure they have the same columns and there is nothing extra in the folder
#'@param out Should the function write out a csv when finished?
#'@param outpath If out is TRUE, then where should it be written?
#'@keywords xlsx
#'@export
#'@examples
#'
#'big.xlsx()

big.xlsx <- function(inpath, out=TRUE, outpath) {
	require(openxlsx)

	y<-list.files(inpath,pattern="xlsx",full.names=TRUE)
	z<-lapply(y,read.xlsx)

	if(out==TRUE){
		v<-do.call(rbind,z)
		write.csv(v,outpath)
	}
	else{v<- do.call(rbind,z)
			 v}
}



#' Eliminate NA's
#' @param df The data frame you want to take the NAs out of.
#' @param zero Do you want to replace the NAs with '0's? Default set to TRUE. If FALSE, then any row containing an NA will be completely removed.
#' @keywords NA
#' @export
#' @examples
#'
#' noNA(c(1,2,NA,4,5))
#' noNA(c(1,2,NA,4,5),zero=FALSE)

noNA  <- function(df,zero=TRUE){
	require(dplyr)
	if(zero==TRUE){ #Replace all NAs with 0
		df[is.na(df)] <- 0

	}
	else{          #Delete any line with an NA
		df <- df %>%
			na.omit()
	}
	df
}



#'Reminders of useful code that couldn't easily be written into functions or were already only one function.
#'@keywords dictionary
#'@param None
#'@export
#'@examples
#'
#'what()

what  <- function() {
	cat("
			Normality Plot: If line is relatively straight, then the data is normal.

			qqplot(rstudent(model))\n
			------------------------------------------------
			Time taken: Insert at beginning and end of code.

			start.time <- Sys.time()
			##code
			end.time <- Sys.time()
			time.taken <- end.time - start.time\n
			------------------------------------------------
			Left and Right Excel Equivalents:

			L <- str_sub(MyData$V4,1,4)
			R <- str_sub(MyData$V4,-4,-1)\n
			------------------------------------------------
			Convert dates into the standard R Date format:

			The format argument should reflect the *starting format* of the date.
			Date <- as.Date(Date,format=\"%m/%d/%Y\")\n
			------------------------------------------------
			Save and Load RDS:

			library(dplyr)
			## Save the object
			saveRDS(m3, \"Sample Merge Data.rds\")
			## Load the object
			v2 <- readRDS(\"BOHAdj.rds\")
			------------------------------------------------
			Concatenate columns to create Unique ID:

			library(stringr)
			df$Unique.ID <- with(df,(str_c(Date, Store, Catalog, Base.GTIN.Number, character(0))))

			------------------------------------------------
			Convert to data.frame

			df.analysis <- as.data.frame(df.analysis)
			"
	)
}


#'Useful RStudio keyboard shortcuts
#'@keywords dictionary
#'@param None
#'@export
#'@examples
#'
#'keyboard()


keyboard <- function() {
	cat("
Move Cursor to Script: CTRL + 1
Move Cursor to Console: CTRL + 2
Fold current selection to make room: ALT + L
Fix messy indents in your code: CTRL + I
Move selection (or current line): Alt+ Up/Down
Use selection for find: Select something, CTRL + F3
Get list of R's guesses of how to finish a word: TAB
Insert \" <- \": ALT + -
Insert \"%>%\": CTRL + SHIFT + M
Define user function where the cursor is: CTRL + ALT + F
			")
}

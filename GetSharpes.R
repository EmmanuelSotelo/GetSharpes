# Copyright Emmanuel Sotelo 2013
# EmmanuelSotelo.NET

# Sample Output:
# > GetSharpes(symbol_list, startDate, endDate)
# Symbol Sharpe.Ratio
# 1     fb   0.05690839
# 2   aapl  -0.90624398
# 3   yhoo   1.63818210
# 4   msft   1.74597094
# 5     gs   0.88405811

# Input: A vector of of stock ticker symbols, a start date, an end date
# 		a boolean indicating whether to save results to a file, a filename (if saving to file)
#		Date Format: YYYY-MM-DD		
# 	Example: symbol_list <- c("fb","aapl","yhoo", "msft", "gs")
# 	Example: GetSharpes(symbol_list, 2012-01-01, 2012-12-31, TRUE, SharpRatios.csv)
GetSharpes <- function(symbols, startDate, endDate, save.data = FALSE, save.name = "SharpeRatioResults.csv"){

	# Split the date strings
	
	start_d <- strsplit(startDate, "-")
	start.year <- start_d[[1]][1]
	start.month <- as.numeric(start_d[[1]][2]) - 1 # Yahoo Finance uses a 0-based index for months. (e.g. January = 0) 
	start.date <- start_d[[1]][3]
    	
	end_d <- strsplit(endDate, "-")		
	end.year <- end_d[[1]][1]
	end.month <- as.numeric(end_d[[1]][2]) - 1 # Yahoo Finance uses a 0-based index for months. (e.g. January = 0) 
	end.date <- end_d[[1]][3]
	
	# If save.data == TRUE, create empty file to be appended with data
	if(save.data){
		row_names <- data.frame( Symbol = character(), Sharpe.Ratio = character() )
		write.table( row_names, append = FALSE, save.name, sep = ",", row.names =FALSE, col.names=TRUE)
	}
	
	
	# Iterate through symbols list and store the computed Sharpe Ratio in a data frame
	results <- data.frame(Symbol = c(), Sharpe.Ratio = c() )
	for(sym in symbols){
     
		url <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
					sym,"&a=",start.month,"&b=",start.date,
					"&c=",start.year,"&d=",end.month,"&e=",end.date,"&f=",end.year,
					"&g=d&ignore=.csv", sep="")    
    
		# If an error is encountered when fetching symbol data, move on to next symbol
		if(
		inherits(
			try(data <-  read.csv(url), silent=TRUE ),
			"try-error"
		)
		) next;	  
    
		data <- OrderByDate(data)
    
		dailyReturns <- DailyReturns(data$Adj.Close) # Get daily retuns for current ticker symbol   
		sharpeRatio <- SharpeRatio(dailyReturns) # Compute Sharp Ratio for current ticker symbol
     
		
		data_row <- data.frame( Symbol = c(sym), Sharpe.Ratio = c(sharpeRatio) )
		
		# If save.data == TRUE, write data to file
		if(save.data){
			# Appends computed Sharpe Ratio to a CSV format file
			write.table( data_row, append = TRUE, save.name, sep = ",", row.names =FALSE, col.names=FALSE)
		}
		
		results <- rbind(results, data_row )	
	}
	
	
	return(results)  
}

# Computes Daily Returns  / The size of the output vector is: (input-1) in length
# Input: A vector containing daily prices
DailyReturns <- function(prices){
  
	deltas <- diff(prices)  
	dailyReturns <- deltas / prices[1:length(prices)-1]
    
	return(dailyReturns)
}

#Computes Sharpe Ratios
# Input: A vector containing returns, period for ShapeRatio being computed
SharpeRatio <- function(dailyReturns){
	
	period <- length(dailyReturns)+1     
	sharpeRatio <- (mean(dailyReturns)/ sd(dailyReturns)) * sqrt(period)
	
	return(sharpeRatio)
  
}

# Orders data frame by "Date" column in Ascending order
# Input: A data frame containing a column named "Date"
OrderByDate <- function(dataframe){  
 
	return(dataframe[order(dataframe$Date) ,])
}
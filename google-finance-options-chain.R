library(RCurl)
library(jsonlite)
library(plyr)

# Initial version of this code based on http://mktstk.wordpress.com/2014/12/29/start-trading-like-a-quant-download-option-chains-from-google-finance-in-r/

# A more direct method to fix the JSON data (making sure that all the keys are quoted). This will be a lot faster
# for large JSON packages.
#
fixJSON <- function(json){
        gsub('([^,{:]+):', '"\\1":', json)
}

# URL templates
#
URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
URL2 = 'http://www.google.com/finance/option_chain?q=%s&output=json&expy=%d&expm=%d&expd=%d'

getOptionQuotes <- function(symbol){
        url = sprintf(URL1, symbol)
        #
        #This creates a list of 6 items. 2 of the items are the data for the puts
        #and calls for ONLY the closest expiry.
        #But more importantly one item in the list is a list of all the expiries
        chain = fromJSON(fixJSON(getURL(url)))
        #
        # Iterate over the expiry dates
        #
        #chain$expirations is a list of the the expiration dates in y,m,d format
        #it is the dropdown that you see where you can choose which expiration you want to look at
        #the options is the data frame with all of the options and the expiry/prices continually get
        #written over each time through the loop
#!!!!!  #note that mlply returns a list. So options is equal to a list where each element
        #in the list is a data frame corresponding to a particular expiry. Right now I have it running
        #on only the first two expiries
        options = mlply(chain$expirations[1:2,], function(y, m, d) {
                url = sprintf(URL2, symbol, y, m, d)
                #download all the data for that expiry. This returns a list
                expiry = fromJSON(fixJSON(getURL(url)))
                #
                #creates two new columns within calls and puts which allows for subsetting later
                expiry$calls$type = "Call"
                expiry$puts$type  = "Put"
                #format the bid and ask as numeric
                expiry$calls$b <- as.numeric(expiry$calls$b)
                expiry$calls$a <- as.numeric(expiry$calls$a)
                expiry$puts$b <- as.numeric(expiry$puts$b)
                expiry$puts$a <- as.numeric(expiry$puts$a)
                #convert all NAs to 0
                #expiry$calls$b[is.na(expiry$calls$b)] <- 0
                #expiry$calls$a[is.na(expiry$calls$a)] <- 0
                #expiry$puts$b[is.na(expiry$puts$b)] <- 0
                #expiry$puts$a[is.na(expiry$puts$a)] <- 0
                #create a column to track the the midpoint of the bid/ask
                expiry$calls$call.mid = (expiry$calls[,"b"] + expiry$calls[,"a"]) / 2
                expiry$puts$put.mid = (expiry$puts[,"b"] + expiry$puts[,"a"]) / 2
                #
                #binds it all into one data frame
                # This is what the original author had:
                #prices = rbind(expiry$calls, expiry$puts)
                optionchain <- cbind(expiry$calls[,c("expiry","strike", "type", "b", "a","call.mid")], 
                                     expiry$puts[,c("type", "b", "a", "put.mid")])
                colnames(optionchain)[c(4,5,8,9)] <- c("call.bid", "call.ask", "put.bid", "put.ask")
                #
                #changes the format of the expiry column (i believe for subsetting)
                optionchain$expiry = sprintf("%4d-%02d-%02d", y, m, d)
                #creates another column to record the underlying price
                optionchain$underlying.price = expiry$underlying_price
                #
                optionchain
                #print(optionchain)
        })
        #
        # Concatenate data for all expiration dates and add in symbol column
        #
        #print(options)
#!!!!   #rbind.fill binds a list of data frames by row, which puts it all into a big table, although
        #I actually don't think I want to do this, since I will end up needind to subset the data by
        #expiry anyway I can instead just loop through each element in the list
        options = cbind(data.frame(symbol), rbind.fill(options))
        #
        #convert to POSIX
        options$expiry <- as.POSIXlt(options$expiry, format = "%Y-%m-%d")
        options[, "strike"] = as.numeric(options[, "strike"])
        #OLD#options[, "oi"] = suppressWarnings(as.integer(options[, "oi"]))
        #
        #OLD#names(options)[c(6, 10, 11, 12)] = c("price", "bid", "ask", "open.interest")
        #
        #OLD#options[, c(1, 16, 15, 6, 10, 11, 17, 14, 12)]
        options
}

#Calculating T
#assume the code is being run on end of day prices
#mCurrent will be 8*60 since it will  be 8 hours from 4 p.m. to midnight
#mSettlement will be 510 since that is always the minutes from midnight til 8:30 on settlement
#mOther will be settlementDay - currentDay - 1 (the minus one since we already have the minutes until midnight and until settlement)

#note that when I am ready to start computing the VIX this is how to find the strike with
#the min difference between bid/ask.
#Fmin <- with(near, strike[abs(call.mid - put.mid) == min(abs(call.mid - put.mid), na.rm = TRUE)])
#Fmin <- Fmin[!is.na(Fmin)]
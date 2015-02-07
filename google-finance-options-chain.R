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
        options = mlply(chain$expirations, function(y, m, d) {
                url = sprintf(URL2, symbol, y, m, d)
                #download all the data for that expiry. This returns a list
                expiry = fromJSON(fixJSON(getURL(url)))
                #
                #creates two new columns within calls and puts which allows for subsetting later
                expiry$calls$type = "Call"
                expiry$puts$type  = "Put"
                #
                #binds it all into one data frame
                prices = rbind(expiry$calls, expiry$puts)
                #
                #changes the format of the expiry column (i believe for subsetting)
                prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
                #creates another column to record the underlying price
                prices$underlying.price = expiry$underlying_price
                #
                prices
        })
        #
        # Concatenate data for all expiration dates and add in symbol column
        #
        options = cbind(data.frame(symbol), rbind.fill(options))
        #
        options[, "strike"] = as.numeric(options[, "strike"])
        options[, "oi"] = suppressWarnings(as.integer(options[, "oi"]))
        #
        names(options)[c(6, 10, 11, 12)] = c("price", "bid", "ask", "open.interest")
        #
        options[, c(1, 16, 15, 6, 10, 11, 17, 14, 12)]
}
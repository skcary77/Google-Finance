#I will have the loop here and say for i in the list run calcVix on each piece.

calculateVIX <- function(chain){
        
        calcT <- function(expiry){
                #minutes from 4 PM to midnight
                mCurrent <- 480
                #minutes from midnight until 8:30 AM of settlement dat
                mSettlement <- 510
                #from midnight of today until midnight of settlement day
                today <- as.POSIXlt(Sys.Date())
                mOtherDays <- (as.numeric(floor(expiry - today)) - 1) * 1440
                mT <- (mCurrent + mSettlement + mOtherDays) / 525,600
                mT
        }
        
        Tx <- calcT(chain$expiry[1])
        
        Rf <- .001
        
        calcF <- function(){
                Fstrike <- with(chain, chain[abs(call.mid - put.mid) == min(abs(call.mid - put.mid), na.rm = TRUE),
                                             c("strike", "call.mid", "put.mid")])
                Fstrike <- Fstrike[complete.cases(Fstrike),]
                Fx <- Fstrike$strike + exp(Rf*Tx) * (Fstrike$call.mid - Fstrike$put.mid)
                Fx
        }
        
        Fx <- calcF()
        
        calcK <- function(Fx){
                #substract each strike from Fx and the strike we want is the smallest positive number
                #create a vector of Fx-strike, then subset [>0], then return the min of that
                #will also probably want to know what row that is in
                #http://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
        }
        
        
        #first create a data frame of only the options we are keeping, then loop over that
        #loop through all expiries calculating the value we need
}
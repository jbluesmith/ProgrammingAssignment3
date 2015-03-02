best<- function(state, outcome) {
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      states <- sapply(data[,7], as.character)
      if (!state%in%states) { 
            stop("invalid state")
            }
      if (outcome=="heart attack") {
            outcomes <- sapply(data[,11], as.numeric)
            by_state <- subset(data, State == state, select = c(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
            minimum <- as.character(by_state$Hospital.Name[which.min(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
            return(minimum) 
      }
      if (outcome=="heart failure") {
            outcomes <- sapply(data[,17], as.numeric)
            by_state <- subset(data, State == state, select = c(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
            minimum <- as.character(by_state$Hospital.Name[which.min(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)])
            return(minimum) 
      }
      if (outcome=="pneumonia") {
            outcomes <- sapply(data[,23], as.numeric)
            by_state <- subset(data, State == state, select = c(Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
            minimum <- as.character(by_state$Hospital.Name[which.min(by_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
            return(minimum) 
      }
      else  {
            stop("invalid outcome")
      }
}
     





### column 11: 30-day mortality rate for heart attack
### column 17: 30-day mortality rate for heart failure
### column 23: 30-day mortality rate for pneumonia
### column 2:  hospital name


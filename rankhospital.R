rankhospital<- function(state, outcome, num = "best") {
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      states <- sapply(data[,7], as.character)
      if(num=="best") {
            num<-1
      }
      if (!state%in%states) { 
            stop("invalid state")
            }
      if (outcome=="heart attack") {
            data[,11]<-as.numeric(data[,11])
            by_state <- subset(data, State == state, select = c(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
            rank <- by_state[order(by_state[,3],by_state[,2], na.last=NA),]
            if(num=="worst") {
                  last<-(tail(rank,1))
                  return(last[,2])
            }
            return(rank[num,2])
      }
      if (outcome=="heart failure") {
            data[,17]<-as.numeric(data[,17])
            by_state <- subset(data, State == state, select = c(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
            rank <- by_state[order(by_state[,3],by_state[,2], na.last=NA),]
            if(num=="worst") {
                  last<-(tail(rank,1))
                  return(last[,2])
            }
            return(rank[num,2])
      }
      if (outcome=="pneumonia") {
            data[,23]<-as.numeric(data[,23])
            by_state <- subset(data, State == state, select = c(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
            rank <- by_state[order(by_state[,3],by_state[,2], na.last=NA),]
            if(num=="worst") {
                  last<-(tail(rank,1))
                  return(last[,2])
            }
            return(rank[num,2])
      }
      else  {
            stop("invalid outcome")
      }
}
best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                      colClasses = "character")
  statename<-unique(outcome1$State)
  outname<-c("heart attack","heart failure","pneumonia")
  if(!sum(statename==state)){stop("invalid state")}
  if(!sum(outname==outcome)){stop("invalid outcome")}
 outcome1[,11]<- as.numeric(outcome1[, 11])
 outcome1[, 17]<- as.numeric(outcome1[, 17])
 outcome1[,23]<-as.numeric(outcome1[, 23])
 names<-c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="heart attack",
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="heart failure",
          Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="pneumonia")
 outcome<-names(names)[names==outcome]
  sdata<-outcome1[outcome1$State==state,]
  sdata<-sdata[order(sdata[outcome],sdata["Hospital.Name" ]),]
  hname<-sdata[1,2]
 return(hname)
  
}
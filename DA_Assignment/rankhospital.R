rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
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
  HOname<-"Hospital.Name"
  sdata2<-na.omit(subset(sdata,select=c(outcome,HOname)))
  
  if(num=="best")
 { hname<-sdata[1,2]}
 else if(num=="worst")
 {
   sdata2<-tail(sdata2,n=1)
 hname<-sdata2[,2]}
 else if(is.numeric(num))
 {
   if(num>length(sdata2[,1]))
        return (NA)
   else
     hname<-sdata2[num,2]
     
 }
  return(hname)
}
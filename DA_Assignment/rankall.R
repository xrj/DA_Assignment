rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  statename<-unique(outcome1$State)
  
  outname<-c("heart attack","heart failure","pneumonia")
  if(!sum(outname==outcome)){stop("invalid outcome")}
  outcome1[,11]<- as.numeric(outcome1[, 11])
  outcome1[, 17]<- as.numeric(outcome1[, 17])
  outcome1[,23]<-as.numeric(outcome1[, 23])
  names<-c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="heart attack",
           Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="heart failure",
           Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="pneumonia")
  outcome<-names(names)[names==outcome]
  HOname<-"Hospital.Name"
  STname<-"State"
  rankdata<-data.frame(hospital=NULL,state=NULL)
  sdata<-subset(outcome1,select=c(outcome,HOname,STname))
  for(i in 1:54)
  {
    state<-statename[i]
    st<-subset(sdata,State==state)
    st<-st[order(st[outcome],st["Hospital.Name"]),]
    st<-na.omit(st)
    st<-subset(st,select=c(HOname,STname))
    names(st)<-c("hospital","state")
    if(num=="best")
    {  data<-head(st,n=1)
       rankdata<-rbind(rankdata,data)
    }
    else if(num=="worst")
    {
      data<-tail(st,n=1)
      rankdata<-rbind(rankdata,data)
    }
    else if(is.numeric(num))
    {
      if(num>length(st[,1]))
     {  data<-c(NA,state)
        rankdata<-rbind(rankdata,data)
      }
      else
      {
        data<-st[num,]
        rankdata<-rbind(rankdata,data)
    }
        
  }
    
  }
  rankdata<-rankdata[order(rankdata$state),]
  return(rankdata)
}
install.packages("data.table")
install.packages("fpc")
install.packages("klaR")
install.packages("zoo")

library("data.table")
library("fpc")
library("ggplot2")
library("klaR")
library("zoo")


tus_wide <- as.data.table(read.csv("uktus15_diary_wide.csv",stringsAsFactors = F))
tus_ind <- as.data.table(read.csv("uktus15_individual.csv",stringsAsFactors = F))
tus_house <- as.data.table(read.csv("uktus15_household.csv",stringsAsFactors = F))
tus_house <- tus_house[strata>=0,]

Mydata<-tus_wide[,c(1:7,9,11,17,19,32:175,608:751)]
# 1:19 Household number / person number / diary number / Household ionterview outcome ? Month / Dayofweek 
# 32:175 Act1_1 to Act1_144
# 608:751 Wher_1 to wher_144
Mydata<-merge(Mydata,tus_ind[,c(1,4,240,588,598)],by=c("serial","pnum"))
# Merge by household and person number
# 240: Full time / part time job
# 588 Houshold type
# 598 Employment status
Mydata<-merge(Mydata,tus_house[,c(1,19)],by=c("serial"))
# Merge by household number
# Household size

# Only productive interviews
Mydata<-Mydata[HhOut==110,] 

#----------CREATE STATE COLUMNS 1 TO 144-------------------------------------------------------------------------------

# Wher == 11 AT HOME
# Activity == 110 SLEEPING

#STATES
# wher ==11 & Act ==110 --> 0 sleeping
# wher ==11 & Act !=110 --> 1 At home & active
# wher !=11  --> 2 Away

define.state<-function(x,y)
{
  ifelse(y==11,ifelse(x==110,0,1),2)
}

for (i in 1:144)
{ act<-paste("act1_",i,sep="")
  wher<-paste("wher_",i,sep="")
  new_col<-as.data.table(mapply(define.state,Mydata[,get(act)],Mydata[,get(wher)]))
  colnames(new_col)<-paste("state_",i,sep="")  
  Mydata<-cbind(Mydata,new_col)  
} 





#############
############
#############




#------------Create Individual work status-----------------------------------------------------------------------------

# Assign 1 where person is over 16, not student and works full time
# Assign 2 when person is over 16, not student and not working
#Assign 0 for all other cases
Mydata<-Mydata[ ,Ind_work:=ifelse(DVAge>=16 & 
                                  (deconact==1| deconact==2 | deconact==3 | deconact==5) &
                                  FtPtWk==1,
                                  1, ifelse(DVAge>=16 & 
                                          (deconact==6| deconact==7 | deconact==9 | deconact==10 | deconact==11),
                                          2,0))]  



#------Assign household profile -----------------------------------------------------------------------------------------

# Couple, No children, over 60 / None working

# Couple with dep and indep children / None working

# One person over / None working

#-----------

H_wkd_1<-Mydata[dhhtype==1 & ddayw==1,]
H_wkd_1[,12:299]<-NULL
#size<-nrow(H_wkd_1)

st_time <-as.POSIXct("04:00:00",format="%H:%M:%S")
end_time <-st_time+as.difftime("23:50:00",format="%H:%M:%S",units="hour")
ts_prob<-as.data.table(seq( from=st_time, to=end_time,by="10 mins"))  
#ts_prob<-strftime(ts_prob[,],format="%H:%M:%S") 

H_wkd_1_prob<-cbind(ts_prob,t(H_wkd_1[,c(16:159)]))
count1<-apply(H_wkd_1_prob,1, function(x) length(which(x==1)))














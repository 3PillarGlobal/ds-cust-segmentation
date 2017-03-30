###############################################################################
# Description: A set of R functions to implement the Independent RFM scoring and the RFM scoring with input breaks.
# Author: Vikas Kukreti
#####################################################################

#################################################################################
getDataFrame <- function(df,startDate,endDate,tIDColName="CustomerID",tDateColName="InvoiceDate",tAmountColName="UnitPrice"){
  
  #order the dataframe by date descendingly
  df <- df[order(df[,tDateColName],decreasing = TRUE),]
  
  #remove the record before the start data and after the end Date
  df <- df[df[,tDateColName]>= startDate,]
  df <- df[df[,tDateColName]<= endDate,]
  
  #remove the rows with the duplicated IDs, and assign the df to a new df.
  newdf <- df[!duplicated(df[,tIDColName]),]
  
  # caculate the Recency(days) to the endDate, the smaller days value means more recent
  Recency<-as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
  
  # add the Days column to the newdf data frame
  newdf <-cbind(newdf,Recency)
  
  #order the dataframe by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[,tIDColName]),]
  
  # caculate the frequency
  fre <- as.data.frame(table(df[,tIDColName]))
  Frequency <- fre[,2]
  newdf <- cbind(newdf,Frequency)
  
  #caculate the Money per deal
  m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
  Monetary <- m[,1]/Frequency
  newdf <- cbind(newdf,Monetary)
  
  return(newdf)
  
} # end of function getDataFrame


################################################################################




getIndependentScore <- function(df,r=5,f=5,m=5) {
  
  if (r<=0 || f<=0 || m<=0) return
  
  #order and the score
  df <- df[order(df$Recency,-df$Frequency,-df$Monetary),]
  R_Score <- scoring(df,"Recency",r)
  df <- cbind(df, R_Score)
  
  df <- df[order(-df$Frequency,df$Recency,-df$Monetary),]
  F_Score <- scoring(df,"Frequency",f)
  df <- cbind(df, F_Score)
  
  df <- df[order(-df$Monetary,df$Recency,-df$Frequency),]
  M_Score <- scoring(df,"Monetary",m)
  df <- cbind(df, M_Score)
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return (df)
  
} # end of function getIndependentScore

################################################################################

################################################################################
# Function
#   scoring(df,column,r=5)
#
# Description
#	A function to be invoked by the getIndepandentScore function
#######################################
scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
} #end of function Scoring

################################################################################
# Function
#   getScoreWithBreaks(df,r,f,m)
#
# Description
#	Scoring the Recency, Frequency, and Monetary in r, f, and m which are vector object containing a series of breaks
#
# Arguments
#	df - A data frame returned by the function of getDataFrame
#	r -  A vector of Recency breaks
#	f -  A vector of Frequency breaks
#	m -  A vector of Monetary breaks
#
# Return Value
#	Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
#
#################################################################################

getScoreWithBreaks <- function(df,r,f,m) {
  
  ## scoring the Recency
  len = length(r)
  R_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(df[p1<df$Recency & df$Recency<=p2,])[1]>0) df[p1<df$Recency & df$Recency<=p2,]$R_Score = len - i+ 2
  }
  
  ## scoring the Frequency	
  len = length(f)
  F_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(df[p1<df$Frequency & df$Frequency<=p2,])[1]>0) df[p1<df$Frequency & df$Frequency<=p2,]$F_Score = i
  }
  if(dim(df[f[len]<df$Frequency,])[1]>0) df[f[len]<df$Frequency,]$F_Score = len+1
  
  ## scoring the Monetary	
  len = length(m)
  M_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(df[p1<df$Monetary & df$Monetary<=p2,])[1]>0) df[p1<df$Monetary & df$Monetary<=p2,]$M_Score = i
  }
  if(dim(df[m[len]<df$Monetary,])[1]>0) df[m[len]<df$Monetary,]$M_Score = len+1
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
  
} # end of function of getScoreWithBreaks


################################################################################
# Function
# 	drawHistograms(df,r,f,m)
#
# Description
#	Draw the histograms in the R, F, and M dimensions so that we can see the quantity of customers in each RFM block.
#
# Arguments
#	df - A data frame returned by the function of getIndependent or getScoreWithBreaks
#	r -  The highest point of Recency
#	f -  The highest point of Frequency
#	m -  The highest point of Monetary
#
# Return Value
#	No return value.
#
#################################################################################
drawHistograms <- function(df,r=5,f=5,m=5){
  
  #set the layout plot window
  par(mfrow = c(f,r))
  
  names <-rep("",times=m)
  for(i in 1:m) names[i]<-paste("M",i)
  
  
  for (i in 1:f){
    for (j in 1:r){
      c <- rep(0,times=m)
      for(k in 1:m){
        tmpdf <-df[df$R_Score==j & df$F_Score==i & df$M_Score==k,]
        c[k]<- dim(tmpdf)[1]
        
      }
      if (i==1 & j==1) 
        barplot(c,col="lightblue",names.arg=names)
      else
        barplot(c,col="lightblue")
      if (j==1) title(ylab=paste("F",i))	
      if (i==1) title(main=paste("R",j))	
      
    }
    
  }
  
  par(mfrow = c(1,1))
  
} # end of drawHistograms function

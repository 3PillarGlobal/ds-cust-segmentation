library(ggplot2, lib.loc="/home/vikask/r/pack/")
# =========================== Manipulating Original Data-Frame =======================================
rm(df)
head(data)
data[,5] <- as.Date(as.POSIXct( data[,5], origin = "1582-10-14", format="%m/%d/%Y %H:%M"))

drops <- c("StockCode","Description","Quantity","Country")
df<-data[ , !(names(data) %in% drops)]


head(df)

# =========================== Manipulating Original Data-Frame ends here======================================= 


#============================remove the rows with the duplicated IDs to see how many customers in total====================

uid <- df[!duplicated(df[,"CustomerID"]),]
dim(uid)

#============================remove the rows with the duplicated IDs to see how many customers in total ends here ====================


# ============================set the startDate and endDate, we will only analysis the records in this date range ====================

startDate <- as.Date("20110101","%Y%m%d")

endDate <- as.Date("20111231","%Y%m%d")



df <- getDataFrame(df,startDate,endDate)

drops <- c("Recency")
df0<-df[ , !(names(df) %in% drops)]

head(df0)

head(df)

# ============================set the startDate and endDate, we will only analysis the records in this date range ====================


# ============================ getIndependentScore ===================================================================
rm(df1)
df1 <-getIndependentScore(df)

head(df1[-(2:3)])


drops <- c("Recency","R_Score","Total_Score")
df00<-df1[ , !(names(df1) %in% drops)]

head(df00)

head(df1)

# ============================ getIndependentScore ends here===================================================================

#==============================Setting Variables ==============================================================================

# set the Recency ranges as 0-120 days, 120-240 days, 240-450 days, 450-500days, and more than 500days.
r <-c(120,240,450,500)

# set the Frequency ranges as 0 – 2times, 2-5 times,5-8 times, 8-10 times, and more than 10 times.
f <-c(2,5,8,10)

# set the Monetary ranges as 0-10 dollars, 10-20 dollars, and so on.

m <-c(10,20,30,100)

#==============================Setting Variables ends here==============================================================================

#==============================getScoreWithBreaks==============================================================================
rm(df2)

df2<-getScoreWithBreaks(df,r,f,m)
drawHistograms(df2)
salesRFM <- df2[with(df2, order(-R_Score, -F_Score, -M_Score)), ]
head(salesRFM, n=10)

salesRFM1 <- df2[with(df2, order(-F_Score, -M_Score)), ]

#==============================getScoreWithBreaks ends here==============================================================================


#============================== Analysis==============================================================================
groupRFM <- count(salesRFM, R_Score, F_Score,M_Score)
groupRFM <- salesRFM$R_Score*100 + salesRFM$F_Score*10 + salesRFM$M_Score
salesRFM <- cbind(salesRFM,groupRFM)


groupRFM1 <- count(salesRFM1, F_Score,M_Score)
groupRFM1 <- salesRFM$F_Score*10 + salesRFM$M_Score
salesRFM1 <- cbind(salesRFM1,groupRFM1)
#============================== Analysis ends here==============================================================================



# ==========================================Analysis Plot =================================================================================
ggplot(salesRFM1, aes(factor(groupRFM1))) +
  geom_bar() +
  ggtitle('Customer Distribution by FM') +
  labs(x="RFM",y="# Customers") + 
  theme(plot.title = element_text(color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold"))
# ==========================================Analysis Plot ends here=================================================================================


# ==========================================Elbow Plot and kmeans Analysis=================================================================================


rm(df3)
drops <- c("Recency","R_Score","F_Score","M_Score","Total_Score")
df3<-df1[ , !(names(df1) %in% drops)]
head(df3)

rm(df4)
df4 <- data.frame("InvoiceNo"=df3$InvoiceNo,
                  "InvoiceDate"=df3$InvoiceDate,
                  "UnitPrice"=df3$UnitPrice,
                  "CustomerID"=df3$CustomerID,
                  "Frequency"=remove_outliers_func(df1$Frequency),
                  "Monetary"=remove_outliers_func(df1$Monetary) ) 

df4 <- na.omit(df4)




head(df4)

rm(mydata)

mydata <- df4[, 5:6]
head(mydata)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mydataNorm <- as.data.frame(lapply(mydata, normalize))

rm(wss)
k.max <-  15
wss <- (nrow(mydataNorm)-1)*sum(apply(mydataNorm,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydataNorm,
                                     centers=i,k.max )$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     lylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

rm(df4Cluster)
df4Cluster <- kmeans(scale(mydataNorm[, 1:2]),8, nstart = 3, iter.max=1000)

# nrow(df4$Frequency)
# nrow(df4$Monetary)

df4Cluster$cluster <- as.factor(df4Cluster$cluster)
df4Cluster$cluster1 <- df4Cluster$cluster
summary(df4Cluster)
head(df4)

df4Cluster$cluster
df4Cluster$centers
df4Cluster$totss
df4Cluster$withinss
df4Cluster$tot.withinss
df4Cluster$betweenss
df4Cluster$size
df4Cluster$iter
df4Cluster$ifault
df4Cluster$cluster1


ggplot(mydataNorm, aes(mydataNorm$Frequency, mydataNorm$Monetary, color = df4Cluster$cluster)) + 
  geom_point(alpha = 0.8, size = 1.5) + 
  ggtitle('Customer Distribution by FM') +
  labs(x="Frequency",y="Monetary") + 
  scale_color_manual(values = c('darkgreen','darkorchid','darkkhaki' ,'darkorange','firebrick','mediumslateblue','gold','deepskyblue','darkviolet','darkgrey'))
# ,'darkmagenta','darkolivegreen','darkorange' 'darkgrey','deepskyblue','firebrick','gold','darkviolet','mediumslateblue'


plot(mydataNorm, col =( df4Cluster$cluster) , main="K-Means result with 8 clusters", pch=20, cex=2)
points(df4Cluster$centers,col=1:8,pch=3)
d <- dist(as.matrix(mydataNorm)) 
hc <- hclust(d)   
plot(hc)  


df5 <- data.frame("InvoiceNo"=df4$InvoiceNo,
                  "CustomerID"=df4$CustomerID,
                  "Frequency"=remove_outliers_func(df4$Frequency),
                  "Monetary"=remove_outliers_func(df4$Monetary),
                  "Cluster"=df4Cluster$cluster1 ) 

df5 <- na.omit(df5)


df5 <- df5[order(df5$Cluster),] 

df6 <- df5[order(df5$Frequency, df5$Monetary),]

df7 <- df5[order(df5$Frequency),]

df8 <- df5[order(df5$Monetary),]

head(df5,2000)
head(df6,1000)
head(df7,1000)
head(df8,2000)

library(cluster)
library(fpc)
plotcluster(mydataNorm,df4Cluster$cluster)
points(df4Cluster$centers,col=1:8,pch=16)


clusplot(mydataNorm, df4Cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


tunek <- kmeansruns(mydataNorm,krange = 1:10,criterion = "ch")
tunek$bestk

tunekw <- kmeansruns(mydataNorm,krange = 1:10,criterion = "asw")
tunekw$bestk


# ==========================================Elbow Plot Analysis and kmeans ends here=================================================================================



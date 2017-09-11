install.packages("XLConnect")
install.packages("plyr")
install.packages("dmm")
install.packages("reshape2")
install.packages("PairedData")
install.packages("ggplot2")
install.packages("mosaic")
library(XLConnect)
library(plyr)
library(ggplot2)
library(mosaic)
library(PairedData)
library(reshape2)
library(dmm)

##
##
##
##DO NOT RUN WHOLE SCRIPT. WILL DESTROY VARIABLES. 
##NOT 100% CONTINUOUS AND WILL REDO OPERATIONS WITH WRONG TEMPORARY DATA
##
##
##Partially for method documentation
##


#Renaming Columns
names(AlboPCR)[names(AlboPCR)=="Titer"] <- "BTiter"
names(AlboPCR)
names(AlboPCR)[names(AlboPCR)=="Titer2"] <- "LTiter"
names(AlboPCR)

#Load Workbook
LHTWorkbook <- loadWorkbook("Life History Traits for R 7_28.xlsx")
#Load Worksheets
AlboPCR <- readWorksheet(LHTWorkbook,"Albopictus PCR", useCachedValues = TRUE)
AegPCR <- readWorksheet(LHTWorkbook,"Aegypti PCR", useCachedValues = TRUE)

AlboBlood <- readWorksheet(LHTWorkbook, "Albopictus Blood", useCachedValues = TRUE)
temp <- readWorksheet(LHTWorkbook, "Aegypti Blood", useCachedValues = TRUE)

###AlboMort
temp <- readWorksheet(LHTWorkbook, "Albopictus Mortality", useCachedValues = TRUE)
###AegMort
temp <- readWorksheet(LHTWorkbook, "Aegypti Mortality", useCachedValues = TRUE)

#AlboEggs
temp <- readWorksheet(LHTWorkbook, "Albopictus Eggs", useCachedValues = TRUE)
AegEggs <- readWorksheet(LHTWorkbook, "Aegypti Eggs", useCachedValues = TRUE)

AlboWings <- readWorksheet(LHTWorkbook, "Albopictus Wings", useCachedValues = TRUE)
AegWings <- readWorksheet (LHTWorkbook, "Aegypti Wings", useCachedValues = TRUE)


#Worksheet trimming
names(AlboPCR) <- AlboPCR[4,]
AlboPCR <- AlboPCR[5:104,1:11]

names(AegPCR) <- AegPCR[4,]
AegPCR <- AegPCR[5:124,1:10]

AlboBlood <- AlboBlood[3:103,4:21]
names(AlboBlood) <- AlboBlood[3,]
AlboBlood <- AlboBlood[4:103,4:21]

#Attempting to create a Blood Observation object for each mosquito and include array of
#BloodObvs in the Blood Data Frame
#
#xBlood.Obvs represents engorgement data and transfered into the xBlood data frame as list objects with their engorgement score corresponding 
#to the DPI of the list index


row.names(AlboBlood) <- NULL    #resets rownames to sequential values

AlboBlood.Obvs <- AlboBlood[1:100,2:11]  #store engorgement data

temp <- AlboBlood
temp <- temp[,-2:-11]   #remove engorgement data
temp$Eng <- AlboBlood.Obvs
AlboBlood <- temp
temp <- temp[,c(1,9,2:8)]

    #AlboBlood loaded into Temp
AlboBlood_C <- temp[136:175,4:21]
AlboBlood_C.Obvs <- AlboBlood_C[,2:11]
AlboBlood_C <- AlboBlood_C[,-2:-11]
AlboBlood_C$Eng <- AlboBlood_C.Obvs
AlboBlood_C <- AlboBlood_C[,c(1,9,2:8)]
AlboBlood_C <- AlboBlood_C[,-4:-5]

AegBlood <- AegBlood[3:123,4:17]
names(AegBlood) <- AegBlood[1,]
AegBlood <- AegBlood[-1,]
row.names(AegBlood) = NULL
AegBlood.Obvs <- AegBlood[1:120,2:7]
View(AegBlood.Obvs)
AegBlood$Eng <- AegBlood.Obvs
AegBlood <- AegBlood[,-2:-7]
temp <- AegBlood[,c(1,9,2:8)]
AegBlood <- temp

AegBlood_C.Obvs <- temp[,2:7]
temp <- temp[,-2:-7]
AegBlood_C <- temp
AegBlood_C$Eng <- AegBlood_C.Obvs
AegBlood_C <- AegBlood_C[,c(1,7,2:6)]


#
#Mortality
#

View(AlboMort)
AlboMort <- AlboMort[-4,] #Remove DPF Factor as it is not being considered yet
AlboMort <- AlboMort[3:17,3:42]
row.names(AlboMort)[1] <- "DPI"
names(AlboMort) <- AlboMort[1,]
names(AlboMort)[1] <- "DPI"
row.names(AlboMort) <- NULL

View(temp)
names(temp) <- temp[3,]
row.names(temp) <- NULL


AlboMort_C <- temp[56:72,3:42]
View(AlboMort_C)
names(AlboMort_C)[1] <- "DPI"



#AEG
View(AegMort)
temp <- AegMort[4:27,3:26]
AegMort <- temp
names(AegMort) <- AegMort[1,]
AegMort <- AegMort[-1,]
names(AegMort)[1] <- "DPI"
row.names(AegMort) <- NULL

View(temp)
names(temp) <- temp[4,]
AegMort_C <- temp[64:71,3:26]
View(AegMort_C)
names(AegMort_C)[1] <- "DPI"


#Eggs
#
#
AlboEggs.Obvs <- AlboEggs[1:100,2:20]
View(AlboEggs.Obvs)
temp <- AlboEggs

AlboFec <- AlboEggs           #AlboEggs would be confusing as AlboEggs$Egg
AlboFec$Egg <- AlboEggs.Obvs
AlboFec <- AlboFec[,-2:-20]
AlboFec <- AlboFec[,c(1,7,2:6)]

View(temp)
temp[151,4:22] <- temp[4,4:22]
names(temp) <- temp[151,]
AlboFec_C <- temp[152:191,3:25]
View(AlboFec_C)
names(AlboFec_C)[1] <- "Sample Name"
AlboFec_C.Obvs <- AlboFec_C[,2:20]
AlboFec_C <- AlboFec_C[,-2:-20]
AlboFec_C$Egg <- AlboFec_C.Obvs
AlboFec_C <- AlboFec_C[,c(1,5,2:4)]



AegFec <- AegEggs[5:126,3:23]
AegFec[1,17:18] <- AegFec[2,17:18]
AegFec[1,19:20] <- c("Positive","Disseminated")
AegFec[-2,]
names(AegFec) <- AegFec[1,]
AegEggs.Obvs <- AegFec[,2:16]
AegFec$Egg <- AegEggs.Obvs
AegFec <- AegFec[,-2:-16]
AegFec <- AegFec[,c(1,7,2:6)]

#Wings
#
#
names(AlboWings) <- AlboWings[2,]
AlboWings <- AlboWings[1:100,c(11,16)]

AlboWings_C <- AlboWings[1:40,c(2,7)]
AlboWings_C[40,1] <- 22   #Missing #22

names(AegWings) <- AegWings[2,]
AegWings_C <- AegWings[1:42,c(2,7)]
AegWings_C <- AegWings_C[-41:-42,]

AegWings <- AegWings[,c(11,16)]
AegWings[120,1] <- 29     #Missing #29


###
###PLOTTING EXPLORING AND WHATNOT
###

names(AlboPCR) <- AlboPCR[4,]
AlboPCR <- AlboPCR[5:104,1:11]
temp <- AlboPCRk

AlboPCR[,3:5] <- lapply(AlboPCR[,3:5], as.numeric)
AlboPCR$`DPI Survival` <- as.integer(AlboPCR$`DPI Survival`)
View(temp)


AlboPCR$BTiter <- as.numeric(AlboPCR$BTiter)

#Plot
aesx <- AlboPCR$DPISurv
aesy <- AlboPCR$Titer
AlboTiterAes <- aes(aesx,aesy)
AlboPCR_plot <- ggplot(AlboPCR, AlboTiterAes) +
  geom_point() +
  geom_smooth(col = 'lime green')
  
AlboPCR_plot + geom_smooth(method = 'loess', col = 'brown')

names(AlboPCR)[ncol(AlboPCR)] <- "DPISurv"
names(AlboPCR)

temp <- AlboPCR
nls_fit <- nls(I(AlboPCR$Titer)~I(a+b*log(AlboPCR$DPISurv)),AlboPCR, start = list(a = 4, b = 1)) #with all exposed group
summary(nls_fit)
cor(AlboPCR$Titer,predict(nls_fit))

AlbPCRDis <- AlboPCR[which(AlboPCR$Disseminated == "true"),]
View(AlbPCRDis)
nls_fit_D <- nls(AlbPCRDis$Titer[]~a+b*log(AlbPCRDis$DPISurv + c),AlbPCRDis, start = list(a = 4, b = 1, c = 4), nls.control(maxiter = 200, minFactor = I(10^(-10)))) #with only disseminated
summary(nls_fit_D)
cor(AlbPCRDis$Titer,predict(nls_fit_D))
ggplot(AlbPCRDis, aes(AlbPCRDis$DPISurv,predict(nls_fit_D))) + geom_point() + geom_smooth()

# #Replace N/A values of Titer with 0's
# for(i in c(1:60,81:120)){
#   if(is.na(AlboPCR[AlboPCR$`Sample Name`==i,"Titer"])){
#     AlboPCR[AlboPCR$`Sample Name`==i,"Titer"] <- 0
#   }
# }

DPI <- seq(0,40)
y <- function(x){
  1/exp(-x/40)
}

plotFun(y(DPI)~DPI,DPI.lim=range(0,40), add = FALSE )
plotFun()
       


###
#Blood Manipulation
###

DPIBlood <- as.integer(names(AlboBlood$Eng))
DPIBlood

t.test(AlboBlood$Eng[29,],AlboBlood$Eng[33,])
mean(AlboBlood$Eng[,1], na.rm = TRUE)
BloodAvgDaily <- vector(mode = "numeric", length = length(AlboBlood$Eng))
BloodAvgDaily <- colMeans(AlboBlood$Eng, na.rm = TRUE)
AlboBlood$Eng
BloodAvgDaily

##############
###
df = data.frame(x=c('a','b','c'), y=3:1)
df
ldf = lapply(as.list(1:nrow(AlboBlood.Obvs)), function(x) AlboBlood.Obvs[x[1],]) #AlboBlood.Obvs[x[1],] returns a reference to variable in list itself
ldf
###
###

names(ldf) #NULL, lists have no names?

ldf[2]
str(ldf[2])
names(ldf)
mode(ldf)
mode(ldf[[2]])
names(ldf[[2]])
ldf[[2]]$`3`
mean(unlist(ldf[[5]]), na.rm = TRUE)
mode(ldf[[2]])
mode(ldf[[2]]$`3`)
unlist(ldf[[5]])

ggplot(ldf[[2]], aes(x = DPIBlood, y = unlist(ldf[[2]][1,]))) + geom_point()

AlboBlood.Obvs <- cbind(AlboBlood$`Sample Number`,AlboBlood.Obvs)
names(AlboBlood.Obvs)[1] <- "Sample Names"
m.AlboBlood <- melt(AlboBlood.Obvs, id.vars = "Sample Names", na.rm = FALSE)
names(AlboBlood)
temp <- melt(as.matrix(AlboBlood$Eng), id.vars = Var1)
ggplot(temp, na.rm = FALSE, aes(Var2, value)) + geom_count()#boxplot(outlier.color = "red")

ggplot(temp, na.rm = FALSE, aes(Var2, value, group = Var2)) + geom_boxplot(outlier.color = "red")

#
#Stat Variable Creation
#

deathsum <- list(length = 23)
for(i in 1:23){
  deathsum[i] <- length(which(Aeg_Z$`Mort.DPI Death`==i))
}
deathsum
nDead <- deathsum

surviving <- list(length = 23)
for(i in 1:23){
  surviving[i] <- 120 - sum(unlist(deathsum[1:i]))#rolling_tot
}
surviving
nSur <- surviving

PDoS <- list(length =23)
PDoS[1] <- (deathsum[1]/120)*100
mode(i) <- "integer"
for(i in 2:23){
  j <- as.integer(i-1)
  PDoS[i] <- as.numeric(deathsum[i])/as.numeric(surviving[i-1])*100
}

pDoT <- lapply(nDead,function(x) (as.numeric(x)/120)*100)


#SubGroups
#
#
LHTWorkbook <- loadWorkbook("Life History Traits for R 7_28.xlsx")
temp <- readWorksheet(LHTWorkbook, "Albopictus Mortality", useCachedValues = TRUE)
View(temp)


AlbStat <- data.frame(DPI = seq.int(1,39))
tempFrame <- "Control"       #Working temporary data frame [Exp, Infected, Uninfected, InfNDis, InfDis, Control]

View(temp)               #Determine appropriate range for allocation
                             #Allocate
StatTemp <- temp[47:50,2:42] #Exp                             
StatTemp <- temp[27:30,2:42] #Uninfected                             
StatTemp <- temp[31:34,2:42] #Infected
StatTemp <- temp[35:38,2:42] #Infected Not Disseminated
StatTemp <- temp[39:42,2:42] #Infected and Disseminated
StatTemp <- temp[43:46,2:42] #Control
View(StatTemp)

StatTemp <- rbind(c("DPI","",seq.int(1,length(StatTemp))),StatTemp) #Add DPI labels to row to visually ensure correct data transfer
StatTemp <- StatTemp[,-2]                             #Subtract DPI 0 data
StatTemp <- t(StatTemp)                               #Transpose data such that each DPI is an "observation" (row)
StatTemp <- as.data.frame(StatTemp)                   #Convert to data frame
names(StatTemp) <- c("DPI","nDead","nSur","pDoS","pDoT")
StatTemp <- StatTemp[-1,]                             #Subtract column names from data field
row.names(StatTemp) <- NULL                           #Reorder row numbers to sequential starting from 1

assign(tempFrame,StatTemp)                            #Assign allocation to correctly named data frame
print(tempFrame)
View(get(tempFrame))                                  #Ensure proper transfer

assign(tempFrame,get(tempFrame)[,-1])                 #Subtract DPI Column

AlbStat$Sur[[tempFrame]] <- get(tempFrame)            #Double Brackets refer directly to the variable and not merely variable name so evaluated expressions can be used as indices
                                                      #This creates AegStat$Sur$ (print(tempFrame)) and assigns the data frame named tempFrame
View(AlbStat)  

#
#
#SubGroups


#Subsetting
Aeg_Z.Infected <- subset(Aeg_Z,Aeg_Z$Infected==TRUE)
View(Aeg_Z.Infected)

Aeg_Z.Uninfected <- subset(Aeg_Z, Aeg_Z$Infected == FALSE)

Aeg_Z.InfNotDis <- subset(Aeg_Z, (Aeg_Z$Infected == TRUE) && (Aeg_Z$Disseminated == FALSE))

Aeg_Z.Dis <- subset(Aeg_Z, Aeg_Z$Disseminated == TRUE)

PlotData <- subset(Aeg_Z, (Aeg_Z$Disseminated == TRUE) && (Aeg_Z$Blood.Average > 0))

#SubGroups = c("NInf","Inf","InfNDis","InfDis")

#Titer wrt Blood Average
ggplot(Aeg_Z[which((Aeg_Z$Disseminated == TRUE)),], aes(x = Aeg_Z$`Blood.Average Excluding Zeros`, y = PCR.Titer), na.rm = TRUE) + geom_point()
ggplot(Aeg_Z, aes(x =`Blood.# Feeding Events`, y = PCR.Titer, color = Disseminated)) + geom_point() + geom_smooth()
ggplot(Aeg_Z, aes(x = `Blood.Max Feeding`>0, fill = Disseminated)) + geom_bar()
  
ggplot(Aeg_Z[which((Aeg_Z$Disseminated == TRUE) && (Aeg_Z$Blood.Average > 0)),], aes(x = `Blood.Average Excluding Zeros`, y = PCR.Titer )) +
  geom_point()

ggplot(Aeg_Z, aes(Aeg_Z$`Mort.DPI Death`[which(Aeg_Z.Dis == TRUE)], y = Aeg_Z$`Blood.Average Excluding Zeros`[which(Aeg_Z.Dis == TRUE)])) + 
  geom_point() + geom_smooth()

ggplot(Aeg_Z, na.rm = TRUE , aes(x = `Blood.Average Excluding Zeros`, y = PCR.Titer, color = Disseminated)) +#color = (Disseminated == TRUE))) + 
  geom_point() + 
  geom_smooth()
  

library(reshape2)
temp <- melt(Aeg_Z, id.vars = "Mosquito")


#AegStat are all factors...

AegStat[1]
AegStat[2]
AegStat[2,1]
class(AegStat$Sur[[2]][2])
lapply(AegStat$Sur[[2]][2],class)
sapply(AegStat$Sur[[2]][2],class)
is.factor(AegStat$Sur[[2]][2,2])


temp <- AegStat$Sur[[1]]


#unfactor all statistics in xxxStat data frames
for(i in 1:length(AlbStat$Sur)){
  AlbStat$Sur[[i]] <- as.data.frame(sapply(AlbStat$Sur[[i]],unfactor))
}


#Create Blood Frequency Data Column
FreqTemp <- rowSums(as.data.frame(!sapply(Alb_Z$Blood$Eng[,1:10],is.na)))
Alb_Z$Blood$Freq <- as.numeric(Alb_Z$Blood$`# Feeding Events`)/FreqTemp 
View(Alb_Z)

Aeg_C$Fec <- AegFec_C
Aeg_C$Wings <- AegWings_C
#Out of order
AegWings_C <- AegWings_C[order(AegWings_C$Mosquito),]
View(AegWings_C)


Alb_Z$PCR <- AlboPCR

moveFrame <- "Positive"
assign(moveFrame,get(temp$PCR$moveFrame))

#Albopictus Blood T-Test

TTests <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],Alb_Z$Blood[x])))

TArray <- array(dim = c(5,5))
View(TArray)

TTests <- as.data.frame(sapply(names(Alb_C$Fec)[2:length(Alb_C$Fec)],function(x) t.test(Alb_C$Fec[x],Alb_Z$Fec[x])))


#
#Quick Subsetting Function
#

#Uninfected = 1
#Infected = 2
#Infected Not Disseminated = 3
#Infected And Disseminated = 4
sGroup <- function(DF,Group){
  Op <- switch(Group,
        "Infected == FALSE",
        "Infected == TRUE",
        "(Infected == TRUE)&(!(Disseminated == TRUE))",
        "(Infected == TRUE)&(Disseminated == TRUE)")
  df <- as.character(deparse(substitute(DF)))
  return(eval(parse(text = paste("subset(",df,",",Op,")"))))
}

####
SubGroups <- c("Uninfected","Infected","Infected Not Disseminated","Infected And Disseminated")

TTests <- NULL
TTests <- data.frame(Holding <- seq.int(1,9))
TTests$Exposed <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],Alb_Z$Blood[x])))
TTests <- TTests[-1]
for(i in 1:4){
  TTests[[i+1]] <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],sGroup(Alb_Z,i)$Blood[x])))
  names(TTests)[i+1] <- SubGroups[i]
  }
View(TTests)


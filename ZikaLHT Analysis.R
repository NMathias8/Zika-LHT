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

#SubGroups <- c("Uninfected","Infected","Infected Not Disseminated","Infected And Disseminated")

#Array of TTests

TTests <- NULL
TTests <- data.frame(Holding <- seq.int(1,9))
TTests$Exposed <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],Alb_Z$Blood[x])))
TTests <- TTests[-1]
for(i in 1:4){
  TTests[[i+1]] <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],sGroup(Alb_Z,i)$Blood[x])))
  names(TTests)[i+1] <- SubGroups[i]
}
View(TTests)

#View all p.values in TTests
#X = Group Y = Variable Comparison

Control.Groups.BloodPValues <- as.data.frame(sapply(1:5,function(x) sapply(1:5,function(y) TTests[[x]][[y]]$p.value)))
names(Control.Groups.BloodPValues) <- c("Exposed",SubGroups)
row.names(Control.Groups.BloodPValues) <- names(Alb_Z$Blood)[2:6]
View(Control.Groups.BloodPValues)

AlbStat.Blood <- list()
AlbStat.Blood[[1]] <- TTests
AlbStat.Blood[[2]] <- Control.Groups.BloodPValues
names(AlbStat.Blood) <- c("C.TTests","C.PValues")

##

FTTests <- data.frame(Holding <- seq.int(1,9))
FTTests$Exposed <- as.data.frame(sapply(names(Alb_C$Fec)[2:4],function(x) t.test(Alb_C$Fec[x],Alb_Z$Fec[x])))
FTTests <- FTTests[-1]
for(i in 1:4){
  FTTests[[i+1]] <- as.data.frame(sapply(names(Alb_C$Fec)[2:4],function(x) t.test(Alb_C$Fec[x],sGroup(Alb_Z,i)$Fec[x])))
  names(FTTests)[i+1] <- SubGroups[i]
}
View(FTTests)
#cycle through x(groups) and y(variables)

Control.Groups.FecPValues <- as.data.frame(sapply(1:5,function(x) sapply(1:3,function(y) FTTests[[x]][[y]]$p.value)))
names(Control.Groups.FecPValues) <- c("Exposed",SubGroups)
row.names(Control.Groups.FecPValues) <- names(Alb_C$Fec[2:4])
View(Control.Groups.FecPValues)

AlbStat.Fec <- NULL
AlbStat.Fec <- list()
#AlbStat.Fec <- c(as.data.frame(FTTests),as.data.frame(Control.Groups.FecPValues))
AlbStat.Fec[[1]] <- FTTests
AlbStat.Fec[[2]] <- Control.Groups.FecPValues
names(AlbStat.Fec) <- c("C.FTTests","C.FPValues")



#Threshold for Dissemination
#
#

t.test(sGroup(Alb_Z,3)$BTiter,sGroup(Alb_Z,4)$BTiter)   #SIG!!!   Alb_Z Infected (Not Disseminated vs. Disseminated)
t.test(sGroup(Aeg_Z,3)$BTiter,sGroup(Aeg_Z,4)$BTiter)   #SUPER SIG!!! Aeg_Z Infected (Not Disseminated vs. Disseminated)



##
##  Function with list input of variables to be analyzed and groups to be analyzed
##


SubGroupsNames <- c("Control","Exposed","Uninfected","Infected","Infected Not Disseminated","Infected And Disseminated")
SubGroups <- c("Aeg_C","Aeg_Z","sGroup(Aeg_Z,1)","sGroup(Aeg_Z,2)","sGroup(Aeg_Z,3)","sGroup(Aeg_Z,4)")

#eval(parse(text = ))
# t.test(eval(parse(text = paste(SubGroups[1],",",SubGroups[2]))))

# VarCompare <- lapply(names(Aeg_Z$Blood[2:6]),function(x) paste("$Blood$`",x,"`"))
VarCompare <- sapply(names(Aeg_Z$Blood[2:6]),function(x) paste("$Blood$`",x,"`",sep = ""))
# for(i in VarCompare){
#    print(paste("i = ",i))
#    for(j in SubGroups[2:length(SubGroups)]){
#       print(paste("Comparing ",j," to:"))
#       l <- match(j,SubGroups)
#       for(k in SubGroups[1:(l-1)]){
#          print(k)
#       }
#    }
# }

#X <- data.frame(dim<- c(length(VarCompare),length(SubGroups),length(SubGroups)-1,9))

X <- data.frame()
X <- array() #???
X <- length()
dim(X) <- c(length(VarCompare),length(SubGroups))
X<-NULL
for(i in VarCompare){
   for(j in SubGroups[2:length(SubGroups)]){
      l <- match(j,SubGroups)
      # for(k in SubGroups[1:(l-1)]){
      #    print(i)
      #    print(j)
      #    print(k)
      #    #X[[match(i,VarCompare)]][[match(j,SubGroups)+1]][[match(k,SubGroups)]] 
      #    temp <- as.data.frame(eval(parse(text = paste("t.test(",j,i,",",k,i,")",sep = ""))))
      # }
      X[[match(i,VarCompare)]][[match(j,SubGroups)]] <- as.array(sapply(SubGroups[1:(l-1)],function(k) eval(parse(text = paste("t.test(",j,i,",",k,i,")",sep = "")))))
   }
}


eval(parse(text = paste("t.test(",SubGroups[1],"$Blood$Freq",",",SubGroups[2],"$Blood$Freq",")")))






for(i in 2:length(SubGroups)){
   X[[i]] <- sapply(SubGroups[1:(i-1)],function(x) eval(parse(text = paste("t.test(",SubGroups[i],VarCompare[1],",",x,VarCompare[1],")",sep = ""))))
}
Y <- as.data.frame(sapply(2:length(SubGroups),function(x) sapply(1:(x-1),function(y) X[[x]][[y]]$p.value)))






TTests <- NULL
TTests <- data.frame(Holding <- seq.int(1,9))
TTests$Exposed <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],Alb_Z$Blood[x])))
TTests <- TTests[-1]
for(i in 1:4){
   TTests[[i+1]] <- as.data.frame(sapply(names(Alb_C$Blood)[2:6],function(x) t.test(Alb_C$Blood[x],sGroup(Alb_Z,i)$Blood[x])))
   names(TTests)[i+1] <- SubGroups[i]
}
View(TTests)


X <- data.frame(matrix(ncol=6,nrow = 6))
length(X) <- 9

SuperTTests <- NULL
SuperTTests <- data.frame(Variable <- TTests)
SuperTTests <- data.frame(Variable2 <- FTTests)

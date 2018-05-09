###################################################################################
#Bujarski et al. 2018 Neuropsychopharmacology                                     #
#                                                                                 #
#Data analysis code for reported results                                          #
#                                                                                 # 
#For any questions please contact Spencer Bujarski at s.bujarski@gmail.com        # 
###################################################################################


#REQUIRED LIBRARIES----
#devtools::install_github("sbujarski/SpPack")
library(SpPack)
library(ggplot2) #plotting
library(multilevel) #MLM analysis
library(dplyr) #data wrangling 2
library(pastecs) #summary stats function stat.desc
library(xlsx) #package to import xls files directly
library(grid) #for multiplot
library(psych) #for alpha (cronbach's alpha function)
library(polycor) #factor analysis related
library(GPArotation) #factor analysis related
library(GGally) #ggpairs plots
library(gridExtra) #tableGrob for adding results table to plots


#CUSTOM FUNCTIONS----

#Custom ggplot theme for vast majority of plots. 
#Superseded by SpTheme function within SpPack at devtools::install_github("sbujarski/SpPack")
Sp.Theme <- function(axis.text.size=12, axis.title.size=12, title.size=16, legend.position="none")
{
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}


#Histogram function using ggplot2
#Superseded by SpHist function within SpPack at devtools::install_github("sbujarski/SpPack")
Sp.Hist <- function(Data, var, bins=30, save=F, text.xy)
{
  Data<-Data[var]
  Data<-na.exclude(Data)
  Mean<-mean(Data[,1])
  SD<-sd(Data[,1])
  Histogram <- ggplot(Data, aes(Data[1])) + geom_histogram(aes(y=..density..), colour="white", bins=bins) + 
    stat_function(fun=dnorm, args=list(mean=Mean, sd=SD), size=3) +
    ggtitle(paste(var, "Histogram", "\n", "Mean =", round(Mean,2), "SD =", round(SD,2))) + scale_x_continuous(var) + Sp.Theme()
  Histogram
  if(save){
    ggsave(Histogram, filename=paste(paste(var, " Histogram.png", sep="")), width = 8, height=6.5, dpi=500)
  }
  return (Histogram)
}


#function to get qq plot with line from ggplot2
#from http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
  
}


#multiplot to grid plot all subject outputs
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#CAIS.Import
#Function to import CAIS Data from excel sheet
###########Not run in this code, but could be very useful for other researchers using CAIS
###########Extracted and processed CAIS data uploaded to github
###########NEEDS TO BE MODIFIED FOR A DIFFERENT CAIS PARADIGM
#Returns list of:
# 1) Sub.Vars -- Single line dataframe of subject level vars (Subject, gAlc, StartTime, Height, Weight, Female, Age, Drinks, SA.Drinks, Ktv, Kat, Rc, Vb, Vp, Mmax, Tbw)
# 2) PredBrAC -- dataframe of Time and CAIS predicted BrAC
# 3) PredBrAC.SA -- PredBrAC of Self-Admin period only
# 4) PredBrAC.Push -- PredBrAC of Push period only (Includes additional variables of Part ["Rise" or "Clamp"], and RCNum (rise/clamp number))
# 5) PredInf -- dataframe of Time and CAIS predicted Infusion
# 6) PredInf.SA -- PredInf of Self-Admin period only
# 7) PredInf.Push -- PredInf of Push period only (Includes additional variables of Part ["Rise" or "Clamp"], and RCNum (rise/clamp number))
# 8) DrinkTimes -- dataframe of DrinkTime (in minutes) of "drinks" and Type ("Push", "SA")
# 9) Responses -- dataframe of response button presses (Type["Push", "SA"], DrinkNum[within type], Time[in Min from start of infusion], Accepted[bool])
# 10) Responses.SA -- dataframe of responses only during SA period
# 11) BrACplot -- Predicted BrAC plot
# 12) Infplot -- Predicted Infusion Rate plot
# 13) TotalPressplot -- Cumulative Response plot during Self-Administration period
# 14) Sumplot -- Plot of summary stats for the subject
CAIS.Import <- function(filename, print, printmulti, outdir="C:/Users")
{
  #Specify response requirement
  Resp.Req <- c(10, 20, 33, 47, 64, 83, 107, 136, 173, 219, 279, 358, 460, 597, 777, 1019, 1342, 1775, 2357, 3139)
  
  #Subject Variables
  General <- read.xlsx(filename, sheetName="General_Information")
  
  gAlc <- as.double(as.character(General["Value"][1,]))
  Drinks <- as.double(as.character(General["Value"][2,]))
  SA.Drinks <- Drinks-3
  Breakpoint <- Resp.Req[SA.Drinks+1]
  StartTime <- as.double(as.character(General["Value"][4,]))
  
  SubjectInfo <- read.xlsx(filename, sheetName="Subject_Information", header=F)
  
  Subject <- as.double(as.character(SubjectInfo["X2"][2,]))
  Height <- as.double(as.character(SubjectInfo["X2"][8,]))
  Weight <- as.double(as.character(SubjectInfo["X2"][9,]))
  Sex <- as.double(as.character(SubjectInfo["X2"][10,]))
  Female <- Sex-1
  Age <- as.double(as.character(SubjectInfo["X2"][11,]))
  Ktv <- as.double(as.character(SubjectInfo["X2"][12,]))
  Kat <- as.double(as.character(SubjectInfo["X2"][13,]))
  Rc <- as.double(as.character(SubjectInfo["X2"][14,]))
  Vb <- as.double(as.character(SubjectInfo["X2"][15,]))
  Vp <- as.double(as.character(SubjectInfo["X2"][16,]))
  Mmax <- as.double(as.character(SubjectInfo["X2"][17,]))
  Tbw <- as.double(as.character(SubjectInfo["X2"][18,]))
  
  Sub.Vars <- data.frame(Subject=Subject, gAlc=gAlc, StartTime=StartTime, Height=Height, Weight=Weight, Female=Female, Age=Age,
                         Drinks=Drinks, SA.Drinks=SA.Drinks, Breakpoint=Breakpoint, 
                         Ktv=Ktv, Kat=Kat, Rc=Rc, Vb=Vb, Vp=Vp, Mmax=Mmax, Tbw=Tbw)
  
  
  #Predicted BrAC
  PredBrAC <- read.xlsx(filename, sheetName="Predicted_BrTh")
  names(PredBrAC)[names(PredBrAC)=="Brth"] <- "BrAC"
  PredBrAC$Subject <- Subject
  
  MaxBrAC <- max(PredBrAC$BrAC)
  Sub.Vars$MaxBrAC <- MaxBrAC
  
  BrACplot <- ggplot(PredBrAC, aes(x=Time, y=BrAC))+
    geom_line() + 
    ggtitle(paste("Subject", Subject, " --  BrAC by Time"))+
    scale_x_continuous("Time (Min)", limits=c(0,180), breaks=c(0,20,40,60,80,100,120,140,160,180))+
    scale_y_continuous("Predicted BrAC", limits=c(0,120), breaks=c(0,20,40,60,80,100,120))+
    theme_bw() + theme(plot.title=element_text(hjust=.5))
  if(print){print(BrACplot)}
  
  #Predicted Infusions
  PredInf <- read.xlsx(filename, sheetName="Predicted_Infusion")
  PredInf$Subject <- Subject
  
  Infplot <- ggplot(PredInf, aes(x=Time, y=Infusion))+
    geom_line() + 
    ggtitle(paste("Subject", Subject, " --  Infusion Rate by Time"))+
    scale_x_continuous("Time (Min)", limits=c(0,180), breaks=c(0,20,40,60,80,100,120,140,160,180))+
    scale_y_continuous("Predicted Inf", limits=c(0,2000))+
    theme_bw() + theme(plot.title=element_text(hjust=.5))
  if(print){print(Infplot)}
  
  #Drink Times
  DrinkTimes <- read.xlsx(filename, sheetName="Drinks_Taken")
  names(DrinkTimes)[names(DrinkTimes)=="Drink.Time"] <- "DrinkTime"
  DrinkTimes$Type <- c(rep("Push",3),rep("SA",dim(DrinkTimes)[1]-3))
  DrinkTimes$Subject <- Subject
  
  #Responses
  Responses <- read.xlsx(filename, sheetName="Work_Progress")
  Responses$Type <- c(rep("Push",3),rep("SA",dim(Responses)[1]-3))
  names(Responses)[names(Responses)=="SetNumber"] <- "DrinkNum"
  Responses$Time <- (Responses$ButtonPressTimeStamp-StartTime)*1440 
  Responses$Subject <- Subject
  
  TotalSAPress <- sum(Responses$Type=="SA")
  TotalSAPress.Accept <- sum(Responses$Type=="SA" & Responses$IsPressAccepted)
  PropAccepted = TotalSAPress.Accept/TotalSAPress
  
  Sub.Vars$TotalSAPress <- TotalSAPress
  Sub.Vars$TotalSAPress.Accept <- TotalSAPress.Accept
  Sub.Vars$PropAccepted <- PropAccepted
  
  Responses.SA <- subset(Responses, Type=="SA")
  Responses.SA$SATime <- Responses.SA$Time - min(Responses.SA$Time)
  
  TotalPressplot <- ggplot(Responses.SA, aes(x=SATime))+
    stat_bin(aes(y = cumsum(..count..)), geom="step", binwidth=.5) +
    ggtitle(paste("Subject", Subject, " --  Cumulative Self-Admin Responses"))+
    scale_x_continuous("Self-Administration Time (Min)", limits=c(-.5,180-min(Responses.SA$Time)))+
    scale_y_continuous("Cumulative Responses", limits=c(0,TotalSAPress))+
    theme_bw()
  if(print){print(TotalPressplot)}
  
  #PredBrAC.SA and PRedInf.SA
  PredBrAC.SA <- subset(PredBrAC, Time >= min(Responses.SA$Time))
  PredBrAC.SA$Time.SA <- PredBrAC.SA$Time - min(PredBrAC.SA$Time)
  PredInf.SA <- subset(PredInf, Time >= min(Responses.SA$Time))
  
  #PredBrAC.Push
  PredBrAC.Push <- subset(PredBrAC, Time < min(Responses.SA$Time))
  PredInf.Push <- subset(PredInf, Time < min(Responses.SA$Time))
  
  #Set default to "Clamp"
  PredBrAC.Push$Part <- "Clamp"
  PredInf.Push$Part <- "Clamp"
  
  #loop through PredInf.Push to set "Rise" or "Clamp"
  for(i in 1:(dim(PredInf.Push)[1]-1))
  {
    if(PredInf.Push$Infusion[i] < PredInf.Push$Infusion[i+1])
    {
      PredBrAC.Push$Part[i] <- "Rise"
      PredInf.Push$Part[i] <- "Rise"
    }
    #correct for initial temporary pauses. In < 10 minutes force set to "Rise"
    if(PredInf.Push$Time[i] < 10)
    {
      PredBrAC.Push$Part[i] <- "Rise"
      PredInf.Push$Part[i] <- "Rise"
    }
  }
  
  #Subject 87 is really weird first time point. 
  if(Subject==87)
  {
    for(i in 1:(dim(PredInf.Push)[1]-1))
    {
      if(PredInf.Push$Time[i] <= 15)
      {
        PredBrAC.Push$Part[i] <- "Rise"
        PredInf.Push$Part[i] <- "Rise"
      }
      else if(PredInf.Push$Time[i] <= 27.5)
      {
        PredBrAC.Push$Part[i] <- "Clamp"
        PredInf.Push$Part[i] <- "Clamp"
      }
      else if(PredInf.Push$Time[i] <= 42.5)
      {
        PredBrAC.Push$Part[i] <- "Rise"
        PredInf.Push$Part[i] <- "Rise"
      }
      else if(PredInf.Push$Time[i] <= 50)
      {
        PredBrAC.Push$Part[i] <- "Clamp"
        PredInf.Push$Part[i] <- "Clamp"
      }
      else if(PredInf.Push$Time[i] <= 65)
      {
        PredBrAC.Push$Part[i] <- "Rise"
        PredInf.Push$Part[i] <- "Rise"
      }
      else
      {
        PredBrAC.Push$Part[i] <- "Clamp"
        PredInf.Push$Part[i] <- "Clamp"
      }
    }
  }
  
  #number rises and clamps
  k <- 1
  for(i in 2:dim(PredInf.Push)[1])
  {
    PredInf.Push$RCNum[i-1] <- k
    PredBrAC.Push$RCNum[i-1] <- k
    if(PredInf.Push$Part[i]=="Rise" && PredInf.Push$Part[i-1]=="Clamp") 
    {
      k <- k+1
    }
  }
  PredInf.Push$RCNum[dim(PredInf.Push)[1]] <- 3
  PredBrAC.Push$RCNum[dim(PredBrAC.Push)[1]] <- 3
  
  #Compute Clamp durations and mean BrAC -- Add to Sub.Vars
  ClampDur <- subset(PredBrAC.Push,Part=="Clamp") %>% group_by(RCNum) %>% summarise(dur=n()*.5)
  Sub.Vars$Clamp1dur <- ClampDur$dur[1]
  Sub.Vars$Clamp2dur <- ClampDur$dur[2]
  Sub.Vars$Clamp3dur <- ClampDur$dur[3]
  
  ClampBrAC <- subset(PredBrAC.Push,Part=="Clamp") %>% group_by(RCNum) %>% summarise(meanBrAC=mean(BrAC))
  Sub.Vars$Clamp1BrAC <- ClampBrAC$meanBrAC[1]
  Sub.Vars$Clamp2BrAC <- ClampBrAC$meanBrAC[2]
  Sub.Vars$Clamp3BrAC <- ClampBrAC$meanBrAC[3]
  
  #Compute Push and SA Duration
  Sub.Vars$SA.Dur <- 180 - min(PredBrAC.SA$Time)
  Sub.Vars$Push.Dur <- 180 - Sub.Vars$SA.Dur
  
  #"Plot" of summary stats
  Sumplot <- ggplot()+
    geom_blank()+
    xlim(0,10) + ylim(0,10)+
    annotate("text", x=5, y=9, label=paste("Subject",Subject,"Summary Stats"), size=5, fontface="bold")+
    annotate("text", x=1, y=7, label=paste("Height(cm): ", Height), size=5, hjust=0)+
    annotate("text", x=1, y=5, label=paste("Weight(kg): ", Weight), size=5, hjust=0)+
    annotate("text", x=1, y=3, label=paste("Female: ", Female), size=5, hjust=0)+
    annotate("text", x=1, y=1, label=paste("Age: ", Age), size=5, hjust=0)+
    annotate("text", x=5, y=7, label=paste("SA Drinks: ", SA.Drinks), size=5, hjust=0)+
    annotate("text", x=5, y=5, label=paste("Max BrAC: ", round(MaxBrAC, digits=4)), size=5, hjust=0)+
    annotate("text", x=5, y=3, label=paste("Breakpoint: ", Breakpoint), size=5, hjust=0)+
    annotate("text", x=5, y=1, label=paste("Total Responses: ", TotalSAPress), size=5, hjust=0)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line.x = element_line(colour = NA), axis.line.y = element_line(colour = NA),
          axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), 
          axis.title.x=element_blank(), axis.title.y=element_blank())
  print(Sumplot)
  
  #Multiplot
  if(printmulti)
  {
    outfile=paste0(outdir,"Subject-",Subject,"-Summary_Plot.png")
    png(outfile, width=11, height=8, units="in", res=500)
    multiplot(BrACplot, Infplot, TotalPressplot, Sumplot, cols=2)
    dev.off()
  }
  #Return
  return(list(Sub.Vars=Sub.Vars, PredBrAC=PredBrAC, PredBrAC.SA=PredBrAC.SA, PredBrAC.Push=PredBrAC.Push, 
              PredInf=PredInf, PredInf.SA=PredInf.SA, PredInf.Push=PredInf.Push,
              DrinkTimes=DrinkTimes, Responses=Responses, Responses.SA=Responses.SA,
              BrACplot=BrACplot, Infplot=Infplot, TotalPressplot=TotalPressplot, Sumplot=Sumplot))
}

#Sample use for demonstrative purposes
CAIS.Sample <- CAIS.Import("Sample CAIS Data.xls", print=T, printmulti=T, outdir=getwd())
CAIS.Sample$Sub.Vars  #Subject level variables
CAIS.Sample$BrACplot #Plot of BrAC over the infusion
CAIS.Sample$Infplot #
CAIS.Sample$TotalPressplot


#Function to print the plots where BrAC curves are predicted by Alcohol Use Severity, and subjective response variables e.g. Figure 5 
BrAC.Bin.plot <- function(model, Severity, SR, SRName=NA, Poly, Cov=NA)
{
  if(is.na(SRName)) {SRName <- SR}
  Coefs <- model$coef$fixed
  
  SR.Mean <- mean(as.matrix(subset(TenMinBrAC, Time.SA.Bin==0)[SR]))
  SR.SD <- sd(as.matrix(subset(TenMinBrAC, Time.SA.Bin==0)[SR]))
  
  PredValues <- data.frame(PCA.Alc=c(rep(-1.26, 33),rep(-0.05, 33),rep(1.57, 33),rep(4.14, 33)), #4 levels of PCA.Alc
                           SR=rep(c(rep(SR.Mean-SR.SD, 11),rep(SR.Mean, 11), rep(SR.Mean+SR.SD, 11)),4),
                           SRlevel=rep(c(rep("-1 SD", 11),rep("Mean", 11), rep("+1 SD", 11)),4),
                           Time.SA.Bin=rep(seq(0,10,1), 12))
  PredValues$SRlevel <- factor(PredValues$SRlevel, levels=c("-1 SD", "Mean", "+1 SD"))
  
  PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
  PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
  PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
  
  #assume full model. -- Fill in zerroes for terms if missing
  for (i in length(Coefs))
  {
    if(is.na(Coefs["(Intercept)"])) Coefs["(Intercept)"]<-0
    if(is.na(Coefs["Time.SA.Bin"])) Coefs["Time.SA.Bin"]<-0
    if(is.na(Coefs["Time.SA.Bin.2"])) Coefs["Time.SA.Bin.2"]<-0
    if(is.na(Coefs["Time.SA.Bin.3"])) Coefs["Time.SA.Bin.3"]<-0
    if(is.na(Coefs["Time.SA.Bin.4"])) Coefs["Time.SA.Bin.4"]<-0
    if(is.na(Coefs["PCA.Alc"])) Coefs["PCA.Alc"]<-0
    if(is.na(Coefs[SR])) Coefs[SR]<-0
    if(is.na(Coefs["Time.SA.Bin:PCA.Alc"])) Coefs["Time.SA.Bin:PCA.Alc"]<-0
    if(is.na(Coefs["Time.SA.Bin.2:PCA.Alc"])) Coefs["Time.SA.Bin.2:PCA.Alc"]<-0
    if(is.na(Coefs["Time.SA.Bin.3:PCA.Alc"])) Coefs["Time.SA.Bin.3:PCA.Alc"]<-0
    if(is.na(Coefs["Time.SA.Bin.4:PCA.Alc"])) Coefs["Time.SA.Bin.4:PCA.Alc"]<-0
    if(is.na(Coefs[paste("Time.SA.Bin",SR,sep=":")])) Coefs[paste("Time.SA.Bin",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.2",SR,sep=":")])) Coefs[paste("Time.SA.Bin.2",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.3",SR,sep=":")])) Coefs[paste("Time.SA.Bin.3",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.4",SR,sep=":")])) Coefs[paste("Time.SA.Bin.4",SR,sep=":")]<-0
    if(is.na(Coefs[paste("PCA.Alc",SR,sep=":")])) Coefs[paste("PCA.Alc",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin","PCA.Alc",SR,sep=":")])) Coefs[paste("Time.SA.Bin","PCA.Alc",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.2","PCA.Alc",SR,sep=":")])) Coefs[paste("Time.SA.Bin.2","PCA.Alc",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.3","PCA.Alc",SR,sep=":")])) Coefs[paste("Time.SA.Bin.3","PCA.Alc",SR,sep=":")]<-0
    if(is.na(Coefs[paste("Time.SA.Bin.4","PCA.Alc",SR,sep=":")])) Coefs[paste("Time.SA.Bin.4","PCA.Alc",SR,sep=":")]<-0
  }
  
  PredValues$BrAC <- Coefs["(Intercept)"] +
    Coefs["Time.SA.Bin"] * PredValues$Time.SA.Bin +
    Coefs["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
    Coefs["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
    Coefs["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
    Coefs["PCA.Alc"] * PredValues$PCA.Alc +
    Coefs[SR] * PredValues$SR +
    Coefs["Time.SA.Bin:PCA.Alc"] * PredValues$Time.SA.Bin * PredValues$PCA.Alc +
    Coefs["Time.SA.Bin.2:PCA.Alc"] * PredValues$Time.SA.Bin.2 * PredValues$PCA.Alc +
    Coefs["Time.SA.Bin.3:PCA.Alc"] * PredValues$Time.SA.Bin.3 * PredValues$PCA.Alc +
    Coefs["Time.SA.Bin.4:PCA.Alc"] * PredValues$Time.SA.Bin.4 * PredValues$PCA.Alc +
    Coefs[paste("Time.SA.Bin",SR,sep=":")] * PredValues$Time.SA.Bin * PredValues$SR +
    Coefs[paste("Time.SA.Bin.2",SR,sep=":")] * PredValues$Time.SA.Bin.2 * PredValues$SR +
    Coefs[paste("Time.SA.Bin.3",SR,sep=":")] * PredValues$Time.SA.Bin.3 * PredValues$SR +
    Coefs[paste("Time.SA.Bin.4",SR,sep=":")] * PredValues$Time.SA.Bin.4 * PredValues$SR +
    Coefs[paste("PCA.Alc",SR,sep=":")] * PredValues$PCA.Alc * PredValues$SR +
    Coefs[paste("Time.SA.Bin","PCA.Alc",SR,sep=":")] * PredValues$Time.SA.Bin * PredValues$PCA.Alc * PredValues$SR +
    Coefs[paste("Time.SA.Bin.2","PCA.Alc",SR,sep=":")] * PredValues$Time.SA.Bin.2 * PredValues$PCA.Alc * PredValues$SR +
    Coefs[paste("Time.SA.Bin.3","PCA.Alc",SR,sep=":")] * PredValues$Time.SA.Bin.3 * PredValues$PCA.Alc * PredValues$SR +
    Coefs[paste("Time.SA.Bin.4","PCA.Alc",SR,sep=":")] * PredValues$Time.SA.Bin.4 * PredValues$PCA.Alc * PredValues$SR 
  
  
  colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
  stylescale <- c("11", "31", "solid")
  if(is.na(Cov))
  {
    return(ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, linetype=SRlevel, colour=as.factor(PCA.Alc))) + 
             stat_smooth(method = "lm", formula = y ~ poly(x, Poly), size = 1.5, se=F) + facet_wrap(~ PCA.Alc) +
             scale_linetype_manual(SRName, values = stylescale)+
             scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
             scale_x_continuous("Time (Min)")+
             scale_y_continuous("Estimated BrAC (mg%)")+
             Sp.Theme(legend.position="right") +
             ggtitle(paste("BrAC Curve by Alcohol Use Severity and", SRName)) +
             theme(panel.background = element_rect(colour = "black", fill=NA),
                   strip.text.x=element_blank(), strip.background = element_blank(),
                   legend.key.width=unit(2,"line")) +
             guides(linetype=guide_legend(override.aes = list(colour="black"))))
  }
  else
  {
    return(ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, linetype=SRlevel, colour=as.factor(PCA.Alc))) + 
             stat_smooth(method = "lm", formula = y ~ poly(x, Poly), size = 1.5, se=F) + facet_wrap(~ PCA.Alc) +
             scale_linetype_manual(SRName, values = stylescale)+
             scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
             scale_x_continuous("Time (Min)")+
             scale_y_continuous("Estimated BrAC (mg%)")+
             Sp.Theme(legend.position="right") +
             ggtitle(paste("BrAC Curve by Alcohol Use Severity and", SRName)) +
             theme(panel.background = element_rect(colour = "black", fill=NA),
                   strip.text.x=element_blank(), strip.background = element_blank(),
                   legend.key.width=unit(2,"line"))+
             guides(linetype=guide_legend(override.aes = list(colour="black"))))
  }
}



#IMPORT DATA----
Screened <- read.csv("Screened.csv") #Data from screened participants
Completers <- read.csv("Completers.csv") #Data from study completers
CAIS.PredBrAC.Merge <- read.csv("CAIS.PredBrAC.Merge.csv") #CAIS predicted BrAC values from all subjects
CAIS.PredBrAC.SA.Merge <- read.csv("CAIS.PredBrAC.SA.Merge.csv") #CAIS predicted BrAC values just for the Self-Administration component
AlcResponses <- read.csv("AlcResponses.csv") #Data from study completers


#Sample Characteristics----
#Demographics
SpDesc(Completers$Age)
cor.test(Completers$Age, Completers$PCA.Alc)
table(Completers["Female"])
summary(aov(PCA.Alc ~ Female, data=Completers))
SpDesc(Completers$BDI)
cor.test(Completers$BDI.sq, Completers$PCA.Alc)
SpDesc(Completers$CPD)
cor.test(Completers$CPD, Completers$PCA.Alc)

#Alcohol Use Variables
SpDesc(Completers[c("DPW", "DPDD", "Drink.Days", "Binge.Per", "ADS", "AUDIT",  "CIWA", "OCDS", "PACS")])
cor.test(Completers$DPW, Completers$PCA.Alc)
cor.test(Completers$DPDD, Completers$PCA.Alc)
cor.test(Completers$Drink.Days, Completers$PCA.Alc)
cor.test(Completers$Binge.Per, Completers$PCA.Alc)
cor.test(Completers$ADS, Completers$PCA.Alc)
cor.test(Completers$AUDIT, Completers$PCA.Alc)
cor.test(Completers$CIWA, Completers$PCA.Alc)
cor.test(Completers$OCDS, Completers$PCA.Alc)
cor.test(Completers$PACS, Completers$PCA.Alc)

#DSM-5 AUD Variables
SpDesc(Completers$AUD.Sxs.L)
cor.test(Completers$AUD.Sxs.L, Completers$PCA.Alc)
table(Completers$AUD.Sev.Lstr)
summary(aov(PCA.Alc ~ AUD.Sev.Lstr, data=Completers))
SpDesc(Completers$AUD.Onset)
cor.test(Completers$AUD.Onset, Completers$PCA.Alc)
SpDesc(Completers$AUD.Sxs.C)
cor.test(Completers$AUD.Sxs.C, Completers$PCA.Alc)
table(Completers$AUD.Sev.Cstr)
summary(aov(PCA.Alc ~ AUD.Sev.Cstr, data=Completers))



#Alcohol Use Severity Factor----
#Use all Beh screen data N=140
#principal component analysis for data reduction
pairs(na.exclude(Screened[c("DPW", "DPDD", "Drink.Days", "Binge.Per",                #TLFB
                            "ADS", "AUDIT",  "CIWA", "OCDS", "PACS",                 #Scales
                            "AUD.Sxs.L", "AUD.Sxs.C")]))                             #SCID

PCA.Alc <- princomp(~DPW + DPDD + Drink.Days + Binge.Per + ADS + AUDIT +  CIWA + OCDS + PACS + AUD.Sxs.L + AUD.Sxs.C,
                    data=Screened, na.action=na.exclude, cor=TRUE)
loadings(PCA.Alc)
summary(PCA.Alc)
PCA.Alc$sd^2 #eigenvalues
dim(PCA.Alc$scores)
plot(PCA.Alc, type="lines") #1 factor clearly the right answer
PCA.Alc.Scree <- ggplot(data=data.frame(Component=seq(1,11,1),Eigenvalue=PCA.Alc$sd^2), aes(x=Component, y=Eigenvalue)) + 
  geom_line(size=1.5) + geom_point(size=4) + ggtitle("Alcohol Use Severity Factor Scree Plot") + scale_x_continuous(breaks=seq(1,11,1)) +
  Sp.Theme()
PCA.Alc.Scree
PCA.Alc$scores[,1]
Screened$PCA.Alc <- -1*PCA.Alc$scores[,1] #Extract PCA scores and reverse sign since PCA switched it
Sp.Hist(Data=Screened, var="PCA.Alc", bins=15, save=T)

cor(na.exclude(Screened[c("DPW", "DPDD", "Drink.Days", "Binge.Per",
                          "ADS", "AUDIT",  "CIWA", "OCDS", "PACS",
                          "AUD.Sxs.L", "AUD.Sxs.C",
                          "PCA.Alc")]))
ggpairs(data=na.exclude(Screened[c("DPW", "DPDD", "Drink.Days", "Binge.Per",
                                   "ADS", "AUDIT",  "CIWA", "OCDS", "PACS",
                                   "AUD.Sxs.L", "AUD.Sxs.C",
                                   "PCA.Alc")]))
SpDesc(Screened$PCA.Alc)


#Alcohol Administration Overview----
#Timing summaries
Push.End.M <- mean(Completers$Clamp1dur + Completers$Clamp2dur + Completers$Clamp3dur + 45)
Push.End.M #Mean push/challenge duration
SA.Start.M <- mean(Completers$Push.Dur)
sd(Completers$Clamp1dur + Completers$Clamp2dur + Completers$Clamp3dur + 45) #sd of mean push/challenge duration

#Mean SA Duration
180-SA.Start.M #mean duration of self-administration
sd(Completers$Push.Dur)#sd of self-administration

SpDesc(Completers[c("Clamp1BrAC", "Clamp2BrAC", "Clamp3BrAC")]) #Mean and SD of BrACs during the challenge

#mean clamp duration
SpDesc(c(Completers$Clamp1dur, Completers$Clamp2dur, Completers$Clamp3dur))
#differences in durations
SpDesc(Completers[c("Clamp1dur", "Clamp2dur", "Clamp3dur")])
Clamp.Durs <- data.frame(Clamp=c(rep(1,67),rep(2,67),rep(3,67)), 
                         Dur=c(Completers$Clamp1dur, Completers$Clamp2dur, Completers$Clamp3dur))
summary(aov(Dur~as.factor(Clamp), data=Clamp.Durs))

#Figure 2
PredBrACplot <- ggplot(data=CAIS.PredBrAC.Merge, aes(x=Time, y=BrAC, colour=as.factor(Subject)))+
  geom_line()+
  scale_x_continuous("Time (min)", limits=c(0,185), breaks=c(seq(0,180,20)), expand = c(0,0))+
  scale_y_continuous("Estimated Breath Alcohol Concentration (mg%)", limits=c(0,130), breaks=c(seq(0,120,20)), expand = c(0,0))+
  annotate("rect", xmin=Push.End.M, xmax=SA.Start.M, ymin=0, ymax=130, alpha=.2) + 
  annotate("text", label="Alcohol Challenge", x=Push.End.M-5, y= 7, hjust=1, fontface="bold", size=4) +
  #geom_vline(xintercept=Push.End.M, linetype="dashed", colour="grey75") +
  annotate("text", label="Restroom Break", x=mean(c(Push.End.M,SA.Start.M)), y=5, angle=90, hjust=0, vjust=.5, fontface="bold", size=4) +
  #geom_vline(xintercept=SA.Start.M, linetype="dashed", colour="grey75") +
  annotate("text", label="Self-Administration", x=SA.Start.M+5, y= 7, hjust=0, fontface="bold", size=4) +
  ggtitle("Individual Breath Alcohol Concentration Curves") +
  Sp.Theme(axis.title.size=14)
PredBrACplot


#Subjective Response to the Alcohol Challenge----
#Merge Completers Variables with AlcResponses
Challenge <- merge(AlcResponses, Completers, by="Subject")

#recode condition/trial variable
Challenge$trial <- Challenge$Cond-1
table(Challenge$trial) #should be 67 of 0,1,2,3

#Z-score transform all SR variables (for combining later)
Challenge$StimZ <- scale(Challenge$Stim, center=T, scale=T)
SpDesc(Challenge[c("Stim","StimZ")]) #N should be 268

Challenge$HapZ <- scale(Challenge$Hap, center=T, scale=T)
SpDesc(Challenge[c("Hap","HapZ")]) #N should be 268

Challenge$VigZ <- scale(Challenge$Vig, center=T, scale=T)
SpDesc(Challenge[c("Vig","VigZ")]) #N should be 268

Challenge$Stim.Comb <- rowSums(Challenge[c("StimZ","HapZ","VigZ")])
SpDesc(Challenge[c("StimZ","HapZ","VigZ","Stim.Comb")]) #N should be 268

Challenge$SedZ <- scale(Challenge$Sed, center=T, scale=T)
SpDesc(Challenge[c("Sed","SedZ")]) #N should be 268

Challenge$SHASZ <- scale(Challenge$SHAS, center=T, scale=T)
SpDesc(Challenge[c("SHAS","SHASZ")]) #N should be 268

Challenge$Sed.Comb <- rowSums(Challenge[c("SedZ","SHASZ")])
SpDesc(Challenge[c("SedZ","SHASZ","Sed.Comb")]) #N should be 268

Challenge$TenZ <- scale(Challenge$Ten, center=T, scale=T)
SpDesc(Challenge[c("Ten","TenZ")]) #N should be 268

Challenge$DepZ <- scale(Challenge$Dep, center=T, scale=T)
SpDesc(Challenge[c("Dep","DepZ")]) #N should be 268

Challenge$NA.Comb <- rowSums(Challenge[c("TenZ","DepZ")])
SpDesc(Challenge[c("TenZ","DepZ","NA.Comb")]) #N should be 268

Challenge$AUQZ <- scale(Challenge$AUQ, center=T, scale=T)


#Stimulation
Challenge$Stim.Comb <- rowSums(Challenge[c("StimZ","HapZ","VigZ")])
SpDesc(Challenge[c("StimZ","HapZ","VigZ","Stim.Comb")]) #N should be 268

#Main effects model
summary(lme(Stim.Comb~trial + PCA.Alc, 
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

#Interactive Model
Challenge.MLM.Stim.Comb <- lme(Stim.Comb~trial + PCA.Alc + trial:PCA.Alc, 
                          data=Challenge, na.action=na.exclude,
                          random = ~trial|Subject,
                          control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.Stim.Comb)
#                   Value  Std.Error  DF    t-value p-value
# (Intercept)   -0.4103407 0.28668307 199 -1.4313391  0.1539
# trial          0.2817624 0.11152560 199  2.5264369  0.0123
# PCA.Alc       -0.0497307 0.13340741  65 -0.3727729  0.7105
# trial:PCA.Alc  0.0004982 0.05189822 199  0.0095998  0.9924

PredValues <- data.frame(trial=rep(seq(0,3,3),4),
                         BrAC=rep(seq(0,60,60),4),
                         PCA.Alc=c(rep(-1.26,2),rep(-0.05,2),rep(1.57,2),rep(4.14,2)))
PredValues$Stim.Comb <- Challenge.MLM.Stim.Comb$coef$fixed["(Intercept)"] +
  Challenge.MLM.Stim.Comb$coef$fixed["trial"] * PredValues$trial +
  Challenge.MLM.Stim.Comb$coef$fixed["PCA.Alc"] * PredValues$PCA.Alc +
  Challenge.MLM.Stim.Comb$coef$fixed["trial:PCA.Alc"] * PredValues$trial * PredValues$PCA.Alc
PredValues

colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
Stim.Comb.plot.NoLegend <- ggplot(data=PredValues, aes(x=BrAC, y=Stim.Comb, colour=as.factor(PCA.Alc))) + geom_line(size=2) + 
  scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
  scale_x_continuous("BrAC Time Point (mg%)") + scale_y_continuous("Stimulation (Combined Score)", limits=c(-1.1,1.5), breaks=seq(-1,1.5,.5)) +
  ggtitle("A. Stimulation") + Sp.Theme(axis.text.size=16, axis.title.size=16, title.size=20) 
Stim.Comb.plot.NoLegend

#Robustness checks
summary(lme(Stim.Comb~trial + Female,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Stim.Comb~trial + Age + trial:Age,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Stim.Comb~trial + BDI.sq,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Stim.Comb~trial + CPD,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))


#Sedation
Challenge$Sed.Comb <- rowSums(Challenge[c("SedZ","SHASZ")])
SpDesc(Challenge[c("SedZ","SHASZ","Sed.Comb")]) #N should be 268

#Main effects model
summary(lme(Sed.Comb~trial + PCA.Alc, 
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

#Interactive Model
Challenge.MLM.Sed.Comb <- lme(Sed.Comb~trial + PCA.Alc + trial:PCA.Alc, 
                         data=Challenge, na.action=na.exclude,
                         random = ~trial|Subject,
                         control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.Sed.Comb)
#                    Value  Std.Error  DF   t-value p-value
# (Intercept)   -0.8183275 0.16136956 199 -5.071139  0.0000
# trial          0.5155327 0.06854715 199  7.520847  0.0000
# PCA.Alc        0.2207654 0.07509301  65  2.939892  0.0045
# trial:PCA.Alc -0.0276579 0.03189828 199 -0.867064  0.3870

PredValues <- data.frame(trial=rep(seq(0,3,3),4),
                         BrAC=rep(seq(0,60,60),4),
                         PCA.Alc=c(rep(-1.26,2),rep(-0.05,2),rep(1.57,2),rep(4.14,2)))
PredValues$Sed.Comb <- Challenge.MLM.Sed.Comb$coef$fixed["(Intercept)"] +
  Challenge.MLM.Sed.Comb$coef$fixed["trial"] * PredValues$trial +
  Challenge.MLM.Sed.Comb$coef$fixed["PCA.Alc"] * PredValues$PCA.Alc +
  Challenge.MLM.Sed.Comb$coef$fixed["trial:PCA.Alc"] * PredValues$trial * PredValues$PCA.Alc
PredValues

colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
Sed.Comb.plot.NoLegend <- ggplot(data=PredValues, aes(x=BrAC, y=Sed.Comb, colour=as.factor(PCA.Alc))) + geom_line(size=2) + 
  scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
  scale_x_continuous("BrAC Time Point (mg%)") + scale_y_continuous("Sedation (Combined Score)", limits=c(-1.1,1.5), breaks=seq(-1,1.5,.5)) +
  ggtitle("B. Sedation") + Sp.Theme(axis.text.size=16, axis.title.size=16, title.size=20) 
Sed.Comb.plot.NoLegend

#robustness checks
summary(lme(Sed.Comb~trial + Female,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Sed.Comb~trial + Age,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Sed.Comb~trial + PCA.Alc + BDI.sq,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(Sed.Comb~trial + PCA.Alc + CPD,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))



#Negative Mood
Challenge$NA.Comb <- rowSums(Challenge[c("TenZ","DepZ")])
SpDesc(Challenge[c("TenZ","DepZ","NA.Comb")]) #N should be 268

#Main effects model
summary(lme(NA.Comb~trial + PCA.Alc, 
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

#Interactive Model
Challenge.MLM.NA.Comb <- lme(NA.Comb~trial + PCA.Alc + trial:PCA.Alc, 
                        data=Challenge, na.action=na.exclude,
                        random = ~trial|Subject,
                        control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.NA.Comb)
#                   Value  Std.Error  DF   t-value p-value
# (Intercept)    0.3333793 0.23715138 199  1.405766  0.1614
# trial         -0.2626073 0.06322133 199 -4.153777  0.0000
# PCA.Alc        0.1961175 0.11035794  65  1.777103  0.0802
# trial:PCA.Alc  0.0299240 0.02941992 199  1.017134  0.3103



PredValues <- data.frame(trial=rep(seq(0,3,3),4),
                         BrAC=rep(seq(0,60,60),4),
                         PCA.Alc=c(rep(-1.26,2),rep(-0.05,2),rep(1.57,2),rep(4.14,2)))
PredValues$NA.Comb <- Challenge.MLM.NA.Comb$coef$fixed["(Intercept)"] +
  Challenge.MLM.NA.Comb$coef$fixed["trial"] * PredValues$trial +
  Challenge.MLM.NA.Comb$coef$fixed["PCA.Alc"] * PredValues$PCA.Alc +
  Challenge.MLM.NA.Comb$coef$fixed["trial:PCA.Alc"] * PredValues$trial * PredValues$PCA.Alc
PredValues

colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
NA.Comb.plot.NoLegend <- ggplot(data=PredValues, aes(x=BrAC, y=NA.Comb, colour=as.factor(PCA.Alc))) + geom_line(size=2) + 
  scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
  scale_x_continuous("BrAC Time Point (mg%)") + scale_y_continuous("Negative Affect (Combined Score)", limits=c(-1.1,1.5), breaks=seq(-1,1.5,.5)) +
  ggtitle("C. Negative Affect") + Sp.Theme(axis.text.size=16, axis.title.size=16, title.size=20) 
NA.Comb.plot.NoLegend

#robustness checks
summary(lme(NA.Comb~trial + Female + trial:Female,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(NA.Comb~trial + Age + PCA.Alc,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(NA.Comb~trial + BDI.sq + PCA.Alc,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(NA.Comb~trial  + PCA.Alc + CPD,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))


#Craving
#Main effect moodel
summary(lme(AUQZ~trial + PCA.Alc, 
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

#Interactive Model
Challenge.MLM.AUQZ <- lme(AUQZ~trial + PCA.Alc + trial:PCA.Alc, 
                     data=Challenge, na.action=na.exclude,
                     random = ~trial|Subject,
                     control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.AUQZ)
#                   Value  Std.Error  DF   t-value p-value
# (Intercept)   -0.3387372 0.09799050 199 -3.456837  0.0007
# trial          0.1955726 0.03469340 199  5.637171  0.0000
# PCA.Alc        0.1763565 0.04559969  65  3.867494  0.0003
# trial:PCA.Alc  0.0028765 0.01614450 199  0.178171  0.8588

PredValues <- data.frame(trial=rep(seq(0,3,3),4),
                         BrAC=rep(seq(0,60,60),4),
                         PCA.Alc=c(rep(-1.26,2),rep(-0.05,2),rep(1.57,2),rep(4.14,2)))
PredValues$AUQZ <- Challenge.MLM.AUQZ$coef$fixed["(Intercept)"] +
  Challenge.MLM.AUQZ$coef$fixed["trial"] * PredValues$trial +
  Challenge.MLM.AUQZ$coef$fixed["PCA.Alc"] * PredValues$PCA.Alc +
  Challenge.MLM.AUQZ$coef$fixed["trial:PCA.Alc"] * PredValues$trial * PredValues$PCA.Alc
PredValues

colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
AUQZ.plot.NoLegend <- ggplot(data=PredValues, aes(x=BrAC, y=AUQZ, colour=as.factor(PCA.Alc))) + geom_line(size=2) + 
  scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) + 
  scale_x_continuous("BrAC Time Point (mg%)") + scale_y_continuous("Alcohol Craving (AUQ)", limits=c(-1.1,1.5), breaks=seq(-1,1.5,.5)) +
  ggtitle("D. Alcohol Craving") + Sp.Theme(axis.text.size=16, axis.title.size=16, title.size=20) 
AUQZ.plot.NoLegend

#Robustness checks
summary(lme(AUQZ~trial + Female + PCA.Alc,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(AUQZ~trial + Age + Age:trial,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(AUQZ~trial + BDI.sq + PCA.Alc,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))

summary(lme(AUQZ~trial + CPD + PCA.Alc,
            data=Challenge, na.action=na.exclude,
            random = ~trial|Subject,
            control=list(opt="optim",maxIter=400)))



#Alcohol Use Severity and Self-Administration----
#Autocorrelation problem
#Problem of too many observations per subject that are too intercorrelated
#Count number of SA BrAC observations per subject
SA.N.Obs <- CAIS.PredBrAC.SA.Merge %>% group_by(Subject) %>% summarise(N.Obs=sum(!is.na(BrAC)))
SpDesc(SA.N.Obs)
#Lag BrAC to look at mean change from 1 obs to next
CAIS.PredBrAC.SA.Merge$BrAC.Lag1 <- lag(CAIS.PredBrAC.SA.Merge$BrAC, 1) #Lag BrAC by 1
CAIS.PredBrAC.SA.Merge$BrAC.Lag1 <- ifelse(CAIS.PredBrAC.SA.Merge$Time.SA==0,NA,CAIS.PredBrAC.SA.Merge$BrAC.Lag1) #Delete lag carryover from prior subject
SpDesc(CAIS.PredBrAC.SA.Merge$BrAC.Lag1) #N should be 13256

#mean absolute BrAC change from n-1 to n
CAIS.PredBrAC.SA.Merge$BrAC.Lag.absD <- abs(CAIS.PredBrAC.SA.Merge$BrAC - CAIS.PredBrAC.SA.Merge$BrAC.Lag1)
SpDesc(CAIS.PredBrAC.SA.Merge$BrAC.Lag.absD) #N should be 13256
#     nbr.val          min          max       median         mean      SE.mean          var      std.dev 
#1.325600e+04 1.131551e-03 2.732491e+00 4.973715e-01 6.677006e-01 4.215250e-03 2.355370e-01 4.853216e-01 
#estimate of correlation between observations (doesnt account for nested data structure)
cor.test(CAIS.PredBrAC.SA.Merge$BrAC,CAIS.PredBrAC.SA.Merge$BrAC.Lag1) 
#      cor:  0.9994874
#Clearly autocorrelation issue is huge. 

#Solution to autocorrelation issues
#Binning BrAC's
#making and labeling 10 minute bins for BrAC
CAIS.PredBrAC.SA.Merge$Time.SA.Bin <- floor(CAIS.PredBrAC.SA.Merge$Time.SA/10)
#View(CAIS.PredBrAC.SA.Merge[c("Time.SA","Time.SA.Bin")])
TenMinBrAC <- CAIS.PredBrAC.SA.Merge %>% group_by(Subject, Time.SA.Bin) %>% summarise(BrAC.M=mean(BrAC), BrAC.SD=sd(BrAC), count=sum(!is.na(BrAC)))
SpDesc(TenMinBrAC)#N should be 698

#count number of observations
BinCounts <- TenMinBrAC %>% group_by(Subject) %>% summarise(N.Bins=sum(!is.na(BrAC.M)))
SpDesc(BinCounts)

#plot bins
BrAC.Bins.Plot.Raw <- ggplot(data=CAIS.PredBrAC.SA.Merge, aes(x=Time.SA, y=BrAC)) + geom_line() +
  geom_line(data=TenMinBrAC, aes(x=(10*Time.SA.Bin + 5), y=BrAC.M), size=1, colour="blue") +
  facet_wrap(~Subject, scales="free") + 
  ggtitle("Self-Administration BrAC Curves") + scale_x_continuous("Self-Administration Time (min)", limits=c(0,100)) + 
  scale_y_continuous("BrAC (mg%)", limits=c(20,120)) +
  Sp.Theme() + theme(axis.text.x=element_blank(), axis.text.y=element_blank())
BrAC.Bins.Plot.Raw

#mean absolute BrAC change from bin n-1 to n
TenMinBrAC$BrAC.MLag1 <- lag(TenMinBrAC$BrAC.M, 1)
TenMinBrAC$BrAC.MLag1 <- ifelse(TenMinBrAC$Time.SA.Bin==0,NA,TenMinBrAC$BrAC.MLag1) #Delete lag carryover from prior subject
TenMinBrAC$BrAC.MLag1.absD <- abs(TenMinBrAC$BrAC.MLag1 - TenMinBrAC$BrAC.M)
SpDesc(TenMinBrAC$BrAC.MLag1.absD) #N should be 631
#     nbr.val          min          max       median         mean      SE.mean          var      std.dev 
#631.00000000   0.01030167  23.56265778   3.70426657   5.05937151   0.17924632  20.27355200   4.50261613 
#estimate of correlation between observations (doesnt account for nested data structure)
cor.test(TenMinBrAC$BrAC.M,TenMinBrAC$BrAC.MLag1) 
#      cor 0.9678949 Autocorrelation still very much an issue. But MUCH lower. 

#test difference in autocorrelation estimates
paired.r(0.9994874,0.9678949,NULL, 13256, 631) # test of independent correlations, different sample sizes
#[1] "test of difference between two independent correlations"
#z = 50.85  With probability =  0


#Merge Binned BrAC data with subject-level variables
TenMinBrAC <- merge(TenMinBrAC, Completers, by="Subject")
#Setting up the polynomial 
TenMinBrAC$Time.SA.Bin.2 <- TenMinBrAC$Time.SA.Bin^2
TenMinBrAC$Time.SA.Bin.3 <- TenMinBrAC$Time.SA.Bin^3
TenMinBrAC$Time.SA.Bin.4 <- TenMinBrAC$Time.SA.Bin^4
TenMinBrAC$Time.SA.Bin.5 <- TenMinBrAC$Time.SA.Bin^5
TenMinBrAC$Time.SA.Bin.6 <- TenMinBrAC$Time.SA.Bin^6


#Figure out polynomial model parameters.
#Model building steps
#1. Figure out Fixed effects model first with Poly terms and PCA.Alc
#2. Includ full Poly x PCA.Alc Interactions
#3. Drop NS highest order interactions
#3. Figure out random effects

SRSA.BrAC.SA.Bin.Poly3.PCA.Alc.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + 
                                             PCA.Alc +
                                             PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                           random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3|Subject, data=TenMinBrAC,
                                           control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                           correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly3.PCA.Alc.Full)
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                             PCA.Alc +
                                             PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4,
                                           random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                           control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                           correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full)
anova(SRSA.BrAC.SA.Bin.Poly3.PCA.Alc.Full, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full)
#                                    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# SRSA.BrAC.SA.Bin.Poly3.PCA.Alc.Full     1 20 4329.277 4420.241 -2144.639                        
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full     2 27 4296.687 4419.489 -2121.343 1 vs 2 46.59003  <.0001

SRSA.BrAC.SA.Bin.Poly5.PCA.Alc.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + Time.SA.Bin.5 +
                                             PCA.Alc +
                                             PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 + PCA.Alc:Time.SA.Bin.5,
                                           random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4+Time.SA.Bin.5|Subject, data=TenMinBrAC,
                                           control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                           correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly5.PCA.Alc.Full)
anova(SRSA.BrAC.SA.Bin.Poly5.PCA.Alc.Full, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full)
#                                    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#SRSA.BrAC.SA.Bin.Poly5.PCA.Alc.Full     1 35 4322.699 4481.887 -2126.349                        
#SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Full     2 27 4296.687 4419.489 -2121.343 1 vs 2 10.01186  0.2642
#Poly4 is sufficient to model the fixed effects. 

#trim fixed effects of highest order interaction. 
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                             PCA.Alc +
                                             PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                           random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                           control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                           correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim)
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           67.33710 0.8395385 624 80.20728  0.0000
# Time.SA.Bin           10.35008 1.3315653 624  7.77287  0.0000
# Time.SA.Bin.2         -2.28251 0.3324543 624 -6.86564  0.0000
# Time.SA.Bin.3          0.19496 0.0488522 624  3.99082  0.0001
# Time.SA.Bin.4         -0.00592 0.0025300 624 -2.33971  0.0196
# PCA.Alc                0.39851 0.3863359  65  1.03152  0.3061
# Time.SA.Bin:PCA.Alc    1.19662 0.6026357 624  1.98564  0.0475
# Time.SA.Bin.2:PCA.Alc -0.23072 0.0983460 624 -2.34599  0.0193
# Time.SA.Bin.3:PCA.Alc  0.01275 0.0056165 624  2.27030  0.0235

#test random effects
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR4 <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                  PCA.Alc +
                                                  PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3|Subject, data=TenMinBrAC,
                                                control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR4)
anova(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR4)
#Poly4 highly random
#                                         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim          1 26 4295.588 4413.841 -2121.794                        
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR4     2 21 4328.122 4423.635 -2143.061 1 vs 2 42.53418  <.0001

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR3 <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                  PCA.Alc +
                                                  PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR3)
anova(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR3)
#Poly3 Highly random
#                                         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim          1 26 4295.588 4413.841 -2121.794                        
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR3     2 21 4327.824 4423.337 -2142.912 1 vs 2 42.23649  <.0001

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR2 <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                  PCA.Alc +
                                                  PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                random=~Time.SA.Bin + Time.SA.Bin.3 + Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                control=list(opt="optim",maxIter=400, msMaxIter = 400, niterEM=100), method="ML", na.action=na.exclude,
                                                correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR2)
anova(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR2)
#Poly2 highly random
#                                         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim          1 26 4295.588 4413.841 -2121.794                        
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR2     2 21 4326.762 4422.274 -2142.381 1 vs 2 41.17388  <.0001

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR1 <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                  PCA.Alc +
                                                  PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                random=~Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR1)
anova(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim, SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR1)
#Poly1 veryhighly random
#                                         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim          1 26 4295.588 4413.841 -2121.794                        
# SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.noR1     2 21 4576.226 4671.739 -2267.113 1 vs 2 290.6384  <.0001

#All polynomial effects found to be random at Level 2 p's < 0.0001

#Final PCA.Alc Model
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                             PCA.Alc +
                                             PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                           random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                           control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                           correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim)
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           67.33710 0.8395385 624 80.20728  0.0000
# Time.SA.Bin           10.35008 1.3315653 624  7.77287  0.0000
# Time.SA.Bin.2         -2.28251 0.3324543 624 -6.86564  0.0000
# Time.SA.Bin.3          0.19496 0.0488522 624  3.99082  0.0001
# Time.SA.Bin.4         -0.00592 0.0025300 624 -2.33971  0.0196
# PCA.Alc                0.39851 0.3863359  65  1.03152  0.3061
# Time.SA.Bin:PCA.Alc    1.19662 0.6026357 624  1.98564  0.0475
# Time.SA.Bin.2:PCA.Alc -0.23072 0.0983460 624 -2.34599  0.0193
# Time.SA.Bin.3:PCA.Alc  0.01275 0.0056165 624  2.27030  0.0235

#plotting final PCA.Alc Model
PredValues <- data.frame(PCA.Alc=c(rep(-1.26, 11),rep(-0.05, 11),rep(1.57, 11),rep(4.14, 11)),
                         Time.SA.Bin=rep(seq(0,10,1), 4))
PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
PredValues$BrAC <- SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["(Intercept)"] +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin"] * PredValues$Time.SA.Bin +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["PCA.Alc"] * PredValues$PCA.Alc +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin:PCA.Alc"] * PredValues$Time.SA.Bin * PredValues$PCA.Alc +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin.2:PCA.Alc"] * PredValues$Time.SA.Bin.2 * PredValues$PCA.Alc +
  SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim$coef$fixed["Time.SA.Bin.3:PCA.Alc"] * PredValues$Time.SA.Bin.3 * PredValues$PCA.Alc
PredValues

colorscale <- scales::seq_gradient_pal("lightblue", "navyblue", "Lab")(seq(0,1,length.out=4))
Time.SA.Bin.PCA.Alc.plot <- ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, colour=as.factor(PCA.Alc))) +
  #geom_point(size=4) + #used only to verify stat_Smooth 
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), size = 2, se=F) +
  Sp.Theme(legend.position="right") + scale_colour_manual("Alcohol Use\nSeverity Factor", values=colorscale) +
  ggtitle("BrAC Curves by Alcohol Use Severity") + 
  scale_x_continuous("Self-Administration Time (min)") + scale_y_continuous("BrAC (mg%)") +
  theme(legend.position = c(.6, .2), legend.text = element_text(size = 12), legend.title = element_text(size = 13, face="bold"))
Time.SA.Bin.PCA.Alc.plot


#Sex differences
SRSA.BrAC.SA.Bin.Poly4.Female.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                            Female +
                                            Female:Time.SA.Bin + Female:Time.SA.Bin.2 + Female:Time.SA.Bin.3 + + Female:Time.SA.Bin.4,
                                          random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                          control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                          correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.Female.Full)
#                         Value Std.Error  DF  t-value p-value
# (Intercept)          69.50810 1.0945263 623 63.50519  0.0000
# Time.SA.Bin          12.95512 1.8311091 623  7.07501  0.0000
# Time.SA.Bin.2        -3.02721 0.4539842 623 -6.66811  0.0000
# Time.SA.Bin.3         0.29140 0.0664277 623  4.38671  0.0000
# Time.SA.Bin.4        -0.01054 0.0034598 623 -3.04697  0.0024
# Female               -4.47715 1.6090985  65 -2.78240  0.0071
# Time.SA.Bin:Female   -4.92112 2.6911340 623 -1.82864  0.0679
# Time.SA.Bin.2:Female  1.44049 0.6655496 623  2.16436  0.0308
# Time.SA.Bin.3:Female -0.19249 0.0971105 623 -1.98215  0.0479
# Time.SA.Bin.4:Female  0.00944 0.0050463 623  1.86982  0.0620

#plotting Sex Efffects
PredValues <- data.frame(Female=c(rep(0, 11),rep(1, 11)),
                         Gender=c(rep("Male", 11),rep("Female", 11)),
                         Time.SA.Bin=rep(seq(0,10,1), 2))
PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
PredValues$BrAC <- SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["(Intercept)"] +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin"] * PredValues$Time.SA.Bin +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Female"] * PredValues$Female +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin:Female"] * PredValues$Time.SA.Bin * PredValues$Female +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.2:Female"] * PredValues$Time.SA.Bin.2 * PredValues$Female +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.3:Female"] * PredValues$Time.SA.Bin.3 * PredValues$Female +
  SRSA.BrAC.SA.Bin.Poly4.Female.Full$coef$fixed["Time.SA.Bin.4:Female"] * PredValues$Time.SA.Bin.4 * PredValues$Female
PredValues

TenMinBrAC$Gender <- ifelse(TenMinBrAC$Female==0,"Male","Female")
colorscale <- scales::seq_gradient_pal("lightgreen", "darkgreen", "Lab")(seq(0,1,length.out=2))
Time.SA.Bin.Female.plot.raw <- ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, colour=Gender)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), size = 2, se=F) +
  Sp.Theme(legend.position="right") + scale_colour_manual("Gender", values=colorscale) +
  ggtitle("BAC Curve Sex Differences") + 
  scale_x_continuous("Self-Administration Time (min)") + scale_y_continuous("BAC (mg%)")+
  theme(legend.position = c(.9, .2), legend.text = element_text(size = 12), legend.title = element_text(size = 13, face="bold"))
Time.SA.Bin.Female.plot.raw

#PCA.Alc Effect survives Female covariation
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.Sx <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                PCA.Alc +
                                                Female +
                                                Female:Time.SA.Bin + Female:Time.SA.Bin.2 + Female:Time.SA.Bin.3 + Female:Time.SA.Bin.4 +
                                                PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                              random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                              control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                              correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.Sx)
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           69.39359 1.1276038 620 61.54075  0.0000
# Time.SA.Bin           12.23701 1.8580873 620  6.58581  0.0000
# Time.SA.Bin.2         -2.87593 0.4577208 620 -6.28314  0.0000
# Time.SA.Bin.3          0.28127 0.0666108 620  4.22255  0.0000
# Time.SA.Bin.4         -0.01041 0.0034567 620 -3.01107  0.0027
# PCA.Alc                0.17180 0.3809613  64  0.45097  0.6535
# Female                -4.32292 1.6509884  64 -2.61838  0.0110
# Time.SA.Bin:Female    -3.95122 2.7199807 620 -1.45266  0.1468
# Time.SA.Bin.2:Female   1.23520 0.6698592 620  1.84397  0.0657
# Time.SA.Bin.3:Female  -0.17857 0.0973570 620 -1.83420  0.0671
# Time.SA.Bin.4:Female   0.00924 0.0050422 620  1.83289  0.0673
# Time.SA.Bin:PCA.Alc    1.05891 0.6204524 620  1.70667  0.0884
# Time.SA.Bin.2:PCA.Alc -0.21269 0.1023913 620 -2.07719  0.0382
# Time.SA.Bin.3:PCA.Alc  0.01207 0.0058447 620  2.06504  0.0393


#BDI effects
SRSA.BrAC.SA.Bin.Poly4.BDI.sq.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                            BDI.sq +
                                            BDI.sq:Time.SA.Bin + BDI.sq:Time.SA.Bin.2 + BDI.sq:Time.SA.Bin.3 + BDI.sq:Time.SA.Bin.4,
                                          random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                          control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                          correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.BDI.sq.Full)
#                        Value Std.Error  DF  t-value p-value
# (Intercept)          66.44276 1.5741798 623 42.20786  0.0000
# Time.SA.Bin           8.76898 2.5215638 623  3.47760  0.0005
# Time.SA.Bin.2        -1.90068 0.6233583 623 -3.04910  0.0024
# Time.SA.Bin.3         0.15987 0.0908691 623  1.75932  0.0790
# Time.SA.Bin.4        -0.00506 0.0046906 623 -1.07953  0.2808
# BDI.sq                0.39933 0.5350299  65  0.74637  0.4581
# Time.SA.Bin:BDI.sq    0.75341 0.8579089 623  0.87819  0.3802
# Time.SA.Bin.2:BDI.sq -0.17500 0.2138646 623 -0.81826  0.4135
# Time.SA.Bin.3:BDI.sq  0.01504 0.0314745 623  0.47776  0.6330
# Time.SA.Bin.4:BDI.sq -0.00032 0.0016361 623 -0.19628  0.8445

SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                            BDI.sq,
                                          random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                          control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                          correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim)
#Trimmed effects from subsequent models
#                        Value Std.Error  DF  t-value p-value
#Time.SA.Bin.4:BDI.sq -0.00032 0.0016361 623 -0.19628  0.8445
#Time.SA.Bin.3:BDI.sq  0.00905 0.0077249 624  1.17207  0.2416
#Time.SA.Bin.2:BDI.sq  0.01049 0.0407532 625  0.25731  0.7970
#Time.SA.Bin:BDI.sq    0.09008 0.1558046 626  0.57816  0.5634
#BDI.sq                0.18854 0.4657124  65  0.40484  0.6869

#No BDI effect on BAC Curves 
#plotting Sex Efffects
PredValues <- data.frame(BDI.sq=c(rep(0.912158, 11),rep(2.490518, 11),rep(4.068878,11)),
                         BDI=c(rep("0.83", 11),rep("6.20", 11),rep("16.56",11)),
                         Time.SA.Bin=rep(seq(0,10,1), 3))
PredValues$BDI <- factor(PredValues$BDI, levels=c("0.83", "6.20", "16.56"))
PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
PredValues$BrAC <- SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["(Intercept)"] +
  SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["Time.SA.Bin"] * PredValues$Time.SA.Bin +
  SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
  SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
  SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
  SRSA.BrAC.SA.Bin.Poly4.BDI.sq.trim$coef$fixed["BDI.sq"] * PredValues$BDI.sq
PredValues

colorscale <- scales::seq_gradient_pal("purple1", "purple4", "Lab")(seq(0,1,length.out=3))
Time.SA.Bin.BDI.sq.plot <- ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, colour=as.factor(BDI))) +
  #geom_point(size=4) + #used only to verify stat_Smooth 
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), size = 2, se=F) +
  Sp.Theme(legend.position="right") + scale_colour_manual("BDI-II", values=colorscale) +
  ggtitle("BrAC Curve by Depressive Symptomatology") + 
  scale_x_continuous("Self-Administration Time (min)") + scale_y_continuous("BrAC (mg%)")+
  theme(legend.position = c(.6, .2), legend.text = element_text(size = 12), legend.title = element_text(size = 13, face="bold"))
Time.SA.Bin.BDI.sq.plot

#Age effects
SRSA.BrAC.SA.Bin.Poly4.Age.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                         Age +
                                         Age:Time.SA.Bin + Age:Time.SA.Bin.2 + Age:Time.SA.Bin.3 + Age:Time.SA.Bin.4,
                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.Age.Full)
# #                     Value Std.Error  DF   t-value p-value
# (Intercept)       67.08255  3.880160 623 17.288604  0.0000
# Time.SA.Bin       -2.62024  6.022121 623 -0.435102  0.6636
# Time.SA.Bin.2      0.14840  1.512168 623  0.098137  0.9219
# Time.SA.Bin.3     -0.03338  0.220568 623 -0.151331  0.8798
# Time.SA.Bin.4      0.00211  0.011366 623  0.185462  0.8529
# Age                0.01216  0.129773  65  0.093671  0.9257
# Time.SA.Bin:Age    0.45475  0.201394 623  2.258012  0.0243
# Time.SA.Bin.2:Age -0.08525  0.050534 623 -1.687057  0.0921
# Time.SA.Bin.3:Age  0.00793  0.007365 623  1.076256  0.2822
# Time.SA.Bin.4:Age -0.00027  0.000379 623 -0.723808  0.4695

SRSA.BrAC.SA.Bin.Poly4.Age.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                         Age + Time.SA.Bin:Age,
                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.Age.trim)
#Trimmed effects
#Time.SA.Bin.4:Age -0.00027  0.000379 623 -0.723808  0.4695
#Time.SA.Bin.3:Age  0.00277  0.001824 624  1.516416  0.1299
#Time.SA.Bin.2:Age -0.01022  0.009450 625 -1.081251  0.2800

#plotting Sex Efffects
PredValues <- data.frame(Age=c(rep(22.61, 11),rep(29.18, 11), rep(35.75,11)),
                         Time.SA.Bin=rep(seq(0,10,1), 3))
PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
PredValues$BrAC <- SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["(Intercept)"] +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Time.SA.Bin"] * PredValues$Time.SA.Bin +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Age"] * PredValues$Age +
  SRSA.BrAC.SA.Bin.Poly4.Age.trim$coef$fixed["Time.SA.Bin:Age"] * PredValues$Time.SA.Bin * PredValues$Age
PredValues

colorscale <- scales::seq_gradient_pal("tomato1", "firebrick3", "Lab")(seq(0,1,length.out=3))
Time.SA.Bin.Age.plot <- ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, colour=as.factor(Age))) +
  #geom_point(size=4) + #used only to verify stat_Smooth 
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), size = 2, se=F) +
  Sp.Theme(legend.position="right") + scale_colour_manual("Age", values=colorscale) +
  ggtitle("BrAC Curve Age Differences") + 
  scale_x_continuous("Self-Administration Time (min)") + scale_y_continuous("BrAC (mg%)")+
  theme(legend.position = c(.6, .2), legend.text = element_text(size = 12), legend.title = element_text(size = 13, face="bold"))
Time.SA.Bin.Age.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.Age <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                 PCA.Alc +
                                                 Age + Time.SA.Bin:Age +
                                                 PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                               random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                               control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                               correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.Age)
#                         Value Std.Error  DF   t-value p-value
# (Intercept)           69.90569  3.584658 623 19.501354  0.0000
# Time.SA.Bin            8.08655  1.727808 623  4.680238  0.0000
# Time.SA.Bin.2         -2.28199  0.332951 623 -6.853824  0.0000
# Time.SA.Bin.3          0.19476  0.048913 623  3.981689  0.0001
# Time.SA.Bin.4         -0.00590  0.002533 623 -2.330016  0.0201
# PCA.Alc                0.50134  0.411090  64  1.219526  0.2271
# Age                   -0.08891  0.120569  64 -0.737450  0.4635
# Time.SA.Bin:Age        0.07831  0.038397 623  2.039448  0.0418
# Time.SA.Bin:PCA.Alc    1.10805  0.600993 623  1.843706  0.0657
# Time.SA.Bin.2:PCA.Alc -0.22915  0.098595 623 -2.324168  0.0204
# Time.SA.Bin.3:PCA.Alc  0.01257  0.005631 623  2.232762  0.0259


#CPD effects
SRSA.BrAC.SA.Bin.Poly4.CPD.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                         CPD +
                                         CPD:Time.SA.Bin + CPD:Time.SA.Bin.2 + CPD:Time.SA.Bin.3 + CPD:Time.SA.Bin.4,
                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                       control=list(opt="optim",maxIter=1000, msMaxIter=1000), method="ML", na.action=na.exclude,
                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.CPD.Full)
#                    Value Std.Error  DF  t-value p-value
# (Intercept)       67.26560 0.9114774 623 73.79843  0.0000
# Time.SA.Bin       10.81977 1.4584854 623  7.41850  0.0000
# Time.SA.Bin.2     -2.69704 0.3617963 623 -7.45457  0.0000
# Time.SA.Bin.3      0.25906 0.0529068 623  4.89646  0.0000
# Time.SA.Bin.4     -0.00891 0.0027502 623 -3.23918  0.0013
# CPD                0.09288 0.1954917  65  0.47511  0.6363
# Time.SA.Bin:CPD   -0.09062 0.3133508 623 -0.28918  0.7725
# Time.SA.Bin.2:CPD  0.19185 0.0788190 623  2.43407  0.0152
# Time.SA.Bin.3:CPD -0.03264 0.0117076 623 -2.78821  0.0055
# Time.SA.Bin.4:CPD  0.00160 0.0006149 623  2.59768  0.0096

#plotting Sex Efffects
PredValues <- data.frame(CPD=c(rep(0, 11),rep(5, 11),rep(10, 11)),
                         Time.SA.Bin=rep(seq(0,10,1), 3))
PredValues$Time.SA.Bin.2 <- PredValues$Time.SA.Bin^2
PredValues$Time.SA.Bin.3 <- PredValues$Time.SA.Bin^3
PredValues$Time.SA.Bin.4 <- PredValues$Time.SA.Bin^4
PredValues$BrAC <- SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["(Intercept)"] +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin"] * PredValues$Time.SA.Bin +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.2"] * PredValues$Time.SA.Bin.2 +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.3"] * PredValues$Time.SA.Bin.3 +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.4"] * PredValues$Time.SA.Bin.4 +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["CPD"] * PredValues$CPD +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin:CPD"] * PredValues$Time.SA.Bin * PredValues$CPD +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.2:CPD"] * PredValues$Time.SA.Bin.2 * PredValues$CPD +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.3:CPD"] * PredValues$Time.SA.Bin.3 * PredValues$CPD +
  SRSA.BrAC.SA.Bin.Poly4.CPD.Full$coef$fixed["Time.SA.Bin.4:CPD"] * PredValues$Time.SA.Bin.4 * PredValues$CPD
PredValues

colorscale <- scales::seq_gradient_pal("grey", "black", "Lab")(seq(0,1,length.out=3))
Time.SA.Bin.CPD.plot <- ggplot(data=PredValues, aes(x=(10*Time.SA.Bin + 5), y=BrAC, colour=as.factor(CPD))) +
  #geom_point(size=4) + #used only to verify stat_Smooth 
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), size = 2, se=F) +
  Sp.Theme(legend.position="right") + scale_colour_manual("Cigarettes per Day", values=colorscale) +
  ggtitle("BrAC Curves by Cigarette Smoking") + 
  scale_x_continuous("Self-Administration Time (min)") + scale_y_continuous("BrAC (mg%)")+
  theme(legend.position = c(.6, .2), legend.text = element_text(size = 12), legend.title = element_text(size = 13, face="bold"))
Time.SA.Bin.CPD.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.CPD <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                 PCA.Alc +
                                                 CPD +
                                                 CPD:Time.SA.Bin + CPD:Time.SA.Bin.2 + CPD:Time.SA.Bin.3 + CPD:Time.SA.Bin.4 +
                                                 PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                               random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                               control=list(opt="optim",maxIter=5000, msMaxIter=1000), method="ML", na.action=na.exclude,
                                               correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.trim.CPD)
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           67.26841 0.9090974 620 73.99472  0.0000
# Time.SA.Bin           10.82843 1.4387147 620  7.52646  0.0000
# Time.SA.Bin.2         -2.69899 0.3590483 620 -7.51705  0.0000
# Time.SA.Bin.3          0.25916 0.0527833 620  4.90983  0.0000
# Time.SA.Bin.4         -0.00890 0.0027389 620 -3.25103  0.0012
# PCA.Alc                0.42231 0.4023126  64  1.04971  0.2978
# CPD                    0.03394 0.2029069  64  0.16727  0.8677
# Time.SA.Bin:CPD       -0.26493 0.3213221 620 -0.82449  0.4100
# Time.SA.Bin.2:CPD      0.23078 0.0796751 620  2.89649  0.0039
# Time.SA.Bin.3:CPD     -0.03506 0.0117329 620 -2.98793  0.0029
# Time.SA.Bin.4:CPD      0.00161 0.0006129 620  2.62556  0.0089
# Time.SA.Bin:PCA.Alc    1.24043 0.6275643 620  1.97659  0.0485
# Time.SA.Bin.2:PCA.Alc -0.27251 0.1025088 620 -2.65842  0.0081
# Time.SA.Bin.3:PCA.Alc  0.01596 0.0058454 620  2.72982  0.0065


#Craving and Self-Administration----
#EB estimates of AUQZ at BrAC = 0.04, and linear slope
#Center trial at .04 timepoint
table(Challenge$trial)
Challenge$trial.04 <- Challenge$trial-2
table(Challenge$trial.04)

Challenge.MLM.AUQZ.04 <- lme(AUQZ~trial.04, 
                        data=Challenge, na.action=na.exclude,
                        random = ~trial.04|Subject,
                        control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.AUQZ.04)
#                 Value  Std.Error  DF  t-value p-value
#(Intercept) 0.09814754 0.11345270 200 0.865097   0.388
#trial.04    0.19629508 0.03420146 200 5.739378   0.000
Challenge.AUQZ.04 <- data.frame(Subject=as.numeric(row.names(data.frame(coef(Challenge.MLM.AUQZ.04)))),
                           AUQZ.04=data.frame(coef(Challenge.MLM.AUQZ.04))$X.Intercept,
                           AUQZ.Slp=data.frame(coef(Challenge.MLM.AUQZ.04))$trial)
Challenge.AUQZ.04
ggplot(data=Challenge.AUQZ.04, aes(x=AUQZ.04, y=AUQZ.Slp)) + geom_point() + Sp.Theme()
cor(Challenge.AUQZ.04[c("AUQZ.04", "AUQZ.Slp")])
#0.5178713

AUQZ.R2 <- cor(Challenge.AUQZ.04[c("AUQZ.04", "AUQZ.Slp")])[1,2]^2
AUQZ.EB.plot <- ggplot(data=Challenge.AUQZ.04, aes(x=AUQZ.04, y=AUQZ.Slp)) + geom_point() + geom_smooth(method=lm, se=F) + 
  ggtitle("D. Alcohol Craving") + scale_x_continuous("Alcohol Craving Level") + scale_y_continuous("Alcohol Craving Slope") + 
  annotate("text", x=2, y=-.1, label=paste("R^2 == ", round(AUQZ.R2,3)), parse=T) + Sp.Theme()
AUQZ.EB.plot

#Merge AUQZ indices
TenMinBrAC <- merge(TenMinBrAC, Challenge.AUQZ.04, by="Subject")

#AUQZ.04 -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                     PCA.Alc +
                                                     AUQZ.04 +
                                                     PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                     AUQZ.04:Time.SA.Bin + AUQZ.04:Time.SA.Bin.2 + AUQZ.04:Time.SA.Bin.3 + AUQZ.04:Time.SA.Bin.4 +
                                                     PCA.Alc:AUQZ.04 + 
                                                     PCA.Alc:AUQZ.04:Time.SA.Bin + PCA.Alc:AUQZ.04:Time.SA.Bin.2 + PCA.Alc:AUQZ.04:Time.SA.Bin.3 + PCA.Alc:AUQZ.04:Time.SA.Bin.4,
                                                   random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                   control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                   correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.Full)
#                                 Value Std.Error  DF  t-value p-value
# (Intercept)                   67.26739 0.8063511 615 83.42197  0.0000
# Time.SA.Bin                   10.79640 1.3532170 615  7.97832  0.0000
# Time.SA.Bin.2                 -2.37242 0.3497204 615 -6.78376  0.0000
# Time.SA.Bin.3                  0.20579 0.0517476 615  3.97688  0.0001
# Time.SA.Bin.4                 -0.00639 0.0026704 615 -2.39138  0.0171
# PCA.Alc                       -0.26472 0.4223056  63 -0.62684  0.5330
# AUQZ.04                        4.05352 0.9863817  63  4.10949  0.0001
# Time.SA.Bin:PCA.Alc            0.91617 0.7095501 615  1.29119  0.1971
# Time.SA.Bin.2:PCA.Alc         -0.25974 0.1849556 615 -1.40434  0.1607
# Time.SA.Bin.3:PCA.Alc          0.02853 0.0276230 615  1.03298  0.3020
# Time.SA.Bin.4:PCA.Alc         -0.00107 0.0014374 615 -0.74665  0.4556
# Time.SA.Bin:AUQZ.04            5.67487 1.6569683 615  3.42485  0.0007
# Time.SA.Bin.2:AUQZ.04         -1.28357 0.4313181 615 -2.97593  0.0030
# Time.SA.Bin.3:AUQZ.04          0.13163 0.0643245 615  2.04638  0.0411
# Time.SA.Bin.4:AUQZ.04         -0.00500 0.0033354 615 -1.50010  0.1341
# PCA.Alc:AUQZ.04               -0.19672 0.3920264  63 -0.50179  0.6176
# Time.SA.Bin:PCA.Alc:AUQZ.04   -1.15356 0.6596677 615 -1.74870  0.0808
# Time.SA.Bin.2:PCA.Alc:AUQZ.04  0.28296 0.1738015 615  1.62806  0.1040
# Time.SA.Bin.3:PCA.Alc:AUQZ.04 -0.03593 0.0262442 615 -1.36916  0.1714
# Time.SA.Bin.4:PCA.Alc:AUQZ.04  0.00164 0.0013773 615  1.19158  0.2339

BrAC.Bin.AUQZ.04.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.Full, Severity="PCA.Alc", 
                                            SR="AUQZ.04", SRName = "Alcohol Craving Level", Poly=4)
BrAC.Bin.AUQZ.04.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                     PCA.Alc +
                                                     AUQZ.04 +
                                                     AUQZ.04:Time.SA.Bin + AUQZ.04:Time.SA.Bin.2 + AUQZ.04:Time.SA.Bin.3,
                                                   random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                   control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                   correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.trim)
#Trimming process
#Time.SA.Bin.4:PCA.Alc:AUQZ.04  0.00164 0.0013773 615  1.19158  0.2339
#Time.SA.Bin.3:PCA.Alc:AUQZ.04 -0.00551 0.0060262 616 -0.91370  0.3612
#Time.SA.Bin.2:PCA.Alc:AUQZ.04  0.02698 0.0325325 617  0.82920  0.4073
#Time.SA.Bin.4:PCA.Alc       -0.00052 0.0013586 618 -0.38247  0.7022 #Very different trimming
#Time.SA.Bin.3:PCA.Alc        0.00617 0.0060147 619  1.02605  0.3053
#Time.SA.Bin.2:PCA.Alc        0.00028 0.0325734 620  0.00866  0.9931
#Time.SA.Bin:PCA.Alc:AUQZ.04 -0.17700 0.1231964 621 -1.43669  0.1513
#PCA.Alc:AUQZ.04       -0.15697 0.3597622  63 -0.43632  0.6641
#Time.SA.Bin:PCA.Alc   -0.08761 0.1265509 622 -0.69226  0.4890
#Time.SA.Bin.4:AUQZ.04 -0.00438 0.0029122 623 -1.50552  0.1327

#Final Model
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           67.13920 0.7601545 624 88.32310  0.0000
# Time.SA.Bin           10.17002 1.2835381 624  7.92342  0.0000
# Time.SA.Bin.2         -2.26283 0.3290642 624 -6.87658  0.0000
# Time.SA.Bin.3          0.19466 0.0486614 624  4.00021  0.0001
# Time.SA.Bin.4         -0.00595 0.0025180 624 -2.36479  0.0183
# PCA.Alc               -0.48611 0.3596459  64 -1.35164  0.1812
# AUQZ.04                4.27902 0.9370059  64  4.56669  0.0000
# Time.SA.Bin:AUQZ.04    4.93710 1.4194329 624  3.47822  0.0005
# Time.SA.Bin.2:AUQZ.04 -0.82064 0.2332809 624 -3.51781  0.0005
# Time.SA.Bin.3:AUQZ.04  0.04184 0.0133068 624  3.14416  0.0017

BrAC.Bin.AUQZ.04.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.04.trim, Severity="PCA.Alc", 
                                            SR="AUQZ.04", SRName = "Craving Level", Poly=4)
BrAC.Bin.AUQZ.04.trim.plot


#AUQZ.Slp -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                      PCA.Alc +
                                                      AUQZ.Slp +
                                                      PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                      AUQZ.Slp:Time.SA.Bin + AUQZ.Slp:Time.SA.Bin.2 + AUQZ.Slp:Time.SA.Bin.3 + AUQZ.Slp:Time.SA.Bin.4 +
                                                      PCA.Alc:AUQZ.Slp + 
                                                      PCA.Alc:AUQZ.Slp:Time.SA.Bin + PCA.Alc:AUQZ.Slp:Time.SA.Bin.2 + PCA.Alc:AUQZ.Slp:Time.SA.Bin.3 + PCA.Alc:AUQZ.Slp:Time.SA.Bin.4,
                                                    random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                    control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                    correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.Full)
#                                  Value Std.Error  DF  t-value p-value
# (Intercept)                    66.15449  1.305440 615 50.67602  0.0000
# Time.SA.Bin                     8.29861  2.040003 615  4.06794  0.0001
# Time.SA.Bin.2                  -2.44660  0.523685 615 -4.67189  0.0000
# Time.SA.Bin.3                   0.26372  0.077829 615  3.38850  0.0007
# Time.SA.Bin.4                  -0.00974  0.004058 615 -2.39909  0.0167
# PCA.Alc                         0.57054  0.698029  63  0.81736  0.4168
# AUQZ.Slp                        6.30335  5.193846  63  1.21362  0.2294
# Time.SA.Bin:PCA.Alc             1.73335  1.092651 615  1.58637  0.1132
# Time.SA.Bin.2:PCA.Alc          -0.67850  0.284018 615 -2.38892  0.0172
# Time.SA.Bin.3:PCA.Alc           0.09171  0.042776 615  2.14386  0.0324
# Time.SA.Bin.4:PCA.Alc          -0.00402  0.002255 615 -1.78172  0.0753
# Time.SA.Bin:AUQZ.Slp           10.51313  8.116360 615  1.29530  0.1957
# Time.SA.Bin.2:AUQZ.Slp          0.90910  2.083469 615  0.43634  0.6627
# Time.SA.Bin.3:AUQZ.Slp         -0.36987  0.309698 615 -1.19430  0.2328
# Time.SA.Bin.4:AUQZ.Slp          0.02064  0.016138 615  1.27869  0.2015
# PCA.Alc:AUQZ.Slp               -1.12987  2.389949  63 -0.47276  0.6380
# Time.SA.Bin:PCA.Alc:AUQZ.Slp   -1.99330  3.744315 615 -0.53235  0.5947
# Time.SA.Bin.2:PCA.Alc:AUQZ.Slp  1.34767  0.979743 615  1.37553  0.1695
# Time.SA.Bin.3:PCA.Alc:AUQZ.Slp -0.22626  0.148662 615 -1.52199  0.1285
# Time.SA.Bin.4:PCA.Alc:AUQZ.Slp  0.01115  0.007879 615  1.41516  0.1575

BrAC.Bin.AUQZ.Slp.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.Full, Severity="PCA.Alc", 
                                             SR="AUQZ.Slp", SRName = "Alcohol Craving Slope", Poly=4)
BrAC.Bin.AUQZ.Slp.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                      PCA.Alc +
                                                      AUQZ.Slp +
                                                      PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 +
                                                      AUQZ.Slp:Time.SA.Bin + AUQZ.Slp:Time.SA.Bin.2,
                                                    random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                    control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                    correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.trim)
#Trimming process
#Time.SA.Bin.4:PCA.Alc:AUQZ.Slp  0.01115  0.007879 615  1.41516  0.1575
#Time.SA.Bin.3:PCA.Alc:AUQZ.Slp -0.02180  0.034970 616 -0.62346  0.5332
#Time.SA.Bin.2:PCA.Alc:AUQZ.Slp -0.10173  0.184585 617 -0.55111  0.5818
#Time.SA.Bin:PCA.Alc:AUQZ.Slp   -0.20690  0.719130 618 -0.28770  0.7737
#PCA.Alc:AUQZ.Slp       -1.30994  2.112818  63 -0.62000  0.5375
#Time.SA.Bin.4:PCA.Alc  -0.00135  0.001249 619 -1.08117  0.2800
#Time.SA.Bin.4:AUQZ.Slp  0.02266  0.016028 620  1.41363  0.1580
#Time.SA.Bin.3:AUQZ.Slp  0.00885  0.073443 621  0.12053  0.9041

#Final Model
#                          Value Std.Error  DF  t-value p-value
# (Intercept)            66.46768  1.226963 622 54.17251  0.0000
# Time.SA.Bin             7.97816  1.627730 622  4.90140  0.0000
# Time.SA.Bin.2          -2.09216  0.340821 622 -6.13858  0.0000
# Time.SA.Bin.3           0.19381  0.048884 622  3.96471  0.0001
# Time.SA.Bin.4          -0.00586  0.002530 622 -2.31840  0.0208
# PCA.Alc                 0.36944  0.387392  64  0.95365  0.3438
# AUQZ.Slp                4.46780  4.610616  64  0.96903  0.3362
# Time.SA.Bin:PCA.Alc     1.09080  0.587847 622  1.85558  0.0640
# Time.SA.Bin.2:PCA.Alc  -0.21859  0.097181 622 -2.24935  0.0248
# Time.SA.Bin.3:PCA.Alc   0.01231  0.005588 622  2.20376  0.0279
# Time.SA.Bin:AUQZ.Slp   12.15523  5.028531 622  2.41725  0.0159
# Time.SA.Bin.2:AUQZ.Slp -0.94552  0.388266 622 -2.43523  0.0152

BrAC.Bin.AUQZ.Slp.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.AUQZ.Slp.trim, Severity="PCA.Alc", 
                                             SR="AUQZ.Slp", SRName = "Craving Slope", Poly=4)
BrAC.Bin.AUQZ.Slp.trim.plot


#Positive Reinforcement----
Challenge.MLM.Stim.Comb.04 <- lme(Stim.Comb~trial.04, 
                             data=Challenge, na.action=na.exclude,
                             random = ~trial.04|Subject,
                             control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.Stim.Comb.04)
#                Value Std.Error  DF  t-value p-value
# (Intercept) 0.1409438 0.3167481 200 0.4449711  0.6568
# trial.04    0.2818875 0.1099158 200 2.5645774  0.0111
Challenge.Stim.Comb.04 <- data.frame(Subject=as.numeric(row.names(data.frame(coef(Challenge.MLM.Stim.Comb.04)))),
                                Stim.Comb.04=data.frame(coef(Challenge.MLM.Stim.Comb.04))$X.Intercept,
                                Stim.Comb.Slp=data.frame(coef(Challenge.MLM.Stim.Comb.04))$trial)
Challenge.Stim.Comb.04
cor(Challenge.Stim.Comb.04[c("Stim.Comb.04", "Stim.Comb.Slp")])
#0.6135265
cor.test(Challenge.Stim.Comb.04$Stim.Comb.04, Challenge.Stim.Comb.04$Stim.Comb.Slp)

Stim.R2 <- cor(Challenge.Stim.Comb.04[c("Stim.Comb.04", "Stim.Comb.Slp")])[1,2]^2
Stim.EB.plot <- ggplot(data=Challenge.Stim.Comb.04, aes(x=Stim.Comb.04, y=Stim.Comb.Slp)) + geom_point() + geom_smooth(method=lm, se=F) + 
  ggtitle("A. Stimulation") + scale_x_continuous("Stimulation Level") + scale_y_continuous("Stimulation Slope") + 
  annotate("text", x=3.5, y=-.7, label=paste("R^2 == ", round(Stim.R2,3)), parse=T) + Sp.Theme()
Stim.EB.plot

#Merge Stim.Comb indices
TenMinBrAC <- merge(TenMinBrAC, Challenge.Stim.Comb.04, by="Subject")


#Stim.Comb.04 -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                          PCA.Alc +
                                                          Stim.Comb.04 +
                                                          PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                          Stim.Comb.04:Time.SA.Bin + Stim.Comb.04:Time.SA.Bin.2 + Stim.Comb.04:Time.SA.Bin.3 + Stim.Comb.04:Time.SA.Bin.4 +
                                                          PCA.Alc:Stim.Comb.04 + 
                                                          PCA.Alc:Stim.Comb.04:Time.SA.Bin + PCA.Alc:Stim.Comb.04:Time.SA.Bin.2 + PCA.Alc:Stim.Comb.04:Time.SA.Bin.3 + PCA.Alc:Stim.Comb.04:Time.SA.Bin.4,
                                                        random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                        control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                        correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.Full)
# #                                      Value Std.Error  DF  t-value p-value
# (Intercept)                        67.30070 0.8482763 615 79.33818  0.0000
# Time.SA.Bin                        10.31706 1.3472091 615  7.65810  0.0000
# Time.SA.Bin.2                      -2.26206 0.3372505 615 -6.70737  0.0000
# Time.SA.Bin.3                       0.19057 0.0496542 615  3.83785  0.0001
# Time.SA.Bin.4                      -0.00566 0.0025743 615 -2.19718  0.0284
# PCA.Alc                             0.36621 0.3959185  63  0.92497  0.3585
# Stim.Comb.04                        0.26466 0.3470207  63  0.76265  0.4485
# Time.SA.Bin:PCA.Alc                 1.32737 0.6300823 615  2.10667  0.0356
# Time.SA.Bin.2:PCA.Alc              -0.33757 0.1602713 615 -2.10624  0.0356
# Time.SA.Bin.3:PCA.Alc               0.03280 0.0240069 615  1.36644  0.1723
# Time.SA.Bin.4:PCA.Alc              -0.00109 0.0012624 615 -0.86142  0.3893
# Time.SA.Bin:Stim.Comb.04           -0.06686 0.5505611 615 -0.12144  0.9034
# Time.SA.Bin.2:Stim.Comb.04          0.10937 0.1366908 615  0.80012  0.4239
# Time.SA.Bin.3:Stim.Comb.04         -0.01914 0.0199385 615 -0.96008  0.3374
# Time.SA.Bin.4:Stim.Comb.04          0.00097 0.0010250 615  0.94758  0.3437
# PCA.Alc:Stim.Comb.04               -0.04103 0.2087342  63 -0.19657  0.8448
# Time.SA.Bin:PCA.Alc:Stim.Comb.04   -0.01727 0.3312738 615 -0.05213  0.9584
# Time.SA.Bin.2:PCA.Alc:Stim.Comb.04  0.02183 0.0824607 615  0.26479  0.7913
# Time.SA.Bin.3:PCA.Alc:Stim.Comb.04 -0.00531 0.0120648 615 -0.44036  0.6598
# Time.SA.Bin.4:PCA.Alc:Stim.Comb.04  0.00032 0.0006229 615  0.51877  0.6041

BrAC.Bin.Stim.Comb.04.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.Full, Severity="PCA.Alc", 
                                                 SR="Stim.Comb.04", SRName = "Stimulation Level", Poly=4)
BrAC.Bin.Stim.Comb.04.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                          PCA.Alc +
                                                          Stim.Comb.04 +
                                                          PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                        random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                        control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                        correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.trim)
#Trimming procedure
#Time.SA.Bin.4:PCA.Alc:Stim.Comb.04  0.00032 0.0006229 615  0.51877  0.6041
#Time.SA.Bin.3:PCA.Alc:Stim.Comb.04  0.00076 0.0029626 616  0.25488  0.7989
#Time.SA.Bin.2:PCA.Alc:Stim.Comb.04  0.00154 0.0158456 617  0.09739  0.9224
#Time.SA.Bin:PCA.Alc:Stim.Comb.04   -0.02138 0.0604540 618 -0.35374  0.7237  #All PCA.Alc x Stim.Comb.04 x Time interactions NS (p >= 0.604)
#PCA.Alc:Stim.Comb.04       -0.08216 0.1821282  63 -0.45110  0.6535 #PCA.Alc x Stim.Comb.04 not an overall moderator of BAC averaged across time
#Time.SA.Bin.4:Stim.Comb.04  0.00093 0.0010190 619  0.91634  0.3598 
#Time.SA.Bin.3:Stim.Comb.04 -0.00091 0.0048697 620 -0.18673  0.8519
#Time.SA.Bin.2:Stim.Comb.04 -0.00505 0.0259533 621 -0.19459  0.8458
#Time.SA.Bin:Stim.Comb.04    0.06477 0.1011107 622  0.64054  0.5221 #All Stim.Comb.04 x Time interactions NS (p >= 0.360)

#Time.SA.Bin.4:PCA.Alc -0.00114 0.0012474 623 -0.91657  0.3597

#Final Trimmed model
# Value Std.Error  DF  t-value p-value
# (Intercept)           67.30040 0.8371559 624 80.39171  0.0000
# Time.SA.Bin           10.35137 1.3333765 624  7.76327  0.0000
# Time.SA.Bin.2         -2.28347 0.3326066 624 -6.86539  0.0000
# Time.SA.Bin.3          0.19515 0.0488527 624  3.99457  0.0001
# Time.SA.Bin.4         -0.00593 0.0025306 624 -2.34384  0.0194
# PCA.Alc                0.41001 0.3849386  64  1.06514  0.2908
# Stim.Comb.04           0.23971 0.3010928  64  0.79613  0.4289 #Main effect of Stim NS 
# Time.SA.Bin:PCA.Alc    1.19689 0.6036520 624  1.98274  0.0478
# Time.SA.Bin.2:PCA.Alc -0.23055 0.0986033 624 -2.33813  0.0197
# Time.SA.Bin.3:PCA.Alc  0.01273 0.0056320 624  2.26000  0.0242 #PCA.Alc effect on trajectories unaffected by controlling for Stim.Comb.04


BrAC.Bin.Stim.Comb.04.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.04.trim, Severity="PCA.Alc", 
                                                 SR="Stim.Comb.04", SRName = "Stimulation Level", Poly=4)
BrAC.Bin.Stim.Comb.04.trim.plot


#Stim.Comb.Slp -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                           PCA.Alc +
                                                           Stim.Comb.Slp +
                                                           PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                           Stim.Comb.Slp:Time.SA.Bin + Stim.Comb.Slp:Time.SA.Bin.2 + Stim.Comb.Slp:Time.SA.Bin.3 + Stim.Comb.Slp:Time.SA.Bin.4 +
                                                           PCA.Alc:Stim.Comb.Slp + 
                                                           PCA.Alc:Stim.Comb.Slp:Time.SA.Bin + PCA.Alc:Stim.Comb.Slp:Time.SA.Bin.2 + PCA.Alc:Stim.Comb.Slp:Time.SA.Bin.3 + PCA.Alc:Stim.Comb.Slp:Time.SA.Bin.4,
                                                         random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                         control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                         correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.Full)
#                                       Value Std.Error  DF  t-value p-value
# (Intercept)                         66.80915 0.9358134 615 71.39153  0.0000
# Time.SA.Bin                         10.65445 1.4730756 615  7.23279  0.0000
# Time.SA.Bin.2                       -2.51857 0.3682434 615 -6.83941  0.0000
# Time.SA.Bin.3                        0.22869 0.0541219 615  4.22551  0.0000
# Time.SA.Bin.4                       -0.00733 0.0028012 615 -2.61778  0.0091
# PCA.Alc                              0.36984 0.4274509  63  0.86523  0.3902
# Stim.Comb.Slp                        1.91409 1.4099342  63  1.35757  0.1794
# Time.SA.Bin:PCA.Alc                  1.06504 0.6751429 615  1.57750  0.1152
# Time.SA.Bin.2:PCA.Alc               -0.28891 0.1731931 615 -1.66816  0.0958
# Time.SA.Bin.3:PCA.Alc                0.03050 0.0261529 615  1.16633  0.2439
# Time.SA.Bin.4:PCA.Alc               -0.00112 0.0013829 615 -0.80724  0.4198
# Time.SA.Bin:Stim.Comb.Slp           -1.15647 2.2211324 615 -0.52067  0.6028
# Time.SA.Bin.2:Stim.Comb.Slp          0.91870 0.5586810 615  1.64441  0.1006
# Time.SA.Bin.3:Stim.Comb.Slp         -0.13508 0.0826810 615 -1.63373  0.1028
# Time.SA.Bin.4:Stim.Comb.Slp          0.00583 0.0043031 615  1.35518  0.1759
# PCA.Alc:Stim.Comb.Slp               -0.07413 0.5950070  63 -0.12458  0.9013
# Time.SA.Bin:PCA.Alc:Stim.Comb.Slp    0.96467 0.9410931 615  1.02506  0.3057
# Time.SA.Bin.2:PCA.Alc:Stim.Comb.Slp -0.19404 0.2439393 615 -0.79546  0.4267
# Time.SA.Bin.3:PCA.Alc:Stim.Comb.Slp  0.01157 0.0372255 615  0.31085  0.7560
# Time.SA.Bin.4:PCA.Alc:Stim.Comb.Slp -0.00009 0.0019828 615 -0.04595  0.9634


BrAC.Bin.Stim.Comb.Slp.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.Full, Severity="PCA.Alc", 
                                                  SR="Stim.Comb.Slp", SRName = "Stimulation Slope", Poly=4)
BrAC.Bin.Stim.Comb.Slp.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                           PCA.Alc +
                                                           Stim.Comb.Slp +
                                                           PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                         random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                         control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                         correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.trim)
#Trimming procedure
#Time.SA.Bin.4:PCA.Alc:Stim.Comb.Slp -0.00009 0.0019828 615 -0.04595  0.9634
#Time.SA.Bin.3:PCA.Alc:Stim.Comb.Slp  0.00991 0.0085567 616  1.15806  0.2473
#Time.SA.Bin.2:PCA.Alc:Stim.Comb.Slp -0.02112 0.0465884 617 -0.45325  0.6505
#Time.SA.Bin:PCA.Alc:Stim.Comb.Slp   -0.10041 0.1730931 618 -0.58007  0.5621 #All 3-way interactions NS (p >= 0.247)
#PCA.Alc:Stim.Comb.Slp               -0.42859 0.5196032  63 -0.82484  0.4126 #PCA.Alc x Stim.Comb.Slp not an overall predictor of BAC
#Time.SA.Bin.4:PCA.Alc       -0.00114 0.0012436 619 -0.91472  0.3607 #Drop PCA.Alc x trial4 first 
#Time.SA.Bin.4:Stim.Comb.Slp  0.00579 0.0042562 620  1.36028  0.1742
#Time.SA.Bin.3:Stim.Comb.Slp -0.02216 0.0196079 621 -1.13035  0.2588
#Time.SA.Bin.2:Stim.Comb.Slp -0.12244 0.1053970 622 -1.16171  0.2458
#Time.SA.Bin:Stim.Comb.Slp    0.28800 0.4001962 623  0.71965  0.4720 #All Stim.Comb.Slp interactions with trial NS (p >= 0.174)

#Final trim model
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           66.74950 0.9049711 624 73.75871  0.0000
# Time.SA.Bin           10.35358 1.3321093 624  7.77232  0.0000
# Time.SA.Bin.2         -2.28537 0.3319632 624 -6.88440  0.0000
# Time.SA.Bin.3          0.19556 0.0487329 624  4.01299  0.0001
# Time.SA.Bin.4         -0.00596 0.0025236 624 -2.36005  0.0186
# PCA.Alc                0.40461 0.3856932  64  1.04904  0.2981
# Stim.Comb.Slp          2.07879 1.2059704  64  1.72375  0.0896 #Trend to a positive effect of stim slope on BAC overall
# Time.SA.Bin:PCA.Alc    1.19669 0.6031021 624  1.98423  0.0477
# Time.SA.Bin.2:PCA.Alc -0.23025 0.0983957 624 -2.34007  0.0196
# Time.SA.Bin.3:PCA.Alc  0.01271 0.0056091 624  2.26511  0.0238

BrAC.Bin.Stim.Comb.Slp.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Stim.Comb.Slp.trim, Severity="PCA.Alc", 
                                                  SR="Stim.Comb.Slp", SRName = "Stimulation Slope", Poly=4)
BrAC.Bin.Stim.Comb.Slp.trim.plot



#Negative Reinforcement----
#EB estimates of NA.Comb at BrAC = 0.04, and linear slope
#Check centering of trial at .04 timepoint
table(Challenge$trial.04)

Challenge.MLM.NA.Comb.04 <- lme(NA.Comb~trial.04, 
                           data=Challenge, na.action=na.exclude,
                           random = ~trial.04|Subject,
                           control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.NA.Comb.04)
#                 Value  Std.Error  DF   t-value p-value
# (Intercept) -0.1275457 0.19701025 200 -0.647407  0.5181
# trial.04    -0.2550914 0.06279709 200 -4.062154  0.0001
Challenge.NA.Comb.04 <- data.frame(Subject=as.numeric(row.names(data.frame(coef(Challenge.MLM.NA.Comb.04)))),
                              NA.Comb.04=data.frame(coef(Challenge.MLM.NA.Comb.04))$X.Intercept,
                              NA.Comb.Slp=data.frame(coef(Challenge.MLM.NA.Comb.04))$trial)
Challenge.NA.Comb.04
ggplot(data=Challenge.NA.Comb.04, aes(x=NA.Comb.04, y=NA.Comb.Slp)) + geom_point() + Sp.Theme()
cor(Challenge.NA.Comb.04[c("NA.Comb.04", "NA.Comb.Slp")])
#-0.3334876
cor.test(Challenge.NA.Comb.04$NA.Comb.04, Challenge.NA.Comb.04$NA.Comb.Slp)


NA.R2 <- cor(Challenge.NA.Comb.04[c("NA.Comb.04", "NA.Comb.Slp")])[1,2]^2
NA.EB.plot <- ggplot(data=Challenge.NA.Comb.04, aes(x=NA.Comb.04, y=NA.Comb.Slp)) + geom_point() + geom_smooth(method=lm, se=F) + 
  ggtitle("C. Negative Affect") + scale_x_continuous("Negative Affect Level") + scale_y_continuous("Negative Affect Slope") + 
  annotate("text", x=5, y=-1.3, label=paste("R^2 == ", round(NA.R2,3)), parse=T) + Sp.Theme()
NA.EB.plot

#Merge NA.Comb indices
TenMinBrAC <- merge(TenMinBrAC, Challenge.NA.Comb.04, by="Subject")

#NA.Comb.04 -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                        PCA.Alc +
                                                        NA.Comb.04 +
                                                        PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                        NA.Comb.04:Time.SA.Bin + NA.Comb.04:Time.SA.Bin.2 + NA.Comb.04:Time.SA.Bin.3 + NA.Comb.04:Time.SA.Bin.4 +
                                                        PCA.Alc:NA.Comb.04 + 
                                                        PCA.Alc:NA.Comb.04:Time.SA.Bin + PCA.Alc:NA.Comb.04:Time.SA.Bin.2 + PCA.Alc:NA.Comb.04:Time.SA.Bin.3 + PCA.Alc:NA.Comb.04:Time.SA.Bin.4,
                                                      random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                      control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                      correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.Full)
#                                    Value Std.Error  DF  t-value p-value
# (Intercept)                      67.63678 0.9254360 615 73.08639  0.0000
# Time.SA.Bin                      10.77644 1.4728629 615  7.31666  0.0000
# Time.SA.Bin.2                    -2.41810 0.3732475 615 -6.47854  0.0000
# Time.SA.Bin.3                     0.21201 0.0556167 615  3.81200  0.0002
# Time.SA.Bin.4                    -0.00666 0.0028989 615 -2.29721  0.0219
# PCA.Alc                           0.37002 0.4178853  63  0.88545  0.3793
# NA.Comb.04                        0.08073 0.7425081  63  0.10873  0.9138
# Time.SA.Bin:PCA.Alc               1.32880 0.6665294 615  1.99361  0.0466
# Time.SA.Bin.2:PCA.Alc            -0.32481 0.1716709 615 -1.89207  0.0590
# Time.SA.Bin.3:PCA.Alc             0.02974 0.0260041 615  1.14374  0.2532
# Time.SA.Bin.4:PCA.Alc            -0.00091 0.0013754 615 -0.65849  0.5105
# Time.SA.Bin:NA.Comb.04            0.27718 1.1859320 615  0.23372  0.8153
# Time.SA.Bin.2:NA.Comb.04         -0.15412 0.3089727 615 -0.49880  0.6181
# Time.SA.Bin.3:NA.Comb.04          0.02519 0.0474117 615  0.53131  0.5954
# Time.SA.Bin.4:NA.Comb.04         -0.00128 0.0025171 615 -0.50823  0.6115
# PCA.Alc:NA.Comb.04               -0.27997 0.2974098  63 -0.94135  0.3501
# Time.SA.Bin:PCA.Alc:NA.Comb.04   -0.41683 0.4747885 615 -0.87792  0.3803
# Time.SA.Bin.2:PCA.Alc:NA.Comb.04  0.13510 0.1231796 615  1.09678  0.2732
# Time.SA.Bin.3:PCA.Alc:NA.Comb.04 -0.01721 0.0188113 615 -0.91510  0.3605
# Time.SA.Bin.4:PCA.Alc:NA.Comb.04  0.00075 0.0009989 615  0.75441  0.4509

BrAC.Bin.NA.Comb.04.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.Full, Severity="PCA.Alc", 
                                               SR="NA.Comb.04", SRName = "Negative Affect Level", Poly=4)
BrAC.Bin.NA.Comb.04.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                        PCA.Alc +
                                                        NA.Comb.04 +
                                                        PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3,
                                                      random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                      control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                      correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.trim)
#Trimming process
#Time.SA.Bin.4:PCA.Alc:NA.Comb.04  0.00075 0.0009989 615  0.75441  0.4509
#Time.SA.Bin.3:PCA.Alc:NA.Comb.04 -0.00341 0.0043387 616 -0.78624  0.4320
#Time.SA.Bin.2:PCA.Alc:NA.Comb.04  0.00544 0.0238138 617  0.22863  0.8192
#Time.SA.Bin:PCA.Alc:NA.Comb.04    0.00912 0.0906704 618  0.10054  0.9199
#Time.SA.Bin.4:NA.Comb.04 -0.00001 0.0018052 619 -0.00802  0.9936 #Order slightly different
#Time.SA.Bin.3:NA.Comb.04 -0.00315 0.0084785 620 -0.37111  0.7107
#Time.SA.Bin.2:NA.Comb.04  0.00759 0.0453468 621  0.16727  0.8672
#Time.SA.Bin:NA.Comb.04   -0.00775 0.1789270 622 -0.04330  0.9655
#PCA.Alc:NA.Comb.04    -0.20436 0.2618541  63 -0.78043  0.4381
#Time.SA.Bin.4:PCA.Alc -0.00114 0.0012486 623 -0.91204  0.3621

#Final Model
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           67.29421 0.8442630 624 79.70763  0.0000
# Time.SA.Bin           10.35031 1.3331584 624  7.76375  0.0000
# Time.SA.Bin.2         -2.28265 0.3327391 624 -6.86019  0.0000
# Time.SA.Bin.3          0.19497 0.0488825 624  3.98861  0.0001
# Time.SA.Bin.4         -0.00592 0.0025319 624 -2.33845  0.0197
# PCA.Alc                0.45156 0.4040889  64  1.11749  0.2680
# NA.Comb.04            -0.23184 0.5212576  64 -0.44477  0.6580
# Time.SA.Bin:PCA.Alc    1.19680 0.6034841 624  1.98316  0.0478
# Time.SA.Bin.2:PCA.Alc -0.23060 0.0985518 624 -2.33991  0.0196
# Time.SA.Bin.3:PCA.Alc  0.01274 0.0056289 624  2.26249  0.0240

BrAC.Bin.NA.Comb.04.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.04.trim, Severity="PCA.Alc", 
                                               SR="NA.Comb.04", SRName = "Negative Affect Level", Poly=4)
BrAC.Bin.NA.Comb.04.trim.plot

#NA.Comb.Slp -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                         PCA.Alc +
                                                         NA.Comb.Slp +
                                                         PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                         NA.Comb.Slp:Time.SA.Bin + NA.Comb.Slp:Time.SA.Bin.2 + NA.Comb.Slp:Time.SA.Bin.3 + NA.Comb.Slp:Time.SA.Bin.4 +
                                                         PCA.Alc:NA.Comb.Slp + 
                                                         PCA.Alc:NA.Comb.Slp:Time.SA.Bin + PCA.Alc:NA.Comb.Slp:Time.SA.Bin.2 + PCA.Alc:NA.Comb.Slp:Time.SA.Bin.3 + PCA.Alc:NA.Comb.Slp:Time.SA.Bin.4,
                                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.Full)
#                                     Value Std.Error  DF  t-value p-value
# (Intercept)                       66.05772  1.239139 615 53.30939  0.0000
# Time.SA.Bin                       10.13481  1.991060 615  5.09016  0.0000
# Time.SA.Bin.2                     -2.35425  0.499173 615 -4.71631  0.0000
# Time.SA.Bin.3                      0.21107  0.073541 615  2.87012  0.0042
# Time.SA.Bin.4                     -0.00666  0.003817 615 -1.74604  0.0813
# PCA.Alc                            0.97447  0.559212  63  1.74257  0.0863
# NA.Comb.Slp                       -4.86803  3.514974  63 -1.38494  0.1710
# Time.SA.Bin:PCA.Alc                1.24182  0.900571 615  1.37892  0.1684
# Time.SA.Bin.2:PCA.Alc             -0.35934  0.229740 615 -1.56414  0.1183
# Time.SA.Bin.3:PCA.Alc              0.03654  0.034485 615  1.05974  0.2897
# Time.SA.Bin.4:PCA.Alc             -0.00126  0.001816 615 -0.69470  0.4875
# Time.SA.Bin:NA.Comb.Slp           -0.81026  5.662719 615 -0.14309  0.8863
# Time.SA.Bin.2:NA.Comb.Slp         -0.33453  1.448841 615 -0.23089  0.8175
# Time.SA.Bin.3:NA.Comb.Slp          0.07149  0.218141 615  0.32774  0.7432
# Time.SA.Bin.4:NA.Comb.Slp         -0.00327  0.011500 615 -0.28410  0.7764
# PCA.Alc:NA.Comb.Slp                3.27414  2.145685  63  1.52592  0.1320
# Time.SA.Bin:PCA.Alc:NA.Comb.Slp   -0.55006  3.448246 615 -0.15952  0.8733
# Time.SA.Bin.2:PCA.Alc:NA.Comb.Slp -0.09027  0.865640 615 -0.10428  0.9170
# Time.SA.Bin.3:PCA.Alc:NA.Comb.Slp  0.01634  0.127731 615  0.12789  0.8983
# Time.SA.Bin.4:PCA.Alc:NA.Comb.Slp -0.00071  0.006638 615 -0.10636  0.9153

BrAC.Bin.NA.Comb.Slp.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.Full, Severity="PCA.Alc", 
                                                SR="NA.Comb.Slp", SRName = "Negative Affect Level", Poly=4)
BrAC.Bin.NA.Comb.Slp.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                         PCA.Alc +
                                                         NA.Comb.Slp +
                                                         PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 +
                                                         PCA.Alc:NA.Comb.Slp,
                                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.trim)
#Trimming procedure
#Time.SA.Bin.4:PCA.Alc:NA.Comb.Slp -0.00071  0.006638 615 -0.10636  0.9153
#Time.SA.Bin.3:PCA.Alc:NA.Comb.Slp  0.00317  0.030926 616  0.10235  0.9185
#Time.SA.Bin.2:PCA.Alc:NA.Comb.Slp  0.03425  0.164474 617  0.20823  0.8351
#Time.SA.Bin:PCA.Alc:NA.Comb.Slp   -0.46891  0.627433 618 -0.74734  0.4551 #No 3-way interactions
#Time.SA.Bin.4:NA.Comb.Slp -0.00389  0.009896 619 -0.39317  0.6943
#Time.SA.Bin.3:NA.Comb.Slp  0.01349  0.044803 620  0.30101  0.7635
#Time.SA.Bin.2:NA.Comb.Slp  0.20017 0.2401054 621  0.83369  0.4048
#Time.SA.Bin:NA.Comb.Slp   -0.31236 0.9148504 622 -0.34143  0.7329 #No main effects of NAslope on Curve parameters
#Time.SA.Bin.4:PCA.Alc -0.00115 0.0012473 623 -0.92503  0.3553

#Final Model
#                         Value Std.Error  DF  t-value p-value
# (Intercept)           66.10867 1.1564374 624 57.16580  0.0000
# Time.SA.Bin           10.36112 1.3334266 624  7.77030  0.0000
# Time.SA.Bin.2         -2.29114 0.3326103 624 -6.88836  0.0000
# Time.SA.Bin.3          0.19678 0.0488484 624  4.02835  0.0001
# Time.SA.Bin.4         -0.00603 0.0025305 624 -2.38288  0.0175
# PCA.Alc                1.00908 0.5201597  63  1.93994  0.0569
# NA.Comb.Slp           -4.62278 3.0894728  63 -1.49630  0.1396
# Time.SA.Bin:PCA.Alc    1.19709 0.6037261 624  1.98284  0.0478
# Time.SA.Bin.2:PCA.Alc -0.23039 0.0986243 624 -2.33608  0.0198
# Time.SA.Bin.3:PCA.Alc  0.01271 0.0056311 624  2.25699  0.0244
# PCA.Alc:NA.Comb.Slp    3.17927 1.8778849  63  1.69300  0.0954 #trend (barely) of Alcohol Use Severity x NA Slope

BrAC.Bin.NA.Comb.Slp.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.NA.Comb.Slp.trim, Severity="PCA.Alc", 
                                                SR="NA.Comb.Slp", SRName = "Negative Affect Slope", Poly=4)
BrAC.Bin.NA.Comb.Slp.trim.plot


#Sedation----
#EB estimates of Sed.Comb at BrAC = 0.04, and linear slope
#Check centering of trial at .04 timepoint
table(Challenge$trial.04)

Challenge.MLM.Sed.Comb.04 <- lme(Sed.Comb~trial.04, 
                            data=Challenge, na.action=na.exclude,
                            random = ~trial.04|Subject,
                            control=list(opt="optim",maxIter=400))
summary(Challenge.MLM.Sed.Comb.04)
#               Value  Std.Error  DF  t-value p-value
# (Intercept) 0.254293 0.19257291 200 1.320502  0.1882
# trial.04    0.508586 0.06794332 200 7.485445  0.0000
Challenge.Sed.Comb.04 <- data.frame(Subject=as.numeric(row.names(data.frame(coef(Challenge.MLM.Sed.Comb.04)))),
                               Sed.Comb.04=data.frame(coef(Challenge.MLM.Sed.Comb.04))$X.Intercept,
                               Sed.Comb.Slp=data.frame(coef(Challenge.MLM.Sed.Comb.04))$trial)
Challenge.Sed.Comb.04
ggplot(data=Challenge.Sed.Comb.04, aes(x=Sed.Comb.04, y=Sed.Comb.Slp)) + geom_point() + Sp.Theme()
cor(Challenge.Sed.Comb.04[c("Sed.Comb.04", "Sed.Comb.Slp")])
#0.6829473
cor.test(Challenge.Sed.Comb.04$Sed.Comb.04, Challenge.Sed.Comb.04$Sed.Comb.Slp)

Sed.R2 <- cor(Challenge.Sed.Comb.04[c("Sed.Comb.04", "Sed.Comb.Slp")])[1,2]^2
Sed.EB.plot <- ggplot(data=Challenge.Sed.Comb.04, aes(x=Sed.Comb.04, y=Sed.Comb.Slp)) + geom_point() + geom_smooth(method=lm, se=F) + 
  ggtitle("B. Sedation") + scale_x_continuous("Sedation Level") + scale_y_continuous("Sedation Slope") + 
  annotate("text", x=3, y=-.05, label=paste("R^2 == ", round(Sed.R2,3)), parse=T) + Sp.Theme()
Sed.EB.plot


#Merge Sed.Comb indices
TenMinBrAC <- merge(TenMinBrAC, Challenge.Sed.Comb.04, by="Subject")

#Sed.Comb.04 -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                         PCA.Alc +
                                                         Sed.Comb.04 +
                                                         PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                         Sed.Comb.04:Time.SA.Bin + Sed.Comb.04:Time.SA.Bin.2 + Sed.Comb.04:Time.SA.Bin.3 + Sed.Comb.04:Time.SA.Bin.4 +
                                                         PCA.Alc:Sed.Comb.04 + 
                                                         PCA.Alc:Sed.Comb.04:Time.SA.Bin + PCA.Alc:Sed.Comb.04:Time.SA.Bin.2 + PCA.Alc:Sed.Comb.04:Time.SA.Bin.3 + PCA.Alc:Sed.Comb.04:Time.SA.Bin.4,
                                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.Full)
#                                     Value Std.Error  DF  t-value p-value
# (Intercept)                       67.35170 0.9019699 615 74.67178  0.0000
# Time.SA.Bin                       11.08837 1.3680351 615  8.10532  0.0000
# Time.SA.Bin.2                     -2.47657 0.3478026 615 -7.12062  0.0000
# Time.SA.Bin.3                      0.21613 0.0513233 615  4.21115  0.0000
# Time.SA.Bin.4                     -0.00674 0.0026425 615 -2.55090  0.0110
# PCA.Alc                            0.34599 0.4659376  63  0.74257  0.4605
# Sed.Comb.04                       -0.03626 0.6021246  63 -0.06022  0.9522
# Time.SA.Bin:PCA.Alc                1.91684 0.7069484 615  2.71143  0.0069
# Time.SA.Bin.2:PCA.Alc             -0.50356 0.1802213 615 -2.79410  0.0054
# Time.SA.Bin.3:PCA.Alc              0.05136 0.0266759 615  1.92546  0.0546
# Time.SA.Bin.4:PCA.Alc             -0.00180 0.0013779 615 -1.30987  0.1907
# Time.SA.Bin:Sed.Comb.04           -2.38553 0.9126445 615 -2.61387  0.0092
# Time.SA.Bin.2:Sed.Comb.04          0.48825 0.2308634 615  2.11488  0.0348
# Time.SA.Bin.3:Sed.Comb.04         -0.03673 0.0338805 615 -1.08417  0.2787
# Time.SA.Bin.4:Sed.Comb.04          0.00073 0.0017361 615  0.41970  0.6748
# PCA.Alc:Sed.Comb.04                0.00973 0.3927883  63  0.02477  0.9803
# Time.SA.Bin:PCA.Alc:Sed.Comb.04   -0.38924 0.5954961 615 -0.65363  0.5136
# Time.SA.Bin.2:PCA.Alc:Sed.Comb.04  0.17005 0.1509206 615  1.12678  0.2603
# Time.SA.Bin.3:PCA.Alc:Sed.Comb.04 -0.02600 0.0221962 615 -1.17132  0.2419
# Time.SA.Bin.4:PCA.Alc:Sed.Comb.04  0.00130 0.0011400 615  1.13660  0.2561

BrAC.Bin.Sed.Comb.04.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.Full, Severity="PCA.Alc", 
                                                SR="Sed.Comb.04", SRName = "Sedation Level", Poly=4)
BrAC.Bin.Sed.Comb.04.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                         PCA.Alc +
                                                         Sed.Comb.04 +
                                                         PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 +
                                                         Sed.Comb.04:Time.SA.Bin + Sed.Comb.04:Time.SA.Bin.2 + Sed.Comb.04:Time.SA.Bin.3,
                                                       random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                       control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                       correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.trim)
#Trimming process
#Time.SA.Bin.4:PCA.Alc:Sed.Comb.04  0.00130 0.0011400 615  1.13660  0.2561
#Time.SA.Bin.3:PCA.Alc:Sed.Comb.04 -0.00148 0.0052169 616 -0.28274  0.7775
#Time.SA.Bin.2:PCA.Alc:Sed.Comb.04  0.00974 0.0287409 617  0.33890  0.7348
#Time.SA.Bin:PCA.Alc:Sed.Comb.04    0.02828 0.1112796 618  0.25411  0.7995 #All 3-way interactions NS
#PCA.Alc:Sed.Comb.04        0.01226 0.3432264  63  0.03572  0.9716 #Alcohol Use Severity x Sedation Level not a predictor
#Time.SA.Bin.4:Sed.Comb.04  0.00072 0.0017320 619  0.41394  0.6791
#Time.SA.Bin.4:PCA.Alc     -0.00114 0.0012386 620 -0.91851  0.3587

#Final Trimmed Model
# (Intercept)               67.35303 0.8606718 621 78.25635  0.0000
# Time.SA.Bin               10.85613 1.3047373 621  8.32055  0.0000
# Time.SA.Bin.2             -2.38307 0.3305289 621 -7.20987  0.0000
# Time.SA.Bin.3              0.20260 0.0487465 621  4.15619  0.0000
# Time.SA.Bin.4             -0.00609 0.0025154 621 -2.42245  0.0157
# PCA.Alc                    0.41078 0.4032516  64  1.01867  0.3122
# Sed.Comb.04               -0.07591 0.5923647  64 -0.12815  0.8984
# Time.SA.Bin:PCA.Alc        1.55290 0.5976160 621  2.59849  0.0096
# Time.SA.Bin.2:PCA.Alc     -0.29455 0.0969356 621 -3.03863  0.0025
# Time.SA.Bin.3:PCA.Alc      0.01629 0.0055438 621  2.93870  0.0034
# Time.SA.Bin:Sed.Comb.04   -2.28165 0.8791289 621 -2.59536  0.0097
# Time.SA.Bin.2:Sed.Comb.04  0.41023 0.1415627 621  2.89788  0.0039
# Time.SA.Bin.3:Sed.Comb.04 -0.02281 0.0079562 621 -2.86664  0.0043

BrAC.Bin.Sed.Comb.04.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.04.trim, Severity="PCA.Alc", 
                                                SR="Sed.Comb.04", SRName = "Sedation Level", Poly=4)
BrAC.Bin.Sed.Comb.04.trim.plot

#Sed.Comb.Slp -- BrAC Curve Analyses
SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.Full <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                          PCA.Alc +
                                                          Sed.Comb.Slp +
                                                          PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 + PCA.Alc:Time.SA.Bin.4 +
                                                          Sed.Comb.Slp:Time.SA.Bin + Sed.Comb.Slp:Time.SA.Bin.2 + Sed.Comb.Slp:Time.SA.Bin.3 + Sed.Comb.Slp:Time.SA.Bin.4 +
                                                          PCA.Alc:Sed.Comb.Slp + 
                                                          PCA.Alc:Sed.Comb.Slp:Time.SA.Bin + PCA.Alc:Sed.Comb.Slp:Time.SA.Bin.2 + PCA.Alc:Sed.Comb.Slp:Time.SA.Bin.3 + PCA.Alc:Sed.Comb.Slp:Time.SA.Bin.4,
                                                        random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                        control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                        correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.Full)
#                                      Value Std.Error  DF  t-value p-value
# (Intercept)                        67.93297  1.506156 615 45.10354  0.0000
# Time.SA.Bin                        14.63410  2.319812 615  6.30831  0.0000
# Time.SA.Bin.2                      -3.31948  0.582431 615 -5.69935  0.0000
# Time.SA.Bin.3                       0.28755  0.085262 615  3.37254  0.0008
# Time.SA.Bin.4                      -0.00838  0.004363 615 -1.92069  0.0552
# PCA.Alc                             0.85336  1.030967  63  0.82772  0.4110
# Sed.Comb.Slp                       -1.19208  2.445275  63 -0.48750  0.6276
# Time.SA.Bin:PCA.Alc                 2.67300  1.588140 615  1.68310  0.0929
# Time.SA.Bin.2:PCA.Alc              -0.68262  0.399132 615 -1.71027  0.0877
# Time.SA.Bin.3:PCA.Alc               0.06897  0.058487 615  1.17918  0.2388
# Time.SA.Bin.4:PCA.Alc              -0.00250  0.002995 615 -0.83575  0.4036
# Time.SA.Bin:Sed.Comb.Slp           -8.59957  3.764527 615 -2.28437  0.0227
# Time.SA.Bin.2:Sed.Comb.Slp          2.11888  0.941746 615  2.24995  0.0248
# Time.SA.Bin.3:Sed.Comb.Slp         -0.19444  0.137297 615 -1.41621  0.1572
# Time.SA.Bin.4:Sed.Comb.Slp          0.00546  0.007003 615  0.77996  0.4357
# PCA.Alc:Sed.Comb.Slp               -0.93171  1.733444  63 -0.53749  0.5928
# Time.SA.Bin:PCA.Alc:Sed.Comb.Slp   -2.50113  2.671145 615 -0.93635  0.3495
# Time.SA.Bin.2:PCA.Alc:Sed.Comb.Slp  0.63768  0.673020 615  0.94749  0.3438
# Time.SA.Bin.3:PCA.Alc:Sed.Comb.Slp -0.06682  0.098901 615 -0.67562  0.4995
# Time.SA.Bin.4:PCA.Alc:Sed.Comb.Slp  0.00261  0.005078 615  0.51398  0.6075

BrAC.Bin.Sed.Comb.Slp.Full.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.Full, Severity="PCA.Alc", 
                                                 SR="Sed.Comb.Slp", SRName = "Sedation Slope", Poly=4)
BrAC.Bin.Sed.Comb.Slp.Full.plot

SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.trim <- lme(BrAC.M ~ Time.SA.Bin + Time.SA.Bin.2 + Time.SA.Bin.3 + Time.SA.Bin.4 + 
                                                          PCA.Alc +
                                                          Sed.Comb.Slp +
                                                          PCA.Alc:Time.SA.Bin + PCA.Alc:Time.SA.Bin.2 + PCA.Alc:Time.SA.Bin.3 +
                                                          Sed.Comb.Slp:Time.SA.Bin + Sed.Comb.Slp:Time.SA.Bin.2 + Sed.Comb.Slp:Time.SA.Bin.3,
                                                        random=~Time.SA.Bin+Time.SA.Bin.2+Time.SA.Bin.3+Time.SA.Bin.4|Subject, data=TenMinBrAC,
                                                        control=list(opt="optim",maxIter=400), method="ML", na.action=na.exclude,
                                                        correlation=corAR1())
summary(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.trim)
#Trimming process
#Time.SA.Bin.4:PCA.Alc:Sed.Comb.Slp  0.00261  0.005078 615  0.51398  0.6075
#Time.SA.Bin.3:PCA.Alc:Sed.Comb.Slp -0.01742  0.023307 616 -0.74744  0.4551
#Time.SA.Bin.2:PCA.Alc:Sed.Comb.Slp  0.07124  0.130134 617  0.54747  0.5843
#Time.SA.Bin:PCA.Alc:Sed.Comb.Slp    0.18631  0.498446 618  0.37378  0.7087 #All 3-ways NS
#PCA.Alc:Sed.Comb.Slp       -0.35925  1.522497  63 -0.23596  0.8142
#Time.SA.Bin.4:Sed.Comb.Slp  0.00542  0.006979 619  0.77615  0.4380
#Time.SA.Bin.4:PCA.Alc      -0.00114  0.001240 620 -0.92113  0.3573

#Final Trimmed Model
# (Intercept)                68.07687  1.489845 621 45.69391  0.0000
# Time.SA.Bin                14.31044  2.267717 621  6.31050  0.0000
# Time.SA.Bin.2              -3.05854  0.443824 621 -6.89133  0.0000
# Time.SA.Bin.3               0.24044  0.051509 621  4.66786  0.0000
# Time.SA.Bin.4              -0.00591  0.002516 621 -2.35026  0.0191
# PCA.Alc                     0.39300  0.389410  64  1.00921  0.3167
# Sed.Comb.Slp               -1.45190  2.407672  64 -0.60303  0.5486
# Time.SA.Bin:PCA.Alc         1.16049  0.587732 621  1.97453  0.0488
# Time.SA.Bin.2:PCA.Alc      -0.22341  0.094926 621 -2.35355  0.0189
# Time.SA.Bin.3:PCA.Alc       0.01230  0.005422 621  2.26921  0.0236
# Time.SA.Bin:Sed.Comb.Slp   -7.77069  3.640532 621 -2.13449  0.0332
# Time.SA.Bin.2:Sed.Comb.Slp  1.52331  0.582494 621  2.61516  0.0091
# Time.SA.Bin.3:Sed.Comb.Slp -0.08938  0.032547 621 -2.74630  0.0062

BrAC.Bin.Sed.Comb.Slp.trim.plot <- BrAC.Bin.plot(SRSA.BrAC.SA.Bin.Poly4.PCA.Alc.Sed.Comb.Slp.trim, Severity="PCA.Alc", 
                                                 SR="Sed.Comb.Slp", SRName = "Sedation Slope", Poly=4)
BrAC.Bin.Sed.Comb.Slp.trim.plot

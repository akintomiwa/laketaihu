# laketaihu
Data Analysis for Drag coefficient analysis in Lake Taihu, P.R China
#COMBINE SEPARATE YEAR CD FOR 2012-2018 TIME SERIES##
rm(list = ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(TTR)
library(forecast)
library(zoo)
library(reshape2)
library(ggpmisc)

home<-"/Users/akinonigbinde/Desktop/NUIST/Thesis/"
setwd(home)
bfg12<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2012N.csv", header = FALSE)
#bfg12$V3<- (sqrt(bfg12$V1)/100)
bfg13<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2013N.csv",header = FALSE)
#bfg13$V3<- (sqrt(bfg13$V1)/100)
bfg14<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2014N.csv",header = FALSE)
#bfg14$V3<- (sqrt(bfg14$V1)/100)
bfg15<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2015N.csv",header = FALSE)
#bfg15$V3<- (sqrt(bfg15$V1)/100)
bfg16<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2016N.csv",header = FALSE)
#bfg16$V3<- (sqrt(bfg16$V1)/100)
bfg17<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2017N.csv",header = FALSE)
#bfg17$V3<- (sqrt(bfg17$V1)/100)
bfg18<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFG2018N.csv",header = FALSE)
#bfg18$V3<- (sqrt(bfg18$V1)/100)

bfgal<-rbind.data.frame(bfg12,bfg13,bfg14,bfg15,bfg16,bfg17,bfg18)
bfgal<-as_data_frame(bfgal)
bfgal$V3<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
bfgal$V3<-as.POSIXct(bfgal$V3,format="%Y/%m/%d")

#WITH A PREBOUND BFG 2012-2018 CD INTO MASTER FILE
bfgal<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/bfg201218sub.csv", header = FALSE)
bfgal<-as_data_frame(bfgal)
bfgal$V3<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
bfgal$V3<-as.POSIXct(bfgal$V3,format="%Y/%m/%d")
##add month characteristics##
bfgal$V4<-substring(bfgal$V3,1,4) #year
bfgal$V5<-substring(bfgal$V3,6,7) #month
bfgal$V6<-substring(bfgal$V3,9,10) #day
colnames(bfgal)<-c("slope","number", "fulldate","year","month","day")

#Time series plot for BFG#
ggplot(bfgal, aes(x=bfgal$fulldate, y=bfgal$slope))+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/5))+ ylim(0.5,2)+
  labs(title="30 day Moving Average of CD in 2012-2018 at BFG",x="Date", y = "Drag Coefficient (CD) (*10^3)" )+
  theme_minimal()+
  # theme(panel.grid.minor.y=element_blank(),
  #         panel.grid.major.y=element_blank())
#coord_cartesian(ylim=c(2012, 2018)) + 
 # scale_y_continuous(breaks=seq(2012, 2018, 1))  # Ticks from 0-10, every .25

#WITH A PREBOUND PTS 2014-2018 CD INTO MASTER FILE
ptsal<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/pts201418.csv", header = FALSE)
ptsal<-as_data_frame(ptsal)
ptsal$V3<-seq(as.Date("2014/1/1"), as.Date("2018/12/31"), "days")
ptsal$V3<-as.POSIXct(ptsal$V3,format="%Y/%m/%d")
##add month characteristics##
ptsal$V4<-substring(ptsal$V3,1,4) #year
ptsal$V5<-substring(ptsal$V3,6,7) #month
ptsal$V6<-substring(ptsal$V3,9,10) #day
colnames(ptsal)<-c("number","slope", "fulldate","year","month","day")

#Time series plot for PTS#
ggplot(ptsal, aes(x=ptsal$fulldate, y=ptsal$slope))+
  geom_point(size=2, shape=21, fill="#CC0000",colour=alpha("black", 1/5)) + ylim(0.5,1.7)+
  labs(title="30 day Moving Average of CD in 2014-2018 at PTS",x="Date", y = "Drag Coefficient (CD) (*10^3)" )+
  # theme_minimal()+
  # theme(panel.grid.minor.y=element_blank(),
  #       panel.grid.major.y=element_blank())
  

#################################
#####Histograms of u wind########
#################################
##########Results 1 and 2########

whistbfg<-ggplot(bfginput, aes(bfginput$wind)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white")+
  theme_bw()+
  #labs(title="Distribution of wind speeds at BFG")+
  xlab("Wind Speed Bin (ms-1)")+
  ylab("Count")+
  xlim(2,8)
whistbfg + theme(axis.title.x = element_text(size=20),
                 axis.title.y = element_text(size=20),
                 axis.text.x  = element_text(vjust=0.5, size=15),
                 axis.text.y  = element_text(vjust=0.5, size=15))+
  coord_cartesian(xlim=c(2.5, 8)) +
  scale_x_continuous(breaks=seq(2, 8, 0.5))


whistpts<-ggplot(ptsinput, aes(ptsinput$wind)) +
  geom_histogram(binwidth=0.5, colour="black", fill="white")+
  theme_bw()+
  #labs(title="Distribution of wind speeds at PTS")+
  xlab("Wind Speed Bin (ms-1)")+
  ylab("Count")+
  xlim(2,8)
whistpts + theme(axis.title.x = element_text(size=20),
                 axis.title.y = element_text(size=20),
                 axis.text.x  = element_text(vjust=0.5, size=15),
                 axis.text.y  = element_text(vjust=0.5, size=15))+
  coord_cartesian(xlim=c(2.5, 8)) +
  scale_x_continuous(breaks=seq(2, 8, 0.5))

# summary(bfginput$wind)
# summary(ptsinput$wind)
# Summarize(bfginput$wind)
# 


############################################
### Joint CD Time series plot BFG and PTS###
############################################
bfgpts<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/bfgpts.csv", header = FALSE)
bfgpts<-as_data_frame(bfgpts)
bfgpts$V4<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
bfgpts$V4<-as.POSIXct(bfgpts$V4,format="%Y/%m/%d")
##add month characteristics##
bfgpts$V5<-substring(bfgpts$V4,1,4) #year
bfgpts$V6<-substring(bfgpts$V4,6,7) #month
bfgpts$V7<-substring(bfgpts$V4,9,10) #day
colnames(bfgpts)<-c("bfgcd","number","ptscd","fulldate","year","month","day")
# bfgpts$bfgcd<-as.numeric(bfgpts$bfgcd)
# bfgpts$ptscd<-as.numeric(bfgpts$ptscd)
summary(bfgpts$ptscd)


ggplot(bfgpts, aes(fulldate)) +
  geom_line(aes(y=bfgcd, colour = "BFG")) +
  geom_line(aes(y=ptscd, colour = "PTS")) +
  scale_colour_hue("Site")+
  labs(title="30 day Moving Average of CD at BFG and PTS",x="Date", y = "Drag Coefficient (CD) (*10^3)" ) 


#################################################
### Joint CD10N Time series plot BFG and PTS###
################################################
#####################Results 5####################
bfgpts10<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/bfgptscd10.csv", header = FALSE)
bfgpts10<-as_data_frame(bfgpts10)
bfgpts10$V4<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
bfgpts10$V4<-as.POSIXct(bfgpts10$V4,format="%Y/%m/%d")
##add month characteristics##
bfgpts10$V5<-substring(bfgpts10$V4,1,4) #year
bfgpts10$V6<-substring(bfgpts10$V4,6,7) #month
bfgpts10$V7<-substring(bfgpts10$V4,9,10) #day
#colnames(bfgpts10)<-c("cd","number","site","fulldate","year","month","day")
colnames(bfgpts10)<-c("bfg","number","pts","fulldate","year","month","day")
# bfgpts10$bfgcd<-as.numeric(bfgpts10$bfgcd)
# bfgpts10$ptscd<-as.numeric(bfgpts10$ptscd)
# summary(bfgpts10$ptscd)
# cdbfgpts<-ggplot(bfgpts10, aes(x=fulldate)) +
#           geom_line(aes(y=bfg, linetype="BFG"),size=.5)+
#           geom_line(aes(y=pts, linetype="PTS"),size=.5)+
#           scale_linetype_manual(values=c(7,11), name = "Site")+
#           theme_bw() +
#           #labs(title="30 day Moving Average of CD10N at BFG and PTS", x="Date", y = "Drag Coefficient (CD) (*10^3)" )+
#           xlab("DOY")+
#           ylab("Drag Coefficient (CD) (*10^3)")+
#           theme(axis.title.x = element_text(size=20),
#                  axis.title.y = element_text(size=20),
#                  axis.text.x  = element_text(vjust=0.5, size=15),
#                  axis.text.y  = element_text(vjust=0.5, size=15))  

cdbfgpts<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, colour="BFG"),size=.5)+
  geom_line(aes(y=pts, colour="PTS"),size=.5)+
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  #labs(title="30 day Moving Average of CD10N at BFG and PTS", x="Date", y = "Drag Coefficient (CD) (*10^3)" )+
  xlab("DOY")+
  ylab("Drag Coefficient (CD) (*10^3)")+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))  



########################
##Special double TS##
########################
############################
## CAN SAFELY BE IGNORED####
# #############################
# bfgcompare<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/3rd try/BFGfreshvssub.csv", header = FALSE)
# bfgcompare<-as_data_frame(bfgcompare)
# bfgcompare$V4<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
# bfgcompare$V4<-as.POSIXct(bfgcompare$V4,format="%Y/%m/%d")
# ##add month characteristics##
# bfgcompare$V5<-substring(bfgcompare$V4,1,4) #year
# bfgcompare$V6<-substring(bfgcompare$V4,6,7) #month
# bfgcompare$V7<-substring(bfgcompare$V4,9,10) #day
# colnames(bfgcompare)<-c("slopesub","slopefresh","number", "fulldate","year","month","day")
# 
# ggplot(bfgcompare, aes(x=bfgcompare$fulldate, y=bfgcompare$slopesub))+
#   geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/5))+ ylim(0.5,2)+
#   labs(title="30 day Moving Average of CD in 2012-2018 at BFG",x="Date", y = "Drag Coefficient (CD)" )+
#   theme_minimal()+
#   theme(panel.grid.minor.y=element_blank(),
#         panel.grid.major.y=element_blank())
# ##compare old and new TS###
# ggplot(bfgcompare, aes(x=bfgcompare$fulldate, y=value, color = variable))+
#   #geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/5))+ ylim(0.5,2)+
#   geom_point(aes(y = bfgcompare$slopesub, col = "new TS")) + 
#   geom_point(aes(y = bfgcompare$slopefresh, col = "old TS"))+
#   labs(title="30 day Moving Average of CD in 2012-2018 at BFG",x="Date", y = "Drag Coefficient (CD)" )+
#   theme_minimal()+
#   theme(panel.grid.minor.y=element_blank(),
#         panel.grid.major.y=element_blank())
# 
# ############################
# ## CAN SAFELY BE IGNORED####
##############################


######################################
####MONTHLY mean and error analysis###
######################################
###/summarize the data##
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

#forBFG#
stat1bfg <- ddply(bfgal, c("month"), summarise,
               N    = sum(!is.na(month)),
               mean = mean(slope, na.rm=TRUE),
               sd   = sd(slope, na.rm=TRUE),
               se   = sd / sqrt(N)
)
stat1bfg
#write.csv(cdata,file = "cdata.csv")
#set month in summary data frame as factor
stat1bfg$month<-as.factor(stat1bfg$month)
#rename month vector
stat1bfg$month<-c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec")
stat1bfg$month <- factor(stat1bfg$month, levels=c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

# Alternate method of changing the factor vector order without fundamentally changing the dataset
# level_order <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec") #this vector might be useful for other plots/analyses
# ggplot(cdata, aes(x = factor(month, level = level_order), y = mean)) + geom_col()

# Standard error of the mean

pd <- position_dodge(0.1) # move them .05 to the left and right

bfgmsd<- ggplot(stat1bfg, aes(x=month, y=mean)) + 
            geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) +
            geom_line() +
            geom_point()

bfgmsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Month", title = "Mean and standard error of CD - BFG")


#forPTS
stat1pts <- ddply(ptsal, c("month"), summarise,
                  N    = sum(!is.na(month)),
                  mean = mean(slope, na.rm=TRUE),
                  sd   = sd(slope, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat1pts
write.csv(stat1pts,file = "stat1pts.csv")
#set month in summary data frame as factor
stat1pts$month<-as.factor(stat1pts$month)
#rename month vector
stat1pts$month<-c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec")
stat1pts$month <- factor(stat1pts$month, levels=c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

#pd <- position_dodge(0.1) # move them .05 to the left and right

ptsmsd<- ggplot(stat1pts, aes(x=month, y=mean)) + 
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) +
          geom_line() +
          geom_point()

ptsmsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Month", title = "Mean and standard error of CD - PTS")

########################################################
### Joint MONTHLY CD mean and error analysis BFG and PTS###
########################################################
##############requires stat1bfg,stat1pts################
########################################################

jointmonth<-cbind(stat1bfg,stat1pts)
colnames(jointmonth)<-c("bfgmonth","bfgnumber","bfgmean","bfgsd","bfgse","ptsmonth","ptsnumber","ptsmean","ptssd","ptsse")

jointmsd<- ggplot(jointmonth, aes(x=bfgmonth)) + 
  geom_errorbar(aes(ymin=bfgmean-bfgse, ymax=bfgmean+bfgse), width=.5) + ylim(0.8,1.5)+
  geom_errorbar(aes(ymin=ptsmean-ptsse, ymax=ptsmean+ptsse), width=.5) + ylim(0.8,1.5)+
  geom_line(aes(y=bfgmean, colour = "BFG")) +
  geom_line(aes(y=ptsmean, colour = "PTS")) +
  geom_point(y=bfgmean, colour = "BFG")+
  geom_point(y=ptsmean, colour = "PTS")+
  scale_colour_hue("BFG", "PTS")+
  labs(title="Monthly mean and standard error at BFG and PTS",x="Date", y = "Drag Coefficient (CD) (*10^-3) +/- se" ) 

#plot
jointmsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Month", title = "Monthly mean and standard error at BFG and PTS")

#############################################################
### Joint MONTHLY CD10N mean and error analysis BFG and PTS##
#############################################################
##############requires stat5bfg,stat5pts#####################changestat1 to stat 5
#############################################################
#####################Results 6####################

##data summary/creation of stat5bfg, stat5pts##

#forBFG#
stat5bfg <- ddply(bfgpts10, c("month"), summarise,
                  N    = sum(!is.na(month)),
                  mean = mean(bfg, na.rm=TRUE),
                  sd   = sd(bfg, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat5bfg
write.csv(stat5bfg,file = "stat5bfg.csv")
#set month in summary data frame as factor
stat5bfg$month<-as.factor(stat5bfg$month)
#rename month vector
stat5bfg$month<-c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec")
stat5bfg$month <- factor(stat5bfg$month, levels=c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

# Alternate method of changing the factor vector order without fundamentally changing the dataset
# level_order <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec") #this vector might be useful for other plots/analyses
# ggplot(cdata, aes(x = factor(month, level = level_order), y = mean)) + geom_col()


bfgmsd10<- ggplot(stat5bfg, aes(x=month, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) +
  geom_line() +
  geom_point()

bfgmsd10 + labs(y="CD10N (*10^3) ± s.e.", x = "Month", title = "Mean and standard error of CD - BFG")


#forPTS
stat5pts <- ddply(bfgpts10, c("month"), summarise,
                  N    = sum(!is.na(month)),
                  mean = mean(pts, na.rm=TRUE),
                  sd   = sd(pts, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat5pts
write.csv(stat5pts,file = "stat5pts.csv")
#set month in summary data frame as factor
stat5pts$month<-as.factor(stat5pts$month)
#rename month vector
stat5pts$month<-c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec")
stat5pts$month <- factor(stat5pts$month, levels=c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec"))

jointmonth10b<-rbind(stat5bfg,stat5pts)
jointmonth10b$V6<-c("BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS")

# ggplot(jointmonth10b, aes(x=month, y=mean, linetype=jointmonth10b$V6, name= "Site")) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
#   geom_line() +
#   geom_point() +
#   theme_bw()+
#   labs(title="Monthly mean and standard error of CD at BFG and PTS",x="Month", y = "Drag Coefficient (*10^-3) +/- se" ) 
# 
# moncdmse<-ggplot(jointmonth10b, aes(x=month, y=mean, linetype=jointmonth10b$V6)) + 
#           geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
#           geom_line() +
#           geom_point() +
#           theme_bw()+
#           labs(x="Month", y = "Drag Coefficient (*10^-3) +/- se" ) 
# #title="Monthly mean and standard error of CD at BFG and PTS",
# 
# moncdmse + theme(axis.title.x = element_text(size=20),
#                  axis.title.y = element_text(size=20),
#                  axis.text.x  = element_text(vjust=0.5, size=15),
#                  axis.text.y  = element_text(vjust=0.5, size=15)) 

moncdmse<-ggplot(jointmonth10b, aes(x=month, y=mean, colour=jointmonth10b$V6)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  scale_color_manual(values=c("blue","black"), name= "Site")+
  geom_line() +
  geom_point() +
  theme_bw()+
  labs(x="Month", y = "Drag Coefficient (*10^-3) +/- sd" ) 
#title="Monthly mean and standard error of CD at BFG and PTS",

moncdmse + theme(axis.title.x = element_text(size=20),
                 axis.title.y = element_text(size=20),
                 axis.text.x  = element_text(vjust=0.5, size=15),
                 axis.text.y  = element_text(vjust=0.5, size=15)) 


#############################################################
### separate CD mean and error analysis BFG and PTS###
#############################################################
##############requires stat5bfg,stat5pts#####################changestat1 to stat 5
#############################################################


# jointmsd10<- ggplot(jointmonth10, aes(x=bfgmonth)) + 
#   geom_errorbar(aes(ymin=bfgmean-bfgse, ymax=bfgmean+bfgse), width=.5) + ylim(0.8,1.5)+
#   geom_errorbar(aes(ymin=ptsmean-ptsse, ymax=ptsmean+ptsse), width=.5) + ylim(0.8,1.5)+
#   geom_line(aes(y=bfgmean, colour = "BFG")) +
#   geom_line(aes(y=ptsmean, colour = "PTS")) +
#   geom_point(y=bfgmean, colour = "BFG")+
#   geom_point(y=ptsmean, colour = "PTS")+
#   scale_colour_hue("BFG", "PTS")+
#   labs(title="Monthly mean CD10N and standard error at BFG and PTS",x="Date", y = "Drag Coefficient (CD) (*10^3)" ) 

#forBFG#
stat2bfg <- ddply(bfgal, c("year"), summarise,
                  N    = sum(!is.na(year)),
                  mean = mean(slope, na.rm=TRUE),
                  sd   = sd(slope, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat2bfg
#write.csv(stat2bfg,file = "stat2bfg.csv")
#set year in summary data frame as factor
stat2bfg$year<-as.factor(stat2bfg$year)
#rename year vector
stat2bfg$year<-c("2012","2013", "2014","2015","2016","2017","2018")
stat2bfg$year <- factor(stat2bfg$year, levels=c("2012","2013", "2014","2015","2016","2017","2018"))

# Alternate method of changing the factor vector order without fundamentally changing the dataset
# level_order <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec") #this vector might be useful for other plots/analyses
# ggplot(cdata, aes(x = factor(month, level = level_order), y = mean)) + geom_col()

# Standard error of the mean

pd <- position_dodge(0.1) # move them .05 to the left and right

bfgamsd<- ggplot(stat2bfg, aes(x=year, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + ylim(1,1.5)+
  geom_line() +
  geom_point()

bfgamsd + labs(y="Drag Coefficient (*10^-3) ± s.e.", x = "Year", title = "Annual Mean and standard error of CD - BFG")


#forPTS
stat2pts <- ddply(ptsal, c("year"), summarise,
                  N    = sum(!is.na(year)),
                  mean = mean(slope, na.rm=TRUE),
                  sd   = sd(slope, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat2pts
#write.csv(stat2pts,file = "stat2pts.csv")
#set year in summary data frame as factor
stat2pts$year<-as.factor(stat2pts$year)
#rename year vector
stat2pts$year<-c("2014","2015","2016","2017","2018")
stat2pts$year <- factor(stat2pts$year, levels=c("2014","2015","2016","2017","2018"))

#pd <- position_dodge(0.1) # move them .05 to the left and right

ptsamsd<- ggplot(stat2pts, aes(x=year, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) + ylim(0.9,1.1)+
  geom_line() +
  geom_point()

ptsamsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Year", title = "Annual Mean and standard error of CD - PTS")




########################################################
### Joint annual mean and error analysis BFG and PTS####
########################################################
#############requires stat2bfg,adjstat2pts##############
########################################################
#write.csv(stat2pts,file = "astat2pts.csv")
#adjust byadding extra 2012 and 2013 rows in Excel
adjstat2pts<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/adjstat2pts.csv", header = TRUE)
jointyear<-cbind(stat2bfg,adjstat2pts)
colnames(jointyear)<-c("bfgyear","bfgnumber","bfgmean","bfgsd","bfgse","ptsyear","ptsnumber","ptsmean","ptssd","ptsse")
jointyear$ptsyear<-as.factor(jointyear$ptsyear)
# #set year in summary data frame as factor
# jointyear$year<-as.factor(stat2bfg$year)
# #rename year vector
# stat2bfg$year<-c("2012","2013", "2014","2015","2016","2017","2018")
# stat2bfg$year <- factor(stat2bfg$year, levels=c("2012","2013", "2014","2015","2016","2017","2018"))

jointamsd<- ggplot(jointyear, aes(x=bfgyear)) + 
              geom_errorbar(aes(ymin=bfgmean-bfgse, ymax=bfgmean+bfgse), width=.1) + ylim(0.8,1.5)+
              geom_errorbar(aes(ymin=ptsmean-ptsse, ymax=ptsmean+ptsse), width=.1) + ylim(0.9,1.5)+
              geom_line(aes(y=bfgmean, linetype="BFG"),size=.5) +
              geom_line(aes(y=ptsmean, linetype="PTS"),size=.5)+
              geom_point(y=bfgmean, pch = "7")+
              geom_point(y=ptsmean, pch = "11")+
              scale_linetype_manual(values=c(7,11))+
              theme_bw() +
              labs(title="Annual mean and standard error at BFG and PTS",x="Date", y = "Drag Coefficient (CD))" ) 

#plot
jointamsd + labs(y="Drag Coefficient (*10^-3) ± s.e.", x = "Year") 

ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(values=c(7,11))+
  theme_bw() +
  labs(title="30 day Moving Average of CD10N at BFG and PTS",x="Date", y = "Drag Coefficient (CD) (*10^3)" ) 

########################################################
#Joint annual CD10N mean and error analysis BFG and PTS#
########################################################
#############requires stat2bfg,adjstat2pts##############
########################################################
#Make stat6bfg and stat6pts#
########################### **Method1 mean and std**
#####################Results 7####################

#forBFG#
stat6bfg <- ddply(bfgpts10, c("year"), summarise,
                  N    = sum(!is.na(year)),
                  mean = mean(bfg, na.rm=TRUE),
                  sd   = sd(bfg, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat6bfg
write.csv(stat6bfg,file = "stat6bfg.csv")
#set year in summary data frame as factor
stat6bfg$year<-as.factor(stat6bfg$year)
#rename year vector
stat6bfg$year<-c("2012","2013", "2014","2015","2016","2017","2018")
stat6bfg$year <- factor(stat6bfg$year, levels=c("2012","2013", "2014","2015","2016","2017","2018"))
# bfgamsd<- ggplot(stat2bfg, aes(x=year, y=mean)) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) + ylim(1,1.5)+
#   geom_line() +
#   geom_point()
# 
# bfgamsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Year", title = "Annual Mean and standard error of CD - BFG")


#forPTS
stat6pts <- ddply(bfgpts10, c("year"), summarise,
                  N    = sum(!is.na(year)),
                  mean = mean(pts, na.rm=TRUE),
                  sd   = sd(pts, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat6pts
write.csv(stat6pts,file = "stat6pts.csv")
#set year in summary data frame as factor
stat6pts$year<-as.factor(stat6pts$year)
#rename year vector
stat6pts$year<-c("2012","2013","2014","2015","2016","2017","2018")
stat6pts$year <- factor(stat6pts$year, levels=c("2012","2013","2014","2015","2016","2017","2018"))

# ptsamsd<- ggplot(stat2pts, aes(x=year, y=mean)) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5) + ylim(0.9,1.1)+
#   geom_line() +
#   geom_point()
# 
# ptsamsd + labs(y="Drag Coefficient (*10^3) ± s.e.", x = "Year", title = "Annual Mean and standard error of CD - PTS")


#adjust byadding extra 2012 and 2013 rows in Excel
adjstat2pts<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/adjstat2pts.csv", header = TRUE)
jointyear10<-rbind(stat2bfg,adjstat2pts)
jointyear10$V6<-c("BFG","BFG","BFG","BFG","BFG","BFG","BFG","PTS","PTS","PTS","PTS","PTS","PTS","PTS")
jointyear10$V6<-as.factor(jointyear10)
jointyear$ptsyear<-as.factor(jointyear$ptsyear)
# #set year in summary data frame as factor
# jointyear$year<-as.factor(stat2bfg$year)
# #rename year vector
# stat2bfg$year<-c("2012","2013", "2014","2015","2016","2017","2018")
# stat2bfg$year <- factor(stat2bfg$year, levels=c("2012","2013", "2014","2015","2016","2017","2018"))
# colnames(jointyear)<-c("bfgyear","bfgnumber","bfgmean","bfgsd","bfgse","ptsyear","ptsnumber","ptsmean","ptssd","ptsse")

jointmonth10b<-rbind(stat5bfg,stat5pts)
jointmonth10b$V6<-c("BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","BFG","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS","PTS")
# 
# anncdmse<-ggplot(jointyear10, aes(x=year, y=mean, linetype=jointyear10$V6)) + 
#             geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
#             geom_line() +
#             geom_point() +
#             theme_bw()+
#             labs(x="Year", y = "Drag Coefficient (*10^3)")
# #title="Annual Drag Coefficient (CD10N) ± s.e. at BFG and PTS",
# 
# 
# anncdmse + theme(axis.title.x = element_text(size=20),
#                  axis.title.y = element_text(size=20),
#                  axis.text.x  = element_text(vjust=0.5, size=15),
#                  axis.text.y  = element_text(vjust=0.5, size=15)) 


anncdmse<-ggplot(jointyear10, aes(x=year, y=mean, colour=jointyear10$V6)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_line() +
  scale_color_manual(values=c("blue","black"), name= "Site")+
  geom_point() +
  theme_bw()+
  labs(x="Year", y = "Drag Coefficient (*10^-3) ± 1 s.d.")
#title="Annual Drag Coefficient (CD10N) ± s.e. at BFG and PTS",


anncdmse + theme(axis.title.x = element_text(size=20),
                 axis.title.y = element_text(size=20),
                 axis.text.x  = element_text(vjust=0.5, size=15),
                 axis.text.y  = element_text(vjust=0.5, size=15)) 


########################################################
############### U vs ustar for BFG and PTS##############
########################################################
#############requires 30day avg daily values ###########
########################################################


#######BFG####
bfginput<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/windustarbfg.csv", header = TRUE)
bfginput<-as_data_frame(bfginput)
bfginput$V4<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
bfginput$V4<-as.POSIXct(bfginput$V4,format="%Y/%m/%d")
p<-ggplot(bfginput,aes(x=wind, y=ustar, group=1)) +
      geom_point(size=2, shape=21, fill="#CC0000",colour=alpha("black", 1/4)) + ylim(0.1,0.25)+ xlim(2,9)+
      geom_smooth(method=lm)+
      geom_text(x = 25, y = 300, label = lm_eqn(bfginput), parse = TRUE)+
      labs(title="Mean daily 8.5m Wind vs U* at BFG ",x="Wind (ms-1)", y = "Ustar (ms-1)")
fit<-lm(ustar ~ wind, bfginput)
coef(fit)
summary(fit)
#write.csv(fit,file = "astat2pts.csv")


#######PTS####
ptsinput<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/windustarpts.csv", header = TRUE)
ptsinput<-as_data_frame(ptsinput)
ptsinput$V4<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
ptsinput$V4<-as.POSIXct(ptsinput$V4,format="%Y/%m/%d")
p<-ggplot(ptsinput,aes(x=wind, y=ustar, group=1)) +
  geom_point(size=2, shape=21, fill="#CC0000",colour=alpha("black", 1/4)) + ylim(0.1,0.25)+ xlim(2,9)+
  geom_smooth(method=lm)+
  #geom_text(x = 25, y = 300, label = lm_eqn(ptsinput), parse = TRUE)+
  labs(title="Mean daily 8.5m Wind vs U* at PTS ",x="Wind (ms-1)", y = "Ustar (ms-1)")
fit<-lm(ustar ~ wind, ptsinput)
coef(fit)
summary(fit)
#write.csv(fit,file = "astat2pts.csv")

########################################################
#### Comparison of U wind and U* at BFG and PTS#########
########################################################
#############requires 30day avg daily values ###########
########################################################
#####################Results 3 and 4####################
###comp of u wind####

# compwind<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/compwind.csv", header = TRUE)
# compwind$date<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")
# # ggplot(compwind, aes(date))+
# #   geom_line(aes(y=bfgwind, colour = "BFG")) + ylim(2,9) +
# #   geom_line(aes(y=ptswind, colour = "PTS")) + ylim(2,9) +
# #   scale_colour_hue("Site")+
# #   labs(title="30 day Moving Average of Wind at BFG and PTS",x="Date", y = "Wind (ms-1)" ) 
# windbfgpts<-ggplot(compwind, aes(x=date)) +
#               geom_line(aes(y=bfgwind, linetype="BFG"),size=.5) + ylim(2.1,9) +
#               geom_line(aes(y=ptswind, linetype="PTS"),size=.5)+ ylim(2.1,9) +
#               scale_linetype_manual(values=c(7,11), name= "Site")+
#               theme_bw() +
#               labs(x="DOY", y = "Wind (ms-1)")
# #title="30 day Moving Average of Wind at BFG and PTS",
# windbfgpts + theme(axis.title.x = element_text(size=20),
#                  axis.title.y = element_text(size=20),
#                  axis.text.x  = element_text(vjust=0.5, size=15),
#                  axis.text.y  = element_text(vjust=0.5, size=15))  

windbfgpts<-ggplot(compwind, aes(x=date)) +
  geom_line(aes(y=bfgwind, colour="BFG"),size=.5) + ylim(2.1,9) +
  geom_line(aes(y=ptswind, colour="PTS"),size=.5)+ ylim(2.1,9) +
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  labs(x="DOY", y = "Wind (ms-1)")
#title="30 day Moving Average of Wind at BFG and PTS",
windbfgpts + theme(axis.title.x = element_text(size=20),
                   axis.title.y = element_text(size=20),
                   axis.text.x  = element_text(vjust=0.5, size=15),
                   axis.text.y  = element_text(vjust=0.5, size=15))  



###comp of ustar
compustar<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/compustar.csv", header = TRUE)
compustar$date<-seq(as.Date("2012/1/1"), as.Date("2018/12/31"), "days")

# ustarbfgpts<-ggplot(compustar, aes(x=date)) +
#               geom_line(aes(y=ustarbfg, linetype="BFG"),size=.5) + ylim(0.1,0.25) +
#               geom_line(aes(y=ustarpts, linetype="PTS"),size=.5) + ylim(0.1,0.25) +
#               scale_linetype_manual(values=c(7,11), name="Site")+
#               theme_bw() +
#               labs(x="DOY", y = "U* (ms-1)" ) 
# #title="30 day Moving Average of U* at BFG and PTS",
# ustarbfgpts + theme(axis.title.x = element_text(size=20),
#                    axis.title.y = element_text(size=20),
#                    axis.text.x  = element_text(vjust=0.5, size=15),
#                    axis.text.y  = element_text(vjust=0.5, size=15))  

ustarbfgpts<-ggplot(compustar, aes(x=date)) +
  geom_line(aes(y=ustarbfg, colour="BFG"),size=.5) + ylim(0.1,0.25) +
  geom_line(aes(y=ustarpts, colour="PTS"),size=.5) + ylim(0.1,0.25) +
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  labs(x="DOY", y = "U* (ms-1)" ) 
#title="30 day Moving Average of U* at BFG and PTS",
ustarbfgpts + theme(axis.title.x = element_text(size=20),
                    axis.title.y = element_text(size=20),
                    axis.text.x  = element_text(vjust=0.5, size=15),
                    axis.text.y  = element_text(vjust=0.5, size=15))  


############################
##CONVERSION OF BFG CD AT 8.5M TO CD10
#######################
cdn_ref<-bfgal$slope*10^-3      #drag coefficient at reference height
# cdn_ref<-0.034596008      #drag coefficient at reference height
z_r <- 8.5  #reference height
k<-0.4      #von Karman constant
z_0m <-exp(log(z_r)-k/sqrt(cdn_ref))
cdn10<-(k*k)/(log(10/z_0m)*log(10/z_0m))*1000
cdn10<-as_data_frame(cdn10)
z_0m<-as_data_frame(z_0m)

cdn10$V2<-bfgal$fulldate
ggplot(cdn10, aes(x=V2, y=value))+
  geom_point(size=2, shape=21, fill="#CC0000",colour=alpha("black", 1/5)) +
  geom_line()+
  labs(title="30 day Moving Average of CD10 in 2012-2018 at BFG",x="Date", y = "Drag Coefficient (CD) (*10^3)" )+
  theme_bw()

write.csv(cdn10,file = "cdn10bfg.csv")
######################################
##CONVERSION OF PTS CD AT 8.5M TO CD10
######################################
cdn_ref<-ptsal$slope*10^-3      #drag coefficient at reference height
# cdn_ref<-0.034596008      #drag coefficient at reference height
z_r <- 8.5  #reference height
k<-0.4      #von Karman constant
z_0m <-exp(log(z_r)-k/sqrt(cdn_ref))
cdn10<-(k*k)/(log(10/z_0m)*log(10/z_0m))*1000
cdn10<-as_data_frame(cdn10)
z_0m<-as_data_frame(z_0m)

cdn10$V2<-ptsal$fulldate
c<-ggplot(cdn10, aes(x=V2, y=value))+
  geom_point(size=2, shape=21, fill="#CC0000",colour=alpha("black", 1/5)) +
  geom_line()+
  labs(title="30 day Moving Average of CD10 in 2014-2018 at PTS",x="Date", y = "Drag Coefficient (CD) (*10^3)" )

c + theme_bw()
write.csv(cdn10,file = "cdn10pts.csv")


#################################
###Profiles of Air Temp - BFG####
#################################

home<-"/Users/akinonigbinde/Desktop/NUIST/Thesis/"
setwd(home)
library(ggplot2)
bfgat14<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2014tapr.csv")
bfgat15<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2015tapr.csv")
bfgat16<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2016tapr.csv")
bfgat17<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2017tapr.csv")

b1<-ggplot(bfgat14, aes(x=bfgat14$doy, y=bfgat14$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(329,360)+
  labs(title="Air temperature in 2014 cold wave at BFG",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
b2<-ggplot(bfgat15, aes(x=bfgat15$doy, y=bfgat15$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(320,349)+
  labs(title="Air temperature in 2015 cold wave at BFG",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
b3<-ggplot(bfgat16, aes(x=bfgat16$doy, y=bfgat16$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(35,55)+
  labs(title="Air temperature in 2016 cold wave at BFG - Event 1",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
b4<-ggplot(bfgat16, aes(x=bfgat16$doy, y=bfgat16$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(319,339)+
  labs(title="Air temperature in 2016 cold wave at BFG - Event 2",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
b5<-ggplot(bfgat17, aes(x=bfgat17$doy, y=bfgat17$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(315,335)+
  labs(title="Air temperature in 2017 cold wave at BFG",x="DOY", y = "Temperature (C)" )+
  theme_minimal()


#################################
###Profiles of Air Temp - PTS####
#################################

home<-"/Users/akinonigbinde/Desktop/NUIST/Thesis/"
setwd(home)
library(ggplot2)
ptsat14<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2014tapr.csv")
ptsat15<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2015tapr.csv")
ptsat16<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2016tapr.csv")
ptsat17<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2017tapr.csv")

p1<-ggplot(ptsat14, aes(x=ptsat14$doy, y=ptsat14$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(329,360)+
  labs(title="Air temperature in 2014; cold wave event at PTS",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
p2<-ggplot(ptsat15, aes(x=ptsat15$doy, y=ptsat15$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(320,349)+
  labs(title="Air temperature in 2015; cold wave event at PTS",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
p3<-ggplot(ptsat16, aes(x=ptsat16$doy, y=ptsat16$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(35,55)+
  labs(title="Air temperature in 2016; cold wave event at PTS - Event 1",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
p4<-ggplot(ptsat16, aes(x=ptsat16$doy, y=ptsat16$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(319,339)+
  labs(title="Air temperature in 2016; cold wave event at PTS - Event 2",x="DOY", y = "Temperature (C)" )+
  theme_minimal()
p5<-ggplot(ptsat17, aes(x=ptsat17$doy, y=ptsat17$Ta_Avg))+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0,20)+ xlim(315,335)+
  labs(title="Air temperature in 2017; cold wave event at PTS",x="DOY", y = "Temperature (C)" )+
  theme_minimal()


#####################################
###DEFINITION OF MULTIPLOT FUNCTION##
#####################################


multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL, title="", 
                      fontsize = 12, fontfamily = "Helvetica") {
  require(grid)
  
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
  
  if (nchar(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (nchar(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    
    # Make each plot, in the correct location
    if (nchar(title)>0) {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily))
    }
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(b1, b2, b3, b4, b5, cols=1, title = "Cold wave events at BFG 2014-2018")
multiplot(p1, p2, p3, p4, p5, cols=1, title = "Cold wave events at PTS 2014-2018")



#################################
###Writing out BFG/PTS master####
#################################
write.csv(bfgpts,file = "bfgptsfull.csv")


#######
##BFG##
#######################################
#Wind, ustar and pressure combo plots##
#######################################
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(TTR)
library(forecast)
library(zoo)
library(reshape2)
library(ggpmisc)

bfgtapr14<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2014tapr.csv")
bfgtapr15<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2015tapr.csv")
bfgtapr18<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2018tapr.csv")

bfgtapr14$TIMESTAMP<-as.POSIXct(bfgtapr14$TIMESTAMP, format="%m/%d/%y")
bfgtapr15$TIMESTAMP<-as.POSIXct(bfgtapr15$TIMESTAMP, format="%m/%d/%y")
bfgtapr18$TIMESTAMP<-as.POSIXct(bfgtapr18$TIMESTAMP, format="%m/%d/%y")

# 
# stat <- function(x) c(min = min(x), max = max(x), mean = mean(x))
# z14 <- read.zoo(file ="/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2014tapr.csv" , header = TRUE, sep = ",", format = "%m/%d/%y", aggregate = stat)
# z15 <- read.zoo(file ="/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2015tapr.csv" , header = TRUE, sep = ",", format = "%m/%d/%y", aggregate = stat)
# z18 <- read.zoo(file ="/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG2018tapr.csv" , header = TRUE, sep = ",", format = "%m/%d/%y", aggregate = stat)
# midbfgpr14<-as_data_frame(z14)



ty1<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="Wind speed during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty2<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Wind speed during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty3<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2018-07-01","2018-08-31")))+
  labs(title="Wind speed during Typhoon Maria event at BFG (2018)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty4<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="U star during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty5<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="U star during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty6<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2018-07-01","2018-08-31")))+
  labs(title="U star during Typhoon Maria event at BFG (2018)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty7<-ggplot(bfgtapr14, aes(x=bfgtapr14$TIMESTAMP, y=bfgtapr14$press_mean))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2))+ ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="Air pressure during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

ty8<-ggplot(bfgtapr15, aes(x=bfgtapr15$TIMESTAMP, y=bfgtapr15$Pressure.kPa))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2)) + ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Air pressure during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

ty9<-ggplot(bfgtapr18, aes(x=bfgtapr18$TIMESTAMP, y=bfgtapr18$Pressure.kPa))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2)) + ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Air pressure during during Typhoon Maria event at BFG (2018)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

multiplot(ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8,ty9, cols=3, title = "Typhoon events at BFG 2014, 2015, 2018")


########
##PTS###
#######################################
#Wind, ustar and pressure combo plots##
#######################################

ptstapr14<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2014tapr.csv")
ptstapr15<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2015tapr.csv")
ptstapr18<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/PTS2018tapr.csv")

ptstapr14$TIMESTAMP<-as.POSIXct(ptstapr14$TIMESTAMP, format="%m/%d/%y")
ptstapr15$TIMESTAMP<-as.POSIXct(ptstapr15$TIMESTAMP, format="%m/%d/%y")
ptstapr18$TIMESTAMP<-as.POSIXct(ptstapr18$TIMESTAMP, format="%m/%d/%y")

ptsinput$V4<-bfginput$V4(c(732:2557))

ty1<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="Wind speed during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty2<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Wind speed during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty3<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$wind))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(4,6)+
  scale_x_datetime(limits = as.POSIXct(c("2018-07-01","2018-08-31")))+
  labs(title="Wind speed during Typhoon Maria event at BFG (2018)",x="Date", y = "Wind Speed (ms-1)" )+
  theme_minimal()

ty4<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="U star during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty5<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="U star during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty6<-ggplot(bfginput, aes(x=bfginput$V4, y=bfginput$ustar))+
  geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000")+ ylim(0.15,0.2)+
  scale_x_datetime(limits = as.POSIXct(c("2018-07-01","2018-08-31")))+
  labs(title="U star during Typhoon Maria event at BFG (2018)",x="Date", y = "U star (ms-1)" )+
  theme_minimal()

ty7<-ggplot(bfgtapr14, aes(x=bfgtapr14$TIMESTAMP, y=bfgtapr14$press_mean))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2))+ ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2014-07-01","2014-08-31")))+
  labs(title="Air pressure during Typhoons Rammasun and Matmo events at BFG (2014)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

ty8<-ggplot(bfgtapr15, aes(x=bfgtapr15$TIMESTAMP, y=bfgtapr15$Pressure.kPa))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2)) + ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Air pressure during Typhoons Soudelor and Goni events at BFG (2015)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

ty9<-ggplot(bfgtapr18, aes(x=bfgtapr18$TIMESTAMP, y=bfgtapr18$Pressure.kPa))+
  #geom_line()+
  geom_point(size=2, shape=21, fill="#CC0000", colour=alpha("black", 1/2)) + ylim(99,102)+
  scale_x_datetime(limits = as.POSIXct(c("2015-07-01","2015-08-31")))+
  labs(title="Air pressure during during Typhoon Maria event at BFG (2018)",x="Date", y = "Pressure (kPa)" )+
  theme_minimal()

multiplot(ty1, ty2, ty3, ty4, ty5, ty6, ty7, ty8,ty9, cols=3, title = "Typhoon events at BFG 2014, 2015, 2018")



###################################
############T- tests ##############
###################################

cwtymaster<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/coldwavetyphoonmaster.csv")
tt1<-t.test(cwtymaster$wincdbfg,cwtymaster$extavebfg, paired = T)
tt2<-t.test(cwtymaster$wincdbfg,cwtymaster$cwcdbfg, paired = T)
tt3<-t.test(cwtymaster$wincdpts,cwtymaster$extavepts, paired = T)
tt4<-t.test(cwtymaster$wincdpts,cwtymaster$cwcdpts, paired = T)

tt5<-t.test(cwtymaster$sumcdbfg,cwtymaster$tycdbfg, paired = T)
tt6<-t.test(cwtymaster$sumcdpts,cwtymaster$tycdpts, paired = T)




names(tt1)


#############################
######Typhoons subplotted #######
#############################
########Results 8###############
# 
# tyts1<-ggplot(bfgpts10, aes(x=fulldate)) +
#   geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
#   geom_line(aes(y=pts, linetype="PTS"),size=.5)+
#   scale_linetype_manual(name = "Sites", values=c(7,11))+
#   theme_bw() +
#   facet_zoom(xlim = as.POSIXct(c("2014-07-14", "2014-07-25")), zoom.size = 2)+
#   labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
#   theme(axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15),
#         axis.text.x  = element_text(vjust=0.5, size=10),
#         axis.text.y  = element_text(vjust=0.5, size=10))
# # title="CD during Typhoon events at BFG 2014, 2015, 2018",
# 
# 
# tyts2<-ggplot(bfgpts10, aes(x=fulldate)) +
#   geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
#   geom_line(aes(y=pts, linetype="PTS"),size=.5)+
#   scale_linetype_manual(name = "Sites", values=c(7,11))+
#   theme_bw() +
#   facet_zoom(xlim = as.POSIXct(c("2015-08-07", "2015-08-25")), zoom.size = 2)+
#   labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
#   theme(axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15),
#         axis.text.x  = element_text(vjust=0.5, size=10),
#         axis.text.y  = element_text(vjust=0.5, size=10))
# # title="CD during Typhoon events at BFG 2014, 2015, 2018",
# 
# 
# tyts3<-ggplot(bfgpts10, aes(x=fulldate)) +
#   geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
#   geom_line(aes(y=pts, linetype="PTS"),size=.5)+
#   scale_linetype_manual(name = "Sites", values=c(7,11))+
#   theme_bw() +
#   facet_zoom(xlim = as.POSIXct(c("2018-07-06", "2018-07-12")), zoom.size = 2)+
#   labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
#   theme(axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15),
#         axis.text.x  = element_text(vjust=0.5, size=10),
#         axis.text.y  = element_text(vjust=0.5, size=10))
# # title=" title =  0.5

multiplot(tyts1, tyts2, tyts3, cols=1)

tyts1<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, colour="BFG"),size=.5) +
  geom_line(aes(y=pts, colour="PTS"),size=.5)+
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2014-07-14", "2014-07-25")), zoom.size = 2)+
  labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x  = element_text(vjust=0.5, size=10),
        axis.text.y  = element_text(vjust=0.5, size=10))
# title="CD during Typhoon events at BFG 2014, 2015, 2018",


tyts2<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, colour="BFG"),size=.5) +
  geom_line(aes(y=pts, colour="PTS"),size=.5)+
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2015-08-07", "2015-08-25")), zoom.size = 2)+
  labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x  = element_text(vjust=0.5, size=10),
        axis.text.y  = element_text(vjust=0.5, size=10))
# title="CD during Typhoon events at BFG 2014, 2015, 2018",


tyts3<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, colour="BFG"),size=.5) +
  geom_line(aes(y=pts, colour="PTS"),size=.5)+
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2018-07-06", "2018-07-12")), zoom.size = 2)+
  labs(x="DOY", y = "Drag Coefficient (CD) (*10^3)")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x  = element_text(vjust=0.5, size=10),
        axis.text.y  = element_text(vjust=0.5, size=10))
# title=" title =  0.5


###################################
####Cold Waves subplotted #########
###################################


cwts1<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(name = "Sites", values=c(7,11))+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2014-11-29", "2014-12-03")), zoom.size = 2)+
  labs(x="Date", y = "Drag Coefficient (CD) (*10^3)" )
# title="CD during Cold Wave events at BFG 2014, 2015, 2018",

cwts2<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(name = "Sites", values=c(7,11))+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2015-11-22", "2015-11-26")), zoom.size = 2)+
  labs(x="Date", y = "Drag Coefficient (CD) (*10^3)" )
# title="CD during Cold Wave events at BFG 2014, 2015, 2018",

cwts3<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(name = "Sites", values=c(7,11))+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2016-02-12", "2016-02-15")), zoom.size = 2)+
  labs(x="Date", y = "Drag Coefficient (CD) (*10^3)" )
# title=" title = 

cwts4<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(name = "Sites", values=c(7,11))+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2016-11-22", "2016-11-25")), zoom.size = 2)+
  labs(x="Date", y = "Drag Coefficient (CD) (*10^3)" )
# title=" title = 

cwts5<-ggplot(bfgpts10, aes(x=fulldate)) +
  geom_line(aes(y=bfg, linetype="BFG"),size=.5) +
  geom_line(aes(y=pts, linetype="PTS"),size=.5)+
  scale_linetype_manual(name = "Sites", values=c(7,11))+
  theme_bw() +
  facet_zoom(xlim = as.POSIXct(c("2017-11-16", "2017-11-19")), zoom.size = 2)+
  labs(x="Date", y = "Drag Coefficient (CD) (*10^3)" )
# title=" title = 

multiplot(cwts1, cwts2, cwts3, cwts4, cwts5, cols=2)


##Read in NDVI data##
ndvi<-read.csv('/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/NDVIbfgpts9.csv')
##Results 9##


##PLOT PTS BFG NDVI TS##

# ggplot(ndvi, aes(date)) +
#   geom_line(aes(y=bfgndvi, linetype="BFG"),size=.5) +
#   scale_linetype_manual(values=c(7), name = "Site")+
#   theme_bw() +
#   labs(x="DOY", y = "NDVI" )+
#   theme(axis.title.x = element_text(size=20),
#         axis.title.y = element_text(size=20),
#         axis.text.x  = element_text(vjust=0.5, size=15),
#         axis.text.y  = element_text(vjust=0.5, size=15))
# #title="NDVI timeseries at BFG",

scale_color_manual(values=c("blue","black"), name= "Site")+
 
  
  ggplot(ndvi, aes(date)) +
  geom_line(aes(y=bfgndvi, colour="BFG"),size=.5) +
  scale_color_manual(values=c("blue","black"), name= "Site")+
  theme_bw() +
  labs(x="DOY", y = "NDVI" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))
#title="NDVI timeseries at BFG",

ggplot(ndvi, aes(date)) +
  geom_line(aes(y=bfgndvi, colour = "BFG")) +
  geom_line(aes(y=ptsndvi, colour = "PTS")) +
  scale_colour_hue("Site")+
  labs(title="NDVI timeseries at BFG and PTS",x="Date", y = "NDVI" )



########################################################
######## monthly [Period] mean NDVI FOR BFG######################*********
########################################################*********
#############requires 30day avg daily values ###########
########################################################
##results 9##
#forBFG#
stat3bfg <- ddply(ndvi, c("no"), summarise,
                  N    = sum(!is.na(no)),
                  mean = mean(bfgndvi, na.rm=TRUE),
                  sd   = sd(bfgndvi, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat3bfg
#write.csv(stat3bfg,file = "stat3bfg.csv")
#set month in summary data frame as factor
stat3bfg$no<-as.factor(ndvi$no)
#rename month vector
stat3bfg$no<-c("1","2","3","4","5","6","7","8", "9","10","11","12","13","14", "15","16","17","18","19","20", "21","22","23")
stat3bfg$no <- factor(stat3bfg$no, levels=c("1","2", "3","4","5","6","7","8", "9","10","11","12","13","14", "15","16","17","18","19","20", "21","22","23"))

# Alternate method of changing the factor vector order without fundamentally changing the dataset
# level_order <- c("Jan","Feb", "Mar","Apr","May","Jun","Jul","Aug", "Sep","Oct","Nov","Dec") #this vector might be useful for other plots/analyses
# ggplot(cdata, aes(x = factor(month, level = level_order), y = mean)) + geom_col()

# Standard error of the mean

pd <- position_dodge(0.1) # move them .05 to the left and right

bfgmndvi<- ggplot(stat3bfg, aes(x=no, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, colour = "blue") +
  geom_line() +
  scale_color_manual(values=c("blue"), name= "Site")+
  geom_point() +
  theme_bw() +
  labs(y="NDVI ± 1 s.d", x = "Period")+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))
bfgmndvi
#, title = "Mean and standard error of NDVI - BFG"



######################################
####Annual mean and error analysis of NDVI at BFG###
######################################
##results 10##

###/summarize the data##
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

# Run the functions length, mean, and sd on the value of "change" for each group, 
# broken down by sex + condition

#forBFG#
ndvi$year<-substring(ndvi$date,1,4) #year

stat4bfg <- ddply(ndvi, c("year"), summarise,
                  N    = sum(!is.na(year)),
                  mean = mean(bfgndvi, na.rm=TRUE),
                  sd   = sd(bfgndvi, na.rm=TRUE),
                  se   = sd / sqrt(N)
)
stat4bfg
#write.csv(stat4bfg,file = "stat4bfg.csv")
#set year in summary data frame as factor
stat4bfg$year<-as.factor(stat4bfg$year)
#rename year vector
stat4bfg$year<-c("2012","2013", "2014","2015","2016","2017","2018")
stat4bfg$year <- factor(stat2bfg$year, levels=c("2012","2013", "2014","2015","2016","2017","2018"))

# Standard error of the mean

#pd <- position_dodge(0.1) # move them .05 to the left and right

# bfgandvi<- ggplot(stat4bfg, aes(x=year, y=mean)) + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
#   geom_line() +
#   geom_point(size=3)+
#   theme_bw()+
#   theme(axis.title.x = element_text(size=20),
#         axis.title.y = element_text(size=20),
#         axis.text.x  = element_text(vjust=0.5, size=15),
#         axis.text.y  = element_text(vjust=0.5, size=15))
# 
# bfgandvi + labs(y="NDVI ± s.e.", x = "Year")
# #title = "Annual Mean and standard error of NDVI - BFG"

bfgandvi<- ggplot(stat4bfg, aes(x=year, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, colour = "blue")+
  geom_line() +
  scale_color_manual(values=c("blue"), name= "Site")+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

bfgandvi + labs(y="NDVI ± 1 s.d.", x = "Year")
#title = "Annual Mean and standard error of NDVI - BFG"



#######################################
#CREATE SLOPE NDVI MATCH UP DATASET#
#######################################
compcd<-by(bfgal$slope, ceiling(1:2557 / 16), mean)
ndvicompcd<-seq(1:160)
ndvicompcd<-as_data_frame(ndvicompcd)
ndvicompcd$V2<-as.numeric(compcd)
de<-data.frame("161","NaN")
names(de)<-c("value","V2")
de$V2<-as.numeric(de$V2)
de$value<-as.integer(de$value)
newdf <- rbind(de, ndvicompcd)
newdf[[1,1]]<-is.nan(newdf[[1,1]])
newdf$V3<-ndvi$bfgndvi
newdf$index<-seq(1:23)
names(newdf)<-c("index","CD","NDVI")
write.csv(newdf,file = "compCDNDVIBFG")

###############################################################
#####Extraction of Period 12-18 datapoints done in Excel########
###############################################################
cordata<-read.csv("/Users/akinonigbinde/Desktop/NUIST/Thesis/BFG slopes/BFGNDVICDin.csv", header = FALSE)
names(cordata)<-c("index","slope","ndvi","beforeafter")


ggplot(cordata) +
  geom_point(aes(x=cordata$slope, y=cordata$ndvi)) +     # Use hollow circles
 # geom_line(lgeomm(cordata$ndvi ~  + cordata$slope, data=cordata))+
  geom_abline(slope = -0.05652047,intercept = 1.32329219)+
  theme_bw() +
  labs(x="CD", y = "NDVI" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))
  
ndp+ 
#title="NDVI v CD at BFG"
coef(lm(cordata$slope ~ cordata$ndvi, data = cordata))

cor(x = cordata$slope, y=cordata$ndvi)

intercept = 1.32329219, slope = -0.05652047

####new try May 2019##

cors <- ddply(cordata, c("beforeafter"), summarise, cor = round(cor(cordata$ndvi, cordata$slope), 2))

p + facet_grid(beforeafter) +
  geom_text(data=cors, aes(label=paste("r=", cor, sep="")), x=30, y=4)

#ex#
cors <- ddply(mtcars, c("vs", "am"), summarise, cor = round(cor(mpg, wt), 2))

p + facet_grid(vs ~ am) +
  geom_text(data=cors, aes(label=paste("r=", cor, sep="")), x=30, y=4)

ggplot(mtcars, aes(mpg, wt)) + 
  geom_smooth(method = "loess", colour = "red", fill = "red") + 
  geom_smooth(method = "lm", colour = "blue", fill = "blue") + 
  geom_point() + facet_grid(vs ~ am, margins=TRUE)


fit<-lm(cordata$ndvi ~ cordata$slope, data=cordata)
summary(fit)$r.squared 
#intercept = 0.3292 slope = -0.05
lm(cordata$ndvi ~ cordata$slope, data=cordata)

ggplot(cordata) +
  geom_point(aes(x=cordata$slope, y=cordata$ndvi)) +     # Use hollow circles
  # geom_line(lgeomm(cordata$ndvi ~  + cordata$slope, data=cordata))+
  geom_abline(slope = -0.05,intercept = 0.3292)+
  theme_bw() +
  labs(x="CD", y = "NDVI" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

#for correlation
ggplot(cordata) +
  geom_point(aes(x=cordata$slope, y=cordata$ndvi)) +     # Use hollow circles
  # geom_line(lgeomm(cordata$ndvi ~  + cordata$slope, data=cordata))+
  theme_bw() +
  labs(x="CD", y = "NDVI" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

cor.test(x = cordata$ndvi, y = cordata$slope, method = "pearson")


###########################################
#Comparison of preRam, Ramm and postRam CD#
###########################################
prepost1<-ggplot(cordata, aes(x=factor(cordata$beforeafter), y=cordata$slope)) +
  geom_boxplot() +     # Use hollow circles
  # geom_line(lgeomm(cordata$ndvi ~  + cordata$slope, data=cordata))+
  theme_bw() +
  labs(x="Time period", y = "Drag Coefficient (CD) (*10^3)" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))


prepost2<-ggplot(cordata, aes(x=factor(cordata$beforeafter), y=cordata$ndvi)) +
  geom_boxplot() +     # Use hollow circles
  # geom_line(lgeomm(cordata$ndvi ~  + cordata$slope, data=cordata))+
  theme_bw() +
  labs(x="Time period", y = "NDVI" )+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

multiplot(prepost1, prepost2, cols=1)

t.test(cordata$slope ~ cordata$beforeafter, cordata)

###########################################
#############Wind Roses####################
###########################################



###########################################
##############U vs U star##################
###########################################


rm(ls=uustar)
uustar<-as_data_frame(compwind)
uustar<-cbind(compwind,compustar)
uustar<- uustar[-c(4) ]

p10<-ggplot(uustar, aes(uustar$bfgwind, uustar$ustarbfg)) + geom_point()+
     theme_bw() +
     labs(x="Wind (ms-1)", y = "u* (ms-1", title = "BFG" )+ xlim(2,9)+ ylim(0.1,0.3)+
     theme(plot.title = element_text(size=25),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
           axis.text.x  = element_text(vjust=0.5, size=15),
            axis.text.y  = element_text(vjust=0.5, size=15))+
            geom_smooth(method = "lm", se = FALSE, colour = "black")
  

p11<-ggplot(uustar, aes(uustar$ptswind, uustar$ustarpts)) + geom_point()+
     theme_bw() +
     labs(x="Wind (ms-1)", y = "u* (ms-1)", title = "PTS")+ xlim(2,9)+ ylim(0.1,0.3)+
     theme(plot.title = element_text(size=25),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20),
          axis.text.x  = element_text(vjust=0.5, size=15),
           axis.text.y  = element_text(vjust=0.5, size=15))+
          geom_smooth(method = "lm", se = FALSE, colour = "black")

p12<-ggplot(uustar, aes(uustar$ptswind, uustar$ustarpts)) + 
  geom_point()+
  theme_bw() +
  #labs(x=expression('u'['10']*' (ms-1)'), y = expression(paste("u* (ms"^"-1")))+ xlim(2,9)+ ylim(0.1,0.3)+
  labs(x=expression('u'['10']*', ms-1'), y = "u* (ms-1)")+ xlim(2,9)+ ylim(0.1,0.3)+
  theme(#plot.title = element_text(size=25),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))+
       #geom_smooth(method = "lm", se = FALSE, colour = "black", name = "PTS")+
        geom_abline(intercept = 0.03106501, slope = 0.02747697, colour = "black")+
        geom_abline(intercept = 0.08050364, slope = 0.01646170, colour = "blue")
#datapts, black line pts, blue line bfg



multiplot(p10,p11)

coef(lm(uustar$ustarpts ~ uustar$ptswind, data = uustar))
coef(lm(uustar$ustarbfg ~ uustar$bfgwind, data = uustar))

###########################################
################CD vs U ###################
###########################################

cdu<-as_data_frame(cbind(compwind$date,compwind$bfgwind,compwind$ptswind,bfgpts10$bfg,bfgpts10$pts))
cdu<-colnames(x ="cdu","date","bfgwind","ptswind","bfgcd","ptscd")

p13<-ggplot(cdu, aes(cdu$V2, cdu$V4)) + geom_point(colour=alpha("black", 1/5))+
  theme_bw() +
  labs(x="Wind (ms-1)", y = "CD10N", title = "BFG" )+ xlim(2,9)+ ylim(0.5,3)+
  theme(plot.title = element_text(size=25),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

p14<-ggplot(cdu, aes(cdu$V3, cdu$V5)) + geom_point(colour=alpha("black", 1/5))+
  theme_bw() +
  labs(x="Wind (ms-1)", y = "CD10N", title = "PTS" )+ xlim(2,9)+ ylim(0.5,3)+
  theme(plot.title = element_text(size=25),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))

multiplot(p13,p14)


####
cor.test(x = cdu$V2, y = cdu$V4, method = "pearson")
cor.test(x = cdu$V3, y = cdu$V5, method = "pearson")

rm(ls=fit,fit2)
bfgfit<-lm(cdu$V2 ~ cdu$V4, data=cdu)
ptsfit<-lm(cdu$V3 ~ cdu$V5, data=cdu)
summary(fit)$r.squared 
summary(bfgfit)
summary(ptsfit)
#intercept = 0.3292 slope = -0.05
lm(cordata$ndvi ~ cordata$slope, data=cordata)


#####

p15<-ggplot(cdu, aes(cdu$V2, cdu$V4)) + geom_point()+
  theme_bw() +
 # geom_line()
  labs(x="Wind (ms-1)", y = "CD10N", title = "BFG" )+ xlim(2,9)+ ylim(0.5,3)+
  theme(plot.title = element_text(size=25),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=15),
        axis.text.y  = element_text(vjust=0.5, size=15))
  
 
trainmod<-subset.data.frame()
predcd<-
  coef(lm(cdu$ustarpts ~ cdu$ptswind, data = uustar))
  coef(lm(cdu$ustarbfg ~ cdu$bfgwind, data = uustar))
  
  
  set.seed(20160227)
  x<-seq(0,50,1)
  y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)
  #for simple models nls find good starting values for the parameters even if it throw a warning
  m<-nls(y~a*x/(b+x))
  #get some estimation of goodness of fit
  cor(y,predict(m))
  
  #plot
  plot(x,y)
  lines(x,predict(m),lty=2,col="red",lwd=3)
  
#Mytry
  
 
  x<-cdu$V2
  y<-cdu$V4
  #for simple models nls find good starting values for the parameters even if it throws a warning
  m<-nls(y~a*x/(b+x))
  #get some estimation of goodness of fit
  cor(y,predict(m))
  
  nls(nl)
  
  #plot
  plot(x,y)
  lines(x,predict(m),lty=2,col="red",lwd=3)
  

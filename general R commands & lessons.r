##############
## general ##
###########################################################################

#get help on any function
help(sum)

#get example on any function
example(sum)

#check working directory
getwd()

#set working directory
setwd("C:/Users/owner/Google Drive/Data & Analyse/.Eerstejaars data/Analyses complete data/R")

#run R script from working directory
source("somerscript.r")

#read data from text file into dataframe
somedata <-read.table('infantry.txt',sep='\t',header=TRUE)

#read data from csv file into data.frame (can be necessary to change the decimal setting of Dutch excel from "," to "." !!!)
alldata<-read.csv("Ervaringsverlagen kwantitatief uitgekleed (min excluded) 20151007.csv", header=TRUE, sep=";", na.strings = "NA")

#transform dates into date variables
alldata$date <- as.Date(alldata$date, "%d-%m-%y")

#subsetting the three data collection years
jr1<-subset(alldata, datajaar==1)
jr2<-subset(alldata, datajaar==2)
jr3<-subset(alldata, datajaar==3)

#intall packages
install.packages('ggplot2')

#load package you want
require(ggplot2)	# voor flexibele en awesome grafieken met mogelijkheid tot smoothlijnen overlay etc
require(plyr)		# voor ddply functie waarmee je makkelijk analyses kan doen over subgroepen (of binnen proefpersonen)
require(colorRamps) # voor mooie kleurtjes in grafieken
require(gdata)		# voor lezen xls bestanden
library(psych) #handige samenvattingsmaten

#get help on a package
help(package = "ggplot2")
help(mean)

#cite a package
citation()
citation("cluster")


#####################
## basic functions ##
#########################################################################

#assign values to a variable
z<-c(3,4,7,3,6,3,7,2,6,8,3)

#calculate mean; median; standard deviation;
mean(alldata$varc1);
median(alldata$varc1);
sd(alldata$varc1);

#samenvatting (package psych)
describe(alldata$varc1)
describe.by(alldata, alldata$group)

#create sequence of numbers with steps of 0.5
y<-seq(5,9,0.5)

#repeat something three times
x<-rep("hello", times=3)

# onzindata maken vanuit normaal verdelingen
gender <- rep(c(rep(1,60),rep(2,20)),3)
betaweights <- c(
  c(rnorm(n = 60, mean = 0, sd = 1), rnorm(n=20, mean = .5, sd = 2)),
  c(rnorm(n = 60, mean = -.5, sd = 1), rnorm(n=20, mean = .5, sd = .5)),
  c(rnorm(n = 60, mean = 0, sd = 3), rnorm(n=20, mean = 1, sd = .2)))
IV <- rep(c("exploration","positive","negative"),each=80)
data <- data.frame(gender,betaweights,IV)


#ignore NA values (= missing values) in mathematical operations on vectors (otherwise it returns 'NA')
w <-c(2,5,NA,7)
sum(w, na.rm=TRUE) #=14

#categorize values using factor
chests <- c('gold', 'silver', 'gems', 'gold', 'gems')
types <- factor(chests)

#call factor levels, the categories, using levels (can combine well with legends)
levels(types) #=gems gold silver

#check class of data for fixing errors like 'cannot do it, x must be numeric'
lapply(jr3_corrdata, class)

#if function
if(alldata3$verslagnr[i] == 1 && alldata3$pp[i] > 1 )
{
  staticclustervars$pp[i] <- alldata3$pp[i-1]
}

#ifelse function: condition, if true then..., if false then...)
staticclustervars$dropout <- ifelse(staticclustervars$door1uitval2 == 1, staticclustervars$dropout <- "Persist", staticclustervars$dropout <- "Dropout")

#for loop: iterate with marker i through rows 1 untill the total length of the dataframe
for (i in 1:length(alldata3$pp))
{
  alldata3$c1c2[i] <- alldata3$c1[i] + alldata3$c2[i]
  alldata3$previousc1[i] <- alldata3$c1[i-1]
}


#conditional for loop - combined for and if
for (i in 1:length(alldata3$pp))
{
  if(alldata3$verslagnr[i] == 1 && alldata3$pp[i] > 1 )
  {
    staticclustervars$pp[i] <- alldata3$pp[i-1]
    staticclustervars$C1_lastmin2[i] <- alldata3$c1[i-3]
  }
}


##############
## vectors ##
########################################################################

#create vector (can only be of one variable type, will convert all to one type if necessary)
c(1,2,3)

#create vector using range
c(5:9)

#put stuff in vector
vectorx <- c('walk', 'the', 'plank')

#call range in vectorx; call a couple of specific locations in vectorx
vectorx[1:3];
vectorx[c(2,3)]

#name the elements in the vector
names(vectorx)<c('word1', 'word2', 'word3')

#call element using name
vectorx['word1']

#any mathematical operation on a vector will be done for all elements in that vector
vectory<-c(1,2,3)
vectory*2 #=2,4,6

#adding two vectors will add the values in corresponding locations
vectorz<-c(4,5,6)
vectory+vectorz #=5,7,9

#can compare whether the elements in a vector are the same (or smaller/larger etc) as values in another vector (on the same location)
vectory==vectorz #= false false false


##############
## matrices ##
#########################################################################

#can make matrix using (input variable, number of rows, number of columns)
y<-1:12
matrix(y,3,4)

#can also adjust dimensions of matrix or assign dimenions to vector (nrows, ncols)
dim(y)<-c(2,6)

#call element using location in matrix [row_nr, column_nr]
y[1,2] #=3

#assign value to element using location in matrix
y[1,2]<-42

#call entire row (or column) in matrix; or range of columns (or rows)
y[2,];
y[,2:4]


################
## dataframes ##
##########################################################################

#create dataframe by binding the elements together on corresponding locations on different vectors
dataframe1 <-data.frame(var1, var2, var3)

#call a dataframe column
dataframe1$var1

#merge two dataframes. for this to work, one column needs to have the same name in both dataframes (like 'participants')
bigdataframe <- merge(x=dataframe1, y=dataframe2)

#add variable to dataframe
dataframe1$newvariable<-newvariable

#split dataframe in two according to some grouping variable - in this case gender
malereg<-regressionweights[regressionweights$gender==1, ]
femalereg <-regressionweights[regressionweights$gender==2, ]

##############
## plotting ##
#########################################################################

#barplot will take the value of a vector element as y-height
#if names are assigned, they will appear as labels on x-axis
barplot(vectory)

#can ad line to plot using h parameter of abline, to illustrate, for xample, the mean
#or even the mean and standard deviation
x<-c(3,5,7,2,6,8,3)
barplot(x)
abline(h=mean(x));
abline(h=mean(x)-sd(x), lty=3)
abline(h=mean(x)+sd(x), lty=3)

#scatterplot of two variables (first variable=x-axis, second=y-axis)
plot(vectory, vectorz)

#plot difference values of timeseries in a histogram using 'diff' function
hist(diff(alldata$c1),prob=T,col="red")

#contour plot generates a kind of elevation lines of a marix; volcano is cool demo matrix (standard in R)
x<-matrix(0,10,10);
x[5,5]<-1;
x[2,9]<--1;
contour(x);
contour(volcano)

#3D plot of a matrix can be made with perspective; expand adjusts the height of z coordinate
persp(x)
persp(volcano, expand=0.2)

#heatmap can be created with image function
image(volcano)

#ggplot2 package seems to be most versatile and flexible for plotting almost anything
#elaborate example: plotting timeseries with ggplot
###################################################
plotjr<-
	#draw base plot: select the data, x-axis, y-axis, grouping variable, and coloring variable
	ggplot(data = jr, aes(x = date, y = yvar, group = pp, color = pp)) +

	#or for loess lines, similar principle, sligthly different starting command 'stat_smooth'
	stat_smooth(data = jr, aes(x = date, y = yvar, group = pp, color = pp), se = FALSE, span = 0.4)

	#make lines, + dots (of size 1) of the variables:
	geom_line()+ 	geom_point(size = 1) +
	scale_y_continuous(breaks=seq(0,max_ybreaks,20)) +
	ylim(0,max_yscale) +

	#label y-axis + x-axis. Optional: make the y-axis go from 0 to 100 with breaks of 10:	scale_y_discrete(breaks=seq(0,100,10)) +
	ylab(ylabel) + 	xlab("date") +
	scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"), limits = as.Date(c(begin, eind))) +

	ggtitle(paste(ylabel, jrlabel, "- raw data")) +

	#select the look: the colors + background + eliminate lines and borders
    scale_colour_gradientn(colours=color(length(yvar))) +	theme_bw() +
	theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

	#output to file with the use of 'paste' function to automatically generate appropriate titels
	png(file.path("C:","Users","p262108","Dropbox","Mandy","Data & Analyse",".Eerstejaars data","Analyses complete data","R","time series graph","output - graphs", paste("timeseries ", ylabel, " ",jrlabel, ".jpg", sep="")), width = 20, height = 10, units = "cm", res = 2000 )
	plotjr
	dev.off()
###################################################


#################
## statistics ##
###########################################################################

#simple correlationtest, also provides confidence interval
cor.test(df$var1, df$var2)

#als je meerdere analyses binnen alle proefpersonen wil doen is het goed om ddply te combineren met functies
#hieronder voorbeeld van correlaties uitrekenen
cor_func <- function(jr3){
return(data.frame(
			cor_avcomm_e1 = cor(jr3$avcomm, jr3$e1),
			cor_c1_c3 = cor(jr3$c1, jr3$c3),
			cor_c1_e1 = cor(jr3$c1, jr3$e1),
			))

#in ddply voer je deze functies uit binnen individuen
jr3_corrdata <-ddply(jr3, "pp", cor_func)

#simple linear model, first enter response variable, than predictor variable
linearmodel1 <- lm(df$dependantvar ~ df$independantvar)

#can plot linear model as a line in any plot, like a scatterplot, using abline
plot(df$dependantvar, df$independantvar)
abline(linearmodel1)

#using package psych, we can get nice descriptives
library(psych)

#descriptives by group- in this case gender
describeBy(regressionweights, regressionweights$gender)

#Easy way for multiple T-tests - in this case differences between gender for each dependent variable
sapply(regressionweights[,c("pos", "neg", "expl", "comm")], 
       function(x) t.test(x ~ regressionweights$gender))

#give confidence intervals for mean
t.test(malereg$pos,conf.int=TRUE)



#########################################
#Calculate intra-individual variability #
####################################################################################

#add variable to dataframe filled with NA's
changeC1<-NA
alldata3$changeC1<-changeC1

#step 1: 
#if the participant is the same as in last measurement
#then calculate the difference between current andd previous measurement

for (i in 2:length(alldata3$c1))
{
  if(alldata3$pp[i] - alldata3$pp[i-1] == 0)
  {
    alldata3$changeC1[i] <- alldata3$c1[i]-alldata3$c1[i-1]
    print(alldata3$changeC1[i])
  }
  else
  {
    alldata3$changeC1[i] <- NA
  }
}
print(alldata3$changeC1)


#step 2: 
#make the changescores absolute

alldata3$varC1<-abs(alldata3$changeC1)

#step 3: 
#calculate the average absolute change score (i.e., variability) of each individual (need library(plyr))

sumdata3 <- ddply(alldata3, "pp", summarise, 
                  meanvarc1 = mean(varC1, na.rm=TRUE)
)


###############
# saving data #
##################################################################################

#write dataframe to csv file - 
#write.csv uses . as decimal separator and , as column separator
write.csv(file="individual cluster variables R output c1 + e1 total.csv", x=alldata, row.names=FALSE)

#write dataframe to csv2 file - 
#write.csv2 uses , as decimal separator and ; as column separator
write.csv2(file="individual cluster variables R output c1 + e1 total.csv", x=alldata, row.names=FALSE)

#write dataframe to csv file as table
#this allows for complete flexibility in what to use as decimal and column separator
write.table(file="individual cluster variables R output c1 + e1 total 3.csv", x=alldata,row.names=FALSE,quote=FALSE,sep=";", dec=".")

#write dataframe to XLS file -
#need to load (or install first) package: library(WriteXLS) 
WriteXLS(ExcelFileName="clusterdata GAP-2 PAM (R) using mean var slope of c1 and e1 (clean) GOED 2.xls", x=clusteralldata)


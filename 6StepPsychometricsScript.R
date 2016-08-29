
#__________________________________________________#
#                                                  #
#          Psychometric analyses in R              #
#               The 6-step process                 #
#                                                  #
#        A Dima, August 2016; Example script       #
#        Contact: alexadima@gmail.com              #
#__________________________________________________#

# R script is commented with #'s; to comment or uncomment code, use Ctrl+Shift+C


# This example script uses a dataset named ChronicPainSurvey.por, which includes 2 questionnaires: the Sickness Impact Profile (SIP; 24 items, y/n answer format) and the Basic Emotions Scale (BES; 21 items, 7-point Likert)
# The script illustrates each step of the 6-step analysis first for binary items (SIP) then for ordinal items (BES); the start and end of sections referring to binary or ordinal are marked with # ===

# You can use this script with the related dataset to explore the 6-step analysis, or adapt to your own dataset
# Please pay attention to ***** signs, they mean you might need to change something to run the script on your computer/dataset
# Optional things that might make your life more meaningful are marked with ^^^^^ and indented 

                # ^^^^^ before you start, if you haven't worked with R before, check this quick intro to R: http://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf


#_______________________#
####   DATA IMPORT   ####
#_______________________#

# set the working directory ####
# (i.e. the folder where your data is located and output will be saved)
setwd("C:/ACTIVE/Rworkshop/analyses") #  *****change as applicable to the file location on your computer*****
# OR go to Session --> Set Working Directory --> To Source File Location

# import the libraries ####
# (i.e. the bundles of R functions that you will use to analyze data)
# the code below installs the libraries if they aren't already in your computer, then loads the libraries in your session
# ***** if you want other packages, add their names in the vector ***** 

for (n in c('SparseM', 'foreign', 'utils', 'relimp', 'ggplot2', 'ggdendro', 
           'psych', 'Hmisc', 'ltm', 'mirt', 'eRm', 
           'mokken', 'lavaan','semTools','semPlot',
           'qgraph','sem','CTT','MBESS','cluster'))
{
  if(!require(n,character.only=TRUE)){install.packages(n)}
  library(n,character.only=TRUE)
}

#  brief info on the uses of these libraries
#  for data viewing, writing: 'utils', 'relimp'
#  for plotting: 'ggplot2', 'ggdendro'
#  for descriptives and various psychometric analyses: 'psych', 'Hmisc',
#  for latent trait models, incl calpha, factor scores, IRT: ltm 
#  for parametric IRT analysis: 'mirt', 'eRm'
#  for Mokken scale analysis (nonparametric IRT): mokken
#  for latent trait models: 'lavaan','semTools','semPlot','qgraph','sem',
#  for classical test theory: 'CTT','MBESS'
#  for cluster analyses: 'cluster'

                # ^^^^^ check out other psychometrics packages at: http://cran.r-project.org/web/views/Psychometrics.html
                # if you want to install them all, use the code below (installs cran task views package, 
                # then uses install.views to get all updated packages in Psychometrics):
                # install.packages("ctv")
                # library(ctv)
                # install.views("Psychometrics")

# import your data ####
# EXAMPLE dataset
mydata <- spss.get("./ChronicPainSurvey.por", use.value.labels=TRUE)

mydata <- ChronicPainSurvey

                # ^^^^^ Here are examples of how you can import data from csv, excel and SPSS if you want to import your own dataset
                # *****change as required, comment out all other importing options*****
                ### from csv:
                # read.table is from the utils library - already imported
                # import your file
                # mydata <- read.table(file="./mydata.csv", #^^^^^ or use file.choose() to select your dataset manually via a window interface
                #                      header=TRUE, # first row contains variable names 
                #                      sep=",",   # comma is separator 
                #                      na.strings="999", # if you have specific numbers for missing data, if not - comment out (put a # in front of this line)
                #                      row.names="id") # assign the variable id to row names
                ### from excel:
                # import relevant library
                # library(xlsx)
                # import your file
                # mydata <- read.xlsx("c:/myexcel.xlsx", 1) # read in the first worksheet from the workbook myexcel.xlsx, first row contains variable names (default)
                # OR
                # mydata <- read.xlsx("c:/myexcel.xlsx", sheetName = "mysheet") # read in the worksheet named mysheet
                ### from SPSS:
                # ***** first save SPSS dataset in portable format
                # SPSS syntax:
                # get file='c:\mydata.sav'.
                # export outfile='c:\mydata.por'. 
                # then in R, import your file (function in Hmisc package)
                # mydata <- spss.get("c:/mydata.por", use.value.labels=TRUE) # last option converts value labels to R factors
                # ^^^^^ more on importing data from sas, stata or systat: http://www.statmethods.net/input/importingdata.html
                # ^^^^^ In the psych package, there is also an option to copy paste the data from clipboard
                # mydata <- read.clipboard() 
                # it works fine if you have specific values for missing data (no empty spaces)
                # see ?read.clipboard if you need to specify other data import options


# check your data ####
# if you want to see the variables (always good to check if these are the data you need)
showData(mydata)
# OR
View(mydata)
# OR
headTail(mydata)

                # ^^^^^ if you need to fix some data manually (might not work on Macs, but not a must)
                # fix(mydata) 
                #... and modify the cells you need
                
                # ^^^^^ if you want to save data fixes outside R *****change the name of file and options as required*****
                # write.table( mydata, file="./nameoffile.csv", quote=F, sep="\t", row.names=F )
                
                # ^^^^^ if you want to import this csv later
                # mydata = read.table( file="./nameoffile.csv", header = TRUE, sep = ",")

#check variable names
names(mydata)

                # ^^^^^ to change variable names use:
                # names(mydata) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", ....)
                # make sure you rename all variables and follow the column order!

# change variable names in example dataset (to more clear names)
names(mydata) <- c("POSTORMEET", "GENDER" ,  "EDUCATION" ,"ANGER" ,   "DESPAIR" , "SHAME"  ,  
                     "ANXIETY" , "HAPPINESS" ,"FRUSTRATION" ,"MISERY", "GUILT",  "NERVOUSNESS", 
                     "JOY", "IRRITATION", "GLOOMINESS", "HUMILIATION", "TENSE",    "LOVING",   
                     "AGGRESSION", "MOURNFUL", "BLAMEWORTHY","WORRIED" , "CHEERFUL" ,"DISGUST" , 
                     "SIP1stayhome", "SIP2changepos", "SIP3slowwalk", "SIP4notdo" ,"SIP5handrail" ,"SIP6liedown", "SIP7holdon", "SIP8othersdo" ,
                     "SIP9slowdress", "SIP10shortup", "SIP11notbend", "SIP12diffchair", "SIP13diffbed", "SIP14noappet", "SIP15diffsocks" ,
                     "SIP16Sshortwalk", "SIP17badsleep", "SIP18helpdress", "SIP19sitlong" ,"SIP20nohjobs", "SIP21irritable", "SIP22slowstairs", 
                     "SIP23staybed", "SIP24allpain")

                # ^^^^^ to change one variable name, use:
                # names(mydata)[names(mydata) == "gp_gender"] <- "gender"
                # check result
                # names(mydata)

# check sample size
nrow(mydata)

# select & prepare the items for analysis ####
                # ^^^^^ generic code looks like this (*****replace the labels below with your variable names*****)
                # myset <- mydata[,c("Item1","Item2","Item3","Item4","Item5","Item6","Item7","Item8","Item9","Item10","Item11","Item12")]


# === binary items

# check values and frequencies for one sample item
table( mydata$SIP1stayhome, exclude=NULL )
# select all items you want to examine - as a list of string values
SIP <- c("SIP1stayhome", "SIP2changepos", "SIP3slowwalk", "SIP4notdo" ,"SIP5handrail" ,"SIP6liedown", "SIP7holdon", "SIP8othersdo" ,
         "SIP9slowdress", "SIP10shortup", "SIP11notbend", "SIP12diffchair", "SIP13diffbed", "SIP14noappet", "SIP15diffsocks" ,
         "SIP16Sshortwalk", "SIP17badsleep", "SIP18helpdress", "SIP19sitlong" ,"SIP20nohjobs", "SIP21irritable", "SIP22slowstairs", 
         "SIP23staybed", "SIP24allpain") 


# run frequencies for all items
# *****change as required*****
for( n in SIP)
{
  cat( "\n", n, ":" );
    print( table( mydata[,n], exclude=NULL ) );
}

# compute new variables with yes=1 and no=0
# ****** if your yes/no variables also have missings and "don't know" answers, uncomment the additional lines
# (e.g. by merging 'no', 'sysmis' and 'dk' answers)
for( n in SIP)
{
  n1 <- paste(n,"YN",sep=""); # this adds a YN at the end of the new variable name
  mydata[ , n1 ] <- NA;
  mydata[ mydata[,n] == "yes", n1 ] <- 1;
  mydata[ mydata[,n] == "no",  n1 ] <- 0;
  # mydata[ mydata[,n] == "I don't know",  n1 ] <- 0;
  # mydata[ mydata[,n] == "Missing value",  n1 ] <- 0;
  
  cat( "\n", n1, ":" );
  print( table( mydata[,n1], exclude=NULL ) );
}

# create a new dataset with these items only
SIPYN <- c("SIP1stayhomeYN", "SIP2changeposYN", "SIP3slowwalkYN", "SIP4notdoYN" ,"SIP5handrailYN" ,"SIP6liedownYN", "SIP7holdonYN", "SIP8othersdoYN" ,
         "SIP9slowdressYN", "SIP10shortupYN", "SIP11notbendYN", "SIP12diffchairYN", "SIP13diffbedYN", "SIP14noappetYN", "SIP15diffsocksYN" ,
         "SIP16SshortwalkYN", "SIP17badsleepYN", "SIP18helpdressYN", "SIP19sitlongYN" ,"SIP20nohjobsYN", "SIP21irritableYN", "SIP22slowstairsYN", 
         "SIP23staybedYN", "SIP24allpainYN") 
SIPdata <- mydata[,SIPYN]
# view the new data set
View(SIPdata)
# check what type of data it is
class(SIPdata$SIP1stayhomeYN)

# === 



# ===  ordinal items

# check values and frequencies for one sample item
table( mydata$ANGER, exclude=NULL )
# select all items you want to examine - as a list of string values
BES <- c("ANGER" ,   "DESPAIR" , "SHAME"  ,  "ANXIETY" , "HAPPINESS" ,"FRUSTRATION" ,"MISERY", "GUILT", 
        "NERVOUSNESS", "JOY", "IRRITATION", "GLOOMINESS", "HUMILIATION", "TENSE",   
        "LOVING",   "AGGRESSION", "MOURNFUL", "BLAMEWORTHY",
        "WORRIED" , "CHEERFUL" ,"DISGUST")

# run frequencies for all items
for( n in BES)
{
  cat( "\n", n, ":" );
  print( table( mydata[,n], exclude=NULL ) );
}

# compute new variables ordinal numeric & 'missing' as NA

                ## ^^^^^ if required, recode the missing label (e.g. "Missing value") as NA
                # for( n in BES)
                # {
                #   mydata[ mydata[,n] == "Missing value",  n ] <- NA;
                # }
            
                # # check if variables look now ok
                # for( n in BES)
                # {
                #   cat( "\n", n, ":" );
                #   print( table( mydata[,n], exclude=NULL ) );
                # }

# check class
class(mydata$ANGER)
# define as numeric
for( n in BES)
{
  mydata[,n] <- as.numeric(mydata[,n])
}
# check class
class(mydata$ANGER)
# check if variables look the same
for( n in BES)
{
  cat( "\n", n, ":" );
  print( table( mydata[,n], exclude=NULL ) );
}

# select only the ordinal item set
BESdata <- mydata[,BES]

# ===

                # ^^^^^ if required, use this code to select only cases without missing values on specific columns
                # names(mydata)
                # mydata.full <- mydata[complete.cases(mydata[4:72]),]


#_______________________#
####   THE 6 STEPS   ####
#_______________________#

#__________________#
####   STEP 1   ####
#__________________#
# the psych package 
# item distributions and descriptives
                # ^^^^^ before you start, check this as a reference doc: http://personality-project.org/r/psych/vignettes/overview.pdf

# distributions ####

# === binary items

install.packages('psych')

library(psych)

# item distributions for yes/no items
# build a table with item distributions *****change as required*****

# 1. build an empty txt file
summariesSIPyn.file <- "./summariesSIPyn.txt";
cat( "Variable\tYes\t%\n", file=summariesSIPyn.file, append=FALSE );
# 2. write a function to include values for an item
write.summary.varyn <- function( x, xname )
{
  no      <- sum( x == 0, na.rm=TRUE );
  total    <- length( x );
  
  cat( paste( xname, "\t", no, "\t", sprintf("%.2f",100*no/total), "%\n", sep="" ), file=summariesSIPyn.file, append=TRUE );
}
# 3. for each item, run this function iteratively
for (n in SIPYN)
{
  write.summary.varyn( mydata[,n], n );
}
# and read the table in R
SIPynsum = read.table( file="./summariesSIPyn.txt", header = TRUE, sep = "\t", quote="\"" )
# 4. add column names
colnames(SIPynsum) <- c("Item label","NO\nCount", "NO\nPercentage")
# 5. view
View(SIPynsum)

# add item content
Item <- c("stay at home most of the time",
          "change position frequently",
          "walk more slowly",
          "not doing any of the jobs that I usually do",
          "use a handrail to get upstairs",
          "lie down to rest more often",
          "have to hold on to something to get out of an easy chair",
          "try to get other people to do things for me",
          "get dressed more slowly",
          "only stand up for short periods of time",
          "try not to bend or kneel down",
          "difficult to get out of a chair",
          "difficult to turn over in bed",
          "appetite is not very good",
          "trouble putting on my socks",
          "only walk short distances",
          "sleep less well",
          "get dressed with help from someone else",
          "sit down for most of the day",
          "avoid heavy jobs around the house",
          "more irritable and bad tempered",
          "go upstairs more slowly",
          "stay in bed most of the time",
          "in pain almost all of the time")

                # ^^^^^ generic code for adding item content (***** modify the quotes below with specific item content *****)
                # Item <- c("Item1","Item2","Item3","Item4","Item5","Item6","Item7","Item8","Item9","Item10","Item11","Item12",
                #               "Item13","Item14","Item15","Item16","Item17","Item18","Item19","Item20","Item21","Item22","Item23","Item24","Item25")

SIPynsum <- cbind(Item, SIPynsum)
# view
View(SIPynsum)
# order items based on percentage
SIPynsumOrder <- SIPynsum[order(SIPynsum[,3]),] 
                # ^^^^^ Note: SIPynsum[order(-SIPynsum[,3]),] would do it in descending order
# view
View(SIPynsumOrder)

# ===


# === ordinal items

# frequencies table BES items

# build a table with item distributions (*****change as required*****)

# 1. build an empty txt file
summariesBES.file <- "./summariesBES.txt";
cat( "Variable\tnever\t2\t3\tsometimes\t5\t6\tvery often\tnever-sometimes\tup to very often\tmissing\n", file=summariesBES.file, append=FALSE );
# 2. write a function to include values for an item
write.summary.var <- function( x, xname )
{
  a1 <- sum( x == 1, na.rm=TRUE );
  a2 <- sum( x == 2, na.rm=TRUE );
  a3 <- sum( x == 3, na.rm=TRUE );
  a4 <- sum( x == 4, na.rm=TRUE );
  a5 <- sum( x == 5, na.rm=TRUE );
  a6 <- sum( x == 6, na.rm=TRUE );
  a7 <- sum( x == 7, na.rm=TRUE );
  a8 <- round((sum( x <=4, na.rm=TRUE )*100/nrow(BESdata)), 2);
  a9 <- round((sum( x >4, na.rm=TRUE )*100/nrow(BESdata)), 2);
  a10 <-sum(is.na(x));
  cat( paste( xname, "\t", a1, "\t", a2, "\t", a3, "\t", a4, "\t", a5, "\t", a6, "\t", a7, "\t", a8, "\t", a9, "\t", a10, "\n", sep="" ), file=summariesBES.file, append=TRUE );
}
# 3. for each item, run this function iteratively
for( n in BES)
{
  write.summary.var( mydata[,n], n );
}
# and read the table in R
BESsum = read.table( file="./summariesBES.txt", header = TRUE, sep = "\t", quote="\"" )
# 4. add column names
colnames(BESsum) <- c("Item label","never", "2","3","sometimes","5","6", "very often", "% 1 to 4", "% 5 to 7", "No. missing")
# 5. view
View(BESsum)

                # ^^^^^^ if you want to add item content, modify the quotes below with specific item content
                # Item <- c("Item1","Item2","Item3","Item4","Item5","Item6","Item7","Item8","Item9","Item10","Item11","Item12",
                #           "Item13","Item14","Item15","Item16","Item17","Item18","Item19","Item20","Item21","Item22","Item23",
                #           "Item24","Item25","Item26","Item27","Item28")
                # BESsum <- cbind(Item, BESsum)
                # # view
                # View(BESsum)

# descriptives BES items
                # ^^^^^^ describe treats all variables as numeric, so use it only for ordinal to ratio when properly coded
descrBES <- as.data.frame( round( psych::describe( BESdata ), 2 ))
View(descrBES)

                # ^^^^^^ if you want to see just the basic descriptives for subgroups (e.g. by gender), use (***** change as needed)
                # BESbyGEN <-describeBy( BESdata, mydata$GENDER, skew=FALSE, ranges = FALSE, mat=TRUE)
                # names(BESbyGEN)
                # # round off the values
                # BESbyGEN$mean <- round( BESbyGEN$mean, 2 )
                # BESbyGEN$sd <- round( BESbyGEN$sd, 2 )
                # BESbyGEN$se <- round( BESbyGEN$se, 2 )
                #     # ***** if needed, exclude the missing data rows
                #     # BESbyGEN <- BESbyGEN[BESbyGEN$group1!="Valeurs manquantes", ]
                # View(BESbyGEN)

# ===



# plots to visualize the data ####



# === binary items

# barplot of endorsement frequencies for binary items (number of respondents who answered yes ***** change as needed)
barplot(SIPynsumOrder[,"NO\nCount"], 
        main = "non-endorsement frequencies for SIP items",
        xlab="Items", 
        ylab="Number of respondents", 
        cex.lab=0.8,
        cex.axis=0.8,
        names.arg=SIPynsum[, "Item label"], 
        las=2, 
        cex.names=0.6)

# heat plot of correlations matrix (tetrachoric) ***** uncomment the png & devoff lines if you want to save as png in the working directory
tetrachoric(SIPdata)
# png('corplotSIPyn.png')
cor.plot(tetrachoric(SIPdata)$rho, numbers=TRUE, main="correlations between SIP items", 
         cex=0.5, cex.axis=0.7)
# dev.off()
# ===




# === ordinal items

# barplots for items
                # ^^^^^^ if you want to print as pdf, use the code below (***** and modify as needed) 
                # pdf( "./BESplots.pdf", width=3*3, height=7*3, paper="special" ); 

jpeg( "./BESplots.jpg", width=3*3, height=7*3, units="in", quality = 90, res=150 ); 
par( mfrow=c(7,3)); # set several plots per rows and columns
for( n in BES)
{
  distr <- table(mydata[,n])
  barplot(distr,  
          main=n, 
          col=gray.colors(20), 
          ylab = "Number of respondents", 
          xlab = "Response (1=SA, 7=SD)");
};
dev.off();

                # ^^^^^^ if you want to do scatter plot matrix if few items, e.g. first 5
                # pairs.panels(BESdata[1:5], jiggle=TRUE)
                
                # ^^^^^^ if you want to introduce another variable from the main dataset, e.g. gender (***** and modify as needed) 
                # check which variables are the items in the main dataset
                # names(mydata)
                # pairs.panels(mydata[4:10], bg=ifelse(mydata$GENDER=="male", "blue", "red"), pch=21, 
                #              jiggle=TRUE,
                #              main="BES items by gender")
        
                # ^^^^^^ if you want to plot error bars for all items by gender
                # error.bars.by(BESdata[1:5], mydata$GENDER)
        
                # ^^^^^^ if you want to do a basic mosaic plot for an item pair
                # plot(table(BESdata[,1], BESdata[,2]))
        
                # ^^^^^^ or a better ggplot for an item pair
                # choose items and state number of response options in the data
                # item1 <- c(1:7)
                # item2 <- c(1:7)
                # # create the data frame
                # BES2itemplot <- expand.grid(item1, item2)
                # # select crosstab values as vector
                # BES2itemplot$value <- as.vector(table(BESdata[,1], BESdata[,2]))
                # # build the plot
                # g <- ggplot(BES2itemplot, aes(Var1, Var2)) + geom_point(aes(size = value), colour = "green") + theme_bw() + xlab("") + ylab("")
                # g + scale_size_continuous(range=c(10,30)) + geom_text(aes(label = value))

# heat plot of correlations matrix ***** uncomment the png & devoff lines if you want to save as png in the working directory
lowerCor(BESdata, method = "spearman")
# png('corplot.png')
cor.plot(lowerCor(BESdata, method = "spearman"), numbers=TRUE, main="correlations between BES items", 
         cex=0.5, cex.axis=0.7)
# dev.off()

# ******** if required, recode items that were worded in the opposite way (here the positive emotion items)
BESdata$HAPPINESS <- 8-BESdata$HAPPINESS
BESdata$JOY <- 8-BESdata$JOY
BESdata$LOVING <- 8-BESdata$LOVING
BESdata$CHEERFUL <- 8-BESdata$CHEERFUL
# repeat the plot
lowerCor(BESdata)
cor.plot(lowerCor(BESdata, method = "spearman"),numbers=TRUE,main="correlations between BES items", 
         cex=0.5, cex.axis=0.7)

# === 

                # ******** if required, exclude from next analyses items that have no/little variance 
                # (e.g. <5% in one category for binary variables, less than 5% in 2 adjacent response options for ordinal variables)
                # SIPdata <- subset( SIPdata, select = -namevar)
                # check what you have left
                # names(SIPdata)
                
                # ^^^^^ if you want, check outliers in item sets
                # outlier(SIPdata, cex=.6)
                # outlier(BESdata, cex=.6)



#__________________#
####   STEP 2   ####
#__________________#
# the mokken, ltm, and mirt packages - IRT analyses

                # ^^^^^^ before you start, check these as guide docs:
                # Non-parametric IRT: http://www.jstatsoft.org/v20/i11/paper 
                # Parametric IRT: http://www.jstatsoft.org/v17/a5/paper


# NIRT ####
# examine item set structure with minimum assumptions


# Define a function to partition an item set into mokken scales - lowerbound from .05 to .60 
# (this tells R to apply aisp to any given data.frame for a set of lowerbounds if you call moscales.for.lowerbounds)
                # ^^^^^^^ change .60 value to higher if you need to see how items behave at higher levels of c 
moscales.for.lowerbounds <- function( x, lowerbounds=seq(from=0.05,to=0.60,by=0.05) )
{
  ret.value <- NULL;
  for( lowerbound in lowerbounds )
  {
    tmp <- aisp( x,  lowerbound=lowerbound );
    if( is.null(ret.value) )
    {
      ret.value <- data.frame( "Item"=rownames(tmp), "Scales."=tmp[,1] );
    }
    else
    {
      ret.value <- cbind( ret.value, "Scales."=tmp[,1] );
    }
    names(ret.value)[ncol(ret.value)] <- paste("c=",sprintf("%.2f",lowerbound),sep="");
  }
  rownames(ret.value) <- NULL;
  ret.value;
}


# === binary items

# Compute scalability coefficients
coefH(SIPdata)
# examine aisp for increasing c levels (run the function you defined above and give it a name)
motable.SIPdata <- moscales.for.lowerbounds( SIPdata )
# see the results
motable.SIPdata
# save it as a data frame
aispSIP <- as.data.frame(motable.SIPdata)
# and view it
View(aispSIP)

                # ^^^^^ can be saved as a csv file
                # write.table(motable.SIPdata, file="./aispAS.csv", quote = FALSE, sep = "\t",row.names = FALSE)

                # ****** important: if some items have H<.30, exclude and repeat coefH and aisp


# select the most appropriate solution, for example code below is for the solution at lowerbound .30
moscales.SIPdata.30 <- aisp(SIPdata,  lowerbound=.3)
moscales.SIPdata.30
# check which items are in which subscales (here an example with 4 subscales, you may look only at the first subscale)
names(SIPdata[,moscales.SIPdata.30==1])
                # if several subscales show up, you can select several
                # names(SIPdata[,moscales.SIPdata.30==2])
                # names(SIPdata[,moscales.SIPdata.30==3])
                # names(SIPdata[,moscales.SIPdata.30==4])

# check H for subscales identified by MSA
coefH(SIPdata[,moscales.SIPdata.30==1])
                # if several subscales show up, you can check several
                # coefH(SIPdata[,moscales.SIPdata.30==2])
                # coefH(SIPdata[,moscales.SIPdata.30==3])
                # coefH(SIPdata[,moscales.SIPdata.30==4])


# check properties for subscales identified by MSA:
# name the first subscale
SIPdata1.30 <- SIPdata[,moscales.SIPdata.30==1]

# check conditional association (local independence)
CA.def.SIPdata1.30 <- check.ca(SIPdata1.30, TRUE)

CA.def.SIPdata1.30$InScale
CA.def.SIPdata1.30$Index
CA.def.SIPdata1.30$Flagged

# check monotonicity at different minsize:
# with default minsize:
monotonicity.def.SIPdata1.30 <- check.monotonicity(SIPdata1.30, minvi = .03)
summary(monotonicity.def.SIPdata1.30)
plot(monotonicity.def.SIPdata1.30)
# try different minsizes 60 to 10 
monotonicity.60.SIPdata1.30 <- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 60)
summary (monotonicity.60.SIPdata1.30)
plot(monotonicity.60.SIPdata1.30)
monotonicity.50.SIPdata1.30<- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 50)
summary(monotonicity.50.SIPdata1.30)
plot(monotonicity.50.SIPdata1.30)
                # ^^^^^ minimum applicable rest group size is 50, but if you really want to see what it does with smaller minsizes, see try below
                # monotonicity.40.SIPdata1.30<- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 40)
                # plot(monotonicity.40.SIPdata1.30)
                # plot(monotonicity.40.SIPdata1.30)
                # monotonicity.30.SIPdata1.30<- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 30)
                # summary (monotonicity.30.SIPdata1.30)
                # plot(monotonicity.30.SIPdata1.30)
                # monotonicity.20.SIPdata1.30<- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 20)
                # summary (monotonicity.20.SIPdata1.30)
                # plot(monotonicity.20.SIPdata1.30)
                # monotonicity.10.SIPdata1.30<- check.monotonicity(SIPdata1.30, minvi = .03, minsize = 10)
                # summary (monotonicity.10.SIPdata1.30)
                # plot(monotonicity.10.SIPdata1.30)
        
                # ^^^^^ if you want to plot monotonicity for specific items (e.g. item number 3 here):
                # plot(monotonicity.def.SIPdata1.30, items=3)
                # plot(monotonicity.60.SIPdata1.30, items=3)
                # plot(monotonicity.50.SIPdata1.30, items=3)
                # plot(monotonicity.40.SIPdata1.30, items=3)


# Investigate the assumption of non-intersecting item response functions (IRFs) using method restscore
restscore.SIPdata1.30 <- check.restscore(SIPdata1.30)
summary(restscore.SIPdata1.30)
plot(restscore.SIPdata1.30)
                # ^^^^^ if you want to investigate the assumption of non-intersecting ISRFs using method pmatrix
                # pmatrix.SIPdata1.30  <- check.pmatrix(SIPdata1.30)
                # summary(pmatrix.SIPdata1.30)
                # plot(pmatrix.SIPdata1.30)
                # # Investigate the assumption of Invariant Item Ordering using method MIIO, mscpm and IT
                # iio.SIPdata1.30  <- check.iio(SIPdata1.30)
                # summary(iio.SIPdata1.30)
                # plot(iio.SIPdata1.30)
                # iio.MSCPM.SIPdata1.30 <- check.iio(SIPdata1.30, method="MSCPM")
                # summary(iio.MSCPM.SIPdata1.30)
                # iio.IT.SIPdata1.30 <- check.iio(SIPdata1.30, method="IT")
                # summary(iio.IT.SIPdata1.30)

# plot ISRFs in a pdf
pdf( "./ISRFs-SIP.pdf", height=3*3, width=7*3, paper="special" ); 
par( mfrow=c(3,7)); 
plot( monotonicity.50.SIPdata1.30, curves="ISRF", ask=FALSE, color.ci="yellow" ); 
dev.off();

# === 



# === ordinal items 
# the same script, just replace the names


# Compute scalability coefficients
coefH(BESdata)
# examine aisp for increasing c levels (run the function you defined above and give it a name)
motable.BESdata <- moscales.for.lowerbounds( BESdata )
# see the results
motable.BESdata
# save it as a data frame
aispBES <- as.data.frame(motable.BESdata)
# and view it
View(aispBES)

write.table(motable.BESdata, file="./aispBES.csv", quote = FALSE, sep = "\t",row.names = FALSE)

# select the most appropriate solution, for example code below is for the solution at lowerbound .30
moscales.BESdata.30 <- aisp(BESdata,  lowerbound=.3)
moscales.BESdata.30
# check which items are in which subscales (here the first subscale)
names(BESdata[,moscales.BESdata.30==1])

# check H for subscales identified by MSA
coefH(BESdata[,moscales.BESdata.30==1])

# check properties for subscales identified by MSA:
# name the first subscale
BESdata1.30 <- BESdata[,moscales.BESdata.30==1]

# check conditional association (local independence)
CA.def.BESdata1.30 <- check.ca(BESdata1.30, TRUE)

CA.def.BESdata1.30$InScale
CA.def.BESdata1.30$Index
CA.def.BESdata1.30$Flagged

# check monotonicity at different minsize:
# with default minsize:
monotonicity.def.BESdata1.30 <- check.monotonicity(BESdata1.30, minvi = .03)
summary(monotonicity.def.BESdata1.30)
plot(monotonicity.def.BESdata1.30)
# try different minsizes 60 to 10 
monotonicity.60.BESdata1.30 <- check.monotonicity(BESdata1.30, minvi = .03, minsize = 60)
summary (monotonicity.60.BESdata1.30)
plot(monotonicity.60.BESdata1.30)
monotonicity.50.BESdata1.30<- check.monotonicity(BESdata1.30, minvi = .03, minsize = 50)
summary(monotonicity.50.BESdata1.30)
plot(monotonicity.50.BESdata1.30)


# Investigate the assumption of non-intersecting item step response functions (ISRFs) using method MIIO (appropriate for ordinal items)
miio.BESdata1.30 <- check.iio(BESdata1.30)
summary(miio.BESdata1.30)
plot(miio.BESdata1.30)


# plot ISRFs in a pdf
pdf( "./ISRFs-BES1.pdf", width=7*3, height=3*3, paper="special" ); 
par( mfrow=c(3,7)); 
plot( monotonicity.def.BESdata1.30, curves="ISRF", ask=FALSE, color.ci="yellow" ); 
dev.off();

# === 



# PIRT ####
# examine PIRT model fit for the unidimensional scales identified via NIRT 


# ===  binary items 

# with package ltm (approximate marginal maximum likelihood)
# MODEL 1:
# original rasch model (discrimination parameter = 1 for all)
fit1 <- rasch(SIPdata1.30, constraint = cbind(length(SIPdata1.30) + 1, 1))
# model summary (item coefficients are item difficulties with standard errors and standardized z values)
summary(fit1)
# transform parameter estimates to probability estimates (probability of item endorsement by average individual)
coef(fit1, prob = TRUE, order = TRUE)
# check model fit (^^^^^ GoF should be ns)
GoF.rasch(fit1, B = 199)
# residuals item pairs (^^^^^ chisq residuals < 3.5 is good - rule of thumb)
margins(fit1)
# residuals item triplets
margins(fit1, type = "three-way", nprint = 2) # prints the first 2 triplets of items with the highest residual values for each response pattern

##### if your rasch model fits reasonably so far, you can go on testing with package eRm (conditional maximum likelihood)

# fit MODEL 1 again
RM.SIPdata1.30 <- RM(SIPdata1.30)
# print model summary 
# ^^^^^ differences from ltm due to different standardization -mean 0 in eRm-, estimation method -mml vs cml- and extra normal trait assumption in ltm
summary(RM.SIPdata1.30)
# plot all ICCs in a single graph
plotjointICC(RM.SIPdata1.30, main="Item Characteristics (1PL)",ylab="Probability", legend=FALSE)
# or individual ICCs with empirical ICC values (i.e. relative frequencies of the positive responses are calculated for each rawscore group)
plotICC(RM.SIPdata1.30, empICC=list("raw",type="b",col="blue",lty="dotted")) # ^^^^^ add ask=FALSE if importing to sweave
# plot person-item map (distribution of person latent scores and location of item difficulties on the latent)
par(mfrow = c(1, 1)) # ^^^^^^ set plots to 1 per graph
plotPImap(RM.SIPdata1.30, sorted=TRUE)
# plot item difficulty & infit statistics (items should be within borders)
plotPWmap(RM.SIPdata1.30)
# separation reliability (proportion of item variance not due to error - similar to C-alpha)
ppr <- person.parameter(RM.SIPdata1.30)
SepRel(ppr)
# goodness of fit indices
gofIRT(ppr)
# information criteria
IC(ppr)
# item fit (between 0.6 and 1.4 acc to Wright BD, Linacre JM. Reasonable mean-square fit values. Rasch Meas Trans. 1994;8(2):370.)
itemfit.SIPdata <- itemfit(ppr)
# check min and max infit and outfit
min(itemfit.SIPdata$i.outfitMSQ)
max(itemfit.SIPdata$i.outfitMSQ)
min(itemfit.SIPdata$i.infitMSQ)
max(itemfit.SIPdata$i.infitMSQ)
#Personfit (z values should be </= 1.96)
personfit.SIPdata <- personfit(ppr)
# number of respondents that don't fit
length(personfit.SIPdata$p.outfitZ[personfit.SIPdata$p.outfitZ > 1.96])
length(personfit.SIPdata$p.infitZ [personfit.SIPdata$p.infitZ > 1.96])
# proportion of respondents that don't fit
(length(personfit.SIPdata$p.outfitZ[personfit.SIPdata$p.outfitZ > 1.96]))*100/(length(personfit.SIPdata$p.outfitZ))
(length(personfit.SIPdata$p.infitZ [personfit.SIPdata$p.infitZ > 1.96]))*100/(length(personfit.SIPdata$p.infitZ))
# subgroup invariance test (median split) based on Andersen's liekelihood ratio test (should be ns)
# ^^^^ default splitcr="median", but can also be a separate variable that specified group membership, e.g. gender, 2 disease conditions, etc
# ^^^^ other test available in the function NPtest
lrres <-LRtest(RM.SIPdata1.30, splitcr = "median")
lrres
# plot item difficulty estimates (& confidence elipses) for high and low latent score groups (should be close to the line, elipses small)
plotGOF(lrres,conf=list(), tlab="number", cex=0.8)
# plot item parameter confidence intervals based on LR test
plotDIF(lrres)


                # ^^^^^^ if only some items and/or persons don't fit the model and you've got plenty left, exclude them from the analysis and rerun the models above
                # you can either exclude them from the item set manually or use the function eRm::stepwiseIt


# if your rasch model does not fit well and there is not much hope to save it by excluding a few items, you can test less restrictive IRT models

        # MODEL 2
        # unconstrained Rasch (discrimination parameter estimated and equal for all)
        # ^^^^^ ok if discrimination parameter within 0.5 and 2 
        # acc to Linacre J. Discrimination, guessing and carelessness: estimating IRT parameters with Rasch. Rasch Meas Trans. 2004;18(1):959-960.
        fit2 <- rasch(SIPdata1.30)
        summary(fit2)
        # test if the discrimination parameter is significantly different from 1 (if p sign, unconstrained is better)
        lavaan::anova(fit1, fit2)
        
        # transform parameter estimates to probability estimates
        coef(fit2, prob = TRUE, order = TRUE)
        # check model fit (GoF and residuals should be ns)
        GoF.rasch(fit2, B = 199)
        # residuals item pairs
        margins(fit2)
        # residuals item triplets
        margins(fit2, type = "three-way", nprint = 4)
        
        # item characteristic curves for unconstrained Rasch model
        par(mfrow = c(1, 1))
        plot(fit2, legend = TRUE, cx = "bottomright", lwd = 3,
             cex.main = 1.5, cex.lab = 1.3, cex = 1.1)

        # ICCs, IICs, TIC and total information for unconstrained Rasch model
        par(mfrow = c(2, 2))
        plot(fit2, legend = TRUE, cx = "bottomright", lwd = 3,
             cex.main = 1.5, cex.lab = 1.3, cex = 0.5)
        plot(fit2, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
             cex.lab = 1.3)
        plot(fit2, type = "IIC", items = 0, lwd = 3, cex.main = 1.5,
             cex.lab = 1.3)
        plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
        info1 <- information(fit2, c(-4, 0))
        info2 <- information(fit2, c(0, 4))
        text(0.5, 0.5, labels = paste("Total Information:", round(info1$InfoTotal, 3),
                                      "\n\nInformation in (-4, 0):", round(info1$InfoRange, 3),
                                      paste("(", round(100 * info1$PropRange, 2), "%)", sep = ""),
                                      "\n\nInformation in (0, 4):", round(info2$InfoRange, 3),
                                      paste("(", round(100 * info2$PropRange, 2), "%)", sep = "")), cex = 1.0)
        par(mfrow = c(1, 1))

        # person fit statistics 
        # (default is alternative="less", i.e. sign negative L_z values indicate unlikely response patterns given the model and theta)
        pf.SIPdata <- ltm::person.fit(fit2)
        pf.SIPdata$Tob
        pf.SIPdata$p.values
        pf.SIPdata$alternative
        # item fit statistics (sign chisq indicate items don't fit the model)
        if.SIPdata <- ltm::item.fit(fit2)
        if.SIPdata
        # plot scores - distribution of person scores with item difficulty locations
        fsc <- factor.scores(fit2)
        plot(fsc, include.items = TRUE, main = "KDE for Person Parameters")
        
        legend("left", "item parameters", pch = 16, cex = 1.2, bty = "n")
        
              
        # MODEL 3
        # 2-parameter model (ltm - latent trait model 
        # ^^^^^ the bit ~ z1 specifies 1 latent z1; max 2 latents possible)
        # ^^^^^ if IRT.param = FALSE, it reports factor analytical results (intercepts ^ loadings)
        fit3 <- ltm(SIPdata1.30 ~ z1)
        summary(fit3)
        # test if the 2-parameter model is better than unconstrained Rasch (^^^^^ if p sign, 2-par is better)
        lavaan::anova(fit2, fit3)
        # ICCs for 2-param model
        par(mfrow = c(1, 1))
        plot(fit3, legend = TRUE, cx = "bottomright", lwd = 3,
             cex.main = 1.5, cex.lab = 1.3, cex = 0.6)
        # person fit
        person.fit(fit3)
        # item fit
        item.fit(fit3)
        # and adapt MODEL 2 script for more detailed analysis
        

        # MODEL 4
        # 3-parameter model (tpm - three parameter model) 
        # ^^^^^ default: type="latent trait" (different discrimination params)
        # ^^^^^ estimation problems can occur, and can give error messages - in which case more advanced model constraints are necessary (look at ?tpm)
        fit4 <- tpm(SIPdata1.30)
        summary(fit4)
        # test if the 3-parameter model is better than unconstrained Rasch (if p sign, 3-par is better)
        lavaan::anova(fit2, fit4)
        # ICCs for 3-param model
        par(mfrow = c(1, 1))
        plot(fit4, legend = TRUE, cx = "bottomright", lwd = 3,
             cex.main = 1.5, cex.lab = 1.3, cex = 0.6)
        # item fit
        item.fit(fit4)
        # person fit
        person.fit(fit4)
        # and adapt MODEL 2 script for more detailed analysis

# === 



# === ordinal items 
# Rating Scale model (equivalent of Rasch for ordinal items)
fit1.BES <- RSM(BESdata #, constrained = FALSE, Hessian=TRUE
        )
fit1.BES
summary(fit1.BES)
thresholds(fit1.BES)
coef(fit1.BES)
        
# Partial Credit model (RSM with parameters estimated for each item)
fit2.BES <- PCM(BESdata #, constrained = FALSE, Hessian=TRUE
        )
fit2.BES
summary(fit2.BES)
        
# test between RSM and PCM
anova(fit1.BES, fit2.BES)
        
lr <- 2 * (fit2.BES$loglik - fit1.BES$loglik)
df <- fit2.BES$npar - fit1.BES$npar
pvalue <- 1 - pchisq(lr, df)
cat("LR statistic: ", lr, " df =", df, " p =", pvalue, "\n")
        
# constrained graded response model (equal discrimination parameters across items)
fit3.BES <- grm(BESdata, constrained = TRUE, Hessian=TRUE)
fit3.BES
summary(fit3.BES)
margins(fit3.BES)
margins(fit3.BES, type="three")

# unconstrained graded response model
fit4.BES <- grm(BESdata, constrained = FALSE, Hessian=TRUE)
fit4.BES
summary(fit4.BES)
margins(fit4.BES)
margins(fit4.BES, type="three")

# test between constrained and unconstrained
anova(fit3.BES, fit4.BES)

# graphs (example for unconstrained GRM model)
par(mfrow = c(1, 1))
plot(fit4.BES, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

# IICs for unconstrained model
par(mfrow = c(1, 2))
plot(fit4.BES, type = "IIC", lwd = 2, cex = 0.5, legend = TRUE, cx = "topleft",
     xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit4.BES, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
     cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
info1 <- information(fit4.BES, c(-4, 0))
info2 <- information(fit4.BES, c(0, 4))
text(0, 1, labels = paste("Information in (-4, 0):",
                          paste(round(100 * info1$PropRange, 1), "%", sep = ""),
                          "\n\nInformation in (0, 4):",
                          paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 0.8)
information(fit4.BES, c(-4, 4))

# examine information from selected items (example with items 2 and 3 selected)
information(fit4.BES, c(-4, 4), items = c(2,3)) 
# to compute how much info an item subset brings to the total, do: 100*totalinfoitems/totalinfoallitems

# plots of ICCs for each response category separately
par(mfrow = c(2, 2))
plot(fit4.BES, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = "topright",
        cy = 0.85, xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
        cex.axis = 1.1)
plot(fit4.BES, category = 2, lwd = 2, cex = 1.2, annot = FALSE,
         xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
         cex.axis = 1.1)
plot(fit4.BES, category = 3, lwd = 2, cex = 1.2, annot = FALSE,
     xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
     cex.axis = 1.1)
plot(fit4.BES, category = 4, lwd = 2, cex = 1.2, annot = FALSE,
     xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
     cex.axis = 1.1)
plot(fit4.BES, category = 5, lwd = 2, cex = 1.2, annot = FALSE,
     xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
     cex.axis = 1.1)
plot(fit4.BES, category = 6, lwd = 2, cex = 1.2, annot = FALSE,
     xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
     cex.axis = 1.1)
plot(fit4.BES, category = 7, lwd = 2, cex = 1.2, annot = FALSE,
     xlab = "Latent Trait", cex.main = 1.0, cex.lab = 1.3,
     cex.axis = 1.1)
# plot scores - distribution of person scores
fscBES <- factor.scores(fit4.BES)
plot(fscBES, main = "KDE for Person Parameters")
par(mfrow = c(1, 1))

# more diagnoses (example for PCM model)
par(mfrow = c(7, 3))
plotICC(fit2.BES, legpos=FALSE)
# plot person-item map (distribution of person latent scores and location of item difficulties on the latent) - RSM and PCM
par(mfrow = c(1, 1)) # ^^^^^^ set plots to 1 per graph
plotPImap(fit2.BES #, sorted=TRUE
)
# itemfit
# plot item difficulty & infit statistics (items should be within borders)
plotPWmap(fit2.BES)
# separation reliability (proportion of item variance not due to error - similar to C-alpha)
ppr <- person.parameter(fit2.BES)
names(ppr)
summary(SepRel(ppr))
# information criteria
IC(ppr)
# item fit (between 0.6 and 1.4 acc to Wright BD, Linacre JM. Reasonable mean-square fit values. Rasch Meas Trans. 1994;8(2):370.)
itemfit.BES <- itemfit(ppr)
names(print(itemfit.BES$i.fit))
# check min and max infit and outfit
min(itemfit.BES$i.outfitMSQ)
max(itemfit.BES$i.outfitMSQ)
min(itemfit.BES$i.infitMSQ)
max(itemfit.BES$i.infitMSQ)
#Personfit (z values should be </= 1.96)
personfit.BES <- personfit(ppr)
# number of respondents that don't fit
length(personfit.BES$p.outfitZ[personfit.BES$p.outfitZ > 2])
length(personfit.BES$p.infitZ [personfit.BES$p.infitZ > 2])

# proportion of respondents that don't fit
(length(personfit.BES$p.outfitZ[personfit.BES$p.outfitZ > 2]))*100/(length(personfit.BES$p.outfitZ))
(length(personfit.BES$p.outfitZ [personfit.BES$p.outfitZ < -2]))*100/(length(personfit.BES$p.outfitZ))
# subgroup invariance test (median split) based on Andersen's liekelihood ratio test (should be ns)
# ^^^^ default splitcr="median", but can also be a separate variable that specified group membership, e.g. gender, 2 disease conditions, etc
# ^^^^ other test available in the function NPtest
lrres <-LRtest(fit2.BES, splitcr = "median")
lrres
# plot item difficulty estimates (& confidence elipses) for high and low latent score groups (should be close to the line, elipses small)
plotGOF(lrres,conf=list(), tlab="number", cex=0.8)
# plot item parameter confidence intervals based on LR test
plotDIF(lrres)

# === 

#__________________#
####   STEP 3   ####
#__________________#
# the psych and lavaan packages - EFA and CFA


# EFA ####

# === binary items 

# compute tetrachoric matrix
SIPdata.tetra <- tetrachoric(SIPdata)

nrow(SIPdata)
# very simple structure analysis with tetrachoric matrix - complexities 1-4 should reach max at the best solution
vss(SIPdata.tetra$rho, n.obs=222)

# factor analysis with extracting factors based on eigen values - scree plot real vs random data
# (^^^^^ eigenvalue >1 used in SPSS is considered the worse criteria by psych developers)
fa.parallel(SIPdata, cor="tet")

# factor analysis with number of factors specified - with polychoric correlations matrix for binary items
fa(SIPdata, nfactors=1, fm="pa", cor="tet") # principal axis factoring
fa(SIPdata, nfactors=1, fm="minres", cor="tet") # minimum residual factor analysis (ordinal least squares)
fa(SIPdata, nfactors=1, fm="wls", cor="tet") # weighted least squares

# cluster analysis
icSIPdata <- iclust(SIPdata.tetra$rho, title = "ICLUST using tetrachoric correlations")
summary(icSIPdata)
iclust.diagram(icSIPdata)
print(icSIPdata, cut=.3)

# perform hierarchical FA - default number of factors = 3
omega(SIPdata.tetra$rho, sl=FALSE) # sl=FALSE plots the hierarchical solution
omega(SIPdata.tetra$rho, sl=TRUE) # sl=TRUE plots the bifactor solution

# example of FA with ML and 3 factors
fa3mlSIPdata <- fa(SIPdata, nfactors=3, fm="ml", cor="tet", rotate="varimax") # maximum likelihood
# to visualize the solution
plot(fa3mlSIPdata)
fa.diagram(fa3mlSIPdata)

                # ^^^^^^ to obtain ICCs and IICs from FA solution in psych package
                # # build model
                # IRTfaSIPdata1.30 <- irt.fa(SIPdata1.30)
                # IRTfaSIPdata1.30
                # # build plots
                # par(mfrow=c(1,3)) 
                # plot(IRTfaSIPdata1.30, type="ICC")
                # plot(IRTfaSIPdata1.30, type="IIC")
                # plot(IRTfaSIPdata1.30, type="test")
                # par(mfrow=c(1,1))
        
                # ^^^^^^ for better labels plots, item names (colnames) can be changed into shorter labels
                # # check current labels
                # colnames(SIPdata1.30) 
                # # rename items
                # colnames(SIPdata1.30) <- c( "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", 
                # "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18", "I19", "I20", "I21", "I22", "I23", "I24")
                # # check new labels
                # colnames(SIPdata1.30) 
                # #... and repeat code above

# === 




# ===  ordinal items 

# factor analysis via parallel analysis
fa.parallel(BESdata, , cor="poly")
# very simple structure analysis
vss(BESdata, 5)
# default FA - 5 factor, min residual & principal axis
fa(BESdata,  nfactors=5, fm="minres", n.iter=10)
fa(BESdata,  nfactors=5, fm="pa")
# plot the fa solution
plot(fa(BESdata,  nfactors=5, fm="pa"))
# plot diagram fa solution
fa.diagram(fa(BESdata,  nfactors=5, fm="pa"))
# pca (in case you need it, but would not advise - data reduction, but not structural validity test)
# principal(BESdata,5,rotate="varimax")

# hierarchical cluster analysis using ICLUST (groups items)
iclust(BESdata, title="ICLUST using Pearson correlations")
summary(iclust(BESdata))
iclust.diagram(iclust(BESdata, title="ICLUST using Pearson correlations"))
# hierarchical factor solution to find omega coefficient
omega(BESdata, nfactors=5, sl=FALSE)
omega(BESdata, nfactors=5, sl=TRUE)

# omega with polychoric matrix
BES.poly <- polychoric(BESdata)
omega(BES.poly$rho, nfactors=5,  sl=FALSE)

# === 



# CFA ####
# the lavaan package 


# === Example for ordinal items 
# (for binary items - similar code, but use "ordered" specification as here: http://lavaan.ugent.be/tutorial/cat.html)

# specify the model
CFA.BES <- '
# factor structure
anger =~ ANGER + FRUSTRATION + IRRITATION + AGGRESSION
sadness =~ DESPAIR + MISERY + GLOOMINESS + MOURNFUL
disgust =~ SHAME + GUILT + HUMILIATION + BLAMEWORTHY + DISGUST
anxiety =~ ANXIETY + NERVOUSNESS + TENSE + WORRIED
happiness =~ HAPPINESS + JOY + LOVING + CHEERFUL
'
# fit the model
fitCFA.BES <- lavaan::cfa(CFA.BES, data=BESdata)
# model summary
summary(fitCFA.BES, standardized=TRUE, fit.measures = TRUE)
# coefficients only
coef(fitCFA.BES)
# CFA diagram from psych package
lavaan.diagram(fitCFA.BES, errors=TRUE)
# OR diagram from semPlot package
semPaths(fitCFA.BES,what="std", label.cex=0.3, edge.label.cex=0.5, sizeLat=5, sizeMan=4, curvePivot = TRUE, rotation=4)

semPaths(fitCFA.BES,what="std",layout="circle",edge.label.cex=0.5, curvePivot = TRUE, rotation=3)


                # ^^^^^ and if you want to try something completely different - structure without latents: network analysis 
                # (http://dx.doi.org/10.1016/j.jrp.2014.07.003)
                # correlations matrix
                qgraph(cor(BESdata), layout = "spring", labels = colnames(BESdata)) 
                # partial correlations matrix
                qgraph(cor(BESdata), layout = "spring", labels = colnames(BESdata), graph="concentration") 

# === 




#__________________#
####   STEP 4   ####
#__________________#
# the psych, CTT and MBESS packages 
# CTT analyses (the same for binary and ordinal)

                # ^^^^^ please check here limitations of C alpha and alternatives: 
                # http://link.springer.com/article/10.1007/s11336-008-9101-0
                # http://link.springer.com/article/10.1007/s11336-008-9102-z


# === binary items

# CTT for a single scale
  # gives:
      # C-alpha  & CIs
      # Guttman's lambda 6 (squared multiple correlation)
      # and CTT item properties - reliability if item excluded, item statistics, response frequencies(%)
CalphaSIPdata <- alpha(SIPdata) 
CalphaSIPdata

# beta can be found in the iclust solution
iclust(SIPdata)

# beta by splitHalf and all guttman indices
splitHalf(SIPdata)
guttman(SIPdata)

# and omega & CIs as per Dunn et al 2014 (http://onlinelibrary.wiley.com/doi/10.1111/bjop.12046/abstract
# ***** recommended number of bootstraps is 1000, but can be slow, so change if needed *****
# ***** interval.type="bca" is recommended, but if not working "perc" may give close results
ci.reliability(data=SIPdata, type="omega", conf.level = 0.95,
                 interval.type="perc", B=100)

# === 

# === ordinal items 

# CTT for a single scale
# gives:
# C-alpha  & CIs
# Guttman's lambda 6 (squared multiple correlation)
# and CTT item properties - reliability if item excluded, item statistics, response frequencies(%)
CalphaBESdata <- alpha(BESdata) 
CalphaBESdata

# beta can be found in the iclust solution
iclust(BESdata)

# beta by splitHalf and all guttman indices
splitHalf(BESdata)
guttman(BESdata)

# and omega & CIs as per Dunn et al 2014 (http://onlinelibrary.wiley.com/doi/10.1111/bjop.12046/abstract
# ***** recommended number of bootstraps is 1000, but can be slow, so change if needed *****
# ***** interval.type="bca" is recommended, but if not working "perc" may give close results
ci.reliability(data=BESdata, type="omega", conf.level = 0.95,
               interval.type="perc", B=100)
# === 


#__________________#
####   STEP 5   ####
#__________________#
# the stats & cluster packages 
# cluster analyses (groups respondents)

# ===  binary items 

# perform hierarchical clustering of participants
# (does hierarhical clustering with complete linkage on distance matrix for binary data - Jaccard index)
hclustSIPdata <- hclust(dist(SIPdata, method="binary"), "complete")
# plot with cluster splits for 2, 3 and 4 clusters 
#(^^^^^^ look for:
      # gaps in height - indicate bigger differences between groups)
      # few clusters - good solution is a parsimonious solution
      # clusters of comparable sizes - good solution includes well-balanced percentages)
plot(hclustSIPdata, cex=0.5)
rect.hclust(hclustSIPdata, 2)
rect.hclust(hclustSIPdata, 3)
rect.hclust(hclustSIPdata, 4)
# or plot with ggplot
ggdendrogram(hclustSIPdata, rotate = TRUE, theme_dendro = FALSE)

# agglomeration schedule - distance coefficients (height) for last 10 steps
# (^^^^^^ basically what you see in the graph, in table format)
aggl.sch <- function(hc)
{
  data.frame(row.names = paste (seq(length(hc$height),1),"Cluster(s)"),
             height = hc$height,
             components = ifelse(hc$merge<0, abs(hc$merge), paste ("Cluster",hc$merge)),
             stringsAsFactors = FALSE)
}
tail(aggl.sch(hclustSIPdata), 10L)

### If you don't identify clear clusters, you probably don't have different response patterns
                # ^^^^^^^ if you see clusters, you can investigate further
                # # save the cluster membership in the original dataset
                # SIPdata$clusternumber <- cutree(hclustSIPdata, 3)
                # # compute percentages of participants in groups 
                # length(SIPdata$clusternumber[SIPdata$clusternumber==1])*100/length(SIPdata$clusternumber)
                # length(SIPdata$clusternumber[SIPdata$clusternumber==2])*100/length(SIPdata$clusternumber)
                # length(SIPdata$clusternumber[SIPdata$clusternumber==3])*100/length(SIPdata$clusternumber)
                # # compare cluster groups on items
                # chisq.test(SIPdata$clusternumber, SIPdata$SIP1stayhomeYN)
                # # compare cluster groups on e.g. demographics 
                # # (***** make sure you have comparable variables here - same participant order)
                # chisq.test(SIPdata$clusternumber, mydata$GENDER)

# and/or perform k-medoids (version of k-means appropriate for binary data)
pamSIPdata <- pam(dist(SIPdata, method="binary"), 3) 
      # ^^^^^ above is computed from the distance matrix, can be computed from dataset too: 
      # pamSIPdata <- pam(SIPdata, 3)
# print the summary
summary(pamSIPdata)
# plot the clusters (should be distinct groups)
clusplot(pamSIPdata)
# plot the silhouette values 
# ^^^^^ (good if close to 1, bad if close to 0, more appropriate for the neighbouring cluster if negative value)
plot(pamSIPdata)

# === 



# ===  ordinal items 

# perform hierarchical clustering of participants 
# (does hierarhical clustering with complete linkage on distance matrix with Euclidean distances - default)
hclustBES <- hclust(dist(BESdata), "complete")
# plot with cluster splits for 2, 3 and 4 clusters
#(^^^^^^ look for:
# gaps in height - indicate bigger differences between groups)
# few clusters - good solution is a parsimonious solution
# clusters of comparable sizes - good solution includes well-balanced percentages)
plot(hclustBES, cex=0.6)
rect.hclust(hclustBES, 2)
rect.hclust(hclustBES, 3)
rect.hclust(hclustBES, 4)
# or plot with ggplot
ggdendrogram(hclustBES, rotate = TRUE, theme_dendro = FALSE)

                # ^^^^^^ if you see clusters
                # # save the cluster membership in the original dataset
                # BESdata$clusternumber <- cutree(hclustBES, 2)
                # # percentages of participants in groups
                # length(BESdata$clusternumber[BESdata$clusternumber==1])*100/length(BESdata$clusternumber)
                # length(BESdata$clusternumber[BESdata$clusternumber==2])*100/length(BESdata$clusternumber)
                # # compare cluster groups on items
                # chisq.test(BESdata$clusternumber, BESdata$ANGER)
                # # compare cluster groups on e.g. demographics 
                # # (***** make sure you add your demographic to the dataset)
                # t.test(clusternumber ~ mydata$GENDER, BESdata)

# and/or perform k-means
kmBESdata <- kmeans(BESdata, 2)
# print the summary
summary(kmBESdata)
# plot the clusters (should be distinct groups)
clusplot(BESdata, kmBESdata$cluster, color=TRUE, shade=TRUE, 
                  labels=1, lines=0)


# === 

#__________________#
####   STEP 6   ####
#__________________#
# select items and report results


###
# take a deep breath, review your output & notes, and decide which model makes more sense
###


# === binary items 
# add items for the single scale - for example with the MOKKEN unidimensional solution at c=.30
SIPdata$SIPscore21<- rowSums(SIPdata[,names(SIPdata[,moscales.SIPdata.30==1])] )

# examine frequencies
table(SIPdata$SIPscore21, exclude=NULL)
# check descriptives
summary(SIPdata$SIPscore21)
# do a histogram
hist(SIPdata$SIPscore21)

# === 


# === ordinal items 
# (4 subscales of 4 items and 1 of 5 items)

# then compute scores for meaningful (sub)scale(s)
# by using CTT package if there are several subscales in an item set 

# specify which items belong to which scales 
myKeys <- make.keys(nvar=21,list(anger = c(1,6,11, 16),
                                 sad=c(2,7,12,17), 
                                 disgust=c(3,8,13, 18, 21), 
                                 anxiety=c(4,9,14, 19), 
                                 happy = c(5,10,15, 20)), 
                    item.labels = colnames(BESdata[,1:21]))
# form several scales, default is average score (totals=FALSE)
# (***** if you want sum scores, say totals=TRUE)
BES.scores <- scoreItems(myKeys, BESdata[,1:21]) 
# check the highlights of the results
BES.scores
# check everything about your scores
print(BES.scores, short=FALSE)
# add them to your itemset
BESdata <- cbind(BESdata, BES.scores$scores)

# examine frequencies
table(BESdata$anger, exclude=NULL)
table(BESdata$sad, exclude=NULL)
table(BESdata$disgust, exclude=NULL)
table(BESdata$anxiety, exclude=NULL)
table(BESdata$happy, exclude=NULL)
# check descriptives
    # for one scale
summary(BESdata$anger)
    # OR
    # Hmisc::describe(BESdata$anger)
    
    # for multiple scales (with psych::describe)
descrScales <- as.data.frame( round( psych::describe( BESdata[,c("anger", "sad", "disgust", "anxiety", "happy")] ), 2 ))
View(descrScales)

# do a histogram
hist(BESdata$anger)

# === 

# ... and copy & paste output from R console or plots window to a word doc
                # ^^^^^^ or move to sweave to write a research report in latex
                # open the .Rnw file
                # select the relevant R code & paste in the relevant R sections
                # add text and latex code to present and interpret results



#__________________#
####     NEXT   ####
#__________________#
# ... after the 6 steps


# ***** insert your code here for...

### correlations with validity measures

### include in a regression model

### test group differences

### run an ANOVA

### ... or anything else your heart (or data analysis plan) desires


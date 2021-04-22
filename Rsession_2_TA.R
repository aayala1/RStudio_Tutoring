############################################################
##Wrangling NDSU Data
##written by Andrea Ayala, last change 4/22/21
#https://guides.library.ucla.edu/psychology/data
##https://www.icpsr.umich.edu/web/pages/ICPSR/index.html
##https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame
##https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r
##https://www.r-bloggers.com/2016/11/5-ways-to-subset-a-data-frame-in-r/
#####################################################################################

############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are left. Not strictly needed but often a good idea
#graphics.off(); #close all graphics windows, in case there are still some open from previous stuff we did

install.packages("data.table") #Here you want to install the packages you will use in your session.  You only need to install packages once.
install.packages("tidyverse") 
install.packages("magrittr")

library(data.table) #Here you want to load the packages you will use.  Packages will need to be reloaded for each session. 
library(tidyverse)
library(magrittr)


getwd() #Here, we are identifying where our scripts are being saved to, automatically.  We can change it if we like.  
setwd("C:/Users/andre/OneDrive/Desktop/Upwork") #This changes our working directory.

df1<-read.csv("C:/Users/andre/OneDrive/Desktop/Upwork/NSDUHsaeTotalsTab01-2019.csv") #Here, we are assigning our 'value' which is the read.csv + path, to the object, 
              #df1. We are assigning it so we can manipulate the data in our R dataframe, without necessarily changing the original.
print(df1) #This prints the entire dataframe to our console.  Go ahead and take a look - we should really save this as a tibble so mess that
#comes with printing doesn't happen.
df1<-as_tibble(df1) #make sure you have the tidyverse loaded, or you will get an error
print(df1)
View(df1)
#So look at df1 in the view window, notice how we have a lot of spaces in our columns, and our first six rows do not contain data.  R can't handle that, so we are going
#to put our data in a format that can be analyzed by R.

##Let's rename our columns so that they are easier to work with.  In addition, we are introducing pipes, the %>% symbol.
##Pipes are an extremely useful tool from the magrittr package that allow you to express a sequence of multiple operations. 
#They can greatly simplify your code and make your operations more intuitive.

df1<-df1 %>%
  rename(
    Order = Table1,  #The code can be read as: column name wanted = current column name.  Don't forget the comma!
    State = X,
    Y12 = X.1,
    Y12CIL = X.2,
    Y12CIH = X.3,
    Y12to17 = X.4,
    Y12to17CIL = X.5,
    Y12to17CIH = X.6,
    Y18to25 = X.7,
    Y18to25CIL = X.8,
    Y18to25CIH = X.9,
    Y26plus = X.10,
    Y26plusCIL = X.11,
    Y26plusCIH = X.12,
    Y18plus = X.13,
    Y18plusCIL = X.14,
    Y18plusCIH = X.15
  ) #don't forget to close the parenthesis. 

View(df1)
#Now, lets get rid of the first seven rows, since they have no data

df1<-df1[-c(1:7), ] #remember to keep the comma where it is, because we are going to drop columns next
View(df1)
print(df1)

#Let's get rid of those columns with confidence intervals - remember, the ones with CIH or CIL? we can get rid of them by name instead of guessing 
#which X they correspond to.

df1<-select(df1, -c(Y12CIL, Y12CIH, Y12to17CIL, Y12to17CIH, Y18to25CIL, Y18to25CIH, Y26plusCIL, Y26plusCIH, Y18plusCIL, Y18plusCIH))
print(df1) 
#You should see a tidier dataset which is a 56X7 tibble.

#So, now we want to reorder our column, so that Y18plus and Y26plus switch places.  How do we do that?

# First, get column names
colnames(df1)

#Let's create an object with our desired column order.  Make sure the column names are encapsulated in quotation marks. 
#Let's also say that we aren't sure we want to make this change permanent in our df1.  So we assign it to df2.

col_order <- c("Order", "State", "Y12",
               "Y12to17", "Y18to25", "Y18plus", "Y26plus")
df2 <- df1[, col_order] #Notice the comma here.  Here it is before the command.  For rows, it was after. 
print(df2)

#Next, we need to remove the spaces from within our columns. It's called trimming the whitespace.  Let's save the dataset to itself so that 
#the changes stick.

df2<-df2 %>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))
print(df2)

#Now, the next problem we face is that of our column identities. All of our columns are categorized as 'character', however, we have lot of continuous data in our
#dataset

#We also have commas in our dataset.  So what do we do? We use the command gsub and the as.numeric command to convert our columns

#We want to make Order an integer,
#keep State as a character, and change the remaining columns to numeric
df2$Order<-as.integer(df2$Order)
df2$State<-as.character(df$State)
print(df2) #print df2 so we can see our dataframe

#We need to code the remaining columns a little differently since the commas in our columns have caused R
#to characterize them as characters instead of numeric.  We use gsub to get rid of the commas in those
#columns.

df2$Y12<-as.numeric(gsub(",", "", df2$Y12))
print(df2) #checking we are on the right track

df2$Y12to17<-as.numeric(gsub(",", "", df2$Y12to17))
df2$Y18to25<-as.numeric(gsub(",", "", df2$Y18to25))
df2$Y18plus<-as.numeric(gsub(",", "", df2$Y18plus))
df2$Y26plus<-as.numeric(gsub(",", "", df2$Y12))
print(df2) #print df2 so that we can see our dataframe

#So we are almost done with our housekeeping tasks.  
#Since we are interested in states only, and there are also regions in our column named, "State", how would we 
#go about getting rid of those?

#We use the same approach as we did above, to get rid of unwanted rows.  However, we are also going to save these to 
#an object so that we can use them later.

df3<-subset(df2, Order == 1:5, select = c("Order", "State","Y12","Y12to17", "Y18to25", "Y18plus", "Y26plus"))
print(df3)

#Now, for our next trick, we are going to drop those rows from df2, and reassign it to df_final

df_final<-df2[-c(1:5), ] #remember to keep the comma where it is, as it represents rows
View(df_final)
print(df_final)


#What if we wanted to save this dataframe into a csv and share it with colleagues.  Easy!

write.csv(df_final, file = "C:/Users/andre/OneDrive/Desktop/NDSU_Final.csv")

######################################Move to file, R session 3#####################################################
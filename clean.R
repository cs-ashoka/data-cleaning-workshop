install.packages("expss")
install.packages("tidyverse")

library(tidyverse)
library(expss)

# put the path of the folder where the csv is present. Alternatively, skip the line below and in RStudio go to "Session->Set Working Directory->To Source File Location" after making sure that the csv file and the code file is present in the same folder (for this you will have to save your code first). In general, adopt the good practice of having source files and code in the same folder.
setwd("<path>")

# reads the csv file and converts it into a dataframe in a variable called df1
df1 <- read.csv("./I.csv")

# renaming the columns from the useless names they had before
names(df1)[8] <- "statecenter"
names(df1)[10] <- "return_preference"

#labelling the data (this function comes from the expss library loaded in the beginning)
df1 = apply_labels(df1, 
                   gstin = "ID Variable (GSTIN)", 
                   returnperiod = "Return Period (Month+Year)",
                   R1filingdate = "GSTR1 Filing Date",
                   cash_cgst = "Cash CGST",
                   cash_sgst = "Cash SGST",
                   cash_igst = "Cash IGST",
                   turnover = "Turnover",
                   statecenter = "State or Center",
                   regstatus = "Registration Status (A stands for 'Application approved')",
                   return_preference = "Return Preference (M stands for Monthly, Q stands for Quarterly)",
                   Ward. = "Ward No.")

# summary shows you if NA values or other weird values are present in the dataframe
summary(df1)

# total tax collected by the STATE
# subset function takes a dataframe and returns a new dataframe which has rows satisfying a condition/conditions
# in our case, the new dataframe is called df_state which contains only those rows which have the statecenter variable equal to "STATE", specifying that the STATE collected the taxes for these rows, which is what we need
df_state = subset(df1, statecenter == "STATE")
cg = sum(df_state$cash_cgst, na.rm = TRUE)
sg = sum(df_state$cash_sgst, na.rm = TRUE)
ig = sum(df_state$cash_igst, na.rm = TRUE)
print(cg+sg+ig) # this shows the answer for this task


# this task is to get the number of occurrences where tax returns were filed post 11th May 2021
# shows the datatypes of all columns
str(df1)
# creates a variable time which is a datetime object of "11-05-2021"
time <- as.POSIXct("11-05-2021", format = "%d-%m-%Y")
print(time)
# converts the column R1filingdate into datetime objects
df1$R1filingdate <- as.POSIXct(df1$R1filingdate, format = "%d-%m-%Y")
#does the conditional summation of occurrences where returns are filed post 11th May 2021
sum(df1$R1filingdate > time, na.rm = TRUE)

#------------------------------------------------------------------------------------------------ tidyverse 

#----------------------------------------------------------------------------------------run this code without paying any attention, this is just to facilitate discussion on tidyverse functions

# creates empty vectors for a new dataframe
period <- unique(df1$returnperiod) # this creates a list containing all the unique values for returnperiods on which we will create our graph
state <- vector() # for storing summation of tax collected by state across return periods
center <- vector() # for storing summation of tax collected by center across return periods
for (x in period) # loop runs for the length of the list period
{
  state <- append(state, sum(subset(df1, returnperiod == x & statecenter == "STATE")$cash_igst, na.rm = TRUE))# sums up the igst collected for reuturn period x over all gstin's collected by the state
  center <- append(center, sum(subset(df1, returnperiod == x & statecenter == "CENTER")$cash_igst, na.rm = TRUE)) #  sums up the igst collected for reuturn period x over all gstin's collected by the center
}

sum <- append(state, center) # sum vector contains sums of igst for both state and center
df11 <- data.frame(period, state, center) # create a new dataframe
df11 <- df11[order(df11$period),] # ordering it by return period

#------------------------------------------------------------------------------------------end of this code snippet

# pivot longer converts a wider dataframe into a longer dataframe to facilitate our purpose of plotting the data easily
df11 <- pivot_longer(df11, c(state, center), names_to = "statecenter", values_to = "sum")

# pivot wider converts the longer dataframe that we created earlier into its initial wider form to be able to visually analyse the data more conveniently without graphing it
df11 <- pivot_wider(df11, names_from = "statecenter", values_from = "sum")


# for more help regarding the parameters of these functions, run the code "??<name of function>". For example, if I want to find out what the "names_from" parameter means in the "pivot_wider" function, then I will run this line of code: "??pivot_wider". This gives me a few search results in the help section of the bottom right window. After this, I select the function I want help with, in this case it will be "tidyr::pivot_wider		Pivot data from long to wide" and click on it. You will get its documentation.

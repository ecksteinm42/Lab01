
# clean memory ------------------------------------------------------------
rm(list = ls())

``
# read in data ------------------------------------------------------------
#set working directory

setwd("C:/Users/Matt2/Documents/DACSS/690DV/Lab01")

filename="Names_2010Census.csv"
namedata=read.csv(filename)

library(tidyverse)

# see data ----------------------------------------------------------


head(namedata)


# see data types ----------------------------------------------------------

str(namedata)


# cleaning ----------------------

namedata$length = nchar(namedata$name)
namedata$pctwhite <- replace(namedata$pctwhite, namedata$pctwhite == "(S)", 0)
namedata$pctwhite <- as.numeric(namedata$pctwhite)
namedata$pctblack <- replace(namedata$pctblack, namedata$pctblack == "(S)", 0)
namedata$pctblack <- as.numeric(namedata$pctblack)
namedata$pctapi <- replace(namedata$pctapi, namedata$pctapi == "(S)", 0)
namedata$pctapi <- as.numeric(namedata$pctapi)
namedata$pctaian <- replace(namedata$pctaian, namedata$pctaian == "(S)", 0)
namedata$pctaian <- as.numeric(namedata$pctaian)
namedata$pct2prace <- replace(namedata$pct2prace, namedata$pct2prace == "(S)", 0)
namedata$pct2prace <- as.numeric(namedata$pct2prace)
namedata$pcthispanic <- replace(namedata$pcthispanic, namedata$pcthispanic == "(S)", 0)
namedata$pcthispanic <- as.numeric(namedata$pcthispanic)

#Replacing (S) with 0 - any time there are so few people of a particular race with a particular name that the percentage needs suppression for privacy, that's effectively zero


# lab items -------------

summary(namedata$pctwhite)

theCuts = c(-0.00001, 10, 25, 50, 75, 100)
theCutsLabels = c("less than 10%", "10 - <25%", "25 - <50%", "50 - <75%", "75 - 100%")
namedata$pctblackgroup = cut(namedata$pctblack,
                            breaks = theCuts,
                            labels = theCutsLabels)

table(namedata$pctblackgroup)
#When I used 0 instead of a very slight negative quantity as my lower bound, the 0's were excluded

#I switched to head(namedata, 100) because loading was extremely long otherwise
namedata2 <- head(namedata, 10)
base= ggplot(data=namedata2) 
del1Draft= base + geom_bar(aes(x=cum_prop100k))
del1Draft
#include axis adjustments

# deliverable 1 ----------------------------------------------------------

library(ggplot2)

base= ggplot(data=namedata2, aes(x= reorder(name, -count), y = count)) 
del1Draft3= base + geom_bar(fill = "gray", stat = "identity")
del1Draft3 = del1Draft3 +  theme_minimal() + theme(axis.text.x = element_text(angle = 90), panel.grid.minor = element_blank()) + labs(title = "10 Most Common Surnames", x = "Surname", y = "Number of Occurrences", subtitle = "United States, 2010", caption = "Source: 2010 US Census") + theme(plot.title = element_text(size = 13))
del1Draft3
#Trying to do it all at once majorly jammed R, so I restricted it to among the 10 most common names
#Thought this one looked best without annotation, so I didn't include any



# save del1Draft ----------------------------------------------------------


# deliverable 2 ----------------------------------------------------------

note = "4 of the 10 most common surnames have only 5 letters"
namedata2$length = nchar(namedata2$name)
base2= ggplot(data=namedata2, aes(x= length)) 
del2Draft3= base2 + geom_histogram(binwidth = 1, aes(x= length)) + labs(title = "Number of Letters in Common Surnames", x = "Number of Letters in Name", y = "Number of Names") + theme_minimal() + theme(plot.title = element_text(size=10)) +  annotate(geom = 'text', size = 2.75, label = note, y = -0.1, x = 7, angle = 0)
del2Draft3
#note to self: tantamount to, in one line, ggplot(data=namedata2, aes(x= length)) + geom_histogram(binwidth = 1, aes(x= length)) + labs(title = "Number of Letters in Common Surnames", x = "Number of Letters in Name", y = "Number of Names") + theme_minimal() + theme(plot.title = element_text(size=10)) +  annotate(geom = 'text', size = 2.75, label = note, y = -0.1, x = 7, angle = 0)

                                                                                  
# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft3, file = "del2Draft3.rds")


# deliverable 3 ----------------------------------------------------------

#Possibilities: color code a num by a cat, bar chart with categories colorized
#Ideas: x = , y = 
#if having trouble, maybe stick with the num-num for now?

base3= ggplot(data=namedata2) 
del3draft2= base3 + geom_point(aes(x=pctwhite, y=pct2prace))
del3draft2
#CLARIFY ON THURSDAY - MY NUMBER OF LETTERS IN NAME THING QUALIFIED A UNIVARIATE NUMERICAL INDEX, RIGHT?
#QUESTION FOR THURSDAY - how are you differentiating the concept of num-cat from a univariate with numerical index? (read still more on categoricals first?)


# deliverable 3 (Example) ----------------------------------------------------------

del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
                                 y=Free.Lunch))
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")
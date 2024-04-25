
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
del1Draft3 = del1Draft3 + theme(axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(title = "10 Most Common Surnames", x = "Surname", y = "Number of Occurrences", subtitle = "United States, 2010", caption = "Source: 2010 US Census") + theme(plot.title = element_text(size = 13))
del1Draft3
#Trying to do it all at once majorly jammed R, so I restricted it to among the 10 most common names
#Thought this one looked best without annotation, so I didn't include any



# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft3, file = "del1Draft6.rds")


# deliverable 2 ----------------------------------------------------------

note = "The more popular a name, the more differentiated from the names below it it is"
base2= ggplot(data=namedata2, aes(x= prop100k)) 
del2Draft2= base2 + geom_histogram(binwidth = 5, aes(x= prop100k)) + labs(title = "Distribution of Common Surnames", x = "Proportion per 100,000", y = "Number within Top 10 Names") + theme(plot.title = element_text(size=10)) + scale_x_reverse() + scale_y_continuous(position = "right") + annotate(geom = 'text', size = 2.75, label = note, y = -0.1, x = 600, angle = 0)
del2Draft2
#This shows how the most common names stick out in terms of their popularity, while names further down the list are closer together in popularity
#but is the axis reversal to show this "legit"?
#Note to self: the x-location of the annotation seems to be of its CENTER

#I could not do aes(x= reorder(prop100k, -count))), since this requires a continuous x-aesthetic and that attempt to arrange it rendered it discrete
#+ scale_x_continuous(breaks = seq(0, 60, by = 5))
#example of labs:  + labs(title = "Histogram with Binwidth",
#x = "Values",
#y = "Density") Add to end of ggplot after its closing parenthesis
#Consider respacing axis labels too


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft2, file = "del2Draft2.rds")


# deliverable 3 ----------------------------------------------------------

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

# clean memory ------------------------------------------------------------
rm(list = ls())

``
# read in data ------------------------------------------------------------
#set working directory

setwd("C:/Users/Matt2/Documents/DACSS/690DV/Lab01")

filename="Names_2010Census.csv"
mydata2=read.csv(filename)


# see data ----------------------------------------------------------


head(mydata2)


# see data types ----------------------------------------------------------

str(mydata2)


# cleaning ----------------------

mydata2$pctwhite <- replace(mydata2$pctwhite, mydata2$pctwhite == "(S)", 0)
mydata2$pctwhite <- as.numeric(mydata2$pctwhite)
mydata2$pctblack <- replace(mydata2$pctblack, mydata2$pctblack == "(S)", 0)
mydata2$pctblack <- as.numeric(mydata2$pctblack)
mydata2$pctapi <- replace(mydata2$pctapi, mydata2$pctapi == "(S)", 0)
mydata2$pctapi <- as.numeric(mydata2$pctapi)
mydata2$pctaian <- replace(mydata2$pctaian, mydata2$pctaian == "(S)", 0)
mydata2$pctaian <- as.numeric(mydata2$pctaian)
mydata2$pct2prace <- replace(mydata2$pct2prace, mydata2$pct2prace == "(S)", 0)
mydata2$pct2prace <- as.numeric(mydata2$pct2prace)
mydata2$pcthispanic <- replace(mydata2$pcthispanic, mydata2$pcthispanic == "(S)", 0)
mydata2$pcthispanic <- as.numeric(mydata2$pcthispanic)

#Replacing (S) with 0 - any time there are so few people of a particular race with a particular name that the percentage needs suppression for privacy, that's effectively zero


# lab items -------------

summary(mydata2$pctwhite)

theCuts = c(-0.00001, 10, 25, 50, 75, 100)
theCutsLabels = c("less than 10%", "10 - <25%", "25 - <50%", "50 - <75%", "75 - 100%")
mydata2$pctblackgroup = cut(mydata2$pctblack,
                            breaks = theCuts,
                            labels = theCutsLabels)

table(mydata2$pctblackgroup)
#When I used 0 instead of a very slight negative quantity as my lower bound, the 0's were excluded


base= ggplot(data=mydata2) 
del1Draft= base + geom_bar(aes(x=cum_prop100k))
del1Draft

# deliverable 1 ----------------------------------------------------------

library(ggplot2)

base= ggplot(data=mydata2) 
del1Draft= base + geom_bar(aes(x=cum_prop100k))
del1Draft
#Trying to do it all at once majorly jammed R, so I restricted it to among the 100 most common names

]
# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


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
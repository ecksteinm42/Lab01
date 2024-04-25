
# clean memory ------------------------------------------------------------
rm(list = ls())

``
# read in data ------------------------------------------------------------
#set working directory

setwd("C:/Users/Matt2/Documents/DACSS/690DV/Lab01")

filename="Names_2010Census.csv"
namedata=read.csv(filename)


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

namedata2 <- head(namedata, 100)
base= ggplot(data=namedata2) 
del1Draft= base + geom_bar(aes(x=cum_prop100k))
del1Draft

# deliverable 1 ----------------------------------------------------------

library(ggplot2)

base= ggplot(data=namedata) 
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
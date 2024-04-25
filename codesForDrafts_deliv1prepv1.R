
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

# deliverable 1 ----------------------------------------------------------

library(ggplot2)

base= ggplot(data=mydata2) 
del1Draft= base + geom_bar(aes(x=name))
del1Draft


#left off here - reconfigure and rename variables for what I'm drafting now

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


#load package
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("plyr","ggplot2","reshape2")
ipak(packages)


#set working directory
setwd("/home/jingan/Dropbox/Github/Pratical_Data_Science/driving_visual_analysis_with_automobile_data")

#load the data
vehicles <- read.csv(unz("vehicles.csv.zip", "vehicles.csv"),
                     stringsAsFactors = F)

#get the label name from another file
labels <- do.call(rbind, strsplit(readLines("varlabels.txt"),
                                  " - "))

#take care of missing value in transmission
vehicles$trany[vehicles$trany == ""] <- NA

#transfer transmission
vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) ==
                            "Auto", "Auto", "Manual")
vehicles$trany2 <- as.factor(vehicles$trany2)
table(vehicles$trany2)

#
mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG =
                   mean(comb08), avgHghy = mean(highway08), avgCity =
                   mean(city08))

ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average MPG") +
  ggtitle("All cars")

#non gasoline powered cars
gasCars <- subset(vehicles, fuelType1 %in% c("Regular
   Gasoline", "Premium Gasoline", "Midgrade Gasoline") &
                    fuelType2 == "" & atvType != "Hybrid")
mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG =
                       mean(comb08))

ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average MPG") +
  ggtitle("Gasoline cars")


gasCars$displ <- as.numeric(gasCars$displ)
ggplot(gasCars, aes(displ, comb08)) + geom_point() +
  geom_smooth()

avgCarSize <- ddply(gasCars, ~year, summarise, avgDispl =
                      mean(displ))

ggplot(avgCarSize, aes(year, avgDispl)) + geom_point() +
  geom_smooth() + xlab("Year") + ylab("Average engine
                                      displacement (l)")

byYear <- ddply(gasCars, ~year, summarise, avgMPG =
                  mean(comb08), avgDispl = mean(displ))
byYear2 <- melt(byYear, id = "year")
levels(byYear2$variable) <- c("Average MPG", "Avg engine
       displacement")

ggplot(byYear2, aes(year, value)) + geom_point() +
  geom_smooth() + 
  facet_wrap(~variable, ncol = 1, scales ="free_y") + 
  xlab("Year") + ylab("")


gasCars4 <- subset(gasCars, cylinders == "4")

ggplot(gasCars4, aes(factor(year), comb08)) + 
  geom_boxplot()+ facet_wrap(~trany2, ncol = 1) + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Year", y = "MPG")

ggplot(gasCars4, aes(factor(year), fill = factor(trany2))) +
  geom_bar(position = "fill") + labs(x = "Year", y = "Proportion
                                     of cars", fill = "Transmission") + 
  theme(axis.text.x =  element_text(angle = 45)) + geom_hline(yintercept = 0.5,linetype = 2)


ggplot(gasCars4, aes(x=factor(year), fill = factor(trany2))) +
  geom_bar(position = "fill") + 
  labs(x = "Year", y = "Proportion of cars", fill = "Transmission") + 
  theme(axis.text.x =  element_text(angle = 45)) + 
  geom_hline(yintercept = 0.5,linetype = 2) 

carsMake <- ddply(gasCars4, ~year, summarise, numberOfMakes =
                    length(unique(make)))

ggplot(carsMake, aes(year, numberOfMakes)) + geom_point() +
  labs(x = "Year", y = "Number of available makes") + 
ggtitle("Four cylinder cars")

uniqMakes <- dlply(gasCars4, ~year, function(x)
  unique(x$make))

commonMakes <- Reduce(intersect, uniqMakes)




###################################################
# Creating a dataframe on broadband affordability #
# Copyright (c) 2016 Arturo Muente-Kunigami       #
################################################### 

if (require("XML") == FALSE) {
  install.packages("XML")
}

require(XML)

#
# User defined function to fetch data from World Bank API
# Variables are: URL, years back to search from, and name of new field
#

get_data <- function(wburl,years_back,new_field) {
  tempdf <- data.frame()
  
  for (i in 1:as.numeric(xmlAttrs(xmlRoot(xmlTreeParse(paste(wburl,"&MRV=",years_back,sep=""))))["pages"]))
  {
    tempurl <- paste(wburl,"&page=",i,"&MRV=",years_back,sep = "")
    a <- xmlToDataFrame(tempurl)
    tempdf <- rbind(tempdf,a)
  }
  
  tempdf$field <- as.numeric(as.character(tempdf$value))
  i <- 1
  while (i <= nrow(tempdf)) {
    if (is.na(tempdf$field[i]) == TRUE) {
      tempdf <- tempdf[-i,]
    } else if (i == 1) {
      i <- 2
    } else if (tempdf$country[i] == tempdf$country[i-1]) {
      tempdf <- tempdf[-i,]
    } else {
      i = i + 1
    }
  }
  names(tempdf)[names(tempdf)=="field"] <- new_field
  return(tempdf[,c(1:4,6)])
}


# URLs for data provided by world bank on income share per quintile (plus highest and
# lowest decile), population, and gdp (nominal).

urllow10 <- "http://api.worldbank.org/countries/indicators/SI.DST.FRST.10?per_page=200"
urlfirst <- "http://api.worldbank.org/countries/indicators/SI.DST.FRST.20?per_page=200"
urlsecnd <- "http://api.worldbank.org/countries/indicators/SI.DST.02ND.20?per_page=200"
urlthird <- "http://api.worldbank.org/countries/indicators/SI.DST.03RD.20?per_page=200"
urlforth <- "http://api.worldbank.org/countries/indicators/SI.DST.04TH.20?per_page=200"
urlfifth <- "http://api.worldbank.org/countries/indicators/SI.DST.05TH.20?per_page=200"
urltop10 <- "http://api.worldbank.org/countries/indicators/SI.DST.10TH.10?per_page=200"
urlpopul <- "http://api.worldbank.org/countries/indicators/SP.POP.TOTL?per_page=200"
urlgdpnom <- "http://api.worldbank.org/countries/indicators/NY.GNP.ATLS.CD?per_page=200"

# creation of empty dataframes for the data

low10 <- get_data(urllow10,30,"decile1")
first <- get_data(urlfirst,30,"decile2")
secnd <- get_data(urlsecnd,30,"decile4")
third <- get_data(urlthird,30,"decile6")
forth <- get_data(urlforth,30,"decile8")
fifth <- get_data(urlfifth,30,"decile9")
top10 <- get_data(urltop10,30,"decile10")
totpop <- get_data(urlpopul,3,"totpop")
gdpnom <- get_data(urlgdpnom,3,"gdpnom")


# create income_dist dataframe

income_dist <- Reduce(function(...) merge(..., by = c("country", "date")),
                      list(low10[,c(2:3,5)], first[,c(2:3,5)], secnd[,c(2:3,5)], 
                           third[,c(2:3,5)], forth[,c(2:3,5)], fifth[,c(2:3,5)], 
                           top10[,c(2:3,5)]))

# determining a value for all deciles

income_dist$decile2 <- income_dist$decile2 - income_dist$decile1
income_dist$decile9 <- income_dist$decile9 - income_dist$decile10
income_dist$decile3 <- income_dist$decile4/2
income_dist$decile4 <- income_dist$decile4/2
income_dist$decile5 <- income_dist$decile6/2
income_dist$decile6 <- income_dist$decile6/2
income_dist$decile7 <- income_dist$decile8/2
income_dist$decile8 <- income_dist$decile8/2
income_dist$tot_income <- income_dist$decile1 + income_dist$decile2 + income_dist$decile3 + 
  income_dist$decile4 + income_dist$decile5 + income_dist$decile6 + income_dist$decile7 + 
  income_dist$decile8 + income_dist$decile9 + income_dist$decile10

#
# the following routine estimates values for deciles that are consistent with the values for
# quintiles from the original data.
#
# The process is long, and can take up to 15 minutes.
#

condit <- function(i) {
  return(as.numeric((income_dist$decile10[i] - income_dist$decile9[i])>(income_dist$decile9[i]-income_dist$decile8[i]))*
           as.numeric((income_dist$decile9[i] - income_dist$decile8[i])>(income_dist$decile8[i]-income_dist$decile7[i]))*
           as.numeric((income_dist$decile8[i] - income_dist$decile7[i])>(income_dist$decile7[i]-income_dist$decile6[i]))*
           as.numeric((income_dist$decile7[i] - income_dist$decile6[i])>(income_dist$decile6[i]-income_dist$decile5[i]))*
           as.numeric((income_dist$decile6[i] - income_dist$decile5[i])>(income_dist$decile5[i]-income_dist$decile4[i]))*
           as.numeric((income_dist$decile5[i] - income_dist$decile4[i])>(income_dist$decile4[i]-income_dist$decile3[i])))}

for (i in 1:nrow(income_dist)) {
  while (condit(i) == 0) {
    if (income_dist$decile8[i] < income_dist$decile9[i]) {
      income_dist$decile8[i] = income_dist$decile8[i] + 0.05
      income_dist$decile7[i] = income_dist$decile7[i] - 0.05
      while (condit(i) == 0) {
        if (income_dist$decile6[i] < income_dist$decile7[i]) {
          income_dist$decile6[i] = income_dist$decile6[i] + 0.05
          income_dist$decile5[i] = income_dist$decile5[i] - 0.05
          while (condit(i) == 0) {
            if (income_dist$decile4[i] < income_dist$decile5[i]) {
              income_dist$decile4[i] = income_dist$decile4[i] + 0.05
              income_dist$decile3[i] = income_dist$decile3[i] - 0.05
            }else{
              income_dist$decile4[i] = (income_dist$decile4[i] + income_dist$decile3[i])/2
              income_dist$decile3[i] = income_dist$decile4[i]
              break }
          }
        }else {
          income_dist$decile6[i] = (income_dist$decile6[i] + income_dist$decile5[i])/2
          income_dist$decile5[i] = income_dist$decile6[i]
          break }
      }
    }else {break}
  }

}

#
# add population and nominal gdp
#

income_dist <- Reduce(function(...) merge(..., by = "country"), list(income_dist, 
               totpop[,c(2,5)], gdpnom[,c(2:3,5)]))

#
# Replace distribution of income per decile for US$ per decile that would make
# broadband affordable (h = estimation of income that should be used for broadband services).
# Currently, h = 5% / Source: ITU Broadband Commission
#

h <- 0.05

income_dist$decile1 <- h*((income_dist$decile1/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile2 <- h*((income_dist$decile2/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile3 <- h*((income_dist$decile3/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile4 <- h*((income_dist$decile4/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile5 <- h*((income_dist$decile5/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile6 <- h*((income_dist$decile6/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile7 <- h*((income_dist$decile7/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile8 <- h*((income_dist$decile8/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile9 <- h*((income_dist$decile9/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12
income_dist$decile10 <- h*((income_dist$decile10/income_dist$tot_income)*income_dist$gdpnom/(income_dist$totpop/10))/12

#
# The only place I could find the monthly price basket for fixed broadband services is the 
# Information Society page of the World Development Indicators. However, I could not
# find the information society data under the API in data.worldbank
# so I downloaded the file as a csv and loaded it to add the price information.
# The database can be found at: http://wdi.worldbank.org/table/5.12 
# save as csv and check path to file in the code
#
# Then, I add fixed broadband basket and broadband penetration as a numeric variable to the dataframe
#

ITU_data <- read.csv("./Projects 2016/Other/Affordability/theinformationsociety.csv", 
            header = FALSE, skip = 4, col.names = c("country","HHwTV", "HHwPC", 
            "Internet penetration", "Broadband penetration", "bps/user Intl bandwidth", 
            "Fixed broadband basket", "Secure servers/million people", "ICTGoodsExports", 
            "ICTGoodsImports","ICTServiceExports"))

income_dist <- merge(income_dist, ITU_data[,c(1,5,7)], by = "country", all.x = TRUE,rm.na = FALSE)
income_dist$Fixed.broadband.basket <- as.numeric(as.character(income_dist$Fixed.broadband.basket))
income_dist$Broadband.penetration <- as.numeric(as.character(income_dist$Broadband.penetration))

#
# Get rid of NA in Fixed Broadband Basket
#

c <- vector(mode = "numeric")
for (i in 1:nrow(income_dist)) {
  if (is.na(income_dist$Fixed.broadband.basket[i] == TRUE)) {
    c <- append(c,i)
  }
}

income_dist <- income_dist[-c,]

#
# Add country classification tables. The table can be found as an excel file at the bottom of
# http://data.worldbank.org/about/country-and-lending-groups
# I use the xlsx package to read the file and then import the different groups
# You will need to update the path to the file
#

if (require("xlsx") == FALSE) {
  install.packages("xlsx")
}
require(xlsx)

class <- read.xlsx("./Projects 2016/Other/Affordability/CLASS.XLS",sheetIndex = 1,startRow = 5,
                    header = TRUE)
income_dist <- merge(income_dist,class[,c(3,4,6,7)],by.x="country",by.y="Economy", all.x = TRUE)

#
# Calculation of % of population and population that can afford broadband
#

cty_ls <- list(income_dist$decile1, income_dist$decile2, income_dist$decile3, income_dist$decile4
               , income_dist$decile5, income_dist$decile6, income_dist$decile7, income_dist$decile8
               , income_dist$decile9, income_dist$decile10)

for (i in 1:nrow(income_dist)) {
  income_dist$per_unaffordable[i] = 0
  income_dist$pop_unaffordable[i] = 0
  for (j in 1:10) {
    if (cty_ls[[j]][[i]]<income_dist$Fixed.broadband.basket[i]) {
      income_dist$per_unaffordable[i] <- income_dist$per_unaffordable[i] + 0.1
      income_dist$pop_unaffordable[i] <- income_dist$pop_unaffordable[i] + 0.1*income_dist$totpop[i]
    }
  }
}


# clean environment

income_dist$tot_income <- NULL
income_dist <- income_dist[,c(1,18,19,20,2,3,4,10,5,11,6,12,7,8,9,13,14,15,16,17,21,22)]
remove(c,class,condit,cty_ls,h,i,j,first,fifth,forth,get_data,gdpnom,ITU_data,low10,secnd,third,top10,totpop,urlfifth,
       urlfirst,urlforth,urlgdpnom,urllow10,urlpopul,urlsecnd,urlthird,urltop10)

# write a csv with the dataframe

write.csv(income_dist, file = "./Projects 2016/Other/Affordability/tidydata.csv")

#
# Create general graphs to show population that can't afford broadband
#
# Create tables for graphs
#

if (require("plyr") == FALSE) {
  install.packages("plyr")
}

require("plyr")

# table for regions

per_region <- as.data.frame(xtabs(income_dist$pop_unaffordable ~ income_dist$Region))
per_region <- per_region[order(per_region$Freq),]
per_region$income_dist.Region <- factor(per_region$income_dist.Region,
                                        levels = per_region$income_dist.Region[order(per_region$Freq)])

#table for regions w/ China and India isolated

per_region2 <- per_region
per_region2 <- rename(per_region2, replace = c("income_dist.Region" = "country", "Freq" = "pop_unaffordable"))
per_region2 <- rbind(per_region2, subset(income_dist, income_dist$country == "India" | income_dist$country == "China",
                          select = c(country, pop_unaffordable)))
per_region2[per_region2$country == "South Asia",2] <- per_region2[per_region2$country == "South Asia",2] - per_region2[per_region2$country == "India",2]
per_region2[per_region2$country == "East Asia & Pacific",2] <- per_region2[per_region2$country == "East Asia & Pacific",2] - per_region2[per_region2$country == "China",2]
per_region2 <- per_region2[order(per_region2$pop_unaffordable),]
per_region2$country <- factor(per_region2$country, levels = per_region2$country[order(per_region2$pop_unaffordable)])



if (require("ggplot") == FALSE) {
  install.packages("ggplot")
}

require(ggplot)

# graph of unaffordability per region

g <- ggplot(per_region, aes(x=income_dist.Region))
g + geom_bar(aes(y = per_region$Freq/1000000), stat = "identity")

# graph of unaffordability with China and India isolated

h <- ggplot(per_region2, aes(x=country))
h + geom_bar(aes(y = per_region2$pop_unaffordable/1000000), stat = "identity")


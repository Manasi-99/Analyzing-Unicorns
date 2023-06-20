# Author: Manasi Todankar
# IST 719: Information Visualization
# POSTER: UNICORN COMPANIES
# filename: mptodank-final-poster-script.R


# Path to Directory
file.choose()
my.dir <- "F:\\SYRACUSE UNIVERSITY\\Spring 2022\\IST 719 Information Visualization\\Data\\"


# Loading the data from the csv file to a dataframe.
fname <- paste0(my.dir, "Unicorn_Companies.csv")
Companies <- read.csv(fname,
                header = TRUE,
                stringsAsFactors = FALSE) 


# Getting the dimensions of the dataset.
dim(Companies)

# Taking a view at the dataset.
View(Companies)

# Checking for null values.
is.null(Companies)

str(Companies)
summary(Companies)


#--------------------------------------------------
# Installing packages
#--------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

#library(RColorBrewer)
#display.brewer.all()

#install.packages("randomcoloR")
#library(randomcoloR)

#install.packages("ggwordcloud")
#library(ggwordcloud)

install.packages("wordcloud")
library(wordcloud)  

install.packages("ggthemes")
library(ggthemes)

install.packages("dplyr")
library(dplyr)
#---------------------------------------------------

colnames(Companies)[2] <- "ValuationBillions"
# colnames(Companies)[4] <- "region"
View(Companies)

# Defining a color palette
unicorn.palette <- c("#792F7A", "#310B58", "#542F5D", "#5E4891", "#CD6889",
                     "#212F7A", "#150653", "#952054", "#6D0139", "#310008",
                     "#094A63", "#1C7676", "#20A6A2", "#00C5CD", "#009ACD",
                     "#F6B23E", "#EF514A", "#DE1D3B")

table(Companies$Founded.Year)

CIndustry <- as.data.frame(table(Companies$Industry))
View(CIndustry)

# Percentage of each industry
perc <- as.data.frame((CIndustry$Freq*100)/1037)
perc
colnames(perc)[1] <- "percentage"
perc


# ---------------------------------------------------
# Presence of each industry 
# PIE CHART
# ---------------------------------------------------
ggplot(Companies) +
  aes(x = "", y = "Industry", fill = Industry) +
  geom_bar(width = 0.4, stat = "identity") +
  coord_polar("y", start = 45) +
  labs(title = "Presence of Industries") +
  scale_fill_manual(values = unicorn.palette) +
  theme_minimal()




# ---------------------------------------------------------
# Which Investors are good at finding future UCs?
# WORDCLOUD
# ---------------------------------------------------------
table(Companies$Select.Inverstors)
table(Companies$Financial.Stage)

# Removing the commas
investor.names <- unlist(strsplit(Companies$Select.Inverstors, "\\,"))
investor.names

class(investor.names)

#sort(table(all.tags), decreasing = TRUE)

# Removing spaces before and after the words
investor.names <- gsub("^\\s+|\\s+$", "", investor.names)
investor.names

table(investor.names)
head(investor.names)

# Sorting investors based on number of companies they have invested in
investor.names2 <- as.data.frame(sort(table(investor.names), decreasing = TRUE))
head(investor.names2)  
View(investor.names2)

size.A <- sqrt(investor.names2$Freq/max(investor.names2$Freq))
size <- round(9 * size.A, 0) + 1
range(size)
plot(size)


# Color palette for wordcloud
myPalFun <- colorRampPalette(c("#6D0139", "#792F7A", "#5E4891", "#20A6A2"))
wordcloud.col <- myPalFun(max(size))[size]

par(mar = c(0,0,0,0))
wordcloud(investor.names2$investor.names, size,
          scale = c(3, .5),
          min.freq = 4,
          max.words = Inf,
          random.order = FALSE,
          random.color = FALSE,
          ordered.colors = TRUE,
          rot.per = .5,
          colors = wordcloud.col)





# ---------------------------------------------------------
# TOP 10 cities housing UCs in terms of numbers
# BARPLOT
# ---------------------------------------------------------
table(Companies$City)

City.UC <- sort(table(Companies$City), decreasing = TRUE)
City.UC

City.UC <- as.data.frame(City.UC[1:10])
View(City.UC)


Cities.UC <- ggplot(data = City.UC, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = unicorn.palette) +
  theme_minimal() +
  theme(legend.position = "none") 
Cities.UC




# ------------------------------------------------------------------
# TOP 10 cities housing UCs in terms of combined valuation
# BARPLOT
# ------------------------------------------------------------------
Companies$ValuationBillions <- as.numeric(gsub("\\$", "", Companies$ValuationBillions))
View(Companies)

Valuation <- aggregate(Companies$ValuationBillions, 
                       list(city = Companies$City),
                       sum)
View(Valuation)

Valuation <- Valuation[order(-Valuation$x),]
Valuation

Valuation <- head(Valuation, n = 10)
Valuation

Cities.valuation <- ggplot(data = Valuation, aes(x = city, y = x, fill = city)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = unicorn.palette) +
  ylim(c(0,700)) +
  theme_minimal() +
  theme(legend.position = "none") 
Cities.valuation




# ------------------------------------------------------------------
# How are the Companies performing since their inception?
# SCATTERPLOT
# ------------------------------------------------------------------
Companies[Companies == "Acq"] <- "Acquired"
Company.year <- Companies[Companies$Founded.Year != "None",]
View(Company.year)

ggplot(Company.year, aes(x = Founded.Year, y = ValuationBillions, color = Financial.Stage)) +
  geom_point(size = 5, alpha = 0.7) +
  scale_fill_manual(values = unicorn.palette) +
  theme_minimal()




# ------------------------------------------------------
# TOP 10 Unicorns
# BARPLOT
# ------------------------------------------------------
new.Companies <- head(Companies, n = 10)
View(new.Companies)

top10 <- ggplot(data = new.Companies, aes(x = Company, y = ValuationBillions, fill = factor(ValuationBillions))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = unicorn.palette) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") 
top10




# -----------------------------------------------------------------
# How have the Start ups risen to become Unicorn Companies?
# WORLDMAP
# -----------------------------------------------------------------
map.country <- as.data.frame(sort(table(Companies$Country), decreasing = TRUE))
map.country

# Some data cleaning
colnames(map.country)[1] <- "region"
map.country$region <- as.character(map.country$region)

# Altering the names of some coutries to match map data
map.country$region[map.country$region == 'United States'] <- "USA"
map.country$region[map.country$region == 'United Kingdom'] <- "UK"
map.country

# Combining world map data and our dataset
mdata <- left_join(map.country, map_data("world"), by = "region")
View(mdata)

# Defining a colour palette for the world map.
map.col2 <- c('#5AB5EE', '#54ACE3', '#4EA3D7', '#499ACC', '#4390C1', '#3D87B6', 
              '#377EAA', '#31759F', '#2C6C94', '#266389', '#205A7D', '#1A5172',
              '#144767', '#0F3E5C', '#093550', '#032C45')


ggplot() + 
  geom_polygon(data = map_data("world"),
               aes(x = long, y = lat, group = group),
               fill = "snow2") +
  geom_polygon(data = mdata,
               aes(x = long, y = lat, group = group, fill = factor(Freq))) +
  scale_fill_manual(values = map.col2) +
  coord_map(xlim=c(-180,180)) +
  theme(legeng.position = "none") +
  theme_map() 


#map.col <- c("#2D27DF", "#3830E1", "#4439E2", "#4F42E4", "#5B4BE5",
#             "#6654E7", "#725DE8", "#7D66EA", "#896FEC", "#9477ED",
#            "#9F80EF", "#AB89F0", "#B692F2", "#C29BF3", "#CDA4F5", 
#             "#D9ADF6", "#E4B6F8")




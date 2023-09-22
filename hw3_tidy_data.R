nameOfCoder <- "Your Name Here"

library(foreign) #read.spss
library(stringr)
library(dplyr)
library(tidyr)

## PREG ##
set.seed(1014)

preg <- matrix(c(NA, sample(20, 5)), ncol = 2, byrow = T)
colnames(preg) <- paste0("treatment", c("a", "b"))
rownames(preg) <- c("John Smith", "Jane Doe", "Mary Johnson")

#tidy_preg <- 
dim(tidy_preg) == c(6,3)

## PEW ##

# Load data

pew <- suppressWarnings(read.spss("pew.sav"))
# Data from http://pewforum.org/Datasets/Dataset-Download.aspx
pew <- as.data.frame(pew)
pew <- pew[c("q16", "reltrad", "income")]

#tidy_pew <-
dim(tidy_pew) == c(180,3)


## TB ##

# Load
raw <- read.csv("tb.csv", na.strings = "")
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)
names(raw)[1] <- "country"

names(raw) <- str_replace(names(raw), "new_sp_", "")
raw$m04 <- NULL
raw$m514 <- NULL
raw$f04 <- NULL
raw$f514 <- NULL


# Melt

#melt_tb <- 
# dim(melt_tb) == c(2273,4)

# Break up variable in to sex and age

# age key for your convenience.
#ages <- c("04" = "0-4", "514" = "5-14", "014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-44", "4554" = "45-54", "5564" = "55-64", "65"= "65+", "u" = NA)

#tidy_tb <- 
# dim(tidy_tb) == c(2273,5)


## WEATHER ##

# Read in the Data

# function for reading in the fixed width weather data
read.customFixedWidth <- function(path, cols) {
  raw_stations <- readLines(path)
  stations <- data.frame(matrix(ncol = 0, nrow = length(raw_stations)))
  
  for(i in 1:nrow(cols)) {
    field <- cols[i, ]
    stations[[field$name]] <- str_trim(str_sub(raw_stations, field$start, field$end))
  }
  stations[stations == ""] <- NA
  stations[] <- lapply(stations, type.convert, as.is = TRUE)
  
  stations
}


# Define format for fixed width file #
cols <- data.frame(
  name =  c("id", "year", "month", "element"),
  start = c(1,     12,    16,      18),
  end =   c(11,    15,    17,      21))

names <- paste0(c("value", "mflag", "qflag", "sflag"), "_", rep(1:31, each = 4))
starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
starts <- starts[-length(starts)]
ends <- c(starts[-1], starts[length(starts)] + 1) - 1

values <- data.frame(name = names, start = starts, end = ends)
cols <- rbind(cols, values)

# Load data and subset to small example
raw <- read.customFixedWidth("weather.txt",  cols)

# Melt and tidy

#melt_weather <- 
#dim(melt_weather) == c(66,4)
# Cast

#tidy_weather <- 
#dim(tidy_weather) == c(33,4)
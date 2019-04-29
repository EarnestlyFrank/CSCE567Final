#Code by Frank Webb
#removes the '$' from price strings so it can be cast as numeric
noCash <- function(x) {
  as.numeric(gsub("\\$", "", x))
}

#function for rough conversion of SI memory notation to bits
toByteValue <- function(x){
  magnitude <- substr(paste(rev(strsplit(x, NULL)[[1]]),collapse = ""), 1, 1)
  value <- substr(x, 1, nchar(x)- 1)
  if(magnitude == "M"){
    value = as.numeric(value) * 1024 * 1024
    floor(value)
  }
  else if(magnitude == "k"){
    value = as.numeric(value) * 1024
    floor(value)
  }
  #used for "varies with device" string in table
  else -1
}

##loading CSVs from downloaded from Kaggle
library(readr)
apple <- (read_csv("AppleStore.csv"))
google <- (read_csv("googleplaystore.csv"))

#Removes apps with text encoding artifacts
apple <- apple[grep("[:alnum:]", apple$track_name[]), ]
google <- google[grep("[:alnum:]", google$App[]), ]

#Alphabetizes and removes apps with the same names
#Not required for Apple because of the unique id values
google <- google[order(google$App),]
google <- google[!duplicated(google$App),]

#str_length is a relic of eariler attempts to do coarser name matching
#between data sets by looking only at the first n characters
#Set to 10000 to capture full length
str_length <- 10000

#match provides the index of the shared apps for both datasets
match.google <- match(strtrim(apple$track_name[], str_length), strtrim(google$App, str_length))
match.apple <- match(strtrim(google$App, str_length), strtrim(apple$track_name[], str_length))

#removes rows where no match (NA) was found
match.google <- match.google[!is.na(match.google)]
match.apple <- match.apple[!is.na(match.apple)]

#formats the price string
google$Price <- noCash(google$Price)
apple$price <- noCash(apple$price)

#Apple data already in bytes
google$Size <-  unlist(apply(google[,"Size"], FUN = toByteValue, MARGIN = 1))

#creates abbreviated apple and google tables with
#just the matching apps, then combines the columns
goog <- google[match.google,]
appl <- apple[match.apple, ]
goog <- goog[order(goog$App),]
appl <- appl[order(appl$track_name),]
apps <- cbind(goog, appl[,c(1:2, 4:17)])

paid.index <- which(apple$price > 0)
free.index <- which(apple$price == 0)

#adds a column for type of app
type <- c()
type[free.index] <- "Free"
type[paid.index] <- "Paid"

apple$type <- type


#writes the tables to active directory
write.csv(apps, "combined.csv", row.names = F)
write.csv(google, "google_cleaned.csv", row.names = F)
write.csv(apple, "apple_cleaned.csv", row.names = F)

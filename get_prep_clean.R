#Written by Stephen Zielke, 2016
#downloads and cleans 2011 Canadian federal election data

library(dplyr)
library(ggmap)
#library(gpclib)
library(maptools)
#library(rgdal)
library(sp)

#getting data
#setwd("") #Set if you need to
dir.create("raw")
dir.create("2011_data")
setwd("raw")
download.file("http://ftp2.cits.rncan.gc.ca/pub/geott/electoral/2011/pd308.2011.zip",
              destfile="shape.zip")
unzip("shape.zip")
download.file("http://www.elections.ca/scripts/OVR2011/34/data_donnees/pollbypoll_bureauparbureau_canada.zip",
              destfile="polls.zip")
unzip("polls.zip")
setwd("..")


#------------------
#functions

#get_poll_num
#returns a numeric vector containing the poll numbers of each row in the results
#requires - vector: vector of polling station numbers
#returns - numeric vector
get_poll_num <- function(raw.vec){
  new.int <- NULL
  raw.vec <- sub(" ","",raw.vec)
  graw <- gregexpr("[0-9]{1,3}",raw.vec)
  for (i in 1:length(raw.vec)){
    new.int <- c(new.int,substr(raw.vec[i],1,attr(graw[[i]],"match.length")))
  }#for
  as.numeric(new.int)
}#get_poll_num

#calc_polls
#calculates the number of polling regions
#requires - vector: id column (poll number)
#returns - numeric 
calc_polls <- function(id){
  total <- 0; current <- 0
  for (i in 1:length(id)){
    if (current != id[i]){
      if (id[i] == (current + 1)){
        total <- total + 1
        current <- id[i]
      }#if
      else {break}
    }#if
  }#for
  total
}#calc_polls

#to_numeric
#ensures all poll results are numeric type
#requires - data.frame: poll.data
#returns - data.frame: poll.data
to_numeric <- function(raw.vec){
  new.vec <- NULL
  for (i in 1:length(raw.vec)){
    x <- as.integer(as.character(raw.vec[i]))
    if (is.na(x)){new.vec <- c(new.vec,NA)}
    else {new.vec <- c(new.vec,x)}
  }#for i
  new.vec
}#to_numeric

#affiliation
#creates a data frame with names and party affiliations
#requries - vector: candidate names
#         - vector: raw names and affiliations
#returns - data.frame
affiliation <- function(n,dat){
  p <- c("Libertarian","Liberal","Conservative","NDP","Green","Bloc")
  o <- 1 #number of other parties
  party <- NULL
  x <- gsub("[ -]",replacement = ".",dat)
  x <- gsub("[*]?",replacement = "",x)
  for (i in 1:length(n)){
    index <- grep(n[i],x)
    temp <- strsplit(x[index],split="[./]")[[1]]
    temp <- temp[3:length(temp)]
    new.party <- intersect(temp,p)
    if (length(new.party) == 0) {new.party <- paste("Other.",o,sep=""); o<-o+1}
    party <- c(party,new.party)
  }#for i
  data.frame(nam=n,party)
}#affiliation

#names_to_party
#converts candidate names to party affiliation
#requires - vector: candidate names
#         - numeric: riding number
#returns - vector: party affiliations
names_to_party <- function(n,num){
  dat <- read.csv(file="raw/table_tableau12.csv",skip=1,header=FALSE,encoding="latin1")
  dat <- as.character(filter(dat, V3 == num)$V4)
  affil <- affiliation(n,dat)
  x <- NULL
  for (i in 1:length(n)) {
    temp <- grep(n[i],affil$n)
    temp <- as.character(affil$party[temp])
    x <- c(x,temp)
  }#for
  x
}#names_to_party

#condense_polls
#condenses the poll.data data frame to only contain
#parties, results and id's
#requires - data.frame: poll.data
#         - numeric: riding number
#returns - data.frame: poll.data
condense_polls <- function(poll.data,num){
  new.poll <- data.frame()
  candidates <- ncol(poll.data) - 4
  party.names <- names_to_party(names(poll.data)[1:candidates],num)
  names(poll.data) <- c(party.names,"Rejected","Total","Electors","id")
  for (i in 1:calc_polls(poll.data$id)){
    temp <- filter(poll.data, id == i)
    if (nrow(temp) == 1){
      new.poll <- rbind(new.poll,temp)
    }#if
    else{
      temp <- as.data.frame(lapply(temp,sum,na.rm=TRUE))
      temp$id <- i
      new.poll <- rbind(new.poll,temp)
    }#else
  }#for
  new.poll
}#condense_polls

#find_poll_winner
#determins the winner of each polling station
#requires - data.frame: poll.data
#returns - data.frame: winners
find_poll_winner <- function(poll.data){
  Winner <- NULL; Percentage <- NULL; Turnout <- NULL
  for (i in 1:nrow(poll.data)){
    temp <- as.vector(poll.data[i,1:(ncol(poll.data)-4)])
    x <- max.col(temp)
    if (is.na(x)){
      Winner <- c(Winner,NA)
      Percentage <- c(Percentage,NA)
      Turnout <- c(Turnout,NA)
    }#if
    else{
      Winner <- c(Winner,names(poll.data)[x])
      p <- 100*(poll.data[i,x]/poll.data$Total[i])
      Percentage <- c(Percentage,p)
      t <- 100*(poll.data$Total[i]/poll.data$Electors[i])
      Turnout <- c(Turnout,t)
    }#else
  }#for
  data.frame(Winner,Percentage,Turnout)
}#find_poll_winner

#get_poll_data
#gets and cleans the poll data
#requires - integer: riding number
#returns - data.frame: polling data
get_poll_data <- function(num){
  f <- paste("raw/pollbypoll_bureauparbureau",num,".csv",sep="")
  poll.data <- read.csv(file=f,encoding="latin1")
  poll.data$id <- get_poll_num(poll.data$Polling.Station.Number.Numéro.du.bureau.de.scrutin)
  poll.data <- poll.data[,5:ncol(poll.data)]
  poll.data[,1] <- to_numeric(poll.data[,1])
  poll.data <- condense_polls(poll.data,num)
  poll.data <- cbind(poll.data,find_poll_winner(poll.data))
  #temp <- assign_parties(poll.data$Winner,num)
  #print(c(nrow(poll.data),nrow(temp)))
  #poll.data <- cbind(poll.data,temp)
  poll.data
}#get_poll_data

#get_party
#gets the pary from a vector of winners
#requires - character: winner with name and party
#returns - character: winning party
get_party <- function(winner){
  parties <- c("Liberal","Conservative","Bloc","Green","NDP")
  x <- strsplit(as.character(winner), split="[ /-]")[[1]]
  x <- intersect(x,parties)
  x
}#get_party

#get_province
#gets the english name from the winners csv
#requires - character: province name
#returns - character: province name
get_province <- function(p){
  prov <- c("Newfoundland and Labrador", "New Brunswick", "Alberta", "Manitoba", "Nova Scotia",
            "Ontario", "Quebec", "Yukon", "British Columbia", "Northwest Territories",
            "Nunavut", "Prince Edward Island", "Saskatchewan")
  x <- strsplit(as.character(p), split="/")[[1]]
  x <- intersect(x,prov)
  x
}#get_province

#clean_riding_names
#removes french riding names
#requires - vector: riding names
#returns - vector: riding names
clean_riding_names <- function(riding.name){
  temp <- NULL
  for (i in 1:length(riding.name)){
    a <- strsplit(as.character(riding.name[i]),split="/")[[1]]
    if (length(a) > 1) {a <- a[1]}
    temp <- c(temp,a)
  }#for
  temp
}#clean_riding_names

#get_winners
#reads in csv of winners and removes french
#requires - NULL
#returns - data.frame: winner data cleaned
get_winners <- function(){
  outcomes <- read.csv(file="raw/table_tableau11.csv",encoding="latin1")
  outcomes <- outcomes[,c(1:3,13)]
  names(outcomes) <- c("Province","Riding.name","id","Elected")
  x <- sapply(outcomes$Elected, get_party)
  outcomes$Elected <- x
  x <- sapply(outcomes$Province, get_province)
  outcomes$Povince <- x
  outcomes$Riding.name <- clean_riding_names(outcomes$Riding.name)
  outcomes
}#remove_poutine

#clean_data
#cleans and writes new data
#requires - numeric: riding id
#returns - NULL
clean_data <- function(num){
  poll <- get_poll_data(num)
  f.name <- paste("2011_data/",num,"_results.csv",sep="")
  write.csv(poll,file=f.name,row.names=FALSE)
  f.name <- paste("2011_data/",num,"_map.csv",sep="")
  map <- rss[rss$FED_NUM == num,]
  map <- fortify(map,region="PD_NUM")
  write.csv(map,file=f.name,row.names=FALSE)
  print(num)
}#clean_data

#commands
rss <- readShapeSpatial("raw/pd_a")
outcomes <- get_winners()
gpclibPermit()
a <- lapply(outcomes$id, clean_data)
write.csv(outcomes,file="2011_data/winners.csv")


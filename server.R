#app displays winners of polling regions of ridings in the Canadian 2011 federal election
#writen by Stephen Zielke, 2016

library(ggplot2)
library(RColorBrewer)
library(shiny)

#reading is basic data and creating variables
winners <- read.csv(file="2011_data/winners.csv")

#variables for colours
p.name <- c("Liberal","Conservative","NDP","Bloc","Green","Libertarian","Other","Other.1","Other.2","Other.3")
p.colour <- c("#F08080","#6495ED","#F4A460","#87CEFA","#2F873E","#ffcc00","#6f6db0","#6f6db0","#6f6db0","#6f6db0")

#get_ridings
#retrieves the names of the ridings in a province
#requires - character: province name
#returns  - vector: riding names
get_ridings <- function(province){
  temp <- grep(province,winners$Province)
  temp <- as.character(winners$Riding.name[temp])
  temp
}#get_ridings

#get_id
#gets the riding id number based on name
#requires - character: riding name
#returns  - integer: riding id number 
get_id <- function(riding){
  temp <- grep(riding,winners$Riding.name,fixed=TRUE)
  if (length(temp) > 1) {temp <- temp[1]}
  temp <- as.character(winners$id[temp])
  temp
}#get_id

#load_results
#loads csv file with results
#requires - integer: riding id number
#returns  - data.frame: results
load_results <- function(id){
  results.f <- paste("2011_data/",id,"_results.csv",sep="")
  read.csv(file=results.f)
}#load_results

#load_map
#loads map csv
#requires - integer: riding id number
#returns  - data.frame: map data
load_map <- function(id){
  map.f <- paste("2011_data/",id,"_map.csv",sep="")
  map <- read.csv(file=map.f)
  map
}#load_map

#make_title
#creates the map title
#requires - integer: riding id number
#returns  - character: map title
make_title <- function(id){
  temp <- grep(id,winners$id)
  temp <- winners[temp,]
  paste(temp$Riding.name[1], " : ", temp$Elected[1],sep="")
}

#make_colour
#creates the colour pallete for the map
#requires - vector: names of poll winning parties
#returns  - colour scale
make_colour <- function(won.poll){
  c.current <- NULL
  for (i in 1:length(won.poll)) {
    index <- grep(won.poll[i],p.name)
    c.current <- c(c.current,p.colour[index])
  }#for
  names(c.current) <- levels(won.poll)
  col.scale <- scale_fill_manual(name = "grp",values = c.current)
  col.scale
}#make_colour

#make_plot
#plots map
#requires - character: riding name
#returns  - plot: ggplot2 
make_plot <- function(riding){
  id <- get_id(riding)
  results <- load_results(id)
  map <- load_map(id)
  points <- merge(map,results,by="id")
  plot.title <- make_title(id)
  col.scale <- make_colour(as.character(levels(results$Winner)))
  p <- ggplot(data=points, aes(x=long,y=lat,group=id, fill=Winner, alpha=Percentage)) +
    geom_polygon() + geom_path(color="White") + col.scale + ggtitle(plot.title)
  p
}#make_plot

shinyServer(
  function(input,output){
    
    output$riding.list <- renderUI({
      inProv <- input$province
      if (is.null(inProv)) {inProv <- "Alberta"}
      riding.list <- get_ridings(inProv)
      selectInput("riding","Riding",choices=riding.list)
      })
    
    output$plot <- renderPlot({
      make_plot(input$riding)
    })  
  }
)
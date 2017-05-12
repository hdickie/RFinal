#shinyScript

library(cluster)
library(ggplot2)
library(Rtsne)
library(magrittr)
library(dplyr)
library(shiny)
library(fmsb)
library(gridExtra)

#TODO suppress function masking warnings
#pkgs <-c('dplyr','shiny','stats','graphics','utils','base')
#for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, 
#                                                      character.only=TRUE))

attach(tags) #for access to functions that make HTML elements

data <- read.csv("youngPeople_responses.csv")

columns <-read.csv("youngPeople_columns.csv",stringsAsFactors = FALSE)
columns[2,1] <- "I prefer fast paced music."
categoricalResponses <- c(74,75,108,109,133,145,146,147,148,149,150)

catChoices <- list()
catChoices[[1]] <- c("never smoked","tried smoking","former smoker","current smoker")
catChoices[[2]] <- c("never","social drinker", "drink a lot")
catChoices[[3]] <- c("i am often early", "i am always on time", "i am often running late")
catChoices[[4]] <- c("never", "only to avoid hurting someone", "sometimes", "everytime it suits me")
catChoices[[5]] <- c("no time at all","less than an hour a day","few hours a day","most of the day")
catChoices[[6]] <- c("female","male")
catChoices[[7]] <- c("left handed","right handed")
catChoices[[8]] <- c("currently a primary school pupil", "primary school", "secondary school","college/bachelor degree")
catChoices[[9]] <- c("no","yes")
catChoices[[10]] <- c("city","village")
catChoices[[11]] <- c("house/bungalow","block of flats")

#shorter label name
names(data)[2] <- "Faster Songs"

numericalResponses <- 141:143
scaleResponses <- c(1:73,76:107,110:132,134:144)

data[is.na(data)] <- 3
#for (i in scaleResponses) {
#  data[which(is.na(data[,i])),i] <- 3
#}

#select first choice as default value for categorical responses
for (i in categoricalResponses) {
  data[which(is.na(data[,i])),i] <- levels(data[,i])[2]
}

myAnswers <- c(5,4,2,1,4,4,2,5,2,1,1,5,3,2,2,5,2,3,2,2,1,1,5,2,3,2,4,5,5,1,3,2,2,2,5,3,5,4,3,2,2,3,2,3,2,1,1,2,1,3,3,3,2,4,2,2,1,2,5,5,5,1,1,2,1,1,4,5,1,2,3,3,2,"tried smoking","social drinker",1,5,2,3,2,2,1,5,4,4,4,4,3,1,4,5,1,5,3,5,5,3,5,5,4,1,3,1,1,4,2,4,"i am always on time","never",4,1,4,3,4,5,4,5,5,2,5,5,2,2,2,4,2,1,3,2,4,1,2,"few hours a day",3,4,3,1,1,1,2,20,180,64,4,"male","right handed","secondary school","no","city","house/bungalow")
names(myAnswers) <- names(data)
myAnswers[scaleResponses] <- lapply(myAnswers[scaleResponses],as.numeric)

###begin questions
q <- list()
p_cat <- 1
for (i in 1:150) {
  if (is.na(match(i,categoricalResponses))){
    q[[i]] <- radioButtons(inputId = paste("q",toString(i),sep=""), selected = 3, label = paste("Question ",toString(i),": ",columns[i,1],sep=""), choices = 1:5, inline = T)
  } else {
    q[[i]] <- radioButtons(inputId = paste("q",toString(i),sep=""), label = paste("Question ",toString(i),": ",columns[i,1],sep=""), choices = catChoices[[p_cat]], inline = T)
    p_cat <- p_cat + 1
  }
  
  if (!is.na(match(i,numericalResponses))){
    q[[i]] <- numericInput(inputId = paste("q",toString(i),sep=""), label = paste("Question ",toString(i),": ",columns[i,1],sep=""), value = 0, min = 0, max = 1000)
  }
}
###end questions

###begin radarchart
#just defining question categories on the data frame
musicCols <- as.data.frame(data[,1:19])
movieCols <- as.data.frame(data[,20:31])
hobbyCols <- as.data.frame(data[,32:63])
phobiaCols<- as.data.frame(data[,64:73])
healthCols <- as.data.frame(data[,74:76])
personalityCols <- as.data.frame(data[,77:133])
moneyCols <- as.data.frame(data[,134:140])
demogCols <- as.data.frame(data[,141:150])

#getting indices of scale questions
#these were for me to look at and should not be used in code
personality.numeric <- as.data.frame(personalityCols[,c(-32,-33,-57)])
demog.numeric <- demogCols[,c(1,2,3,4)]

hobb.1 <- c(1,4,5,7,8,10,12,14,15,28,31) #most dedication
hobb.2 <- c(2,3,9,11,17,18,22,24,25,29)
hobb.3 <- c(6,13,16,19,20,21,23,26,27,30,32) #least dedication

pers.emotion <- c(4,9,16,18,19,20,21,22,24,27,30,36,42,43,48,49)
pers.pers <- c(6,8,10,13,17,25,28,29,44,45,49,50,51)
pers.3 <- c(1,2,3,5,7,11,12,14,15,23,26,31)#cut out 32 and 33 bc categorical
pers.4 <- c(35,36,37,38,39,42,43,48,52,53,54)

scale.indices <- c(list(1:19),list(20:31),list(31 + hobb.1),list(31 + hobb.2),list(31 + hobb.3),list(64:73),list(pers.emotion + 76),list(pers.pers + 76),list(pers.3 + 76),list(pers.4 + 76),list(133 + 1:4))

#could have used scale.indices here, but writing it this way is more clear.
dfs <- list(musicCols,
            movieCols,
            hobbyCols[,hobb.1], hobbyCols[,hobb.2], hobbyCols[,hobb.3],
            phobiaCols,
            personality.numeric[,pers.emotion],personality.numeric[,pers.pers],personality.numeric[,pers.3],personality.numeric[,pers.4],
            moneyCols,
            demog.numeric)

#plotting mean results that will go on analysis page
radar.inputs <- list()
for (i in 1:length(dfs)) {
  curr <- dfs[[i]]
  
  max.rad <- apply(curr,2,max)
  min.rad <- apply(curr,2,min)
  
  mean.rad <- dfs[[i]] %>%
    apply(2,as.numeric) %>%
    apply (2,mean) %>%
    t() %>%
    as.data.frame()
  
  
  radar.temp <- rbind(max.rad,min.rad,mean.rad) %>%
    as.data.frame()
  
  
  radar.inputs[[i]] <- radar.temp
}

#TODO do this programmatically to get a better spectrum spread
color <- list(rgb(244/255,65/255,65/255,.5),
              rgb(244/255,155/255,65/255,.5),
              rgb(244/255,244/255,65/255,.5),
              rgb(157/255,244/255,65/255,.5),
              rgb(66/255,244/255,65/255,.5),
              rgb(66/255,244/255,146/255,.5),
              rgb(66/255,244/255,217/255,.5),
              rgb(66/255,209/255,244/255,.5),
              rgb(66/255,152/255,244/255,.5),
              rgb(66/255,92/255,244/255,.5),
              rgb(110/255,66/255,244/255,.5),
              rgb(155/255,66/255,244/255,.5))

#I did these inline because it was making html appear as plaintext otherwise
#par1 <- mainPanel(fluidRow(
#  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"))
#))
#par2 <- mainPanel(fluidRow(
#  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot4"), plotOutput("plot5"), plotOutput("plot6"))
#))
#par3 <- mainPanel(fluidRow(
#  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot7"), plotOutput("plot8"), plotOutput("plot9"))
#))
#par4 <- mainPanel(fluidRow(
#  splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot10"), plotOutput("plot11"))
#))


###end radarchart


###########BEGIN PAGE CONTENT
youngPeople <- imageOutput("youngPeople")

#begin text stuff
title <- p("Become a Cool Mom",align = "center",style="font-size:90px")
subtitle <- h4("Explore the preferences, interests, habits, opinions, and fears of young people",align = "center")
mainText <- mainPanel(br(),br(),p("In 2013, students of a Statistics class at a University in Bratislava, Slovakia were asked to invite their friends to fill out a survey of 150 questions that covered 8 different categories:",style="font-size:22px;margin-right:-700px"))
catList <- ul(li("Music preferences (19 items)"),li("Movie preferences (12 items)"),li("Hobbies & interests (32 items)"),li("Phobias (10 items)"),li("Health habits (3 items)"),li("Personality traits, views on life, & opinions (57 items) - 3 categorical"),li("Spending habits (7 items)"),li("Demographics (10 items) - 6 categorical"),style="margin-top:100px;font-size:22px;margin-left:80px;")
mainText2 <- mainPanel(br(),"These data were released under the CC0: Public Domain License. Lucky us!",style="font-size:22px;")
rqHead <- mainPanel()

r1.text <- "These three radar charts offer some easy insights- no love for Country or Opera music, Physics and Chemistry or Western movies! Comedy and fantasy are crowd pleasers- but history gets more love than adrenaline sports!"
r2.text <- "The only thing Slovakians hate more than Writing is Gardening, and Friends > The Internet > Outside. As long as we're not trying to tactfully persuade a pack of angry dogs to go vegan..."
r3.text <- "Compassion and Empathy for the gold! God is good but we don't fear rapture here. Authentic funny people these Slovakians- maybe I can borrow their jokes."
r4.text <- "Big on kids, ambivalent on the scale of personal dogs and achievements. Spending habits are remarkably average! I think I'd like to meet the average Slovakian."
#end text stuff

##begin your results text stuff
y1.text <- ""
y2.text <- ""
y3.text <- ""
y4.text <- ""
##end your results text stuff


page <- navbarPage("Young People Survey 2013",tabPanel("Home",title,subtitle,br(),youngPeople,mainText,catList,mainText2,rqHead),
                     tabPanel("Analysis",
                                mainPanel(r1.text,fluidRow(
                                  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"))
                                )),
                                mainPanel(r2.text,fluidRow(
                                  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot4"), plotOutput("plot5"), plotOutput("plot6"))
                                )),
                                mainPanel(r3.text,fluidRow(
                                  splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("plot7"), plotOutput("plot8"), plotOutput("plot9"))
                                )),mainPanel(r4.text,fluidRow(
                                  splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot10"), plotOutput("plot11"))
                                ))
                              ),
                     tabPanel("Take the Survey",q),
                     tabPanel("Your Results",mainPanel(y1.text,fluidRow(
                       splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("yplot1"), plotOutput("yplot2"), plotOutput("yplot3"))
                     )),
                     mainPanel(y2.text,fluidRow(
                       splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("yplot4"), plotOutput("yplot5"), plotOutput("yplot6"))
                     )),
                     mainPanel(y3.text,fluidRow(
                       splitLayout(cellWidths = c("33%", "33%","33%"), plotOutput("yplot7"), plotOutput("yplot8"), plotOutput("yplot9"))
                     )),
                     mainPanel(y4.text,fluidRow(
                       splitLayout(cellWidths = c("50%", "50%"), plotOutput("yplot10"), plotOutput("yplot11"))
                     )),
                              plotOutput(outputId = "scatterplot")))

###########END PAGE CONTENT

ui <- fluidPage(page)

server <- function(input, output) {
  
  ###begin turning input into radar input
  
  #chart 1 - 12 variables
  
  radar.inputs <- list()
  your.inputs <- list()
  
  ###end turning input into radar input
  
  for (i in 1:11) {
    
      local({
        curr <- dfs[[i]]
        nms <- names(curr)
        maxVec <- rep(5,length(curr))
        minVec <- rep(1,length(curr))
        
        my_i <- i
        output[[paste("plot",toString(i),sep="")]] <- renderPlot({
          inputVec <- apply(curr,2,mean) %>%
              t() %>%
              as.data.frame() 
          
          print(my_i)
          radar.inputs[[my_i]] <- rbind(maxVec,minVec,inputVec)
          radar.inputs[[my_i]] <- as.data.frame(radar.inputs[[my_i]])
          
          radarchart(radar.inputs[[my_i]],axistype = 0, pfcol = color[[my_i]])
          
        })
      })
    
    local({
      
      curr <- dfs[[i]]
      nms <- names(curr)
      maxVec <- rep(5,length(curr))
      minVec <- rep(1,length(curr))
      
      my_i <- i
      output[[paste("yplot",toString(i),sep="")]] <- renderPlot({
        
        #TODO get inputVec
        inputVec <- list()
        for (j in 1:length(scale.indices[[my_i]])) {
          inputVec[[j]] <- input[[paste("q",scale.indices[[my_i]][[j]],sep="")]]
        }
        inputVec <- as.numeric(inputVec)
        
        your.inputs[[my_i]] <- rbind(maxVec,minVec,inputVec)
        your.inputs[[my_i]] <- as.data.frame(your.inputs[[my_i]])
        names(your.inputs[[my_i]]) <- nms
        
        radarchart(your.inputs[[my_i]],axistype = 0, pfcol = color[[my_i]])
      })
    })
  }
  
  output$youngPeople <- renderImage({
    
    return(list(
      src = "young_people.jpg",
      contentType = "image/png",
      width = 272*3,
      height = 140*3,
      alt = "Mrs.George... is there alcohol in this?",
      style="display: block; margin-left: auto; margin-right: auto;"
    ))
  },deleteFile = FALSE)
  
  output$scatterplot <- renderPlot({
    set.seed(42)
    
    newRow <- list()
    for (i in 1:150) {
      newRow[[i]] <- input[[names(input)[[i]]]]

    }
    names(newRow) <- names(data)
    for (i in scaleResponses) {
      newRow[[i]] <- as.numeric(newRow[[i]])
    }
    
    data <- as.data.frame(rbind(data,newRow,myAnswers))
    
    #calculate cluster stuff
    gower_dist <- as.matrix(daisy(data, metric = "gower"))
    
    pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
    classifiedCluster <- pam_fit[[3]][[1011]] 
    tsne.obj <- Rtsne(gower_dist, is_distance = TRUE)
    
    tsne_data <- tsne.obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      mutate(cluster = factor(pam_fit$clustering))
    
    sz <- rep(2,1012)
    sz[[1011]] <- 10
    sz[[1012]] <- 6
    
    tsne_data$sz <- sz
    
    #plot projection
    ggplot(aes(x = X, y = Y), data = tsne_data) +
      geom_point(aes(color = cluster, size = sz))
    
    
  })
}

shinyApp(ui = ui, server = server)
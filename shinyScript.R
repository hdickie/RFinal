#shinyScript

library(cluster)
library(ggplot2)
library(Rtsne)
library(magrittr)
library(dplyr)
library(shiny)

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

numericalResponses <- 141:143
scaleResponses <- c(1:73,76:107,110:132,134:144)

for (i in scaleResponses) {
  data[which(is.na(data[,i])),i] <- 3
}

for (i in categoricalResponses) {
  data[which(is.na(data[,i])),i] <- levels(data[,i])[2]
}

myAnswers <- c(5,4,2,1,4,4,2,5,2,1,1,5,3,2,2,5,2,3,2,2,1,1,5,2,3,2,4,5,5,1,3,2,2,2,5,3,5,4,3,2,2,3,2,3,2,1,1,2,1,3,3,3,2,4,2,2,1,2,5,5,5,1,1,2,1,1,4,5,1,2,3,3,2,"tried smoking","social drinker",1,5,2,3,2,2,1,5,4,4,4,4,3,1,4,5,1,5,3,5,5,3,5,5,4,1,3,1,1,4,2,4,"i am always on time","never",4,1,4,3,4,5,4,5,5,2,5,5,2,2,2,4,2,1,3,2,4,1,2,"few hours a day",3,4,3,1,1,1,2,20,180,64,4,"male","right handed","secondary school","no","city","house/bungalow")
names(myAnswers) <- names(data)
myAnswers[scaleResponses] <- lapply(myAnswers[scaleResponses],as.numeric)

q <- list()
p_cat <- 1
for (i in 1:150) {
  if (is.na(match(i,categoricalResponses))){
    q[[i]] <- radioButtons(inputId = columns[i,2], selected = 3, label = paste("Question ",toString(i),": ",columns[i,1],sep=""), choices = 1:5, inline = T)
  } else {
    q[[i]] <- radioButtons(inputId = columns[i,2], label = paste("Question ",toString(i),": ",columns[i,1],sep=""), choices = catChoices[[p_cat]], inline = T)
    p_cat <- p_cat + 1
  }
  
  if (!is.na(match(i,numericalResponses))){
    q[[i]] <- numericInput(inputId = columns[i,2], label = paste("Question ",toString(i),": ",columns[i,1],sep=""), value = 0, min = 0, max = 1000)
  }
}

ui <- fluidPage(q,
                
                plotOutput(outputId = "scatterplot"))

server <- function(input, output) {
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
    pam_fit[[3]][[1011]] <- 3   #so that i have my own color
    pam_fit[[3]][[1012]] <- 4   #so that i have my own color
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
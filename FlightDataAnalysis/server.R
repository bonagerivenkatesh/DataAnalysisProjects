
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)


#path <- "./tempdataset" #dataset input to the program
path <- "./dataset" 
files <- dir(path, pattern = '\\.csv', full.names = TRUE)
tables <- lapply(files, read.csv)
flightdata<-do.call(rbind, tables)
citiesdest <- subset(flightdata, !duplicated(DEST_CITY_NAME))
citiesori <- subset(flightdata, !duplicated(ORIGIN_CITY_NAME))

shinyServer(function(input, output) {
  
  #dynamically loads the list of origin city names
  output$selector1 <- renderUI({
    selectizeInput("src", "Choose Source City:", choices=as.vector(citiesori$ORIGIN_CITY_NAME),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )) })
  
  #dynamically loads the list of destination names
  output$selector2 <- renderUI({
    selectizeInput("dest", "Choose Destination City:", choices=as.vector(citiesdest$DEST_CITY_NAME),
                   options = list(
                     placeholder = 'Please select an option below',
                     onInitialize = I('function() { this.setValue(""); }')
                   )) })
  

  #function to get the bisiest route based on the user selection input
  observeEvent(input$action1, {
    
    #output$updatetext1 <- renderText({"Updating the graph...Please Wait" })
  
    year = input$var1
    month = input$var2
    
    customdata = flightdata
    
    if(year!="All")
    {
      y=as.integer(year)
      customdata <- subset(customdata , customdata$YEAR == y)
      
    }
    
    if(month!="All")
    {
      m=as.integer(month)
      customdata <- subset(customdata , customdata$MONTH == m)
    }
    
    if(1)
    {
  
      withProgress(message = 'Getting the new Graph for Busiest Routes', value = 0, {
    
    
      uniqueorigin <-unique(customdata$ORIGIN_CITY_NAME)
      countorig <- length(uniqueorigin)
      total <- data.frame(DEST_CITY_NAME = character(0), count = numeric(0), ORIGIN_CITY_NAME = character(0))
      for (i in 1:countorig)
      {
        tempDB <- subset(customdata , ORIGIN_CITY_NAME == uniqueorigin[i])
        destgroupby <- group_by(tempDB,DEST_CITY_NAME)
        countme <- summarise(destgroupby, count=n())
        countme$ORIGIN_CITY_NAME=uniqueorigin[i]
        incProgress(1/countorig, detail = paste("Percentage Complete", i*100/countorig))
        total <- rbind(total, countme)
      }
      
      busiestroute <- total[order(total$count,decreasing=T)[1:10],]
      
    })
      
      output$plot1 <- renderPlot({ 
        ggplot(busiestroute, aes(x=factor(paste(as.character(busiestroute$ORIGIN_CITY_NAME),as.character(busiestroute$DEST_CITY_NAME),sep = ' to \n')),y=busiestroute$count))+ geom_bar(stat = "identity") +  labs(x = "Busiest Routes",y = "Number of Flights")
      })
      
      output$updatetext1 <- renderText({"  " })
      
      
    }
    else
    {
      output$updatetext1 <- renderText({"Data for the selected input is missing from the dataset" })
    }  
    
    
    
    })
  
  
  #function to get the airline delays based on the user selection input
  observeEvent(input$action2, {
    
    year = input$var3
    month = input$var4
    
    sourceair = input$src
    destair = input$dest
    
    if(sourceair!=destair | sourceair=="")
    {
    
      customdata = flightdata
    
    if(year!="All")
    {
      y=as.integer(year)
      customdata <- subset(customdata , customdata$YEAR == y)
      
    }
    
    if(month!="All")
    {
      m=as.integer(month)
      customdata <- subset(customdata , customdata$MONTH == m)
    }
    
    if(sourceair!="")
    {
      customdata <- subset(customdata , customdata$ORIGIN_CITY_NAME == sourceair)
    }
    
    if(destair!="")
    {
      customdata <- subset(customdata , customdata$DEST_CITY_NAME == destair)
    }
    
    
    if(1)  
    {
      if(month!="All")
      {
        withProgress(message = 'Getting the new Graph for Carrier Delay', value = 0, {
        
        airline <- (unique(customdata$CARRIER))
        num_of_airlines = length(airline)
        
        unique_days <- unique(customdata$DAY_OF_WEEK)
        unique_days <- unique_days[order(unique(customdata$DAY_OF_WEEK))]
        num_of_days = length(unique_days)
        
        delay <- customdata$ARR_DELAY_NEW
        
        total <- data.frame(DAY_OF_WEEK = character(0), AverageDelay = numeric(0), CARRIER = character(0) )
        
        
        for (i in 1:num_of_airlines)
        {
          
          
          tempDB <- subset(customdata , CARRIER == airline[i])
          destgroupby <- group_by(tempDB,DAY_OF_WEEK)
          
          countme <- summarise(destgroupby, count=mean(ARR_DELAY_NEW, na.rm = TRUE))
          
          
          countme$DAY_OF_WEEK <- as.character(countme$DAY_OF_WEEK)
          
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "1"] <- "Monday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "2"] <- "Tuesday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "3"] <- "Wednesday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "4"] <- "Thursday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "5"] <- "Friday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "6"] <- "Saturday"
          countme$DAY_OF_WEEK[countme$DAY_OF_WEEK == "7"] <- "Sunday"
          
          countme$CARRIER = airline[i]
          
          total <- rbind(total, countme)
          
          incProgress(1/num_of_airlines, detail = paste("Percentage Complete", i*100/num_of_airlines))
          
        }
        
        total$DAY_OF_WEEK <- factor(total$DAY_OF_WEEK, ordered = T, levels = c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        
        })
        
        output$plot2 <- renderPlot({ 
          ggplot(data=total, aes(x=DAY_OF_WEEK, y=count, group=CARRIER, colour=CARRIER)) +
            geom_line() +
            geom_point() +  labs(x = "Week Day",y = "Delays in Minutes", color = "Airline Carriers")
        })
        
        output$updatetext2 <- renderText({"  " })
        
      }
      else
      {
        
        withProgress(message = 'Getting the new Graph for Carrier Delay', value = 0, {
        
        airline <- (unique(customdata$CARRIER))
        num_of_airlines = length(airline)
        
        unique_months <- unique(customdata$MONTH)
        unique_months <- unique_months[order(unique(customdata$MONTH))]
        num_of_months = length(unique_months)
        
        delay <- customdata$ARR_DELAY_NEW
        
        total <- data.frame(MONTH = character(0), AverageDelay = numeric(0), CARRIER = character(0) )
        
        
        for (i in 1:num_of_airlines)
        {
          tempDB <- subset(customdata , CARRIER == airline[i])
          destgroupby <- group_by(tempDB,MONTH)
          
          countme <- summarise(destgroupby, count=mean(ARR_DELAY_NEW, na.rm = TRUE))
          
          
          countme$MONTH <- as.character(countme$MONTH)
          
          countme$MONTH[countme$MONTH == "1"] <- "January"
          countme$MONTH[countme$MONTH == "2"] <- "February"
          countme$MONTH[countme$MONTH == "3"] <- "March"
          countme$MONTH[countme$MONTH == "4"] <- "April"
          countme$MONTH[countme$MONTH == "5"] <- "May"
          countme$MONTH[countme$MONTH == "6"] <- "June"
          countme$MONTH[countme$MONTH == "7"] <- "July"
          countme$MONTH[countme$MONTH == "8"] <- "August"
          countme$MONTH[countme$MONTH == "9"] <- "September"
          countme$MONTH[countme$MONTH == "10"] <- "October"
          countme$MONTH[countme$MONTH == "11"] <- "November"
          countme$MONTH[countme$MONTH == "12"] <- "December"
          
          countme$CARRIER = airline[i]
          
          total <- rbind(total, countme)
          
          incProgress(1/num_of_airlines, detail = paste("Percentage Complete", i*100/num_of_airlines))
          
        }
        
        total$MONTH <- factor(total$MONTH, ordered = T, levels = c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
        
        })
        output$plot2 <- renderPlot({ 
          ggplot(data=total, aes(x=MONTH, y=count, group=CARRIER, colour=CARRIER)) +
            geom_line() +
            geom_point() + labs(x = "Month",y = "Delays in Minutes", color = "Airline Carriers")
        })
        
        output$updatetext2 <- renderText({"  " })
      }  
    }
    else
    {
      output$updatetext2 <- renderText({"Data for the selected input is missing from the dataset" }) 
    }
      
    
    
    }else
    {
      output$updatetext2 <- renderText({"Not a valid selection" })
    }
    
    
  })
  
  
  #function to get the busiest airport based on the user selection input
  
  observeEvent(input$action3, {
  
    year = input$var5
    month = input$var6
    
    customdata = flightdata
    
    if(year!="All")
    {
      y=as.integer(year)
      customdata <- subset(customdata , customdata$YEAR == y)
      
    }
    
    if(month!="All")
    {
      m=as.integer(month)
      customdata <- subset(customdata , customdata$MONTH == m)
    }
    
    
    if(1)
    {
      withProgress(message = 'Getting the new Graph for Busiest Airport', value = 0, {
      uniqueorigin <-unique(customdata$ORIGIN)
      countorig <- length(uniqueorigin)
      total <- data.frame(count = numeric(0), ORIGIN = character(0), ORIGIN_CITY_NAME = character(0))
      for (i in 1:countorig)
      {
        tempDB <- subset(customdata , ORIGIN == uniqueorigin[i])
        origingroupby <- group_by(tempDB,ORIGIN)
        countme <- summarise(origingroupby, count=n())
        countme$ORIGIN_CITY_NAME = unique(tempDB$ORIGIN_CITY_NAME)
        total <- rbind(total, countme)
        incProgress(1/countorig, detail = paste("Percentage Complete", i*100/countorig))
      }
      
      
      noofflights <- total[order(total$count,decreasing=T)[1:10],]
      
      BUSIEST_AIRPORT = noofflights$ORIGIN
      CITY = noofflights$ORIGIN_CITY_NAME
      NUMBER_OF_FLIGHTS = noofflights$count
      
      BUSIEST_AIRPORTS = paste(BUSIEST_AIRPORT, ",",CITY)
      
      })
      
      output$plot3 <- renderPlot({ 
        ggplot(noofflights, aes(x=BUSIEST_AIRPORTS,y=NUMBER_OF_FLIGHTS, color = factor(BUSIEST_AIRPORTS))) + geom_point(size = 5) +  labs(x = "BUSIEST AIRPORTS",y = "NUMBER OF Flights", color = "BUSIEST AIRPORTS")
      })
      
      output$updatetext3 <- renderText({"  " })
      
      #ggplot(noofflights, aes(x=BUSIEST_AIRPORTS,y=NUMBER_OF_FLIGHTS, color = factor(BUSIEST_AIRPORTS)))+ geom_point(size = 5) + geom_text_repel(aes(label=NUMBER_OF_FLIGHTS), size = 3) +  labs(x = "BUSIEST AIRPORTS",y = "NUMBER OF AIRPORTS", color = "BUSIEST AIRPORTS")  
    }
    else
    {
      output$updatetext3 <- renderText({"Data for the selected input is missing from the dataset" })
    }
    
      
  })
  

})

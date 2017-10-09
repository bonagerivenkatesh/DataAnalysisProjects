
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  navbarPage("Flight Data Analysis",
             tabPanel("Busiest Routes",
                      fluidPage(
                        
                        fluidRow(
                          helpText("Know the busiest routes."),
                          
                          selectInput("var1", 
                                      label = "Select year",
                                      choices = c("All", "2011",
                                                  "2012", "2013","2014", "2015"),
                                      selected = "All"),
                          selectInput("var2", 
                                      label = "Select month",
                                      choices = c("All","1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                      selected = "All"),
                          actionButton("action1","Submit")
                          
                        ),
                        
                        hr(),
                        textOutput("updatetext1"),
                        plotOutput("plot1")
                        
                      )),
             
             tabPanel("Carrier Delay",
                      fluidPage(
                        
                        fluidRow(
                          helpText("Know the carrier delays."),
                          
                          uiOutput("selector1"),
                          uiOutput("selector2"),
                          selectInput("var3", 
                                      label = "Select year",
                                      choices = c("All", "2011",
                                                  "2012", "2013","2014", "2015"),
                                      selected = "All"),
                          selectInput("var4", 
                                      label = "Select month",
                                      choices = c("All","1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                      selected = "All"),
                          actionButton("action2","Submit")
                          
                        ),
                        hr(),
                        textOutput("updatetext2"),
                        plotOutput("plot2")
                      )),
             tabPanel("Busiest airport",
                      fluidPage(
                        
                        fluidRow(
                          helpText("Number of flights from the busiest airport"),
                          selectInput("var5", 
                                      label = "Select year",
                                      choices = c("All", "2011",
                                                  "2012", "2013","2014", "2015"),
                                      selected = "All"),
                          selectInput("var6", 
                                      label = "Select month",
                                      choices = c("All","1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                                      selected = "All"),
                          actionButton("action3","Submit")),
                          hr(),
                        textOutput("updatetext3"),
                          plotOutput("plot3")
                      ))
))
)

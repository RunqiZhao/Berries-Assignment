library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(gridExtra)

load("strawberries.Rdata")

stb_d <- filter(stb,Values != "(NA)" & Values != "(D)" & Values != "(Z)")
stb_d$Values <- gsub(",", "",stb_d$Values)
stb_d$Values <- as.numeric(stb_d$Values)
stb_d$Year <- as.character(stb_d$Year)
stb_d_table <- stb_d
stb_d_plot1 <- stb_d
stb_d_plot2 <- stb_d

ui <- fluidPage(

    titlePanel("Strawberries"),
    
    sidebarLayout(
      tabsetPanel(
          conditionalPanel(
            'input.dataset === "stb_d_table"'),
          
          conditionalPanel(
            'input.dataset === "stb_d_plot1"'),
          
          conditionalPanel(
            'input.dataset === "stb_d_plot2" ',
             )
       ),
       
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            tabPanel("Strawberries Data",
                     fluidRow(
                       column(4,
                              selectInput("year",
                                          "Year: ",
                                          c("All",
                                            unique(stb_d$Year)))
                       ),
                       column(4,
                              selectInput("state",
                                          "State:",
                                          c("All",
                                            unique(stb_d$State)))
                       ),
                       column(4,
                              selectInput("pro",
                                          "Production:",
                                          c("All",
                                            unique(stb_d$Production)))
                       ),
                       column(4,
                              selectInput("measures",
                                          "Measures:",
                                          c("All",
                                            unique(stb_d$Measures)))
                       ),
                       column(4,
                              selectInput("materials",
                                          "Materials:",
                                          c("All",
                                            unique(stb_d$Materials)))
                       ),
                       column(4,
                              selectInput("chemical_1",
                                          "Chemical:",
                                          c("All",
                                            unique(stb_d$Chemical)))
                       ),
                     ),
                     DT::dataTableOutput("table")
            ),
            
            tabPanel("Chemical boxplot",
 
                     fluidRow(
                       column(4,
                              selectInput("chemical",
                                          "Chemical:",
                                          c("All",
                                            unique(stb_d$Chemical)))
                              
                       ),
                       column(4,
                              selectInput("c_s",
                                          "Class:",
                                          c("None","State"))
                              
                       ),
                     ),
                     plotOutput("ChemicalPlot"),
            ),
            
            tabPanel("Acres Planted",
                     
                     fluidRow(
                       column(4,
                              selectInput("state_2",
                                          "State:",
                                          c("All",
                                            unique(stb_d$State)))
                              
                       ),
                       column(4,
                              selectInput("year_2",
                                          "Year Comapre:",
                                          c("None",unique(stb_d$Year)))
                              
                       ),
                     ),
                     plotOutput("APPlot"),
            )
          )
        )
    )
)

server <- function(input, output) {

  output$table <- DT::renderDataTable(DT::datatable({
    data <- stb_d_table
    if (input$year != "All") {
      data <- data[data$Year == input$year,]
    }
    if (input$state != "All") {
      data <- data[data$State == input$state,]
    }
    if (input$pro != "All") {
      data <- data[data$Production == input$pro,]
    }
    if (input$measures != "All") {
      data <- data[data$Measures == input$measures,]
    }
    if (input$materials != "All") {
      data <- data[data$Materials == input$materials,]
    }
    if (input$chemical_1 != "All") {
      data <- data[data$Chemical == input$chemical_1,]
    }
    data
  }))
  
  output$ChemicalPlot <- renderPlot({
    ay1 <- filter(stb_d_plot1, Measures == " MEASURED IN LB / ACRE / APPLICATION")
    if (input$chemical == "All") {
      if(input$c_s == "None"){
        p <- ggplot(ay1, aes(x = Chemical, y = Values)) +
          geom_boxplot() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 13, face = "bold"))
      }
      else if(input$c_s == "State"){
        p <- ggplot(ay1, aes(x = Chemical, y = Values, color = State)) +
          geom_boxplot() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 13, face = "bold"))
      }
    }
    else if(input$chemical != "All") {
      ay2 <- filter(ay1, Chemical == input$chemical)
      if(input$c_s == "None"){
        p <- ggplot(ay2, aes(x = Chemical, y = Values)) +
          geom_boxplot() 
      }
      else if(input$c_s == "State"){
        p <- ggplot(ay2, aes(x = Chemical, y = Values, color = State)) +
          geom_boxplot() 
      }
      
    }
    p
  }
  )
  
  output$APPlot <- renderPlot({
    ay3 <- filter(stb_d, Production == " ACRES PLANTED")
    if (input$state_2 == "All") {
      if(input$year_2 == "None"){
        p <- ggplot(ay3, aes(x = State, y = Values)) +
             geom_boxplot() +
              theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 13, face = "bold"))
      }
      else if(input$year_2 != "None"){
        p <- ggplot(ay3, aes(x = Year, y = Values)) +
          geom_boxplot() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1),
                axis.text = element_text(size = 11),
                axis.title = element_text(size = 13, face = "bold"))
      }
    }
    else if(input$state_2 != "All") {
      ay4 <- filter(ay3, State == input$state_2)
      if(input$year_2 == "None"){
        p <- ggplot(ay4, aes(x = State, y = Values)) +
          geom_boxplot() 
      }
      else if(input$year_2 != "None"){
        p <- ggplot(ay4, aes(x = Year, y = Values,group = 1)) +
              geom_point() + geom_line()
      }
      
    }
    p
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

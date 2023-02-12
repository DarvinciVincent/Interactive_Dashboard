# Student name: Nghia Nguyen
# Student number: 000275466

library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)

data(mtcars)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
  h1("Exploratory Analysis on mtcars dataset"),
  h2("This app provides interactive user interface to perfom Exploratory Analysis on mtcars dataset"),
  p("Student name: Nghia Nguyen"),
  p("Student number: 000275466"),
  sidebarLayout(
    sidebarPanel(
      # Input 1:
      sliderInput("HorsePower", "Select the desired Horse Power", min = min(mtcars$hp), max = max(mtcars$hp), value = c(100,200)),
      
      # Input 2:
      checkboxGroupInput("GearType", "Gear Gear Types", choices = sort(unique(mtcars$gear)),
                         selected = sort(unique(mtcars$gear))),
      
      # Input 3:
      checkboxGroupInput("Cyclinders", "Select Cyclinders", choices = sort(unique(mtcars$cyl)),
                         selected = sort(unique(mtcars$cyl))),
      
      # Input 4:
      checkboxGroupInput("Transmission", "Transmission Types", choices = sort(unique(mtcars$am)),
                         selected = sort(unique(mtcars$am)))

    ),
    
    mainPanel(
      h4("Scatterplot - WT vs. MPG", style = "font-family:'times';font-si16pt"),
      h5("Scatter showing the relationship between wt and mpg in terms of Cylinder Values."),
      # Output 1: Scatter plot
      plotOutput("myscatter"), 
      br(),
      em("Plot Comment:"), div("We can infer from the scatterplot that there is negative correlation between mpg and weight. 
          The heavier the car is, the lesser the mileage it can drive.", style = "color:pink"),
      
      
        
      br(),
      br(),
      br(),
      h4("Scatterplot -CY vs. VS", style = "font-family:'times';font-si16pt"),
      h5("A scatter plot of cyl(Number of Cylinders) and vs(Engine Type(0 = V-shaped, 1 = straight)) according to am Transmission (0 = automatic, 1 = manual)"),
      # Output 3: Scatter plot
      plotOutput("myplot"),
      br(),
      em("Plot Comment:"), div("From the Scatterplot, we see that most of the cars with 8 cylinders are automatic transmission cars. On the other hand, those with 4 cylinders are mostly manual transmission car.",style = "color:pink"),
      
      
      br(),
      br(),
      br(),
      h4("Stacked Bar graph - GEAR", style = "font-family:'times';font-si16pt"),
      h5("A stacked bar graph of the number of each gear type and how they are further divided out by cyl."),
      # Output 2: Stacked bar graph
      plotOutput("my_stacked_bar_graph"),
      br(),
      em("Plot Comment:"), div("From the stacked bar, we see that cars with 3 gears in the dataset have mostly 8 cylinder engines whereas cars with 4 gears have mostly 4 cylinder engines.",style = "color:pink")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Output 1
  output$myscatter = renderPlot({
    p = mtcars %>%
      filter(hp >=  input$HorsePower[1] & hp <=  input$HorsePower[2])%>%
      filter(gear %in% input$GearType)%>%
      filter(cyl %in% input$Cyclinders)%>%
      filter(am %in% input$Transmission)%>%
      ggplot(aes(x = wt, y = mpg, col = factor(cyl), shape = factor(cyl))) +
      geom_point(size = 3, alpha = 0.6) +
      labs(x = "Weight", y = "Milage",caption = "Source: MTCARS Dataset")+
      ggtitle("Weight vs. Miles/(US) gallon")+
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  })
  
  
  # Output 2
  output$my_stacked_bar_graph = renderPlot({
    p2 = mtcars %>%
      filter(gear %in% input$GearType)%>%
      filter(cyl %in% input$Cyclinders)%>%
      filter(am %in% input$Transmission)%>%
      ggplot(aes(x = gear, fill = factor(cyl)))+
      geom_bar()+
      ggtitle("Distribution of Gears vs Cylinders")+
      labs(x = "Gear Types", y = "Frequency", caption = "Source: MTCARS Dataset")+
      theme(plot.title = element_text(hjust = 0.5))
    print(p2)
  })
  
  # Output 3
  output$myplot = renderPlot({
    p3 = mtcars %>%
      filter(hp >=  input$HorsePower[1] & hp <=  input$HorsePower[2])%>%
      filter(gear %in% input$GearType)%>%
      filter(cyl %in% input$Cyclinders)%>%
      filter(am %in% input$Transmission)%>%
      ggplot(aes(x = cyl, y = vs, col = factor(am), shape = factor(am))) +
      geom_jitter(width = 0.1, alpha = 0.5) +
      labs(x = "Cylinders", y = "Engine Type",caption = "Source: MTCARS Dataset")+
      ggtitle("Cylinders vs. Engine Type")+
      theme(plot.title = element_text(hjust = 0.5))
    print(p3)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

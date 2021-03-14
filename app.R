#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages('tidytuesdayR')
library(shiny)
library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2020-03-10')
tuesdata <- tidytuesdayR::tt_load(2020, week = 11)
tuition_cost <- tuesdata$tuition_cost
historical_tuition <- tuesdata$historical_tuition

# Use this when exceeding 'tidytuesdayR' usage limits 
# historical_tuition <- read.csv(file = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

# Clean data set
historical_tuition <-
    historical_tuition %>%
    mutate(
        year_only = strsplit(year, "-") %>% 
            as.data.frame() %>% 
            t %>% 
            data.frame(stringsAsFactors = F) %>% 
            pull(1) %>%
            as.numeric()
    ) %>%
    filter(type == "All Institutions") %>%
    filter(tuition_type == "4 Year Constant") %>%
    filter(year_only >= 2000)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Year Range"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("starting_year",
                        "Starting Year:",
                        min = 2000,
                        max = 2016,
                        value = 2010)
            ),         

        # Show a plot of the generated distribution
        mainPanel(
            h1("Tuition Fee of 4-Year Colleges and Universities in the US"),
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        historical_tuition %>% 
            filter(year_only > input$starting_year) %>% 
            ggplot(aes(x = year_only, y = tuition_cost)) + 
            theme_minimal() + 
            geom_smooth() +
            labs(x = "Year",
                 y = "Tuition Fee")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

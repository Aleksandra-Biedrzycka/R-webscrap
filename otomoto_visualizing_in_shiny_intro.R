#Exercise 0: Shiny example
# library(shiny)
 # runExample("01_hello")      # a histogram
 # runExample("02_text")       # tables and data frames
 # runExample("03_reactivity") # a reactive expression
 # runExample("04_mpg")        # global variables
 # runExample("05_sliders")    # slider bars
 # runExample("06_tabsets")    # tabbed panels
 # runExample("07_widgets")    # help text and submit buttons
 # runExample("08_html")       # Shiny app built from HTML
 # runExample("09_upload")     # file upload wizard
 # runExample("10_download")   # file download wizard
 # runExample("11_timer")      # an automated timer

#Exercise 1: Shiny Toyota histograms
library(shiny)

passat <- read.csv('toyota.csv')

ui <- fluidPage(
  titlePanel("Volvo histograms"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:",
                  c("Price" = "price",
                    "Age" = "car_age",
                    "Mileage" = "mileage",
                    "Fuel" = "fuel")),
      
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)


server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(passat[,input$variable], breaks = input$bins, col = "#75AADB", border = "white",
         xlab = input$variable,
         main = "Histogram")
  })
}

shinyApp(ui = ui, server = server)

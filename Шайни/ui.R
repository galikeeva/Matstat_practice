ui <- function(request){fluidPage(  
    titlePanel("COVID-19. Dead"),
    fluidRow(
        column(3,checkboxGroupInput("Country", "Choose the countries",
                                    c("Russia","Italy", "USA", "France"),
                                    selected = "Russia")
        ),
        column(3,radioButtons("real_for", "Type of plot",
                              c("Data", "Data and forecast"))
        ),
        column(3,conditionalPanel("input.real_for === 'Data and forecast'",
                                  radioButtons("model", "Model", 
                                               c("Additive", "Multiplicative"))))
    ),
    column(3,sliderInput("past", "Days for past data",
                         min = 30, max = 90,
                         value = 30, step = 10)
    ),
    column(3,conditionalPanel("input.real_for === 'Data and forecast'",
                              sliderInput("forecast", "Days for forecast",
                                          min = 7, max = 35,
                                          value = 7, step = 7))
    ),
    plotOutput("death_plot")
)
}

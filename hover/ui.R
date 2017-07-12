library( shiny )
library( tidyverse )
library( gridExtra )

ui <- fluidPage(
    plotOutput("main", hover = "plot_hover", height = 600)
)

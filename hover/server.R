server <- function(input, output) {
    ## Stores the most recent non-NULL hover state
    values <- reactiveValues( hover_save = NULL )
    
    ## Finds the closest point using the most recent non-NULL hover state
    getContext <- reactive({
        if( !is.null( input$plot_hover ) ) { values$hover_save <- input$plot_hover }
        nearPoints( mtcars, values$hover_save, threshold = 10, maxpoints = 1 )
    })

    ## Generates a ggplot associated with the hover context
    plotContext <- reactive({
        ctx <- getContext()
        ctx %>% select( wt, drat, mpg, qsec ) %>% gather( Variable, Value ) %>%
            ggplot( aes( x = Variable, y = Value ) ) + geom_bar( stat="identity" ) + theme_bw() +
            theme( plot.background = element_rect( color = "gray", fill = "gray90" ),
                  text = element_text(face="bold", size=12),
                  axis.text = element_text(face="bold", size=12) ) +
            ggtitle( rownames(ctx) )
    })

    ## Main plot that shows the data and hover-sensitive inset
    output$main <- renderPlot({
        ## Plot the data and retrieve hover context
        gg <- ggplot( mtcars, aes( x = wt, y = mpg ) ) + geom_point() + theme_bw() +
            theme( text = element_text(face="bold", size=14),
                  axis.text = element_text(face="bold", size=14) )
        ctx <- getContext()
        if( nrow(ctx) == 0 ) return( gg )

        ## Compute the four corners of the context plot in data coordinates
        x0 <- ctx$wt; x1 <- x0 + sign( mean(mtcars$wt) - x0 ) * diff( range(mtcars$wt) ) * 0.25
        y0 <- ctx$mpg; y1 <- y0 + sign( mean(mtcars$mpg) - y0 ) * diff( range(mtcars$mpg) ) * 0.5

        ## Aggregate the data and context plot
        gg + annotation_custom( grob = ggplotGrob( plotContext() ),
                               xmin = min( x0, x1), xmax = max( x0, x1 ),
                               ymin = min( y0, y1), ymax = max( y0, y1 ) )
    })
}

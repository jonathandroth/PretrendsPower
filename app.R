#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Power Analysis for Pre-trends Tests"),
    
    fileInput(inputId = "betahat",
              label = "Upload event-study estimates in csv form",
              accept = c(".csv")),
    
    fileInput(inputId = "sigma",
              label = "Upload event-study covariance matrix in csv form",
              accept = c(".csv")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tableOutput(outputId = "table"), width = 6
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #betahat_df <- reactive({read.csv(input$filedata$betahat)})
    
    # if(is.null(input$betahat)){
    #     df <- read.csv("example_beta.csv")
    # }else{
    #     df <- reactive(input$betahat)
    # }
    #req(input$betahat)    
    
    #df <- read.csv("example_beta.csv")
    
    get_betahat_df <- reactive(
        if(is.null(input$betahat)){
            read.csv("example_beta.csv")
        }else{
            read.csv(input$betahat$datapath)
        }
    )
    
    get_sigma <- reactive(
        if(is.null(input$sigma)){
            as.matrix(read.csv("example_sigma.csv"))
        }else{
           as.matrix(read.csv(input$sigma$datapath))
        }
    )

    
    #output$table <- renderDT(betahat_df)
    output$distPlot <- renderPlot({
        df <- get_betahat_df()
        sigma <- get_sigma()
        df$se <- sqrt(diag(sigma))
        
        beta_alt <- 0.05*(df$t+1)
        
        rejectionProbability_NIS <- 
            function(betaPre, SigmaPre, thresholdTstat.Pretest = 1.96){
                require(mvtnorm)
                
                ub <- sqrt( diag( as.matrix(SigmaPre) ) ) * thresholdTstat.Pretest
                lb <- -ub
                
                #Delte the rownames and colnames of SigmaPre, since pmvnorm complains if these don't match each other
                rownames(SigmaPre) <- NULL
                colnames(SigmaPre) <- NULL
                
                power <- 1- 
                    pmvnorm(mean = betaPre,
                            sigma = SigmaPre,
                            lower = lb,
                            upper =  ub)
                
                return(power)
            }
        
        prePeriodIndices <- which(df$t < -1)
        betaPreActual <- df$betahat[prePeriodIndices]
        betaPreAlt <- beta_alt[prePeriodIndices]
        sigmaPre <- sigma[prePeriodIndices, prePeriodIndices]
        
        power_against_betaalt <- rejectionProbability_NIS(betaPre = betaPreAlt, SigmaPre = sigmaPre)
        power_against_0 <- rejectionProbability_NIS(betaPre = 0*betaPreAlt, SigmaPre = sigmaPre)    
        
        
        likelihood_betaalt <- dmvnorm(x = betaPreActual, mean = betaPreAlt, sigma = sigmaPre)
        likelihood_0 <- dmvnorm(x = betaPreActual, mean = 0*betaPreAlt, sigma = sigmaPre)
        # draw the histogram with the specified number of bins
        ggplot2::ggplot(data = df, aes(x = t, y = betahat, ymin = betahat - 1.96* se, ymax = betahat + 1.96*se)) + 
            geom_point(aes(color = "Estimated Coefs")) + geom_pointrange() +
            geom_point(aes(y=beta_alt, color = "Hypothesized Trend"), size = 2.5) +
            geom_line(aes(y=beta_alt, color = "Hypothesized Trend")) + 
            scale_color_manual(values = c("black", "red"), name = "")
    })
    
    # output$table <- renderTable(data.frame(Power = power_against_betaalt, 
    #                                        `Bayes Factor` = power_against_betaalt / power_against_0,
    #                                        `Likelihood Ratio` = likelihood_betaalt / likelihood_0 )) 
    ##Compute power against 
}

# Run the application 
shinyApp(ui = ui, server = server)

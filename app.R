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
              label = "Upload a csv file containing event-study estimates (with columns t and beta).",
              accept = c(".csv"),
              width = 600),
    
    fileInput(inputId = "sigma",
              label = "Upload a csv file with the event-study covariance matrix",
              accept = c(".csv"),
              width = 600),
    
    fileInput(inputId = "betatrue",
              label = "Upload a csv file with the hypothesized difference in trends (with column beta_true)",
              accept = c(".csv"),
              width = 600),
    
    checkboxInput(inputId = "showMeanAfterPretest", label = "Show Mean After Pre-Testing", value = FALSE, width = NULL),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tableOutput(outputId = "table"), width = 6
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           h1("Instructions"),
           p("This app enables power calculations for pre-trends tests for event-study designs. It also allows for evaluation of the distortions from pre-testing."),
           p(HTML("See <a href = https://jonathandroth.github.io/assets/files/roth_pretrends_testing.pdf>Pre-test with Caution: Event-Study Estimates After Testing for Parallel Trends (Roth, 2020)</a> for more details.")),
           h2("Inputs"),
           p("The user inputs 3 csv files containing:"),
           p(HTML("<b> Event Study Coefficients: </b> A CSV file with a column <i> beta </i> with the coefficients, and a column <i> t </i> with the event-time, where 0 corresponds with the first treated period and -1 is normalized to 0 (the coefficient for -1 can be omitted). <a href=https://github.com/jonathandroth/PretrendsPower/blob/master/example_beta.csv>Example</a>")),
           p(HTML("<b> Covariance matrix: </b> A CSV file with the variance-covariance matrix for the event-plot. <a href=https://github.com/jonathandroth/PretrendsPower/blob/master/example_sigma.csv>Example</a>")),
           p(HTML("<b> Hypothesized Trend: </b> A CSV file containing the hypothesized counterfactual trend in the column <i> beta_true </i>.  <a href=https://github.com/jonathandroth/PretrendsPower/blob/master/example_beta_true.csv>Example</a>")),
           p(HTML("The event-study estimates can come from any asymptotically normal estimator, including two-way fixed effects models, or the estimators of Callaway & Sant'Anna (2020), Sun & Abraham (2020), or Freyaldenhoven et al (2019).")),
           p("There is also a check-box:"),
           p(HTML("<b> Show Mean After Pre-Testing: </b> If this checkbox is clicked, then the mean event-study coefficients conditional on passing the pre-test are displayed.")),
           h2("Outputs"),
           p(HTML("<b> Power: </b> the probability that no significant pre-period coefficient would be detected under the hypothesized trend.")),
           p(HTML("<b> Bayes Factor: </b> the relative probability that no significant pre-period coefficient would be detected under the hypothesized trend relative to under parallel trends.")),
           p(HTML("<b> Likelihood Ratio: </b> the relative likelihood of the estimated coefficients under the hypothesized trend relative to under parallel trends.")),
           p(HTML("<b> An Event Plot: </b> displays the estimated coefficients and SEs, the hypothesized true trend. If <i> Show Mean After Pre-testing </i> is checked, it also displays the expected value of the coefficients conditional on not finding a significant pre-treatment coefficient under the hypothesized trend."))
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
    
    
    ## This function loads the user-inputted betahat if given, otherwise the default betahat_df
    get_betahat_df <- reactive(
        if(is.null(input$betahat)){
            read.csv("example_beta.csv")
        }else{
            read.csv(input$betahat$datapath)
        }
    )
    
    
    get_betatrue <- reactive(
        if(is.null(input$betatrue)){
            read.csv("example_beta_true.csv")$beta_true
        }else{
            read.csv(input$betatrue$datapath)$beta_true
        }
    )
    
    ## This function loads the user-inputted sigma if given, otherwise the default sigma
    get_sigma <- reactive(
        if(is.null(input$sigma)){
            as.matrix(read.csv("example_sigma.csv"))
        }else{
           as.matrix(read.csv(input$sigma$datapath))
        }
    )


    ##This function creates the dfs used to display the event-study plot and power resutls
    make_data_for_plots_and_tables <- function(){
        
        #Load the betahat and sigma
        df <- get_betahat_df()
        sigma <- get_sigma()

        #Remove column/row names for sigma, sicne this yields problems with mtvmnorm
        colnames(sigma) <- NULL
        rownames(sigma) <- NULL
        #sigma <- 0.5*(sigma + t(sigma)) #to avoid numerical precision issues, make sigma symmetric by construction
        
        if(is.null(df$t) | is.null(df$betahat)){stop("The csv with the event-studies must have columns t and betahat.")}
        if(nrow(df) != NROW(sigma)){stop("The dimension of the uploaded covariance matrix must correspond with the number of event-study coefficients. Did you upload a covariance matrix?")}
        
        df$se <- sqrt(diag(sigma))
        
        #beta_true <- 0.05*(df$t+1)
        beta_true <- get_betatrue()
        if(is.null(beta_true)){stop("The csv with they hypothesized difference in trends must have column beta_true")}
        if(nrow(df) != length(beta_true)){stop("The dimension of the hypothesized difference in trends must correspond with the number of event-study coefficients. Did you upload a beta_true?")}
        df$beta_true <- beta_true 
        
        #Function for computing the power of the non-individually significant pre-test
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
        
        #Function for computing the mean of betaPre after pre-testing
        meanBetaPre_NIS <- 
            function(betaPre, sigmaPre, thresholdTstat.Pretest = 1.96){
                require(tmvtnorm)
                
                ub <- sqrt( diag( as.matrix(sigmaPre) ) ) * thresholdTstat.Pretest
                lb <- -ub
                
                meanBetaPre <- tmvtnorm::mtmvnorm(mean = betaPre, sigma = sigmaPre,
                                                  lower = lb, upper = ub,
                                                  doComputeVariance = F)$tmean
                
                return(meanBetaPre)
            }
        #Function for computing the mean of betapost after pre-testing
        meanBetaPost_NIS <- function(beta, 
                                     sigma,
                                     prePeriodIndices = 1:(dim(as.matrix(sigma))[1] -1),
                                     postPeriodIndices = dim(as.matrix(sigma))[1],
                                     tVec = c( seq(-dim(as.matrix(sigma))[1] -1, -1), 1),
                                     referencePeriod = 0,
                                     thresholdTstat.Pretest = 1.96,
                                     eta = NULL,
                                     ...){
            
            betaPre <- beta[prePeriodIndices]
            sigmaPre <- sigma[prePeriodIndices, prePeriodIndices]
            
            betaPost <- beta[postPeriodIndices]
            tVecPost <- tVec[postPeriodIndices]
            
            sigma12 <- sigma[postPeriodIndices, prePeriodIndices]
            
            meanBetaPost <- betaPost + sigma12 %*% solve(sigmaPre) %*% 
                (meanBetaPre_NIS(betaPre = betaPre,
                                 sigmaPre = sigmaPre,
                                 thresholdTstat.Pretest = thresholdTstat.Pretest) -
                     betaPre)
            
            if(is.null(eta)){
                relativeT <- tVecPost - referencePeriod
                
                df <- data.frame(betaPostConditional = meanBetaPost,
                                 betaPostUnconditional = betaPost,
                                 relativeT = relativeT)
            }else{
                etaPost <- eta[postPeriodIndices]
                gammaPostConditional <- t(etaPost) %*% meanBetaPost
                gammaPostUnconditional <- t(etaPost) %*% betaPost
                
                df <- data.frame(betaPostConditional = gammaPostConditional,
                                 betaPostUnconditional = gammaPostUnconditional,
                                 relativeT = 1)
            }
            return(df)
        }
        
        
        #Extract the objets corresponding with the pre-period
        prePeriodIndices <- which(df$t < -1)
        postPeriodIndices <- which(df$t > -1)
        betaPreActual <- df$betahat[prePeriodIndices]
        betaPreAlt <- beta_true[prePeriodIndices]
        sigmaPre <- sigma[prePeriodIndices, prePeriodIndices]
        
        #Compute power against the alt trend and power against 0 (i.e. size of test)
        power_against_betatrue <- rejectionProbability_NIS(betaPre = betaPreAlt, SigmaPre = sigmaPre)
        power_against_0 <- rejectionProbability_NIS(betaPre = 0*betaPreAlt, SigmaPre = sigmaPre)    
        
        #COmpute likelihoods under beta=betaPreAlt and beta=0
        likelihood_betatrue <- dmvnorm(x = betaPreActual, mean = betaPreAlt, sigma = sigmaPre)
        likelihood_0 <- dmvnorm(x = betaPreActual, mean = 0*betaPreAlt, sigma = sigmaPre)
        
        #Compute the means after pre-testing
        meanBetaPre <- meanBetaPre_NIS(betaPre = betaPreAlt,sigmaPre = sigmaPre)
        meanBetaPost <- meanBetaPost_NIS(beta = beta_true, 
                                         sigma = sigma, 
                                         prePeriodIndices = prePeriodIndices,
                                         postPeriodIndices = postPeriodIndices,
                                         tVec = df$t)$betaPostConditional
        
        meanAfterPretesting_df <- data.frame(t= c(df$t[prePeriodIndices], df$t[postPeriodIndices], -1),
                                             meanAfterPretesting = c(meanBetaPre, meanBetaPost, 0 ))
        
        df <- dplyr::left_join(df, meanAfterPretesting_df, by = c("t"))
        #Create a data frame displaying the power, BF, and LR
        df_power <- 
            data.frame(Power = power_against_betatrue, 
                       `Bayes Factor` = power_against_betatrue / power_against_0,
                        `Likelihood Ratio` = likelihood_betatrue / likelihood_0 ) 
        
        
        #Return the dataframes in a list
        return(list(df_eventplot = df, df_power = df_power))
    }
    
    output$table <- renderTable(make_data_for_plots_and_tables()$df_power)

    output$distPlot <- renderPlot({

        # draw the histogram with the specified number of bins
        p<- ggplot2::ggplot(data = make_data_for_plots_and_tables()$df_eventplot, aes(x = t, y = betahat, ymin = betahat - 1.96* se, ymax = betahat + 1.96*se)) +
            geom_point(aes(color = "Estimated Coefs", shape = "Estimated Coefs")) + geom_pointrange() +
            geom_point(aes(y=beta_true, color = "Hypothesized Trend", shape = "Hypothesized Trend"), size = 2.5) +
            geom_line(aes(y=beta_true, color = "Hypothesized Trend")) +
            scale_color_manual(values = c("black", "red", "blue"), 
                               breaks = c("Estimated Coefs", "Hypothesized Trend", "Expectation After Pre-testing"), 
                               name = "") +
             scale_shape_discrete(#values = c(1,2,3),
                                breaks = c("Estimated Coefs", "Hypothesized Trend", "Expectation After Pre-testing"), 
                               name = "") +
            xlab("Relative Time") + ylab("") +
            ggtitle("Event Plot and Hypothesized Trends")
        
        
        if(input$showMeanAfterPretest){
            p <- p + 
                 geom_point(aes(y=meanAfterPretesting, color = "Expectation After Pre-testing", shape = "Expectation After Pre-testing"), size = 2.5) +
                 geom_line(aes(y=meanAfterPretesting, color = "Expectation After Pre-testing"), linetype = "dashed")
        }
        p
    })


}

# Run the application 
shinyApp(ui = ui, server = server)

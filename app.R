# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# List of required packages 
packages <- c('shiny', 'ggplot2', 'tidyverse', 'shinythemes', 'ggfortify', 'reshape2', 'broom','Hmisc')


# Install necessary packages
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {install.packages(setdiff(packages, rownames(installed.packages())))}

library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(reshape2)
library(broom)
library(Hmisc)

# Read in data
data <- read_csv('long_return.csv')

# Define UI for app that plots histograms, regression plots, and tests
# equalities of mean to investigate the October Effect
ui <- navbarPage(title = "",   theme = shinytheme("flatly"),
       # Introduction of the web app                  
       tabPanel("Introduction", verticalLayout(
       withTags ({
           div(class="desc",
               h3("This is our web app"),
               p("In this web app, we do some analysis about stock returns.
  "),
               p("The contents are as below.")
           )
       }),
       withTags ({
           div(class="desc",                            
               h3("Single Stock Analysis"),
               ul(li("A histogram of log-returns over more than one year"),
                  li("A plot to assess closeness of log returns data to a normal distribution"),
               li("The expected mean, variance, and their confidence intervals "),
               li("A regression of log returns over time "),
           ))
       }),
       withTags ({
           div(class="desc",
               h3("Compare Two Stocks"),
               ul(li("A plot comparing the log-returns distribution of 2 stocks"),
                  li("A t-test of equality of the means of the 2 stocks log-returns"),
                  li("A correlation test of the 2 stocks log-returns"),
                  li("A regression of log returns of one over the other "))
           )
       }))),
       
       # Single Stock Analysis
       tabPanel("Single Stock Analysis", sidebarLayout(
           sidebarPanel( selectizeInput('Stock', label = 'Stock Choice', 
                                        choices=c('GOOG', 'NVDA','AMZN','RPD','SPLK','PYPL','AAPL','SNPS','MU','MSFT')),
                         numericInput("ConfidenceInterval", "Confidence Interval:",
                                      min = 0.0, max = 1.0, value = 0.95, step = 0.05),
                         sliderInput("NumBins",
                                     "Number of Bins:",
                                     min = 1,
                                     max = 40,
                                     value = 30),
                         selectizeInput('RegRes', label = 'Regression Plot', 
                                        choices=c("Regression", "Residuals"))
           ), 
           mainPanel(
               plotOutput("ggHistPlot"),
               plotOutput("Normal_plot"),
               tableOutput("confTable"), 
               plotOutput("regressionPlot"),
               htmlOutput("regressionEQ"),
           ))),
       # Two Stock Analysis Tab 
       tabPanel("Compare Two Stocks", sidebarLayout(
           sidebarPanel(
               selectizeInput('Stock1', label = 'Stock 1', 
                              choices=c('GOOG', 'NVDA','AMZN','RPD','SPLK','PYPL','AAPL','SNPS','MU','MSFT')),
               selectizeInput('Stock2', label = 'Stock 2', 
                              choices=c('GOOG', 'NVDA','AMZN','RPD','SPLK','PYPL','AAPL','SNPS','MU','MSFT')),
               numericInput("Alpha", "Alpha:",
                            min = 0.0, max = 1.0, value = 0.05, step = 0.05),
               selectizeInput('RegResStocks', label = 'Regression Plot', 
                              choices=c("Regression", "Residuals"))
           ), 
           mainPanel(
               plotOutput("compareDistPlot"),
               htmlOutput("meanTestingStocks"),
               htmlOutput("corTestingStocks"),
               plotOutput("compareRegressionPlot"),
               htmlOutput("regressionEQ2Stocks")
           ))),
  
       # Panel about our team 
       tabPanel("About Us",verticalLayout(
           withTags ({
               div(class="welcome",
                   h3("You come to the end of our web app!"),
                   br(),
                   p("We have put a lot of heart and effort into this web app. Hope you enjoy!"),
                   br(),
                   p("Created by Jiawen Huang (jh4179),  Liuhaoyue Li (ll3351),  Yuchen Fei (yf2515)."),
                   br(),
                   p("Thanks."),
                   br(),br())
           }),
           img(src='https://pristhenewblackk.files.wordpress.com/2014/11/thank-you.jpg', width=400, height=300, align = "center")
       ))
  )

# Server containing 
server <- function(input, output, session) {
    # Plot histogram of the log returns for one stock
    output$ggHistPlot <- renderPlot({
        # Define the stock
        stock = input$Stock
        # Plot single histogram
        ggplot(data, aes(x = !!ensym(stock))) +
            geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5, bins = input$NumBins, color = 'royalblue', fill = 'skyblue') +
            geom_density(alpha=0.6) +
            xlab('Daily Log Returns') +
            ylab('Density') + 
            ggtitle(label=stock, subtitle = "All Days from 2016-01-04 to 2019-11-29") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 30), plot.subtitle = element_text(hjust = 0.5, size = 15)) 
    })
    # Render an table with estimates of the mean and variance
    # and include their confidence intervals
    output$confTable<-renderTable({
        # Define the stock
        stock = input$Stock
        summary = summarise(data, Average = mean(!!ensym(stock)), Variance = var(!!ensym(stock)), Sample = length(!!ensym(stock)))
        # Calculate mean, variance, and number of samples 
        xbar = summary$Average 
        sbar = summary$Variance
        nsamp = summary$Sample
        # Calculate mean confidence interval
        alpha = 1.0 - input$ConfidenceInterval 
        t = qt(alpha/2, df=nsamp-1)
        xbar_conf_lower = round(xbar+t*sqrt(sbar)/sqrt(nsamp), 6)
        xbar_conf_upper = round(xbar-t*sqrt(sbar)/sqrt(nsamp), 6)
        # Calculate confidence interval for variance 
        chi_lower = qchisq(alpha/2, df=nsamp-1)
        sbar_conf_lower = round(((nsamp - 1)*sbar)/chi_lower, 6)
        chi_upper = qchisq(1-alpha/2, df=nsamp-1)
        sbar_conf_upper = round(((nsamp - 1)*sbar)/chi_upper, 6)
        xbar_ci = paste("[", xbar_conf_lower, ", ", xbar_conf_upper, "]", sep="")
        sbar_ci = paste("[", sbar_conf_lower, ", ", sbar_conf_upper, "]", sep="")
        # Put all values into a data frame to display as table 
        tibble("Parameter" = c("Mean", "Variance"), "Estimate" = c(xbar, sbar), "Confidence Interval" = c(xbar_ci, sbar_ci))
    }, 
    width = "100%",
    digits = 4, 
    striped = TRUE)
    # Plot the regression of the log returns 
    output$regressionPlot <- renderPlot({
        # Define the stock 
        stock = input$Stock
        # Calculate the regression of the selcted stock over time
        fit <- lm(get(stock) ~ c(1:985), data = data)
        data$predicted <- predict(fit)
        data$residuals <- residuals(fit)
        rsq <- round(summary(fit)$r.squared, 3)
        # Function to decide which type of plot to show based on selection from user
        # User can choose to display regression or residuals 
        reg.plot <- function(type = c('Regression', 'Residuals')) {
            if (type == 'Regression') {
                ggplot(data, aes(x = c(1:985), y = !!ensym(stock))) +
                    geom_point() +
                    geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
                    xlab('Date') + 
                    ggtitle(label="Regression over Time") +
                    theme_bw() +
                    theme(plot.title = element_text(hjust = 0.5, size = 20)) +
                    geom_text(x=Inf,y=+Inf, hjust=2, vjust = 2, label = paste0("R2=", rsq), size = 5)
            } else if (type == 'Residuals'){
                plot(c(1:985),resid(fit),
                   ylab="Residuals" , xlab="Time"
                   , main="Residuals")
            }
        }
        reg.plot(type = input$RegRes)
    })
    # Display the results of the regression for a particular stock  
    output$regressionEQ <- renderText({
        stock = input$Stock
        # Get estimate information from the linear model
        fit <- lm(get(stock) ~ DATE, data = data)
        data$predicted <- predict(fit)
        data$residuals <- residuals(fit)
        fit <- tidy(fit)
        beta0 <- signif(fit[1,2],3)
        beta1 <- signif(fit[2,2],3)
        sign <- if_else(sign(beta1) == 1, "+", "-")
        paste("<center>", "<h5>", "<br>", "The regression of", "<b>", stock, "</b>", "over time is modeled by", "</h5>",
              "<h4>",stock, "=", "<b>", beta0, "</b>", sign , "<b>", abs(beta1), "</b>", "&#215", "time", "</h4>", "</center>", "<br>", "<br>",  sep= ' ')
    })
    # Plot Normal_plot to determine normality of distributions 
    output$Normal_plot <- renderPlot({
        # Define the stock 
        stock = input$Stock
        # Print Normal Probability Plot to find if the stock is close to a normal distribution
        ggplot(data, aes(sample = !!ensym(stock))) +
            stat_qq() + stat_qq_line() +
            xlab('Theoretical') + 
            ylab('Sample') + 
            ggtitle(label="Normal Probability Plot") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5, size = 20))
    })
    # Density plot comparing the distribution of two stocks
    # chosen by the user 
    output$compareDistPlot <- renderPlot({
        # Define stock 1 
        stock1 = input$Stock1
        # Define stock 2
        stock2 = input$Stock2
        # Subset data based on two chosen stocks to compare and melt dataframe for plotting
        data <- data %>% select(stock1, stock2)
        data <- melt(data)
        # Calculate mean for plotting
        m.data <- data %>% 
            group_by(variable) %>% 
            summarise_all(mean)
        # Density plot  
        ggplot(data, aes(x = value, fill=variable)) +
            geom_density(alpha=0.5, position="identity") +
            geom_vline(data=m.data, aes(xintercept=value, colour=variable), 
                       linetype='dashed', size=1) +
            ggtitle(label=paste(stock1, "vs", stock2, sep = " "), subtitle = "All Days from 2016-01-04 to 2019-11-19") +
            theme_bw() +
            xlab('Value') + 
            ylab('Density') + 
            theme(plot.title = element_text(hjust = 0.5, size = 30), plot.subtitle = element_text(hjust = 0.5, size = 15)) +
            geom_text(x=+Inf, y = Inf, hjust=1, vjust = 3, label = paste(stock1, signif(m.data$value[1],3), sep = " : "), size = 5) +
            geom_text(x=-Inf, y = Inf, hjust=0, vjust = 3, label = paste(stock2, signif(m.data$value[2],3), sep = " : "), size = 5)
    })
    # Compare the means of stock twos and conduct a t-test 
    output$meanTestingStocks<-renderText({
        # Define stock 1 
        stock1 = input$Stock1
        # Define stock 2
        stock2 = input$Stock2
        s1 <- data %>% 
            select(stock1) %>%
            unlist(use.names = F)
        s2 <- data %>% 
            select(stock2) %>%
            unlist(use.names = F)
        ttest = t.test(s1, s2, conf.level= 1-input$Alpha)
        tstatistic = ttest$statistic
        tpvalue = signif(ttest$p.value,2) 
        answer <- if_else(tpvalue > input$Alpha, "No", "Yes")
        paste("<center>", "<h5>",
              "Are the means of data", "<b>",stock1, "</b>", "and", "<b>", stock2, "</b>", "statistically different", "<br>", 
              "based on your chosen alpha of", "<b>", input$Alpha, "</b>", "?", "</h5>", 
              "<h4>", answer, ", because the p-value is", "<b>", tpvalue, "</b>", "</h4>", "<br>", sep = " ")
    })
    # Correlation of two stocks
    output$corTestingStocks<-renderText({
      # Define stock 1 
      stock1 = input$Stock1
      # Define stock 2
      stock2 = input$Stock2
      a <- rcorr(as.matrix(data[,c(stock1, stock2)]))
      correlation <- round(a$r[1,2],3)
      pvalue <- round(a$P[1,2],3)

      answer <- if_else(pvalue > input$Alpha, "No", "Yes")
      paste("<center>", "<h5>",
            "The correlation of", "<b>",stock1, "</b>", "and", "<b>", stock2, "</b>", "is", "</h5>","<h4>",
            correlation, ".", "</b>", "</h4>", "<h5>",
            "Are the correlation of", "<b>",stock1, "</b>", "and", "<b>", stock2, "</b>", "statistically different from 0", "<br>", 
            "based on your chosen alpha of", "<b>", input$Alpha, "</b>", "?", "</h5>", 
            "<h4>", answer, ", because the p-value is", "<b>", pvalue, "</b>", "</h4>", "<br>", sep = " ")
    })
    # Regression plot comparing stocks 
    output$compareRegressionPlot <- renderPlot({
        # Define stock 1 
        stock1 = input$Stock1
        # Define stock 2
        stock2 = input$Stock2
        # Calculate regression of one stock on the other
        fit <- lm(get(stock2) ~ get(stock1), data = data)
        data$predicted <- predict(fit)
        data$residuals <- residuals(fit)
        s1 <- data %>% 
          select(stock1) %>%
          unlist(use.names = F)
        rsq <- round(summary(fit)$r.squared, 3)
        # User gives choice of regression or residuals
        reg.plot <- function(type = c('Regression', 'Residuals')) {
            if (type == 'Regression') {
                ggplot(data, aes(x = !!ensym(stock1), y = !!ensym(stock2))) +
                    geom_point() +
                    geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
                    theme_bw() +
                    geom_text(x=Inf,y=+Inf, hjust=2, vjust = 2, label = paste0("R2=", rsq), size = 7)
            } else if (type == 'Residuals'){
               plot(s1, resid(fit),main="Residuals",xlab= "Stock1",ylab= "Residuals")
    
            }
        }
        reg.plot(type = input$RegResStocks)
    })
    # Regression equation for 2 stocks 
    output$regressionEQ2Stocks <- renderText({
        # Define stock 1 
        stock1 = input$Stock1
        # Define stock 2
        stock2 = input$Stock2
        # Get estimate information from the linear model
        fit <- lm(get(stock2) ~ get(stock1), data = data)
        data$predicted <- predict(fit)
        data$residuals <- residuals(fit)
        fit <- tidy(fit)
        beta0 <- signif(fit[1,2],3)
        beta1 <- signif(fit[2,2],3)
        sign <- if_else(sign(beta1) == 1, "+", "-")
        paste("<center>", "<h5>", 
              "The regression of","<b>", stock2, "</b>", "on", "<b>", stock1, "</b>", "is modeled by", "</h5>", 
              "<h4>", stock2, "=", "<b>",  beta0, "</b>", sign, "<b>", abs(beta1), "</b>", "&#215", stock1, "</h4>", sep= ' ')
    })
}


# Run the application 
shinyApp(ui = ui, server = server)


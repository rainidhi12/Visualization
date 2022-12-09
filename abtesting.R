library(shiny)
library(tidyverse)
library(dplyr)
library(scales)


control = read.csv("control_group.csv", sep = ";")
test =  read.csv("test_group.csv", sep = ";")

names(control) = c("CampaignName", "Date", "SpendUSD", "Impressions", "Reach", "WebsiteClicks", "Searches", "ViewContent", "AddToCart", "Purchases")
names(test) = c("CampaignName", "Date", "SpendUSD", "Impressions", "Reach", "WebsiteClicks", "Searches", "ViewContent", "AddToCart", "Purchases")

control <- control %>%
  mutate(CRT = Purchases * 100/AddToCart,
         IMRT = WebsiteClicks*100/Impressions
  )
test <- test %>%
  mutate(CRT = Purchases * 100/AddToCart,
         IMRT = WebsiteClicks*100/Impressions
  )




ui = fluidPage(
  titlePanel("A/B Testing"),
  tabsetPanel(
    tabPanel("About Project", fluid = FALSE,
             sidebarLayout(
               sidebarPanel(
                 h2("Abstract"),
                 p("This is a glance at A/B testing by visualization alone. I have 
                   plotted graphs of various parameters and speculated on the final
                   inference. These claims can be confirmed or denied by conducting a hypothesis test.")
               ),
               mainPanel(
                 h1("A/B Test Dataset"),
                 p("In modern data analytics, deciding whether two numerical samples
                   come from the same underlying distribution is called A/B testing.
                   The names refers to the labels of the two samples, A and B.
                   A/B testing, also known as split testing, refers to a randomized experimentation 
                   process wherein two or more versions of a variable (web page,
                   page element, etc.) are shown to different segments of website visitors 
                   at the same time to determine which version leaves the maximum impact
                   and drives business metrics. Essentially, A/B testing eliminates all the guesswork
                   out of website optimization and enables experience optimizers to
                   make data-backed decisions. In A/B testing, A refers to ‘control’ or the original 
                   testing variable. Whereas B refers to ‘variation’ or a new version of
                   the original testing variable.A/B testing is one of the components of the overarching
                   process of Conversion Rate Optimization (CRO), using which we can gather both
                   qualitative and quantitative user insights. We can further use this collected data to
                   understand user behavior, engagement rate, pain points, and
                   even satisfaction with website features, including new features, revamped page sections, etc.")
               )
             )
             
             
    ),
    tabPanel("Univariate", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "datatype",
                              label = "Choose the data type",
                              c("Control" = 'control',
                                "Test" = 'test',
                                "Both" = "both")
                 ),
                 selectInput(inputId = "col",
                             label = "Choose a variable",
                             c("Reach" = "Reach",
                               "Spend in USD" = "SpendUSD",
                               "Impressions" = "Impressions",
                               "Website Clicks" = "WebsiteClicks",
                               "Searches" = "Searches",
                               "View Content" = "ViewContent",
                               "Add to Cart" = "AddToCart",
                               "Purchases" = "Purchases"))
                 
               ),
               mainPanel(
                 
                 # Output: Histogram ----
                 plotOutput(outputId = "single")
                 
               )
               )
    ),
  
  
    tabPanel("Bivariate", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "col1",
                                        label = "choose x axis",
                                        c("Reach" = "Reach",
                                          "Spend in USD" = "SpendUSD",
                                          "Impressions" = "Impressions",
                                          "Website Clicks" = "WebsiteClicks",
                                          "Searches" = "Searches",
                                          "View Content" = "ViewContent",
                                          "Add to Cart" = "AddToCart",
                                          "Purchases" = "Purchases")),
                            selectInput(inputId = "col2",
                                        label = "choose y axis",
                                        c("Reach" = "Reach",
                                          "Spend in USD" = "SpendUSD",
                                          "Impressions" = "Impressions",
                                          "Website Clicks" = "WebsiteClicks",
                                          "Searches" = "Searches",
                                          "View Content" = "ViewContent",
                                          "Add to Cart" = "AddToCart",
                                          "Purchases" = "Purchases"))
                            
                 
               ),
               mainPanel(
                 
                 # Output: Histogram ----
                
                 plotOutput(outputId = "bivary")
                 
               )
               
             )),
             tabPanel("Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "observ",
                              label = "Choose the observation",
                              c("Conversion Rate" = "crt",
                                "Impression Rate" = "imrt"
                                )
                 )),
                 mainPanel(
                   
                   # Output: Histogram ----
                   
                   plotOutput(outputId = "crt_imrt")
                   
                 )
                 
                 )),
             tabPanel("Conclusion", fluid = TRUE,
                      mainPanel(
                        h1("Summary of Analysis"),
                        p("1.The daily spend on test data is higher than the daily spend on control data."),
                         p(" 2.There is variation in spend pattern for test and control data, however 
                          both test and control experience a dip in spend at the middle of the month."),
                          p("3.There is a clear increase in trend in control over the month as website clicks 
                          increase, so did unique searches. For the test data however, there is significantly 
                          less variation across the month."),
                          p("4.Even though it might look like test has steady increase in purchase vs. add to cart,
                          more people are adding to cart in control than in test.\n"),
                        h2("Conclusion"),
                        p("Initially, there is no significant difference in conversion rate for both test data and
                          control data but for last eight days of the month, there is a significant increase in 
                          conversion rate of control data.")
                      )
                      ))
  )


  

server = function(input, output) {
  
  output$single <- renderPlot({
  
  if (input$datatype == 'control'){
    
    plot <- ggplot() + 
      geom_smooth(control, mapping = aes(x =factor(Date, levels = unique(Date)), 
                                         y = control[,input$col], group=1), se=F) + theme_bw() +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      labs(x = "Date", y = input$col, title = paste("Graph of", input$col, "vs. Date for control"))
  }
  
  else if (input$datatype == 'test'){
    plot <- ggplot() + 
      geom_smooth(test, mapping = aes(x =factor(Date, levels = unique(Date)), 
                                      y = test[,input$col], group=1), se=F) + theme_bw() +
      
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      labs(x = "Date", y = input$col, title = paste("Graph of", input$col, "vs. Date for test"))
  }
  
  else {
    plot <-ggplot() + 
      geom_smooth(test, mapping = aes(x =factor(Date, levels = unique(Date)) , y =test[,input$col], group=1, 
                                      colour = "test"), se=F)+theme_bw()+
      geom_smooth(control, mapping = aes(x = factor(Date, levels = unique(Date)), y =control[,input$col], group=1,
                                         color="control"), se=F)+theme_bw()+
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
      labs(x= "Date", y = input$col, title =paste("Graph of", input$col, "vs. Date"))
  }
  
  plot
})

  output$bivary <- renderPlot(ggplot() + 
                                geom_smooth(test, mapping = aes(x =test[,input$col1], y =test[,input$col2], group=1, 
                                                                colour = "test"), se=F) + theme_bw() +
                                geom_smooth(control, mapping = aes(x =control[,input$col1], y =control[,input$col2], group=1,
                                                                   color="control"),se=F) + theme_bw()+
                                labs(x = input$col1, y = input$col2, title=paste("Graph of", input$col1, "vs.", input$col2 ) ))
  
  output$crt_imrt <- renderPlot({
    if (input$observ == 'crt'){
      
      
      plot_1 <- ggplot()+
        geom_smooth(test, mapping = aes(x=factor(Date, levels = unique(Date)), 
                                        y = CRT, group = 1, colour = "test"), se = F ) + theme_bw()+
        geom_smooth(control, mapping = aes(x=factor(Date, levels = unique(Date)), 
                                           y = CRT, group = 1, color="control"), se = F ) +theme_bw()+
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+labs(x="Date", 
                                                                        y="Conversion Rate(%)", title = "Conversion Rate(%) for each day")
    }
    else{
      
      plot_1 <- ggplot()+
        geom_smooth(test, mapping = aes(x=factor(Date, levels = unique(Date)), 
                                        y = IMRT, group = 1, colour = "test"),se=F ) + theme_bw()+
        geom_smooth(control, mapping = aes(x=factor(Date, levels = unique(Date)),
                                           y = IMRT, group = 1, color = "control"), se = F ) +theme_bw()+
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+labs(x="Date",
                                                                        y="Impression Rate(%)", title = "Impression Rate(%) for each day")
    }
    plot_1
  }
    
  )
}


shinyApp(ui = ui, server = server)
  


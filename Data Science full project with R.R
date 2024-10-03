library("shiny")
library("dplyr")
library("reader")
library("arules")

ui = fluidPage(
  titlePanel("Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Choose CSV File",accept = ".csv"),
      br(),
      actionButton("Cleaning_Data", "Cleaning Data"),
      br(),
      actionButton("Data_Visualization","Data Visualization"),
      br(),
      actionButton("k_means","K Means"),
      sliderInput("n_clusters","Number of Clusters : ",min = 2,max = 4,value = 3),
      br(),
      actionButton("apriori","Apriori"),
      numericInput("support", "Enter Support (between 1 and 0.001):", value = 0.01, min = 0.001, max = 1, step = 0.001),
      numericInput("confidence", "Enter Confidence (between 1 and 0.001):", value = 0.01, min = 0.001, max = 1, step = 0.001)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaning Data",tabsetPanel(tabPanel("check outliers", plotOutput("check_outliers")),
                                             tabPanel("check NA value", verbatimTextOutput("check_NA_value")),
                                             tabPanel("check duplicated", verbatimTextOutput("check_duplicated")))),
        
        tabPanel("Data Visualization",tabsetPanel(tabPanel("Cash vs Credit",plotOutput("Cash_vs_Credit")),
                                                  tabPanel("Age vs Total Spending",plotOutput("Age_vs_Total_Spending")),
                                                  tabPanel("City Total Spending",plotOutput("City_Total_Spending")),
                                                  tabPanel("Total Spending Distribution",plotOutput("Total_Spending_Distribution")),
                                                  tabPanel("all previous plots in one dashboard",plotOutput("one_dashboard")))),
        tabPanel("K Means",tabsetPanel(tabPanel("table of K-Means",verbatimTextOutput("table")),
                                       tabPanel("Plot of K-Means",plotOutput("plot_kmeans")))),
        tabPanel("Apriori",verbatimTextOutput("apriori"))
      )
    )
  )
)

server=function(input,output){
  data = reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  observeEvent(input$Cleaning_Data,{
    dff=data()[!duplicated(data()), ]
    outliers=boxplot.stats(dff$count)$out
    dff=dff[!dff$count %in% outliers, ]
    output$check_outliers <- renderPlot({
      par(mfrow=c(2, 2))
      boxplot(dff$count,main = "count",col = "cyan4")
      boxplot(dff$total,main = "total",col = "cyan4")
      boxplot(dff$rnd,main = "rnd",col = "cyan4")
      boxplot(dff$age,main = "age",col = "cyan4")
    })
    output$check_NA_value <- renderPrint({
      paste("the sum of NA values =",sum(is.na(dff)))
    })
    output$check_duplicated <- renderPrint({
      paste("the sum of duplicated =",sum(duplicated(dff)))
    })
  })
  observeEvent(input$Data_Visualization,{
    dff=data()[!duplicated(data()), ]
    outliers=boxplot.stats(dff$count)$out
    dff=dff[!dff$count %in% outliers, ]
    output$Cash_vs_Credit <- renderPlot({
      payment_table=table(dff$paymentType)
      percentage = paste0(round(100*payment_table/sum(payment_table),2),"%")
      pie(payment_table
          ,labels = percentage
          ,main = "cash and credit totals."
          ,col = c("grey","blue"))
      legend("bottomleft", legend = c("cash", "credit"),fill =c( "grey","blue"))
    })
    output$Age_vs_Total_Spending <- renderPlot({
      age_total = aggregate(total ~ age,data=dff,FUN=sum)
      plot(age_total$age,
           age_total$total
           ,xlab = "age"
           ,ylab = "total spending"
           ,main = "age and sum of total spending."
           ,col = "red")
    })
    output$City_Total_Spending <- renderPlot({
      city_total = aggregate(total ~ city,data = dff,FUN = sum)
      city_total_sort=sort(city_total$total,decreasing = TRUE)
      barplot(city_total_sort
              ,names = city_total$city
              ,xlab = "city"
              ,ylab = "total spending"
              ,main = "city total spending"
              ,col = "blue")
    })
    output$Total_Spending_Distribution <- renderPlot({
      boxplot(dff$total
              ,main = "distribution of total spending. "
              ,col = "cyan4"
              ,border = "blue"
              ,xlab = "total spending")
    })
    output$one_dashboard <- renderPlot({
      par(mfrow=c(2, 2))
      payment_table=table(dff$paymentType)
      percentage = paste0(round(100*payment_table/sum(payment_table),2),"%")
      pie(payment_table
          ,labels = percentage
          ,main = "cash and credit totals."
          ,col = c("grey","blue"))
      legend("bottomleft", legend = c("cash", "credit"),fill =c( "grey","blue"))
      
      age_total = aggregate(total ~ age,data=dff,FUN=sum)
      plot(age_total$age,
           age_total$total
           ,xlab = "age"
           ,ylab = "total spending"
           ,main = "age and sum of total spending."
           ,col = "red")
      city_total = aggregate(total ~ city,data = dff,FUN = sum)
      city_total_sort=sort(city_total$total,decreasing = TRUE)
      barplot(city_total_sort
              ,names = city_total$city
              ,xlab = "city"
              ,ylab = "total spending"
              ,main = "city total spending"
              ,col = "blue")
      boxplot(dff$total
              ,main = "distribution of total spending. "
              ,col = "cyan4"
              ,border = "blue"
              ,xlab = "total spending")
    })
  })
  observeEvent(input$k_means,{
    dff=data()[!duplicated(data()), ]
    outliers=boxplot.stats(dff$count)$out
    dff=dff[!dff$count %in% outliers, ]
    output$table <- renderPrint({
      my_columns = aggregate(total ~ customer + age ,data=dff,FUN=sum)
      k_means = kmeans(my_columns[,c("age","total")],centers = input$n_clusters)
      final = cbind(my_columns,cluster = k_means$cluster)
      print(final)
    })
    output$plot_kmeans <- renderPlot({
      my_columns = aggregate(total ~ customer + age ,data=dff,FUN=sum)
      k_means = kmeans(my_columns[,c("age","total")],centers = input$n_clusters)
      final = cbind(my_columns,cluster = k_means$cluster)
      plot(final$age, final$total, col = final$cluster, pch = 19,
           xlab = "Age", ylab = "Total Spending", main = "K-means Clustering")
    })
  })
  observeEvent(input$apriori,{
    dff=data()[!duplicated(data()), ]
    outliers=boxplot.stats(dff$count)$out
    dff=dff[!dff$count %in% outliers, ]
    if(input$support > 1 | input$support < 0.001) {
      showNotification("The support should be between 1 and 0.001", type = "warning")
      return()
    }
    if(input$confidence > 1 | input$confidence < 0.001) {
      showNotification("The confidence should be between 1 and 0.001", type = "warning")
      return()
    }
    output$apriori <- renderPrint({
      transactions = strsplit(dff$items,",")
      transactions = as(transactions, "transactions")
      apriori_rules = apriori(transactions, parameter = list(supp = input$support, conf = input$confidence , minlen = 2))
      inspect(apriori_rules)
    })
  })
  
}
shinyApp(ui = ui , server = server)


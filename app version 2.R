#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
load(url("https://raw.githubusercontent.com/ktj123/Shinny-App-Project-Data-Breach/master/PRC_Data_Breach_Chronology.RData"))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Data Breach Cases in US"),

    # Generate a row with a sidebar
    sidebarLayout(
        
        # Define the sidebar
      sidebarPanel( width = 3,
        helpText("Choose the State or United States"),
        selectInput(inputId = "Sta", 
                    label = "State:",
                    choices = c("United States", "NJ","CA","TX","NY","IL","MI","CT",
                                "MD","DE","GA","OR","FL","OH","NE",
                                "MN","MA","CO","KY","DC",
                                "NV","NC","MO","IN","AK","PA",
                                "VA","NM","HI","TN","VT","UT","AZ",
                                "KS","AL","WA","IA","SC","WI","LA",
                                "WY","NH","OK","MS","AR","RI","ME",
                                "MT","ID","WV","SD","ND","PR","UNKN"), 
                    selected = "United States"),
        
        hr(), #Horizontal Line for visual separation
        
        helpText("Choices in Bar Charts"),
        
        selectInput(inputId = "x", 
                    label = "X-axis in Bar Chart:",
                    choices = c("Year","Type_of_breach","Type_of_organization"), 
                    selected = "Year"),
        # Select Colors
        selectInput(inputId = "color_b", 
                    label = "Choose Bar Color",
                    choices = c("red", "green", "grey", "yellow", "darkblue","pink","purple"), 
                    selected = "darkblue"), 
        
        helpText("Choices in Dot and Line Charts"),
        
        # Set min/max of Cohort Values
        selectInput(inputId = "min", 
                    label = "Choose Trend Range (Min):", 
                    choices = c(2005,2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017),
                    selected= 2005),
        
        selectInput(inputId = "max",
                    label = "Choose Trend Range (Max):", 
                    choices = c(2006,2007,2008,2009,2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                    selected= 2018),
        
        # Set font size
        sliderInput(inputId = "font", 
                    label = "Choose Font Size:", 
                    min = 10, max = 35, 
                    value = 15),
       
        # Set point color
        selectInput(inputId = "color_p", 
                    label = "Choose point Color (not for Frequency Chart)",
                    choices = c("red", "green", "grey", "yellow", "darkblue","pink","purple"), 
                    selected = "darkblue"),
        
        # Set point size
        sliderInput(inputId = "size_p", 
                    label = "Choose point Size: (not for Frequency Chart)", 
                    min = 0, max = 10, 
                    value = 4),
        
        # Set alpha level
        sliderInput(inputId = "alpha", 
                    label = "Point Transparency: (not for Frequency Chart)", 
                    min = 0, max = 1, 
                    value = 1),
        
        helpText("Data from Privacy Rights Clearinghouse (https://privacyrights.org/).")
       ),

        #Output: Type of plot
        mainPanel(
            tabsetPanel(
            tabPanel("Data Breach Cases 2005-2018", plotOutput(outputId = "Bars1"),
                     
                     hr(),
                     strong("Type of Organization"),    
                     p("BSF: Businesses (Financial and Insurance Services)"),
                     p("BSO: Businesses (Other)"),
                     p("BSR: Businesses (Retail/Merchant including Online Retail)"),
                     p("EDU: Educational Institutions"),
                     p("GOV: Government & Military"),
                     p("MED: Healthcare, Medical Providers and Medical Insurance Services"),
                     p("NGO: Nonprofits"),
                     p("UNKN: Unknown"),
                     strong("Type of Breach"),
                     p("CARD: Fraud Involving Debit and Credit Cards Not Via Hacking"),
                     p("HACK: Hacked by an Outside Party or Infected by Malware"),
                     p("INSD: Insider (employee, contractor or customer)"),
                     p("PHYS: hysical (paper documents that are lost, discarded or stolen)"),
                     p("PORT: Portable Device (lost, discarded or stolen) laptop, PDA, smartphone, memory stick, CDs, hard drive, data tape, etc."),
                     p("STAT: Stationary Computer Loss such as lost, inappropriately accessed, discarded or stolen computer or server not designed for mobility"),
                     p("DISC: Unintended Disclosure Not Involving Hacking, Intentional Breach or Physical Loss"),
                     p("UNKN: not enough information about breach to know how exactly the information was exposed")
                     ),

            tabPanel("Data Breach Trend by Organization", 
                     #verbatimTextOutput("summary"),

                    fluidRow(sidebarPanel(
                         selectInput(inputId = "Org", 
                                     label = "Choose the organization",
                                     choices = c("BSF", "BSO", "BSR", "EDU", "GOV","MED","NGO","UNKN"), 
                                     selected = "EDU"),  
                    )),
                    hr(),
                    
                    fluidRow(plotOutput(outputId ="Lines1")),
                    hr(),
                    
                    strong("Type of Organization"),    
                    p("BSF: Businesses (Financial and Insurance Services)"),
                    p("BSO: Businesses (Other)"),
                    p("BSR: Businesses (Retail/Merchant including Online Retail)"),
                    p("EDU: Educational Institutions"),
                    p("GOV: Government & Military"),
                    p("MED: Healthcare, Medical Providers and Medical Insurance Services"),
                    p("NGO: Nonprofits"),
                    p("UNKN: Unknown")
                    ),                    
            
            tabPanel("Data Breach Trend by Type", 

                     fluidRow(sidebarPanel(
                       selectInput(inputId = "Type", 
                                   label = "Choose the Breach Type",
                                   choices = c("CARD", "HACK", "INSD", "PHYS", "PORT","STAT","DISC","UNKN"), 
                                   selected = "HACK"),                       
                     )),
                     hr(),
                     
                     fluidRow(plotOutput(outputId ="Lines2")),
                     hr(), 
                     
                     strong("Type of Breach"),
                     p("CARD: Fraud Involving Debit and Credit Cards Not Via Hacking"),
                     p("HACK: Hacked by an Outside Party or Infected by Malware"),
                     p("INSD: Insider (employee, contractor or customer)"),
                     p("PHYS: hysical (paper documents that are lost, discarded or stolen)"),
                     p("PORT: Portable Device (lost, discarded or stolen) laptop, PDA, smartphone, memory stick, CDs, hard drive, data tape, etc."),
                     p("STAT: Stationary Computer Loss such as lost, inappropriately accessed, discarded or stolen computer or server not designed for mobility"),
                     p("DISC: Unintended Disclosure Not Involving Hacking, Intentional Breach or Physical Loss"),
                     p("UNKN: not enough information about breach to know how exactly the information was exposed")
                        ),
            
            tabPanel("Data Breach Frequency", 
                     plotOutput(outputId ="Lines3.1"),
                     plotOutput(outputId ="Lines3.2")),
            
            tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
            )
        )
)
)
            

# Define a server for the Shiny app
server<-function(input, output) {
    
    #subset data by State
    dat1 <- reactive({
    if(input$Sta=="United States"){
      ds1<-PRC_Data_Breach_Chronology
    }
      else{
      ds1 <- PRC_Data_Breach_Chronology[PRC_Data_Breach_Chronology$State %in% input$Sta,]
      }
    
        return(ds1)
    })
    
    # Bar chart by states

    output$Bars1 <- renderPlot({
        d1<-subset(dat1())
        xx<-barplot(table(d1[,input$x]),
                main=paste("Data Breach Cases 2005-2018 in", input$Sta),
                ylab="Number of Cases",
                xlab=input$x,
                col=input$color_b)
    })
    
    
    # Scatter Plots by Organization

    output$Lines1 <- renderPlot({
      d2<-subset(dat1())
      # add a column with value 1 for count
      d2$count1<-rep(1,nrow(d2))

      #subset again
      d3<-subset(d2,d2$Type_of_organization==input$Org)
      
      #summary
      #output$summary <- renderPrint({
       # dataset <- d3
       # summary(dataset)})     
      
      ggplot(d3,aes(x=Year,y=count1))+
        stat_summary(fun= sum, geom="point",colour=input$color_p, size=input$size_p, alpha=input$alpha)+
        geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust =-0.5, size = 5)+
        xlim(as.numeric(input$min), as.numeric(input$max))+
        labs(title = paste("Data Breach Trend in", input$Sta, "(",input$min,"-",input$max,")"), y="Number of Cases")+
        theme_bw()+theme(text = element_text(size = input$font))
    }) 
    
    # Scatter Plots by Type
    
    output$Lines2 <- renderPlot({
      d2<-subset(dat1())
      # add a column with value 1 for count
      d2$count1<-rep(1,nrow(d2))
      
      #subset again
      d4<-subset(d2,d2$Type_of_breach==input$Type)
      
      ggplot(d4,aes(x=Year,y=count1))+
        stat_summary(fun= sum, geom="point", colour=input$color_p, size=input$size_p, alpha=input$alpha)+
        geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust =-0.5, size = 5)+
        xlim(as.numeric(input$min), as.numeric(input$max))+
        labs(title = paste("Data Breach Trend in", input$Sta, "(",input$min,"-",input$max,")"), y="Number of Cases")+
        theme_bw()+theme(text = element_text(size = input$font))
    }) 

    # Comparison
    
    output$Lines3.1 <- renderPlot({
      d1<-subset(dat1())
      ggplot(d1,aes(Year,color=Type_of_breach))+
        geom_freqpoly(linetype ="solid", size=1,binwidth = 1)+
        xlim(as.numeric(input$min), as.numeric(input$max))+
        labs(title = paste("Data Breach Cases in", input$Sta,"by Type", "(",input$min,"-",input$max,")"), y="Number of Cases")+
        theme_bw()+theme(text = element_text(size = input$font))
    })    
    output$Lines3.2 <- renderPlot({
      d1<-subset(dat1())    
      ggplot(d1,aes(Year,color=Type_of_organization))+
        geom_freqpoly(linetype ="solid",size=1, binwidth = 1)+
        xlim(as.numeric(input$min), as.numeric(input$max))+
        labs(title = paste("Data Breach Cases in", input$Sta,"by Organization", "(",input$min,"-",input$max,")"), y="Number of Cases")+
        theme_bw()+theme(text = element_text(size = input$font))
    })
    
    # Dataset
    output$datasheet<-DT::renderDataTable({
               DT::datatable(data=PRC_Data_Breach_Chronology[,1:7],
                options=list(pageLength= 20),
                          rownames=FALSE)
    })
    
    }
# Run the application 
shinyApp(ui = ui, server = server)

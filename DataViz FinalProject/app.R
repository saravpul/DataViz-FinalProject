library(shinydashboard)
library(shiny)
library(ggplot2)
library(MASS)
library(dplyr)
library(magrittr)
library(ggthemes)
library(here)
library(tools)
library(reshape2)
library(scales)

here::here()
# alldata <- read.csv(here::here("data", "data.csv"),stringsAsFactors = FALSE) #has data with NA
# alldata1 <- read.csv(here::here("data", "datarank.csv"),stringsAsFactors = FALSE)

alldata <- read.csv(here::here("data", "alldata.csv"),stringsAsFactors = FALSE) #has data with NA
alldata1 <- read.csv(here::here("data", "noNAdata.csv"),stringsAsFactors = FALSE)

alldata1$STABBR <- as.factor(alldata1$STABBR)
alldata1$CONTROL <- as.factor(alldata1$CONTROL)
alldata1$HIGHDEG <- as.factor(alldata1$HIGHDEG)
alldata1$INSTNM <- as.factor(alldata1$INSTNM)
alldata1$CCBASIC <- as.factor(alldata1$CCBASIC)

alldata1$ADM_RATE <- as.numeric(alldata1$ADM_RATE)
alldata1$SAT_AVG <- as.numeric(alldata1$SAT_AVG)
alldata1$AVGFACSAL <- as.numeric(alldata1$AVGFACSAL)
alldata1$TUITIONFEE_IN <- as.numeric(alldata1$TUITIONFEE_IN)
alldata1$GRAD_DEBT_MDN_SUPP <- as.numeric(alldata1$GRAD_DEBT_MDN_SUPP)
alldata1$MD_EARN_WNE_P10 <- as.numeric(alldata1$MD_EARN_WNE_P10)

alldata1$PCTPELL <- as.numeric(alldata1$PCTPELL)
alldata1$FEMALE_DEBT_MDN <- as.numeric(alldata1$FEMALE_DEBT_MDN)
alldata1$MALE_DEBT_MDN <- as.numeric(alldata1$MALE_DEBT_MDN)
alldata1$UGDS_WHITE <- as.numeric(alldata1$UGDS_WHITE	)
alldata1$UGDS_BLACK <- as.numeric(alldata1$UGDS_BLACK)	
alldata1$UGDS_HISP <- as.numeric(alldata1$UGDS_HISP	)
alldata1$UGDS_ASIAN <- as.numeric(alldata1$UGDS_ASIAN)

alldata1 <- alldata1[complete.cases(alldata1),]

ccbasic <- c("Baccalaureate/Associate's Colleges: Associate's Dominant", 
             "Doctoral Universities: Highest Research Activity", 
             "Doctoral Universities: Highest Research Activity",
             "Doctoral Universities: Moderate Research Activity", 
             "Master's Colleges & Universities: Larger Programs", 
             "Master's Colleges & Universities: Medium Programs",
             "Master's Colleges & Universities: Small Programs", 
             "Baccalaureate Colleges: Arts & Sciences Focus", 
             "Baccalaureate Colleges: Diverse Fields",
             "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's", 
             "Special Focus Four-Year: Faith-Related Institutions", 
             "Special Focus Four-Year: Medical Schools & Centers",
             "Special Focus Four-Year: Other Health Professions Schools", 
             "Special Focus Four-Year: Engineering Schools", 
             "Special Focus Four-Year: Other Technology-Related Schools",
             "Special Focus Four-Year: Business & Management Schools", 
             "Special Focus Four-Year: Arts, Music & Design Schools", 
             "Special Focus Four-Year: Law Schools",
             "Special Focus Four-Year: Other Special Focus Institutions")

stateList <-  list(`New England`= c("Connecticut"="CT", "Maine"="ME", "Massachusetts"="MA", "New Hampshire"="NH",  "Rhode Island"="RI", "Vermont"="VT"),
                   `Mid East`= c( "Delaware"="DE", "Washington DC"="DC", "Maryland"="MD", "New Jersey"="NJ", "New York"="NY", "Pennsylvania"="PA"),
                   `Great Lakes`= c( "Illinois"="IL", "Indiana"="IN", "Michigan"="MI",  "Ohio"="OH", "Wisconsin"="WI"),
                   `Plains`= c("Iowa"="IA", "Kansas"="KS", "Minnesota"="MN", "Missouri"="MO", "Nebraska"="NE", "North Dakota"="ND","South Dakota"= "SD"),
                   `Southeast` = c("Alabama"="AL","Arkansas"="AR", "Florida"="FL", "Georgia"="GA", "Kentucky"="KY","Louisiana"="LA", "Mississippi"="MS",  "North Carolina"="NC", "South Carolina"="SC",  "Tennessee"="TN", "Virginia"="VA", "West Virginia"="WV"),
                   `Southwest`= c("Arizona"="AZ","New Mexico"="NM", "Oklahoma"="OK", "Texas"="TX"),
                   `Rocky Mountains`= c("Colorado"="CO",  "Idaho"="ID", "Montana"="MT", "Utah"="UT", "Wyoming"="WY"),
                   `Far West`= c("Alaska"="AK", "California"="CA", "Hawaii"="HI", "Nevada"="NV", "Oregon"="OR", "Washington"="WA"),
                   `Outlying Areas`= c("American Samoa"="AS", "Micronesia"="FM", "Guam"="GU", "Marshall Islands"="MH","Northern Mariana Islands"= "MP", "Puerto Rico"="PR", "Palau"="PW", "U.S. Virgin Islands" ="VI"))


varList <- c('Average SAT Score',
             'Average Faculty Salary',
             'Admission Rate',
             'Instate Tuition Fee',
             'Percentage receiving a Pell Grant',
             'Median Debt after Graduation',
             'Median Earnings 10 years post enrollment')



#----------------------start of UI----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "CollegeScorecard"),
  dashboardSidebar(disable = TRUE
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        side = "right",
        title = "Analysis of Colleges across the USA",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "800px", width = "100px",
        
        #tabpanel for TAB3 INSIGHTS within tabBox--------------------------------------------         
        tabPanel("Insights from across the nation",
                 tabBox(title = "Insights from across the nation",
                        id = "tabsubset", height = "700px", width = "100px",
                        tabPanel("DIY Plot",
                                 h4(strong("DIY Plot", style = "color:#3c8dbc")),
                                 sidebarPanel(
                                   
                                   #To make plot3 fill the whole main panel 
                                   tags$head(tags$style("#plot3{height:70vh !important;}")), 
                                   
                                   #To make plot4 fill the whole main panel 
                                   tags$head(tags$style("#plot4{height:80vh !important;}")), 
                                   h4(strong("Plot the relationship between any 2 variables", style = "color:#3c8dbc")),
                                   
                                   #X variable
                                   selectInput('xVar',
                                               'Select X variable',
                                               choices = varList,
                                               selected = 'Admission Rate'
                                   ),
                                   #Y variable
                                   selectInput('yVar',
                                               'Select Y variable',
                                               choices = varList,
                                               selected = "Median Earnings 10 years post enrollment"
                                   ),
                                   
                                   radioButtons('method',
                                                'Choose the Model:',
                                                choices = c('Linear Model' = 'lm',
                                                            'LOESS' = 'loess')     
                                   )
                                 ),#end of side bar panel
                                 
                                 mainPanel(
                                   plotOutput('plot3') #In same tab as the user input for TOP12
                                 )
                        ),#end of inside-tabpanel for DIY PLOT 
                        
                        tabPanel("Did you know?",
                                 sidebarPanel(
                                   
                                   h4(strong(" ", style = "color:#3c8dbc")),
                                   
                                   #X variable
                                   radioButtons('button1',
                                                'Select one',
                                                choices = c('Median Earnings of colleges (differentiated by the highest degree awarded)',
                                                            'Number of public colleges in every state that offer Bachelors degrees',
                                                            'Differences in debt accrued in public, private and  non-profit'),
                                                
                                                selected = 'Number of public colleges in every state that offer Bachelors degrees'
                                   )
                                   
                                 ),#end of side bar panel
                                 
                                 mainPanel(
                                   plotOutput('plot4') #In same tab as the user input for TOP12
                                 )
                        )#end of inside-tabpanel for extra insights
                        
                 )#end of inner tabbox
        ), #end of tabPanel for TAB3 INSIGHTS
        
        
        
        
        #tabpanel for COMPARE COLLEGES within tabBox--------------------------------------------
        tabPanel("Compare Colleges",
                 h4(strong("Compare Colleges", style = "color:#3c8dbc")),
                 sidebarPanel(
                   #To make plot2 fill the whole main panel 
                   tags$head(tags$style("#plot2{height:60vh !important;}")), 
                   h4(strong("College 1", style = "color:#3c8dbc")),
                   #State1
                   selectInput('st1',
                               'Select a state',
                               choices = stateList,
                               selected = "TX"
                   ),
                   
                   #Select college1 from state 1
                   uiOutput('college1'),
                   
                   h4(strong("College 2", style = "color:#3c8dbc")),
                   #State2
                   selectInput('st2',
                               'Select a state',
                               choices = stateList,
                               selected = "TX"
                   ),
                   
                   #Select college2 from state 2
                   uiOutput('college2'),
                   
                   #choose attributes for comparison
                   h4(strong("Features", style = "color:#3c8dbc")),
                   checkboxGroupInput('chosencol',
                                      'Select a feature to compare',
                                      choices = c(
                                        'Average SAT Score',
                                        'Admission Rate',
                                        'Tuition Fee',
                                        'Debt after Graduation')
                   ),
                   
                   radioButtons('button',
                                'Ethnicity of Student Population',
                                choices = c('View',
                                            'Hide'),
                                selected = 'Hide'
                   )
                 ),
                 mainPanel(
                   tableOutput('table'),
                   plotOutput('plot2')
                 )
        ), #end of tabPanel for Compare Colleges
        
        
        #tabpanel for TOP 12 ANALYSIS within tabBox--------------------------------------------
        
        tabPanel("State-wise Top 12 Analysis", 
                 h4(strong("State-wise Top 12 Analysis", style = "color:#3c8dbc")),
                 sidebarPanel(
                   #To make plot1 fill the whole main panel 
                   tags$head(tags$style("#plot1{height:80vh !important;}")), 
                   #State1
                   h4(strong("State", style = "color:#3c8dbc")),
                   
                   selectInput('state',
                               'Select a state',
                               choices = stateList,
                               selected = "TX"
                   ),
                   
                   #Top 12 based on
                   h4(strong("Top 12 based on:", style = "color:#3c8dbc")),
                   
                   selectInput('sortby',
                               'Select a variable',
                               choices = c(
                                 'Average SAT Score' = 'SAT_AVG',
                                 'Average Faculty Salary' = 'AVGFACSAL',
                                 'Admission Rate'= 'ADM_RATE',
                                 'In-state Tuition Fee'= 'TUITIONFEE_IN',
                                 'Percentage of undergraduates who receive a Pell Grant' = 'PCTPELL',
                                 'Debt after Graduation' = 'GRAD_DEBT_MDN_SUPP',
                                 'Earnings 10 years post enrollment'= 'MD_EARN_WNE_P10'
                               ),
                               selected = 'AVGFACSAL'
                   ),
                   selectInput('hilo',
                               'High or Low?',
                               choices = c(
                                 'High',
                                 'Low'
                               ),
                               selected = 'High'
                   ),
                   
                   #choose feature for comparison
                   h4(strong("Compare a feature across Top 12 colleges \n in selected state", style = "color:#3c8dbc")),
                   radioButtons('compare1',
                                'Select a feature:',
                                choices = c(
                                  'Average SAT Score',
                                  'Average Faculty Salary',
                                  'Admission Rate',
                                  'In-state Tuition Fee',
                                  'Percentage of undergraduates who receive a Pell Grant',
                                  'Debt after Graduation',
                                  'Earnings 10 years post enrollment',
                                  "Debt & Earnings",
                                  "Debt (Males vs. Females)",
                                  "Student population by Ethinicity (%)"
                                ),
                                selected = "Student population by Ethinicity (%)"
                   )
                 ),#end of side bar panel
                 mainPanel(
                   plotOutput('plot1') #In same tab as the user input for TOP12
                 )
                 
        ),#end of tabPanel for TOP 12
        
        #tabpanel for TOP 12 NATIONAL ANALYSIS within tabBox--------------------------------------------
        
        tabPanel("Nation-wide Top 12 Analysis", 
                 h4(strong("Nation-wide Top 12 Analysis", style = "color:#3c8dbc")),
                 sidebarPanel(
                   #To make plot1 fill the whole main panel 
                   tags$head(tags$style("#plot0{height:80vh !important;}")), 
                   
                   #Top 12 based on
                   h4(strong("Top 12 based on:", style = "color:#3c8dbc")),
                   
                   selectInput('sortby0',
                               'Select a variable',
                               choices = c(
                                 'Average SAT Score' = 'SAT_AVG',
                                 'Average Faculty Salary' = 'AVGFACSAL',
                                 'Admission Rate'= 'ADM_RATE',
                                 'In-state Tuition Fee'= 'TUITIONFEE_IN',
                                 'Percentage of undergraduates who receive a Pell Grant' = 'PCTPELL',
                                 'Debt after Graduation' = 'GRAD_DEBT_MDN_SUPP',
                                 'Earnings 10 years post enrollment'= 'MD_EARN_WNE_P10'
                               ),
                               selected = 'AVGFACSAL'
                   ),
                   selectInput('hilo0',
                               'High or Low?',
                               choices = c(
                                 'High',
                                 'Low'
                               ),
                               selected = 'High'
                   ),
                   
                   #choose feature for comparison
                   h4(strong("Compare a feature across Top 12 colleges", style = "color:#3c8dbc")),
                   radioButtons('compare0',
                                'Select a feature:',
                                choices = c(
                                  'Average SAT Score',
                                  'Average Faculty Salary',
                                  'Admission Rate',
                                  'In-state Tuition Fee',
                                  'Percentage of undergraduates who receive a Pell Grant',
                                  'Debt after Graduation',
                                  'Earnings 10 years post enrollment',
                                  "Debt & Earnings",
                                  "Debt (Males vs. Females)",
                                  "Student population by Ethinicity (%)"
                                ),
                                selected = 'Average SAT Score'
                   )
                 ),#end of side bar panel
                 
                 mainPanel(
                   plotOutput('plot0') #In same tab as the user input for TOP12
                 )
                   )#end of side bar panel
                
        )#end of tabPanel for TOP 12 NATIONAL     
        )#End of tabBox
      )#End of fluidRow
  
    )#End of dashboardBody
  # )# End of dashboardPage

#----------------------END of UI----------------------------------------------------------------------

server <- function(input, output) { 
  
  #----------------------------------------------extras------------------------
  output$plot4 <- renderPlot({
    
    d4 <- alldata1 %>%
      group_by(HIGHDEG) %>%
      summarise(count = length(HIGHDEG),
                earnMedian = median( MD_EARN_WNE_P10))
    
    
    a <- ggplot(d4)
    
    d5 <- alldata %>%
      dplyr::select(STABBR, CONTROL) %>% 
      filter(CONTROL == "Public") %>% 
      group_by(STABBR) %>%
      summarise(count = length(CONTROL)) %>% 
      arrange(desc(count))
    
    b <- ggplot(d5)
    
    d6<- alldata %>%
      dplyr::select(STABBR, CONTROL, GRAD_DEBT_MDN_SUPP) %>% 
      group_by(CONTROL) 
    
    d6 <- d6[complete.cases(d6),]%>%
      summarise(median = median(GRAD_DEBT_MDN_SUPP))%>% 
      arrange(desc(median))
    
    c <- ggplot(d6)
    
    if (input$button1 == 'Median Earnings of colleges (differentiated by the highest degree awarded)') {
      a <- a +
        ggtitle(input$button1) +
        geom_bar(aes(x= HIGHDEG, y= earnMedian, fill = HIGHDEG),
                 stat = "identity")+
        
        theme_fivethirtyeight()+
        
        scale_y_continuous(labels = scales::comma)+
        
        labs( x = "",
              y = "Earnings 10 years post enrollment") +
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 15),
              axis.line = element_line(color = "black"))+
        
        #setting legend properties
        theme( legend.title = element_text(color = "transparent", size=16),
               legend.text = element_text(size=16))
      
      a
    }
    else if (input$button1 == 'Number of public colleges in every state that offer Bachelors degrees'){
      
      b +
        ggtitle(input$button1) +
        geom_bar(aes(x= reorder(STABBR, -count), y= count, fill = STABBR), show.legend= FALSE,
                 stat = "identity")+
        
        theme_fivethirtyeight()+
        
        labs( x = "",
              y = "Number of public colleges in state") +
        
        geom_text(aes(x= reorder(STABBR, -count), y= count, label= count),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 0.6,
                  vjust = -0.2,
                  size = 4,
                  face = 'bold',
                  color = "black")+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 9),
              axis.line = element_line(color = "black"))
      
    }
    else if (input$button1 == 'Differences in debt accrued in public, private and  non-profit'){
      
      c +
        ggtitle(input$button1) +
        geom_bar(aes(x= CONTROL, y= median, fill = CONTROL), show.legend= FALSE,
                 stat = "identity")+
        
        theme_fivethirtyeight()+
        
        labs( x = "",
              y = "Debt accrued ($)") +
        
        geom_text(aes(x= CONTROL, y= median, label= scales::dollar(as.numeric(median))),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 0.6,
                  vjust = -0.2,
                  size = 10,
                  face = 'bold',
                  color = "black")+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 19),
              axis.line = element_line(color = "black"))
      
      
    }
  })
  
  #----------------------------------------------DIY Plots------------------------
  d3 <- reactive({
    
    validate(
      need(input$xVar != input$yVar, "X and Y variables have to be different")
    )
    
    x <- alldata1 %>% 
      dplyr::select(SAT_AVG,AVGFACSAL,ADM_RATE,TUITIONFEE_IN,PCTPELL,GRAD_DEBT_MDN_SUPP,MD_EARN_WNE_P10) %>% 
      
      plyr::rename(c("SAT_AVG"="AverageSATScore","AVGFACSAL"="AverageFacultySalary",
                     "ADM_RATE"="AdmissionRate","TUITIONFEE_IN"="InstateTuitionFee",
                     "PCTPELL"= "PercentagereceivingaPellGrant",
                     "GRAD_DEBT_MDN_SUPP"="MedianDebtafterGraduation",
                     "MD_EARN_WNE_P10"="MedianEarnings10yearspostenrollment"))
    
    x
  })
  
  
  output$plot3 <- renderPlot({
    
    #removing SPACES  from input to aes_string 
    xV <- gsub(" ", "", input$xVar)
    yV <- gsub(" ", "", input$yVar)
    
    ggplot(d3(), aes_string(x= xV, y= yV)) +
      
      geom_point(alpha=0.5) +
      
      geom_smooth(method=input$method, se=FALSE)  +
      
      ggtitle(paste(input$xVar,"vs.",input$yVar)) +
      
      labs(x = input$xVar, y = input$yVar) +
      
      theme_fivethirtyeight()+
      
      
      theme(axis.title = element_text(color = "black", size = 16),
            axis.text.y = element_text(face='bold',size = 15),
            axis.text.x = element_text(face='bold',size = 15),
            axis.line = element_line(color = "black"))+
      
      
      #setting legend properties
      theme( legend.title = element_text(size=16),
             legend.text = element_text(size=16))
    
    
    
    
  })
  
  #----------------------------------------------compare colleges------------------------
  #dataframe containing all colleges under State 1
  d1 <- reactive({
    
    x <- alldata1 %>%
      filter(.,alldata1$STABBR == input$st1)%>%
      filter(., PREDDEG == 3) %>%
      filter(., CCBASIC %in% ccbasic) %>%
      # filter(., CCBASIC %in% seq(14,23)) %>%
      arrange(.,(INSTNM)) 
    
  })
  
  
  #dataframe containing all colleges under State 2
  d2 <- reactive({
    y <- alldata1 %>%
      filter(.,alldata1$STABBR == input$st2)%>%
      filter(., PREDDEG == 3) %>%
      filter(., CCBASIC %in% ccbasic) %>%
      # arrange(.,desc(AVGFACSAL))  %>%
      arrange(.,(INSTNM)) 
    y
  })
  
  
  #Display Colleges that fall under State 1
  output$college1 <- renderUI({
    selectInput('col1',
                'Select a college',
                choices = d1()$INSTNM
    )
  })
  
  
  
  #Display Colleges that fall under State 2
  output$college2 <- renderUI({
    
    selectInput('col2',
                'Select a college',
                choices = d2()$INSTNM
    )
  })
  
  #Output TABLE based on colleges selected and columns chosen through checkboxes
  output$table <- renderTable({
    
    validate(
      need(input$col1 != "", "This state does not have colleges providing satisfying the following criteria\n - predominantly bachelor's degrees\n -4 year or Master's Programs")
    )
    
    validate(
      need(input$col2 != "", "This state does not have colleges providing satisfying the following criteria\n - predominantly bachelor's degrees\n -4 year or Master's Programs")
    )
    # 
    validate(
      need(input$col1 != input$col2, "Selected colleges must be different")
    )
    
    
    
    z <- alldata1 %>%
      filter(.,INSTNM %in% c(input$col1,input$col2))%>%
      
      plyr::rename(., c("SAT_AVG"="Average SAT Score", "ADM_RATE"="Admission Rate", "TUITIONFEE_IN"= "Tuition Fee", 
                        "GRAD_DEBT_MDN_SUPP" ="Debt after Graduation", "STABBR"="State", "INSTNM"="College" )) %>% 
      dplyr::select(.,State, College,input$chosencol)
    
  })
  
  output$plot2 <- renderPlot({
    
    if(input$button == 'View') {
      
      df1 <- alldata1 %>%
        filter(.,INSTNM %in% c(input$col1,input$col2))%>%
        dplyr::select(INSTNM,UGDS_WHITE,	UGDS_BLACK,	UGDS_HISP,	UGDS_ASIAN) %>%
        plyr::rename(c("UGDS_WHITE"="White", "UGDS_BLACK"="Black",
                       "UGDS_HISP"="Hispanic", "UGDS_ASIAN"="Asian")) %>%
        melt(id.vars='INSTNM')
      
      df1$variable <- as.factor(df1$variable)
      
      ggplot(df1) +
        
        geom_bar(aes(variable, (100* value), fill = variable),
                 stat = "identity",
                 position  =  position_dodge(width  =  0.9),
                 width = 0.5)+
        
        scale_fill_manual(name="Ethnicity",
                          values = c("blue","black","brown","orange"))+
        
        facet_wrap(~INSTNM) +
        
        labs(x = "",
             y = "Student Population (in %)") +
        
        theme_fivethirtyeight()+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.x = element_blank(),
              axis.text.y = element_text(color = "black", size = 23),
              axis.line = element_line(color = "black")) +
        
        #title strip for facets
        theme(strip.background = element_rect(fill = "navyblue" ),
              strip.text = element_text(color = "white",
                                        face = "bold",
                                        size = 16),
              panel.spacing.x = unit(0.6, "cm")) +
        
        #setting legend properties
        theme( legend.title = element_text(color = "transparent"),
               legend.text = element_text(size=16))
    }
    
    else NULL
    
  })
  
  #----------------------------------------------END compare colleges  END------------------------
  
  #----------------------------------------------STATE-WISE TOP 12 ANALYSIS------------------------
  
  
  #dataframe containing TOP 12 colleges under State 1
  top12_state <- reactive({
    # x <- alldata %>%
    #   filter(.,alldata$STABBR == input$state)%>%
    #   filter(., PREDDEG == 3) %>%
    #   filter(., CCBASIC %in% ccbasic) 
    
    x <- alldata1 %>%
      filter(.,alldata1$STABBR == input$state)%>%
      filter(., PREDDEG == 3) %>%
      filter(., CCBASIC %in% ccbasic)
    
    if (input$hilo == 'High') {
      
      x <-  arrange(x,desc(x[,input$sortby]))  %>%
        head(., n = 12)
      x
    }
    else {
      
      x <-  arrange(x,(x[,input$sortby]))  %>%
        head(., n = 12)
      x
    }
    
  })
  
  
  output$plot1<- renderPlot({
    
    base <- ggplot(top12_state()) +
      
      ggtitle(input$compare1) +
      
      coord_flip() +
      
      theme_fivethirtyeight()+
      
      
      theme(axis.title = element_text(color = "black", size = 16),
            axis.text.y = element_text(face='bold',size = 15),
            axis.text.x = element_text(face='bold',size = 15),
            axis.line = element_line(color = "black")) +
      
      labs(x="") +
      
      #setting legend properties
      theme( legend.title = element_text(color = "transparent"),
             legend.text = element_text(size=16))
    
    
    
    
    if (input$compare1 == 'Average SAT Score') {
      base +
        geom_bar(aes_string(x = "INSTNM", y = "SAT_AVG", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y = SAT_AVG, label= SAT_AVG),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1.2,
                  size =11,
                  face = 'bold',
                  color = "black")+
        labs(y = input$compare1,
             fill = 'Control')
    }
    
    else if (input$compare1 == 'Average Faculty Salary') {
      base +
        geom_bar(aes_string(x = "INSTNM", y = "AVGFACSAL", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y = AVGFACSAL, label= scales::dollar(AVGFACSAL)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1.2,
                  size =11,
                  face = 'bold',
                  color = "black")+
        labs(y = input$compare1,
             fill = 'Control')
    }
    
    else if (input$compare1 == 'Admission Rate') {
      
      base +
        geom_bar(aes_string(x = "INSTNM", y = "ADM_RATE", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y = ADM_RATE, label= scales::percent(ADM_RATE)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 0.4,
                  size = 9,
                  face = 'bold',
                  color = "black")+
        
        labs(y = input$compare1,
             fill = 'Control') +
        
        theme(axis.text.x = element_blank())
    }
    
    else if (input$compare1 == 'In-state Tuition Fee') {
      base +
        geom_bar(aes_string(x = "INSTNM", y = "TUITIONFEE_IN", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y =TUITIONFEE_IN, label= scales::dollar(TUITIONFEE_IN)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1,
                  size =11,
                  face = 'bold',
                  color = "black")+
        labs(y = input$compare1,
             fill = 'Control')
    }
    
    else if (input$compare1 == 'Percentage of undergraduates who receive a Pell Grant') {
      base +
        geom_bar(aes_string(x = "INSTNM", y = "PCTPELL", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y =PCTPELL, label= scales::percent(PCTPELL)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1,
                  size =11,
                  face = 'bold',
                  color = "black")+
        labs(y = input$compare1,
             fill = 'Control')
    }    
    
    else if (input$compare1 == 'Debt after Graduation') {
      base +
        geom_bar(aes_string(x = "INSTNM", y = "GRAD_DEBT_MDN_SUPP", fill = top12_state()$CONTROL),
                 stat = 'identity')+
        geom_text(aes(x = INSTNM, y = GRAD_DEBT_MDN_SUPP, label= scales::dollar(GRAD_DEBT_MDN_SUPP)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1.2,
                  size =11,
                  face = 'bold',
                  color = "black")+
        labs(y = input$compare1,
             fill = 'Control')
    }
    else if (input$compare1 == 'Earnings 10 years post enrollment') {
      base +
        
        geom_bar(aes_string(x = "INSTNM", y = "MD_EARN_WNE_P10", fill = top12_state()$CCBASIC),
                 stat = 'identity')+
        
        geom_text(aes(x = INSTNM, y = MD_EARN_WNE_P10, label= scales::dollar(MD_EARN_WNE_P10)),
                  position  =  position_dodge(width  =  0.9),
                  hjust = 1.2,
                  size =11,
                  face = 'bold',
                  color = "black")+
        
        labs(y = input$compare1,
             fill = 'Carnegie Classification') + 
        
        theme_minimal() +
        
        
        theme(plot.title = element_text(color = "black", size = 19, face = 'bold'),
              axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 15),
              axis.line = element_line(color = "black")) +
        
        labs(x="") +
        
        #setting legend properties
        theme( legend.title = element_text(size=14),
               legend.text = element_text(size=14))
    }
    
    else if (input$compare1 == 'Debt & Earnings') {
      
      df1 <- top12_state() %>%
        dplyr::select(INSTNM,GRAD_DEBT_MDN_SUPP,MD_EARN_WNE_P10) %>%
        plyr::rename(c("GRAD_DEBT_MDN_SUPP"="Debt", "MD_EARN_WNE_P10"="Earnings")) %>%
        melt(id.vars='INSTNM')
      
      df1$variable <- as.factor(df1$variable)
      
      ggplot(df1) +
        
        ggtitle(input$compare1) +
        
        geom_bar(aes(INSTNM, value,fill = variable),
                 stat = "identity",
                 position  =  position_dodge(width  =  0.6),
                 width = 0.4)+
        
        scale_fill_manual(name="Debt & Earnings ($)",
                          values = c("red","seagreen"))+
        
        coord_flip() +
        
        labs(x = "",
             y = "Debt & Earnings($)")+
        
        theme_fivethirtyeight()+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 15),
              axis.line = element_line(color = "black")) +
        
        #setting legend properties
        theme( legend.title = element_text(color = "transparent"),
               legend.text = element_text(size=16))
    }
    
    else if (input$compare1 == "Debt (Males vs. Females)") {
      
      df1 <- top12_state() %>%
        dplyr::select(INSTNM,FEMALE_DEBT_MDN, MALE_DEBT_MDN) %>%
        plyr::rename(c("FEMALE_DEBT_MDN"="Female", "MALE_DEBT_MDN"="Male")) %>%
        melt(id.vars='INSTNM')
      
      df1$variable <- as.factor(df1$variable)
      
      ggplot(df1) +
        
        ggtitle(input$compare1) +
        
        geom_bar(aes(INSTNM, value, fill = variable),
                 stat = "identity",
                 position  =  position_dodge(width  =  0.6),
                 width = 0.4)+
        
        scale_fill_manual(name="Gender",
                          values = c("hotpink3","midnightblue"))+
        
        coord_flip() +
        
        labs(x = "",
             y = "Debt ($)")  +
        
        theme_fivethirtyeight()+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 15),
              axis.line = element_line(color = "black")) +
        
        #setting legend properties
        theme( legend.title = element_text(face='bold',size=16),
               legend.text = element_text(size=16))
    }
    
    else if (input$compare1 == "Student population by Ethinicity (%)") {
      
      df1 <- top12_state() %>%
        dplyr::select(INSTNM,UGDS_WHITE,	UGDS_BLACK,	UGDS_HISP,	UGDS_ASIAN) %>%
        plyr::rename(c("UGDS_WHITE"="White", "UGDS_BLACK"="Black",
                       "UGDS_HISP"="Hispanic", "UGDS_ASIAN"="Asian")) %>%
        reshape2::melt(id.vars='INSTNM')
      
      df1$variable <- as.factor(df1$variable)
      
      ggplot(df1) +
        
        ggtitle(input$compare1) +
        
        geom_bar(aes(variable, (100* value), fill = variable),
                 stat = "identity",
                 position  =  position_dodge(width  =  3.3),
                 width = .8)+
        
        scale_fill_manual(name="Ethnicity",
                          values = c("blue","black","brown","orange"))+
        
        facet_wrap(~INSTNM)+
        
        labs(x = "",
             y = "Student population(%)") +
        
        theme_fivethirtyeight() +
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.x = element_blank(),
              axis.text.y = element_text(color = "black", size = 23),
              axis.line = element_line(color = "black")) +
        
        #title strip for facets
        theme(strip.background = element_rect(fill = "navyblue" ),
              strip.text = element_text(color = "white",
                                        face = "bold",
                                        size = 12),
              panel.spacing.x = unit(0.6, "cm"),
              panel.border = element_rect(colour = "black", fill=NA, size=4)) +
        
        #setting legend properties
        theme( legend.title = element_text(color = "transparent"),
               legend.text = element_text(size=16))
    }
    
  })#end of output plot1
    #-----------------------END OF STATEWISE TOP 12-----------------------------------------------------
    
    #dataframe containing TOP 12 colleges in the nation based on sort by feature
    top12_nation <- reactive({
      
      x <- alldata1 %>%
        filter(., PREDDEG == 3) %>%
        filter(., CCBASIC %in% ccbasic)
      
      if (input$hilo0 == 'High') {
        
        x <-  arrange(x,desc(x[,input$sortby0]))  %>%
          head(., n = 12)
        x
      }
      else {
        
        x <-  arrange(x,(x[,input$sortby0]))  %>%
          head(., n = 12)
        x
      }
      
    })
    
    
    output$plot0<- renderPlot({
      
      base <- ggplot(top12_nation()) +
        
        ggtitle(input$compare0) +
        
        coord_flip() +
        
        theme_fivethirtyeight()+
        
        
        theme(axis.title = element_text(color = "black", size = 16),
              axis.text.y = element_text(face='bold',size = 15),
              axis.text.x = element_text(face='bold',size = 15),
              axis.line = element_line(color = "black")) +
        
        labs(x="") +
        
        #setting legend properties
        theme( legend.title = element_text(color = "transparent"),
               legend.text = element_text(size=16))
      
      
      
      
      if (input$compare0 == 'Average SAT Score') {
        base +
          geom_bar(aes_string(x = "INSTNM", y = "SAT_AVG", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y = SAT_AVG, label= SAT_AVG),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1.2,
                    size =11,
                    face = 'bold',
                    color = "black")+
          labs(y = input$compare0,
               fill = 'Control')
      }
      
      else if (input$compare0 == 'Average Faculty Salary') {
        base +
          geom_bar(aes_string(x = "INSTNM", y = "AVGFACSAL", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y = AVGFACSAL, label= scales::dollar(AVGFACSAL)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1.2,
                    size =11,
                    face = 'bold',
                    color = "black")+
          labs(y = input$compare0,
               fill = 'Control')
      }
      
      else if (input$compare0 == 'Admission Rate') {
        
        base +
          geom_bar(aes_string(x = "INSTNM", y = "ADM_RATE", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y = ADM_RATE, label= scales::percent(ADM_RATE)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 0.4,
                    size = 9,
                    face = 'bold',
                    color = "black")+
          
          labs(y = input$compare0,
               fill = 'Control') +
          
          theme(axis.text.x = element_blank())
      }
      
      else if (input$compare0 == 'In-state Tuition Fee') {
        base +
          geom_bar(aes_string(x = "INSTNM", y = "TUITIONFEE_IN", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y =TUITIONFEE_IN, label= scales::dollar(TUITIONFEE_IN)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1,
                    size =11,
                    face = 'bold',
                    color = "black")+
          labs(y = input$compare0,
               fill = 'Control')
      }
      
      else if (input$compare0 == 'Percentage of undergraduates who receive a Pell Grant') {
        base +
          geom_bar(aes_string(x = "INSTNM", y = "PCTPELL", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y =PCTPELL, label= scales::percent(PCTPELL)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1,
                    size =11,
                    face = 'bold',
                    color = "black")+
          labs(y = input$compare0,
               fill = 'Control')
      }    
      
      else if (input$compare0 == 'Debt after Graduation') {
        base +
          geom_bar(aes_string(x = "INSTNM", y = "GRAD_DEBT_MDN_SUPP", fill = top12_nation()$CONTROL),
                   stat = 'identity')+
          geom_text(aes(x = INSTNM, y = GRAD_DEBT_MDN_SUPP, label= scales::dollar(GRAD_DEBT_MDN_SUPP)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1.2,
                    size =11,
                    face = 'bold',
                    color = "black")+
          labs(y = input$compare0,
               fill = 'Control')
      }
      else if (input$compare0 == 'Earnings 10 years post enrollment') {
        base +
          
          geom_bar(aes_string(x = "INSTNM", y = "MD_EARN_WNE_P10", fill = top12_nation()$CCBASIC),
                   stat = 'identity')+
          
          geom_text(aes(x = INSTNM, y = MD_EARN_WNE_P10, label= scales::dollar(MD_EARN_WNE_P10)),
                    position  =  position_dodge(width  =  0.9),
                    hjust = 1.2,
                    size =11,
                    face = 'bold',
                    color = "black")+
          
          labs(y = input$compare0,
               fill = 'Carnegie Classification') + 
          
          theme_minimal() +
          
          
          theme(plot.title = element_text(color = "black", size = 19, face = 'bold'),
                axis.title = element_text(color = "black", size = 16),
                axis.text.y = element_text(face='bold',size = 15),
                axis.text.x = element_text(face='bold',size = 15),
                axis.line = element_line(color = "black")) +
          
          labs(x="") +
          
          #setting legend properties
          theme( legend.title = element_text(size=14),
                 legend.text = element_text(size=14))
      }
      
      else if (input$compare0 == 'Debt & Earnings') {
        
        df1 <- top12_nation() %>%
          dplyr::select(INSTNM,GRAD_DEBT_MDN_SUPP,MD_EARN_WNE_P10) %>%
          plyr::rename(c("GRAD_DEBT_MDN_SUPP"="Debt", "MD_EARN_WNE_P10"="Earnings")) %>%
          melt(id.vars='INSTNM')
        
        df1$variable <- as.factor(df1$variable)
        
        ggplot(df1) +
          
          ggtitle(input$compare0) +
          
          geom_bar(aes(INSTNM, value,fill = variable),
                   stat = "identity",
                   position  =  position_dodge(width  =  0.6),
                   width = 0.4)+
          
          scale_fill_manual(name="Debt & Earnings ($)",
                            values = c("red","seagreen"))+
          
          coord_flip() +
          
          labs(x = "",
               y = "Debt & Earnings($)")+
          
          theme_fivethirtyeight()+
          
          
          theme(axis.title = element_text(color = "black", size = 16),
                axis.text.y = element_text(face='bold',size = 15),
                axis.text.x = element_text(face='bold',size = 15),
                axis.line = element_line(color = "black")) +
          
          #setting legend properties
          theme( legend.title = element_text(color = "transparent"),
                 legend.text = element_text(size=16))
      }
      
      
      else if (input$compare0 == "Debt (Males vs. Females)") {
        
        df1 <- top12_nation() %>%
          dplyr::select(INSTNM,FEMALE_DEBT_MDN, MALE_DEBT_MDN) %>%
          plyr::rename(c("FEMALE_DEBT_MDN"="Female", "MALE_DEBT_MDN"="Male")) %>%
          melt(id.vars='INSTNM')
        
        df1$variable <- as.factor(df1$variable)
        
        ggplot(df1) +
          
          ggtitle(input$compare0) +
          
          geom_bar(aes(INSTNM, value, fill = variable),
                   stat = "identity",
                   position  =  position_dodge(width  =  0.6),
                   width = 0.4)+
          
          scale_fill_manual(name="Gender",
                            values = c("hotpink3","midnightblue"))+
          
          coord_flip() +
          
          labs(x = "",
               y = "Debt ($)")  +
          
          theme_fivethirtyeight()+
          
          
          theme(axis.title = element_text(color = "black", size = 16),
                axis.text.y = element_text(face='bold',size = 15),
                axis.text.x = element_text(face='bold',size = 15),
                axis.line = element_line(color = "black")) +
          
          #setting legend properties
          theme( legend.title = element_text(face='bold',size=16),
                 legend.text = element_text(size=16))
      }
      else if (input$compare0 == "Student population by Ethinicity (%)") {
        
        df1 <- top12_nation() %>%
          dplyr::select(INSTNM,UGDS_WHITE,	UGDS_BLACK,	UGDS_HISP,	UGDS_ASIAN) %>%
          plyr::rename(c("UGDS_WHITE"="White", "UGDS_BLACK"="Black",
                         "UGDS_HISP"="Hispanic", "UGDS_ASIAN"="Asian")) %>%
          reshape2::melt(id.vars='INSTNM')
        
        df1$variable <- as.factor(df1$variable)
        
        ggplot(df1) +
          
          ggtitle(input$compare0) +
          
          geom_bar(aes(variable, (100* value), fill = variable),
                   stat = "identity",
                   position  =  position_dodge(width  =  3.3),
                   width = .8)+
          
          scale_fill_manual(name="Ethnicity",
                            values = c("blue","black","brown","orange"))+
          
          facet_wrap(~INSTNM)+
          
          labs(x = "",
               y = "Student population(%)") +
          
          theme_fivethirtyeight() +
          
          
          theme(axis.title = element_text(color = "black", size = 16),
                axis.text.x = element_blank(),
                axis.text.y = element_text(color = "black", size = 23),
                axis.line = element_line(color = "black")) +
          
          #title strip for facets
          theme(strip.background = element_rect(fill = "navyblue" ),
                strip.text = element_text(color = "white",
                                          face = "bold",
                                          size = 12),
                panel.spacing.x = unit(0.6, "cm"),
                panel.border = element_rect(colour = "black", fill=NA, size=4)) +
          
          #setting legend properties
          theme( legend.title = element_text(color = "transparent"),
                 legend.text = element_text(size=16))
      }
  })#end of outputPlot0
  
  #----------------------------------------------End TOP 12 ANALYSIS  End------------------------
}




shinyApp(ui, server)
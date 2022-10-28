#needed libaries for the project 
library(tidyverse) 
library(ggplot2)
library(areaplot)
library(ggcorrplot)
library(shiny)
library(dplyr)
library(broom)
library(stringi)
library(shinycssloaders)
library(highcharter)
library(shinythemes)

#new combined data file 
sui_and_eco = read_csv("sui_and_eco.csv")

# creating a custom theme for the highcharter to fit with the shiny project 
my_own_theme <- hc_theme(
  colors = c("#708090", "green", "blue", "red","yellow","orange"),
  chart = list(
    backgroundColor = "black",
    valueDecimals = 2
  ),
  title = list(
    style = list(
      color = "white",
      fontFamily = "Lato"
    )
  ),
  subtitle = list(
    style = list(
      color = "#666666",
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Lato",
      color = "gray"
    ),
    itemHoverStyle = list(
      color = "white"
    )
  )
)

options(highcharter.theme = my_own_theme)



#rounding the number of suicdes to only 2 digits.
sui_and_eco$suicides_100k <- round(sui_and_eco$suicides_100k, digits = 2)


# Define UI
ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("Economic Growth on
             Australian Suicide", theme = shinytheme("cyborg"),
             tabPanel("Australian Suicide dashboard", fluid = TRUE,
                      # Sidebar layout with a input and output definitions
              
              
              fluidRow(column(12, offset= 1, align ="centre",
                             div(img(src="https://www.dailymaverick.co.za/wp-content/uploads/MC-MHStockout-Spotlight.jpg?w=1600"
                                     
                                     , height=300, width=750))),
                      
                      fluidRow(column(12, offset= 3,align = 'centre', 
                                      titlePanel(title = '', windowTitle = 'Australian suicide'),h1(id="big-heading","Suicide and Economic Growth"),
                                      tags$style(HTML("#big-heading{color: white;font-size: 35px; 
                                           font-style: bold;}")))
                      ),  sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(inputId = "x",
                                      label = "Select X variable:",
                                      choices = c("year","population","age","sex","gdppc"),
                                      selected = "year"),
                          
                          selectInput(inputId = "y",
                                      label = "Select Y variable:",
                                      choices = c("suicides_no","population","suicides_100k"
                                                  ,"gdppc","age","sex"),
                                      selected = "suicides_no"),
                        
                          
                          
                          selectInput(inputId = "z",
                                      label = "Colored by Age Ranges :",
                                      choices = c("age"),
                                      selected = "age"),
                        
                        selectInput('column', 'Select columns to view data entries:', 
                                    names(sui_and_eco), 
                                    multiple = TRUE)),
                        
                      
                        mainPanel(
                          br(),
                          DT::DTOutput('table'),
                          plotOutput("plot3")
                        )
                      )),
              br(),
              textOutput("global_text5"),
              br(),
              textOutput("global_text0"),
              br(),
              highchartOutput("hcontainer2", height = "500px"),
              br(),
              br(),
              textOutput("global_text1"),
              br(),
              br(),
              highchartOutput("hcontainer", height = "500px"),
              br(),
              br(),
              textOutput("global_text3"),
              br(),
              br(),
              highchartOutput("hcontainer1", height = "500px"),
              br(),
              br(),
              textOutput("global_text2"),
              br(),
              br(),
              highchartOutput("hcontainer3", height = "500px"),
              br(),
              br(),
              textOutput("global_text4"), br(), br(), ),

             
             tabPanel("More Infomation", fluid = TRUE, icon = icon("info-circle"),
                      titlePanel("Data Sources and Data Checking"), br(),
                      mainPanel("----------------------------------------------------------DATA SETS--------------------------------------------------------------"),
                      mainPanel("The first datasets used for this dashboard comes from the World Health Organisation. More specifaclly the  
                                                             Mortality Database from 1979-2015 
                                                             that has been compiled to include 
                                                             each countries suicide statistics 
                                                             that includes sex, age, population 
                                                             in that year and the number of suicides that took place."),br(),
                      mainPanel("As for repersenting the Economic growth portion of the issue the data used comes from a well respected orginisation named The Maddison Project which provides a database of 
                                information on economic growth and income levels 
                                over long periods of time. This dataset in particulr is the 2020 version that holds 
                                169 countries up to the 2018 year."),
                      
                      mainPanel("---------------------------------------------------DATA CHECKING----------------------------------------------------------"), br(""), 
                      mainPanel("•	Data type check: Which envolves ensuring that the data provided is correctly identified as the correct data type in the dataset. e.g., Character types that should be numeric."),
mainPanel("•	Removed irrelevant data from the dataset: From the World Health Organisation data set, data was removed that was not from Australia."), br(),
mainPanel("•	Handle missing data: As allthe Australian 2005 suicide data was missing it was removed form the dataset. "),
mainPanel("•	Consistency check: The final check was to understand if there is any inconsistency with how the data is presented. 
Through using this checklist above, it showed that there were missing values in 
the World Health Organisation dataset. After removing the irrelevant data leaving 
only the Australian data left. The dataset displayed NA values for all the suicide 
numbers found in the year 2005 for all age and sex values. Consequently, as the
suicide data is fundamental for the statistical analysis portion of the exploration 
it would be most appropriate to remove all the 2005 data pertaining to Australian suicide. 
The tool used for this action was the generic na.omit function which returns the object 
with the incomplete cases removed from it."),
mainPanel("======================================================================")),
             
             tabPanel("References", fluid = TRUE, icon = icon("globe-americas"),
                      titlePanel("References"), mainPanel(
                        
                        "DATA SET LINKS:", br(),
                        
                        tags$a(href = "https://www.kaggle.com/datasets/szamil/who-suicide-statistics",
                                                                  "1. World Health Organisation Mortality Database from 1979-201", target = "_blank"), br(),
                        tags$a(href = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2020",
                               "2. The Maddison Project which provides a database of information on economic growth and income levels", target = "_blank"), br(),
                      
                
                      
                      "•	Elbogen, E. B., Lanier, M., Montgomery, A. E., Strickland, 
                                                           S., Wagner, H. R., & Tsai, J. (2020). 
                                                           Financial strain and suicide attempts in 
                                                           a nationally representative sample of US adults. 
                                                           American Journal of Epidemiology, 189(11), 1266-1274."), br(),
                      mainPanel("•	Wahlbeck, K., & Awolin, M. (2009). 
                                The impact of economic crises on the risk of 
                                depression and suicide: a literature review."), br(), br(),
                      mainPanel("• Stats & Facts. (n.d.). Suicide Prevention Australia. https://www.suicidepreventionaust.org/news/statsandfacts

.com"), br(),br(), mainPanel("• Australian Institute of Health and Welfare. (2021). Deaths by suicide over time. Australian Institute of Health and Welfare. https://www.aihw.gov.au/suicide-self-harm-monitoring/data/deaths-by-suicide-in-australia/suicide-deaths-over-time

"),)
  )
)
             
             
             
server <- function(input, output, session) {
  
  output$table <- DT::renderDT({
    sui_and_eco%>%
      select(c(input$column))
  })
          
               output$plot3 <- renderPlot({
                 ggplot(data = sui_and_eco, aes_string(x = input$x, 
                                                       y = input$y, color = input$z)) +
                   geom_point() + geom_smooth(method=lm, se=FALSE) +
                   theme(
                     # get rid of panel grids
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     # Change plot and panel background
                     plot.background=element_rect(fill = "#E2DED0"),
                     panel.background = element_rect(fill = 'black'),
                     # Change legend 
                     legend.position = c(0.25, 0.9),
                     legend.direction = "horizontal",
                     legend.background = element_rect(fill = "black", color = NA),
                     legend.key = element_rect(color = "gray", fill = "white"),
                     legend.title = element_text(color = "white"),
                     legend.text = element_text(color = "white")
                   )})
               output$hcontainer1 <- renderHighchart({
                 hc3 <- sui_and_eco %>% 
                   group_by(gdppc) %>% 
                   summarise(suicides_100k = sum(suicides_100k)) %>% 
                   hchart('areaspline', hcaes(x = 'gdppc', y = suicides_100k), name = "Number of suicides (per 100k)", regression = TRUE) %>%
                   hc_add_dependency("plugins/highcharts-regression.js") %>% 
                   hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("GDP: <b> {point.x} </b> <br> Number of suicides (per 100K): <b>{point.y}</b>")) %>%
                 hc_title(text = "<b>Corrolation between GDP (per capita) and the number of suicides (per 100k)</b>") 
               })
               
               output$hcontainer2 <- renderHighchart({
                 hc2 <- sui_and_eco %>% 
                   group_by(year) %>% 
                   summarise(suicides_100k = sum(suicides_100k)) %>% 
                   hchart('spline', hcaes(x = 'year', y = suicides_100k), name = "Number of suicides (per 100k)", regression = TRUE) %>% 
                   hc_add_dependency("plugins/highcharts-regression.js") %>% 
                   hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b> {point.x} </b> <br> Number of suicides (per 100K): <b>{point.y}</b>")) %>%
                hc_title(text = "<b>Corrolation between the year from 1979-2015 and the number of suicides (per 100k)</b>") 
               })
               
               output$hcontainer <- renderHighchart({
                 hc <- sui_and_eco %>% 
                   filter(year >= 1979) %>% 
                   group_by(sex, age) %>% 
                   summarise(suicide_100k = (sum(suicides_no)) / (sum(population)) * 100000) %>%
                   hchart('column', hcaes(x = 'age', y = suicide_100k, group = 'sex')) %>%
                   hc_colors(c("red", "blue")) %>% 
                   hc_title(text = "<b>Average Sex and Age range distribution between the number of suicides (per 100k) from 1979-2015 </b>") 
               })
               
               output$hcontainer3 <- renderHighchart({
                 hc4 <- sui_and_eco %>%
                   group_by(age) %>% 
                   summarise(suicides = sum(suicides_no)) %>%
                   hchart(
                     "pie", hcaes(x = "age", y = suicides),
                     name = "Total number of suicides"
                   ) %>% hc_tooltip(borderWidth = 1.5, headerFormat = "", pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Number of suicides (per 100K): <b>{point.y}</b>")) %>% 
                 hc_title(text = "<b>Total number of  Australian suicides for each Age range from 1979-2015 </b>")
               })
               
               output$global_text0 <- renderText({
                 "The widget above allows to change the variables of both the x and y axies to explore for yourself the realtionships between which ever variables you find most interesting. the bottom select tool allows you
                 to have access to the full relevent datasets used in this dashboard. The more vaiables to pick the more infomation you will have access to. If you wish to remove this explore feature simply delete/backspace the variables to exit back to display the graph.
                 The interactive graph which displays differnt colour lines are the linear regression lines for each of the age groups shown in the key in the top left portion of the graph.  "
               })
               
               output$global_text1 <- renderText({
                 "The line plot above shows the total number of suicides for every 100,000 people for that particualr year as shown on the x-axis.
                 Their are many factors that influence this relationship such as social, economic and environmental aspects. This visual narrative will 
                 exclusively focus on the econimic factor of this issue. This plot allows you to hover
                 over a particular time period that may intrest you and it will show you the exact number of deaths for that specific year you selcted.
                 additionally their is a regression line to help visualize this relationship."
               })
               
               output$global_text2 <- renderText({
                 "The line area plot with a regression line above depicts the relationship between the the Gross domestic product (per capita) and the number of suicdes for every 100,000 
                 poeple in Australia for the current GDP. As you can see from the green regression line the relationhsip is relatively steady over the past 35 years which suggests that GDP is unliky 
                a significant factor when it comes to the relationship with the number of suicides and GDP in Australia. This slight negative trend shown by the regression line supports some of the latest research economic strain has on suicide (Elbogen et al., 2020),(Wahlbeck & Awolin (2009). 
                 "
               })
               
               
               output$global_text3 <- renderText({
                 "The barplot above displays the average age range distribution and from which sex for every 100,000 people in Australia.
                 this barplot allows you to visualise not only which age range is commiting the most amount of suicide but also from which sex.
                 for example as you can see the male bars are signifacntlly a lot greater than the female bars which suggests that males are commiting 
                 suicide more than female from the period 1979-2015 in Australia."
               })
               
               
               output$global_text4 <- renderText({
                 "The pie chart above displays the total number of suicides for each age range from the years 1979-2015.
                 if you highlight a particualr age range you will see a popup that will show you the percentage of the number of suicides
                 that particualr age range has commited. "
               })
               
               output$global_text5 <- renderText({
                 "According to the Australian suicide prevention organisation, suicide in Austrlia was the 15th leading cause of death overall which accounts for 1.8% of total deaths in the country. Therefore, 
                 looking into the history of suicide in Australia today is an important issue."
               })
               
               
               
             }
             
             
shinyApp(ui = ui, server = server)
             
             
             
                      
            
                      
                      
                      
                      
#Choices for drop-downs
vars_borough <- c(unique(NYPD_for_map$Borough))
vars_offense <- c(unique(NYPD_for_map$Offense))



shinyUI(navbarPage("7 Major Felonies in NYC", id="nyc", theme = shinytheme("journal"),
                   
        tabPanel("Insights Before Exploration", 
                 
                 fluidRow(
                   h1(strong("The Dataset: 7 Major Felony Incidents in New York City")),
                   p(),
                   h3("- The dataset is obtained from", a("link", href = "https://data.cityofnewyork.us/Public-Safety/NYPD-7-Major-Felony-Incidents/hyij-8hr7")),
                   h1(strong("What is CompStat?")),
                   p(),
                   tags$ul(
                     tags$li(h3("A strategic control system")),
                     tags$li(h3("Collection and feedbackof information on crime and related quality of life issues")),
                     tags$li(h3("The Compstat Process: Collect, Analyze, Map Crime Data, Essential police performance measures"))
                   ),
                   h1(strong("Crime Types")),
                   p(),
                   h3("- In the dataset there are 7 different types of crimes. 3 of them are intuitive however I want to clarify the definitons of the remaining 4 types:"),
                   tags$ul(
                     tags$li(h3("Grand Larceny: Stealing property valued in excess of $1000 – money, car, antiques, clothing etc.-")),
                     tags$li(h3("Grand Larceny of Motor Vehicle: Stealing a vehicle that is worth between $30,000 - $60,000")),
                     tags$li(h3("Felony Assault: Requires that a victim suffers a physical injury")),
                     tags$li(h3("Murder & Non-Negligent Manslaughter: Killing of one human being by another"))
                     
                   )
                   
                  
                 
                 
                 )
        ),

                 
               
    
        tabPanel("Map",
            div(class = "outer",

                tags$head(
                  includeCSS("style.css"),
                  includeScript("gomap.js")
                ),
                
                leafletOutput("map", width="100%", height="100%"),
                
                absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 350, height = "auto",
                              
                              hr(),

                              h1("HOW SAFE IS YOUR NEIGHBOURHOOD?"),
                              
                              h4("Please choose first a Borough then a crime type to start.\nYou can also filter the crimes by dates ranging from 2006-2015 "),
                              br(),
                              h4("For privacy reasons, incidents have been moved to the midpoint of the street segment on which they occur."),
                              hr(),
                              checkboxGroupInput("borough", h4("Please select one borough"), vars_borough),
                              checkboxGroupInput("offense", h4("Please select offense type"), vars_offense),
                              dateRangeInput("date", label = h3("Date range"),
                                             start = "2006-01-01",
                                             end = "2015-12-31")
                              
                ) #End of absolutepanel
            )#End of div
        ), # End of map tab panel
        
        tabPanel("Graphical Exploration",
            
            fluidRow(
              
              titlePanel("Felonies in NYC Graphical Exploration")
            ),
            
            br(),
            
            fluidRow(
              plotOutput("barchart1", width = "100%", height = 600)
              
            ),
            
            hr(),
            
            
            
            fluidRow(
                 
                 column(3,
                        selectizeInput("x","Select X axis:", c("None" ,"Offense", "Borough", "Year"))
                        ),
                 
                 column(3,
                        selectInput('y', 'Select Y axis:',
                                    c("None","Total_Felonies", "Normalized"))
                 ),

                 column(3,
                        selectInput('facet_row', 'Facet Row by:',
                                    c(None='.', c("Offense", "Borough", "Year")))
                        ),

                 column(3,
                        selectInput('facet_col', 'Facet Column by:',
                                    c(None='.', c("Offense", "Borough", "Year")))
                        )
                
            ) #End of fluidRow
          
        ), #End of Graphical Exploration tabPanel
        
        
        
        
      tabPanel("Data Table",
               
               fluidRow(
                         column(4,
                                selectInput("off",
                                            "Offense",
                                            c("All",unique(as.character(NYPD_for_map$Offense))))),

                         column(4,
                                selectInput("bor",
                                            "Borough",
                                            c("All", unique(as.character(NYPD_for_map$Borough))))),
              
                        column(4,
                               selectInput("c","Year", c("All", c(2006:2015))))
                 
               ), #End of fluidRow
               
               hr(),
            
              fluidRow(
                DT::dataTableOutput("data_table")
              )
      ) #End of tabPanel Data Table

  )
) # End of ShinyUI
      
              
                   

                   
                   
                   
                   
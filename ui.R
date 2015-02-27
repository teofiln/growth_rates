library(shiny)
library(shinythemes)

# get a named list of the active experiments 
# not used
#EXPS <- as.list(dir(pattern="*.csv"))
#names(EXPS) <- EXPS  # to be used in selectInput("Experiment"...) bellow

# update the datasets 
# creates symbolic links from original files in different folder
# will work only localy
# not used
#system("./update_datasets.sh")
#system("./copy_datasets.sh")

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel(h4("Cyclotella salinity experiments")),
  tabsetPanel(id = "tabsets",
              
              tabPanel("Growth curves", value = "tab1",
                       sidebarLayout(fluid = TRUE,
                         sidebarPanel(width = 4,
                                      radioButtons("Experiment1",
                                                   label = h4("Experiment"),
                                                   choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                  "C. wallercreekii tubes temperature" = 2, 
                                                                  "C. cryptica tubes salinity" = 3, 
                                                                  "C. wallercreekii flasks salinity" = 4, 
                                                                  "C. wallercreekii flasks 2 salinity" = 5,
                                                                  "C. cryptica flasks salinity" = 6),
                                                   selected = 1),
                                      
                                      uiOutput("whichSelectInput1")
                                      ), # end sidebarPanel
                         mainPanel(width = 8,
                                   plotOutput('Plot1', height = 800)
                                   ) # end mainPanel
                         ) # end first sidebarLayot
                       ), # end first tab
            
            tabPanel("Compare slopes", value = "tab2",
                     sidebarLayout(
                       sidebarPanel(width = 2,
                                    radioButtons("Experiment2",
                                                 label = h4("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    
                                    uiOutput("whichSelectInput")
                                    ),
                       mainPanel(width = 10,
                                 plotOutput('Plot2', height = 500),
                                 tableOutput('TableAOV')
                                 ) # end mainPanel
                       ) # end second sidebarPanel
                     ), # end second tab

            tabPanel("Growth rates through time", value = "tab4",
                     sidebarLayout(
                       sidebarPanel(width = 4,
                                    radioButtons("Experiment4",
                                                 label = h4("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    uiOutput("whichCheckBoxInput4")#,
                       ),
                       mainPanel(width = 8,
                                 plotOutput('Plot4', height = 800)
                       ) # end mainPanel
                     ) # end fourth sidebarPanel
            ), # end fourt tab

            tabPanel("Compare growth rates", value = "tab3",
                     sidebarLayout(
                       sidebarPanel(width = 4,
                                    radioButtons("Experiment3",
                                                 label = h4("Experiment"),
                                                 choices = list("C. wallercreekii tubes salinity" = 1, 
                                                                "C. wallercreekii tubes temperature" = 2, 
                                                                "C. cryptica tubes salinity" = 3),
                                                 selected = 1),
                                    uiOutput("whichCheckBoxInput3")#,
                                   # actionButton("actionPlot3", "Plot")
                       ),
                       mainPanel(width = 8,
                                 plotOutput('Plot3', height = 800)#,
                       ) # end mainPanel
                     ) # end third sidebarPanel
                  ) # end third tab
        ) # end tabsetPanel

)) # end shinyUI and fluidPage

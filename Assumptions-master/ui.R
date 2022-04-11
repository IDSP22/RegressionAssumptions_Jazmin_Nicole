
# Future work
# source("ticTacToe.R")

# Define UI ----
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Regr. Assumptions",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Regression_Assumptions")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      
      # hides sidebar toggle "hamburger" icon (I HATE IT)
      tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),    
      
      # specifying dimensions NOT really necessary 
      #width = 250,
      
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "instruction", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prereq", icon = icon("book")),
        menuItem("Challenge", tabName = "explore", icon = icon("cogs")),
        menuItem("Game", tabName = "qqq", icon = icon("gamepad")),
        menuItem("References", tabName = "refs", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo", 
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview ----
        tabItem(
          tabName = "instruction",
          h1("Regression Assumptions and Diagnostics"),
          
          # Revised overview text for learner to know what to expect
          p("This app will allow you to explore how to read diagnostic plots 
            while interacting with different transformations to help you better 
            understand some regression assumptions. In addition, will provide a 
            preview on how to explore data before modeling."),
          
          h2("Instructions"),
          p("There are three parts to this app: a Prerequisites page where you can
            refresh your understandings of the ideas, a Challenge page, and a 
            Tic-Tac-Toe game."),
          
          h3("Challenge Page"),
          p("For the Challenge page, the objective is to show you how Regression Assumptions can be explored BEFORE and AFTER fitting a linear model."),
          p(""),
          tags$ul(
            tags$li("A ", tags$em("Mystery Model"), " is generated with variables or their
                    transformations being the response (Y) or the predictor
                    variables (X1, X2)."),
            tags$li("Examine how the diagnostic plots change when you adjust the 
                    predictors and response variables using different transformations. 
                    Notice that transforming the Y or X variables will effect 
                    plots in different ways. You also have the option to change 
                    the variances and sample size of each term."),
            tags$li("Read the challenge prompt and adjust the sliders to find a
                    potential solution."),
            tags$li("Once you have an answer, check to see if you are correct."),
            tags$li("Complete as many challenges as you desire.")
          ),
          h3("Tic-Tac-Toe Game"),
          p("Your objective is to win Tic-Tac-Toe. You'll be able to choose
            whether you play as the X's or O's. You'll select a square and a 
            question will appear. Answer the question correctly, and you'll get
            that square; answer incorrectly and you'll lose that square."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "Prerequisites",
              icon = icon("book"),
              style = "default",
              size = "large")
          ),
          br(),
          h2("Acknowledgements"),
          p("This app was developed and coded by TJ McIntyre, with the help 
            of Ryan Voyack and was updated by Lydia Bednarczyk.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/3/2021 by NJH.")
          )
        ),
        ### Prerequisites ----
        tabItem(
          tabName = "prereq",
          h2("Prerequisites"),
          p("In order to get the most out of this app, it is important to 
            understand background information about assumptions and diagnostic
            plots in regression."),
          tags$ul(
            tags$li("Transforming the x values is appropriate when non-linearity 
                    is the only problem (i.e., the independence, normality, 
                    and equal variance conditions are met). Transforming the 
                    y values should be considered when non-normality and/or 
                    unequal variances are the problems with the model."),
            tags$li("The Fitted vs Residuals plot can be used to check the 
                    assumption of linearity (any location on the x axis, the 
                    average residual should be close to 0) and it can also be 
                    used to check the assumption of equal variances (at any 
                    location on the x axis, the variability of the residual 
                    should be similar)."),
            tags$li("The Normal Q-Q plot can be used to check the assumption 
                    of normal errors: i.e. the majority of the points should 
                    be a straight line. Skewness can also be seen by this plot. 
                    See the ", 
                    a(href = 'https://psu-eberly.shinyapps.io/QQ_Plot/', 'Q-Q 
                            Plot', class = "bodylinks"), " app for further
                    exploration."),
            tags$li("The Scale-Location plot can be used to check the assumption 
                    of equal variances, at any location of the x axis, the upper 
                    bound of the residuals should be similar."),
            tags$li("The Cook's Distance plot shows the values of leverage, 
                    standardized residuals, and Cook's Distance of each data point
                    which can be used to determine high leverage points, outliers 
                    and influential points.")
          ),
          br(),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "start",
              label = "Explore!",
              icon = icon("bolt"),
              style = "default",
              size = "large"
            )
          )
        ),
        ### Challenge page ----
        tabItem(
          tabName = "explore",
          h2("Transformations, Sample Size, and Variances vs. Diagnostic Plots"),
          tags$ol(
            tags$li("Select a mystery model to analyze. Each model is generated 
                    with Y as the response variable and X1 and X2 as the
                    predictor variables."),
            tags$li("Read the challenge question and adjust the sliders accordingly
                    to find a potential solution."),
            tags$li("Click the 'View Feedback' button to see if you are correct."),
            tags$li("Once you complete a challenge, click the 'New Challenge' 
                    button to receive a new challenge question.")
          ),
          p("You can also click the 'New sample' button to analyze
            a different sample of data from the mystery model selected. Once you
            have completed your desired amount of challenges, click the 'Play' 
            button to move onto the tic-tac-toe game."),
          br(),
          
      # one fluid row / page -----------
          fluidRow(
            
    # Replicate same structure but with two tabs that are subject to the slider controls
    
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
          
          # Sidebar control
          wellPanel(
            h3("Challenge Controls"),
            selectInput(
              inputId = "model", 
              label = "Select mystery model", 
              choices = c("Model 1", "Model 2", "Model 3")
            ),
            sliderInput(
              inputId = "n",
              label = "Sample size", 
              min = 10,
              max = 510, 
              value = 50, 
              step = 5
            ),
            h4("Transformation Controls"),
            selectInput(
              inputId = "x", 
              label = "Transformation of X1", 
              choices = c(
                "Log(X1)" = "logx", 
                "Square root of X1" = "sqrtx",
                "None" = "nonex"), 
              selected = "nonex"
            ),    
            sliderInput(
              inputId = "x1v", 
              label = "Variance of X1", 
              min = 0, 
              max = 20, 
              value = 2, 
              step = 1
            ),
            br(),
            selectInput(
              inputId = "x2", 
              label = "Transformation of X2", 
              choices = c(
                "Log(X2)" = "logx2", 
                "Square root of X2" = "sqrtx2", 
                "None" = "nonex2"),
              selected = "nonex2"
            ), 
            sliderInput(
              inputId = "x2v", 
              label = "Variance of X2", 
              min = 0, 
              max = 20, 
              value = 2, 
              step = 1
            ),
            br(),
            selectInput(
              inputId = "y", 
              label = "Transformation of Y", 
              choices = c(
                "Log(Y)" = "logy",
                "Square root of Y" = "sqrty",
                "None" = "noney"
              ), 
              selected = "noney",
            ),
            sliderInput(
              inputId = "yv", 
              label = "Variance of Y", 
              min = 0, 
              max = 20, 
              value = 2, 
              step = 1
            ),
            bsButton(
              inputId = "submitD", 
              label = "New sample", 
              style = "default",
              icon = icon("retweet"), 
              size = "large"
            )
          )
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Tabset w/ before and after plots ----
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Before linear model fit",
                             withMathJax(),
                             useShinyalert(),
                             h2("Data analysis"),
                             p("Diagnostic plots test for three main assumptions: 
                              a) Normally distributed errors, 
                              b) Constant variance of errors, and, 
                              c) Points of influence or outliers. 
                              This section is meant to demonstrate that there are certain trends in the data, that could be explored before modelling."),

                            p("Normally distributed errors could be obtained if either the outcome variable or the predictor variables are fairly symmetric. These properties could be analyzed with a boxplot of the variables. 
                            Also, it is important to notice that as the sample size increases, is less relevant to analyze this assumption in the data. As regression would be robust against violations of this assumption."),
                            
                            p("Whereas constant variance of errors and outliers couls also be analyzed either in the boxplot or with a scatterplot."),
                             
                             plotOutput(
                               outputId = "boxplot",
                               width = "100%",
                               height = "750px"
                             )
                             ),
                    
                    
                    # column 2: diagnostic plots ---------
                    tabPanel("Aftter linear model fit", 
                               
                               h3("Challenge"),
                              
                               uiOutput("challenges", class = "largerFont"),
                               plotOutput(
                                 outputId = "plots",
                                 width = "100%",
                                 height = "750px"
                               ),
                               tags$script(HTML(
                                 "$(document).ready(function() {
                document.getElementById('plots').setAttribute('aria-label',
                `Collage of dynamic plots including Residuals vs. Fitted, Normal 
              Q-Q, Scale-Location, and Residuals vs. Leverage.`)
               })"
                               )),
                               br(),
                               bsButton(
                                 inputId = "answer", 
                                 label = "View Feedback", 
                                 style = "default",
                                 size = "large",
                                 disabled = FALSE
                               ),
                               uiOutput("answers"),
                               br(),
                               fluidRow(
                                 column(
                                   width = 2,
                                   offset = 0,
                                   bsButton(
                                     inputId = "challenge", 
                                     label = "New Challenge", 
                                     style = "default",
                                     size = "large",
                                     disabled = FALSE
                                   )
                                 ),
                                 column(
                                   width = 2,
                                   offset = 8,
                                   div(
                                     style = "text-align: right;",
                                     bsButton(
                                       inputId = "begin",
                                       label = "Play!",
                                       icon = icon("bolt"),
                                       style = "default",
                                       size = "large"
                                     )
                                   )
                                 )
                               )
                               
                             , 
                             
                             
                             
                             )
                    
                    
        )
        
      )
    )
    
    
            
          ) # end of fluidRow to hold entire thing
    
    
        ),
        
        ### Tic-Tac-Toe game ----
        tabItem(
          tabName = "qqq",
          withMathJax(),
          useShinyalert(),
          h2("Regression Tic-Tac-Toe"),
          p("To play, click on any one of the buttons that have a question mark.
            A question will appear to the right with possible answers. If you 
            answer correctly, you will take the square; if not, the computer will
            take the square. Try your best to win the game!"
          ),
          # tttUI(namespaceID = "gamePage")
          # Pre-module Code
          h3(
            uiOutput("player")
          ),
          fluidRow(
            div(
              class = "col-sm-12 col-md-4",
              h3("Game Board"),
              br(),
              uiOutput(
                "gameBoard",
                class = "game-board")
            ),
            div(
              class = "col-sm-12 col-md-8",
              h3("Question"),
              withMathJax(
                uiOutput("question")),
              uiOutput("extraOutput"),
              h3("Answer"),
              uiOutput("answer"),
              uiOutput("mark"),
              uiOutput("feedback"),
              bsButton(
                inputId = "submit",
                label = "Submit",
                size = "large",
                style = "default",
                disabled = TRUE
              ),
              bsButton(
                inputId = "reset",
                label = "Reset Game",
                color = "primary",
                size = "large",
                style = "danger"
              ),
              br(),
              #These two triggers help with MathJax re-rendering
              uiOutput("trigger1"),
              uiOutput("trigger2")
            )
          )
        ),
        
        ### References ----
        tabItem(
          tabName = "refs",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Attali, Dean and Edwards, Tristan. (2020). shinyalert: Easily Create 
            Pretty Popup Messages (Modals) in 'Shiny'. (v2.0.0) [R Package]. Available
            from https://CRAN.R-project.org/package=shinyalert"
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. 
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package]. 
            Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards 
            with 'Shiny'. (v0.7.1) [R Package]. 
            Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). 
            shiny: Web application framework for R. (v1.4.0) [R Package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hijmans, Robert J. (2021). raster: Geographic Data Analysis and Modeling. 
            (v3.4-10) [R Package]. Available from https://CRAN.R-project.org/package=raster"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs 
            widgets for shiny. (v0.5.0) [R Package]. Available from 
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )



  
  
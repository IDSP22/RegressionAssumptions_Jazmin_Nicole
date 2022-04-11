

# source helper functions from R script


# Server ----
# source helper functions right away
source("helperFunctions.R", local=TRUE)

server <- function(input, output, session) {
  

  
  #Go buttons ---- 
  observeEvent(
    eventExpr = input$infoex,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = " Move the sliders to see their effect on the diagnostic plots.",
        type = "info"
      )
    })
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = "Instructions",
        text = "Click on desired square, answer the question, then hit submit and 
      go to next question.",
        type = "info"
      )
    })
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prereq"
      )
    })
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    })
  observeEvent(
    eventExpr = input$begin,
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "qqq"
      )
    })
  #Gray out buttons ----
  
  observeEvent(
    eventExpr = input$start, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = TRUE
      )
    })
  observeEvent(
    eventExpr = input$challenge, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = FALSE
      )
    })
  observeEvent(
    eventExpr = input$answer, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "answers", 
        disabled = TRUE
      )
    })
  observeEvent(
    eventExpr = input$begin, 
    handlerExpr = {
      updateButton(
        session = session, 
        inputId = "submit", 
        disabled = TRUE
      )
    })
  
  ## Plot outputs ----
  observeEvent(
    eventExpr = c(input$go, input$submitD), 
    handlerExpr = {
      output$plots <- renderPlot({
        if (input$model == "Model 1") {
          nonex <- rnorm(input$n, mean = 1000, sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n, mean = 1000, sd = sqrt(input$x2v))
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- (rnorm(input$n, mean = 3, sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        } else if (input$model == "Model 2") {
          nonex <- rnorm(input$n, mean = 1000, sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n, mean = 1000, sd = sqrt(input$x2v))
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- rnorm(input$n, mean = 1000, sd = input$yv)
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        } else { 
          nonex <- rnorm(input$n, mean = 1000, sd = input$x1v)**2
          nonex2 <- rnorm(input$n, mean = 1000, sd = input$x2v)**2
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- (rnorm(input$n, mean = 1000, sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        }
        
        ### Modeling choices ----
        if (input$x == 'none') {
          modelx = nonex
        } else if (input$x == 'logx') {
          modelx = logx
        } else {
          modelx = sqrtx
        }
        
        if (input$x2 == 'none') {
          modelx2 = nonex2
        } else if (input$x == 'logx2') {
          modelx2 = logx2
        } else {
          modelx2 = sqrtx2
        }
        
        if (input$y == 'none') {
          modely = noney
        } else if (input$x == 'logy') {
          modely = logy
        } else {
          modely = sqrty
        }
        
        par(mfrow = c(2,2))
        plot(
          lm(modely ~ modelx + modelx2 ),
          pch = 19, 
          lwd = 2,
          id.n = 0,
          cex.axis = 1.5, 
          cex.lab = 1.5,
          cex.caption = 1.5
        )
      })
    })
  
  index <- reactiveVal(7)
  
  #Outputting a new activity ----
  observeEvent(input$challenge | input$go, {
    if (input$model == "Model 1") {
      index(sample(x = c(1:7,18:20), size = 1))
    } else if (input$model == "Model 2") {
      index(sample(x = c(8:13,18:20), size = 1))
    } else {
      index(sample(x = 14:20, size = 1))
    }
    
    output$challenges <- renderUI({
      p(bankc[index(), 2])
    })
  })
  
  #Output for answer box when nothing is in the box ----
  observeEvent(
    eventExpr = input$challenge,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
  ) 
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
  )  
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      output$answers <- 
        renderText("Please hit the view feedback button for feedback")
    }
  ) 
  
  # tttServer(
  #   namespaceID = "gamePage",
  #   gridSize = 3,
  #   questionBank = questionBank,
  #   parent = session
  # )
  
  # Pre-Module code ----
  #output of the answers ----
  observeEvent(
    eventExpr = input$answer,
    handlerExpr = {
      output$answers <- renderUI({
        p(bankc[index(), 3])
      })
    }
  )
  #Tic tac toe ----
  # Variables
  activeBtn <- NA
  activeQuestion <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  
  gameProgress <- FALSE
  
  
  # Define navigation buttons
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "qqq")
    })
  
  # Read in data and generate the first subset ----
  qSelected <- 
    sample(
      x = seq_len(nrow(questionBank)),
      size = TILE_COUNT,
      replace = FALSE)
  
  gameSet <- questionBank[qSelected,]
  
  # Program the Reset Button
  observeEvent(
    eventExpr = input$reset,
    handlerExpr = {
      .generateStatement(session,
                         object = "reset",
                         verb = "interacted",
                         description = "Game board has been reset.")
      .gameReset()
    })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    sapply(
      X = 1:GRID_SIZE,
      FUN = function(row) {
        sapply(X = 1:GRID_SIZE,
               FUN = function(column) {
                 id <- paste0("grid-",
                              row,
                              sep = "-",
                              column)
                 board[[index]] <<- tags$li(
                   actionButton(
                     inputId = paste0("grid-",
                                      row,
                                      sep = "-",
                                      column),
                     label = "?",
                     color = "primary",
                     style = "bordered",
                     class = "grid-fill"
                   ),
                   class = "grid-tile"
                 )
                 observeEvent(
                   eventExpr = session$input[[id]],
                   handlerExpr = {
                     activeBtn <<- id
                     .boardBtn(id)
                     .generateStatement(session,
                                        object = activeBtn,
                                        verb = "interacted",
                                        description = paste0("Tile ",
                                                             activeBtn,
                                                             " selected. Rendering
                                                               question: ",
                                                             activeQuestion,
                                                             ".")
                     )
                     output$mark <- renderUI(NULL)
                     output$feedback <- renderUI(NULL)
                   }
                 )
                 index <<- index + 1
               })
      })
    
    tags$ol(
      board,
      class = paste(
        "grid-board",
        "grid-fill",
        paste0("grid-",
               GRID_SIZE,
               "x",
               GRID_SIZE)
      ))
  })
  
  # Program Submit Button ----
  observeEvent(
    eventExpr = input$submit,
    handlerExpr = {
      index <- .tileIndex(activeBtn)
      answer <- ""
      if (gameSet[index, "format"] == "numeric") {
        answer <- gameSet[index, "answer"]
      }
      else {
        answer <- gameSet[index, gameSet[index, "answer"]]
      }
      success <- input$ans == answer
      
      if (is.null(success) || length(success) == 0) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Please select an answer before pressing Submit.",
          type = "error"
        )
      }
      else if (success) {
        updateButton(
          session = session,
          inputId = activeBtn,
          label = player,
          disabled = TRUE
        )
        scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
        
        output$mark <- renderIcon(
          icon = "correct",
          width = 50
        )
        output$feedback <- renderUI(
          paste("Your answer is correct!")
        )
      }
      else {
        updateButton(
          session = session,
          inputId = activeBtn,
          label = opponent,
          disabled = TRUE
        )
        scoreMatrix <<- .score(scoreMatrix,
                               activeBtn,
                               -1)
        output$mark <- renderIcon(
          icon = "incorrect",
          width = 50
        )
        output$feedback <- renderUI(
          paste("Your answer is incorrect. The correct answer is",
                answer,
                ".")
        )
      }
      
      # Check for game over states
      .gameState <- .gameCheck(scoreMatrix)
      completion <- ifelse(
        test = .gameState == "continue",
        yes = FALSE,
        no = TRUE)
      interactionType <- ifelse(
        test = gameSet[index,]$format == "numeric",
        yes = "numeric",
        no = "choice")
      
      .generateAnsweredStatement(
        session,
        object = activeBtn,
        verb = "answered",
        description = gameSet[index,]$question,
        response = input$ans,
        interactionType = interactionType,
        success = success,
        completion = completion
      )
      
      if (.gameState == "win") {
        .generateStatement(
          session,
          object = "qqq",
          verb = "completed",
          description = "Player has won the game.")
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "You Win!",
          text = "You've filled either a row, a column, or a main
                     diagonal. Start over and play a new game.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      }
      else if (.gameState == "lose") {
        .generateStatement(
          session,
          object = "qqq",
          verb = "completed",
          description = "Player has lost the game.")
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "You lose :(",
          text = "Take a moment to review the concepts and then try
                     again.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      }
      else if (.gameState == "draw") {
        .generateStatement(
          session,
          object = "game",
          verb = "completed",
          description = "Game has ended in a draw.")
        confirmSweetAlert(
          session = session,
          inputId = "endGame",
          title = "Draw!",
          text = "Take a moment to review the concepts and then try
                     again.",
          btn_labels = "Start Over"
        )
        output$mark <- renderUI(NULL)
        output$feedback <- renderUI(NULL)
      }
      if (is.null(success) || length(success) == 0) {
        updateButton(
          session = session,
          inputId = "submit",
          disabled = FALSE
        )
      }
      else{
        updateButton(
          session = session,
          inputId = "submit",
          disabled = TRUE
        )
      }
      disabled = TRUE
    })
  
  
  observeEvent(
    eventExpr = input$pages,
    handlerExpr = {
      if (input$pages == "qqq") {
        if (!gameProgress) {
          shinyalert(
            title = "Player Select",
            text = "Select whether you want to play as O or X.",
            showConfirmButton = TRUE,
            confirmButtonText = "Play as X",
            showCancelButton = TRUE,
            cancelButtonText = "Play as O"
          )
          gameProgress <<- TRUE
        }
      }
      .generateStatement(session,
                         verb = "experienced",
                         description = paste0("Navigated to ",
                                              input$pages,
                                              " tab.")
      )
    },
    ignoreInit = TRUE)
  
  observeEvent(
    eventExpr = input$endGame,
    handlerExpr = {
      .generateStatement(session,
                         object = "endGame",
                         verb = "interacted",
                         description = paste("Game has been reset.")
      )
      .gameReset()
    })
  
  observeEvent(
    eventExpr = input$shinyalert,
    handlerExpr = {
      if (input$shinyalert == TRUE) {
        player <<- "X"
        opponent <<- "O"
      }
      if (input$shinyalert == FALSE) {
        player <<- "O"
        opponent <<- "X"
      }
      .generateStatement(session,
                         object = "shinyalert",
                         verb = "interacted",
                         description = paste0("User has selected player: ",
                                              player)
      )
      
      output$player <- renderUI({
        return(paste0("You are playing as ",
                      player,
                      "."))
      })
    })
  
  ## Data Analysis:----------
  
  ##Boxplot 
  
  observeEvent(
    eventExpr = c(input$go, input$submitD), 
    handlerExpr = {
      output$boxplot <- renderPlot({
        if (input$model == "Model 1") {
          nonex <- rnorm(input$n, mean = 1000, sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n, mean = 1000, sd = sqrt(input$x2v))
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- (rnorm(input$n, mean = 3, sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        } else if (input$model == "Model 2") {
          nonex <- rnorm(input$n, mean = 1000, sd = sqrt(input$x1v))
          nonex2 <- rnorm(input$n, mean = 1000, sd = sqrt(input$x2v))
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- rnorm(input$n, mean = 1000, sd = input$yv)
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        } else { 
          nonex <- rnorm(input$n, mean = 1000, sd = input$x1v)**2
          nonex2 <- rnorm(input$n, mean = 1000, sd = input$x2v)**2
          e <- rnorm(input$n, mean = 0, sd = .2**2)
          y <- (rnorm(input$n, mean = 1000, sd = input$yv))**2
          
          logx <- log(nonex)
          logx2 <- log(nonex2)
          logy <- log(y)
          
          sqrtx <- sqrt(nonex)
          sqrtx2 <- sqrt(nonex2)
          sqrty <- sqrt(y)
        }
        
        ### Modeling choices ----
        if (input$x == 'none') {
          modelx = nonex
        } else if (input$x == 'logx') {
          modelx = logx
        } else {
          modelx = sqrtx
        }
        
        if (input$x2 == 'none') {
          modelx2 = nonex2
        } else if (input$x == 'logx2') {
          modelx2 = logx2
        } else {
          modelx2 = sqrtx2
        }
        
        if (input$y == 'none') {
          modely = noney
        } else if (input$x == 'logy') {
          modely = logy
        } else {
          modely = sqrty
        }
        
        par(mfrow = c(2,2))
        plot(x=modelx, y=modely,
             pch = 19, 
             lwd = 2,
             id.n = 0,
             cex.axis = 1.5, 
             cex.lab = 1.5,
             cex.caption = 1.5,
             cex.main = 3,
             xlab = "x",
             ylab = "y",
             main = "Scatterplot x vs y"
        )
        
        plot(x=modelx2, y=modely,
             pch = 19, 
             lwd = 2,
             id.n = 0,
             cex.axis = 1.5, 
             cex.lab = 1.5,
             cex.caption = 1.5,
             cex.main = 3,
             xlab = "x2",
             ylab = "y",
             main = "Scatterplot x2 vs y"
        )
        
        
        boxplot(modely,
                pch = 19, 
                lwd = 2,
                id.n = 0,
                cex.axis = 1.5, 
                cex.lab = 1.5,
                cex.caption = 1.5,
                cex.main = 3,
                xlab ="y", 
                main = "Distribution outcome variable"
        )
        
        exes.dat <- data.frame(modelx, modelx2)
        colnames(exes.dat) <- c("x", "x2")
        
        boxplot(exes.dat,
                pch = 19, 
                lwd = 2,
                id.n = 0,
                cex.axis = 1.5, 
                cex.lab = 1.5,
                cex.caption = 1.5,
                cex.main = 3,
                main = "Distribution input variables"
        )
        
      })
    })
  
  
  
  #boastUtils::typesetMath(session = session)
}

# Boast App call ----
#boastUtils::boastApp(ui = ui, server = server)
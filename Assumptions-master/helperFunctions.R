


# Helper Functions
.tileCoordinates <- function(tile = NULL,
                             index = NULL) {
  row <- -1
  col <- -1
  # if: button tile is given, derive from id
  # else: derive from index
  if (!is.null(tile)) {
    # grid-[row]-[col]
    tile <- strsplit(x = tile,
                     split = "-")[[1]]
    tile <- tile[-1] # remove oxo
    row <- strtoi(
      x = tile[1])
    col <- strtoi(
      x = tile[2])
  }
  else {
    row <- (index - 1) %/% GRID_SIZE + 1
    col <- index - (GRID_SIZE * (row - 1))
  }
  coordinates <- list("row" = row,
                      "col" = col)
  return(coordinates)
}

.tileIndex <- function(tile) {
  coords <- .tileCoordinates(tile)
  index = GRID_SIZE * (coords$row - 1) + coords$col
  return(index)
}

.btnReset <- function(index) {
  coords <- .tileCoordinates(index = index)
  id <- paste0(
    "grid-",
    coords$row,
    sep = "-",
    coords$col)
  updateButton(
    session = session,
    inputId = id,
    label = "?",
    disabled = FALSE
  )
}

.score <- function(score, tile, value) {
  i <- .tileCoordinates(tile)
  score[i$row, i$col] <- value
  return(score)
}

.gameCheck <- function(mat) {
  rows <- rowSums(mat)
  cols <- colSums(mat)
  
  if (GRID_SIZE > 1) {
    mainD <- sum(diag(mat))
    rotated <- apply(X = t(mat),
                     MARGIN = 2,
                     FUN = rev)
    offD <- sum(diag(rotated))
    if (GRID_SIZE %in% rows ||
        GRID_SIZE %in% cols ||
        mainD == GRID_SIZE || offD == GRID_SIZE) {
      return("win")
    }
    else if (-GRID_SIZE %in% rows ||
             -GRID_SIZE %in% cols == 1 ||
             mainD == -GRID_SIZE || offD == -GRID_SIZE) {
      return("lose")
    }
    else if (any(mat == 0)) {
      return("continue")
    }
    else {
      return("draw")
    }
  }
  else {
    ifelse(
      test = rows == 1 && rows != 0,
      yes = return("win"),
      no = return("lose")
    )
  }
}

.boardBtn <- function(tile) {
  index <- .tileIndex(tile)
  activeQuestion <<- gameSet[index, "id"]
  output$question <- renderUI({
    withMathJax()
    return(gameSet[index, "question"])
  }
  )
  output$answer <- .ansFunc(index, gameSet)
  
  if (gameSet[index, "extraOutput"] != "") {
    output$extraOutput <- renderText({
      gameSet[index, "extraOutput"]
    })
  }
  else {
    output$extraOutput <- NULL
  }
  
  #Retrigger MathJax processing
  output$trigger1 <- renderUI({
    withMathJax()
  })
  output$trigger2 <- renderUI({
    withMathJax()
  })
  
  #Enable Submit Button
  updateButton(
    session = session,
    inputId = "submit",
    disabled = FALSE)
}

.ansFunc <- function(index, df) {
  if (df[index, "format"] == "numeric") {
    renderUI({
      numericInput(
        inputId = "ans",
        label = df[index, "label"],
        value = 0)
    })
  }
  else if (df[index, "format"] == "two") {
    renderUI({
      radioGroupButtons(
        inputId = "ans",
        choices = list(df[index, "A"],
                       df[index, "B"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = fontawesome::fa(name = "far fa-square")
        ),
        status = "textGame",
        direction = "horizontal",
        individual = TRUE
      )
    })
  }
  else if (df[index, "format"] == "three") {
    renderUI({
      radioGroupButtons(
        inputId = "ans",
        choices = list(df[index, "A"],
                       df[index, "B"],
                       df[index, "C"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = fontawesome::fa(name = "far fa-square")
        ),
        status = "textGame",
        direction = "vertical"
      )
    })
  }
  else {
    renderUI({
      radioGroupButtons(
        inputId = "ans",
        choices = list(df[index, "A"],
                       df[index, "B"],
                       df[index, "C"],
                       df[index, "D"]
        ),
        selected = character(0),
        checkIcon = list(
          yes = icon("check-square"),
          no = fontawesome::fa(name = "far fa-square")
        ),
        status = "textGame",
        direction = "vertical"
      )
    })
  }
}

.gameReset <- function() {
  lapply(
    X = 1:TILE_COUNT,
    FUN = .btnReset)
  qSelected <<- sample(
    seq_len(nrow(questionBank)),
    size = TILE_COUNT,
    replace = FALSE)
  gameSet <<- questionBank[qSelected,]
  
  output$question <- renderUI({
    return("Click a button on the game board to get started on your new game.")
  })
  output$answer <- renderUI({""})
  output$extraOutput <- renderUI({""})
  scoreMatrix <<- matrix(
    data = rep.int(0,times = TILE_COUNT),
    nrow = GRID_SIZE,
    ncol = GRID_SIZE
  )
  gameProgress <- FALSE
  activeBtn <- NA
  
  updateButton(
    session = session,
    inputId = "submit",
    disabled = TRUE)
}

## BEGIN App Specific xAPI Wrappers ----
.generateStatement <- function(session,
                               verb = NA,
                               object = NA,
                               description = NA) {
  if(is.na(object)){
    object <- paste0("#shiny-tab-",
                     session$input$pages)
  }
  
  stmt <- boastUtils::generateStatement(
    session,
    verb = verb,
    object = object,
    description = description
  )
  
  response <- boastUtils::storeStatement(
    session = session,
    statement = stmt)
  return(response)
}

.generateAnsweredStatement <- function(
    session,
    verb = NA,
    object = NA,
    description = NA,
    interactionType = NA,
    response = NA,
    success = NA,
    completion = FALSE) {
  stmt <- boastUtils::generateStatement(
    session,
    verb = verb,
    object = object,
    description = paste0(
      "Question ",
      activeQuestion,
      sep = ": ",
      description),
    interactionType = interactionType,
    success = success,
    response = response,
    completion = completion,
    extensions = list(
      ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/scoreMatrix",
      value = paste(as.data.frame(scoreMatrix),
                    collapse = ", ")
    )
  )
  
  response <- boastUtils::storeStatement(
    session = session,
    statement = stmt)
  return(response)
}
## END App Specific xAPI Wrappers ----
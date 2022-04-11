# Load packages ----
library(shiny)
library(boastUtils)
library(shinyalert)
library(fontawesome)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(shinyWidgets)#  for alerts
library(shinyjs) # for running javascript on the server side
library(shinycssloaders)
library(gridExtra) # for combining multiple ggplots into one object 
library(shinyBS) # for popovers
library(ggplot2)


# Future work
# source("ticTacToe.R")

# Define global constants and load question banks ----
GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2

questionBank <- read.csv("questionBank.csv", header = TRUE)
bankc <- read.csv("ChallengeOutput.csv", header = TRUE)


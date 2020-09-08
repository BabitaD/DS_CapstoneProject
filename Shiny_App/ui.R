# GitHub Ref: https://github.com/oraclejavanet/coursera-data-science-capstone

library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(tm)

shinyUI(
  navbarPage("Word Prediction App",
             theme = shinytheme("spacelab"),
             tabPanel("Word Predict",
                      fluidPage(
                        titlePanel("Word Prediction"),
                        sidebarLayout(
                          sidebarPanel(
                            textInput("userInput",
                                      "Enter a phrase:",
                                      value =  "",
                                      placeholder = "Enter text here"),
                            br()
                            ),
                          mainPanel(
                            h4("Entered Phrase:"),
                            verbatimTextOutput("userSentence"),
                            br(),
                            h4("Word Predictions: "),
                            verbatimTextOutput("prediction1"),
                            verbatimTextOutput("prediction2"),
                            verbatimTextOutput("prediction3")
                          )
                        )
                      )
             )
  )
)

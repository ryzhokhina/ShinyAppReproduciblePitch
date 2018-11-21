#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Distribution of Sample Means"),
  h5("This example illustrates how the distribution of sample means becomes Gaussian with increasing sample size."),
  br(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       radioButtons("dist_type", "Distribution Type:",
                    c("Normal" = "norm",
                      "Exponential" = "exp",
                      "Uniform" = "unif")
                    ),
       br(),
       sliderInput("sample_size",
                   "Sample size:",
                   min = 10,
                   max = 250,
                   value = 100),
       sliderInput("num_rep",
                   "Number of repetitions:",
                   min = 10,
                   max = 250,
                   value = 100),
       br(), br(),
       actionButton("resample", label = "Draw New Sample"),
       br(), br(),br(), br(),br(), br()

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

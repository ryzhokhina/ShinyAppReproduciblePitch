#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  generate_data<-function(type, min, max, n)
  {
    q_func<- switch (type,
                     norm = qnorm,
                     exp = qexp,
                     unif  = qunif,
                     qnorm
    )
    p_func<- switch (type,
                     norm = pnorm,
                     exp = pexp,
                     unif  = punif,
                     pnorm
    )

    q_func(runif(n, p_func(min), p_func(max)))
  }

  generate_means<-function(type, min, max, n, n_rep)
  {
    means = 1:n_rep
    for (i in 1:n_rep) {
      means[i] = mean(generate_data(type, min, max, n))
    }
    means
  }


  get_data <- reactive(
  {
    d_type = input$dist_type
    x_min = switch(d_type, norm = -3, unif = 0,  exp = 0, -3)
    x_max = switch(d_type, norm =  3, unif = 1,  exp = 4,  3)

    input$resample
    generate_data(d_type, x_min, x_max, input$sample_size)
  })


  output$distPlot <- renderPlot({
    # Data
    x = get_data()
    x = sort(x)
    d_type = input$dist_type
    n = input$sample_size

    # Plot Setings
    par(mfrow=c(3,1), mar=c(8,5,2,2))
    c_col = "blue"
    p_cex =2

    #  Density
    p_x = seq(min(x), max(x), length.out = 500)
    d_x = y0 = switch(d_type,
                      norm = dnorm(p_x),
                      unif = dunif(p_x),
                      exp = dexp(p_x),
                      dnorm(p_x))

    plot(
      p_x, d_x, type = "l", frame = F,
      main  = "Population", ylab="Probability", xlab = "",
      cex.axis = p_cex,
      cex.lab = p_cex,
      cex.main = p_cex,
      cex.sub =p_cex)
    polygon(c(min(p_x), p_x, max(p_x)), c (0, d_x, 0), col = c_col)

    # Sample Histogram - draw the histogram with the specified number of bins
    bins <- seq(min(x), max(x), length.out = 50)

    hist(x, breaks = bins,
         col = c_col,
         main = "Sample",
         xlab = "",
         cex.axis = p_cex,
         cex.lab = p_cex,
         cex.main = p_cex,
         cex.sub =p_cex
         )

    # Plot of distribution of Sample Means
    means= generate_means(d_type, min(x), max(x), n, input$num_rep)
    means<- sort(means)
    y_m = dnorm(p_x,switch(d_type,
                         norm = 0,
                         unif = 0.5,
                         exp = 1,
                         0),
               switch(d_type,
                      norm = 1/sqrt(n),
                      unif = 1/sqrt(12)/sqrt(n),
                      exp = 1/sqrt(n),
                      0))
    breaks_mh = seq(min(x), max(x), length.out = 150)
    y_m=y_m/sum(y_m)*length(means)*mean(diff(breaks_mh))/mean(diff(p_x))
    h= hist(means, breaks = breaks_mh,
         col = c_col,
         main = "Sample Means",
         xlab = "", cex.axis = p_cex,
         cex.lab = p_cex,
         cex.main = p_cex,
         cex.sub =p_cex)
    points(p_x,y_m, type="l",lwd=2)

  }, width=700,height=600)

})

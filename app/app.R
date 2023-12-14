library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage(
    title = "Runge-Kutta Method",
    theme = shinytheme("flatly"),
    tabPanel("Method",
             fluidRow(
               column(12,
                      h2("Runge-Kutta Fourth-Order Method"),
                      p("A method to solve ordinary differential equation (also called ODE) that uses a better approximation than the Euler method."),
               ),
               column(12,
                      h3("Example"),
                      p("To solve the initial value problem an ordinary differential equation, dy/dx=-2x-y with y(0)=-1 for y at x=0.1 and x=0.2 with step length 0.1 using the Runge-Kutta method."),
                      
                      p("1. Calculate the slopes k1, k2, k3 and k4."),
                      p("2. Use these slopes to estimate the value of y at the next point."),
                      p("Given: f(x,y)= -2x-y, x0=0, y0=-1, h=0.1"),
                      
                      p("k1 = h*f(x0, y0)"),
                      p("k2 = h*f(x0 + (1/2)h, y0 +(1/2)k1)"),
                      p("k3 = h*f(x0 + (1/2)h, y0 +(1/2)k2)"),
                      p("k4 = h*f(x0 + h, y0 + k3)"),
                      
                      p("k1 = 0.1, k2 = 0.085, k3 = 0.08575, k4 = 0.071425"),
                      
                      p("By the Runge-Kutta method of the fourth order, we have the next value of y:"),
                      p("y1 = y0+ (1/6)*(k1+2*k2+2*k3+k4)"),
                      
                      p("y1 = -0.9145125"),
                      
                      p("For x = 0.2, y2 = -0.8561927"),
               )
             )
    ), 
    tabPanel("Calculator",
             fluidRow(
               column(6,
                      textInput("f_str", "Differential Equation dy/dx = ", value = "-2*x - y"),
                      numericInput("x0", "Initial x", value = 0),
                      numericInput("y0", "Initial y", value = -1),
                      numericInput("h", "Step Length", value = 0.1),
                      numericInput("x_end", "Calculate y at x", value = 0.2),
                      actionButton("calculate", "Calculate")
               ),
               column(6,
                      tableOutput("results"),
                      plotOutput("plot1", height = 350)
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Function to safely evaluate the differential equation
  safeEval <- function(str) {
    eval(parse(text = str), envir = parent.frame())
  }
  
  # Convert the differential equation string to a function
  f <- reactive({
    function(x, y) {
      safeEval(input$f_str)
    }
  })
  
  # Runge-Kutta calculation
  runge_kutta <- function(f, x0, y0, x_end, h) {
    x_vals = seq(x0, x_end, by = h)
    y_vals = numeric(length(x_vals))
    y_vals[1] = y0
    
    for (i in 1:(length(x_vals) - 1)) {
      k1 = h * f(x_vals[i], y_vals[i])
      k2 = h * f(x_vals[i] + 0.5 * h, y_vals[i] + 0.5 * k1)
      k3 = h * f(x_vals[i] + 0.5 * h, y_vals[i] + 0.5 * k2)
      k4 = h * f(x_vals[i] + h, y_vals[i] + k3)
      y_vals[i + 1] = y_vals[i] + (1/6) * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    return(data.frame(x = x_vals, y = y_vals))
  }
  
  # Reactive expression for Runge-Kutta results
  results <- reactive({
    input$calculate
    isolate({
      runge_kutta(f(), input$x0, input$y0, input$x_end, input$h)
    })
  })
  
  # Render the table
  output$results <- renderTable({
    req(results())
    results()
  })
  
  # Render the plot
  output$plot1 <- renderPlot({
    req(results())
    ggplot(results(), aes(x = x, y = y)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = 'Runge-Kutta Method', x = 'x', y = 'y')
  })
}

# Run the application
shinyApp(ui = ui, server = server)

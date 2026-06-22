# Code By Yann Cohen. Assistance with Algorithm - Yoav Kessler. Improved execution with AI
library(shiny)
library(glue)
library(matrixStats)

sequential_sim <- \(n_simulations,vector_size){
  store_iterations <- integer(n_simulations)
  
  for(i in 1:n_simulations){
    
    iteration <- 0
    v <- 1:vector_size
    
    while(length(unique(v)) > 1){
      iteration <- iteration + 1
      v <- sample(v,vector_size, replace = TRUE)
    }
    store_iterations[i] <- iteration
    
  }
  return(store_iterations)
}

vectorized_sim <- function(n_simulations, vector_size) {
  N <- vector_size
  if (N <= 1) return(integer(n_simulations))
  
  V <- matrix(1:N, nrow = n_simulations, ncol = N, byrow = TRUE)
  orig_idx <- seq_len(n_simulations)
  store_iterations <- integer(n_simulations)
  iter <- 0L
  
  n_active <- n_simulations
  row_id <- rep(seq_len(n_active), times = N)   # built once, reused while n_active is unchanged
  
  while (n_active > 0) {
    iter <- iter + 1L
    
    col_id  <- sample.int(N, n_active * N, replace = TRUE)
    lin_idx <- row_id + (col_id - 1L) * n_active   # column-major linear index, no cbind()
    V <- V[lin_idx]
    dim(V) <- c(n_active, N)                       # in-place reshape, no matrix() copy
    
    converged <- rowMaxs(V) == rowMins(V)           # no n_active x N comparison matrix
    
    if (any(converged)) {
      keep <- !converged
      store_iterations[orig_idx[converged]] <- iter
      V <- V[keep, , drop = FALSE]
      orig_idx <- orig_idx[keep]
      n_active <- nrow(V)
      if (n_active > 0) row_id <- rep(seq_len(n_active), times = N)  # rebuilt only on shrink
    }
  }
  
  store_iterations
}

run_simulations <- \(n_simulations, vector_size){
  
  if(n_simulations > 1e3 & vector_size >= 500){
    # execute sequential version
    
    results <- sequential_sim(n_simulations, vector_size)
  } else{
    # execute vectorized version
    
    results <- vectorized_sim(n_simulations, vector_size)
    
  }
  
  
  return(results)
  
}





# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Application title
  h1("Iterations to Convergence Simulation", align = "center"),
  br(),
  withMathJax(),
  helpText(
    "This simulator generates the distribution of iterations",
    "until reaching convergence on a single value for a given vector size.",
    br(), br(),
    "$$V_0 = \\{v_1, v_2, \\ldots, v_N\\} \\quad (N \\text{ unique values})$$",
    "$$V_k = \\text{sample}(V_{k-1}, N, \\text{replace} = TRUE)$$",
    "$$\\text{Stop at step } K \\text{ when } \\text{unique}(V_K) = 1$$"
  ),
  br(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("n_simulations",
                   "Number of simulations",
                   min = 100,
                   max = 1e4, step = 100,
                   value = 1e3,
                   updateOn = 'blur'
      ),
      numericInput("v_size",
                   "Vector size \U1D441:",
                   min = 3,
                   max = 1e4, step = 10,
                   value = 10,
                   updateOn = 'blur'
      ),
      
      verbatimTextOutput("info"),
      verbatimTextOutput("stats"),
      actionButton("restart", "", icon = icon("refresh"), class = "btn-sm btn-secondary")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distribution_of_iterations")
    )
  ),
  h5("Developed by", a("Yann Cohen", href = "https://iamyannc.github.io/Yann-dev/", target = "_blank"), "in collaboration with", a("Prof. Yoav Kessler", href = "https://scholar.google.com/citations?user=aygeuW8AAAAJ&hl=en", target = "_blank"))
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  raw_dist <- reactive({
    input$restart
    run_simulations(input$n_simulations, input$v_size)
  })

  output$distribution_of_iterations <- renderPlot({
    hist(raw_dist(),
      breaks = 100,
      main = paste("Number of iterations untill convergence"),
      sub = paste("Number of simulations:", scales::comma(input$n_simulations), sep = " "),
      xlab = paste("Mean = ", round(mean(raw_dist()), 3), sep = "")
    )
  })


  output$info <- renderPrint({
    glue(
      "Number of simulations: {scales::comma(input$n_simulations)}\nVector size: {input$v_size}\nX̄/N ratio: {round(mean(raw_dist())/ input$v_size,2)}
      "
    )
  })

  output$stats <- renderPrint({
    glue(
      "SD of the iterations: {round(sd(raw_dist()), 3)}
    Mean iterations: {round(mean(raw_dist()), 3)}
    MD iterations: {round(median(raw_dist()), 3)}
    MO iterations: {as.numeric(names(sort(table(raw_dist()), T)[1]))} ({sort(table(raw_dist()), T)[1]} times)
    Max/Min iterations: {max(raw_dist())}/{min(raw_dist())}
    "
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

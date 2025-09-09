server <- function(input, output, session){
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = readr::read_csv(input$upload$datapath),
           tsv = readr::read_tsv(input$upload$datapath),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  observeEvent(data(), {
    df_cols <- names(data())
    
    updateSelectInput(session, "yi", choices = c("", df_cols))
    updateSelectInput(session, "vi", choices = c("", df_cols))
    updateSelectInput(session, "sei", choices = c("", df_cols))
    updateSelectInput(session, "x1", choices = c("", df_cols))
    updateSelectInput(session, "NoPB", choices = c("", df_cols))  # Allow blank option
  })
  
  PBinfo <- reactive({
    cat("PBinfo triggered\n")
    req(data())
    cat("Data loaded\n")
    df <- data()
    
    req(input$yi, input$x1)
    cat("yi:", input$yi, " | x1:", input$x1, "\n")
    
    df$yi <- as.numeric(df[[input$yi]])
    df$x1 <- as.numeric(df[[input$x1]]) 
    
    if (input$vtype == "vi"){
      req(input$vi)
      validate(
        need(is.numeric(df[[input$vi]]), "Please select a column with numeric values for the sampling variance.")
      )
      df$vi <- as.numeric(df[[input$vi]])
    } else if (input$vtype == "sei"){
      req(input$sei)
      validate(
        need(is.numeric(df[[input$sei]]), "Please select a column with numeric values for the standard error.")
      )
      df$vi <- as.numeric(df[[input$sei]])^2
    }else {
      stop("Unknown sampling variance/SE type. Please choose 'vi' or 'sei'.")
    }
    
    if(is.null(input$NoPB) || input$NoPB == ""){
      df$NoPB = rep(FALSE, nrow(df))
    }else{
      validate(
        #need(input$NoPB %in% names(df), "You inputted a label for column to indicate 
        #   which studies are affected by publication bias. This columns does not exist in your data."),
        need(is.logical(df[[input$NoPB]]) == FALSE, "The column to indicate which studies are affected 
           by publication bias must only contain logical values (i.e., TRUE or FALSE).")
      )
      df$NoPB <- df[[input$NoPB]]
    }
    
    check_cols <- c("yi", "x1", "vi", "NoPB")
    # Validate that df$vi is numeric and not null
    validate(
      need(!is.null(df$vi), "Computed sampling variance (vi) is NULL."),
      need(is.numeric(df$vi), "Computed sampling variance (vi) is not numeric.")
    )
    
    # Track how many rows before and after exclusion (cases with NA)
    n_before <- nrow(df)
    df <- df[complete.cases(df[, check_cols]), ]
    n_after <- nrow(df)
    n_removed <- n_before - n_after
    
    # Notify user if rows were removed
    if (n_removed > 0) {
      showNotification(
        paste(n_removed, "case(s) with missing values in", paste(check_cols, collapse = ", "), "were excluded."),
        type = "warning"
      )
    }
    
    # Final check
    validate(
      need(nrow(df) > 1, "Not enough complete cases to proceed with analysis.")
    )
    
    #random and mixed-effects model
    rem <- rma(yi=yi, vi = vi, data = df)
    x1 <- as.formula(paste0("~", as.character(input$x1)))
    mem <- rma(yi=yi, vi = vi, data = df, mods= x1)
    
    #default values
    beta1 = 0
    I2res = c(0, 0.25, 0.5, 0.75)
    PB = c(0, 0.05, 0.2, 0.5, 1)
    if(rem$beta > 0){Zcv <- qnorm(0.025, lower.tail=F) 
    } else{ Zcv <- qnorm(0.025, lower.tail=T) }
    
    
    if (input$default == "yes"){
      validate(
        need(!is.null(input$beta0.spec), "Please specify a numerical value for beta0."),
        need(is.numeric(input$beta0.spec), "The specified value for beta0 is not numeric."),
        need(!is.null(input$beta1.spec), "Please specify a numerical value for beta1."),
        need(is.numeric(input$beta1.spec), "The specified value for beta1 is not numeric."),
        need(!is.null(input$I2.spec), "Please specify a numerical value for I2res between 0 and 1."),
        need(is.numeric(input$I2.spec), "The specified value for I2res is not numeric."),
        need(input$I2.spec >= 0 & input$I2.spec <100, "The specified value for I2res is outside the possible range: [0, 100)."),
        need(!is.null(input$PB.spec), "Please specify a numerical value for PB between 0 and 1."),
        need(is.numeric(input$PB.spec), "The specified value for PBis not numeric."),
        need(input$PB.spec >= 0 & input$PB.spec <=1, "The specified value for PBis outside the possible range: [0, 1].")
      )
      beta0.spec = input$beta0.spec
      beta1.spec = input$beta1.spec
      I2res.spec = input$I2.spec/100
      PB.spec = input$PB.spec
      PBbetas = PB_betas(dat = df, beta0 = beta0.spec,
                         beta1=beta1.spec, PB.spec, 
                         I2=I2res.spec, mods= df$x1, Zcv = Zcv )
      tab_betas <- matrix(c(round(rem$beta, 4), "-", round(mem$beta[1], 4), round(mem$beta[2], 4),
                            round(PBbetas[1,1], 4), round(PBbetas[2,1], 4)),  byrow = T, nrow=3,
                          dimnames= list(  c("REM", "MEM", "PB Sensitivity"),
                                           c("beta0","beta1")
                          ))
      return(list("data" = df, "rem" = rem, "mem" = mem, "beta1" = beta1, 
                  "I2res" = I2res, "PB" = PB,  "Zcv" = Zcv,  "beta0.spec" =  beta0.spec,
                  "beta1.spec" = beta1.spec, "I2res.spec" = I2res.spec, 
                  "PB.spec" = PB.spec, "PBbetas" = PBbetas, "tab_betas" = tab_betas 
      ))
    } 
    
    return(list("data" = df, "rem" = rem, "mem" = mem, "beta1" = beta1, "I2res" = I2res,
                "PB" = PB,  "Zcv" = Zcv
    ))
  })
  
  output$BiasPlot1 <- renderPlot(    
    width =  function() input$width,
    height = function() input$height,
    res = 96,
    {
      req(PBinfo())
      p <- PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                            mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =0, 
                            mod.title = paste0("Moderator: ", input$x1) ,
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB)
      return(p)
    }
  )
  
  output$download_Fig1 <- downloadHandler(
    filename = "Bias_Plot1.png",# paste0(tools::file_path_sans_ext(Bias_Plot), ".png"),#"Bias_Plot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggsave("Bias_Plot1.png", plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =0, 
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB),
             device = device)
    }
  )
  
  
  output$BiasPlot2 <- renderPlot(    
    width =  function() input$width,
    height = function() input$height,
    res = 96,
    {
      
      p <- PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                            mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta)/2, 
                            mod.title = paste0("Moderator: ", input$x1) ,
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB)
      return(p)
    }
  )
  
  output$download_Fig2<- downloadHandler(
    filename = "Bias_Plot2.png",# paste0(tools::file_path_sans_ext(Bias_Plot), ".png"),#"Bias_Plot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggsave("Bias_Plot2.png", plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta)/2, 
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB), 
             device = device)
    }
  )
  
  
  output$BiasPlot3 <- renderPlot(    
    width =  function() input$width,
    height = function() input$height,
    res = 96,
    {
      req(PBinfo())
      p <- PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                            mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta), 
                            mod.title = paste0("Moderator: ", input$x1) ,
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB)
      return(p)
    }
  )
  
  output$download_Fig3 <- downloadHandler(
    filename = "Bias_Plot3.png",# paste0(tools::file_path_sans_ext(Bias_Plot), ".png"),#"Bias_Plot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggsave("Bias_Plot3.png", plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1, 
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta), 
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB),
             device = device)
    }
  )
  
  
  output$AddBiasPlot <- renderPlot(    
    width =  function() input$width,
    height = function() input$height,
    res = 96,
    {
      req(input$default == "yes")
      req(PBinfo())
      p <- Plot_additional_analysis(data = PBinfo()$data, beta0 = PBinfo()$beta0.spec, 
                                    beta1=PBinfo()$beta1.spec, I2 = PBinfo()$I2res.spec, 
                                    PB = PBinfo()$PB.spec, mem = PBinfo()$mem,
                                    betasPB = PBinfo()$PBbetas)
      
      return(p)
    }
  )
  
  output$download_AddBiasPlot <- downloadHandler(
    filename = "AddBiasPlot.png",# paste0(tools::file_path_sans_ext(Bias_Plot), ".png"),#"Bias_Plot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggsave("AddBiasPlot.png", plot = Plot_additional_analysis(data = PBinfo()$data, beta0 = PBinfo()$beta0.spec, 
                                                                beta1=PBinfo()$beta1.spec, I2 = PBinfo()$I2res.spec, 
                                                                PB = PBinfo()$PB.spec, mem = PBinfo()$mem,
                                                                betasPB = PBinfo()$PBbetas),
             device = device)
    }
  )
  
  output$BiasText <- renderPrint({
    req(input$default == "yes")
    validate(need(is.matrix(PBinfo()$PBbetas), "PBbetas is not a matrix"))
    print(paste0("The biased intercept is ", round(PBinfo()$PBbetas[1,1], 4), 
                 " and the biased moderator effect is ",round(PBinfo()$PBbetas[2,1],4), "."))
  })
  
  output$table <- renderTable(PBinfo()$tab_betas , striped = TRUE,rownames = TRUE, colnames = TRUE,) 
  
  output$rem <- renderPrint({
    
    req(PBinfo()$rem)
    tryCatch({
      print(PBinfo()$rem)
    }, error = function(e) {
      cat("Error in summary():", e$message)
    })
  })
  
  output$mem <- renderPrint({
    
    req(PBinfo()$mem)
    tryCatch({
      print(PBinfo()$mem)
    }, error = function(e) {
      cat("Error in summary():", e$message)
    })
  })
  
  output$RegPlot<- renderPlot(    
    width =  function() input$width,
    height = function() input$height,
    res = 96,
    {
      p4 <- regplot(PBinfo()$mem)
      return(p4)
    }
  )
  
  
  
  output$head <- renderTable({
    req(data())
    head(data(), input$n)
  })
}

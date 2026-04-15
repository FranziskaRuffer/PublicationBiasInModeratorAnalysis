library(PublicationBiasInModeratorAnalysis)
server <- function(input, output, session){

  # check whether no data was uploaded -> default data: Lehmann et al. (2018)
  isDefaultData <- reactive({
    is.null(input$upload) || is.null(input$upload$datapath)
  })

  data <- reactive({

    if (is.null(input$upload)) {

      dat <- metadat::dat.lehmann2018

      dat <- dat[, c("yi", "vi", "Preregistered", "Gender", "PRPublication")]
      dat <- subset(dat, Gender == "Females" & !is.na(Preregistered))

      dat$Preregistered <- ifelse(dat$Preregistered == "Pre-Registered", 1, 0)
      dat$NoPB <- ifelse(dat$Preregistered == 1 | dat$PRPublication == "No", TRUE, FALSE)

    } else {
      # whenever data is uploaded, check which file type and separators are used
      # and read in data correctly

      first_line <- readLines(input$upload$datapath, n = 1)

      sep <- if (grepl(";", first_line)) {
        ";"
      } else if (grepl("\t", first_line)) {
        "\t"
      } else {
        ","
      }

      dat <- tryCatch({

        if (sep == ";") {
          readr::read_delim(
            file = input$upload$datapath,
            delim = ";",
            locale = readr::locale(decimal_mark = ","),
            show_col_types = FALSE
          )
        } else {
          readr::read_delim(
            file = input$upload$datapath,
            delim = sep,
            locale = readr::locale(decimal_mark = "."),
            show_col_types = FALSE
          )
        }
      }, error = function(e) {
        showNotification(paste("File read error:", e$message), type = "error")
        return(data.frame())
      })

      if (ncol(dat) <= 1) {
        dat <- readr::read_delim(
          input$upload$datapath,
          delim = sep,
          show_col_types = FALSE
        )
      }
    }
    dat
  })

  observeEvent(data(), {

    df <- data()
    df_cols <- names(df)
    is_default <- isDefaultData()

    if (is_default) {
      # for the default analysis (i.e., when launching the app), select the variable as described in the analysis in
      # the corresponding paper
      updateSelectInput(session, "yi",
                        choices = c("", df_cols),
                        selected = "yi")

      updateSelectInput(session, "vi",
                        choices = c("", df_cols),
                        selected = "vi")

      updateSelectInput(session, "sei",
                        choices = c("", df_cols),
                        selected = "")

      updateSelectInput(session, "x1",
                        choices = c("", df_cols),
                        selected = "Preregistered")

      updateSelectizeInput(session, "NoPB",
                           choices = c("None" = "", df_cols),
                           selected = if ("NoPB" %in% df_cols) "NoPB" else "",
                           server = TRUE)

    } else {
      # for any uploaded data, select columns in the data as variable (NoPB does not
      # need to be selected)
      updateSelectInput(session, "yi",
                        choices = c("", df_cols),
                        selected = if (is_default) df_cols[1] else "")

      updateSelectInput(session, "vi",
                        choices = c("", df_cols),
                        selected = if (is_default) df_cols[2] else "")

      updateSelectInput(session, "sei",
                        choices = c("", df_cols),
                        selected = "")

      updateSelectInput(session, "x1",
                        choices = c("", df_cols),
                        selected = if (is_default && "Preregistered" %in% df_cols) "Preregistered" else "")

      updateSelectizeInput(session, "NoPB",
                           choices = c("None" = "", df_cols),
                           selected = if (is_default && "NoPB" %in% df_cols) "NoPB" else "",
                           server = TRUE)
    }
  })

  # check whether all required variables in a new dataset are selected & available
  analysis_ready <- reactive({
    df <- data()

    if (is.null(df) || nrow(df) == 0) return(FALSE)

    if (!nzchar(input$yi)) return(FALSE)
    if (!nzchar(input$x1)) return(FALSE)

    if (!(input$yi %in% names(df))) return(FALSE)
    if (!(input$x1 %in% names(df))) return(FALSE)

    if (input$vtype == "vi") {
      if (!nzchar(input$vi)) return(FALSE)
      if (!(input$vi %in% names(df))) return(FALSE)
    }

    if (input$vtype == "sei") {
      if (!nzchar(input$sei)) return(FALSE)
      if (!(input$sei %in% names(df))) return(FALSE)
    }

    TRUE
  })


  PBinfo <- reactive({

    # only start calculations after data and variables are selected
    req(analysis_ready())

    df <- data()
    # validating that the moderator and effect sizes variable are actually in the dataset
    validate(
      need(input$yi %in% names(df), "Invalid yi selection"),
      need(input$x1 %in% names(df), "Invalid x1 selection")
    )

    # assuring that the effect size variable is numeric
    df$yi <- suppressWarnings(as.numeric(df[[input$yi]]))
    validate(
      need(!all(is.na(df$yi)), "Selected effect size column is not numeric.")
    )

    # checking if the moderator variable is numeric, otherwise converting it to numeric
    if (is.numeric(df[[input$x1]])) {
      df$x1 <- df[[input$x1]]
    } else {
      df$x1 <- as.numeric(as.factor(df[[input$x1]]))
    }
    # checking that the sampling variances or SEs are numeric (can be converted to numeric)
    if (input$vtype == "vi"){
      df$vi <- suppressWarnings(as.numeric(df[[input$vi]]))
      validate(
        need(!all(is.na(df$vi)), "Selected sampling variance column is not numeric.")
      )
      validate(
        need(is.numeric(df[[input$vi]]), "Please select a column with numeric values for the sampling variance.")
      )
    } else if (input$vtype == "sei"){
      df$sei <- suppressWarnings(as.numeric(df[[input$sei]]))
      validate(
        need(!all(is.na(df$sei)), "Selected standard error column is not numeric.")
      )

     df$vi <- as.numeric(df[[input$sei]])^2
    }else {
      stop("Unknown sampling variance/SE type. Please choose 'vi' or 'sei'.")
    }

    if (is.null(input$NoPB) || input$NoPB == "" || !(input$NoPB %in% names(df))) {
      # No column selected → assume all studies potentially biased
      df$NoPB <- rep(FALSE, nrow(df))
    } else {
      col <- df[[input$NoPB]]
      # Convert common formats to logical
      if (is.logical(col)) {
        df$NoPB <- col
      } else if (is.numeric(col)) {
        df$NoPB <- col == 1
        showNotification(
          paste("The values of the publication bias indicator variable were converted such that a (numeric) value of
              1 indicates no publication bias (i.e. No Publication Bias = 'TRUE'), while
              any other value indicates publication bias (i.e. No Publication Bias = 'FALSE')."),
          type = "warning"
        )
      } else if (is.character(col)) {
        df$NoPB <- tolower(col) %in% c("true", "yes", "1")
        showNotification(
          paste("The values of the publication bias indicator variable were converted such that the
                following (character) values - 'true', 'yes' or '1' - indicate no publication bias (i.e. No Publication Bias = 'TRUE'), while
                any other value indicates publication bias (i.e. No Publication Bias = 'FALSE')."),
          type = "warning"
        )
      } else {
        validate(
          need(FALSE, "NoPB column must be logical, numeric (0/1), or yes/no.")
        )
      }
    }

    check_cols <- c("yi", "x1", "vi", "NoPB")
    # Validate that df$yi exists and df$vi is numeric and not null
    validate(
      need(input$yi %in% names(df), "Selected effect size column does not exist.")
    )
    validate(
      need(is.numeric(df$vi), "Computed sampling variance (vi) is not numeric.")
    )

    # Track how many rows before and after exclusion (cases with NA)
    n_before <- nrow(df)
    df <- df[stats::complete.cases(df[, check_cols]), ]
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
    rem <- metafor::rma(yi=yi, vi = vi, data = df)
    x1 <- as.formula(paste0("~", as.character(input$x1)))
    mem <- metafor::rma(yi=yi, vi = vi, data = df, mods= x1)

    #default values
    beta1 = 0
    I2res = c(0, 0.25, 0.5, 0.75)
    PB = c(0, 0.05, 0.2, 0.5, 1)
    Zcv <- qnorm(0.025, lower.tail=as.logical(input$lower.tail))


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
        need(input$PB.spec >= 0 & input$PB.spec <=1, "The specified value for PB is outside the possible range: [0, 1].")
      )
      beta0.spec = input$beta0.spec
      beta1.spec = input$beta1.spec
      I2res.spec = input$I2.spec/100
      PB.spec = input$PB.spec
      PBbetas = PublicationBiasInModeratorAnalysis::PB_betas(dat = df, beta0 = beta0.spec,
                         beta1=beta1.spec, PB.spec,
                         I2=I2res.spec, mods= df$x1, Zcv = Zcv, lower.tail = as.logical(input$lower.tail))
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
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                            lower.tail = as.logical(input$lower.tail))
      return(p)
    }
  )

  output$download_Fig1 <- downloadHandler(
    filename = function() {"Bias_Plot1.png"},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggplot2::ggsave(file, plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1,
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =0,
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                                                       lower.tail = as.logical(input$lower.tail)),
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
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                            lower.tail = as.logical(input$lower.tail))
      return(p)
    }
  )

  output$download_Fig2<- downloadHandler(
    filename = function() {"Bias_Plot2.png"},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggplot2::ggsave(file, plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1,
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta)/2,
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                                                       lower.tail = as.logical(input$lower.tail)),
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
                            I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                            lower.tail = as.logical(input$lower.tail))
      return(p)
    }
  )

  output$download_Fig3 <- downloadHandler(
    filename = function() {"Bias_Plot3.png"},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggplot2::ggsave(file, plot = PBanalysis_plots(dat =  PBinfo()$data, mods =  PBinfo()$data$x1,
                                                       mem = PBinfo()$mem, Zcv = PBinfo()$Zcv, beta0 =as.numeric(PBinfo()$rem$beta),
                                                       mod.title = paste0("Moderator: ", input$x1) ,
                                                       I2res = PBinfo()$I2res, beta1 = PBinfo()$beta1, PB = PBinfo()$PB,
                                                       lower.tail = as.logical(input$lower.tail)),
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
    filename = function() {"AddBiasPlot.png"},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = input$width, height = input$height,  units = "px")
      }
      ggplot2::ggsave(file, plot = Plot_additional_analysis(data = PBinfo()$data, beta0 = PBinfo()$beta0.spec,
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
      p4 <- metafor::regplot(PBinfo()$mem)
      return(p4)
    }
  )


  output$head <- renderTable({
    req(data())
    head(data(), input$n)
  })
}

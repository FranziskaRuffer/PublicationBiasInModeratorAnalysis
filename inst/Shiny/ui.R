ui <- fluidPage( 
  
  tags$head(
    tags$style(HTML("
     .custom-heading h6 {
        font-size: 1.25em;
        font-weight: bold;
      }
      .custom-heading .MathJax_SVG {
        font-size: 1.25em !important;
      }
    
     .MathJax_SVG {
        color: black !important;
      }

      /* Set color to black for all helpText */
      .help-block {
        color: black !important;
      }
    "))),
  tags$head(
    tags$style(HTML("
    .hanging-indent {
      text-indent: -1.5em;
      padding-left: 1.5em;
      margin-bottom: 1em;
    }
  "))
  ),
  
  titlePanel("Publication Bias in Moderator Effects"),
  
  # App Input
  sidebarLayout(
    sidebarPanel(
      
      #Obtaining data set information
      # selectInput("modtype", "Type of Moderator",
      #             c(binary = "binary", continuous = "continuous")),
      h3(strong("Input Default Analysis")),
      h4("Please upload your meta-analytic data as csv or tsv file."),
      fileInput("upload", NULL, accept = c(".csv", ".tsv")),
      
      
      selectInput("yi", "Effect size", choices = NULL),
      
      selectInput("vtype", "Effect size variance/standard error",
                  choices = c("Sampling variance" = "vi", 
                              "Standard error" = "sei")),
      
      conditionalPanel(
        condition = "input.vtype == 'vi'",
        selectInput("vi", "Sampling variance", choices = NULL),
      ),
      
      conditionalPanel(
        condition = "input.vtype == 'sei'",
        selectInput("sei", "Standard error", choices = NULL),
      ),
      
      selectInput("x1", "Moderator", choices = NULL),
      selectInput("NoPB", "Publication Bias indicator (optional)", choices = NULL),
      
      numericInput("n", "Number of rows displayed in the 'Dataset' tab.", value = 5, min = 1, step = 1),
      
      h3(strong("Input Additional Analysis")),
      selectInput("default", "Do you want to deviate from the default setting?",
                  c( "no",  "yes")),
      
      conditionalPanel(
        condition = "input.default == 'yes'",
        h4("Please specify the follwing 'true' parameter values:"), 
        h5(strong("Mixed-effects meta-analysis intercept and slope (i.e., moderator effect):")),
        numericInput("beta0.spec",
                     label = withMathJax("$$\\beta_{0}$$"),
                     value = 0, min = -5, max = 5 ),  
        numericInput("beta1.spec",
                     label = withMathJax("$$\\beta_{1}$$"),
                     value = 0, min = -5, max = 5 ),
        
        h5(strong("Residual heterogneity:")),
        numericInput("I2.spec",label = withMathJax("$$I_{res}^{2}$$"),
                     value = 25, min = 0, max = 100 ),
        
        h5(strong("Proportion of statistically non-significant effect sizes published:")),
        numericInput("PB.spec",
                     label = "PB",
                     value = 0.2, min = 0, max = 1 ),  
      ),
      
      #Figure settings
      h5(strong("Plot size in pixels.")),
      sliderInput("height", "height", min = 100, max = 1200, value = 800),
      sliderInput("width", "width", min = 100, max = 1200, value = 700),
      
      h4("You can download the Publication Bias Figures by pressing the buttons below."),
      downloadButton("download_Fig1",  "Download Default Analysis Figure 1"),
      downloadButton("download_Fig2",  "Download Default Analysis Figure 2"),
      downloadButton("download_Fig3",  "Download Default Analysis Figure 3"),
      downloadButton("download_AddBiasPlot", "Download Additional Analysis Figure 4")
    ), 
    
    #App Output
    mainPanel(
      navset_card_underline(
        title = " ",
        nav_panel(
          "Default Sensitivity Analysis", 
          tags$h4(strong("Default Visualizations")),
          
          tags$h5(strong("Figure 1: The true overall effect size is zero.")),
          card( style = "margin-bottom: 60px;",
                plotOutput("BiasPlot1", width = "700px", height = "800px"#) #imageOutput("BiasPlot1",  width = 700, height =800
                )),
          
          tags$h5(strong("Figure 2: The true overall effect size is half the observed effect size (see Metafor output: Random-effects model).")),
          card(style = "margin-bottom: 60px;",
               plotOutput("BiasPlot2", width = "700px", height = "800px"#) #imageOutput("BiasPlot2",  width = 700,  height =800
               )),
          
          tags$h5(strong("Figure 3: The true overall effect size is the observed effect size (see Metafor output: Random-effects model).")),
          card(style = "margin-bottom: 60px;",
               plotOutput("BiasPlot3", width = "700px", height = "800px"#) #imageOutput("BiasPlot3",  width = 700, height =800
               ))),
        
        nav_panel(
          "Additional Sensitivity Analysis",
          tags$h4(strong("Additional Visualization")),
          tags$h5(strong("Figure 4: Additional Sensitivity Analysis")),
          
          card(style = "margin-bottom: 60px;",
               plotOutput("AddBiasPlot", width = "700px", height = "800px"#imageOutput("AddBiasPlot",  width = 700,  height =800
               )),
          
          tags$h4(strong("Results Additional Sensitivity Analysis")),
          card(style = "margin-bottom: 60px;",
               verbatimTextOutput("BiasText")),
          
          
          card(class = "mb-100",
               tags$h5(strong("Table 1: Comparing Random-Effects (REM), Mixed-Effects (MEM) Meta-Analysis Results with the Additional Sensitivity Analysis")),
               tableOutput("table") )
        ),
        
        nav_panel("Metafor Output", 
                  tags$h4(strong("Random-Effects Model")),
                  card(style = "margin-bottom: 60px;",
                       verbatimTextOutput("rem")),
                  tags$h4(strong("Mixed-Effects Model")),
                  card( style = "margin-bottom: 60px;",
                        verbatimTextOutput("mem")),
                  tags$h4(strong("Bubble Plot")),
                  card(style = "margin-bottom: 60px;",
                       plotOutput("RegPlot",  width = 700, height =800 #imageOutput("RegPlot",  width = 700, height =800
                       ))),
        
        nav_panel("Data set", 
                  card( class = "mb-4",
                        tableOutput("head"))),
        
        
        nav_panel("User Guide", 
                  
                  tags$h4(strong("Corresponding Paper")),
                  card(class = "mb-4",
                       helpText("For a more details on the reasoning and formulas behind this application,
                              check out the correspongind preprint here: LINK")), 
                  
                  tags$h4(strong("Default Analyses/Figures")),
                  tags$h5(strong("How to Read the Figures")),
                  card(class = "mb-4",
                       helpText("")),
                  
                  
                  tags$h4(strong("Addititional Analysis/Figure")),
                  card(class = "mb-4",
                       helpText("")),
                  
                  tags$h5(strong("Choosing Realistic Parameter Values for Additional Analyses")),
                  card(class = "mb-4",
                       withMathJax(helpText("When you want to deviate from the default analyses and the default parameters estimates (described above), you can perform additional publication bias
                    analyses. For that, you need to choose realistic values for the true intercept \\(\\beta_{0}\\), moderator effect  \\(\\beta_{1}\\), and residual heterogeneity \\(I_{res}^2\\). 
                                         The following gives some guidelines to help you with this process: ",br(),br(),
                                            "One aspect to consider is the realistic true effect size range. For instance, when you're analysing standardizes mean differences and your moderator values 
                                                               range from 0 to 20, specifying a large moderator effect (e.g. \\(\\beta_{1}=0.5\\) may be unrealistic 
                                                               as the true effect sizes would range up to \\(\\beta_{0} + 0.5 * 20 = \\beta_{0} + 10\\). ",br(),br(),
                                            "Concerning the residual
                                                               heterogeneity parameter \\(I_{res}^2\\), you could perform sensitivity analyses using the \\(I_{res}^2\\)
                                                               estimated by the mixed-effects meta-analysis outout (see 'Metafor Output' Tab) and an \\(I_{res}^2\\) that 
                                                               is lower than the estimated and one that is larger than the estimated \\(I_{res}^2\\). This is important, since
                                                               publication bias can distort heterogeneity estimates (Augusteijn et al., 2019; Hedges & Vevea, 1996; Jackson, 2006).
                                                               So, for instance, when the estimated \\(I_{res}^2 =60\\%\\), you 
                                                               could run three analyses with \\(I_{res}^2 =60\\%\\), and \\(I_{res}^2 =60\\% \\pm 25\\%\\). ",br(),br(),
                                            "The publication bias
                                                               parameter (\\(PB\\)) can range from 0 to 1. Setting it to zero (i.e., extreme publication bias where none of the statistically
                                                               non-significant effect sizes are being published) only makes sense, when the vast majority of effect sizes from published studies in your 
                                                               meta-analysis are statistically significant. Setting \\(PB\\) to one implies that no publication selection bias is applied. Research 
                                                               in social sciences has reported publication selection proportions between .04 and 0.85 (Cooper et al., 1997; Franco et al., 2014; 
                                                               Mathur & VanderWeele, 2021)."))),
                  
                  
                  tags$h4(strong("Supported Input")),
                  tags$h5(strong("Effect Size Measures")),
                  card(class = "mb-4",
                       helpText("The current version is restricted to be used for approximately normally distributed effect sizes 
                  such as standardized mean differences,... , etc. Please make sure, that you calculated one of these
                                effect sizes and their standard errors/sampling variances beforehand.")),
                  
                  tags$h5(strong("Moderators")),
                  card(class = "mb-4",
                       helpText("Moderators can only be analysed one at a time. For the analysis, the moderator needs to be numeric. So, if you have a binary moderator with character values,
                                please transform those values to, for instance, zeros and ones beforehand. The current version of the app can be used to analyse either binary or 
                                continuous moderators. Categorical moderators with more than two categories are not supported at the moment.")),
                  
                  tags$h5(strong("Publication Bias Indicator")),
                  card(class = "mb-4",
                       helpText("You can specify in your data set whether publication bias may affect an effect size. With the publication bias indicator, you can distinguish between studies that are assumed to be
                       affected by publication bias and those that are likely not affected by it. This indicator is used in the analysis to only apply publication
                       bias to those effect sizes for which the indicator is 'FALSE'. For instance, the selection of pre-registered studies for publication could be less or not at all influenced by 
                       whether the effect size is statistically signigicant or not. So, it might make sense to not apply publication bias to those effect sizes from pre-registered studies and 
                       to, hence, set the publication bias indicator to 'TRUE' for those effect sizes. Another example are unpublished studies, as they were not selected for publication
                       in the first place. So, you can avoid adding publication bias to unpublished studies by setting the publication bias indicator column to 'TRUE' for these effect sizes."
                       )),
                  
                  tags$h4(strong("Handling Missing Values")),
                  card(class = "mb-4",
                       helpText("The current version performs complete case analyses only. This entails that whenever
                                an effect size, sampling variance/standard error, or moderator value (or Publication Bias 
                                Indicator value 'NoPB') is missing, the information from the corresponding
                                primary study is excluded from all analysis.")),
                  
                  tags$h4(strong("Dependencies")),
                  card(style = "margin-bottom: 60px;",
                       helpText("The current version does not account for dependencies between effect sizes (e.g., 
                                several effect sizes coming from the same lab or paper). Such dependencies affect
                                the standard errors reported in the Metafor Output Tab. Please ignore this output
                                whenever dependencies are present and conduct a multi-level meta-analysis or 
                                a meta-analysis with robust standard errors instead (e.g., see Cheung, 2019). ")),
                  
                  tags$h4(strong("References")),
                  card(style = "margin-bottom: 60px;",
                       div(
                         class = "hanging-indent",
                         "Augusteijn, H. E. M., van Aert, R. C. M., & van Assen, M. A. L. M. (2019). The effect of publication bias on the Q test and assessment of heterogeneity. Psychological Methods, 24(1), 116–134. https://doi.org/10.1037/met0000197"
                       ),
                       div(
                         class = "hanging-indent",
                         "Cheung, M. W.-L. (2019). A guide to conducting a meta-analysis with non-independent effect sizes. Neuropsychology Review, 29(4), 387–396. https://doi.org/10.1007/s11065-019-09415-6"
                       ),
                       div(
                         class = "hanging-indent",
                         "Cooper, H., DeNeve, K., & Charlton, K. (1997). Finding the missing science: The fate of studies submitted for review by a human subjects committee. Psychological Methods, 2(4), 447–452. https://doi.org/10.1037/1082-989X.2.4.447"
                       ),
                       div(
                         class = "hanging-indent",
                         "Franco, A., Malhotra, N., & Simonovits, G. (2014). Publication bias in the social sciences: Unlocking the file drawer. Science, 345(6203), 1502–1505. https://doi.org/10.1126/science.1255484"
                       ),
                       div(
                         class = "hanging-indent",
                         "Hedges, L. V., & Vevea, J. L. (1996). Estimating effect size under publication bias: Small sample properties and robustness of a random effects selection model. Journal of Educational and Behavioral Statistics, 21(4), 299–332. https://doi.org/10.3102/10769986021004299"
                       ),
                       div(
                         class = "hanging-indent",
                         "Jackson, D. (2006). The implications of publication bias for meta-analysis’ other parameter. Statistics in Medicine, 25(17), 2911–2921. https://doi.org/10.1002/sim.2293"
                       ), 
                       div(
                         class = "hanging-indent",
                         "Mathur, M. B., & VanderWeele, T. J. (2021). Estimating publication bias in meta-analyses of peer-reviewed studies: A meta-meta-analysis across disciplines and journal tiers. Research Synthesis Methods, 12(2), 176–191. https://doi.org/10.1002/jrsm.1464"
                       ),
                       div(
                         class = "hanging-indent",
                         "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1–48. https://doi.org/10.18637/jss.v036.i03"
                       )
                  )
                  #)
        )
      )
    )
  )
)


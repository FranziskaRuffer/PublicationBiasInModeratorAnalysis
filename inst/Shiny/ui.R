ui <- fluidPage(

  tags$head(
    tags$style(
    HTML("
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
    tags$style(
      HTML("
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
      bslib::navset_card_underline(
        title = " ",
        bslib::nav_panel(
          "Default Sensitivity Analysis",
          tags$h4(strong("Default Visualizations")),

          tags$h5(strong("Figure 1: The true overall effect size is zero.")),
          bslib::card( style = "margin-bottom: 60px;",
                       plotOutput("BiasPlot1", width = "700px", height = "800px"#) #imageOutput("BiasPlot1",  width = 700, height =800
                )),

          tags$h5(strong("Figure 2: The true overall effect size is half the observed effect size (see Metafor output: Random-effects model).")),
          bslib::card(style = "margin-bottom: 60px;",
                      plotOutput("BiasPlot2", width = "700px", height = "800px"#) #imageOutput("BiasPlot2",  width = 700,  height =800
               )),

          tags$h5(strong("Figure 3: The true overall effect size is the observed effect size (see Metafor output: Random-effects model).")),
          bslib::card(style = "margin-bottom: 60px;",
                      plotOutput("BiasPlot3", width = "700px", height = "800px"#) #imageOutput("BiasPlot3",  width = 700, height =800
               ))),

        bslib::nav_panel(
          "Additional Sensitivity Analysis",
          tags$h4(strong("Additional Visualization")),
          tags$h5(strong("Figure 4: Additional Sensitivity Analysis")),

          bslib::card(style = "margin-bottom: 60px;",
                      plotOutput("AddBiasPlot", width = "700px", height = "800px"#imageOutput("AddBiasPlot",  width = 700,  height =800
               )),

          tags$h4(strong("Results Additional Sensitivity Analysis")),
          bslib::card(style = "margin-bottom: 60px;",
                      verbatimTextOutput("BiasText")),


          bslib::card(class = "mb-100",
                      tags$h5(strong("Table 1: Comparing Random-Effects (REM), Mixed-Effects (MEM) Meta-Analysis Results with the Additional Sensitivity Analysis")),
               tableOutput("table") )
        ),

        bslib::nav_panel("Metafor Output",
                         tags$h4(strong("Random-Effects Model")),
                  bslib::card(style = "margin-bottom: 60px;",
                               verbatimTextOutput("rem")),
                  tags$h4(strong("Mixed-Effects Model")),
                  bslib::card( style = "margin-bottom: 60px;",
                               verbatimTextOutput("mem")),
                  tags$h4(strong("Bubble Plot")),
                  bslib::card(style = "margin-bottom: 60px;",
                              plotOutput("RegPlot",  width = 700, height =800 #imageOutput("RegPlot",  width = 700, height =800
                       )),
                  bslib::card(
                    class = "mb-4",
                    helpText("Note: All output shown in this tab was generated using the metafor R package (Viechtbauer, 2010)."
                  ))
                  ),

        bslib::nav_panel("Data set",
                         bslib::card( class = "mb-4",
                                      tableOutput("head"))),


        bslib::nav_panel("User Guide",

                         tags$h4(strong("Corresponding Paper and R Code")),
                  bslib::card(class = "mb-4",
                              markdown("For more details on the reasoning and formulas behind this application, please refer to the corresponding paper [here](LINK).
                              The R code for this Shiny app and the analyses described in the paper can be found on the
                              [GitHub project page](https://github.com/FranziskaRuffer/PublicationBiasInModeratorAnalysis).
                              On this page, you can also find a guide on how to run
                              the Shiny app locally in your own R environment.")),

                  tags$h4(strong("Default Analyses/Figures")),

                  bslib::card(class = "mb-4",
                              withMathJax(helpText("The purpose of the default analyses and their figures is to indicate how much a moderator effect can be influenced by publication bias,
                              depending on the characteristics of the meta-analysis. To evaluate how robust an observed moderator effect is to influences of publication bias,
                              the analyses assume a null effect for the moderator and generate intercept and slope/moderator effects given a certain amount of publication bias. If
                              publication bias can bias a zero moderator effect enough to induce an artificial moderator effect that is close to the observed
                              moderator effect, then we would have evidence that the observed moderator effect may have been caused by publication bias."))),
                  tags$h5(strong("How to Read the Figures")),
                  bslib::card(class = "mb-4",
                              withMathJax(helpText("In each sub-figure, the effect sizes are on the y-axis and the moderator values on the x-axis. The observed effect sizes from your data set are shown as
                              data points in the shape of triangles or circles. The data point size reflects the effect size's weight in the meta-analysis; that is, the larger the data point, the larger
                              its weight in the meta-analysis. Observed effect sizes displayed as triangles are assumed to be affected by publication bias (i.e., the publication bias indicator
                              'No Publication Bias' = FALSE), while observed effect sizes displayed as circles were assumed to be free of publication bias (i.e., 'No Publication Bias' = TRUE).
                              The regression line from a mixed-effects meta-regression analysis of the original data is shown as a black solid line (check the 'Metafor Output' tab for the details of
                              this model). The broken lines in different colours show the mixed-effects meta-regression lines, given a certain amount of publication bias. For instance, a publication
                              bias of 0.05 refers to assuming that only 5% of the statistically non-significant effect sizes are published, while all statistically significant effect sizes are published.
                              Hence, a publication bias of 1 is the same as assuming the absence of publication bias. The four sub-figures within a figure only differ by the amount of true effect size heterogeneity in the form of
                              \\(I^{2}\\) that is assumed for the publication bias analyses (broken coloured lines). For instance, subfigure \\(a\\) assumes no heterogeneity as \\(I^{2}=0%\\),
                              while subfigure \\(d\\) assumes a large amount of heterogeneity \\(I^{2}=75%\\).
                              Figures 1, 2 and 3 differ in their assumptions about the true overall effect size or, equivalently,
                              the true intercept \\(\\beta_{0}\\) assumed in the publication bias analyses. Figure 1 assumes that the true effect for all studies is
                              zero (i.e., the true intercept \\(\\beta_{0}\\) is zero and the true moderator effect
                              \\(\\beta_{1}\\) is zero). In Figures 2 and 3, the true effect size in all studies is equal to half of the
                              observed average effect size and the observed average effect size, respectively. The overall effect size estimates come from fitting a random-effect meta-analysis (see the 'Metafor Output'
                              tab for more details of the model).",br(),br(),

                              "Using the default figures, we can assess whether the publication-biased intercept and slope/moderator effect estimates are similar to those
                               obtained by meta-analysing the observed data. They are similar when a coloured broken line from the publication bias analysis approaches the black
                               solid line from the meta-analysis on the observed data. Whenever this occurs in a plausible scenario, we may suspect that the observed moderator effect is not
                               robust to publication bias and that publication bias could have induced an artificial moderator effect. 'Plausible scenario' in this context refers to interpreting only
                               those scenarios that are realistic for a given meta-analysis. For instance, if your meta-analysis includes published, statistically non-significant effect sizes, results from publication bias analyses
                               assuming extreme publication bias (i.e., 'Publication Bias' = 0, red dotted lines) are not plausible/possible and should not be interpreted."))),


                  tags$h4(strong("Additional Analysis/Figure")),
                  bslib::card(class = "mb-4",
                              helpText("If you want to deviate from the assumptions of the default analyses, you can conduct additional analyses by specifying the true values for the intercept,
                              slope/moderator effect, and amount of residual heterogeneity.
                              The figure from such an analysis is similar to those from the default analysis. The difference is that you only see two regression lines, one from
                                       meta-analysing the original data (black solid line) and one from your additional sensitivity analysis (orange dashed line). By playing around with the input values for the true intercept
                                       and slope parameters, the true amount of residual heterogeneity and publication bias, you can see whether any combination of values can produce a
                                       meta-regression line that approaches the line from an analysis of the original data. The exact intercept and slope estimates from this publication bias sensitivity analysis
                                       are displayed below the figure. In addition, Table 1 contains the results from fitting a random-effect meta-analysis (REM) and a mixed-effects meta-regression (MEM)
                                       to your data, as well as the results from the publication bias sensitivity analysis (PB Sensitivity). You can find more details about the fitted random and
                                       mixed-effects models in the 'Metafor Output' tab. Note: the MEM results were used to produce the black solid regression line.")),

                  tags$h5(strong("Choosing Realistic Parameter Values for Additional Analyses")),
                  bslib::card(class = "mb-4",
                              withMathJax(helpText("When you want to deviate from the default analyses and the default parameters estimates, you can perform additional publication bias
                    analyses. For that, you need to choose realistic values for the true intercept \\(\\beta_{0}\\), moderator effect  \\(\\beta_{1}\\), and residual heterogeneity \\(I_{res}^2\\).
                                         The following gives some guidelines to help you with this process: ",br(),br(),
                                            "One aspect to consider is the realistic true effect size range. For instance, when you're analysing standardizes mean differences and your moderator values
                                                               range from 0 to 20, specifying a large moderator effect (e.g. \\(\\beta_{1}=0.5\\) may be unrealistic
                                                               as the true effect sizes would range up to \\(\\beta_{0} + 0.5 * 20 = \\beta_{0} + 10\\). ",br(),br(),
                                            "Concerning the residual
                                                               heterogeneity parameter \\(I_{res}^2\\), you could perform sensitivity analyses using the \\(I_{res}^2\\)
                                                               estimated by the mixed-effects meta-analysis (see 'Metafor Output' Tab) and an \\(I_{res}^2\\) that
                                                               is lower than the estimated and one that is larger than the estimated \\(I_{res}^2\\). This is important, since
                                                               publication bias can distort heterogeneity estimates (Augusteijn et al., 2019; Hedges & Vevea, 1996; Jackson, 2006).
                                                               So, for instance, when the estimated \\(I_{res}^2 =60\\%\\), you
                                                               could run three analyses with \\(I_{res}^2 =60\\%\\), and \\(I_{res}^2 =60\\% \\pm 25\\%\\). ",br(),br(),
                                            "The publication bias
                                                               parameter can range from 0 to 1. Setting it to zero (i.e., extreme publication bias where none of the statistically
                                                               non-significant effect sizes are being published) only makes sense, when the vast majority of effect sizes from published studies in your
                                                               meta-analysis are statistically significant. Setting Publication Bias to one implies that no publication selection bias is applied. Research
                                                               in social sciences has reported publication selection proportions between .04 and 0.85 (Cooper et al., 1997; Franco et al., 2014;
                                                               Mathur & VanderWeele, 2021)."))),


                  tags$h4(strong("Supported Input")),
                  tags$h5(strong("Effect Size Measures")),
                  bslib::card(class = "mb-4",
                              helpText("The current version is restricted to be used for approximately normally distributed effect sizes,
                  such as, for instance, standardized mean differences, (Fisher's Z transformed) correlations, and log odds ratios. Please make sure that you have calculated one of these
                                effect sizes and their standard errors/sampling variances beforehand.")),

                  tags$h5(strong("Moderators")),
                  bslib::card(class = "mb-4",
                              helpText("Moderators can only be analysed one at a time. For the analysis, the moderator needs to be numeric. So, if you have a binary moderator with character values,
                                please transform those values to, for instance, zeros and ones beforehand. The current version of the app can be used to analyse either binary or
                                continuous moderators. Categorical moderators with more than two categories are not supported at the moment.")),

                  tags$h5(strong("Publication Bias Indicator")),
                  bslib::card(class = "mb-4",
                              helpText("You can specify in your data set whether publication bias may affect an effect size. With the publication bias indicator, you can distinguish between studies that are assumed to be
                       affected by publication bias and those that are likely not affected by it. This indicator is a TRUE/FALSE variable, and it is used in the analysis to only apply publication
                       bias to those effect sizes for which the indicator is 'FALSE'. For instance, the selection of pre-registered studies for publication could be less or not at all influenced by
                       whether the effect size is statistically significant or not. So, it might make sense to not apply publication bias to those effect sizes from pre-registered studies and
                       to, hence, set the publication bias indicator to 'TRUE' for those effect sizes. Another example is unpublished studies, as they were not selected for publication
                       in the first place. So, you can avoid adding publication bias to unpublished studies by setting the publication bias indicator column to 'TRUE' for these effect sizes."
                       )),

                  tags$h4(strong("Handling Missing Values")),
                  bslib::card(class = "mb-4",
                              helpText("The current version performs complete case analyses only. This entails that whenever
                                an effect size, sampling variance/standard error, or moderator value (or optionally the publication bias
                                indicator value) is missing, the information from the corresponding
                                primary study is excluded from all analyses. In this case, a small note pops up with information
                                about how many studies were excluded.")),

                  tags$h4(strong("Dependencies")),
                  bslib::card(style = "margin-bottom: 60px;",
                              helpText("The current version does not account for dependencies between effect sizes.
                              That is, it is assumed that each study contributes one effect size to the meta-analysis.")),

                  tags$h4(strong("References")),
                  bslib::card(style = "margin-bottom: 60px;",
                       div(
                         class = "hanging-indent",
                         "Augusteijn, H. E. M., van Aert, R. C. M., & van Assen, M. A. L. M. (2019). The effect of publication bias on the Q test and assessment of heterogeneity. Psychological Methods, 24(1), 116–134. https://doi.org/10.1037/met0000197"
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


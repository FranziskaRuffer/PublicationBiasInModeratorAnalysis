# PublicationBiasInModeratorAnalysis

This project contains the R code, Figures, Shiny App and data analyses described in the accompanying 
paper on the "Effects of Publication Bias on Moderator Effects in Meta-Analysis". You can find the 
paper [here]().

## 🚀 Run the Shiny App
### Option 1: Open the hosted app (no R required)

The app is also deployed on [shinyapps.io](https://www.shinyapps.io/?utm_source=chatgpt.com).
👉 Click [here](https://franziskaruffer.shinyapps.io/shiny/) to launch the app.


### Option 2: Run directly from GitHub (requires R)

You can load the R package containing the Shiny App in R by running the code below:

```bash
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
```

Then run the app directly from your local environment by calling pb_mods_App()

```bash
PublicationBiasInModeratorAnalysis::pb_mods_App()
```
For an example of how to do this, you can have a look at the analysis of the 
two applied examples. You can find those analyses in the results_applied_examples.R
code in the paper folder. 


## Reproduce the Analyses 

To reproduce all analyses from the paper, you can download and rerun two R scripts saved
in the paper folder: 
* results_applied_examples.R
* results_theoretical_examples.R

The first script will reproduce sensitivity analyses and figures for the meta-analysis
on the Red Romance Effect (Lehmann et al., 2017) and the meta-analysis on
primary care effectiveness interventions (Baskerville et al., 2012).

The second script reproduces all theoretical analyses and the first four figures
in the paper (see paper/Figures). 

Both scripts load all the necessary functions from this R package (see R folder), 
as well as all the necessary additional R packages in the versions used for the 
original analysis. 

## Where to find what

The project page is organised in folders to distinguish functions, the shiny app and
analyses carried out for the paper. The folders contain the following: 

* *R*: contains R files which define all functions used for the analyses (functions_exp_val.R),
     for plotting the figures (functions_plotting.R), and for running the Shiny app (run_app.R).
     Most of these functions can be called via the PublicationBiasInModeratorAnalysis package.
* *inst/Shiny*: contains the server (server.R) and user interface (ui.R) definitions for the Shiny app 
    which are combined in the app.R file. 
* *man*: contains the manuals of all R functions that can be called with the PublicationBiasInModeratorAnalysis package.
* *paper*: contains analysis scripts used for generating the results in the paper, as well as the figures. 


### License 
The source code is licensed under the MIT License.

Figures and documentation are licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-shield]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg


# PublicationBiasInModeratorAnalysis
[!DOI]()

This project contains the  R code, Figures, Shiny App and data analyses described in the accompanying 
paper on the "Effects of Publication Bias on Moderator Effects in Meta-Analysis".

## 🚀 Run the Shiny App
### Option 1: Open the hosted app (no R required)

The app is also deployed on [shinyapps.io](https://www.shinyapps.io/?utm_source=chatgpt.com).
👉 Click [here]() to launch the app.


### Option 2: Run directly from GitHub (requires R)

You can load the R package containing the Shiny App in R by running the code below:

```bash
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
```

Then run the app directly from your local environment by calling pb_mods_App()

```bash
PublicationBiasInModeratorAnalysis::pb_mods_App()
```
For an example on how to do this, you can have a look at the analysis of the 
two applied examples. You can find those analyses in the results_applied_examples.R
code in the paper folder. 


## Reproduce the Analyses 

To reproduce the all analyses, you can download and rerun two R scripts saved
in the paper folder: 
* results_applied_examples.R
* results_theoretical_examples.R

The first script will reproduce sensitivity analyses and figures for the meta-analysis
on the Red Romance Effect (Lehmann et al., 2017) and the meta-analysis on
primary care effectiveness interventions (Baskerville et al., 2012).

The second script reproduces all theoretical analyses and the first four figure
in the paper (see paper/Figures). 

Both scripts load all the necessary functions from this R package (see R folder), 
as well as all the necessary additional R packages in the versions used for the 
original analysis. 

### License
Figures and documentation are licensed under CC-BY-4.0. The source code is licensed under the MIT License.


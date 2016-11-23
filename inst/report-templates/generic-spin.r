#' ---
#' title: "RAStestR Report"
#' ---
#'
#' ## Summary
#'

#+ requirements, echo = FALSE, include = FALSE
library(ggplot2)
library(dplyr)
library(stringr)
library(knitr)
plot.theme = list(
  theme_grey(base_size = 16),
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
)

#' This is a basic RASTestR report for comparing outputs of a 
{{ sprintf("**new model %s**", label2) }}
#' to outputs of a
{{ sprintf("**base model %s**.", label1) }}
#' The new model is 
{{ sprintf("*type %s*", type2) }}
#' and the base model is 
{{ sprintf("*type %s*.", type1) }}
#' The data sources of the two models are listed below:
#'
{{ sprintf(" - **Base**: %s\n - **Compare**: %s", file1, file2) }}
#'
#' The following sections are included in this report:

#+ print-section, echo = FALSE, results = "asis"
cat(paste0(seq_along(childsections), ". ", childsections), sep = '\n')

#+ loop-children, echo = FALSE, include = FALSE
out = NULL
for (section.label in childsections) {
  chunkfile = sectionchunk(section.label)
  print(chunkfile)
  out = c(out, spin_child(chunkfile))
}

#+ print-report, echo= FALSE, results = "asis"
cat(out, sep = '\n\n')

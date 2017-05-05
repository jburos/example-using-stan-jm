## warning: requires R version >= 3.3.0
install.packages('devtools')
install.packages('rmarkdown')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('survminer')
install.packages('stringr') ## without this, docopt install sometimes fails
install.packages('docopt')
install.packages('DT')
# devtools::install_github('sambrilleman/rstanarm', ref = 'develop2', args = '--preclean', local = TRUE)
devtools::install_github('jburos/rstanarm', ref = 'fix-posterior-predict-newdata',  args = '--preclean', local = TRUE)
install.packages('JM')

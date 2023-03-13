# TypologyGenerator ![image](https://user-images.githubusercontent.com/41451354/224658201-33dd358b-c92d-42b7-9b17-040c3ce869d1.png)

TypologyGenerator, an R-Shiny HTML application to construct distinct farm typologies using the generalized framework as described in Hassall et al (submitted). 

## For users
TypologyGenerator can be downloaded as a bundle through TypologyGenerator.zip. 

The following libraries are required to be installed before first use;
```r
devtools::install_github("ricardo-bion/ggradar", 
                          dependencies = TRUE)
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinythemes")
install.packages("rhandsontable")
install.packages("shinyWidgets")
install.packages("vegan")
install.packages("ade4")
install.packages("MASS")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("viridis") 
install.packages("dendextend")
```

To run the software application, open the file server.R (in RStudio) and press “Run App”. This will open the software application for use. Help pages are provided on a tab within the shiny application. Example data are provided in the file TP.csv

## For developers 
All necessary R codes are provided as individual files.

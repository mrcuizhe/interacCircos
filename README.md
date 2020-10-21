# NGCircos.r

A R package for Next Generation Circos(NGCircos)

## Introduction

NGCircos is developed in JavaScripts based on D3.js and JQuery.js

NGCircos is originally published at NAR Genomics and Bioinformatics and available at https://wlcb.oit.uci.edu/NG-Circos 

Since R language is much more friendly in data visualization and plot programming than html, we ported NGCircos to this R package based on the *htmlwidgets* framework.

## Features

* 20 modules(*ARC*, *AUXILIARYLINE*, *BACKGROUND*, *BUBBLE*, *CHORD*, *CNV*, *COMBINATION*, *COMPARE*, *GENE*, *HEATMAP*, *HISTOGRAM*, *LEGEND*, *LINE*, *LINK*, *LOLLIPOP*, *REDIRECT*, *SCATTER*, *SNP*, *TEXT*, *WIG*) of NGCircos are ported to NGCircos.r and all parameters and animations are kept. All modules are now presented in function.
* Instead of transforming data using python script in NGCircos, the data file can be directly input into NGCircos.r which will automatically transform data into data.frame and then be input to module function.
* NGCircos.r is much more programming friendly than NGCircos. Users can draw a interactive Circos plot as easy as drawing a plot using ggplot2.


## Installation

        # You need devtools for that
        if (!require('devtools')){install.packages('devtools')}
        devtools::install_github('mrcuizhe/NGCircos.r', build_vignettes = TRUE)
        
## Document

Document is available at 

- [NGCircos.r-jupyter notebook](https://mybinder.org/v2/gh/mrcuizhe/NGCircos.r/master?filepath=doc%2FNGCircos.r_document.ipynb)

Or 

- [NGCircos.r-pdf](https://github.com/mrcuizhe/NGCircos.r/blob/master/NGCircos_1.0.0.pdf)

        
## Contact

Please contact cuizhe@hit.edu.cn or mrcuizhe@gmail.com for help

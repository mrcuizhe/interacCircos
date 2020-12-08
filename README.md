# rNGCircos

A R package for Next Generation Circos(NGCircos)

## Introduction

NG-Circos is developed in JavaScripts based on D3.js and JQuery.js

NG-Circos is originally published at NAR Genomics and Bioinformatics and available at 

- [NG-Circos Document](https://wlcb.oit.uci.edu/NG-Circos)

- [NG-Circos Paper](https://academic.oup.com/nargab/article/2/3/lqaa069/5901067)

Since R language is much more friendly in data visualization and plot programming than html, we published this R package based on *NG-Circos* and the *htmlwidgets* framework.

## Features

* 20 modules(*ARC*, *AUXILIARYLINE*, *BACKGROUND*, *BUBBLE*, *CHORD*, *CNV*, *COMBINATION*, *COMPARE*, *GENE*, *HEATMAP*, *HISTOGRAM*, *LEGEND*, *LINE*, *LINK*, *LOLLIPOP*, *REDIRECT*, *SCATTER*, *SNP*, *TEXT*, *WIG*) of NG-Circos are available at rNGCircos and all parameters and animations are kept. All modules are now presented in function.
* Instead of transforming data using python script in NG-Circos, the data file can be directly input into rNGCircos which will automatically transform data into data.frame and then be input to module function.
* rNGCircos is much more programming friendly than NG-Circos. Users can draw an interactive Circos plot as easy as drawing a plot using ggplot2.


## Installation

        # Via devtools
        if (!require('devtools')){install.packages('devtools')}
        devtools::install_github('mrcuizhe/rNGCircos', build_vignettes = TRUE)
        
## Document

Document is available at 

- [rNGCircos-jupyter notebook](https://mybinder.org/v2/gh/mrcuizhe/rNGCircos/master?filepath=doc%2FrNGCircos_document.ipynb) & Run each cell for demo ( Shift + Enter ).

Or 

- [NGCircos.r-pdf](https://github.com/mrcuizhe/rNGCircos/blob/master/rNGCircos_1.0.0.pdf)

        
## Contact

Please contact cuizhe@hit.edu.cn or mrcuizhe@gmail.com for help

# interacCircos

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/interacCircos)](https://CRAN.R-project.org/package=interacCircos)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GPLv3](https://www.gnu.org/graphics/gplv3-or-later.png)](https://www.gnu.org/licenses/gpl-3.0.html)
<!-- badges: end -->

The **interacCircos** package is designed for the generation of interactive Circos plot

## Introduction

The **interacCircos** package is inspired by [circosJS](https://github.com/nicgirault/circosJS), [BioCircos.js](https://pubmed.ncbi.nlm.nih.gov/26819473/) and [NG-Circos](https://academic.oup.com/nargab/article/2/3/lqaa069/5901067).

circosJS, BioCircos.js and NG-Circos are developed in JavaScript to provide a framework for displaying the interactive Circos plot in website. 

For these 3 JavaScript library, plot are all drawn through different JS functions(back-end) and displayed through HTML element(front-end), which requires users be familiar with JavaScript language and HTML language very much. Besides, the way of data input in JavaScript is not user-friendly, especially for data with millions of rows.

We integrate the modules of circosJS, BioCircos.js and NG-Circos into this R package: **interacCircos**, based on *htmlwidgets* framework.

## Features

* 21 modules(*Arc*, *Auxilirayline*, *Background*, *Bubble*, *Chord*,*chord.p*, *Cnv*, *Combination*, *Compare*, *Gene*, *Heatmap*, *Histogram*, *Legend*, *Line*, *Link*, *Lollipop*, *Redirect*, *Scatter*, *Snp*, *Text*, *Wig*) of circosJS, BioCircos.js and NG-Circos are available at **interacCircos**. All modules from different libraries are now presented in R function and compatible to each other.

* Instead of transforming data using python script of manually, the data can be directly input into **interacCircos** through data.frame format, which is one of the most common data type of R and can easily cooperate with other R packages.

* **interacCircos** is much more programming friendly. Users can draw a multi-track interactive Circos plot in at least 2 steps, as easy as drawing a plot using ggplot2.

## Installation

By devtools

        # Via devtools
        if (!require('devtools')){install.packages('devtools')}
        devtools::install_github('mrcuizhe/interacCircos')
        
        # htmlwidgets, RColorBrewer, plyr are required !  

By Bioconductor:

        #Via Bioconductor
        if (!requireNamespace("BiocManager", quietly = TRUE))
                install.packages("BiocManager")
        
        BiocManager::install("interacCircos")

        # htmlwidgets, RColorBrewer, plyr are required !  
        # Bioconductor version: ≥ Release (3.13)
        # R version ≥ 4.1

## Documentation

Documentation is available at 

- [interacCircos-documentation](https://mrcuizhe.github.io/interacCircos_documentation/index.html)

Or 

- [interacCircos-pdf](https://mrcuizhe.github.io/interacCircos_documentation/html/all_parameters.html)

## Citation

Please cite [interacCircos](https://doi.org/10.1093/bioinformatics/btab232) for publication

## Contact

Please contact cuizhe@hit.edu.cn or mrcuizhe@gmail.com for help


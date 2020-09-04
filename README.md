# NGCircos.r
A R package for Next generation Circos(NGCircos)

## Introduction
NGCircos is developed in JavaScripts and published at https://wlcb.oit.uci.edu/NG-Circos

We developed this R packaged for NGCircos based on htmlwidget framework

## Installation

        # You need devtools for that
        if (!require('devtools')){install.packages('devtools')}
        devtools::install_github('mrcuizhe/NGCircos.r', build_vignettes = TRUE)
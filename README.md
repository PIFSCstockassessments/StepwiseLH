# StepwiseLH
Package to generate life history probability distributions from an estimate of maximum length.

## GitHub Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by servicemark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

# Package Installation

```
library(devtools)
install_github("PIFSCstockassessments/StepwiseLH")
```

# Usage

`StepwiseLH`'s primary method is `Get_distributions` with requires the following parameters:

  `Family` is the taxon for which life history parameter estimates will be generated (see table below for available values).

  `Lmax` is the mean of the normal distribution of 99th percentile of length in a typical survey or catch dataset.

  `Lmax.SD` is the standard deviation of the normal distribution of 99th percentile of length.

  `M_method` is the model used to generate natural mortality (*M*). Default value is 0.04. If a number is entered, this number will represent survivorship at max age (*S*). If "Then_2014" is entered, *M* will be estimate following
   the Then et al. (2014) method. If "Pauly_Then_2014" is entered, *M* will be estimated using the Then et al. (2014) updated equation of the Pauly et al. (1980) method.

  `n_iter` is the number of Monte Carlo iterations used to generate output distributions. Default value is 5000.

  'dist_summary' is a true/false boolean to specify if summary statistics are generated. Default value is True.

The following example generates random distributions of _Acanthuridae_ (Surgeonfishes) with a `Lmax` mean and associated standard deviations, as well as surviorship at max age equal to 0.04.
```
library(StepwiseLH)
Data <- Get_distributions(Family="Acanthuridae", Lmax.mean=600, Lmax.SD=50, M_method=0.04)
```
`Data` is the **data.table** object returned from `Get_distributions`:
```
      Lmax            Linf             K                 A0            Lmat            Amat              M                Amax       
 Min.   :401.2   Min.   :356.1   Min.   :0.05388   Min.   :-0.6   Min.   :250.4   Min.   : 0.409   Min.   :0.01252   Min.   : 16.55  
 1st Qu.:566.1   1st Qu.:532.9   1st Qu.:0.19818   1st Qu.:-0.6   1st Qu.:412.8   1st Qu.: 3.756   1st Qu.:0.08833   1st Qu.: 26.41  
 Median :599.1   Median :570.4   Median :0.26766   Median :-0.6   Median :456.1   Median : 5.568   Median :0.10490   Median : 30.68  
 Mean   :599.3   Mean   :571.0   Mean   :0.29653   Mean   :-0.6   Mean   :460.6   Mean   : 6.575   Mean   :0.10479   Mean   : 33.11  
 3rd Qu.:633.6   3rd Qu.:608.0   3rd Qu.:0.36577   3rd Qu.:-0.6   3rd Qu.:502.6   3rd Qu.: 8.218   3rd Qu.:0.12188   3rd Qu.: 36.44  
 Max.   :795.8   Max.   :776.2   Max.   :1.32707   Max.   :-0.6   Max.   :791.4   Max.   :53.919   Max.   :0.19445   Max.   :257.05  
                                                                                  NA's   :117                                        
          Lmax     Linf          K   A0     Lmat      Amat          M     Amax
   1: 614.3950 570.0183 0.25820404 -0.6 396.4040  4.004236 0.13282306 24.23432
   2: 512.2795 484.6708 0.26622224 -0.6 367.7741  4.742076 0.13572667 23.71587
   3: 682.3660 673.2720 0.26642125 -0.6 637.8082 10.448808 0.06272129 51.32031
   4: 626.8624 611.9785 0.21054987 -0.6 497.2772  7.352349 0.13695462 23.50323
   5: 401.2273 356.0943 0.35028486 -0.6 295.5570  4.458554 0.07497098 42.93496
  ---                                                                         
4996: 608.2304 597.3491 0.52836143 -0.6 458.5285  2.161971 0.11358994 28.33768
4997: 634.4598 590.6622 0.09441384 -0.6 440.5053 13.905965 0.12567246 25.61322
4998: 625.6482 593.5151 0.17901557 -0.6 554.4157 14.593953 0.08575178 37.53713
4999: 545.6864 483.2731 0.20089035 -0.6 375.8214  6.884384 0.09015481 35.70387
5000: 637.0055 646.0454 0.22407480 -0.6 559.9136  8.392491 0.09547127 33.71565
```

### Available families

The following `Family` values available for `Get_distributions`:


| Family             | Common Name                            |
| ------------------ | ---------------------------------------|
| `Acanthuridae`     | Surgeonfishes                          |
| `Carangidae`       | Jacks                                  |
| `Haemulidae`       | Grunts and sweetlips                   |
| `Labridae`         | Wrasses                                |
| `Lethrinidae`      | Emperors                               |
| `Lutjanidae`       | Snappers                               |
| `Mullidae`         | Goatfishes                             |
| `Scaridae`         | Parrotfishes                           |
| `Serranidae`       | Groupers                               |
| `Shark`            | Sharks(Lamniforms and Carcharhiniforms)|

Note: This tool is untested and not recommended for Anthias (family Serranidae) and small wrasse genera (e.g.,*Cirrhilabrus*, *Halichoeres*, *Labroides*, and *Pseudojuloides*).

This tool is also built and tested for species in warmer sea surface temperature (i.e. >20C).


## References 
Nadon, MO and Ault, JS (2016) A stepwise stochastic simulation approach to estimate life history parameters 
	for data-poor fisheries. Can. J. Fish. Aqua. Sci. 73:1874-1884.

Erickson, K and Nadon, MO (in prep) An extension of the stepwise stochastic simulation approach for estimating missing life history parameters
	for sharks, groupers, and other taxa.



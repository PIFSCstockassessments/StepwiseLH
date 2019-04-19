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

`StepwiseLH`'s primary method is `Get_distributions`. *TODO: [Short description]*. `Lmax` is the 99th percentile of length in a typical survey or catch dataset.


The following example generates random distributions of _Acanthuridae_ (Surgeonfishes) governed by Life History parameters, `Lmax` mean and standard deviations, as well as surviorship at max age.
```
library(StepwiseLH)
Data <- Get_distributions(Family="Acanthuridae", Lmax.mean=600, Lmax.SD=50, M_method=0.04)
```
Results were stored as a **data.table** object named `Data`:
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


| Family             | Common Name                      |
| ------------------ | -------------------------------- |
| `Acanthuridae`     | Surgeonfishes                    |
| `Carangidae`       | Jacks                            |
| `Lethrinidae`      | Emperors                         |
| `Lutjanidae`       | Snappers                         |
| `Mullidae`         | Goatfishes                       |
| `Scaridae`         | Parrotfishes                     |
| *Serranidae* [1]   | Groupers                         |
| *Labridae* [1]     | Wrasses                          |
| *Haemulidae* [1]   | Grunts/sweetlips                 |
| *Shark* [1]        | Carcharinids and other families  |

[1] The following Family Life History parameters are experimental: Ther are from unpublished sources, and subject to peer review. 

## References 
TODO



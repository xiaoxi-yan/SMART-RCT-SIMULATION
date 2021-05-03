Title: "Sequential Multiple Assignment Randomized Trial (SMART) to identify optimal sequences of telemedicine interventions for improving initiation of insulin therapy: A simulation study"

*Author: Xiaoxi Yan is mainly responsible for the writing of the code. 
Email address: xiaoxi.yan@u.duke.nus.edu

Configuration:
> sessionInfo()
R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
[1] gridExtra_2.3     reshape2_1.4.3    latex2exp_0.4.0   ggplot2_3.2.1    
[5] dplyr_1.0.2       doParallel_1.0.15 iterators_1.0.12  foreach_1.4.7    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3       rstudioapi_0.10  magrittr_1.5     tidyselect_1.1.0
 [5] munsell_0.5.0    colorspace_1.4-1 R6_2.4.1         rlang_0.4.10    
 [9] plyr_1.8.4       stringr_1.4.0    tools_3.6.1      grid_3.6.1      
[13] packrat_0.5.0    gtable_0.3.0     withr_2.1.2      digest_0.6.22   
[17] lazyeval_0.2.2   tibble_2.1.3     lifecycle_0.2.0  crayon_1.3.4    
[21] farver_2.0.1     purrr_0.3.3      vctrs_0.3.6      codetools_0.2-16
[25] glue_1.4.2       labeling_0.3     stringi_1.4.3    compiler_3.6.1  
[29] pillar_1.4.2     generics_0.0.2   scales_1.1.0     pkgconfig_2.0.3 


README
1. The following files contain only functions: "functions.R" contains data generation functions for SMART and RCT design, and a IPW mean calculation function for SMART AI.

2. Require installation of packages stated above ("doParallel", "reshape2", "gridExtra", "ggplot2","dplyr","latex2exp","multcomp") if not installed. This could be done by uncommenting the indicated line in "functions.R". 

2. Run "base_simulation.R" to generate the data and produce the results for Table 2 and Figure 3,4,5 in manuscript.
    - A new folder "output/", and subfolders "mean/" and "best_AI/" will be produced. Within the subfolders, csv files with simulation results for each design and sample size will be produced.
    - Table 2 and Figure 3,4,5 used in manuscript will be output into "output/" folder.
    - This may take a few minutes depending on computer.

3. Run "sensitivity_simulation.R" to generate the data and produce the results for Table 6 in manuscript.
    - A "sensitivity" subfolder will be produced in "output", storing csv files with simulation results for each threshold level and sample size. This may take 10-30 minutes depending on computer.
    - Table 6 used in manuscript will be output into "output/" folder.
    



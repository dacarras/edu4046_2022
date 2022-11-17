Code: Mediación ejemplos
================
dacarras
Noviembre 17, 2022

# Folder locations

``` r
# -------------------------------------------------------------------
# folder locations
# -------------------------------------------------------------------

# -----------------------------------------------
# get local path from relative path
# -----------------------------------------------

local_path <- function(x){
paste0(tools::file_path_as_absolute(x),'/')  
}

# -----------------------------------------------
# dacarras folders
# -----------------------------------------------

data_folder   <- local_path(getwd())
output_folder <- local_path(getwd())

# -----------------------------------------------
# check folders
# -----------------------------------------------

list.files(data_folder)
```

    ##  [1] "00_prev"                                       
    ##  [2] "data_aut.rds"                                  
    ##  [3] "edu4046_clase_16a_tracing_rules.pdf"           
    ##  [4] "edu4046_clase_16a_tracing_rules.pptx"          
    ##  [5] "edu4046_clase_16b_decomposition.pdf"           
    ##  [6] "edu4046_clase_16b_decomposition.pptx"          
    ##  [7] "edu4046_clase_16b_mediation_examples_otl_files"
    ##  [8] "edu4046_clase_16c_examples_aut_code.rmd"       
    ##  [9] "edu4046_clase_16c_examples_aut.pdf"            
    ## [10] "edu4046_clase_16c_examples_aut.pptx"           
    ## [11] "edu4046_clase_16d_examples_otl_code.rmd"       
    ## [12] "edu4046_clase_16d_examples_otl.pdf"            
    ## [13] "edu4046_clase_16d_examples_otl.pptx"

``` r
list.files(output_folder)
```

    ##  [1] "00_prev"                                       
    ##  [2] "data_aut.rds"                                  
    ##  [3] "edu4046_clase_16a_tracing_rules.pdf"           
    ##  [4] "edu4046_clase_16a_tracing_rules.pptx"          
    ##  [5] "edu4046_clase_16b_decomposition.pdf"           
    ##  [6] "edu4046_clase_16b_decomposition.pptx"          
    ##  [7] "edu4046_clase_16b_mediation_examples_otl_files"
    ##  [8] "edu4046_clase_16c_examples_aut_code.rmd"       
    ##  [9] "edu4046_clase_16c_examples_aut.pdf"            
    ## [10] "edu4046_clase_16c_examples_aut.pptx"           
    ## [11] "edu4046_clase_16d_examples_otl_code.rmd"       
    ## [12] "edu4046_clase_16d_examples_otl.pdf"            
    ## [13] "edu4046_clase_16d_examples_otl.pptx"

``` r
# -----------------------------------------------
# load main library
# -----------------------------------------------

library(dplyr)
```

# Read files

``` r
# -------------------------------------------------------------------
# open file
# -------------------------------------------------------------------

data_aut <- readRDS('data_aut.rds')

# -----------------------------------------------
# check data structure
# -----------------------------------------------

dplyr::glimpse(data_aut)
```

    ## Rows: 10,273
    ## Columns: 155
    ## $ cty    <chr> "CHL", "CHL", "CHL", "CHL", "CHL", "CHL", "CHL", "CHL", "CHL", …
    ## $ cod    <dbl> 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152…
    ## $ ge1    <dbl> 4, 4, 3, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 3, 4, 4, 3, 4, 4, 3, 4, …
    ## $ ge2    <dbl> 4, 4, 4, 4, 4, 3, 4, 4, 4, 4, 3, 4, 3, 3, 4, 4, 2, 4, 4, 3, 4, …
    ## $ ge3    <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 4, 4, 3, 4, 4, 2, 4, 4, 4, 4, …
    ## $ ge4    <dbl> 2, 1, 2, 2, 1, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 4, 1, 1, 4, 4, …
    ## $ ge5    <dbl> 3, 1, 2, 3, 1, 2, 2, 1, 4, 1, 2, 2, 2, 2, 1, 1, 3, 1, 2, 4, 3, …
    ## $ ge6    <dbl> 4, 2, 3, 2, 2, 1, 3, 1, 3, 2, 3, 2, 2, 2, 1, 1, 3, 1, 2, 3, 4, …
    ## $ au1    <dbl> 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, NA, 2, 1, 1, 4, 2, 1, 2, 2,…
    ## $ au2    <dbl> 1, 1, 2, 3, 1, 2, 3, 2, 3, 1, 3, 1, NA, 2, 1, 1, 3, 2, 2, 2, 2,…
    ## $ au3    <dbl> 2, 3, 3, 2, 2, 3, 3, 2, 3, 1, 3, 2, NA, 3, 1, 1, 4, 3, 3, NA, 3…
    ## $ au4    <dbl> 2, 1, 2, 4, 1, 2, 2, 1, 2, 1, 3, 2, NA, 2, 1, 1, 4, 2, 1, 2, 2,…
    ## $ au5    <dbl> 2, 2, 4, 3, 2, 3, 4, 2, 3, 2, 3, 3, NA, 3, 1, 3, 3, 3, 2, 3, 3,…
    ## $ au6    <dbl> 2, 3, 2, 2, 1, 1, 4, 2, 2, 2, 2, 2, NA, 2, 1, 4, 4, 1, 1, 2, 2,…
    ## $ au7    <dbl> 3, 1, 3, 3, 1, 2, 4, 2, 3, 2, 3, 2, 2, 3, 1, 2, 3, 1, 2, 3, 3, …
    ## $ au8    <dbl> 2, 1, 3, 2, 1, 2, 2, 1, 3, 2, 2, 2, 2, 3, 1, 1, 3, 2, 2, 2, 2, …
    ## $ au9    <dbl> 2, 2, 3, 4, 2, 1, 3, 1, 3, 2, 2, 2, 3, 3, 1, 2, 4, 2, 2, 3, 3, …
    ## $ op1    <dbl> 3, 2, 3, 3, 2, 2, 2, 3, 3, 4, 2, 3, 2, 3, 2, 3, 4, 3, 4, 3, 3, …
    ## $ op2    <dbl> 2, 3, 4, 2, 4, 4, 4, 4, 3, 4, 2, 4, 1, 3, 4, 4, 4, 2, 4, 3, 3, …
    ## $ op3    <dbl> 4, 2, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 4, 2, 3, …
    ## $ op4    <dbl> 1, 1, 2, 1, 3, 2, 3, 2, 2, 3, 4, 2, 2, 2, 1, 1, 3, 2, 3, 2, 3, …
    ## $ op5    <dbl> 2, 4, 2, 2, 3, 2, 4, 2, 3, 4, 2, 3, 2, 3, 2, 3, 3, 4, 3, 4, 3, …
    ## $ op6    <dbl> 3, 2, 3, 3, 3, 4, 4, 4, 3, 3, 2, 2, 1, 3, 2, 3, 2, 2, 3, 4, 3, …
    ## $ op7    <dbl> 2, 3, 2, 3, 3, 3, 3, 4, 4, 4, 3, 4, 2, 2, 3, 4, 4, 3, 3, 3, 3, …
    ## $ edm    <dbl> 3, 5, 3, 3, 3, 2, 4, 3, 3, 4, 3, 6, 4, NA, 3, 5, 4, 3, 3, 3, 3,…
    ## $ edf    <dbl> 1, 4, 3, 4, 3, 3, 3, 5, 4, 3, 6, 4, 5, 4, 4, 4, 4, 3, 2, 4, 3, …
    ## $ edx    <dbl> 5, 2, 3, 3, 3, 4, 3, 3, 3, 3, 3, 2, 2, 2, 3, 2, 2, 3, 4, 3, 3, …
    ## $ bok    <dbl> 2, 2, 2, 0, 2, 1, 2, 1, 5, 1, 0, 1, 0, 1, 4, 0, 1, 2, 2, 2, 3, …
    ## $ gen    <dbl> -0.92459074, 0.49690288, -0.73055044, -0.51244350, 0.49690288, …
    ## $ aut    <dbl> -0.09383099, -0.45230038, 0.88528115, 1.00822372, -1.16122960, …
    ## $ inm    <dbl> 3, 2, 2, 1, 1, 1, 2, 1, 2, 2, 1, 3, 0, 1, 3, 1, 1, 1, 1, 1, 1, …
    ## $ inf    <dbl> 3, 1, NA, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 3, 0, 1, 3, 1, 1,…
    ## $ inx    <dbl> 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, …
    ## $ civ1   <dbl> 0.47372376, -0.95632807, 0.05312028, -1.78701995, 0.30548237, -…
    ## $ civ2   <dbl> 0.29496728, -0.35696812, -0.68293581, -1.60826347, 0.45269358, …
    ## $ civ3   <dbl> 0.21084658, -0.45160390, -0.18872672, -1.69238417, -0.05203059,…
    ## $ civ4   <dbl> -0.03100042, -0.71448107, -0.46211899, -2.13401782, 0.59990480,…
    ## $ civ5   <dbl> 0.77866128, -0.91426773, -0.02048533, -1.80805012, 0.96793285, …
    ## $ opd    <dbl> -0.9356208, -0.7463138, -0.3483589, -0.7463138, 0.3745659, 0.10…
    ## $ ses    <dbl> 1.16665222, -1.17623742, 0.03919470, -0.77188999, -0.49257816, …
    ## $ age    <dbl> 14.33, 13.67, 13.67, 13.42, 13.92, 14.00, 13.58, 14.17, 14.17, …
    ## $ sex    <dbl> 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, NA, 1, 0, 1, 1,…
    ## $ id_k   <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, …
    ## $ id_s   <dbl> 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301, 301…
    ## $ id_j   <dbl> 31001, 31001, 31001, 31001, 31001, 31001, 31001, 31001, 31001, …
    ## $ id_i   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, …
    ## $ wt     <dbl> 63.31969, 63.31969, 63.31969, 63.31969, 63.31969, 63.31969, 63.…
    ## $ wi     <dbl> 2.111111, 2.111111, 2.111111, 2.111111, 2.111111, 2.111111, 2.1…
    ## $ wj     <dbl> 29.99354, 29.99354, 29.99354, 29.99354, 29.99354, 29.99354, 29.…
    ## $ wh     <dbl> 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,…
    ## $ wa1    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ wa2    <dbl> 1.091896, 1.091896, 1.091896, 1.091896, 1.091896, 1.091896, 1.0…
    ## $ wb1    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ wb2    <dbl> 1.091896, 1.091896, 1.091896, 1.091896, 1.091896, 1.091896, 1.0…
    ## $ ws     <dbl> 0.12251224, 0.12251224, 0.12251224, 0.12251224, 0.12251224, 0.1…
    ## $ jkz    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ year   <dbl> 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 200…
    ## $ cl1    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl2    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl3    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl4    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl5    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl6    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cl7    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cln    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ etm    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0,…
    ## $ etf    <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ etx    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ bkc    <dbl> 2, 2, 2, 0, 2, 1, 2, 1, 4, 1, 0, 1, 0, 1, 4, 0, 1, 2, 2, 2, 3, …
    ## $ grp    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ main   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ ses_c  <dbl> -0.5276149, -0.5276149, -0.5276149, -0.5276149, -0.5276149, -0.…
    ## $ ses_g  <dbl> -0.00000000000000002359931, -0.00000000000000002359931, -0.0000…
    ## $ ses_w  <dbl> 1.694267166, -0.648622471, 0.566809649, -0.244275040, 0.0350367…
    ## $ ses_m  <dbl> 1.16665222, -1.17623742, 0.03919470, -0.77188999, -0.49257816, …
    ## $ ses_b  <dbl> -0.5276149, -0.5276149, -0.5276149, -0.5276149, -0.5276149, -0.…
    ## $ sex_c  <dbl> 0.5294118, 0.5294118, 0.5294118, 0.5294118, 0.5294118, 0.529411…
    ## $ sex_g  <dbl> 0.5136601, 0.5136601, 0.5136601, 0.5136601, 0.5136601, 0.513660…
    ## $ sex_w  <dbl> -0.5294118, 0.4705882, -0.5294118, -0.5294118, 0.4705882, 0.470…
    ## $ sex_m  <dbl> -0.5136601, 0.4863399, -0.5136601, -0.5136601, 0.4863399, 0.486…
    ## $ sex_b  <dbl> 0.01575162, 0.01575162, 0.01575162, 0.01575162, 0.01575162, 0.0…
    ## $ edu    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0,…
    ## $ edu_c  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.0…
    ## $ edu_g  <dbl> 0.1473249, 0.1473249, 0.1473249, 0.1473249, 0.1473249, 0.147324…
    ## $ edu_w  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.0…
    ## $ edu_m  <dbl> -0.1473249, -0.1473249, -0.1473249, -0.1473249, -0.1473249, -0.…
    ## $ edu_b  <dbl> -0.1473249, -0.1473249, -0.1473249, -0.1473249, -0.1473249, -0.…
    ## $ bkz    <dbl> 0.2097103, 0.2097103, 0.2097103, -1.5483159, 0.2097103, -0.6693…
    ## $ bkz_c  <dbl> -0.2786303, -0.2786303, -0.2786303, -0.2786303, -0.2786303, -0.…
    ## $ bkz_g  <dbl> 0.000000000000000004565365, 0.000000000000000004565365, 0.00000…
    ## $ bkz_w  <dbl> 0.4883406, 0.4883406, 0.4883406, -1.2696856, 0.4883406, -0.3906…
    ## $ bkz_m  <dbl> 0.2097103, 0.2097103, 0.2097103, -1.5483159, 0.2097103, -0.6693…
    ## $ bkz_b  <dbl> -0.2786303, -0.2786303, -0.2786303, -0.2786303, -0.2786303, -0.…
    ## $ opd_c  <dbl> -0.1580373, -0.1580373, -0.1580373, -0.1580373, -0.1580373, -0.…
    ## $ opd_g  <dbl> -0.0000000000000003162329, -0.0000000000000003162329, -0.000000…
    ## $ opd_w  <dbl> -0.77758356, -0.58827652, -0.19032166, -0.58827652, 0.53260315,…
    ## $ opd_m  <dbl> -0.9356208, -0.7463138, -0.3483589, -0.7463138, 0.3745659, 0.10…
    ## $ opd_b  <dbl> -0.1580373, -0.1580373, -0.1580373, -0.1580373, -0.1580373, -0.…
    ## $ cln_c  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN…
    ## $ cln_g  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN…
    ## $ cln_w  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cln_m  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ cln_b  <dbl> NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN…
    ## $ civ    <dbl> 0.47372376, -0.95632807, 0.05312028, -1.78701995, 0.30548237, -…
    ## $ civ_c  <dbl> -0.3996126, -0.3996126, -0.3996126, -0.3996126, -0.3996126, -0.…
    ## $ civ_g  <dbl> -0.06120699, -0.06120699, -0.06120699, -0.06120699, -0.06120699…
    ## $ civ_w  <dbl> 0.87333639, -0.55671544, 0.45273291, -1.38740731, 0.70509500, 0…
    ## $ civ_m  <dbl> 0.5349307, -0.8951211, 0.1143273, -1.7258130, 0.3666894, -0.327…
    ## $ civ_b  <dbl> -0.3384056, -0.3384056, -0.3384056, -0.3384056, -0.3384056, -0.…
    ## $ civ1_c <dbl> -0.3996126, -0.3996126, -0.3996126, -0.3996126, -0.3996126, -0.…
    ## $ civ1_g <dbl> -0.06120699, -0.06120699, -0.06120699, -0.06120699, -0.06120699…
    ## $ civ1_w <dbl> 0.87333639, -0.55671544, 0.45273291, -1.38740731, 0.70509500, 0…
    ## $ civ1_m <dbl> 0.5349307, -0.8951211, 0.1143273, -1.7258130, 0.3666894, -0.327…
    ## $ civ1_b <dbl> -0.3384056, -0.3384056, -0.3384056, -0.3384056, -0.3384056, -0.…
    ## $ civ2_c <dbl> -0.3838400, -0.3838400, -0.3838400, -0.3838400, -0.3838400, -0.…
    ## $ civ2_g <dbl> -0.05902985, -0.05902985, -0.05902985, -0.05902985, -0.05902985…
    ## $ civ2_w <dbl> 0.67880728, 0.02687189, -0.29909581, -1.22442347, 0.83653359, 0…
    ## $ civ2_m <dbl> 0.35399713, -0.29793827, -0.62390596, -1.54923362, 0.51172343, …
    ## $ civ2_b <dbl> -0.3248102, -0.3248102, -0.3248102, -0.3248102, -0.3248102, -0.…
    ## $ civ3_c <dbl> -0.3820875, -0.3820875, -0.3820875, -0.3820875, -0.3820875, -0.…
    ## $ civ3_g <dbl> -0.06785588, -0.06785588, -0.06785588, -0.06785588, -0.06785588…
    ## $ civ3_w <dbl> 0.592934073, -0.069516409, 0.193360767, -1.310296676, 0.3300568…
    ## $ civ3_m <dbl> 0.27870246, -0.38374802, -0.12087084, -1.62452829, 0.01582529, …
    ## $ civ3_b <dbl> -0.3142316, -0.3142316, -0.3142316, -0.3142316, -0.3142316, -0.…
    ## $ civ4_c <dbl> -0.3353538, -0.3353538, -0.3353538, -0.3353538, -0.3353538, -0.…
    ## $ civ4_g <dbl> -0.06487674, -0.06487674, -0.06487674, -0.06487674, -0.06487674…
    ## $ civ4_w <dbl> 0.30435335, -0.37912730, -0.12676522, -1.79866405, 0.93525857, …
    ## $ civ4_m <dbl> 0.03387632, -0.64960433, -0.39724225, -2.06914108, 0.66478154, …
    ## $ civ4_b <dbl> -0.2704770, -0.2704770, -0.2704770, -0.2704770, -0.2704770, -0.…
    ## $ civ5_c <dbl> -0.3248387, -0.3248387, -0.3248387, -0.3248387, -0.3248387, -0.…
    ## $ civ5_g <dbl> -0.0557692, -0.0557692, -0.0557692, -0.0557692, -0.0557692, -0.…
    ## $ civ5_w <dbl> 1.1034999649, -0.5894290443, 0.3043533519, -1.4832114404, 1.292…
    ## $ civ5_m <dbl> 0.83443048, -0.85849853, 0.03528387, -1.75228092, 1.02370205, -…
    ## $ civ5_b <dbl> -0.2690695, -0.2690695, -0.2690695, -0.2690695, -0.2690695, -0.…
    ## $ aut_c  <dbl> 0.09828992, 0.09828992, 0.09828992, 0.09828992, 0.09828992, 0.0…
    ## $ aut_g  <dbl> -0.0000000000000001333542, -0.0000000000000001333542, -0.000000…
    ## $ aut_w  <dbl> -0.192120909, -0.550590301, 0.786991222, 0.909933795, -1.259519…
    ## $ aut_m  <dbl> -0.09383099, -0.45230038, 0.88528115, 1.00822372, -1.16122960, …
    ## $ aut_b  <dbl> 0.09828992, 0.09828992, 0.09828992, 0.09828992, 0.09828992, 0.0…
    ## $ gen_c  <dbl> -0.05562358, -0.05562358, -0.05562358, -0.05562358, -0.05562358…
    ## $ gen_g  <dbl> -0.00000000000000005106615, -0.00000000000000005106615, -0.0000…
    ## $ gen_w  <dbl> -0.8689672, 0.5525265, -0.6749269, -0.4568199, 0.5525265, 0.114…
    ## $ gen_m  <dbl> -0.92459074, 0.49690288, -0.73055044, -0.51244350, 0.49690288, …
    ## $ gen_b  <dbl> -0.05562358, -0.05562358, -0.05562358, -0.05562358, -0.05562358…
    ## $ k1     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ k2     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k3     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k4     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k5     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k6     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k7     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k8     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k9     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k10    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ k11    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

# Population Model

## Naive Model

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(aut, ses, civ, id_j) %>%
             na.omit()

# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
aut ~ c*ses # y <- x
aut ~ b*civ # y <- m

civ ~ a*ses # m <- x

# effects of interest
xm  := a
my  := b
xy  := c
tot := c + a*b
ind := a*b
dir := c
'

# -----------------------------------------------
# fit model
# -----------------------------------------------

m01 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09)

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m01, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 8 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                         7
    ## 
    ##   Number of observations                          5149
    ##   Number of missing patterns                         1
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              3339.049
    ##   Degrees of freedom                                 3
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -12326.077
    ##   Loglikelihood unrestricted model (H1)     -12326.077
    ##                                                       
    ##   Akaike (AIC)                               24666.154
    ##   Bayesian (BIC)                             24711.979
    ##   Sample-size adjusted Bayesian (BIC)        24689.736
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses        (c)   -0.008    0.013   -0.655    0.512   -0.008   -0.008
    ##     civ        (b)   -0.652    0.014  -45.509    0.000   -0.652   -0.579
    ##   civ ~                                                                 
    ##     ses        (a)    0.404    0.011   36.780    0.000    0.404    0.456
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut              -0.039    0.011   -3.396    0.001   -0.039   -0.039
    ##    .civ              -0.056    0.011   -5.096    0.000   -0.056   -0.063
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.660    0.013   50.740    0.000    0.660    0.660
    ##    .civ               0.624    0.012   50.740    0.000    0.624    0.792
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.340
    ##     civ               0.208
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm                0.404    0.011   36.780    0.000    0.404    0.456
    ##     my               -0.652    0.014  -45.509    0.000   -0.652   -0.579
    ##     xy               -0.008    0.013   -0.655    0.512   -0.008   -0.008
    ##     tot              -0.272    0.013  -20.321    0.000   -0.272   -0.272
    ##     ind              -0.264    0.009  -28.606    0.000   -0.264   -0.264
    ##     dir              -0.008    0.013   -0.655    0.512   -0.008   -0.008

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m01) %>%
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs    | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:-------|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm  | :=  | a      | xm    |  0.40 | 0.01 |  36.78 |   0.00 |     0.38 |     0.43 |
| my  | :=  | b      | my    | -0.65 | 0.01 | -45.51 |   0.00 |    -0.68 |    -0.62 |
| xy  | :=  | c      | xy    | -0.01 | 0.01 |  -0.66 |   0.51 |    -0.03 |     0.02 |
| tot | :=  | c+a\*b | tot   | -0.27 | 0.01 | -20.32 |   0.00 |    -0.30 |    -0.25 |
| ind | :=  | a\*b   | ind   | -0.26 | 0.01 | -28.61 |   0.00 |    -0.28 |    -0.25 |
| dir | :=  | c      | dir   | -0.01 | 0.01 |  -0.66 |   0.51 |    -0.03 |     0.02 |

## Clustered Errors

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(aut, ses, civ, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
aut ~ c*ses # y <- x
aut ~ b*civ # y <- m

civ ~ a*ses # m <- x

# effects of interest
xm  := a
my  := b
xy  := c
tot := c + a*b
ind := a*b
dir := c
'

# -----------------------------------------------
# fit model
# -----------------------------------------------

m02 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09,
       cluster = 'id_j') 

# Mote: last line requests clustered errors

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m02, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 8 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                         7
    ## 
    ##   Number of observations                          5149
    ##   Number of clusters [id_j]                        176
    ##   Number of missing patterns                         1
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 0.000       0.000
    ##   Degrees of freedom                                 0           0
    ##   Information                                 Observed            
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              3339.049          NA
    ##   Degrees of freedom                                 3           3
    ##   P-value                                        0.000          NA
    ##   Scaling correction factor                                     NA
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000          NA
    ##   Tucker-Lewis Index (TLI)                       1.000          NA
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -12326.077  -12326.077
    ##   Loglikelihood unrestricted model (H1)     -12326.077  -12326.077
    ##                                                                   
    ##   Akaike (AIC)                               24666.154   24666.154
    ##   Bayesian (BIC)                             24711.979   24711.979
    ##   Sample-size adjusted Bayesian (BIC)        24689.736   24689.736
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.000       0.000
    ##   P-value RMSEA <= 0.05                             NA          NA
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                        Robust.cluster
    ##   Information                                  Expected
    ##   Information saturated (h1) model           Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses        (c)   -0.008    0.016   -0.521    0.602   -0.008   -0.008
    ##     civ        (b)   -0.652    0.014  -45.619    0.000   -0.652   -0.579
    ##   civ ~                                                                 
    ##     ses        (a)    0.404    0.021   19.383    0.000    0.404    0.456
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut              -0.039    0.016   -2.338    0.019   -0.039   -0.039
    ##    .civ              -0.056    0.028   -2.009    0.045   -0.056   -0.063
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.660    0.021   31.051    0.000    0.660    0.660
    ##    .civ               0.624    0.020   31.604    0.000    0.624    0.792
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.340
    ##     civ               0.208
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm                0.404    0.021   19.383    0.000    0.404    0.456
    ##     my               -0.652    0.014  -45.619    0.000   -0.652   -0.579
    ##     xy               -0.008    0.016   -0.521    0.602   -0.008   -0.008
    ##     tot              -0.272    0.019  -14.555    0.000   -0.272   -0.272
    ##     ind              -0.264    0.014  -18.467    0.000   -0.264   -0.264
    ##     dir              -0.008    0.016   -0.521    0.602   -0.008   -0.008

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m02) %>%
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs    | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:-------|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm  | :=  | a      | xm    |  0.40 | 0.02 |  19.38 |    0.0 |     0.36 |     0.45 |
| my  | :=  | b      | my    | -0.65 | 0.01 | -45.62 |    0.0 |    -0.68 |    -0.62 |
| xy  | :=  | c      | xy    | -0.01 | 0.02 |  -0.52 |    0.6 |    -0.04 |     0.02 |
| tot | :=  | c+a\*b | tot   | -0.27 | 0.02 | -14.56 |    0.0 |    -0.31 |    -0.24 |
| ind | :=  | a\*b   | ind   | -0.26 | 0.01 | -18.47 |    0.0 |    -0.29 |    -0.24 |
| dir | :=  | c      | dir   | -0.01 | 0.02 |  -0.52 |    0.6 |    -0.04 |     0.02 |

## Bootstrapped errors

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(aut, ses, civ, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
aut ~ c*ses # y <- x
aut ~ b*civ # y <- m

civ ~ a*ses # m <- x

# effects of interest
xm  := a
my  := b
xy  := c
tot := c + a*b
ind := a*b
dir := c
'

# -----------------------------------------------
# fit model
# -----------------------------------------------

m03 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09,
       bootstrap = 5000,
       cluster = 'id_j')

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m03, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 8 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                         7
    ## 
    ##   Number of observations                          5149
    ##   Number of clusters [id_j]                        176
    ##   Number of missing patterns                         1
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 0.000       0.000
    ##   Degrees of freedom                                 0           0
    ##   Information                                 Observed            
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              3339.049          NA
    ##   Degrees of freedom                                 3           3
    ##   P-value                                        0.000          NA
    ##   Scaling correction factor                                     NA
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000          NA
    ##   Tucker-Lewis Index (TLI)                       1.000          NA
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -12326.077  -12326.077
    ##   Loglikelihood unrestricted model (H1)     -12326.077  -12326.077
    ##                                                                   
    ##   Akaike (AIC)                               24666.154   24666.154
    ##   Bayesian (BIC)                             24711.979   24711.979
    ##   Sample-size adjusted Bayesian (BIC)        24689.736   24689.736
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.000       0.000
    ##   P-value RMSEA <= 0.05                             NA          NA
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                        Robust.cluster
    ##   Information                                  Expected
    ##   Information saturated (h1) model           Structured
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses        (c)   -0.008    0.016   -0.521    0.602   -0.008   -0.008
    ##     civ        (b)   -0.652    0.014  -45.619    0.000   -0.652   -0.579
    ##   civ ~                                                                 
    ##     ses        (a)    0.404    0.021   19.383    0.000    0.404    0.456
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut              -0.039    0.016   -2.338    0.019   -0.039   -0.039
    ##    .civ              -0.056    0.028   -2.009    0.045   -0.056   -0.063
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.660    0.021   31.051    0.000    0.660    0.660
    ##    .civ               0.624    0.020   31.604    0.000    0.624    0.792
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.340
    ##     civ               0.208
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm                0.404    0.021   19.383    0.000    0.404    0.456
    ##     my               -0.652    0.014  -45.619    0.000   -0.652   -0.579
    ##     xy               -0.008    0.016   -0.521    0.602   -0.008   -0.008
    ##     tot              -0.272    0.019  -14.555    0.000   -0.272   -0.272
    ##     ind              -0.264    0.014  -18.467    0.000   -0.264   -0.264
    ##     dir              -0.008    0.016   -0.521    0.602   -0.008   -0.008

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m03,
boot.ci.type = "bca.simple") %>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs    | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:-------|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm  | :=  | a      | xm    |  0.40 | 0.02 |  19.38 |    0.0 |     0.36 |     0.45 |
| my  | :=  | b      | my    | -0.65 | 0.01 | -45.62 |    0.0 |    -0.68 |    -0.62 |
| xy  | :=  | c      | xy    | -0.01 | 0.02 |  -0.52 |    0.6 |    -0.04 |     0.02 |
| tot | :=  | c+a\*b | tot   | -0.27 | 0.02 | -14.56 |    0.0 |    -0.31 |    -0.24 |
| ind | :=  | a\*b   | ind   | -0.26 | 0.01 | -18.47 |    0.0 |    -0.29 |    -0.24 |
| dir | :=  | c      | dir   | -0.01 | 0.02 |  -0.52 |    0.6 |    -0.04 |     0.02 |

## Graded Response model

-   Can’t account for clustered errors simultaneously.

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(au1:au9, ses, civ, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
lau =~ 1*au1
lau =~ 1*au2
lau =~ 1*au3
lau =~ 1*au4
lau =~ 1*au5
lau =~ 1*au6
lau =~ 1*au7
lau =~ 1*au8
lau =~ 1*au9

lau~~var*lau

lau ~ c*ses # y <- x
lau ~ b*civ # y <- m

civ ~ a*ses # m <- x

# effects of interest
xm  := a
my  := b
xy  := c
tot := c + a*b
ind := a*b
dir := c
'

# -----------------------------------------------
# ordinal indicators
# -----------------------------------------------

aut_items <- data_ch09 %>%
             dplyr::select(au1:au9) %>%
             names()

# -----------------------------------------------
# fit model
# -----------------------------------------------

m04 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       estimator = 'WLSMV',
       data = data_ch09,
       ordered= aut_items,
       bootstrap = 5000
       )

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m04, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 21 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        33
    ## 
    ##   Number of observations                          5020
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              1645.904    1821.767
    ##   Degrees of freedom                                51          51
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.909
    ##   Shift parameter                                           10.441
    ##     simple second-order correction (WLSMV)                        
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             56645.440   30432.383
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.863
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.972       0.942
    ##   Tucker-Lewis Index (TLI)                       0.975       0.949
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.079       0.083
    ##   90 Percent confidence interval - lower         0.076       0.080
    ##   90 Percent confidence interval - upper         0.082       0.086
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.064       0.064
    ## 
    ## Weighted Root Mean Square Residual:
    ## 
    ##   WRMR                                           4.427       4.427
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   lau =~                                                                
    ##     au1               1.000                               0.700    0.684
    ##     au2               1.000                               0.700    0.684
    ##     au3               1.000                               0.700    0.684
    ##     au4               1.000                               0.700    0.684
    ##     au5               1.000                               0.700    0.684
    ##     au6               1.000                               0.700    0.684
    ##     au7               1.000                               0.700    0.684
    ##     au8               1.000                               0.700    0.684
    ##     au9               1.000                               0.700    0.684
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   lau ~                                                                 
    ##     ses        (c)   -0.004    0.010   -0.459    0.647   -0.006   -0.006
    ##     civ        (b)   -0.522    0.009  -58.911    0.000   -0.746   -0.658
    ##   civ ~                                                                 
    ##     ses        (a)    0.401    0.011   36.652    0.000    0.401    0.454
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .au1               0.000                               0.000    0.000
    ##    .au2               0.000                               0.000    0.000
    ##    .au3               0.000                               0.000    0.000
    ##    .au4               0.000                               0.000    0.000
    ##    .au5               0.000                               0.000    0.000
    ##    .au6               0.000                               0.000    0.000
    ##    .au7               0.000                               0.000    0.000
    ##    .au8               0.000                               0.000    0.000
    ##    .au9               0.000                               0.000    0.000
    ##    .civ              -0.043    0.011   -3.897    0.000   -0.043   -0.049
    ##    .lau               0.000                               0.000    0.000
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     au1|t1            0.066    0.017    3.898    0.000    0.066    0.064
    ##     au1|t2            1.100    0.021   53.014    0.000    1.100    1.076
    ##     au1|t3            1.655    0.028   58.195    0.000    1.655    1.619
    ##     au2|t1           -0.197    0.017  -11.670    0.000   -0.197   -0.193
    ##     au2|t2            0.771    0.019   41.411    0.000    0.771    0.754
    ##     au2|t3            1.632    0.028   58.688    0.000    1.632    1.596
    ##     au3|t1           -0.768    0.019  -39.720    0.000   -0.768   -0.751
    ##     au3|t2            0.265    0.017   15.579    0.000    0.265    0.259
    ##     au3|t3            1.342    0.024   55.994    0.000    1.342    1.313
    ##     au4|t1           -0.105    0.017   -6.168    0.000   -0.105   -0.102
    ##     au4|t2            1.120    0.021   52.700    0.000    1.120    1.095
    ##     au4|t3            1.757    0.031   57.591    0.000    1.757    1.718
    ##     au5|t1           -0.873    0.020  -43.398    0.000   -0.873   -0.854
    ##     au5|t2           -0.037    0.017   -2.153    0.031   -0.037   -0.036
    ##     au5|t3            0.833    0.019   42.926    0.000    0.833    0.815
    ##     au6|t1           -0.327    0.018  -18.634    0.000   -0.327   -0.320
    ##     au6|t2            0.590    0.018   32.942    0.000    0.590    0.577
    ##     au6|t3            1.392    0.025   56.330    0.000    1.392    1.362
    ##     au7|t1           -1.060    0.022  -49.048    0.000   -1.060   -1.036
    ##     au7|t2            0.053    0.017    3.164    0.002    0.053    0.052
    ##     au7|t3            1.233    0.023   54.615    0.000    1.233    1.205
    ##     au8|t1           -0.235    0.017  -13.922    0.000   -0.235   -0.230
    ##     au8|t2            0.909    0.019   47.152    0.000    0.909    0.889
    ##     au8|t3            1.703    0.029   57.804    0.000    1.703    1.665
    ##     au9|t1           -0.634    0.019  -34.013    0.000   -0.634   -0.620
    ##     au9|t2            0.567    0.018   32.080    0.000    0.567    0.554
    ##     au9|t3            1.434    0.025   57.262    0.000    1.434    1.402
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .lau      (var)    0.275    0.006   47.215    0.000    0.563    0.563
    ##    .au1               0.556                               0.556    0.532
    ##    .au2               0.556                               0.556    0.532
    ##    .au3               0.556                               0.556    0.532
    ##    .au4               0.556                               0.556    0.532
    ##    .au5               0.556                               0.556    0.532
    ##    .au6               0.556                               0.556    0.532
    ##    .au7               0.556                               0.556    0.532
    ##    .au8               0.556                               0.556    0.532
    ##    .au9               0.556                               0.556    0.532
    ##    .civ               0.619    0.013   48.383    0.000    0.619    0.794
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     au1               1.000                               1.000    1.000
    ##     au2               1.000                               1.000    1.000
    ##     au3               1.000                               1.000    1.000
    ##     au4               1.000                               1.000    1.000
    ##     au5               1.000                               1.000    1.000
    ##     au6               1.000                               1.000    1.000
    ##     au7               1.000                               1.000    1.000
    ##     au8               1.000                               1.000    1.000
    ##     au9               1.000                               1.000    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     lau               0.437
    ##     au1               0.468
    ##     au2               0.468
    ##     au3               0.468
    ##     au4               0.468
    ##     au5               0.468
    ##     au6               0.468
    ##     au7               0.468
    ##     au8               0.468
    ##     au9               0.468
    ##     civ               0.206
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm                0.401    0.011   36.652    0.000    0.401    0.454
    ##     my               -0.522    0.009  -58.911    0.000   -0.746   -0.658
    ##     xy               -0.004    0.010   -0.459    0.647   -0.006   -0.006
    ##     tot              -0.213    0.010  -20.643    0.000   -0.305   -0.305
    ##     ind              -0.209    0.007  -31.570    0.000   -0.299   -0.299
    ##     dir              -0.004    0.010   -0.459    0.647   -0.006   -0.006

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m04,
boot.ci.type = "bca.simple") %>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs    | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:-------|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm  | :=  | a      | xm    |  0.40 | 0.01 |  36.65 |   0.00 |     0.38 |     0.42 |
| my  | :=  | b      | my    | -0.52 | 0.01 | -58.91 |   0.00 |    -0.54 |    -0.50 |
| xy  | :=  | c      | xy    |  0.00 | 0.01 |  -0.46 |   0.65 |    -0.02 |     0.01 |
| tot | :=  | c+a\*b | tot   | -0.21 | 0.01 | -20.64 |   0.00 |    -0.23 |    -0.19 |
| ind | :=  | a\*b   | ind   | -0.21 | 0.01 | -31.57 |   0.00 |    -0.22 |    -0.20 |
| dir | :=  | c      | dir   |  0.00 | 0.01 |  -0.46 |   0.65 |    -0.02 |     0.01 |

## Graded Response model (2pl)

-   Can’t account for clustered errors simultaneously.

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(au1:au9, ses, civ, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
lau =~ NA*au1
lau =~ l2*au2
lau =~ l3*au3
lau =~ l4*au4
lau =~ l5*au5
lau =~ l6*au6
lau =~ l7*au7
lau =~ l8*au8
lau =~ l9*au9

lau~~1*lau

lau ~ c*ses # y <- x
lau ~ b*civ # y <- m

civ ~ a*ses # m <- x

# effects of interest
xm  := a
my  := b
xy  := c
tot := c + a*b
ind := a*b
dir := c
'

# -----------------------------------------------
# ordinal indicators
# -----------------------------------------------

aut_items <- data_ch09 %>%
             dplyr::select(au1:au9) %>%
             names()

# -----------------------------------------------
# fit model
# -----------------------------------------------

m04 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       estimator = 'WLSMV',
       data = data_ch09,
       ordered= aut_items,
       bootstrap = 5000
       )

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m04, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 30 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        41
    ## 
    ##   Number of observations                          5020
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                               408.823     755.442
    ##   Degrees of freedom                                43          43
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.543
    ##   Shift parameter                                            2.140
    ##     simple second-order correction (WLSMV)                        
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             56645.440   30432.383
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.863
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.994       0.977
    ##   Tucker-Lewis Index (TLI)                       0.993       0.975
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.041       0.057
    ##   90 Percent confidence interval - lower         0.038       0.054
    ##   90 Percent confidence interval - upper         0.045       0.061
    ##   P-value RMSEA <= 0.05                          1.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.032       0.032
    ## 
    ## Weighted Root Mean Square Residual:
    ## 
    ##   WRMR                                           2.206       2.206
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   lau =~                                                                
    ##     au1               0.594    0.008   70.030    0.000    0.795    0.772
    ##     au2       (l2)    0.568    0.008   67.493    0.000    0.761    0.741
    ##     au3       (l3)    0.433    0.009   46.486    0.000    0.580    0.571
    ##     au4       (l4)    0.560    0.008   66.571    0.000    0.750    0.731
    ##     au5       (l5)    0.433    0.009   46.588    0.000    0.579    0.570
    ##     au6       (l6)    0.503    0.009   57.634    0.000    0.674    0.660
    ##     au7       (l7)    0.477    0.009   55.393    0.000    0.638    0.627
    ##     au8       (l8)    0.590    0.008   72.752    0.000    0.790    0.768
    ##     au9       (l9)    0.512    0.008   61.590    0.000    0.686    0.671
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   lau ~                                                                 
    ##     ses        (c)   -0.006    0.018   -0.351    0.725   -0.005   -0.005
    ##     civ        (b)   -1.004    0.024  -41.959    0.000   -0.750   -0.662
    ##   civ ~                                                                 
    ##     ses        (a)    0.401    0.011   36.652    0.000    0.401    0.454
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .au1               0.000                               0.000    0.000
    ##    .au2               0.000                               0.000    0.000
    ##    .au3               0.000                               0.000    0.000
    ##    .au4               0.000                               0.000    0.000
    ##    .au5               0.000                               0.000    0.000
    ##    .au6               0.000                               0.000    0.000
    ##    .au7               0.000                               0.000    0.000
    ##    .au8               0.000                               0.000    0.000
    ##    .au9               0.000                               0.000    0.000
    ##    .civ              -0.043    0.011   -3.897    0.000   -0.043   -0.049
    ##    .lau               0.000                               0.000    0.000
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     au1|t1            0.069    0.017    4.079    0.000    0.069    0.067
    ##     au1|t2            1.103    0.021   53.360    0.000    1.103    1.072
    ##     au1|t3            1.658    0.028   58.402    0.000    1.658    1.611
    ##     au2|t1           -0.195    0.017  -11.532    0.000   -0.195   -0.190
    ##     au2|t2            0.773    0.019   41.602    0.000    0.773    0.753
    ##     au2|t3            1.634    0.028   58.879    0.000    1.634    1.592
    ##     au3|t1           -0.772    0.019  -39.881    0.000   -0.772   -0.760
    ##     au3|t2            0.261    0.017   15.305    0.000    0.261    0.257
    ##     au3|t3            1.339    0.024   56.061    0.000    1.339    1.318
    ##     au4|t1           -0.103    0.017   -6.044    0.000   -0.103   -0.100
    ##     au4|t2            1.122    0.021   52.923    0.000    1.122    1.093
    ##     au4|t3            1.758    0.030   57.761    0.000    1.758    1.714
    ##     au5|t1           -0.877    0.020  -43.581    0.000   -0.877   -0.864
    ##     au5|t2           -0.040    0.017   -2.372    0.018   -0.040   -0.040
    ##     au5|t3            0.830    0.019   42.885    0.000    0.830    0.817
    ##     au6|t1           -0.328    0.018  -18.629    0.000   -0.328   -0.321
    ##     au6|t2            0.589    0.018   32.922    0.000    0.589    0.577
    ##     au6|t3            1.392    0.025   56.463    0.000    1.392    1.363
    ##     au7|t1           -1.062    0.022  -49.072    0.000   -1.062   -1.042
    ##     au7|t2            0.051    0.017    3.051    0.002    0.051    0.051
    ##     au7|t3            1.231    0.023   54.692    0.000    1.231    1.208
    ##     au8|t1           -0.232    0.017  -13.719    0.000   -0.232   -0.225
    ##     au8|t2            0.912    0.019   47.514    0.000    0.912    0.886
    ##     au8|t3            1.706    0.029   58.011    0.000    1.706    1.658
    ##     au9|t1           -0.635    0.019  -33.963    0.000   -0.635   -0.621
    ##     au9|t2            0.567    0.018   32.085    0.000    0.567    0.554
    ##     au9|t3            1.433    0.025   57.390    0.000    1.433    1.403
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .lau               1.000                               0.558    0.558
    ##    .au1               0.427                               0.427    0.403
    ##    .au2               0.475                               0.475    0.451
    ##    .au3               0.696                               0.696    0.674
    ##    .au4               0.490                               0.490    0.465
    ##    .au5               0.696                               0.696    0.675
    ##    .au6               0.588                               0.588    0.564
    ##    .au7               0.631                               0.631    0.607
    ##    .au8               0.434                               0.434    0.410
    ##    .au9               0.573                               0.573    0.549
    ##    .civ               0.619    0.013   48.383    0.000    0.619    0.794
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     au1               1.000                               1.000    1.000
    ##     au2               1.000                               1.000    1.000
    ##     au3               1.000                               1.000    1.000
    ##     au4               1.000                               1.000    1.000
    ##     au5               1.000                               1.000    1.000
    ##     au6               1.000                               1.000    1.000
    ##     au7               1.000                               1.000    1.000
    ##     au8               1.000                               1.000    1.000
    ##     au9               1.000                               1.000    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     lau               0.442
    ##     au1               0.597
    ##     au2               0.549
    ##     au3               0.326
    ##     au4               0.535
    ##     au5               0.325
    ##     au6               0.436
    ##     au7               0.393
    ##     au8               0.590
    ##     au9               0.451
    ##     civ               0.206
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm                0.401    0.011   36.652    0.000    0.401    0.454
    ##     my               -1.004    0.024  -41.959    0.000   -0.750   -0.662
    ##     xy               -0.006    0.018   -0.351    0.725   -0.005   -0.005
    ##     tot              -0.409    0.020  -19.970    0.000   -0.306   -0.306
    ##     ind              -0.403    0.015  -27.241    0.000   -0.301   -0.301
    ##     dir              -0.006    0.018   -0.351    0.725   -0.005   -0.005

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m04,
boot.ci.type = "bca.simple") %>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs    | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:-------|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm  | :=  | a      | xm    |  0.40 | 0.01 |  36.65 |   0.00 |     0.38 |     0.42 |
| my  | :=  | b      | my    | -1.00 | 0.02 | -41.96 |   0.00 |    -1.05 |    -0.96 |
| xy  | :=  | c      | xy    | -0.01 | 0.02 |  -0.35 |   0.73 |    -0.04 |     0.03 |
| tot | :=  | c+a\*b | tot   | -0.41 | 0.02 | -19.97 |   0.00 |    -0.45 |    -0.37 |
| ind | :=  | a\*b   | ind   | -0.40 | 0.01 | -27.24 |   0.00 |    -0.43 |    -0.37 |
| dir | :=  | c      | dir   | -0.01 | 0.02 |  -0.35 |   0.73 |    -0.04 |     0.03 |

# Multigroup Model

## Free parameters

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------


data_ch09_mg <- data_aut %>%
                dplyr::filter(cty %in% c('CHL')) %>%
                dplyr::filter(year == 2009) %>%
                dplyr::select(aut, ses, civ, inx, id_j) %>%
                na.omit()

# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
aut ~ c(c1,c2)*ses # y <- x
aut ~ c(b1,b2)*civ # y <- m

civ ~ c(a1,a2)*ses # m <- x

# free intercepts
aut ~ c(i1, i2)*1
civ ~ c(i3, i4)*1


# indirect effects g1
tot1 := c1 + a1*b1
ind1 := a1*b1
dir1 := c1

## indirect effects g2
tot2 := c2 + a2*b2
ind2 := a2*b2
dir2 := c2
'


# -----------------------------------------------
# fit model
# -----------------------------------------------

m05 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09_mg,
       group = 'inx',
       cluster = 'id_j')

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m05, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 14 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ## 
    ##   Number of observations per group:                   
    ##     1                                             2924
    ##     0                                             2161
    ##   Number of clusters [id_j]:                          
    ##     1                                              174
    ##     0                                              174
    ##   Number of missing patterns per group:               
    ##     1                                                1
    ##     0                                                1
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                 0.000       0.000
    ##   Degrees of freedom                                 0           0
    ##   Information                                 Observed            
    ##   Test statistic for each group:
    ##     1                                            0.000       0.000
    ##     0                                            0.000       0.000
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              3180.614   21691.871
    ##   Degrees of freedom                                 6           6
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  0.147
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000       1.000
    ##   Tucker-Lewis Index (TLI)                       1.000       1.000
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -12118.662  -12118.662
    ##   Loglikelihood unrestricted model (H1)     -12118.662  -12118.662
    ##                                                                   
    ##   Akaike (AIC)                               24265.324   24265.324
    ##   Bayesian (BIC)                             24356.800   24356.800
    ##   Sample-size adjusted Bayesian (BIC)        24312.313   24312.313
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000       0.000
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.000       0.000
    ##   P-value RMSEA <= 0.05                             NA          NA
    ##                                                                   
    ##   Robust RMSEA                                               0.000
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.000       0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                        Robust.cluster
    ##   Information                                  Expected
    ##   Information saturated (h1) model           Structured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses       (c1)   -0.002    0.020   -0.095    0.924   -0.002   -0.002
    ##     civ       (b1)   -0.685    0.019  -36.221    0.000   -0.685   -0.603
    ##   civ ~                                                                 
    ##     ses       (a1)    0.424    0.022   19.304    0.000    0.424    0.484
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut       (i1)   -0.009    0.019   -0.475    0.635   -0.009   -0.009
    ##    .civ       (i3)   -0.005    0.028   -0.178    0.859   -0.005   -0.006
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.645    0.026   24.454    0.000    0.645    0.635
    ##    .civ               0.603    0.022   27.523    0.000    0.603    0.766
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.365
    ##     civ               0.234
    ## 
    ## 
    ## Group 2 [0]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses       (c2)   -0.022    0.023   -0.969    0.333   -0.022   -0.021
    ##     civ       (b2)   -0.625    0.022  -28.989    0.000   -0.625   -0.545
    ##   civ ~                                                                 
    ##     ses       (a2)    0.330    0.029   11.560    0.000    0.330    0.352
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut       (i2)   -0.070    0.024   -2.973    0.003   -0.070   -0.072
    ##    .civ       (i4)   -0.141    0.034   -4.141    0.000   -0.141   -0.165
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.663    0.032   20.721    0.000    0.663    0.695
    ##    .civ               0.636    0.027   23.470    0.000    0.636    0.876
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.305
    ##     civ               0.124
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     tot1             -0.292    0.022  -13.233    0.000   -0.292   -0.294
    ##     ind1             -0.290    0.016  -18.046    0.000   -0.290   -0.292
    ##     dir1             -0.002    0.020   -0.095    0.924   -0.002   -0.002
    ##     tot2             -0.228    0.028   -8.117    0.000   -0.228   -0.213
    ##     ind2             -0.206    0.020  -10.547    0.000   -0.206   -0.192
    ##     dir2             -0.022    0.023   -0.969    0.333   -0.022   -0.021

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m05)%>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs  | op  | rhs       | block | group | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:-----|:----|:----------|------:|------:|:------|------:|-----:|-------:|-------:|---------:|---------:|
| tot1 | :=  | c1+a1\*b1 |     0 |     0 | tot1  | -0.29 | 0.02 | -13.23 |   0.00 |    -0.34 |    -0.25 |
| ind1 | :=  | a1\*b1    |     0 |     0 | ind1  | -0.29 | 0.02 | -18.05 |   0.00 |    -0.32 |    -0.26 |
| dir1 | :=  | c1        |     0 |     0 | dir1  |  0.00 | 0.02 |  -0.10 |   0.92 |    -0.04 |     0.04 |
| tot2 | :=  | c2+a2\*b2 |     0 |     0 | tot2  | -0.23 | 0.03 |  -8.12 |   0.00 |    -0.28 |    -0.17 |
| ind2 | :=  | a2\*b2    |     0 |     0 | ind2  | -0.21 | 0.02 | -10.55 |   0.00 |    -0.24 |    -0.17 |
| dir2 | :=  | c2        |     0 |     0 | dir2  | -0.02 | 0.02 |  -0.97 |   0.33 |    -0.07 |     0.02 |

``` r
lavaan::parameterEstimates(m05)%>% 
dplyr::filter(label %in% c('a1','a2')) %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs | block | group | label |  est |   se |     z | pvalue | ci.lower | ci.upper |
|:----|:----|:----|------:|------:|:------|-----:|-----:|------:|-------:|---------:|---------:|
| civ | \~  | ses |     1 |     1 | a1    | 0.42 | 0.02 | 19.30 |      0 |     0.38 |     0.47 |
| civ | \~  | ses |     2 |     2 | a2    | 0.33 | 0.03 | 11.56 |      0 |     0.27 |     0.39 |

``` r
lavaan::parameterEstimates(m05)%>% 
dplyr::filter(label %in% c('b1','b2')) %>%
knitr::kable(., digits = 2)
```

| lhs | op  | rhs | block | group | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:----|:----|:----|------:|------:|:------|------:|-----:|-------:|-------:|---------:|---------:|
| aut | \~  | civ |     1 |     1 | b1    | -0.69 | 0.02 | -36.22 |      0 |    -0.72 |    -0.65 |
| aut | \~  | civ |     2 |     2 | b2    | -0.63 | 0.02 | -28.99 |      0 |    -0.67 |    -0.58 |

``` r
lavaan::parameterEstimates(m05)%>% 
dplyr::filter(label %in% c('tot1','tot2')) %>%
knitr::kable(., digits = 2)
```

| lhs  | op  | rhs       | block | group | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:-----|:----|:----------|------:|------:|:------|------:|-----:|-------:|-------:|---------:|---------:|
| tot1 | :=  | c1+a1\*b1 |     0 |     0 | tot1  | -0.29 | 0.02 | -13.23 |      0 |    -0.34 |    -0.25 |
| tot2 | :=  | c2+a2\*b2 |     0 |     0 | tot2  | -0.23 | 0.03 |  -8.12 |      0 |    -0.28 |    -0.17 |

## Constrained direct and second stage parameters

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------


data_ch09_mg <- data_aut %>%
                dplyr::filter(cty %in% c('CHL')) %>%
                dplyr::filter(year == 2009) %>%
                dplyr::select(aut, ses, civ, inx, id_j) %>%
                na.omit()

# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
aut ~ c(c1,c1)*ses # y <- x
aut ~ c(b1,b1)*civ # y <- m

civ ~ c(a1,a2)*ses # m <- x

# free intercepts
aut ~ c(i1, i2)*1
civ ~ c(i3, i4)*1


'


# -----------------------------------------------
# fit model
# -----------------------------------------------

m05c <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09_mg,
       group = 'inx',
       cluster = 'id_j')

# -----------------------------------------------
# likelihood ratio test
# -----------------------------------------------

lavaan::lavTestLRT(m05, m05c, method = 'satorra.bentler.2001')
```

    ## Scaled Chi-Squared Difference Test (method = "satorra.bentler.2001")
    ## 
    ## lavaan NOTE:
    ##     The "Chisq" column contains standard test statistics, not the
    ##     robust test that should be reported per model. A robust difference
    ##     test is a function of two standard (not robust) statistics.
    ##  
    ##      Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)
    ## m05   0 24265 24357 0.0000                              
    ## m05c  2 24266 24344 4.2715     3.6471       2     0.1615

``` r
# -----------------------------------------------
# conclusion
# -----------------------------------------------

# Note: first stage moderation. 
#       Family's Political interested accelerates
#       the effect of socioeconomic level on civic knowledge.
```

# Multilevel Mediation

## Multilevel latent covariate

-   1-1-1 model (unconflated MLM)
-   Preacher, K. J., Zyphur, M. J., & Zhang, Z. (2010). A general
    multilevel SEM framework for assessing multilevel mediation.
    Psychological Methods, 15(3), 209–233.
    <https://doi.org/10.1037/a0020141>
-   [download
    link](https://www.dropbox.com/s/gxa0hj0o61fu7tg/Preacher%2C%20Zyphur%2C%20Zhang_2010.pdf?dl=1)

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch09 <- data_aut %>%
             dplyr::filter(cty %in% c('CHL')) %>%
             dplyr::filter(year == 2009) %>%
             dplyr::select(aut, ses, civ, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
level: 1
aut ~ c1*ses # y <- x
aut ~ b1*civ # y <- m
civ ~ a1*ses # m <- x

level: 2
aut ~ c2*ses # y <- x
aut ~ b2*civ # y <- m
civ ~ a2*ses # m <- x

## L1 effects
xm1  := a1
my1  := b1
xy1  := c1
tot1 := c1 + a1*b1
ind1 := a1*b1
dir1 := c1

## L2 effects
xm2  := a2
my2  := b2
xy2  := c2
tot2 := c2 + a2*b2
ind2 := a2*b2
dir2 := c2
'


# -----------------------------------------------
# fit model
# -----------------------------------------------

m04 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch09,
       cluster = 'id_j')

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m04, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 30 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ## 
    ##   Number of observations                          5149
    ##   Number of clusters [id_j]                        176
    ##   Number of missing patterns -- level 1              1
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 0.000
    ##   Degrees of freedom                                 0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2018.838
    ##   Degrees of freedom                                 6
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -17599.585
    ##   Loglikelihood unrestricted model (H1)     -17599.585
    ##                                                       
    ##   Akaike (AIC)                               35223.170
    ##   Bayesian (BIC)                             35301.728
    ##   Sample-size adjusted Bayesian (BIC)        35263.597
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual (corr metric):
    ## 
    ##   SRMR (within covariance matrix)                0.000
    ##   SRMR (between covariance matrix)               0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ## 
    ## 
    ## Level 1 [within]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses       (c1)   -0.016    0.016   -0.995    0.320   -0.016   -0.012
    ##     civ       (b1)   -0.664    0.016  -40.759    0.000   -0.664   -0.504
    ##   civ ~                                                                 
    ##     ses       (a1)    0.143    0.014   10.277    0.000    0.143    0.144
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.000                               0.000    0.000
    ##    .civ               0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.636    0.013   49.895    0.000    0.636    0.744
    ##    .civ               0.482    0.010   49.853    0.000    0.482    0.979
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.256
    ##     civ               0.021
    ## 
    ## 
    ## Level 2 [id_j]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses       (c2)   -0.022    0.051   -0.421    0.673   -0.022   -0.042
    ##     civ       (b2)   -0.610    0.069   -8.903    0.000   -0.610   -0.882
    ##   civ ~                                                                 
    ##     ses       (a2)    0.656    0.033   20.150    0.000    0.656    0.875
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut              -0.039    0.017   -2.281    0.023   -0.039   -0.099
    ##    .civ              -0.069    0.024   -2.917    0.004   -0.069   -0.122
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.024    0.005    4.808    0.000    0.024    0.155
    ##    .civ               0.076    0.011    7.062    0.000    0.076    0.235
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.845
    ##     civ               0.765
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     xm1               0.143    0.014   10.277    0.000    0.143    0.144
    ##     my1              -0.664    0.016  -40.759    0.000   -0.664   -0.504
    ##     xy1              -0.016    0.016   -0.995    0.320   -0.016   -0.012
    ##     tot1             -0.111    0.018   -6.014    0.000   -0.111   -0.085
    ##     ind1             -0.095    0.010   -9.966    0.000   -0.095   -0.073
    ##     dir1             -0.016    0.016   -0.995    0.320   -0.016   -0.012
    ##     xm2               0.656    0.033   20.150    0.000    0.656    0.875
    ##     my2              -0.610    0.069   -8.903    0.000   -0.610   -0.882
    ##     xy2              -0.022    0.051   -0.421    0.673   -0.022   -0.042
    ##     tot2             -0.422    0.030  -13.848    0.000   -0.422   -0.814
    ##     ind2             -0.400    0.049   -8.183    0.000   -0.400   -0.772
    ##     dir2             -0.022    0.051   -0.421    0.673   -0.022   -0.042

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m04)%>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs  | op  | rhs       | block | level | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:-----|:----|:----------|------:|------:|:------|------:|-----:|-------:|-------:|---------:|---------:|
| xm1  | :=  | a1        |     0 |     0 | xm1   |  0.14 | 0.01 |  10.28 |   0.00 |     0.12 |     0.17 |
| my1  | :=  | b1        |     0 |     0 | my1   | -0.66 | 0.02 | -40.76 |   0.00 |    -0.70 |    -0.63 |
| xy1  | :=  | c1        |     0 |     0 | xy1   | -0.02 | 0.02 |  -0.99 |   0.32 |    -0.05 |     0.02 |
| tot1 | :=  | c1+a1\*b1 |     0 |     0 | tot1  | -0.11 | 0.02 |  -6.01 |   0.00 |    -0.15 |    -0.07 |
| ind1 | :=  | a1\*b1    |     0 |     0 | ind1  | -0.10 | 0.01 |  -9.97 |   0.00 |    -0.11 |    -0.08 |
| dir1 | :=  | c1        |     0 |     0 | dir1  | -0.02 | 0.02 |  -0.99 |   0.32 |    -0.05 |     0.02 |
| xm2  | :=  | a2        |     0 |     0 | xm2   |  0.66 | 0.03 |  20.15 |   0.00 |     0.59 |     0.72 |
| my2  | :=  | b2        |     0 |     0 | my2   | -0.61 | 0.07 |  -8.90 |   0.00 |    -0.74 |    -0.48 |
| xy2  | :=  | c2        |     0 |     0 | xy2   | -0.02 | 0.05 |  -0.42 |   0.67 |    -0.12 |     0.08 |
| tot2 | :=  | c2+a2\*b2 |     0 |     0 | tot2  | -0.42 | 0.03 | -13.85 |   0.00 |    -0.48 |    -0.36 |
| ind2 | :=  | a2\*b2    |     0 |     0 | ind2  | -0.40 | 0.05 |  -8.18 |   0.00 |    -0.50 |    -0.30 |
| dir2 | :=  | c2        |     0 |     0 | dir2  | -0.02 | 0.05 |  -0.42 |   0.67 |    -0.12 |     0.08 |

## Multilevel (1-2-1-1) Open Classroom Discussion

``` r
# -------------------------------------------------------------------
# lavaan
# -------------------------------------------------------------------

# -----------------------------------------------
# prep data
# -----------------------------------------------

data_ch16 <- data_aut %>%
             dplyr::filter(year == 2016) %>%
             dplyr::select(aut, ses, civ, opd, cln, id_j) %>%
             na.omit()


# -----------------------------------------------
# define model
# -----------------------------------------------

lavaan_model <-'
level: 1
aut ~ c11*ses
aut ~ b11*civ
aut ~ e11*opd

civ ~ a11*ses
civ ~ d11*opd

opd ~ f11*ses


level: 2
aut ~ c21*ses
aut ~ b21*civ
aut ~ e21*opd

civ ~ a21*ses
civ ~ d21*opd

opd ~ f21*ses


## indirect effects 2
ind1 := f21*e21
ind2 := f21*d21*b21
ind3 := a21*b21
dir2 := c21
tot2 := c21 + ind1 + ind2 + ind3

'


# -----------------------------------------------
# fit model
# -----------------------------------------------

m04 <- lavaan::sem(lavaan_model, 
       mimic = 'MPLUS',
       data = data_ch16,
       cluster = 'id_j')

# -----------------------------------------------
# display summary
# -----------------------------------------------

lavaan::summary(m04, 
  fit.measures=TRUE,
  standardized=TRUE,
  rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 52 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        21
    ## 
    ##   Number of observations                          4916
    ##   Number of clusters [id_j]                        178
    ##   Number of missing patterns -- level 1              1
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                                 0.001
    ##   Degrees of freedom                                 0
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1759.945
    ##   Degrees of freedom                                12
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    1.000
    ##   Tucker-Lewis Index (TLI)                       1.000
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)             -24202.325
    ##   Loglikelihood unrestricted model (H1)     -24202.324
    ##                                                       
    ##   Akaike (AIC)                               48446.650
    ##   Bayesian (BIC)                             48583.155
    ##   Sample-size adjusted Bayesian (BIC)        48516.425
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.000
    ##   90 Percent confidence interval - lower         0.000
    ##   90 Percent confidence interval - upper         0.000
    ##   P-value RMSEA <= 0.05                             NA
    ## 
    ## Standardized Root Mean Square Residual (corr metric):
    ## 
    ##   SRMR (within covariance matrix)                0.000
    ##   SRMR (between covariance matrix)               0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Observed
    ##   Observed information based on                Hessian
    ## 
    ## 
    ## Level 1 [within]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses      (c11)   -0.039    0.017   -2.292    0.022   -0.039   -0.030
    ##     civ      (b11)   -0.528    0.016  -32.988    0.000   -0.528   -0.441
    ##     opd      (e11)    0.011    0.013    0.828    0.408    0.011    0.011
    ##   civ ~                                                                 
    ##     ses      (a11)    0.150    0.015    9.755    0.000    0.150    0.138
    ##     opd      (d11)    0.142    0.011   12.415    0.000    0.142    0.176
    ##   opd ~                                                                 
    ##     ses      (f11)    0.048    0.019    2.450    0.014    0.048    0.036
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.000                               0.000    0.000
    ##    .civ               0.000                               0.000    0.000
    ##    .opd               0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.681    0.014   48.678    0.000    0.681    0.802
    ##    .civ               0.560    0.012   48.659    0.000    0.560    0.948
    ##    .opd               0.903    0.019   48.669    0.000    0.903    0.999
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.198
    ##     civ               0.052
    ##     opd               0.001
    ## 
    ## 
    ## Level 2 [id_j]:
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   aut ~                                                                 
    ##     ses      (c21)   -0.023    0.053   -0.429    0.668   -0.023   -0.043
    ##     civ      (b21)   -0.595    0.076   -7.845    0.000   -0.595   -0.877
    ##     opd      (e21)   -0.022    0.081   -0.274    0.784   -0.022   -0.017
    ##   civ ~                                                                 
    ##     ses      (a21)    0.599    0.035   16.945    0.000    0.599    0.775
    ##     opd      (d21)    0.506    0.098    5.159    0.000    0.506    0.269
    ##   opd ~                                                                 
    ##     ses      (f21)    0.113    0.037    3.088    0.002    0.113    0.275
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut              -0.046    0.018   -2.544    0.011   -0.046   -0.118
    ##    .civ              -0.094    0.024   -3.966    0.000   -0.094   -0.165
    ##    .opd              -0.010    0.026   -0.398    0.691   -0.010   -0.034
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .aut               0.022    0.005    4.331    0.000    0.022    0.149
    ##    .civ               0.069    0.011    6.452    0.000    0.069    0.212
    ##    .opd               0.085    0.013    6.612    0.000    0.085    0.924
    ## 
    ## R-Square:
    ##                    Estimate
    ##     aut               0.851
    ##     civ               0.788
    ##     opd               0.076
    ## 
    ## Defined Parameters:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##     ind1             -0.003    0.009   -0.273    0.785   -0.003   -0.005
    ##     ind2             -0.034    0.013   -2.583    0.010   -0.034   -0.065
    ##     ind3             -0.357    0.050   -7.086    0.000   -0.357   -0.680
    ##     dir2             -0.023    0.053   -0.429    0.668   -0.023   -0.043
    ##     tot2             -0.416    0.032  -13.072    0.000   -0.416   -0.793

``` r
#------------------------------------------------
# indirect effects
#------------------------------------------------

lavaan::parameterEstimates(m04)%>% 
dplyr::filter(op == ':=') %>%
knitr::kable(., digits = 2)
```

| lhs  | op  | rhs                | block | level | label |   est |   se |      z | pvalue | ci.lower | ci.upper |
|:-----|:----|:-------------------|------:|------:|:------|------:|-----:|-------:|-------:|---------:|---------:|
| ind1 | :=  | f21\*e21           |     0 |     0 | ind1  |  0.00 | 0.01 |  -0.27 |   0.79 |    -0.02 |     0.02 |
| ind2 | :=  | f21*d21*b21        |     0 |     0 | ind2  | -0.03 | 0.01 |  -2.58 |   0.01 |    -0.06 |    -0.01 |
| ind3 | :=  | a21\*b21           |     0 |     0 | ind3  | -0.36 | 0.05 |  -7.09 |   0.00 |    -0.46 |    -0.26 |
| dir2 | :=  | c21                |     0 |     0 | dir2  | -0.02 | 0.05 |  -0.43 |   0.67 |    -0.13 |     0.08 |
| tot2 | :=  | c21+ind1+ind2+ind3 |     0 |     0 | tot2  | -0.42 | 0.03 | -13.07 |   0.00 |    -0.48 |    -0.35 |

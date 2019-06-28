Pillar 1 - Student Topic Profile
================
DARS
2019-06-28

-   [Set up](#set-up)
-   [Data](#data)
-   [Extract sample of one course](#extract-sample-of-one-course)
-   [Grade prediction](#grade-prediction)

**Considerations**:

-   extract course descriptions from courses not offer in 2018-2019 e.g. SCI2012.

-   give more weight to 3000-level courses

-   restrict prep courses to UCM courses

Set up
======

Data
====

Remove course enrollments from 2007 from the student model because we have no information on the previous course enrolment of these students, hence no data on their previous academic performance and skills acquired in previous courses.

``` r
d_student_model <- d_student_model %>% filter(Year_numerical != 2007)
```

Consider courses currently on offer and with at least 20 enrollments since 2008

``` r
d_course <- tibble(target = course_current) %>%
  
  mutate(student_model = target        %>% map(find_df, d_student_model),
         n             = student_model %>% map_dbl(nrow)) %>%
  
  filter(n > 20)
```

Beta of topic model to inspect results

``` r
beta  <- app_model$Beta[[1]] %>% group_by(topic) %>% top_n(10, beta)
gamma <- app_model$Gamma[[1]] %>%  mutate(topic = topic %>% str_replace(" ", "_"))
remove(app_model)
```

Extract sample of one course
============================

``` r
{z <- find_df("HUM2025", d_student_model) %>%
  select(Grade, matches("GPA|Topic"))

n_sample <- nrow(z)
n_train  <- floor(0.67 * n_sample)
n_test   <- n_sample - n_train 

allocation <- sample(rep(c("train", "test"), times = c(n_train, n_test)))
z <- z %>% mutate(allocation = allocation)
z_train <- z %>% filter(allocation == "train") %>% select(-allocation)
z_test  <- z %>% filter(allocation == "test" ) %>% select(-allocation)} # prepare sample
```

Grade prediction
================

``` r
assess_prediction <- function(test_set = z_test, predictions){
  
  test_set <- test_set %>% mutate(y_hat = predictions,
                                  res = Grade - y_hat)

 # hist(test_set$y_hat)
#  plot(test_set$y_hat, test_set$res)
#  plot(test_set$Grade, test_set$res)
  plot(test_set$Grade, test_set$y_hat)
  
  mae <- test_set$res %>% abs %>% mean
  print(str_c("MAE: ", mae %>% round(2)))
  
}
```

``` r
# data
x <- z_train %>% select(matches("Topic|GPA")) %>% as.matrix
y <- z_train %>% pull(Grade)
x_test <- z_test %>% select(matches("Topic|GPA"))

# Boost
m_boost <- xgboost(data = x, label = y,
                   max_depth = 2, eta = .3, subsample = 1, nrounds = 1000, colsample_bytree = .5,
                   objective = "reg:linear")
```

    ## [1]  train-rmse:4.799419 
    ## [2]  train-rmse:3.458556 
    ## [3]  train-rmse:2.521405 
    ## [4]  train-rmse:1.876167 
    ## [5]  train-rmse:1.436764 
    ## [6]  train-rmse:1.141091 
    ## [7]  train-rmse:0.947768 
    ## [8]  train-rmse:0.825622 
    ## [9]  train-rmse:0.747958 
    ## [10] train-rmse:0.697675 
    ## [11] train-rmse:0.651874 
    ## [12] train-rmse:0.621803 
    ## [13] train-rmse:0.599182 
    ## [14] train-rmse:0.584800 
    ## [15] train-rmse:0.568013 
    ## [16] train-rmse:0.556221 
    ## [17] train-rmse:0.540190 
    ## [18] train-rmse:0.519643 
    ## [19] train-rmse:0.507746 
    ## [20] train-rmse:0.494228 
    ## [21] train-rmse:0.483354 
    ## [22] train-rmse:0.477699 
    ## [23] train-rmse:0.468314 
    ## [24] train-rmse:0.462113 
    ## [25] train-rmse:0.454845 
    ## [26] train-rmse:0.446108 
    ## [27] train-rmse:0.441841 
    ## [28] train-rmse:0.434795 
    ## [29] train-rmse:0.430145 
    ## [30] train-rmse:0.425875 
    ## [31] train-rmse:0.422227 
    ## [32] train-rmse:0.419589 
    ## [33] train-rmse:0.415214 
    ## [34] train-rmse:0.411487 
    ## [35] train-rmse:0.408779 
    ## [36] train-rmse:0.406918 
    ## [37] train-rmse:0.404215 
    ## [38] train-rmse:0.400888 
    ## [39] train-rmse:0.398630 
    ## [40] train-rmse:0.396556 
    ## [41] train-rmse:0.394661 
    ## [42] train-rmse:0.392498 
    ## [43] train-rmse:0.391593 
    ## [44] train-rmse:0.390751 
    ## [45] train-rmse:0.388897 
    ## [46] train-rmse:0.387378 
    ## [47] train-rmse:0.386658 
    ## [48] train-rmse:0.385650 
    ## [49] train-rmse:0.384453 
    ## [50] train-rmse:0.383845 
    ## [51] train-rmse:0.382854 
    ## [52] train-rmse:0.382389 
    ## [53] train-rmse:0.381349 
    ## [54] train-rmse:0.380449 
    ## [55] train-rmse:0.380021 
    ## [56] train-rmse:0.379276 
    ## [57] train-rmse:0.378657 
    ## [58] train-rmse:0.378206 
    ## [59] train-rmse:0.377769 
    ## [60] train-rmse:0.377455 
    ## [61] train-rmse:0.376968 
    ## [62] train-rmse:0.376650 
    ## [63] train-rmse:0.376276 
    ## [64] train-rmse:0.376100 
    ## [65] train-rmse:0.375806 
    ## [66] train-rmse:0.375673 
    ## [67] train-rmse:0.375364 
    ## [68] train-rmse:0.375241 
    ## [69] train-rmse:0.375049 
    ## [70] train-rmse:0.374807 
    ## [71] train-rmse:0.374603 
    ## [72] train-rmse:0.374467 
    ## [73] train-rmse:0.374263 
    ## [74] train-rmse:0.374095 
    ## [75] train-rmse:0.373995 
    ## [76] train-rmse:0.373820 
    ## [77] train-rmse:0.373724 
    ## [78] train-rmse:0.373555 
    ## [79] train-rmse:0.373501 
    ## [80] train-rmse:0.373410 
    ## [81] train-rmse:0.373347 
    ## [82] train-rmse:0.373233 
    ## [83] train-rmse:0.373107 
    ## [84] train-rmse:0.373021 
    ## [85] train-rmse:0.372930 
    ## [86] train-rmse:0.372821 
    ## [87] train-rmse:0.372776 
    ## [88] train-rmse:0.372705 
    ## [89] train-rmse:0.372658 
    ## [90] train-rmse:0.372600 
    ## [91] train-rmse:0.372559 
    ## [92] train-rmse:0.372507 
    ## [93] train-rmse:0.372469 
    ## [94] train-rmse:0.372426 
    ## [95] train-rmse:0.372373 
    ## [96] train-rmse:0.372343 
    ## [97] train-rmse:0.372302 
    ## [98] train-rmse:0.372290 
    ## [99] train-rmse:0.372273 
    ## [100]    train-rmse:0.372240 
    ## [101]    train-rmse:0.372208 
    ## [102]    train-rmse:0.372199 
    ## [103]    train-rmse:0.372175 
    ## [104]    train-rmse:0.372161 
    ## [105]    train-rmse:0.372137 
    ## [106]    train-rmse:0.372131 
    ## [107]    train-rmse:0.372116 
    ## [108]    train-rmse:0.372108 
    ## [109]    train-rmse:0.372093 
    ## [110]    train-rmse:0.372079 
    ## [111]    train-rmse:0.372067 
    ## [112]    train-rmse:0.372055 
    ## [113]    train-rmse:0.372050 
    ## [114]    train-rmse:0.372040 
    ## [115]    train-rmse:0.372032 
    ## [116]    train-rmse:0.372021 
    ## [117]    train-rmse:0.372012 
    ## [118]    train-rmse:0.372008 
    ## [119]    train-rmse:0.371998 
    ## [120]    train-rmse:0.371995 
    ## [121]    train-rmse:0.371987 
    ## [122]    train-rmse:0.371982 
    ## [123]    train-rmse:0.371979 
    ## [124]    train-rmse:0.371974 
    ## [125]    train-rmse:0.371971 
    ## [126]    train-rmse:0.371966 
    ## [127]    train-rmse:0.371964 
    ## [128]    train-rmse:0.371962 
    ## [129]    train-rmse:0.371958 
    ## [130]    train-rmse:0.371954 
    ## [131]    train-rmse:0.371951 
    ## [132]    train-rmse:0.371949 
    ## [133]    train-rmse:0.371947 
    ## [134]    train-rmse:0.371943 
    ## [135]    train-rmse:0.371941 
    ## [136]    train-rmse:0.371940 
    ## [137]    train-rmse:0.371938 
    ## [138]    train-rmse:0.371936 
    ## [139]    train-rmse:0.371934 
    ## [140]    train-rmse:0.371933 
    ## [141]    train-rmse:0.371931 
    ## [142]    train-rmse:0.371929 
    ## [143]    train-rmse:0.371927 
    ## [144]    train-rmse:0.371925 
    ## [145]    train-rmse:0.371924 
    ## [146]    train-rmse:0.371923 
    ## [147]    train-rmse:0.371921 
    ## [148]    train-rmse:0.371921 
    ## [149]    train-rmse:0.371920 
    ## [150]    train-rmse:0.371919 
    ## [151]    train-rmse:0.371919 
    ## [152]    train-rmse:0.371919 
    ## [153]    train-rmse:0.371918 
    ## [154]    train-rmse:0.371917 
    ## [155]    train-rmse:0.371916 
    ## [156]    train-rmse:0.371916 
    ## [157]    train-rmse:0.371915 
    ## [158]    train-rmse:0.371915 
    ## [159]    train-rmse:0.371915 
    ## [160]    train-rmse:0.371914 
    ## [161]    train-rmse:0.371913 
    ## [162]    train-rmse:0.371913 
    ## [163]    train-rmse:0.371912 
    ## [164]    train-rmse:0.371912 
    ## [165]    train-rmse:0.371912 
    ## [166]    train-rmse:0.371911 
    ## [167]    train-rmse:0.371911 
    ## [168]    train-rmse:0.371911 
    ## [169]    train-rmse:0.371910 
    ## [170]    train-rmse:0.371910 
    ## [171]    train-rmse:0.371910 
    ## [172]    train-rmse:0.371910 
    ## [173]    train-rmse:0.371909 
    ## [174]    train-rmse:0.371909 
    ## [175]    train-rmse:0.371909 
    ## [176]    train-rmse:0.371909 
    ## [177]    train-rmse:0.371909 
    ## [178]    train-rmse:0.371909 
    ## [179]    train-rmse:0.371909 
    ## [180]    train-rmse:0.371909 
    ## [181]    train-rmse:0.371909 
    ## [182]    train-rmse:0.371908 
    ## [183]    train-rmse:0.371908 
    ## [184]    train-rmse:0.371908 
    ## [185]    train-rmse:0.371908 
    ## [186]    train-rmse:0.371908 
    ## [187]    train-rmse:0.371908 
    ## [188]    train-rmse:0.371908 
    ## [189]    train-rmse:0.371908 
    ## [190]    train-rmse:0.371908 
    ## [191]    train-rmse:0.371908 
    ## [192]    train-rmse:0.371908 
    ## [193]    train-rmse:0.371908 
    ## [194]    train-rmse:0.371907 
    ## [195]    train-rmse:0.371907 
    ## [196]    train-rmse:0.371907 
    ## [197]    train-rmse:0.371907 
    ## [198]    train-rmse:0.371907 
    ## [199]    train-rmse:0.371907 
    ## [200]    train-rmse:0.371907 
    ## [201]    train-rmse:0.371907 
    ## [202]    train-rmse:0.371907 
    ## [203]    train-rmse:0.371907 
    ## [204]    train-rmse:0.371907 
    ## [205]    train-rmse:0.371907 
    ## [206]    train-rmse:0.371907 
    ## [207]    train-rmse:0.371907 
    ## [208]    train-rmse:0.371907 
    ## [209]    train-rmse:0.371907 
    ## [210]    train-rmse:0.371907 
    ## [211]    train-rmse:0.371907 
    ## [212]    train-rmse:0.371907 
    ## [213]    train-rmse:0.371907 
    ## [214]    train-rmse:0.371907 
    ## [215]    train-rmse:0.371907 
    ## [216]    train-rmse:0.371907 
    ## [217]    train-rmse:0.371907 
    ## [218]    train-rmse:0.371907 
    ## [219]    train-rmse:0.371907 
    ## [220]    train-rmse:0.371907 
    ## [221]    train-rmse:0.371907 
    ## [222]    train-rmse:0.371907 
    ## [223]    train-rmse:0.371907 
    ## [224]    train-rmse:0.371907 
    ## [225]    train-rmse:0.371907 
    ## [226]    train-rmse:0.371907 
    ## [227]    train-rmse:0.371907 
    ## [228]    train-rmse:0.371907 
    ## [229]    train-rmse:0.371907 
    ## [230]    train-rmse:0.371907 
    ## [231]    train-rmse:0.371907 
    ## [232]    train-rmse:0.371907 
    ## [233]    train-rmse:0.371907 
    ## [234]    train-rmse:0.371907 
    ## [235]    train-rmse:0.371907 
    ## [236]    train-rmse:0.371907 
    ## [237]    train-rmse:0.371907 
    ## [238]    train-rmse:0.371907 
    ## [239]    train-rmse:0.371907 
    ## [240]    train-rmse:0.371907 
    ## [241]    train-rmse:0.371907 
    ## [242]    train-rmse:0.371907 
    ## [243]    train-rmse:0.371907 
    ## [244]    train-rmse:0.371907 
    ## [245]    train-rmse:0.371907 
    ## [246]    train-rmse:0.371907 
    ## [247]    train-rmse:0.371907 
    ## [248]    train-rmse:0.371907 
    ## [249]    train-rmse:0.371907 
    ## [250]    train-rmse:0.371907 
    ## [251]    train-rmse:0.371907 
    ## [252]    train-rmse:0.371907 
    ## [253]    train-rmse:0.371907 
    ## [254]    train-rmse:0.371907 
    ## [255]    train-rmse:0.371907 
    ## [256]    train-rmse:0.371907 
    ## [257]    train-rmse:0.371907 
    ## [258]    train-rmse:0.371907 
    ## [259]    train-rmse:0.371907 
    ## [260]    train-rmse:0.371907 
    ## [261]    train-rmse:0.371907 
    ## [262]    train-rmse:0.371907 
    ## [263]    train-rmse:0.371907 
    ## [264]    train-rmse:0.371907 
    ## [265]    train-rmse:0.371907 
    ## [266]    train-rmse:0.371907 
    ## [267]    train-rmse:0.371907 
    ## [268]    train-rmse:0.371907 
    ## [269]    train-rmse:0.371907 
    ## [270]    train-rmse:0.371907 
    ## [271]    train-rmse:0.371907 
    ## [272]    train-rmse:0.371907 
    ## [273]    train-rmse:0.371907 
    ## [274]    train-rmse:0.371907 
    ## [275]    train-rmse:0.371907 
    ## [276]    train-rmse:0.371907 
    ## [277]    train-rmse:0.371907 
    ## [278]    train-rmse:0.371907 
    ## [279]    train-rmse:0.371907 
    ## [280]    train-rmse:0.371907 
    ## [281]    train-rmse:0.371907 
    ## [282]    train-rmse:0.371907 
    ## [283]    train-rmse:0.371907 
    ## [284]    train-rmse:0.371907 
    ## [285]    train-rmse:0.371907 
    ## [286]    train-rmse:0.371907 
    ## [287]    train-rmse:0.371907 
    ## [288]    train-rmse:0.371907 
    ## [289]    train-rmse:0.371907 
    ## [290]    train-rmse:0.371907 
    ## [291]    train-rmse:0.371907 
    ## [292]    train-rmse:0.371907 
    ## [293]    train-rmse:0.371907 
    ## [294]    train-rmse:0.371907 
    ## [295]    train-rmse:0.371907 
    ## [296]    train-rmse:0.371907 
    ## [297]    train-rmse:0.371907 
    ## [298]    train-rmse:0.371907 
    ## [299]    train-rmse:0.371907 
    ## [300]    train-rmse:0.371907 
    ## [301]    train-rmse:0.371907 
    ## [302]    train-rmse:0.371907 
    ## [303]    train-rmse:0.371907 
    ## [304]    train-rmse:0.371907 
    ## [305]    train-rmse:0.371907 
    ## [306]    train-rmse:0.371907 
    ## [307]    train-rmse:0.371907 
    ## [308]    train-rmse:0.371907 
    ## [309]    train-rmse:0.371907 
    ## [310]    train-rmse:0.371907 
    ## [311]    train-rmse:0.371907 
    ## [312]    train-rmse:0.371907 
    ## [313]    train-rmse:0.371907 
    ## [314]    train-rmse:0.371907 
    ## [315]    train-rmse:0.371907 
    ## [316]    train-rmse:0.371907 
    ## [317]    train-rmse:0.371907 
    ## [318]    train-rmse:0.371907 
    ## [319]    train-rmse:0.371907 
    ## [320]    train-rmse:0.371907 
    ## [321]    train-rmse:0.371907 
    ## [322]    train-rmse:0.371907 
    ## [323]    train-rmse:0.371907 
    ## [324]    train-rmse:0.371907 
    ## [325]    train-rmse:0.371907 
    ## [326]    train-rmse:0.371907 
    ## [327]    train-rmse:0.371907 
    ## [328]    train-rmse:0.371907 
    ## [329]    train-rmse:0.371907 
    ## [330]    train-rmse:0.371907 
    ## [331]    train-rmse:0.371907 
    ## [332]    train-rmse:0.371907 
    ## [333]    train-rmse:0.371907 
    ## [334]    train-rmse:0.371907 
    ## [335]    train-rmse:0.371907 
    ## [336]    train-rmse:0.371907 
    ## [337]    train-rmse:0.371907 
    ## [338]    train-rmse:0.371907 
    ## [339]    train-rmse:0.371907 
    ## [340]    train-rmse:0.371907 
    ## [341]    train-rmse:0.371907 
    ## [342]    train-rmse:0.371907 
    ## [343]    train-rmse:0.371907 
    ## [344]    train-rmse:0.371907 
    ## [345]    train-rmse:0.371907 
    ## [346]    train-rmse:0.371907 
    ## [347]    train-rmse:0.371907 
    ## [348]    train-rmse:0.371907 
    ## [349]    train-rmse:0.371907 
    ## [350]    train-rmse:0.371907 
    ## [351]    train-rmse:0.371907 
    ## [352]    train-rmse:0.371907 
    ## [353]    train-rmse:0.371907 
    ## [354]    train-rmse:0.371907 
    ## [355]    train-rmse:0.371907 
    ## [356]    train-rmse:0.371907 
    ## [357]    train-rmse:0.371907 
    ## [358]    train-rmse:0.371907 
    ## [359]    train-rmse:0.371907 
    ## [360]    train-rmse:0.371907 
    ## [361]    train-rmse:0.371907 
    ## [362]    train-rmse:0.371907 
    ## [363]    train-rmse:0.371907 
    ## [364]    train-rmse:0.371907 
    ## [365]    train-rmse:0.371907 
    ## [366]    train-rmse:0.371907 
    ## [367]    train-rmse:0.371907 
    ## [368]    train-rmse:0.371907 
    ## [369]    train-rmse:0.371907 
    ## [370]    train-rmse:0.371907 
    ## [371]    train-rmse:0.371907 
    ## [372]    train-rmse:0.371907 
    ## [373]    train-rmse:0.371907 
    ## [374]    train-rmse:0.371907 
    ## [375]    train-rmse:0.371907 
    ## [376]    train-rmse:0.371907 
    ## [377]    train-rmse:0.371907 
    ## [378]    train-rmse:0.371907 
    ## [379]    train-rmse:0.371907 
    ## [380]    train-rmse:0.371907 
    ## [381]    train-rmse:0.371907 
    ## [382]    train-rmse:0.371907 
    ## [383]    train-rmse:0.371907 
    ## [384]    train-rmse:0.371907 
    ## [385]    train-rmse:0.371907 
    ## [386]    train-rmse:0.371907 
    ## [387]    train-rmse:0.371907 
    ## [388]    train-rmse:0.371907 
    ## [389]    train-rmse:0.371907 
    ## [390]    train-rmse:0.371907 
    ## [391]    train-rmse:0.371907 
    ## [392]    train-rmse:0.371907 
    ## [393]    train-rmse:0.371907 
    ## [394]    train-rmse:0.371907 
    ## [395]    train-rmse:0.371907 
    ## [396]    train-rmse:0.371907 
    ## [397]    train-rmse:0.371907 
    ## [398]    train-rmse:0.371907 
    ## [399]    train-rmse:0.371907 
    ## [400]    train-rmse:0.371907 
    ## [401]    train-rmse:0.371907 
    ## [402]    train-rmse:0.371907 
    ## [403]    train-rmse:0.371907 
    ## [404]    train-rmse:0.371907 
    ## [405]    train-rmse:0.371907 
    ## [406]    train-rmse:0.371907 
    ## [407]    train-rmse:0.371907 
    ## [408]    train-rmse:0.371907 
    ## [409]    train-rmse:0.371907 
    ## [410]    train-rmse:0.371907 
    ## [411]    train-rmse:0.371907 
    ## [412]    train-rmse:0.371907 
    ## [413]    train-rmse:0.371907 
    ## [414]    train-rmse:0.371907 
    ## [415]    train-rmse:0.371907 
    ## [416]    train-rmse:0.371907 
    ## [417]    train-rmse:0.371907 
    ## [418]    train-rmse:0.371907 
    ## [419]    train-rmse:0.371907 
    ## [420]    train-rmse:0.371907 
    ## [421]    train-rmse:0.371907 
    ## [422]    train-rmse:0.371907 
    ## [423]    train-rmse:0.371907 
    ## [424]    train-rmse:0.371907 
    ## [425]    train-rmse:0.371907 
    ## [426]    train-rmse:0.371907 
    ## [427]    train-rmse:0.371907 
    ## [428]    train-rmse:0.371907 
    ## [429]    train-rmse:0.371907 
    ## [430]    train-rmse:0.371907 
    ## [431]    train-rmse:0.371907 
    ## [432]    train-rmse:0.371907 
    ## [433]    train-rmse:0.371907 
    ## [434]    train-rmse:0.371907 
    ## [435]    train-rmse:0.371907 
    ## [436]    train-rmse:0.371907 
    ## [437]    train-rmse:0.371907 
    ## [438]    train-rmse:0.371907 
    ## [439]    train-rmse:0.371907 
    ## [440]    train-rmse:0.371907 
    ## [441]    train-rmse:0.371907 
    ## [442]    train-rmse:0.371907 
    ## [443]    train-rmse:0.371907 
    ## [444]    train-rmse:0.371907 
    ## [445]    train-rmse:0.371907 
    ## [446]    train-rmse:0.371907 
    ## [447]    train-rmse:0.371907 
    ## [448]    train-rmse:0.371907 
    ## [449]    train-rmse:0.371907 
    ## [450]    train-rmse:0.371907 
    ## [451]    train-rmse:0.371907 
    ## [452]    train-rmse:0.371907 
    ## [453]    train-rmse:0.371907 
    ## [454]    train-rmse:0.371907 
    ## [455]    train-rmse:0.371907 
    ## [456]    train-rmse:0.371907 
    ## [457]    train-rmse:0.371907 
    ## [458]    train-rmse:0.371907 
    ## [459]    train-rmse:0.371907 
    ## [460]    train-rmse:0.371907 
    ## [461]    train-rmse:0.371907 
    ## [462]    train-rmse:0.371907 
    ## [463]    train-rmse:0.371907 
    ## [464]    train-rmse:0.371907 
    ## [465]    train-rmse:0.371907 
    ## [466]    train-rmse:0.371907 
    ## [467]    train-rmse:0.371907 
    ## [468]    train-rmse:0.371907 
    ## [469]    train-rmse:0.371907 
    ## [470]    train-rmse:0.371907 
    ## [471]    train-rmse:0.371907 
    ## [472]    train-rmse:0.371907 
    ## [473]    train-rmse:0.371907 
    ## [474]    train-rmse:0.371907 
    ## [475]    train-rmse:0.371907 
    ## [476]    train-rmse:0.371907 
    ## [477]    train-rmse:0.371907 
    ## [478]    train-rmse:0.371907 
    ## [479]    train-rmse:0.371907 
    ## [480]    train-rmse:0.371907 
    ## [481]    train-rmse:0.371907 
    ## [482]    train-rmse:0.371907 
    ## [483]    train-rmse:0.371907 
    ## [484]    train-rmse:0.371907 
    ## [485]    train-rmse:0.371907 
    ## [486]    train-rmse:0.371907 
    ## [487]    train-rmse:0.371907 
    ## [488]    train-rmse:0.371907 
    ## [489]    train-rmse:0.371907 
    ## [490]    train-rmse:0.371907 
    ## [491]    train-rmse:0.371907 
    ## [492]    train-rmse:0.371907 
    ## [493]    train-rmse:0.371907 
    ## [494]    train-rmse:0.371907 
    ## [495]    train-rmse:0.371907 
    ## [496]    train-rmse:0.371907 
    ## [497]    train-rmse:0.371907 
    ## [498]    train-rmse:0.371907 
    ## [499]    train-rmse:0.371907 
    ## [500]    train-rmse:0.371907 
    ## [501]    train-rmse:0.371907 
    ## [502]    train-rmse:0.371907 
    ## [503]    train-rmse:0.371907 
    ## [504]    train-rmse:0.371907 
    ## [505]    train-rmse:0.371907 
    ## [506]    train-rmse:0.371907 
    ## [507]    train-rmse:0.371907 
    ## [508]    train-rmse:0.371907 
    ## [509]    train-rmse:0.371907 
    ## [510]    train-rmse:0.371907 
    ## [511]    train-rmse:0.371907 
    ## [512]    train-rmse:0.371907 
    ## [513]    train-rmse:0.371907 
    ## [514]    train-rmse:0.371907 
    ## [515]    train-rmse:0.371907 
    ## [516]    train-rmse:0.371907 
    ## [517]    train-rmse:0.371907 
    ## [518]    train-rmse:0.371907 
    ## [519]    train-rmse:0.371907 
    ## [520]    train-rmse:0.371907 
    ## [521]    train-rmse:0.371907 
    ## [522]    train-rmse:0.371907 
    ## [523]    train-rmse:0.371907 
    ## [524]    train-rmse:0.371907 
    ## [525]    train-rmse:0.371907 
    ## [526]    train-rmse:0.371907 
    ## [527]    train-rmse:0.371907 
    ## [528]    train-rmse:0.371907 
    ## [529]    train-rmse:0.371907 
    ## [530]    train-rmse:0.371907 
    ## [531]    train-rmse:0.371907 
    ## [532]    train-rmse:0.371907 
    ## [533]    train-rmse:0.371907 
    ## [534]    train-rmse:0.371907 
    ## [535]    train-rmse:0.371907 
    ## [536]    train-rmse:0.371907 
    ## [537]    train-rmse:0.371907 
    ## [538]    train-rmse:0.371907 
    ## [539]    train-rmse:0.371907 
    ## [540]    train-rmse:0.371907 
    ## [541]    train-rmse:0.371907 
    ## [542]    train-rmse:0.371907 
    ## [543]    train-rmse:0.371907 
    ## [544]    train-rmse:0.371907 
    ## [545]    train-rmse:0.371907 
    ## [546]    train-rmse:0.371907 
    ## [547]    train-rmse:0.371907 
    ## [548]    train-rmse:0.371907 
    ## [549]    train-rmse:0.371907 
    ## [550]    train-rmse:0.371907 
    ## [551]    train-rmse:0.371907 
    ## [552]    train-rmse:0.371907 
    ## [553]    train-rmse:0.371907 
    ## [554]    train-rmse:0.371907 
    ## [555]    train-rmse:0.371907 
    ## [556]    train-rmse:0.371907 
    ## [557]    train-rmse:0.371907 
    ## [558]    train-rmse:0.371907 
    ## [559]    train-rmse:0.371907 
    ## [560]    train-rmse:0.371907 
    ## [561]    train-rmse:0.371907 
    ## [562]    train-rmse:0.371907 
    ## [563]    train-rmse:0.371907 
    ## [564]    train-rmse:0.371907 
    ## [565]    train-rmse:0.371907 
    ## [566]    train-rmse:0.371907 
    ## [567]    train-rmse:0.371907 
    ## [568]    train-rmse:0.371907 
    ## [569]    train-rmse:0.371907 
    ## [570]    train-rmse:0.371907 
    ## [571]    train-rmse:0.371907 
    ## [572]    train-rmse:0.371907 
    ## [573]    train-rmse:0.371907 
    ## [574]    train-rmse:0.371907 
    ## [575]    train-rmse:0.371907 
    ## [576]    train-rmse:0.371907 
    ## [577]    train-rmse:0.371907 
    ## [578]    train-rmse:0.371907 
    ## [579]    train-rmse:0.371907 
    ## [580]    train-rmse:0.371907 
    ## [581]    train-rmse:0.371907 
    ## [582]    train-rmse:0.371907 
    ## [583]    train-rmse:0.371907 
    ## [584]    train-rmse:0.371907 
    ## [585]    train-rmse:0.371907 
    ## [586]    train-rmse:0.371907 
    ## [587]    train-rmse:0.371907 
    ## [588]    train-rmse:0.371907 
    ## [589]    train-rmse:0.371907 
    ## [590]    train-rmse:0.371907 
    ## [591]    train-rmse:0.371907 
    ## [592]    train-rmse:0.371907 
    ## [593]    train-rmse:0.371907 
    ## [594]    train-rmse:0.371907 
    ## [595]    train-rmse:0.371907 
    ## [596]    train-rmse:0.371907 
    ## [597]    train-rmse:0.371907 
    ## [598]    train-rmse:0.371907 
    ## [599]    train-rmse:0.371907 
    ## [600]    train-rmse:0.371907 
    ## [601]    train-rmse:0.371907 
    ## [602]    train-rmse:0.371907 
    ## [603]    train-rmse:0.371907 
    ## [604]    train-rmse:0.371907 
    ## [605]    train-rmse:0.371907 
    ## [606]    train-rmse:0.371907 
    ## [607]    train-rmse:0.371907 
    ## [608]    train-rmse:0.371907 
    ## [609]    train-rmse:0.371907 
    ## [610]    train-rmse:0.371907 
    ## [611]    train-rmse:0.371907 
    ## [612]    train-rmse:0.371907 
    ## [613]    train-rmse:0.371907 
    ## [614]    train-rmse:0.371907 
    ## [615]    train-rmse:0.371907 
    ## [616]    train-rmse:0.371907 
    ## [617]    train-rmse:0.371907 
    ## [618]    train-rmse:0.371907 
    ## [619]    train-rmse:0.371907 
    ## [620]    train-rmse:0.371907 
    ## [621]    train-rmse:0.371907 
    ## [622]    train-rmse:0.371907 
    ## [623]    train-rmse:0.371907 
    ## [624]    train-rmse:0.371907 
    ## [625]    train-rmse:0.371907 
    ## [626]    train-rmse:0.371907 
    ## [627]    train-rmse:0.371907 
    ## [628]    train-rmse:0.371907 
    ## [629]    train-rmse:0.371907 
    ## [630]    train-rmse:0.371907 
    ## [631]    train-rmse:0.371907 
    ## [632]    train-rmse:0.371907 
    ## [633]    train-rmse:0.371907 
    ## [634]    train-rmse:0.371907 
    ## [635]    train-rmse:0.371907 
    ## [636]    train-rmse:0.371907 
    ## [637]    train-rmse:0.371907 
    ## [638]    train-rmse:0.371907 
    ## [639]    train-rmse:0.371907 
    ## [640]    train-rmse:0.371907 
    ## [641]    train-rmse:0.371907 
    ## [642]    train-rmse:0.371907 
    ## [643]    train-rmse:0.371907 
    ## [644]    train-rmse:0.371907 
    ## [645]    train-rmse:0.371907 
    ## [646]    train-rmse:0.371907 
    ## [647]    train-rmse:0.371907 
    ## [648]    train-rmse:0.371907 
    ## [649]    train-rmse:0.371907 
    ## [650]    train-rmse:0.371907 
    ## [651]    train-rmse:0.371907 
    ## [652]    train-rmse:0.371907 
    ## [653]    train-rmse:0.371907 
    ## [654]    train-rmse:0.371907 
    ## [655]    train-rmse:0.371907 
    ## [656]    train-rmse:0.371907 
    ## [657]    train-rmse:0.371907 
    ## [658]    train-rmse:0.371907 
    ## [659]    train-rmse:0.371907 
    ## [660]    train-rmse:0.371907 
    ## [661]    train-rmse:0.371907 
    ## [662]    train-rmse:0.371907 
    ## [663]    train-rmse:0.371907 
    ## [664]    train-rmse:0.371907 
    ## [665]    train-rmse:0.371907 
    ## [666]    train-rmse:0.371907 
    ## [667]    train-rmse:0.371907 
    ## [668]    train-rmse:0.371907 
    ## [669]    train-rmse:0.371907 
    ## [670]    train-rmse:0.371907 
    ## [671]    train-rmse:0.371907 
    ## [672]    train-rmse:0.371907 
    ## [673]    train-rmse:0.371907 
    ## [674]    train-rmse:0.371907 
    ## [675]    train-rmse:0.371907 
    ## [676]    train-rmse:0.371907 
    ## [677]    train-rmse:0.371907 
    ## [678]    train-rmse:0.371907 
    ## [679]    train-rmse:0.371907 
    ## [680]    train-rmse:0.371907 
    ## [681]    train-rmse:0.371907 
    ## [682]    train-rmse:0.371907 
    ## [683]    train-rmse:0.371907 
    ## [684]    train-rmse:0.371907 
    ## [685]    train-rmse:0.371907 
    ## [686]    train-rmse:0.371907 
    ## [687]    train-rmse:0.371907 
    ## [688]    train-rmse:0.371907 
    ## [689]    train-rmse:0.371907 
    ## [690]    train-rmse:0.371907 
    ## [691]    train-rmse:0.371907 
    ## [692]    train-rmse:0.371907 
    ## [693]    train-rmse:0.371907 
    ## [694]    train-rmse:0.371907 
    ## [695]    train-rmse:0.371907 
    ## [696]    train-rmse:0.371907 
    ## [697]    train-rmse:0.371907 
    ## [698]    train-rmse:0.371907 
    ## [699]    train-rmse:0.371907 
    ## [700]    train-rmse:0.371907 
    ## [701]    train-rmse:0.371907 
    ## [702]    train-rmse:0.371907 
    ## [703]    train-rmse:0.371907 
    ## [704]    train-rmse:0.371907 
    ## [705]    train-rmse:0.371907 
    ## [706]    train-rmse:0.371907 
    ## [707]    train-rmse:0.371907 
    ## [708]    train-rmse:0.371907 
    ## [709]    train-rmse:0.371907 
    ## [710]    train-rmse:0.371907 
    ## [711]    train-rmse:0.371907 
    ## [712]    train-rmse:0.371907 
    ## [713]    train-rmse:0.371907 
    ## [714]    train-rmse:0.371907 
    ## [715]    train-rmse:0.371907 
    ## [716]    train-rmse:0.371907 
    ## [717]    train-rmse:0.371907 
    ## [718]    train-rmse:0.371907 
    ## [719]    train-rmse:0.371907 
    ## [720]    train-rmse:0.371907 
    ## [721]    train-rmse:0.371907 
    ## [722]    train-rmse:0.371907 
    ## [723]    train-rmse:0.371907 
    ## [724]    train-rmse:0.371907 
    ## [725]    train-rmse:0.371907 
    ## [726]    train-rmse:0.371907 
    ## [727]    train-rmse:0.371907 
    ## [728]    train-rmse:0.371907 
    ## [729]    train-rmse:0.371907 
    ## [730]    train-rmse:0.371907 
    ## [731]    train-rmse:0.371907 
    ## [732]    train-rmse:0.371907 
    ## [733]    train-rmse:0.371907 
    ## [734]    train-rmse:0.371907 
    ## [735]    train-rmse:0.371907 
    ## [736]    train-rmse:0.371907 
    ## [737]    train-rmse:0.371907 
    ## [738]    train-rmse:0.371907 
    ## [739]    train-rmse:0.371907 
    ## [740]    train-rmse:0.371907 
    ## [741]    train-rmse:0.371907 
    ## [742]    train-rmse:0.371907 
    ## [743]    train-rmse:0.371907 
    ## [744]    train-rmse:0.371907 
    ## [745]    train-rmse:0.371907 
    ## [746]    train-rmse:0.371907 
    ## [747]    train-rmse:0.371907 
    ## [748]    train-rmse:0.371907 
    ## [749]    train-rmse:0.371907 
    ## [750]    train-rmse:0.371907 
    ## [751]    train-rmse:0.371907 
    ## [752]    train-rmse:0.371907 
    ## [753]    train-rmse:0.371907 
    ## [754]    train-rmse:0.371907 
    ## [755]    train-rmse:0.371907 
    ## [756]    train-rmse:0.371907 
    ## [757]    train-rmse:0.371907 
    ## [758]    train-rmse:0.371907 
    ## [759]    train-rmse:0.371907 
    ## [760]    train-rmse:0.371907 
    ## [761]    train-rmse:0.371907 
    ## [762]    train-rmse:0.371907 
    ## [763]    train-rmse:0.371907 
    ## [764]    train-rmse:0.371907 
    ## [765]    train-rmse:0.371907 
    ## [766]    train-rmse:0.371907 
    ## [767]    train-rmse:0.371907 
    ## [768]    train-rmse:0.371907 
    ## [769]    train-rmse:0.371907 
    ## [770]    train-rmse:0.371907 
    ## [771]    train-rmse:0.371907 
    ## [772]    train-rmse:0.371907 
    ## [773]    train-rmse:0.371907 
    ## [774]    train-rmse:0.371907 
    ## [775]    train-rmse:0.371907 
    ## [776]    train-rmse:0.371907 
    ## [777]    train-rmse:0.371907 
    ## [778]    train-rmse:0.371907 
    ## [779]    train-rmse:0.371907 
    ## [780]    train-rmse:0.371907 
    ## [781]    train-rmse:0.371907 
    ## [782]    train-rmse:0.371907 
    ## [783]    train-rmse:0.371907 
    ## [784]    train-rmse:0.371907 
    ## [785]    train-rmse:0.371907 
    ## [786]    train-rmse:0.371907 
    ## [787]    train-rmse:0.371907 
    ## [788]    train-rmse:0.371907 
    ## [789]    train-rmse:0.371907 
    ## [790]    train-rmse:0.371907 
    ## [791]    train-rmse:0.371907 
    ## [792]    train-rmse:0.371907 
    ## [793]    train-rmse:0.371907 
    ## [794]    train-rmse:0.371907 
    ## [795]    train-rmse:0.371907 
    ## [796]    train-rmse:0.371907 
    ## [797]    train-rmse:0.371907 
    ## [798]    train-rmse:0.371907 
    ## [799]    train-rmse:0.371907 
    ## [800]    train-rmse:0.371907 
    ## [801]    train-rmse:0.371907 
    ## [802]    train-rmse:0.371907 
    ## [803]    train-rmse:0.371907 
    ## [804]    train-rmse:0.371907 
    ## [805]    train-rmse:0.371907 
    ## [806]    train-rmse:0.371907 
    ## [807]    train-rmse:0.371907 
    ## [808]    train-rmse:0.371907 
    ## [809]    train-rmse:0.371907 
    ## [810]    train-rmse:0.371907 
    ## [811]    train-rmse:0.371907 
    ## [812]    train-rmse:0.371907 
    ## [813]    train-rmse:0.371907 
    ## [814]    train-rmse:0.371907 
    ## [815]    train-rmse:0.371907 
    ## [816]    train-rmse:0.371907 
    ## [817]    train-rmse:0.371907 
    ## [818]    train-rmse:0.371907 
    ## [819]    train-rmse:0.371907 
    ## [820]    train-rmse:0.371907 
    ## [821]    train-rmse:0.371907 
    ## [822]    train-rmse:0.371907 
    ## [823]    train-rmse:0.371907 
    ## [824]    train-rmse:0.371907 
    ## [825]    train-rmse:0.371907 
    ## [826]    train-rmse:0.371907 
    ## [827]    train-rmse:0.371907 
    ## [828]    train-rmse:0.371907 
    ## [829]    train-rmse:0.371907 
    ## [830]    train-rmse:0.371907 
    ## [831]    train-rmse:0.371907 
    ## [832]    train-rmse:0.371907 
    ## [833]    train-rmse:0.371907 
    ## [834]    train-rmse:0.371907 
    ## [835]    train-rmse:0.371907 
    ## [836]    train-rmse:0.371907 
    ## [837]    train-rmse:0.371907 
    ## [838]    train-rmse:0.371907 
    ## [839]    train-rmse:0.371907 
    ## [840]    train-rmse:0.371907 
    ## [841]    train-rmse:0.371907 
    ## [842]    train-rmse:0.371907 
    ## [843]    train-rmse:0.371907 
    ## [844]    train-rmse:0.371907 
    ## [845]    train-rmse:0.371907 
    ## [846]    train-rmse:0.371907 
    ## [847]    train-rmse:0.371907 
    ## [848]    train-rmse:0.371907 
    ## [849]    train-rmse:0.371907 
    ## [850]    train-rmse:0.371907 
    ## [851]    train-rmse:0.371907 
    ## [852]    train-rmse:0.371907 
    ## [853]    train-rmse:0.371907 
    ## [854]    train-rmse:0.371907 
    ## [855]    train-rmse:0.371907 
    ## [856]    train-rmse:0.371907 
    ## [857]    train-rmse:0.371907 
    ## [858]    train-rmse:0.371907 
    ## [859]    train-rmse:0.371907 
    ## [860]    train-rmse:0.371907 
    ## [861]    train-rmse:0.371907 
    ## [862]    train-rmse:0.371907 
    ## [863]    train-rmse:0.371907 
    ## [864]    train-rmse:0.371907 
    ## [865]    train-rmse:0.371907 
    ## [866]    train-rmse:0.371907 
    ## [867]    train-rmse:0.371907 
    ## [868]    train-rmse:0.371907 
    ## [869]    train-rmse:0.371907 
    ## [870]    train-rmse:0.371907 
    ## [871]    train-rmse:0.371907 
    ## [872]    train-rmse:0.371907 
    ## [873]    train-rmse:0.371907 
    ## [874]    train-rmse:0.371907 
    ## [875]    train-rmse:0.371907 
    ## [876]    train-rmse:0.371907 
    ## [877]    train-rmse:0.371907 
    ## [878]    train-rmse:0.371907 
    ## [879]    train-rmse:0.371907 
    ## [880]    train-rmse:0.371907 
    ## [881]    train-rmse:0.371907 
    ## [882]    train-rmse:0.371907 
    ## [883]    train-rmse:0.371907 
    ## [884]    train-rmse:0.371907 
    ## [885]    train-rmse:0.371907 
    ## [886]    train-rmse:0.371907 
    ## [887]    train-rmse:0.371907 
    ## [888]    train-rmse:0.371907 
    ## [889]    train-rmse:0.371907 
    ## [890]    train-rmse:0.371907 
    ## [891]    train-rmse:0.371907 
    ## [892]    train-rmse:0.371907 
    ## [893]    train-rmse:0.371907 
    ## [894]    train-rmse:0.371907 
    ## [895]    train-rmse:0.371907 
    ## [896]    train-rmse:0.371907 
    ## [897]    train-rmse:0.371907 
    ## [898]    train-rmse:0.371907 
    ## [899]    train-rmse:0.371907 
    ## [900]    train-rmse:0.371907 
    ## [901]    train-rmse:0.371907 
    ## [902]    train-rmse:0.371907 
    ## [903]    train-rmse:0.371907 
    ## [904]    train-rmse:0.371907 
    ## [905]    train-rmse:0.371907 
    ## [906]    train-rmse:0.371907 
    ## [907]    train-rmse:0.371907 
    ## [908]    train-rmse:0.371907 
    ## [909]    train-rmse:0.371907 
    ## [910]    train-rmse:0.371907 
    ## [911]    train-rmse:0.371907 
    ## [912]    train-rmse:0.371907 
    ## [913]    train-rmse:0.371907 
    ## [914]    train-rmse:0.371907 
    ## [915]    train-rmse:0.371907 
    ## [916]    train-rmse:0.371907 
    ## [917]    train-rmse:0.371907 
    ## [918]    train-rmse:0.371907 
    ## [919]    train-rmse:0.371907 
    ## [920]    train-rmse:0.371907 
    ## [921]    train-rmse:0.371907 
    ## [922]    train-rmse:0.371907 
    ## [923]    train-rmse:0.371907 
    ## [924]    train-rmse:0.371907 
    ## [925]    train-rmse:0.371907 
    ## [926]    train-rmse:0.371907 
    ## [927]    train-rmse:0.371907 
    ## [928]    train-rmse:0.371907 
    ## [929]    train-rmse:0.371907 
    ## [930]    train-rmse:0.371907 
    ## [931]    train-rmse:0.371907 
    ## [932]    train-rmse:0.371907 
    ## [933]    train-rmse:0.371907 
    ## [934]    train-rmse:0.371907 
    ## [935]    train-rmse:0.371907 
    ## [936]    train-rmse:0.371907 
    ## [937]    train-rmse:0.371907 
    ## [938]    train-rmse:0.371907 
    ## [939]    train-rmse:0.371907 
    ## [940]    train-rmse:0.371907 
    ## [941]    train-rmse:0.371907 
    ## [942]    train-rmse:0.371907 
    ## [943]    train-rmse:0.371907 
    ## [944]    train-rmse:0.371907 
    ## [945]    train-rmse:0.371907 
    ## [946]    train-rmse:0.371907 
    ## [947]    train-rmse:0.371907 
    ## [948]    train-rmse:0.371907 
    ## [949]    train-rmse:0.371907 
    ## [950]    train-rmse:0.371907 
    ## [951]    train-rmse:0.371907 
    ## [952]    train-rmse:0.371907 
    ## [953]    train-rmse:0.371907 
    ## [954]    train-rmse:0.371907 
    ## [955]    train-rmse:0.371907 
    ## [956]    train-rmse:0.371907 
    ## [957]    train-rmse:0.371907 
    ## [958]    train-rmse:0.371907 
    ## [959]    train-rmse:0.371907 
    ## [960]    train-rmse:0.371907 
    ## [961]    train-rmse:0.371907 
    ## [962]    train-rmse:0.371907 
    ## [963]    train-rmse:0.371907 
    ## [964]    train-rmse:0.371907 
    ## [965]    train-rmse:0.371907 
    ## [966]    train-rmse:0.371907 
    ## [967]    train-rmse:0.371907 
    ## [968]    train-rmse:0.371907 
    ## [969]    train-rmse:0.371907 
    ## [970]    train-rmse:0.371907 
    ## [971]    train-rmse:0.371907 
    ## [972]    train-rmse:0.371907 
    ## [973]    train-rmse:0.371907 
    ## [974]    train-rmse:0.371907 
    ## [975]    train-rmse:0.371907 
    ## [976]    train-rmse:0.371907 
    ## [977]    train-rmse:0.371907 
    ## [978]    train-rmse:0.371907 
    ## [979]    train-rmse:0.371907 
    ## [980]    train-rmse:0.371907 
    ## [981]    train-rmse:0.371907 
    ## [982]    train-rmse:0.371907 
    ## [983]    train-rmse:0.371907 
    ## [984]    train-rmse:0.371907 
    ## [985]    train-rmse:0.371907 
    ## [986]    train-rmse:0.371907 
    ## [987]    train-rmse:0.371907 
    ## [988]    train-rmse:0.371907 
    ## [989]    train-rmse:0.371907 
    ## [990]    train-rmse:0.371907 
    ## [991]    train-rmse:0.371907 
    ## [992]    train-rmse:0.371907 
    ## [993]    train-rmse:0.371907 
    ## [994]    train-rmse:0.371907 
    ## [995]    train-rmse:0.371907 
    ## [996]    train-rmse:0.371907 
    ## [997]    train-rmse:0.371907 
    ## [998]    train-rmse:0.371907 
    ## [999]    train-rmse:0.371907 
    ## [1000]   train-rmse:0.371907

``` r
y_boost <- predict(m_boost, as.matrix(x_test))
assess_prediction(predictions = y_boost)
```

![](C:\Users\JeanLuc\Desktop\DARS_2019\md%20files\WI%20GP%20RF_files/figure-markdown_github/unnamed-chunk-6-1.png)

    ## [1] "MAE: 0.86"

``` r
# RF
m_RF <- randomForest(as.matrix(x), y,
                     ntree = 1000, mtry = floor(ncol(z_train)/3))

y_rf <- predict(m_RF, as.matrix(x_test))
assess_prediction(predictions = y_rf)
```

![](C:\Users\JeanLuc\Desktop\DARS_2019\md%20files\WI%20GP%20RF_files/figure-markdown_github/unnamed-chunk-6-2.png)

    ## [1] "MAE: 0.83"

``` r
m_lasso <- cv.glmnet(as.matrix(x), y,
                     nfolds = 10, type.measure = "mae")
y_lasso <- predict(m_lasso, newx = as.matrix(x_test), s="lambda.min")
assess_prediction(predictions = y_lasso)
```

![](C:\Users\JeanLuc\Desktop\DARS_2019\md%20files\WI%20GP%20RF_files/figure-markdown_github/unnamed-chunk-6-3.png)

    ## [1] "MAE: 0.85"

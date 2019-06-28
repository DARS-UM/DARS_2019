Data Preparation
================
DARS
2019-06-28

-   [Import Data](#import-data)
    -   [List of AoD's and Assessments](#list-of-aods-and-assessments)
    -   [Course Data](#course-data)
-   [Variable Engineering](#variable-engineering)
    -   [Course Data](#course-data-1)
        -   [AoD](#aod)
        -   [Courses](#courses)
    -   [Ttarget courses](#ttarget-courses)
-   [Save Data](#save-data)

Import Data
===========

The datasets we use are saved as spreadsheet on our google drive *DARS* (with exeption of grade data saved as csv files on the computer for privacy reasons). We use the function `gsheet2tbl` to import them to `R Studio` as tibbles. We use the tibble data format (an evolution of the data frame format) because this is the format of reference of the `tidyverse` on whose tools our analysis is heavily based.

List of AoD's and Assessments
-----------------------------

First, we import the spreadsheet with information pertraining the Aims of the Degree (AoD) and Assessments from the drive and save it under `lists_brut`. `lists_brut` contains 4 columns, under which we find the `19` types of assessment, the `18` aims of the degree (AoD) of the degree, and two columns containing binary vectors indicating which assessment types and AoD we will consider when ploting the data\[^1\]. Then we create a list with this same columns, but instead of having binary vectors for the plots, we keep vectors of only the names of relevant assessments and AoDs for the plots (`Assessment_plot`adnd `AoD_plot` respectively). We also any imported emtpy cells.

``` r
list_AoD_assessment <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1239912347') %>%
  
  map(na.omit)

# selection of most important types of assessment an AoD to keep plots clear
list_AoD_assessment$`Assessment for Plot` <- list_AoD_assessment$Assessment[list_AoD_assessment$`Assessment for Plot` == 1] %T>% print
```

    ##  [1] "Paper"          "Essay"          "Presentation"   "Written Exam"  
    ##  [5] "Take Home"      "Lab"            "Research Prop." "Debate"        
    ##  [9] "Poster"         "Group"          "Participation"

``` r
list_AoD_assessment$`Aim for Plot`        <- list_AoD_assessment$Aim       [list_AoD_assessment$`Aim for Plot`        == 1] %T>% print
```

    ##  [1] "Elementary Knowledge"     "Advanced Knowledge"      
    ##  [3] "Kn. in Wider Context"     "Problem Solving Skills"  
    ##  [5] "Critical Thinking Skills" "Communication Skills"    
    ##  [7] "Writing Skills"           "Learning Skills"         
    ##  [9] "Research Skills"          "Reflective Skills"       
    ## [11] "Ethical Skills"           "Collaborative Skills"    
    ## [13] "Intercultural Skills"

Course Data
-----------

We import three spreadsheets from the drive and transform them into the so-called *tidy format*. The tibble `d_course` contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered. The tibble `d_assessment` indicates which type(s) of assessment each course contains with one row per course-assessment; and the tibble `d_ILO` indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD.

``` r
d_course <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1655700848') %>% print
```

    ## # A tibble: 280 x 11
    ##    `Course ID` `Course Title` Period `Period (additi~ `Period (additi~
    ##    <chr>       <chr>          <chr>  <chr>            <lgl>           
    ##  1 AAM2001     Academic Advi~ Perio~ <NA>             NA              
    ##  2 AAM2002     Academic Advi~ Perio~ <NA>             NA              
    ##  3 AAM2003     Academic Advi~ Perio~ <NA>             NA              
    ##  4 AAM2004     Academic Advi~ Perio~ <NA>             NA              
    ##  5 AAM2005     Academic Advi~ Perio~ <NA>             NA              
    ##  6 AAM2006     Academic Advi~ Perio~ <NA>             NA              
    ##  7 AAM2007     Academic Advi~ Perio~ <NA>             NA              
    ##  8 CAP3000     Capstone       Perio~ Period 4         NA              
    ##  9 COR1002     Philosophy of~ Perio~ Period 5         NA              
    ## 10 COR1003     Contemporary ~ Perio~ Period 4         NA              
    ## # ... with 270 more rows, and 6 more variables: Concentration <chr>,
    ## #   `Concentration (additional)` <chr>, Cluster <chr>, `Missing from ILO
    ## #   File` <dbl>, `Most Recent Catalogue` <chr>, Type <chr>

``` r
d_assessment <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1102665750') %>%
  
  mutate_if(is.numeric, as.logical) %>%
  
  # Transform data into tidy format to facilitate manipulation
  gather(key   = Assessment, value = assessment_covered, Paper : Participation) %>%
  
  filter(assessment_covered) %>%
  
  select(- c(assessment_covered, `Comment on Assessment`)) %T>%
  
  print
```

    ## # A tibble: 482 x 2
    ##    `Course ID` Assessment
    ##    <chr>       <chr>     
    ##  1 CAP3000     Paper     
    ##  2 COR1003     Paper     
    ##  3 HUM1003     Paper     
    ##  4 HUM1007     Paper     
    ##  5 HUM1010     Paper     
    ##  6 HUM1013     Paper     
    ##  7 HUM1014     Paper     
    ##  8 HUM2008     Paper     
    ##  9 HUM2014     Paper     
    ## 10 HUM2021     Paper     
    ## # ... with 472 more rows

``` r
d_ILO <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=1896457050')
```

``` r
d_AoD_from_ILO <- d_ILO %>%
  
  rename(ILO = Objectives) %>%
  
  mutate_if(is.numeric, as.logical) %>%
  
  # Transform data into tidy format to facilitate manipulation
  gather(key = AoD, value = AoD_covered, `Matrix Complete` : `Intercultural Skills`) %>%
  
  filter(AoD_covered) %>%
  
  select(- c(AoD_covered, Comments, Comments_for_Jeroen)) %T>%
  
  print
```

    ## # A tibble: 1,232 x 3
    ##    `Course ID` ILO                                                    AoD  
    ##    <chr>       <chr>                                                  <chr>
    ##  1 COR1002     1. To know the major approaches in the philosophy of ~ Core 
    ##  2 COR1002     2. To have knowledge of the major problems or topics ~ Core 
    ##  3 COR1002     3. To have knowledge of a number of specific problems~ Core 
    ##  4 COR1003     1. To understand the main trends in politics, demogra~ Core 
    ##  5 COR1003     2. To develop a critical attitude towards the interpr~ Core 
    ##  6 COR1003     3. To Develop a critical understanding concerning the~ Core 
    ##  7 COR1004     1. To provide the students with a basic understanding~ Core 
    ##  8 COR1004     2. To understand the central concepts like justice an~ Core 
    ##  9 COR1005     1. To offer a broad overview of scientific models and~ Core 
    ## 10 COR1005     2. To teach students how to work with models in diffe~ Core 
    ## # ... with 1,222 more rows

Variable Engineering
====================

Course Data
-----------

The analysis performed on the course data is aimed at discoving what the contribution of each course is towards the fullfilment of the AoDs, and comparing different types of curricula, or programs, based on this infromation.

### AoD

In this analysis, we conside that a course can cover an AoD in two ways: a course covers an AoD if one of its ILOs covers it, or if one of its assessments cover it. For instance, if one of ILO of a course states that the students will learn to analyze empirical data in the context of academic research, then the course in question covers the AoD `Research Skills`; and if one of the assessment is a group presentation, then the course also covers the AoD `Collaborative Skills` and `Communication Skills`.

#### AoD from ILOs

(For this section we use: `d_ILO`-indicates which AoD(s) the intended learning objectives (ILOs) of the courses cover with one row per course-ILO-AoD)

In order to determine which AoD each course covers with its ILOs, we first eliminate the AoD that are not covered by the ILOs, and then we keep only one instance of each combination of course and AoD in case a course had several ILOs covering the same AoD.

``` r
d_AoD_from_ILO
```

    ## # A tibble: 1,232 x 3
    ##    `Course ID` ILO                                                    AoD  
    ##    <chr>       <chr>                                                  <chr>
    ##  1 COR1002     1. To know the major approaches in the philosophy of ~ Core 
    ##  2 COR1002     2. To have knowledge of the major problems or topics ~ Core 
    ##  3 COR1002     3. To have knowledge of a number of specific problems~ Core 
    ##  4 COR1003     1. To understand the main trends in politics, demogra~ Core 
    ##  5 COR1003     2. To develop a critical attitude towards the interpr~ Core 
    ##  6 COR1003     3. To Develop a critical understanding concerning the~ Core 
    ##  7 COR1004     1. To provide the students with a basic understanding~ Core 
    ##  8 COR1004     2. To understand the central concepts like justice an~ Core 
    ##  9 COR1005     1. To offer a broad overview of scientific models and~ Core 
    ## 10 COR1005     2. To teach students how to work with models in diffe~ Core 
    ## # ... with 1,222 more rows

#### AoD from Assessment

(For this section we use: `d_assessment`-indicates which type(s) of assessment each course contains with one row per course-assessment)

In order to determine which AoD each course covers with its assessments, we need to create a binary matrix which indicates which AoD(s) each assessment type covers. We have created such matrix on our google drive and we save it in the following piece of code as `map_assessment_AoD`. `map_assessment_AoD` indicates that, for instance, the assessment type `Essay` covers the AoD `Critical Thinking Skills`, `Communication Skills` and `Writing Skill`. Thus, the first step is to import this matrix:

``` r
map_assessment_AoD <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1soRA1u5zf9oLNirGmZ9yZ7m2ccPa3XFemnxkj5AVRXo/edit#gid=719531216') %>%
  
  mutate_if(is.numeric, as.logical) %>%
  
  # Transform data into tidy format to facilitate manipulation
  gather(key = AoD, value = AoD_covered, `Matrix Complete` : `Intercultural Skills`) %>%
  
  filter(AoD_covered) %>%
  
  select(- AoD_covered) %>%
  
  arrange(Assessment) %T>%
  
  print
```

    ## # A tibble: 39 x 2
    ##    Assessment        AoD                     
    ##    <chr>             <chr>                   
    ##  1 Artistic Creation Communication Skills    
    ##  2 Assignments       Problem Solving Skills  
    ##  3 Debate            Critical Thinking Skills
    ##  4 Debate            Communication Skills    
    ##  5 Essay             Critical Thinking Skills
    ##  6 Essay             Communication Skills    
    ##  7 Essay             Writing Skills          
    ##  8 Feedback          Critical Thinking Skills
    ##  9 Feedback          Communication Skills    
    ## 10 Group             Communication Skills    
    ## # ... with 29 more rows

Now that we have the matrix `map_assessment_AoD`, we want to find out which AoDs are covered by a course through its assessments. To do this we create an empty tibble `d_AoD_assessment` to store which assessment is covered by each course, and which AoD said assessment covers.Then, we fill in the information with a loop. In the loop, we first extract a row of `d_assessment` and save it as `observation`. Then, we determine the corresponding assessment which we save as `assessment`. Then, we use the matrix `map_assessment_AoD` to determine which AoD `assessment` covers and use `cbind` and `rbind` to add the information to the tibble `d_AoD_assessment`.

``` r
d_AoD_from_assessment <- d_assessment %>%
  
  left_join(map_assessment_AoD, by = "Assessment") %>%
  
  filter(!is.na(AoD)) %T>%
  
  print
```

    ## # A tibble: 1,089 x 3
    ##    `Course ID` Assessment AoD                     
    ##    <chr>       <chr>      <chr>                   
    ##  1 CAP3000     Paper      Problem Solving Skills  
    ##  2 CAP3000     Paper      Critical Thinking Skills
    ##  3 CAP3000     Paper      Communication Skills    
    ##  4 CAP3000     Paper      Writing Skills          
    ##  5 CAP3000     Paper      Research Skills         
    ##  6 COR1003     Paper      Problem Solving Skills  
    ##  7 COR1003     Paper      Critical Thinking Skills
    ##  8 COR1003     Paper      Communication Skills    
    ##  9 COR1003     Paper      Writing Skills          
    ## 10 COR1003     Paper      Research Skills         
    ## # ... with 1,079 more rows

``` r
rm(map_assessment_AoD)
```

#### Combining AoD from ILOs and from assessments

Finally, we can use a `rbind` to combine the two tibbles `d_AoD_ILO` and `d_AoD_assessment`. We also use `distinct` in case a course covers an AoD with both its ILOS and its assessments.

``` r
d_AoD <- rbind(
  d_AoD_from_ILO        %>% select(`Course ID`, AoD),
  d_AoD_from_assessment %>% select(`Course ID`, AoD)
  ) %>%
  
  distinct %>%
  
  arrange(`Course ID`) %T>%
  
  print
```

    ## # A tibble: 1,231 x 2
    ##    `Course ID` AoD                   
    ##    <chr>       <chr>                 
    ##  1 AAM2001     Academic Expertise    
    ##  2 AAM2001     Reflective Skills     
    ##  3 AAM2001     Decision-making Skills
    ##  4 AAM2001     Communication Skills  
    ##  5 AAM2002     Academic Expertise    
    ##  6 AAM2002     Reflective Skills     
    ##  7 AAM2002     Decision-making Skills
    ##  8 AAM2002     Communication Skills  
    ##  9 AAM2003     Academic Expertise    
    ## 10 AAM2003     Reflective Skills     
    ## # ... with 1,221 more rows

``` r
rm(d_AoD_from_assessment, d_AoD_from_ILO)
```

### Courses

(In this section we use: `d_course`- contains information at the course-level such as in which cluster and concentration(s) they belong, and in which period(s) they are offered.)

Now that we have a clear overview of the distribution of AoDs (`d_AoD`) and assessments (`d_assessment`) among the courses, let us add variables to the tibble `d_course` that contain the information at the course level. For this we create the following three tibbles: 1) `d_ILO_detail` contains two columns indicating the code of the course and the number of ILOs it contains. 2) `d_assessment_detail` contains three columns indicating the code of the course, the number of assessments it covers and a list of the assessments it covers. 3) `d_AoD_detail` contains three columns indicating the code of the course, the number of AoD it covers and a list of the AoD it covers. Then we use a `full_join` to add these variables to the tibble `d_course`.

``` r
count_and_paste <- function(df, var, colnam1, colnam2){
  
  df %>%
    
    group_by(`Course ID`) %>%
    
    summarize(
      !!ensym(colnam1) := n_distinct(!!enquo(var)),
      !!ensym(colnam2) := str_c(!!enquo(var), collapse = ", ")
      )
  
}

join_by_course_ID <- function(df1, df2) left_join(df1, df2, by = "Course ID")
```

``` r
d_course <- d_course %>%
  
  join_by_course_ID( d_AoD        %>% count_and_paste(AoD       , n_AoD       , `AoD Covered`        ) %T>% print) %>%
  
  join_by_course_ID( d_assessment %>% count_and_paste(Assessment, n_assessment, `Assessments Covered`)           ) %>%
  
  join_by_course_ID( d_ILO        %>% count_and_paste(Objectives, n_ILO       , `ILO Covered`        )           ) %>%
  
  select(
    `Course ID`, `Course Title`, Cluster,
    n_ILO,
    n_assessment, `Assessments Covered`,
    n_AoD, `AoD Covered`,
    everything()
    ) %T>%
  
  print
```

    ## # A tibble: 279 x 3
    ##    `Course ID` n_AoD `AoD Covered`                                         
    ##    <chr>       <int> <chr>                                                 
    ##  1 AAM2001         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  2 AAM2002         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  3 AAM2003         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  4 AAM2004         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  5 AAM2005         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  6 AAM2006         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  7 AAM2007         4 Academic Expertise, Reflective Skills, Decision-makin~
    ##  8 CAP3000        10 Advanced Knowledge, Academic Expertise, Graduate Stud~
    ##  9 COR1002         7 Core, Elementary Knowledge, Reflective Skills, Ethica~
    ## 10 COR1003         7 Core, Elementary Knowledge, Critical Thinking Skills,~
    ## # ... with 269 more rows
    ## # A tibble: 280 x 17
    ##    `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##    <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ##  1 AAM2001     Academic Advi~ <NA>        1            1 Oral            
    ##  2 AAM2002     Academic Advi~ <NA>        1            1 Oral            
    ##  3 AAM2003     Academic Advi~ <NA>        1            1 Oral            
    ##  4 AAM2004     Academic Advi~ <NA>        1            1 Oral            
    ##  5 AAM2005     Academic Advi~ <NA>        1            1 Oral            
    ##  6 AAM2006     Academic Advi~ <NA>        1            1 Oral            
    ##  7 AAM2007     Academic Advi~ <NA>        1            1 Oral            
    ##  8 CAP3000     Capstone       <NA>        3            3 Paper, Presenta~
    ##  9 COR1002     Philosophy of~ Philos~     3            2 Essay, Written ~
    ## 10 COR1003     Contemporary ~ History     3            3 Paper, Written ~
    ## # ... with 270 more rows, and 11 more variables: n_AoD <int>, `AoD
    ## #   Covered` <chr>, Period <chr>, `Period (additional)` <chr>, `Period
    ## #   (additional bis)` <lgl>, Concentration <chr>, `Concentration
    ## #   (additional)` <chr>, `Missing from ILO File` <dbl>, `Most Recent
    ## #   Catalogue` <chr>, Type <chr>, `ILO Covered` <chr>

``` r
rm(count_and_paste, join_by_course_ID)
```

The undegraduate research courses (*UGR-*) are only present at the `3000` level (advanced level). Yet, these course are also offered at the `2000` level (intermediate level). We use an `rbind` to duplicate the rows of the course `UGR3000` and mutate their `Code` to `UGR2000`.

``` r
UGR_2000 <- d_course %>%
  
  filter( `Course ID` %in% c("UGR3001", "UGR3002", "UGR3003", "UGR3005")) %>%
  
  mutate(
    `Course ID` = case_when(
      `Course ID` == "UGR3001" ~ "UGR2001",
      `Course ID` == "UGR3002" ~ "UGR2002",
      `Course ID` == "UGR3003" ~ "UGR2003",
      `Course ID` == "UGR3005" ~ "UGR2005"
      )
    ) %T>%
  
  print
```

    ## # A tibble: 4 x 17
    ##   `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##   <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ## 1 UGR2001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 2 UGR2002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 3 UGR2003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 4 UGR2005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## # ... with 11 more variables: n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>, `ILO Covered` <chr>

``` r
d_course <- d_course %>% rbind(UGR_2000)

# print UGR course
print(d_course %>% filter(str_detect(`Course ID`, "UGR") ) )
```

    ## # A tibble: 8 x 17
    ##   `Course ID` `Course Title` Cluster n_ILO n_assessment `Assessments Co~
    ##   <chr>       <chr>          <chr>   <int>        <int> <chr>           
    ## 1 UGR3001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 2 UGR3002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 3 UGR3003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 4 UGR3005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## 5 UGR2001     Undergraduate~ Methods     5            2 Paper, Presenta~
    ## 6 UGR2002     Undergraduate~ Skills     NA            6 Paper, Essay, P~
    ## 7 UGR2003     Applied Resea~ Skills     NA            5 Paper, Presenta~
    ## 8 UGR2005     Artistic Rese~ Methods    NA            2 Paper, Research~
    ## # ... with 11 more variables: n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>, `ILO Covered` <chr>

``` r
rm(UGR_2000)
```

Finally, we add a series of informative variable at the course level.

``` r
d_course <- d_course %>%
  
  separate(
    col     = `Course ID`,
    into    = c("Letters", "Number"),
    sep     = 3,
    remove  = FALSE,
    convert = TRUE
    ) %>%
  
  mutate(
    Level   = case_when(
      between(Number, 1000, 1999) ~ "Introductory",
      between(Number, 2000, 2999) ~ "Intermediate",
      between(Number, 3000, 3999) ~ "Advanced"    )
    ) %>%
  
  select(`Course ID`, Letters, Number, Level, everything()) %T>%
  
  print
```

    ## # A tibble: 284 x 20
    ##    `Course ID` Letters Number Level `Course Title` Cluster n_ILO
    ##    <chr>       <chr>    <int> <chr> <chr>          <chr>   <int>
    ##  1 AAM2001     AAM       2001 Inte~ Academic Advi~ <NA>        1
    ##  2 AAM2002     AAM       2002 Inte~ Academic Advi~ <NA>        1
    ##  3 AAM2003     AAM       2003 Inte~ Academic Advi~ <NA>        1
    ##  4 AAM2004     AAM       2004 Inte~ Academic Advi~ <NA>        1
    ##  5 AAM2005     AAM       2005 Inte~ Academic Advi~ <NA>        1
    ##  6 AAM2006     AAM       2006 Inte~ Academic Advi~ <NA>        1
    ##  7 AAM2007     AAM       2007 Inte~ Academic Advi~ <NA>        1
    ##  8 CAP3000     CAP       3000 Adva~ Capstone       <NA>        3
    ##  9 COR1002     COR       1002 Intr~ Philosophy of~ Philos~     3
    ## 10 COR1003     COR       1003 Intr~ Contemporary ~ History     3
    ## # ... with 274 more rows, and 13 more variables: n_assessment <int>,
    ## #   `Assessments Covered` <chr>, n_AoD <int>, `AoD Covered` <chr>,
    ## #   Period <chr>, `Period (additional)` <chr>, `Period (additional
    ## #   bis)` <lgl>, Concentration <chr>, `Concentration (additional)` <chr>,
    ## #   `Missing from ILO File` <dbl>, `Most Recent Catalogue` <chr>,
    ## #   Type <chr>, `ILO Covered` <chr>

Ttarget courses
---------------

``` r
d_course <- d_course %>%
  mutate(target = Number > 2000 & Type == "Elective" & (! str_detect(`Course ID`, "SA")))
```

Save Data
=========

``` r
save(d_course, list_AoD_assessment, d_AoD, d_assessment, d_ILO,
     file = "Output/data_general.RDATA")
save(d_course, file = "Output/data_pillar_2.RDATA")
```

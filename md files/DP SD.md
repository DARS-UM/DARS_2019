Data Preparation
================
DARS
2019-06-28

-   [Set up](#set-up)
-   [Import Data](#import-data)
-   [Variable Engineering](#variable-engineering)
    -   [Augment d\_transcript with d\_course](#augment-d_transcript-with-d_course)
        -   [Issues](#issues)

Set up
======

Import Data
===========

The tibble `d_transcript` contains the transcript information of students as was provided. It has 40 columns, and rows correspond to a type of grade (e.g. final grade, attendance) per student per course per time they took it.

``` r
col_parsing <- cols(
  `Student Number`                      = "c",
  `Program (Abbreviation)`              = "c",
  `Appraisal Status`                    = "c",
  `Module Booking Reason (Description)` = "c",
  `Object name`                         = "c",
  `Start date`                          = "c",
  `End Date`                            = "c"
  )

read_csv_own_parsing <- function(file) file %>% read_csv(col_types = col_parsing)
```

``` r
d_transcript <- paste0("Input/Raw Grades/grades", 1 : 2, ".csv") %>%
  
  map(read_csv_own_parsing) %>%
  
  bind_rows

rm(col_parsing, read_csv_own_parsing)
```

Variable Engineering
====================

-   Is `Academic Work ID` a unique ID number?

(In this section we use: `d_transcript`- contains the transcript information of students as was provided. It has 40 columns, and rows correspond roughly to a type of grade (e.g. final grade, attendance) per student per course per time they took it).

Our dataframe contains many variables and rows that are either empty or meaningless for our analysis. First, we filter, the final grades of studets to keep only relevant rows (Final Confirmed Grades are those with `Appraisal (Description)` as "Grade supervisor"). Then we select ony the 10 variables that we will use for the anlysis, and give them more comprehensible names.

The data set `d_transcript` contains both the failed grades and the grade of the resit, and does not distinguish between them (same student, same course, same yera, same period, but different grade).

``` r
course_letter_UCM <- c("COR", "HUM", "SCI", "SSC", "SKI", "PRO", "UGR", "CAP") %>% paste(collapse = "|")

is_UCM      <- function(course_code) course_code %>% str_detect(course_letter_UCM)
is_elective <- function(course_code) course_code %>% str_detect("HUM|SCI|SSC")
```

``` r
d_transcript <- d_transcript %>%
  
  # for simplicity, we only keep 
  # courses taken in the framework of the *BA Liberal Arts and Sciences (UCM)* (Program (Abbreviation) = 7501)
  
  filter(`Program (Abbreviation)` == "7501") %>%
  
  # The dataset has multiple rows for each student - course.
  # Following guidelines of Richard Vos, we only consider: 
  # `Appraisal (Description)` == "Grade supervisor"
  # and `Appraisal Type` == "7055"
  
  filter(
    `Appraisal (Description)` == "Grade supervisor",
    `Appraisal Type`          == "7055" # removes ~ 15 observations
    )
```

``` r
d_transcript <- d_transcript %>%
  # keep variables: Student ID, course ID, when a course is taken and the grade 
  select(
    `Student ID`   = `Student Number`,
    `Course ID`    = `Module (Abbrev.)`,
    Year_numerical = `Academic Year`,
    Period         = `Academic Session`,
    Grade          = `Grade symbol`
    )
```

``` r
clean_grade <- function(grade){
  
  if(grade == "NG" | is.na(grade)) return(NA) # conservative approach
  
  grade %>% str_replace(",", ".") %>% as.numeric
  
}
```

``` r
clean_period <- function(period){
  
  # Course spanning over multiple period are recorder with a 1, 2 or 3
  if(period == 1) return("1 to 6")
  if(period == 2) return("1 to 3")
  if(period == 3) return("4 to 6")
  
  # Course spanning over a single period are recorded in the 100's, 200's, etc.
  # The digit of the hundred indicates the period.
  period %/% 100 %>% as.character
  
}
```

``` r
clean_year <- function(year) str_c(year, year + 1, sep = "-")
```

``` r
d_transcript <- d_transcript %>%
  
  mutate(
    
    # Clean grade, period and year
    Grade          = Grade          %>% map_dbl(clean_grade ),
    Period         = Period         %>% map_chr(clean_period),
    `Academic Year`= Year_numerical %>% clean_year,
    
    # extract first period for courses spanning mutliple periods
    period_numerical = substr(Period, 1, 1),
    
    # Combine year & period to chronologically order courses using a single variable
    time = str_c(Year_numerical, period_numerical) %>% as.numeric,
    
    grade_ceil = ceiling(Grade)
    
    )
```

``` r
d_transcript <- d_transcript %>% filter(!is.na(Grade))
```

Augment d\_transcript with d\_course
------------------------------------

``` r
load("Output/data_general.RDATA")
d_transcript_augmented <- d_transcript %>%
  left_join(select(d_course, - Period), by = c("Course ID"))
```

### Issues

``` r
d_transcript %>%
  
  # multiple rows with unique student ID - course ID - time combination.
  add_count(
    `Student ID`, `Course ID`, time,
    sort = TRUE
    ) %>%
  print

d_transcript %>%
  
  # multiple rows with unique student ID - course ID - time combination - grade.
  add_count(
    `Student ID`, `Course ID`, time, Grade,
    sort = TRUE
    ) %>%
  print

d_transcript %>%
  
  # SSC2046, a course I took, but excluded from my transcript is absent from the data set
  filter(`Student ID` == "6087587") %>%
  arrange(`Course ID`) %>%
  pull(`Course ID`)

  # ~ 10 incomplete rows for a student's profile
  d_transcript %>% filter(`Student ID` == "0481343") %>% View

#TODO: list issues here
```

"Output/data\_general.RDATA" \# Save Data

``` r
save(d_transcript, d_transcript_augmented, file = "Output/data_student.RDATA")
```

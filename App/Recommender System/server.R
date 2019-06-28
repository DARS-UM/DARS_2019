library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell) #for stemming
library(glmnet)   #for grade prediction
library(seewave)# KL distance
library(stm) #to fit new article
library(tm) #read pdf
library(pdftools)
library(topicmodels) #for posterior fit new article with LDA

#Load for debugging:
# load("App/Recommender System/data_pillar_1.RDATA")
# load("App/Recommender System/app_model.RDATA") #contains app_model and full dataframe of topic models
# load("App/Recommender System/app_stm.RDATA")   #contains stm model
# load("App/Recommender System/best_lda.RDATA")  #contains lda model for calculating new documents
# load("App/Recommender System/rules_clean.RDATA")
# load("App/Recommender System/grade_prediction.RDATA")
# load("App/Recommender System/student_TP.RDATA")
# load("App/Recommender System/d_text.RDATA")
#---------------------------------------finish: load for debugging ---------------------------------

#
#General set up ---------------------------------------------------------------------------------
load("data_pillar_1.RDATA")
load("app_model.RDATA") #contains app_model and full dataframe of topic models
load("app_stm.RDATA")   #contains stm model
load("best_lda.RDATA")  #contains lda model for calculating new documents
load("rules_clean.RDATA")
load("grade_prediction.RDATA")
load("student_TP.RDATA")
load("d_text.RDATA")


course_all      <- app_model$`All Courses`[[1]]
course_advanced <- course_all[!str_detect(course_all,"HUM10|SCI10|SSC10")]
kw_used_lda         <- app_model$kw[[1]] #is the same as in ui
kw_used_stm         <- app_stm$kw[[1]] # app_model$kw[[1]] #is the same as in ui

lda <- best_lda

model     <- app_stm$Model[[1]]
corpus    <-  app_stm$Corpus[[1]]
doc       <-  corpus$documents
vocab     <- corpus$vocab
doc_names <- app_stm$document_names

model_data <- list(corpus = corpus, doc = doc, vocab = vocab, model = model, doc_names = doc_names)

titles <-  d_text$catalogues %>%
  select(`Course ID`, `Course Title`, department) %>% 
  distinct %>% mutate(`c_title`= case_when(is.na(`Course Title`) ~ `Course ID`,
                                           T ~ `Course Title`),
                      `Course_id` = case_when(str_length(`Course ID`) == 7 ~ `Course ID`,
                                              T ~ "XXX0000")) %>% 
  transmute(`Course ID` = `Course ID`, course_and_title = paste(`Course_id`, `c_title`, sep = " - "))

# titles <-  d_text$catalogues %>%
#   select(`Course ID`, `Course Title`, department) %>% 
#   distinct %>% transmute(`Course ID`= `Course ID`,
#                          c_title= case_when(is.na(`Course Title`) ~ `Course ID`,
#                                             T ~ `Course Title`))

#
#server ---------------------------------------------------------------------------------
function(input, output, session) {
  
  # # ---------------------------------------------------------------------------------
  # ## -- RED FLAGS --
  # # ---------------------------------------------------------------------------------
  # 
  # ##Set up
  # 
  # #Raw Data for RS:
  # raw_rules <- rules_clean %>%
  #   select(-rules_rules)   %>%
  #   filter(type_rule == "SR")
  # 
  # #Helper function: return tibble of type_rules
  # get_type_rules <-  function(type) {
  #   tmp <- raw_rules %>%
  #     filter(ID == type) %>%
  #     pull(rules_RS)
  #   
  #   return(tmp[[1]])
  # }
  # 
  # #Helper: resettabe input
  # output$resetable_input <- renderUI({
  #   times <- input$reset_input
  #   div(id = letters[(times %% length(letters)) + 1],
  #       checkboxGroupInput(
  #         inputId  = "course_chosen",
  #         label    = "Tentative Courses for following period",
  #         choices  = course_all,
  #         #selected = course_all,
  #         inline   = TRUE
  #       ))
  # })
  # 
  # 
  # # Helper function: Return student
  # student_trans <- function(d_student) {
  # 
  #   # Student profile
  #   student <- list()
  #   
  #   # Student transcript
  #   student$transcript <- d_transcript %>%
  #     
  #     filter(
  #       `Student ID` == d_student
  #     ) %>%
  #     
  #     select(
  #       course = `Course ID`,
  #       grade  = Grade
  #     ) %>%
  #     
  #     mutate(
  #       fail       = grade < 5.5,
  #       low        = grade < 6.5,
  #       grade_ceil = ceiling(grade)
  #     )
  #   
  #   # Courses with low score
  #   student$course_low <- student$transcript %>%
  #     filter(
  #       low
  #     ) %>%
  #     pull(
  #       course
  #     )
  #   
  #   # Courses with fail score
  #   student$course_fail <- student$transcript %>%
  #     filter(
  #       fail
  #     ) %>%
  #     pull(
  #       course
  #     )
  #   
  #   # Course not taken
  #   student$course_not_taken <- setdiff(
  #     x = d_course$`Course ID`, # list of all courses offered
  #     y = student$transcript$course
  #   )
  #   return(student)
  # }
  # 
  # #Reactive student function 
  # current_student <- reactive({
  #  student_trans(input$student)
  # })
  # 
  # #
  # ##Red Flags
  # output$red_flags <- renderTable({
  #   #student
  #   student <- current_student()
  #   
  #   # rules
  #   rules <- list()
  #   
  #   # rules not
  #   rules$not <- get_type_rules("THL") %>%
  #     
  #     # select appropriate rules
  #     filter(
  #       lhs_course %in% student$course_not_taken,
  #       rhs_course %in% input$course_chosen
  #       ) %>%
  #     
  #   mutate(
  #     `Red Flag` = rhs_course,
  #     Reason     = paste(
  #       "You have not taken ",
  #       lhs_course,
  #       " and ",
  #       round(confidence * 100, digits = 1),
  #       "% of the ",
  #       lhs.rhsTake.count,
  #       " students who have taken ", 
  #       rhs_course,
  #       " without first taking ",
  #       lhs_course,
  #       " obtained less than 6.5/10 in  ",
  #       rhs_course,
  #       ".",
  #       sep = ""
  #     )
  #   )
  #   
  #   
  #   # rules low
  #   rules$low <- get_type_rules("HL") %>%
  #     
  #     filter(
  #       lhs_course %in% student$course_low,
  #       rhs_course %in% input$course_chosen
  #       ) %>%
  #     
  #   mutate(
  #     `Red Flag` = rhs_course,
  #     Reason = paste(
  #       "You obtained less than 6.5/10 in ",
  #           lhs_course,
  #           " and ",
  #           round(confidence * 100, digits = 1),
  #           "% of the ",
  #           lhs.rhsTake.count,
  #           " students who have taken ",
  #           rhs_course,
  #           " after obtaining less than 6.5/10 in ",
  #           lhs_course,
  #           " obtained less than 6.5/10 in  ",
  #           rhs_course,
  #           ".",
  #           sep = ""
  #           )
  #   )
  #   
  #   
  #   # rules fail
  #   rules$fail <- get_type_rules("PF") %>%
  #     
  #     filter(
  #       lhs_course %in% student$course_fail,
  #       rhs_course %in% input$course_chosen
  #       ) %>%
  #     
  #   mutate(
  #     `Red Flag` = rhs_course,
  #     Reason     = paste(
  #       "You have failed ",
  #       lhs_course,
  #       " and ",
  #       round(confidence * 100, digits = 1),
  #       "% of the ",
  #       lhs.rhsTake.count,
  #       " students who have taken ", 
  #       rhs_course,
  #       " after failing ",
  #       lhs_course,
  #       " also failed ",
  #       rhs_course,
  #       ".",
  #       sep = ""
  #     )
  #       
  #   )
  #   
  #   
  #   # rules grade
  #   rules$grade <- get_type_rules("G") %>%
  #     
  #     left_join(
  #       student$transcript,
  #       by = c("lhs_course" = "course")
  #       ) %>%
  #     
  #     # select appropriate rules
  #     filter(
  #       grade <= lhs_outcome, # lhs
  #       rhs_course %in% input$course_chosen
  #     ) %>%
  #     
  #   mutate(
  #     `Red Flag` = rhs_course,
  #     Reason     = paste(
  #       "You have obtained less than ",
  #           lhs_outcome,
  #           "/10 in ",
  #           lhs_course,
  #           " and ",
  #           round(confidence * 100, digits = 1),
  #           "% of the ",
  #           lhs.rhsTake.count,
  #           " students who have taken ",
  #           rhs_course,
  #           " after obtaining less than ",
  #           lhs_outcome,
  #           "/10 in ",
  #           lhs_course,
  #           " obtained less than ",
  #           rhs_outcome,
  #           "/10 in ",
  #           rhs_course,
  #           ".",
  #           sep = ""
  #         )
  #   )
  #     
  #   
  #   
  #   rules <- rules %>%
  #     
  #     # bind data sets from lists `rules` into one data set
  #     lapply(
  #       FUN = function(rules) {
  #         rules %>% select(
  #           rhs_course,
  #           confidence,
  #           `Red Flag`,
  #           Reason
  #           )      
  #         }
  #       ) %>%
  #     
  #     bind_rows %>%
  #     
  #     # if several rule with same rhs, keep rule with highest confidence
  #     group_by(
  #       rhs_course
  #       ) %>%
  #     top_n(
  #       n  = 1,
  #       wt = confidence
  #       ) %>% 
  #     ungroup
  #   
  #   #output
  #   if(nrow(student$transcript) == 0){
  #     
  #     tibble(ERROR = "Student ID not found")
  #     
  #   }else if(nrow(rules) == 0){
  #     
  #     tibble(`Warning` = "No warning")
  #     
  #   }else{
  #     
  #      rules  %>%
  #        select(`Red Flag`, Reason) %>%
  #       rename(`Warning for` = `Red Flag`)
  #     
  #   }
  #   
  #   
  # })
  
  # ---------------------------------------------------------------------------------
  ## -- TRAFFIC LIGHTS --
  # ---------------------------------------------------------------------------------
  
  #
  ##Set up
  
  #Reactive Vectors 
  
  #student_ID
  student_ID_traffic <- reactive({
    req(input$student_traffic)
  })
  #course_ID
  course_ID_traffic  <- reactive({ 
    req(input$course_chosen_traffic) 
    })
  
  #Helper function: predict grades
  my_predict <- function(model, profile){
    predict.cv.glmnet(object = model, newx = profile, s = "lambda.min")
  }
  
  #Helper: resettabe input
  output$resetable_input_traffic <- renderUI({
    times <- input$reset_input_traffic
    div(id = letters[(times %% length(letters)) + 1],
        checkboxGroupInput(
          inputId  = "course_chosen_traffic",
          label    = "Tentative Courses",
          choices  = course_advanced,
          selected = c("SSC2028", "SCI2040", "SSC2050", "HUM3034", "SSC3038"), #course_advanced,
          inline   = TRUE
        ))
  })
  
  ##OUTPUTS
  ###Table
  output$traffic_lights <- renderTable({
    
    #read vectors
    student_ID <- student_ID_traffic()
    course_ID <- course_ID_traffic()
    
  
    # preparatory <- d_prep %>%
    #   filter(target %in% course_ID) %>%
    #   group_by(target) %>%
    #   top_n(5, prep_score) %>%
    #   mutate(Preparation = paste(`Preparatory Courses`, collapse =" | ")) %>% 
    #   select(target, Preparation) %>%
    #   distinct
    
    preparatory <-  d_prep %>%
      filter(target %in% course_ID) %>%
      group_by(target) %>%
      top_n(5, prep_score) %>%
      left_join(titles, by = c("Preparatory Courses" = "Course ID")) %>%
      mutate(Preparation = paste(course_and_title, collapse =" | ")) %>% 
      select(target, Preparation) %>%
      distinct
    
    #predict
    student_prof <- student_profile_nest_app %>% 
      filter(`Student ID` == student_ID) %>%
      pull(profile) %>% .[[1]]
    
    #output
    fit_lasso_app %>%

      filter(target %in% course_ID) %>%

      mutate(prediction = cv %>% map_dbl(my_predict, student_prof)) %>%

      mutate(Red    = prediction < 5.5,
             Orange = prediction %>% between(5.5, 7),
             Green  = prediction > 7) %>%
      #gathered
      gather(key = Warning, value = Truth, Red:Green) %>% 
      filter(Truth == TRUE, Warning != "Green") %>% 
      select(-Truth, -cv, -prediction) %>%
      
      #expanded
      #mutate_at(vars(flag_red, flag_orange, flag_green), funs(ifelse(.,., " ")))%>%
      #select(-cv) %>%
     left_join(preparatory, by = "target") %>%
      rename(Course = target, `Preparatory Courses` = Preparation)
  
  })
  
  # ---------------------------------------------------------------------------------
  ## -- COURSE RECOMMENDATION --
  # ---------------------------------------------------------------------------------

  ###Set up
  use_past <- reactive({ input$use_past })

   # Helper functions:

  ##Return student topic profile
  student_topics <- function(d_student){

    student_TP %>% filter(`Student ID` == d_student) %>% top_n(1, time) %>%
      select(-time, -`Student ID`) %>%
      gather(key = topic, value = weight) %>%
      mutate(topic = str_replace(topic, "_", " "),
             weight = weight/sum(weight))

  }


  ##KL distance
  my_kl <- function(distribution, reference) {
    kl.dist(reference, tibble(distribution), base = 2)[[1]]
  }

  #Stemmer function
  stem_hunspell <- function(term) {

    stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
    n_stems <- length(stems)            # number of stems

    if (n_stems == 0) term              # if no stems, return original term
    else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem

  }

  #Cast function
  my_cast_dtm <- function(data) data %>%
    count(`Course ID`, word) %>%
    cast_dfm(`Course ID`, word, n)

  #Reactive student function
  student_tp <- reactive({
    student_topics(input$student_rec)
  })

  #Reactive article function
  article_tp <- reactive({
    req(input$article_input)

    article_raw <- pdf_text(input$article_input$datapath) %>% tibble(text =.) %>%
      .$text    %>%
      unlist(.) %>%
      unname()  %>%
      paste(., collapse = " ") %>%
      tibble(text = .) %>%
      unnest_tokens(output = word, input = text) %>%

      #--- for LDA---
      anti_join(stop_words) %>%
      mutate(word_old = word, word = word_old %>% map_chr(stem_hunspell))%>%
      inner_join(tibble(word = best_lda@terms), by = "word") %>%
      count(word) %>%
      right_join(tibble(word = best_lda@terms)) %>%
      mutate(n = replace(n, is.na(n),0)) %>%
      mutate(`Course ID` = "Article Input")      %>%
      cast_dfm(`Course ID`, word, n)
    #---    end    ---

    #Modify according to STM LDA:
    article_gamma <- posterior(best_lda, article_raw) %>% 
      .$topics    #returns a course by topic matrix

    #tidy
    article_gamma_tidy <-
      #---  for LDA ---
    tibble(topic = 1:length(article_gamma), gamma = article_gamma[1,]) %>%
      mutate(document = "Article Input", topic = paste("Topic ", topic, sep = ""))
      #---   end   ---
  })

  #recommendations
  recommendations_data <- reactive({

    beta_distribution  <- app_model$Beta[[1]]  # for LDA
    gamma_distribution <- app_model$Gamma[[1]] # for LDA
    course_titles      <- app_model$`Course Titles`[[1]]

    # Key words
    key_words_additional <- c(
      input$key_word_1,
      input$key_word_2,
      input$key_word_3,
      input$key_word_4,
      input$key_word_5
    )


    stem_hunspell <- function(term) {

      stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
      n_stems <- length(stems)            # number of stems

      if (n_stems == 0) term              # if no stems, return original term
      else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem

    }


    key_words_additional <- sapply(key_words_additional, stem_hunspell)

    key_words            <- c(input$key_words, key_words_additional)

    #Student profiles align with courses
    student <- list()
    student$topic_score <- beta_distribution %>%

      # Interest profile
      filter(
        term %in% key_words
      ) %>%
      group_by(
        topic
      ) %>%
      summarize(
        topic_score = sum(beta)
      ) %>%
      ungroup %>% ############### NORMALIZE
      mutate(student_interest = topic_score/sum(topic_score)) %>%
      select(-topic_score) %>%
      full_join(gamma_distribution %>%
                  spread(key = document, value = gamma),
                by = "topic")

    #KL distance
    ref <- student$topic_score %>% select(student_interest)

    kl_dist <- student$topic_score %>%
      select(-topic) %>%
      map_dbl(my_kl, reference = ref) %>%
      tibble(course = names(.), distance = .) %>%
      arrange((distance))

    #recommendations
    recommendations <- kl_dist %>%
      filter(course != "student_interest") %>%
      # Recommendations
      top_n(
        n  = -20,
        wt = distance
      ) %>%

      # Key words per recommendation
      left_join(
        gamma_distribution,
        by = c("course" = "document")
      ) %>%

      left_join(
        beta_distribution,
        by = "topic"
      ) %>%

      filter(
        term %in% key_words
      ) %>%

      group_by(
        term,
        course
      ) %>%
      summarize(
        word_contribution_to_document = sum(gamma * beta)
      ) %>%

      group_by(
        course
      ) %>%
      top_n(
        n  = 3,
        wt = word_contribution_to_document
      ) %>%
      arrange(desc(word_contribution_to_document)) %>%
      group_by(course) %>%
      summarize(
        key_words = paste(term, collapse = ", "),
        doc_score = sum(word_contribution_to_document)
      ) %>%
      ungroup %>%

      # Editing
      left_join(
        course_titles,
        by = c("course" = "Course ID")
      ) %>%
      arrange(desc(doc_score))
    })

  #Recommendations based on article---

  # recommendations_article <- reactive({
  #   beta_distribution  <- app_model$Beta[[1]]  # for LDA
  #   gamma_distribution <- app_model$Gamma[[1]] # for LDA
  #   course_titles      <- app_model$`Course Titles`[[1]]
  # 
  # 
  # 
  #    article_joined <- article_tp() %>%
  #      spread(key = document,value = gamma) %>%full_join(gamma_distribution %>%
  #                spread(key = document, value = gamma),
  #              by = "topic")
  # 
  #   #KL distance
  #   ref <- article_joined %>% select('Article Input')
  # 
  #   kl_dist <- article_joined %>%
  #     select(-topic) %>%
  #     map_dbl(my_kl, reference = ref) %>%
  #     tibble(course = names(.), distance = .) %>%
  #     arrange((distance))
  # 
  #   #recommendations
  #   recommendations <- kl_dist %>%
  #     filter(course != "Article Input") %>%
  #     # Recommendations
  #     top_n(
  #       n  = -20,
  #       wt = distance
  #       ) %>%
  #     # Editing
  #     left_join(
  #       course_titles,
  #       by = c("course" = "Course ID")
  #     )
  # 
  # })


  ##OUTPUTS
  ###Choice buttons

  # Input: button for the courses students wants to take following semester
  output$display_key_words <- renderUI({

    #Set up
    beta_distribution  <- app_model$Beta[[1]] # for LDA
    gamma_distribution <- app_model$Gamma[[1]] #for LDA
    course_titles      <- app_model$`Course Titles`[[1]]

    with_guess <- student_tp() %>%
      top_n(10, weight) %>%
      left_join(beta_distribution, by = "topic") %>%
      group_by(topic) %>%
      top_n(3, beta) %>% pull(term)

    kw_select <- if(use_past()){
      intersect(with_guess, kw_used_lda)
    } else {
      NA
    }

    #kw_select <- intersect(with_guess, app_model$kw[[1]])

    #Checkbox
      checkboxGroupInput(
        inputId  = "key_words",
        label    = "Academic Interest",
        choices  = sort(kw_used_lda),
        selected = kw_select,
        inline   = TRUE
      )
      # tags$head(tags$style("#key_words{color: black; font-size: 19px;}"))

    })


  ###Table
  output$course_recommendation <- renderTable({

    recommendations_data() %>%

      transmute(
        Code = course,
        Course = `Course Title`,
        `because you selected` = key_words
        ) %>%
      mutate(c_title = case_when(is.na(Course) ~ `Code`,
                                T ~ Course),
             length  = str_length(Code),

             Code    = case_when(length == 7 ~ Code,
                                     T ~ "unknown"),
             Course   = c_title) %>%
      select(-c_title, -length)


  })

  # ###Article output
  # output$pdfview <- renderTable({
  #   #article_tp()
  #  recommendations_article()
  # })
  
  # # ---------------------------------------------------------------------------------
  # ## -- STM COURSE RECOMMENDATION --
  # # ---------------------------------------------------------------------------------
  # 
  # ###Set up
  # use_past_stm <- reactive({ input$use_past_stm })
  # 
  # # Helper functions:
  # 
  # ##Return student topic profile
  # student_topics_stm <- function(d_student){
  #   
  #   student_TP %>% filter(`Student ID` == d_student) %>% top_n(1, time) %>%
  #     select(-time, -`Student ID`) %>%
  #     gather(key = topic, value = weight) %>%
  #     mutate(topic = str_replace(topic, "_", " "),
  #            weight = weight/sum(weight))
  #   
  # }
  # 
  # 
  # ##KL distance
  # my_kl <- function(distribution, reference) {
  #   kl.dist(reference, tibble(distribution), base = 2)[[1]]
  # }
  # 
  # #Stemmer function
  # stem_hunspell <- function(term) {
  #   
  #   stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
  #   n_stems <- length(stems)            # number of stems
  #   
  #   if (n_stems == 0) term              # if no stems, return original term
  #   else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
  #   
  # }
  # 
  # ##Get context function
  # get_context_words_stm <- function(document_id_text){ # needs as input a tibble with an id column and a text column
  #   
  #   document_id_text   %>%
  #     unnest_tokens(output = word, input = text, token = "sentences") %>%  #split into sentences
  #     
  #     mutate(Sentence = as.integer(rownames(.))) %>%
  #     
  #     unnest_tokens(output = word, input = word) %>%                      #split into words
  #     
  #     anti_join(stop_words, by = "word") %>%                              #remove stop words
  #     
  #     mutate(word_old = word,
  #            word = word_old %>% map_chr(stem_hunspell)) %>%              #stemm
  #     
  #     count(Sentence, word) %>%                                           #count occurance in each sentence
  #     
  #     mutate(in_corpus = case_when(
  #       
  #       word %in% best_lda@terms ~ T,
  #       T ~ F
  #       
  #     )) %>%                                             #deterine whether it is in the LDA corpus (oov)
  #     
  #     group_by(Sentence) %>%
  #     
  #     mutate(count_IN_corpus = sum(in_corpus),
  #            count_OUT_corpus = sum(!in_corpus)) %>%     #count words in corpus and out of corpus per sentence
  #     
  #     mutate(
  #       proportion_out_of_corpus = count_OUT_corpus/(count_IN_corpus + count_OUT_corpus) #what proportion of a sentence is out of the corpus?
  #       
  #     ) %>% # view histogram:    pull(proportion_out_of_corpus) %>% hist()
  #     filter( #proportion_out_of_corpus != 0,  #if all words are in the corpus, they are accounted for
  #       proportion_out_of_corpus  > .3, #words where we already captured the context
  #       proportion_out_of_corpus  < .6 ) %>% #words from which we can't reconstruct the context
  #     filter(in_corpus == T) %>%                         #keep words in corpus
  #     ungroup() %>%
  #     distinct(Sentence, word) %>% # count(word) %>% arrange(desc(n)) # see how many times we are adding each word: count(word) %>% arrange(desc(n))
  #     select(word) %>%
  #     mutate(word_old = "NA") #make it consistent with receiving tibble
  # }
  # 
  # 
  # #Cast function
  # my_cast_dtm <- function(data) data %>%
  #   count(`Course ID`, word) %>%
  #   cast_dfm(`Course ID`, word, n)
  # 
  # #Reactive student function
  # student_tp_stm <- reactive({
  #   student_topics_stm(input$student_rec_stm)
  # })
  # 
  # #Reactive article function
  # article_tp_stm <- reactive({
  #   req(input$article_input_stm) #######---------------------------------------this has not been tested in stm yet
  #   article_id_text <- pdf_text(input$article_input_stm$datapath) %>%
  #     tibble(text =.) %>%
  #     .$text    %>%
  #     unlist(.) %>%
  #     unname()  %>%
  #     paste(., collapse = " ") %>%
  #     tibble(text = .)
  #   
  #   context <- get_context_words_stm(article_id_text)
  #   
  #   article_raw <- article_id_text %>%
  #     unnest_tokens(output = word, input = text) %>%
  #     anti_join(stop_words, by = "word") %>%
  #     mutate(word_old = word,
  #            word     = word_old %>% map_chr(stem_hunspell)) %>%
  #     rbind(context) %>%
  #     count(word) %>%
  #     mutate(`Course ID` = "Article Input")  %>%
  #     
  #     #--- for LDA---
  #     # anti_join(stop_words) %>%
  #     # mutate(word_old = word, word = word_old %>% map_chr(stem_hunspell))%>%
  #     # inner_join(tibble(word = best_lda@terms), by = "word") %>%
  #     # count(word) %>%
  #     # right_join(tibble(word = best_lda@terms)) %>%
  #     # mutate(n = replace(n, is.na(n),0)) %>%
  #     # mutate(`Course ID` = "Article Input")      %>%
  #     # cast_dfm(`Course ID`, word, n)
  #     #---    end    ---
  #   
  #   #---   for STM ---
  #   # mutate(`Course ID` = "Article Input")      %>%
  #   #   mutate(word_old = word, word = word_old %>% map_chr(stem_hunspell)) %>%
  #   cast_dfm(`Course ID`, word, n) %>%
  #     asSTMCorpus() #for STM
  #   
  #   newdocs       <- alignCorpus(new = article_raw, old.vocab = model_data$vocab)
  #   
  #   #---     end  ---
  #   
  #   #Modify according to STM LDA:
  #   article_gamma <- fitNewDocuments(model = model_data$model, documents = newdocs$documents) #returns a course by topic matrix
  #   
  #   #tidy
  #   article_gamma_tidy <-
  #     #---   for stm   ---
  #     article_gamma$theta %>%
  #     tidy() %>%
  #     rename_all(funs(str_replace_all(., "X", ""))) %>%
  #     gather(key = topic, value = gamma) %>%
  #     mutate(document = "Article Input", topic = as.integer(topic))
  #   #---     end      ---
  # })
  # 
  # #recommendations
  # recommendations_data_stm <- reactive({
  #   
  #   beta_distribution  <- app_stm$Beta[[1]]  # for STM
  #   gamma_distribution <- app_stm$Gamma[[1]] # for STM
  #   course_titles      <- app_model$`Course Titles`[[1]]
  #   
  #   # Key words
  #   key_words_additional <- c(
  #     input$key_word_1_stm,
  #     input$key_word_2_stm,
  #     input$key_word_3_stm,
  #     input$key_word_4_stm,
  #     input$key_word_5_stm
  #   )
  #   
  #   
  #   stem_hunspell <- function(term) {
  #     
  #     stems   <- hunspell_stem(term)[[1]] # look up the term in the dictionary
  #     n_stems <- length(stems)            # number of stems
  #     
  #     if (n_stems == 0) term              # if no stems, return original term
  #     else              stems[[n_stems]]  # if multiple stems, return last (most basic) stem
  #     
  #   }
  #   
  #   
  #   key_words_additional <- sapply(key_words_additional, stem_hunspell)
  #   
  #   key_words            <- c(input$key_words_stm, key_words_additional)
  #   
  #   #Student profiles align with courses
  #   student <- list()
  #   student$topic_score <- beta_distribution %>%
  #     
  #     # Interest profile
  #     filter(
  #       term %in% key_words
  #     ) %>%
  #     group_by(
  #       topic
  #     ) %>%
  #     summarize(
  #       topic_score = sum(beta)
  #     ) %>%
  #     ungroup %>% ############### NORMALIZE
  #     mutate(student_interest = topic_score/sum(topic_score)) %>%
  #     select(-topic_score) %>%
  #     full_join(gamma_distribution %>%
  #                 spread(key = document, value = gamma),
  #               by = "topic")
  #   
  #   #KL distance
  #   ref <- student$topic_score %>% select(student_interest)
  #   
  #   kl_dist <- student$topic_score %>%
  #     select(-topic) %>%
  #     map_dbl(my_kl, reference = ref) %>%
  #     tibble(course = names(.), distance = .) %>%
  #     arrange((distance))
  #   
  #   #recommendations
  #   recommendations <- kl_dist %>%
  #     filter(course != "student_interest") %>%
  #     # Recommendations
  #     top_n(
  #       n  = -20,
  #       wt = distance
  #     ) %>%
  #     
  #     # Key words per recommendation
  #     left_join(
  #       gamma_distribution,
  #       by = c("course" = "document")
  #     ) %>%
  #     
  #     left_join(
  #       beta_distribution,
  #       by = "topic"
  #     ) %>%
  #     
  #     filter(
  #       term %in% key_words
  #     ) %>%
  #     
  #     group_by(
  #       term,
  #       course
  #     ) %>%
  #     summarize(
  #       word_contribution_to_document = sum(gamma * beta)
  #     ) %>%
  #     
  #     group_by(
  #       course
  #     ) %>%
  #     top_n(
  #       n  = 3,
  #       wt = word_contribution_to_document
  #     ) %>%
  #     arrange(desc(word_contribution_to_document)) %>%
  #     group_by(course) %>%
  #     summarize(
  #       key_words = paste(term, collapse = ", "),
  #       doc_score = sum(word_contribution_to_document)
  #     ) %>%
  #     ungroup %>%
  #     
  #     # Editing
  #     left_join(
  #       course_titles,
  #       by = c("course" = "Course ID")
  #     ) %>%
  #     arrange(desc(doc_score))
  # })
  # 
  # #Recommendations based on article---
  # 
  # recommendations_article_stm <- reactive({
  #   
  #   beta_distribution  <- app_stm$Beta[[1]]  # for STM
  #   gamma_distribution <- app_stm$Gamma[[1]] # for STM
  #   course_titles      <- app_model$`Course Titles`[[1]]
  #   
  #   
  #   
  #   article_joined <- article_tp_stm() %>%
  #     spread(key = document, value = gamma) %>%
  #     full_join(gamma_distribution %>% spread(key = document, value = gamma),
  #               by = "topic")
  #   
  #   #KL distance
  #   ref <- article_joined %>% select('Article Input')
  #   
  #   kl_dist <- article_joined %>%
  #     select(-topic) %>%
  #     map_dbl(my_kl, reference = ref) %>%
  #     tibble(course = names(.), distance = .) %>%
  #     arrange((distance))
  #   
  #   #recommendations
  #   recommendations <- kl_dist %>%
  #     filter(course != "Article Input") %>%
  #     # Recommendations
  #     top_n(
  #       n  = -20,
  #       wt = distance
  #     ) %>%
  #     # Editing
  #     left_join(
  #       course_titles,
  #       by = c("course" = "Course ID")
  #     )
  #   
  # })
  # 
  # 
  # ##OUTPUTS
  # ###Choice buttons
  # 
  # # Input: button for the courses students wants to take following semester
  # output$display_key_words_stm <- renderUI({
  #   
  #   #Set up
  #   beta_distribution  <- app_stm$Beta[[1]]  %>% mutate(topic = paste("Topic ", topic, sep = "")) # app_model$Beta[[1]] #
  #   gamma_distribution <- app_stm$Gamma[[1]] %>% mutate(topic = paste("Topic ", topic, sep = "")) # app_model$Gamma[[1]] #
  #   course_titles      <- app_model$`Course Titles`[[1]]
  #   
  #   with_guess <- student_tp_stm() %>%
  #     top_n(10, weight) %>%
  #     left_join(beta_distribution, by = "topic") %>%
  #     group_by(topic) %>%
  #     top_n(3, beta) %>% pull(term)
  #   
  #   kw_select <- if(use_past_stm()){
  #     intersect(with_guess, kw_used_stm)
  #   } else {
  #     NA
  #   }
  #   
  #   #kw_select <- intersect(with_guess, app_model$kw[[1]])
  #   
  #   #Checkbox
  #   checkboxGroupInput(
  #     inputId  = "key_words_stm",
  #     label    = "Academic Interest",
  #     choices  = sort(kw_used_stm),
  #     selected = kw_select,
  #     inline   = TRUE
  #   )
  #   # tags$head(tags$style("#key_words{color: black; font-size: 19px;}"))
  #   
  # })
  # 
  # 
  # ###Table
  # output$course_recommendation_stm <- renderTable({
  #   
  #   recommendations_data_stm() %>%
  #     
  #     transmute(
  #       Code = course,
  #       Course = `Course Title`,
  #       `because you selected` = key_words
  #     ) %>%
  #     mutate(c_title = case_when(is.na(Course) ~ `Code`,
  #                                T ~ Course),
  #            length  = str_length(Code),
  #            
  #            Code    = case_when(length == 7 ~ Code,
  #                                T ~ "unknown"),
  #            Course   = c_title) %>%
  #     select(-c_title, -length)
  #   
  #   
  # })
  # 
  # ###Test output
  # output$pdfview_stm <- renderTable({
  #   #article_tp_stm()
  #   recommendations_article_stm()
  # })
  # #______________________________________________________________________________end course recommendatios based on stm
}
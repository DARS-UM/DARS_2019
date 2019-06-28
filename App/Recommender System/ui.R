library(tidyverse)
library(tidytext)
library(shiny)
library(shinythemes)

#
#General set up---------------------------------------------------------------------------------

load("data_pillar_1.RDATA")
load("app_model.RDATA") #contains distribution, kw, course_all, course_following_semester.
load("app_stm.RDATA") 
load("rules_clean.RDATA")

kw_used_lda         <- app_model$kw[[1]] #is the same as in ui
kw_used_stm         <- app_stm$kw[[1]] # app_model$kw[[1]] #is the same as in ui

#
# ui ---------------------------------------------------------------------------------
navbarPage(
  theme = shinytheme("united"),
  
  # App title
  title = "Recommender System",
  
  # # ---------------------------------------------------------------------------------
  # # -- RED FLAGS --
  # # ---------------------------------------------------------------------------------
  # 
  # tabPanel(
  #   
  #   # Panel Title
  #   title = "Red Flag",
  #   
  #   # Sidebar layout
  #   sidebarLayout(
  #     
  #     # Sidebar panel for inputs
  #     sidebarPanel(
  #       
  #       # Input: student id number
  #       textInput(
  #         inputId = "student",
  #         label   = "Student ID",
  #         value   = "6113335"
  #         ),
  #       
  #       uiOutput('resetable_input'),
  #       tags$head(tags$style("#resetable_input{color: black; font-size: 14px;}")),
  #       tags$hr(),
  #       actionButton("reset_input", "Reset inputs")
  #       
  #       ),
  #     
  #     # Main panel for displaying outputs
  #     mainPanel(
  #       
  #       # Output: dataset
  #       tableOutput(
  #         outputId = "red_flags"
  #         ),
  #       tags$head(tags$style("#red_flags{color: black; font-size: 20px;}"))
  #       
  #       )
  #     
  #     )
  #   
  #   ),
  
  # ---------------------------------------------------------------------------------
  # -- TRAFFIC LIGHTS --
  # ---------------------------------------------------------------------------------
  
  tabPanel(
    
    # Panel Title
    title = "Traffic Lights",
    
    # Sidebar layout
    sidebarLayout(
      
      # Sidebar panel for inputs
      sidebarPanel(
        
        # Input: student id number
        textInput(
          inputId = "student_traffic",
          label   = "Student ID",
          value   = "6113335"
        ),
        
        uiOutput('resetable_input_traffic'),
        tags$head(tags$style("#resetable_input_traffic{color: black; font-size: 19px;}")),
        tags$hr(),
        actionButton("reset_input_traffic", "Reset inputs")
        
      ),
      
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: dataset
        tableOutput(
          outputId = "traffic_lights"
        ),
        tags$head(tags$style("#traffic_lights{color: black; font-size: 20px;}"))
        
      )
      
    )
    
  ),
  
  # ---------------------------------------------------------------------------------
  # -- COURSE RECOMMENDATIONS --
  # ---------------------------------------------------------------------------------

  tabPanel(

    # Panel Title
    title = "Course Recommender",


    # Sidebar layout
    sidebarLayout(

      # Sidebar panel for inputs
      sidebarPanel(
        # Input: student id number
        textInput(
          inputId = "student_rec",
          label   = "Student ID",
          value   = "6113335"
        ),

        #Input: button to use previous data and avoid cold start
        checkboxInput(
          inputId = "use_past",
          label = "Guess my interests"
        ),

        #Output: Dependant Input shows which courses to select: either empty or based on previous data
        uiOutput("display_key_words"),

        # Input: additional key word 1-5
        textInput(
          inputId = "key_word_1",
          label   = "Additional Key Word 1" #,
          #value   = "war"
          ),
        textInput(
          inputId = "key_word_2",
          label   = "Additional Key Word 2"
          ),
        textInput(
          inputId = "key_word_3",
          label   = "Additional Key Word 3"
          ),
        textInput(
          inputId = "key_word_4",
          label   = "Additional Key Word 4"
          ),
        textInput(
          inputId = "key_word_5",
          label   = "Additional Key Word 5"
          ) #,  #THIS COMMA IS NEEDED WHEN FOLLOWING SECTION IS INCLUDED ----------------------------------------------------------------------
        # fileInput("article_input", "Choose PDF File",
        #           accept = c(
        #             ".pdf")
        # )

        # fileInput(
        #   inputId = "article_input",
        #   label = "Recommend me based on this article:",
        #   accept = c('.pdf'),
        #   placeholder = "Upload file (.pdf only)")

        ),

      # Main panel for displaying outputs
      mainPanel(
        h3("Course Recommendations"),
        mainPanel( width = 12,
          tableOutput(outputId = "course_recommendation"),
          tags$head(tags$style("#course_recommendation{color: black; font-size: 20px;}")
                    ) #,  #THIS COMMA IS NEEDED WHEN FOLLOWING SECTION IS INCLUDED ----------------------------------------------------------------------
          # h3("Recommendations based on article"),
          # tableOutput("pdfview")
                  )

        )

      )

    ) #,      #THIS COMMA IS NEEDED WHEN FOLLOWING SECTION IS INCLUDED ----------------------------------------------------------------------
  # #___________________________________________________________________________________________
  # #                      COURSE RECOMMENDATIONS WITH STM
  # #--------------------------------------------------------------------------------------------
  # tabPanel(
  # 
  #   # Panel Title
  #   title = "STM Course Recommender",
  # 
  # 
  #   # Sidebar layout
  #   sidebarLayout(
  # 
  #     # Sidebar panel for inputs
  #     sidebarPanel(
  #       # Input: student id number
  #       textInput(
  #         inputId = "student_rec_stm",
  #         label   = "Student ID",
  #         value   = "6113335"
  #       ),
  # 
  #       #Input: button to use previous data and avoid cold start
  #       checkboxInput(
  #         inputId = "use_past_stm",
  #         label = "Guess my interests"
  #       ),
  # 
  #       #Output: Dependant Input shows which courses to select: either empty or based on previous data
  #       uiOutput("display_key_words_stm"),
  # 
  #       # Input: additional key word 1-5
  #       textInput(
  #         inputId = "key_word_1_stm",
  #         label   = "Additional Key Word 1" #,
  #         #value   = "war"
  #       ),
  #       textInput(
  #         inputId = "key_word_2_stm",
  #         label   = "Additional Key Word 2"
  #       ),
  #       textInput(
  #         inputId = "key_word_3_stm",
  #         label   = "Additional Key Word 3"
  #       ),
  #       textInput(
  #         inputId = "key_word_4_stm",
  #         label   = "Additional Key Word 4"
  #       ),
  #       textInput(
  #         inputId = "key_word_5_stm",
  #         label   = "Additional Key Word 5"
  #       ),
  # 
  #       fileInput("article_input_stm", "Choose PDF File",
  #                 accept = c(
  #                   ".pdf")
  #       )
  # 
  #       # fileInput(
  #       #   inputId = "article_input",
  #       #   label = "Recommend me based on this article:",
  #       #   accept = c('.pdf'),
  #       #   placeholder = "Upload file (.pdf only)")
  # 
  #     ),
  # 
  #     # Main panel for displaying outputs
  #     mainPanel(
  #       h3("Course Recommendations based on STM"),
  #       mainPanel( width = 12,
  #                  tableOutput(outputId = "course_recommendation_stm"),
  #                  tags$head(tags$style("#course_recommendation_stm{color: black; font-size: 20px;}")
  #                  ),
  #                  h3("Recommendations based on article"),
  #                  tableOutput("pdfview_stm")
  #       )
  # 
  #     )
  # 
  #   )
  # 
  # )
  # #--------------------------------------------------------------------------------------end of course recommendations with STM
  
  )

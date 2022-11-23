library(shiny)
library(tidyverse)
library(shinythemes)
library(ggplot2)
#  install.packages("emojifont")
#library(emojifont)

source("C:/Users/ARFOT/Documents/Actulab_final/Data_engineering.R")
source("C:/Users/ARFOT/Documents/Actulab_final/code_boostings.R")

model_ia <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_ia.rds")
model_re1 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re1.rds")
model_re2 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re2.rds")
model_re3 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re3.rds")
model_re4 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re4.rds")
model_re5 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re5.rds")
model_re6 <- readRDS(file = "C:/Users/ARFOT/Documents/Actulab_final/model_re6.rds")

choices_reas <- shopping %>% pull(ReassFinal) %>%  unique %>% as.character %>% sort()

#choices_reas <- c("Pas de filtre", shopping %>% pull(ReassFinal) %>%  unique %>% as.character %>% sort())
#choices_reas[which(choices_reas == "None")] <- "IA Groupe Financier"

choices_def <- c("Pas de filtre", "STD : accepté standard", "RAT : rated (surprimé)", "PP : postpone (remis)", 
                 "DEC : decline (refus)", "WIT : withdraw (annulé)")
  

choices_model <- c("Modèle IA", "Modèle Re1", "Modèle Re2",
  "Modèle Re3", "Modèle Re4", "Modèle Re5", "Modèle Re6")

#shopping %>% pull(DecisionFinale) %>%  unique %>% as.character %>% sort()

type_graphs <- c("Histogram", "Boxplot", "Bar")

shopping2 <- shopping %>% select( -c(which(names(shopping) == "CaseNumber"),
                        which(names(shopping) == "num_reas")) )

#continue_var <- shopping2 %>% dplyr::select_if(is.numeric) %>% colnames %>%  unique %>% as.character

continue_var <- c("Age", "Montant", "BMIDeclare", "Sexe", "DecisionFinale")

condition_var <- shopping2 %>% dplyr::select(contains("Condition")) %>% colnames %>%  unique %>% as.character 

condition_idre <- shopping2 %>% dplyr::select(contains("IndRe")) %>% colnames %>%  unique %>% as.character 

 
ui <- navbarPage(title = span("Du Shopping intelligent - Rostan"), theme = shinytheme("flatly"),
                 #img(src="C:/Users/ARFOT/Documents/Actulab/Shiny_app/OIP.png", align = "center"),
                 
                 tabPanel("Data vizualisation",
                           
                          
                          fluidRow( 
                            plotOutput('plots')
                            
                          ),
 
                          hr(),
                          
                          # checkboxInput("filtre", label = "Appliquer un filtre", value = FALSE),
                          
                          fluidRow(   
                            column( 
                            radioButtons("f_cont", "Variable", 
                                         choices = continue_var,
                                         selected = continue_var[1]
                                         ), 
                            width = 4),
                            
                            column(
                              selectInput(inputId = "var_condition", label = "Condition médicale", 
                                          choices = condition_var, 
                                          selected = condition_var[1], 
                                          multiple = F),
                              
                              selectInput(inputId = "var_idre", label = "Indice Réassureur consulté", 
                                          choices = condition_idre, 
                                          selected = condition_idre[1], 
                                          multiple = F),
                              
                              width = 4),
                            
                            column( 
                              radioButtons("type_graph", "Type de graphique", 
                                           choices = type_graphs,
                                           selected = type_graphs[1]
                              ),
                              width = 4)#,
                            
                            # column(#offset = 0.7,
                            #        radioButtons("f_df", "Filtre décision finale", 
                            #                     choices = choices_def,
                            #                     selected = choices_def[1]
                            #                     ),
                            #        width = 3) 
                          )),
                 
                 tabPanel("Classification Condition médicale",
                   
                          fluidRow( 
                            plotOutput('plots_2')
                            
                          ),
                          
                          hr(),
                          
                          fluidRow(
                            column( 
                              radioButtons("f_reas", "Réassureur final", 
                                           choices = choices_reas,
                                           selected = choices_reas[1]
                              ), 
                              width = 4),
                            
                            column( 
                              numericInput("nombre", label = h4("Nombre"), value = 5),
                              width = 4)
                          )
                 ),
                 
                 tabPanel("Predictions",
                          
                          fluidRow(
                            column(4,
                                   h3("Déclaration d'assurabilité de l'assuré : saisie des données"),
                                   sliderInput('age', 'Age', 
                                               min=1, max=100, value=15, 
                                               step=1, round=0), 
                                   
                                   numericInput("montant", label = h4("Montant d'assurance"), value = min(shopping$Montant), step = 25),
                                   
                                   sliderInput('imc', h4("Indice de masse corporelle"), 
                                               min=1, max=100, value=15, 
                                               step=0.01, round=2),
                                   
                                   radioButtons("sexe", label = h4("Sexe"),
                                                choices = c("F", "M"), 
                                                selected = "F"),
                                   br(), 
                                   
                                   selectInput(inputId = "conditions", label = h4("Conditions médicales"), 
                                               choices = condition_var, 
                                               selected = condition_var[1],
                                               multiple = T)
                                   
                            ),
                            column(4, 
                                   h3("Analyse de la déclaration"),
                                   selectInput(inputId = "reassureurs", label = "Réassureurs consultés", 
                                               choices = c("None", "Re1", "Re2", "Re3", "Re4", "Re5", "Re6"), 
                                               selected = "None", 
                                               multiple = T),
                                   
                                   selectInput(inputId = "model", label = "Modèle Prédictif", 
                                                  choices = choices_model, 
                                                  selected = choices_model[1], 
                                               multiple = F)
                            ),
                            column(4,
                                   h4(textOutput("selected_model")),
                                   actionButton("calculate", label = "Décision finale prédite", icon = icon("user-secret"), width = 500),
                                   verbatimTextOutput("to_final")
                            )
                          ) 
                          ),
                 tabPanel("Interprétation des modèles",
                          
                          fluidRow(
                            column(
                            radioButtons("p_ref", "Filtre Réassureur", 
                                         choices = choices_reas,
                                         selected = choices_reas[1]
                                        ),
                            width = 3) 
                          ))
)

server <- function(input, output, session) {
 
  # data_reas <- reactive({
  #     shopping2 %>% dplyr::filter(ReassFinal == input$f_reas)
  # })
  model <- reactive({input$model})
  
  output$plots <- renderPlot({
 
    p0 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "None") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("IA Groupe Financier") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p1 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re1") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re1") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p2 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re2") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re2") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p3 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re3") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re3") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p4 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re4") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re4") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p5 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re5") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re5") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    p6 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re6") %>% 
      ggplot(aes(x = get(input$f_cont))) + 
      geom_histogram() + 
      ggtitle("Re6") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    
    q0 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "None") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("IA Groupe Financier") + 
      ylab("Frequence") +
      xlab(input$f_cont) 
    
    q1 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re1") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("Re1") + 
      ylab("Frequence") +
      xlab(input$f_cont) 
    
    q2 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re2") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("Re2") + 
      ylab("Frequence") +
      xlab(input$f_cont)
 
    q3 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re3") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("Re3") + 
      ylab("Frequence") +
      xlab(input$f_cont)
 
    
    q4 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re4") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() +  
      ggtitle("Re4") + 
      ylab("Frequence") +
      xlab(input$f_cont)
 
    q5 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re5") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("Re5") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    q6 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re6") %>% 
      ggplot(aes(y = get(input$f_cont))) + 
      geom_boxplot() + 
      ggtitle("Re6") + 
      ylab("Frequence") +
      xlab(input$f_cont)
    
    if (input$f_cont == "BMIDeclare") {
        q0 <- q0 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") +
          geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          geom_text(aes(0, 30,label = 30, hjust = 6.3, vjust = - 0.9))
        
        q1 <- q1 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5))
        
        q2 <- q2 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5))
        
        q3 <- q3 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5))
        
        q4 <- q4 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5))
        
        q5 <- q5 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5))
        
        q6 <- q6 + geom_hline(aes(yintercept = 18.5, colour = "red"), size = 1.1) + 
          geom_hline(aes(yintercept = 30, colour = "red"), size = 1.1) +
          theme(legend.position="none") #+
          #geom_text(aes(0, 18.5,label = 18.5, hjust = 4, vjust = 1.5)) + 
          #geom_text(aes(0, 30,label = 30, hjust = 4, vjust = 1.5)) 
    }
    
    t0 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "None") %>%  
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar() + 
      ggtitle("IA Groupe Financier") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    
    t1 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re1") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar() + 
      ggtitle("Re1") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    t2 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re2") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar( ) + 
      ggtitle("Re2") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    t3 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re3") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar( ) + 
      ggtitle("Re3") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    t4 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re4") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar( ) +  
      ggtitle("Re4") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    t5 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re5") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar( ) + 
      ggtitle("Re5") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    t6 <- shopping2 %>% 
      dplyr::filter(ReassFinal == "Re6") %>% 
      ggplot(aes(x = get(input$f_cont), y = ..count../sum(..count..))) + 
      geom_bar( ) + 
      ggtitle("Re6") + 
      ylab("Frequence") +
      xlab(input$f_cont) +
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) + 
      geom_text(aes(label = paste0(round(..count../sum(..count..), 3)*100, " %")),
                vjust = -0.5, stat='count',
                color = "blue", 
                fontface = "bold") + 
      geom_text(aes(label = paste0("(", ..count.., ")")),
                vjust = 1.5, stat='count',
                color = "green", 
                fontface = "bold")
    
    if (input$type_graph == "Histogram") {
      gridExtra::grid.arrange(p0, p1, p2, p3, p4, p5, p6, nrow = 1)
    } 
    else if (input$type_graph == "Boxplot") {
  
      gridExtra::grid.arrange(q0, q1, q2, q3, q4, q5, q6, nrow = 1)
    }
    else if (input$type_graph == "Bar") {
      
      gridExtra::grid.arrange(t0, t1, t2, t3, t4, t5, t6, nrow = 1)
    }
    
  })  
  
   indice_cond <- reactive({
      if (input$f_reas == "None") { 1 }
     else if (input$f_reas == "Re1") { 2 }
     else if (input$f_reas == "Re2") { 3 }
     else if (input$f_reas == "Re3") { 4 }
     else if (input$f_reas == "Re4") { 5 }
     else if (input$f_reas == "Re5") { 6 }
     else if (input$f_reas == "Re6") { 7 }
   }) 
   
   choices_model <- c("Modèle IA", "Modèle Re1", "Modèle Re2",
                      "Modèle Re3", "Modèle Re4", "Modèle Re5", "Modèle Re6")
   
   final_model <- reactive({
     if (input$model == "Modèle IA") { model_ia }
     else if (input$model == "Modèle Re1") { model_re1 }
     else if (input$model == "Modèle Re2") { model_re2  }
     else if (input$model == "Modèle Re3") { model_re3  }
     else if (input$model == "Modèle Re4") { model_re4  }
     else if (input$model == "Modèle Re5") { model_re5  }
     else if (input$model == "Modèle Re6") { model_re6  }
   })
   
   final_reas <- reactive({
     if (input$model == "Modèle IA") { "None" }
     else if (input$model == "Modèle Re1") { "Re1" }
     else if (input$model == "Modèle Re2") { "Re2"  }
     else if (input$model == "Modèle Re3") { "Re3"}
     else if (input$model == "Modèle Re4") { "Re4"}
     else if (input$model == "Modèle Re5") {"Re5"}
     else if (input$model == "Modèle Re6") {"Re6"}
   })
   
   to_data3 <- reactive({
     dt <- data_cond[, c(8, indice_cond())]
     names(dt) <- c("condition", "freas")
     dt
   }) 
   
output$plots_2 <- renderPlot({ 
 
  ia1 <- to_data3()  %>% 
    slice_max(freas, n = input$nombre) %>%
    ggplot(aes(x = reorder(condition, -freas))) +  
    geom_bar(aes(y = freas), stat="identity")  +
    labs(x = "Conditions les plus acceptées", title = input$f_reas)+
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  ia2 <- to_data3() %>% 
    slice_min(freas, n = input$nombre) %>%
    ggplot(aes(x = reorder(condition, +freas))) +  
    geom_bar(aes(y = freas), stat="identity") +
    labs(x = "Conditions les moins acceptées", title = input$f_reas)+
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  gridExtra::grid.arrange(ia1, ia2, nrow = 1)
})
  ## update f_cont
  observeEvent( 
    eventExpr = {
      input$var_condition
      input$var_idre
    },           
    handlerExpr = { 
    updatedValues <- c(continue_var, input$var_idre, input$var_condition)
    updateRadioButtons(session, "f_cont", choices = updatedValues)
  })
 
 
  ## Nom du modèle pour le visuel
  output$selected_model <- renderText({
    paste("Prédiction faite pour le ", model())
  })
  
  output$to_final <- renderPrint({
    new_data <- shopping
    id1 <- which(colnames(new_data) == "IndRe1")
    
    new_data[1, "Age"] <- as.integer(input$age)
    new_data[1, "Montant"] <- as.integer(input$montant)
    new_data[1, "BMIDeclare"]  <- as.numeric(input$imc)
    new_data[1, "Sexe"] <- as.factor(input$sexe)    
    
    new_data[1, id1:ncol(new_data)] <- "0"
    
    new_data[1, id1:(id1 + 5)] <-  sapply(input$reassureurs, function(x) {
        # c("Aucun", "Re1", "Re2", "Re3", "Re4", "Re5", "Re6")
        to_return <- rep("0", 6)
        for (i in 1:6) {
          if (x == paste0("Re", i)) to_return[i] <- "1"
        }
        to_return
      })
    
    new_data[1, (id1 + 6) : ncol(new_data)] <-  sapply(input$conditions, function(x) {
      to_return <- rep("0", 48)
        for (i in 1:48) {
          if (x == paste0("Condition", i)) to_return[i] <- "1"
        }
      to_return
      })
    
    nlev <- nlevels(to_split(reas = final_reas())$data_train$DecisionFinale)
    v_matrix <- xgb_prepar(dataset2 = new_data, reas2 = final_reas())$valid_matrix
    preds <- predict(final_model(), newdata = v_matrix, type = "prob")
    
    final <- NULL
    if (nlev == 2) {
      final <- case_when(which.max(preds[1:nlev]) - 1 == 0 ~ "Surprime",
                         which.max(preds[1:nlev]) - 1 == 1 ~ "Standard",
                         TRUE ~ "Error" 
      )
    }
    
    if (nlev == 4) {
      final <- case_when(which.max(preds[1:nlev]) - 1 == 0 ~ "Refus",
                         which.max(preds[1:nlev]) - 1 == 1 ~ "Remis",
                         which.max(preds[1:nlev]) - 1 == 2 ~ "Surprime",
                         which.max(preds[1:nlev]) - 1 == 1 ~ "Standard",
                         TRUE ~ "Error" 
      )
    }
    
    final
  })
  
}


shinyApp(ui = ui, server = server)

#runApp('C:/Users/ARFOT/Documents/Actulab/Shiny_app/main_app')
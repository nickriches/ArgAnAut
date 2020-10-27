    
    library(shiny)
    library(tidyverse)
    library(readtext)
    library(gridExtra)
    library(cowplot)
    library(knitr)
    
    shinyApp(
      
      ui <- fluidPage(
        
        navbarPage("ArgAnAut",
        
        tabPanel("(1) Instructions",
                 uiOutput('Rmarkdown_instructions')
        ),
        
        tabPanel("(2) Analyse data",
        
          sidebarPanel(h1("Enter text"),
                   radioButtons("radio", label = h3("How do you wish to enter your data?"),
                                choices = list("Upload file (.doc, .docx, or .txt)" = 1, "Enter text in textbox" = 2), 
                                width = '100%', selected = 1),
                   conditionalPanel(condition = "input.radio == 1",
                                    fileInput("text_file", "Select file",
                                              multiple = FALSE,
                                              accept = c("text/plain",
                                                         "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                                                         "application/msword")
                                    )
                   ),
                   conditionalPanel(condition = "input.radio == 2",
                                    textAreaInput("text_file_TA", "Enter text here...",
                                                  placeholder = "Enter text here...",
                                                  width = "100%", height = "100%", resize = "both")
                                    # verbatimTextOutput("value")
                   )
                ), # End of sidebarPanel

          mainPanel(
            h1("ArgAnAut"),
            h3("(Aut-omatic, An-analysis of Arg-ument Structure)"),
            h5("Pred1 = Perc. of predicates with 1 argument, \n
                Pred2 = Perc. of predicates with 2 arguments,\n
                Pred3 = Perc. of predicates with 3 arguments,\n
                UTS = Unidentified Thematic Structures (verbs with no args, or args with no verbs),\n
                mean_PAS = Average valency,\n
                Arg_Om = Perc. of arguments omitted"),
            plotOutput("ui_plot")
            ) # End of mainPanel
           ) # End of (2) Analyse Data tabPanel
         ) # End of navbarPage
        ), # End of fluidPage

      
      server <- function(input, output, session){
        
        
        output$Rmarkdown_instructions <- renderUI({
          HTML(markdown::markdownToHTML(knit('Rmarkdown_instructions.Rmd',
                                             )))
        })
        
        
        text <- reactive({
          
          if(is.null(input$text_file)) return(NULL)
  
          if(!is.null(input$text_file)){
            text <- readtext(input$text_file$datapath)$text
          }
          
          return(text)
          
        })
        
        
        # Reactive expression to read data file.
        
        df <- reactive({
          
          req(text())
          
          #### Clean up data ====
          
          text_vector <- unlist(strsplit(text(), "==="))
          
          df.result <- as.data.frame(NULL)
          
          # This statement can be used to run the code line by line
          # browser(); one <- 1; one <- 1; one <- 1; one <- 1
          
          for(i in 1:length(text_vector)){
            
          text <- text_vector[i]
          
          ########
          ########
          
          # This is just a file I use to test the code.
          # text <- readtext::readtext("MSc Semantic Analysis - my answers.docx")

          # Idenfity all labels (consist of non-label character: [^A|V|X|I]). This creates a VECTOR.
          all_labels <- unlist(str_extract_all(text, "[\x20]*[\x2c]*[\x2e]*[\x0A]*[\x0D]*[A|V|X|I]+[\x20]*[\x2c]*[\x2e]*[\x0A]*[\x0D]*"))
          # non-target characters: \x20 = space, \x2c = comma, \x2e = period, \x0A = next line, \x0D= return
          # Code identifies a series of one more more non-target characters, e.g. "..."
          
          df <- as.data.frame(all_labels)
          
          #Function to trim non-target characters from beginning or end
          trimNTC <- function(x){
            result <- trimws(x, whitespace = "[\x20]*[\x2c]*[\x2e]*[\x0A]*[\x0D]*")
            return(result)
          }
          
          #Trim non-target characters from beginning and end
          df$all_labels <- sapply(df$all_labels, trimNTC)
          
          # Remove unwanted single characters
          # Remove first person pronoun I and indefinite article "A"
          df %>%
            filter(all_labels != "I") %>%
            filter(all_labels != "A") -> df

          # Various functions ====
          
          # Various functions to count As, count Xs, identify labels with isolated Vs, identify labels with isolate As
          
          # Detects isolated verb if string contains a V and no A or I
          isolated_verb <- function(x){
            return <- FALSE
            if((str_detect(x, "V") == TRUE) &
               (str_detect(x, "A") == FALSE) &
               (str_detect(x, "I") == FALSE)) {return <- TRUE}
            return(return)
            }
          
          # Detects isolated argument if string contains an A, but no V. Also in case of ellipsis, e.g.
          # "and Jane did"  this is not coded as an isolated argument
          isolated_argument <- function(x){
            return <- FALSE
            if((str_detect(x, "A") == TRUE) &
               (str_detect(x, "V") == FALSE) &
               (x != "AI")) {return <- TRUE}# e.g. "and Jack did"
            return(return)
          }
          
          # Create UTS variable (from either isolated arguments, e.g. AX, or isolated verbs, e.g. XV)
          
          df$isolated_argument <- sapply(df$all_labels, isolated_argument)
          df$isolated_verb <- sapply(df$all_labels, isolated_verb)
          
          df$UTS <- as.numeric(df$isolated_argument == TRUE |
                               df$isolated_verb == TRUE)
          
          # Count number of arguments per line (counts number of As)
          
          count_args <- function(x){
            return(str_count(x, "A"))
          }
          
          has_verb <- function(x){
            return(str_detect(x, "V"))
          }
          
          # Where proposition has a Verb, replace Is (Implicit Arguments) with As for the purposes of counting
          
          df$has_verb <- sapply(df$all_labels, has_verb)
          
          I2A <- function(x){
            return(str_replace_all(x, "I", "A"))
          }
          
          df$all_labels[which(df$has_verb == TRUE)] <- sapply(df$all_labels[which(df$has_verb == TRUE)], I2A)
          
          df$num_args <- sapply(df$all_labels, count_args)
          
          df$pred1 <- as.numeric(df$num_args == 1); df$pred1[which(df$UTS == TRUE)] <- 0 # NB percentages need to exclude UTS
          df$pred2 <- as.numeric(df$num_args == 2); df$pred2[which(df$UTS == TRUE)] <- 0 # NB percentages need to exclude UTS
          df$pred3 <- as.numeric(df$num_args == 3); df$pred3[which(df$UTS == TRUE)] <- 0 # NB percentages need to exclude UTS
          
        
          pred1 <- round(100*mean(df$pred1, na.rm = TRUE), 1)
          pred2 <- round(100*mean(df$pred2, na.rm = TRUE), 1)
          pred3 <- round(100*mean(df$pred3, na.rm = TRUE), 1)
          UTS <- round(100*mean(df$UTS), 1)
          
          # Calculate mean_PAS (mean number of arguments). NB this excludes utterances which do not have a verb.
          # And it includes propositions with omitted arguments.
          
          df %>% filter(has_verb == TRUE) %>% summarise(num_args = mean(num_args)) -> mean_PAS.df
          
          mean_PAS <- round(mean_PAS.df$num_args[1], 2)
          
          
          
          # Calculate Arg_om (percentage of arguments omitted)
          # NB this is for any utterance with a verb
          
          count_As_and_Xs <- function(x){
            return(str_count(x, "A") + str_count(x, "X"))
          }
          
          count_Xs <- function(x){
            return(str_count(x, "X"))
          }
          
          df$num_As_and_Xs <- sapply(df$all_labels, count_As_and_Xs)
          df$num_Xs <- sapply(df$all_labels, count_Xs)
          df$has_X <- as.numeric(df$num_Xs >= 1)
        
          
          df %>%
            filter(num_As_and_Xs >= 2) %>% 
            summarise(arg_om <- mean(has_X)) ->
            df.Arg_om
          
          Arg_om <- round(100*(df.Arg_om[1,1]), 1)
          
          class <- c("UTS", "pred1", "pred2", "pred3", "Arg_om", "mean_PAS")
          
          perc <- c(UTS, pred1, pred2, pred3, Arg_om, mean_PAS)
          
          df.table <- data.frame(class, perc)
          
          df.table$id <- paste("Participant", i)
          
          df.table$upper <- NA
          
          df.table$lower <- NA
          
          df.result <- rbind(df.result, df.table)
        
          # browser(); one <- 1; one <- 1; one <- 1
          
          } # end of for-loop
        

          
          freq <- c(NA, NA, NA, NA, NA, NA)
          perc <- c(2.54, 12.83, 58.02, 20.28, 0.15, 2.08)
          lower <- c(0, 2.91, 41.37, 7.48, 0, 1.87)
          upper <- c(8.45, 22.74, 74.67, 33.08,  1.09, 2.30)
          
          id <- c("norms", "norms", "norms", "norms", "norms", "norms")
          
          df.norms <- data.frame(class, perc, id, upper, lower)
          
          df.result <- rbind(df.result, df.norms)

          return(df.result)
          
          })
    
        
        output$ui_plot <- renderPlot({
      
            req(df())
            
            # First Pred1 - Pred3 and UTS
          
            g1 <- ggplot(df() %>% filter(id != "norms") %>%
                         filter(class == "UTS" | class == "pred1" | class == "pred2" | class == "pred3"),
                         aes(fill = id, x = class, y = perc))
            
            g1 <- g1 + geom_text(aes(label=perc),
                                 position=position_dodge(width=0.9), vjust=-0.25,
                                 colour="grey40",
                                 size=3)
            
            g1 <- g1 + geom_bar(stat="identity", position=position_dodge())
            
            g1 <- g1 + geom_errorbar(data = df() %>% filter(id == "norms") %>%
                                     filter(class == "UTS" | class == "pred1" | class == "pred2" | class == "pred3"),
                                     aes(x = class, ymin = lower, ymax = upper),
                                     colour = "red")
            
            g1 <- g1 + geom_point(data = df() %>% filter(id == "norms") %>%
                                  filter(class == "UTS" | class == "pred1" | class == "pred2" | class == "pred3"),
                                  aes(x = class, y = perc),
                                  size=2, shape=23, fill="red", colour = "red")
            
            g1 <- g1 + theme_bw()
            
            g1 <- g1 + theme(legend.position = "none")
                        
            g1 <- g1 + theme(axis.title.x=element_blank())
            
            g2 <- ggplot(df() %>% filter(id != "norms") %>%
                         filter(class == "mean_PAS"),
                         aes(fill = id, x = class, y = perc))
            
            g2 <- g2 + geom_text(aes(label=perc),
                                 position=position_dodge(width=0.9), vjust=-0.25,
                                 colour="grey40",
                                 size=3)
            
            g2 <- g2 + geom_bar(stat="identity", position=position_dodge())
            
            g2 <- g2 + geom_errorbar(data = df() %>% filter(id == "norms") %>%
                                     filter(class == "mean_PAS"),
                                     aes(x = class, ymin = lower, ymax = upper),
                                     colour = "red")
            
            g2 <- g2 + geom_point(data = df() %>% filter(id == "norms") %>%
                                  filter(class == "mean_PAS"),
                                  aes(x = class, y = perc),
                                  size=2, shape=23, fill="red", colour = "red")
            
            g2 <- g2 + theme_bw()
            
            g2 <- g2 + theme(legend.position = "none")
            
            g2 <- g2 + theme(axis.title.x=element_blank())
            
            g2 <- g2 + labs(y="Mean number of args per proposition")

            g3 <- ggplot(df() %>% filter(id != "norms") %>%
                           filter(class == "Arg_om"),
                         aes(fill = id, x = class, y = perc))
            
            g3 <- g3 + geom_text(aes(label=perc),
                                 position=position_dodge(width=0.9), vjust=-0.25,
                                 colour="grey40",
                                 size=3)
            
            g3 <- g3 + geom_bar(stat="identity", position=position_dodge())
            
            g3 <- g3 + geom_errorbar(data = df() %>% filter(id == "norms") %>%
                                       filter(class == "Arg_om"),
                                     aes(x = class, ymin = lower, ymax = upper),
                                     colour = "red")
            
            g3 <- g3 + geom_point(data = df() %>% filter(id == "norms") %>%
                                    filter(class == "Arg_om"),
                                  aes(x = class, y = perc),
                                  size=2, shape=23, fill="red", colour = "red")
            
            g3 <- g3 + theme_bw()
            
            g3 <- g3 + theme(axis.title.x=element_blank())
            
            g3 <- g3 + labs(y="Perc. of 2 or 3-place predicates with args. omitted")
            
            # g3
            
            
            # grid.arrange(g1, g2, g3, ncol=3)
            
            plot_grid(g1, g2, g3, align = "h", nrow = 1,
                      rel_widths = c(4, 1.7, 3.3))
            
    
        }, height = 400, width = 500)
      
        }
      
        )
    
    # shinyApp(ui, server)
    
  
    # https://gist.github.com/wch/5436415/
    
    # https://stackoverflow.com/questions/23906512/making-error-bars-on-one-group-in-bar-chart-in-ggplot2
    
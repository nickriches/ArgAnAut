    
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
          
          text_vector <- unlist(strsplit(text(), "==="))
          
          df.result <- as.data.frame(NULL)
          
          # browser(); one <- 1; one <- 1; one <- 1; one <- 1
          
          for(i in 1:length(text_vector)){
            
          text <- text_vector[i]
          
          # text <- readtext::readtext("1819 Second argument structure analysis session - WITH CODES CG and MK.docx")
          
          # Idenfity all labels (consist of non-label character: [^A|V|X|I])
          all_labels <- unlist(str_extract_all(text, "[\x20|\x2c|\x2e|\n|\r][A|V|X|I]+[\x20|\x2c|\x2e|\n|\r]"))
          # \x20 = space, \x2c = comma, \x2e = period, \n = next line, \r = return
          
          #Trim 
          all_labels <- sapply(all_labels, trimws)
          
          # Remove first person pronoun I
          all_labels <- all_labels[which(all_labels != "I")]
          all_labels_merged <- paste0(all_labels, collapse = "")
        
          # Calculate total number of labels
          tot_all_labels <- length(all_labels)
          
          # Identify labels containing V
          all_labels_with_V <- all_labels[which(grepl("V", all_labels))]
          
          # Identify labels without V
          # all_labels_without_V <- all_labels[which(grepl("V", all_labels) == FALSE)]
          
          # Calculate percentage of arguments omitted. NB we do this using all labels with a verb
          
          all_labels_with_V_merged <- paste0(all_labels_with_V, collapse= "")
          total_arg_om <- nchar(str_extract_all(all_labels_with_V_merged, "X"))
          
          total_arg <- nchar(str_extract_all(all_labels_merged, "A"))
          
          perc_arg_om <- (total_arg_om / total_arg)*100
          
          count_As <- function(x){
            return(str_count(x, "A"))
          }
          
          has_As <- sapply(all_labels, count_As)
          
          mean_As <- mean(has_As)
          
          tot_As <- count_As(all_labels_merged)
          
          count_Xs <- function(x){
            return(str_count(x, "X"))
          }
          
          has_Xs <- sapply(all_labels, count_Xs)
          
          mean_Xs <- mean(has_Xs)
          
          tot_Xs <- count_Xs(all_labels_merged)
          
          perc_arg_om <- (tot_Xs / (tot_As + tot_Xs))*100
          
          count_Vs <- function(x){
            return(str_count(x, "V"))
          }
          
          num_Vs <- sapply(all_labels, count_Vs)
          
          mean_Vs <- mean(num_Vs)
          
          UTS <- length(which(num_Vs == 0))
          
          perc_labels_UTS <- (UTS/tot_all_labels)*100
    
          pred1_tot <- length(which(has_As == 1))
          pred2_tot <- length(which(has_As == 2))
          pred3_tot <- length(which(has_As == 3))
          
          pred1_perc <- 100*(pred1_tot/tot_all_labels)
          pred2_perc <- 100*(pred2_tot/tot_all_labels)
          pred3_perc <- 100*(pred3_tot/tot_all_labels)
          
          mean_PAS <- mean(c(rep(1, times = pred1_tot), rep(2, times = pred2_tot), rep(3, times = pred3_tot)))
          
          class <- c("UTS", "pred1", "pred2", "pred3", "Arg_om", "mean_PAS")
          
          perc <- c(perc_labels_UTS, pred1_perc, pred2_perc, pred3_perc, perc_arg_om, mean_PAS)
          
          df.table <- data.frame(class, perc)
          
          df.table$id <- paste("Participant", i)
          
          df.table$upper <- NA
          
          df.table$lower <- NA
          
          df.result <- rbind(df.result, df.table)
        
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
            
            # g2
            
            
            
            
            g3 <- ggplot(df() %>% filter(id != "norms") %>%
                           filter(class == "Arg_om"),
                         aes(fill = id, x = class, y = perc))
            
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
    
shinyServer(
  
  function(input, output, session){
    
    
    files_sanjayo1<-reactive({
      req(input$files_sanjay)
      req(input$sub_san)
      f2a= data.frame(input$files_sanjay) %>% 
        separate("type", c("type_1", "extension"), sep ="\\/") %>% 
        filter(extension=='pdf') %>%
        mutate(`Match percent`= 0, `Keywords matched`= "None")
      
      
    })
    #####################################
    
    
    
    
    #######################################################################
    observeEvent(input$files_sanjay, output$up<-renderUI(actionButton("sub_san", label = "Upload!")))
    
    
    
    
    
    
    #criterias###############
    
    
    reactive_df <- reactiveValues(df1_sanjay = data.frame(filter_criteria = character()), find_val= "")
    observeEvent(reactive_df$df1_sanjay, reactive_df$df1_sanjay <- subset(reactive_df$df1_sanjay, filter_criteria != "()"))
    
    
    
    observeEvent(input$sub_san, {
      output$dnd_crit_enter <- renderUI({
      
      # text_inputs
      req(input$files_sanjay)
      textInput("texts", "Enter critera")
      
    })
      
      output$submit_btn <- renderUI({
        
        req(input$files_sanjay)
        actionButton("ec_input", "Submit!")
        
      })
      
      
      observeEvent(input$ec_input,{
                   
                   output$dnd_crit_enter <- renderUI({
                     
                     # text_inputs
                     req(input$files_sanjay)
                     textInput("texts", "Enter critera")
                     
                   })
                   
                   output$find<- renderUI({
                     
                     actionButton("find", "Find!")
                     
                   })
                   
                   output$rem_all<- renderUI({
                     
                     
                     actionButton("rem_all", "Remove all criteria!")
                     
                   })
                   
                   }
                   )
      
      
      #remove_all_crit
      
      observeEvent(input$rem_all, reactive_df$df1_sanjay<-data.frame(filter_criteria = character()))
      
      ##############
      observeEvent(input$ec_input, reactive_df$df1_sanjay <- reactive_df$df1_sanjay %>% add_row("filter_criteria" = tolower(str_squish(input$texts)), .after = nrow(reactive_df$df1_sanjay)))
      
      
      
      observeEvent(reactive_df$df1_sanjay, output$dnd_ui <- renderUI({
        bucket_list(
          header = h4("Keyword list"),
          group_name = "bucket list",
          orientation = "horizontal",
          # rows from sanjay_df1[filter_criteria] is passed to Criterias
          add_rank_list("Keywords:", labels = c(reactive_df$df1_sanjay$filter_criteria), input_id = "crts"),
          # The values from Criterias can be dragged to any of the columns or ranked.
          
          add_rank_list("Remove keywords", labels = NULL, input_id = "grp_here")
        )
      }))
      
      observeEvent(input$grp_here, reactive_df$df1_sanjay <- reactive_df$df1_sanjay %>% filter(!(filter_criteria %in% input$grp_here)))
      
      observeEvent(input$find, reactive_df$find_val<-
                     
                     c(input$crts)
                     
                     )
      
      hamal<- reactiveValues(hml= data.frame(name="NULL", match_percent= 0, keywords="NULL"))
      #Actual algorithm-----------------------------------------
      observeEvent(input$find, {
        req(input$find)
        req(input$files_sanjay)
        req(length(reactive_df$find_val)>=1)
        
        hamal$hml<- data.frame(name="NULL", match_percent= 0, keywords="NULL")
        
        #*******************************
        for(items in seq(nrow(files_sanjayo1()))){
          
          
          text1<- c(extract_text((files_sanjayo1())[items,"datapath"]))
          text2<- strsplit(str_squish(gsub("[^[:alnum:] #.@\r\n]", "", text1)), "\r\n")
          text3<- tolower(gsub("\\. ", " ", text2))
          cv<- data.frame(name=1, text= text3)
          srch<- reactive_df$find_val
          srch_num= vector()
          
          for (i in seq(length(srch))){
            srch_num[i] = c(str_count(srch[i],"\\w+" ))
            
          }
          srch_num_uniq <- unique(srch_num)
          
          for(i in seq(length(srch_num_uniq))){
            
            assign(paste0("df", as.character(srch_num_uniq[i])),
                   
                   cv %>% unnest_tokens(find_here, text, token = "ngrams", n = srch_num_uniq[i])
                   
            )
            
            
          }
          
          
          count=0
          kwds= ""
          for(i in seq(length(srch_num))){
            
            if(nrow(  eval(str2expression(paste0("df", srch_num[i]))) %>% filter(find_here==srch[i])   )>0){
              
              count= count+1
              kwds= paste0(kwds, "[" , srch[i], "] ")
            }
            
          }
          
          match_percent<- (count/length(srch))*100
          
          #
          
          hamal$hml <- hamal$hml %>% add_row("match_percent" = match_percent, "name"=(files_sanjayo1())[items,"name"],"keywords" = kwds,  .after = nrow(hamal$hml))
          ##
          count=NULL
          kwds=NULL
          cv = NULL
          #exists(deparse(substitute(x)))
          for(i in seq(length(srch_num_uniq))){
            
            assign(paste0("df", as.character(srch_num_uniq[i])),
                   
                   NULL
                   
            )
            
            
          }
          
          
        }
        
        #**************************************
        }
       )
      
      #____________________________________________________________ 
      observeEvent(input$find, output$sliders<-renderUI({
        req(length(reactive_df$find_val)>=1)
        sliderInput('pct', "Threshold for percent display", min = 0, max = 100, value = c(0,100))
        
      }))
      
      #___________________________sliderinput________________________
      
      
      
      #___________________________sliderinput________________
      
      
      tbl_final<- reactive({
        hamal$hml %>%mutate(match_percent= round(match_percent, digits = 2)) %>%  filter(name!="NULL", match_percent>=(input$pct)[1], match_percent<=(input$pct)[2]) %>% arrange(desc(match_percent)) %>% mutate(match_percent_character=as.character(match_percent)) 
      })
      #final_display_table:
      observeEvent(input$find, {
        req(length(reactive_df$find_val)>=1)
        output$f_t<- renderTable({
          
          tbl_final()%>%select(-match_percent_character) %>%  rename("File name"= "name", "Match percent"="match_percent", "Keywords matched"= "keywords")
          
          
          })
        
        output$plt<- renderPlot({
          
          ggplot(data = tbl_final())+
            geom_bar(mapping = aes(x=reorder(match_percent_character, match_percent), fill= match_percent_character))+
            labs(x= "Match percent", y= "Counts", title = "Percent match count for keywords")+
            theme(legend.position = "none")
        })
        
      })
      
      #for download
     observeEvent(input$find, output$save_final <- downloadHandler(
        filename = function() {
          paste(as.character(Sys.time()), "csv", sep = ".")
        },
        content = function(file) {
          # write.csv(save3(), file)
          readr::write_csv(tbl_final()%>%select(-match_percent_character) %>%  rename("File name"= "name", "Match percent"="match_percent", "Keywords matched"= "keywords")
                     ,file)
                 
        }
      ))
      
      
    }
    
    
    )
    
    
    
    
  }
  
)
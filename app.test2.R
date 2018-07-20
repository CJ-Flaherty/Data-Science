library(data.table)
library(shiny)
library(rvest)
library(DT)
library(tidyverse)
poke <- fread("poke.csv")
#
#Read ME!
# The commented code at the top was used to get description data and clean the pokemon data set,
# so I don't want it to run every time.
#
#Question 1 works well, aside from the issue of certain pokemon not having great descriptions (explained
#below), and some ugly characters I didn't have time to clean. Additionally, I added a link to the
#bulbapedia for each pokemon, so even the ones whose descriptions don't work there's a way to get 
#some information.

#Question 2 is fine

#question 3 doesn't tell you which stat your pokemon has the highest percentile in,
#but allows you to see where your pokemon compares for all stats. In a way this isn't what
#was asked for but in a way its more powerful. It also doesn't give percentile values, but
#represents  similar (couldn't get true percentiles to work. tried using quantile and some other
#stuff) information graphically

#Question 4's only problem is that the label can be hard to read. This is fixable but not for me
#under these time constraints









#poke <- poke %>%
 # filter(grepl("Male", Name, fixed = TRUE) == FALSE) ##removed some stuff, spaces, male, weird chars


#descriptions <- NULL

#names <- poke$Name


#for (i in 1:length(names)) {
  
 # url = paste("https://bulbapedia.bulbagarden.net/wiki/", names[i] , "_(Pokewithaccentmon)", sep = "")
  
  #lines <- readLines(url) #594,595,596
  
  #line1 <- lines[[594]]
  
  #line2 <- lines[[595]]
  
  #line3 <- lines[[596]]
  
  #desc1 = paste(line1, line2, line3, sep = " ")
  
  #desc1clean <- gsub("<.*?>", "", desc1)
  

  
  #descriptions[i] <- desc1clean
#}


 # poke$description = descriptions




#Above this point I did some cleaning of the pokemon data set, removing some entries
#that were strange (ie Male/Female/Mega versions)

#Then, I used the for loop to download the descriptions from bulbapedia. It works for most pokemon,
#but some pages seem to have a different format (different number of lines above their description),
#so unfortunately this method doesn't get their descriptions.


colnames(poke)[3] <- "Type" #grave thing wasn't working who knows why but this fixed it




ui <- fluidPage(selectizeInput(inputId = "selection",
                               label = "Select a Pokemon!",
                               choices = poke$Name),  #doing computations on server
                selectizeInput(inputId = "statchoice",
                               choices = c("HP","Attack", "Defense", "Special Attack",
                                           "Special Defense", "Speed"),
                               label = "Select a stat!"),
                textOutput(outputId = "text1"),
                textOutput(outputId = "text2"),
                dataTableOutput(outputId = "table1"),
                plotOutput(outputId = "plot1"),
                plotOutput(outputId = "plot2")
)
      
      
    


server <- function(input, output) {
 
 
  
  output$text1 <- renderText({
    

    
    paste(poke %>%
    data.frame() %>%
    filter(Name == input$selection) %>%
    .$description,
    paste("For more information, see", paste("https://bulbapedia.bulbagarden.net/wiki/", input$selection , "_(Pokemon)", sep = ""), sep = " "),
    sep = " ")
  })
  
    
   output$table1 <- renderDataTable({ 
       type <- poke %>%
       data.frame() %>%
       filter(Name == input$selection) %>%
       .$Type
       
       poke %>%
         data.frame() %>%
         filter(Type == type) %>%
         sample_n(5)
  })
   
   data <- reactive({
     if ( "HP" %in% input$statchoice) return(poke$HP)
     if ( "Attack" %in% input$statchoice) return(poke$Attack)
     if ( "Defense" %in% input$statchoice) return(poke$Defense)
     if ( "Special Attack" %in% input$statchoice) return(poke$`Sp. Atk`)
     if ( "Special Defense" %in% input$statchoice) return(poke$`Sp. Def`)
     if ( "Speed" %in% input$statchoice) return(poke$Speed)
   })
   
   index <- reactive({
     if ( "HP" %in% input$statchoice) return(6)
     if ( "Attack" %in% input$statchoice) return(7)
     if ( "Defense" %in% input$statchoice) return(8)
     if ( "Special Attack" %in% input$statchoice) return(9)
     if ( "Special Defense" %in% input$statchoice) return(10)
     if ( "Speed" %in% input$statchoice) return(11)
   })
   
   ourstat <- reactive({
     stat <- input$statchoice
     poke %>%
       filter(Name == input$selection) %>%
       .$stat
   })
      
  output$plot1 <- renderPlot({
    

    
    ourvalue <-  poke %>%
      filter(Name == input$selection) %>%
      .[,index()]
    
    
    
    
    
    poke %>%
      ggplot(aes(x=data(), fill= "red"))+
      geom_density() +
      geom_vline(aes(xintercept = ourvalue)) +
      labs(title = "Pokemon Statistics", x = "stat", y = "density", caption = "Black line denotes your pokemon's stat")
    
  
      
                     
  })
    
  output$plot2 <- renderPlot({
   m1 <- poke %>%
      lm(HP ~ Speed, data = .)
   
   ourpoke <- subset(poke, Name == input$selection)
   
   name <- poke %>%
     filter(Name == input$selection) %>%
     .$Name
    
   
   poke %>%
      ggplot(aes(x = Speed, y = HP)) +
      geom_point()+
      geom_abline(slope = m1$coefficients[2],
                 intercept = m1$coefficients[1],
                 col = "red") +
     geom_point(data = ourpoke, color = "blue", size = 5) +
     geom_text(data=ourpoke, label=name, vjust=1) +
     labs(title = "HP vs. Speed", x = "Speed", y = "HP", caption = "Your Pokemon is the labelled blue dot.")
   
    
  })
    
    
}
  
  
  
  
   



# Run the application 
shinyApp(ui = ui, server = server)


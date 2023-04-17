library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(DT)


ui <- dashboardPage(
  
  dashboardHeader(title = "Książki Amazon", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Statystyki", tabName = "statystyki", icon = icon("chart-line")),
      menuItem("Jakość ocen", tabName = "jakość",icon = icon("table")),
      menuItem("Ilość ocen", tabName = "sprzedane", icon = icon("dollar-sign"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "statystyki",
        fluidRow(
          box(
            title = "Boxploty porównujące statystyki między gatunkami książek.",
            status = "primary",
            solidHeader = TRUE,
            selectInput("os_y", label = h3("Wybierz zmienną:"), 
                        choices = list("Cena" = "Price", "Ilość recenzji" = "Reviews",
                                       "Jakość recenzji" = "User.Rating")),
            plotOutput("boxplot")
          ),
          box(
            title = "Barplot pokazujący, którego gatunku książek było więcej w danym roku.",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("barplot") 
          )
        ),
        fluidRow(
          column(
            selectInput("in_gestosc", label = h3("Wybierz zmienną:"), 
                        choices = list("Cena" = "Price", "Ilość recenzji" = "Reviews",
                                       "Jakość recenzji" = "User.Rating")),
            sliderInput("liczba_hist", 'Ilość "kubełków" w histogramie:' ,
                        min = 1, max = 100,
                        value = 30, step = 1),
            plotOutput("gestosc"), width = 12
          )
        )
      ),
      tabItem(
        radioButtons("rok", "Rok",inline = TRUE,
                     choices = list(
                       "2022" = 2022, "2021" = 2021, "2020" = 2020,
                       "2019" = 2019, "2018" = 2018, "2017" = 2017,
                       "2016" = 2016, "2015" = 2015, "2014" = 2014,
                       "2013" = 2013, "2012" = 2012, "2011" = 2011,
                       "2010" = 2010, "2009" = 2009),
                     selected = "2022"),
        tabName = "sprzedane",
        fluidRow(
          box(
            title = "30 najchętniej recenzowanych autorów w poszczególnych latach.",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("najlepiej_sprzed_autor")
            
          ),
          box(
            title = " 3 najchętniej oceniane książki w danym roku.",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("najlepsza_ksiazka")
          )
        )
      ),
      tabItem(
        tabName = "jakość",
        fluidRow(
          verbatimTextOutput("outputText1")
          
        ),
        sliderInput("min_oc", "Minimalna ocena, od której wyświetlane są książki.",
                    min = 3.6, max = 5,
                    value = 4.8, step = 0.1),
        fluidRow(
          column(
            DT::dataTableOutput("ksiazki_oceny"), width = 12
          )
        )
      )
    )
  )
  
  
  
)



server <- function(input, output){
  

  amazon <- read.csv("bestsellers_with_categories_2022_03_27.csv")
  wyfiltrowane <- amazon %>%
    arrange(Name, -Reviews) %>%
    filter(duplicated(Name) == FALSE)
  lata <- unique(amazon$Year)
  
  output$najlepiej_sprzed_autor <- renderPlot({
    sprzed_autor <- amazon %>% filter(Year == input$rok) %>% arrange(desc(Reviews)) %>% 
      distinct(Name, .keep_all = TRUE) %>% distinct(Author, .keep_all = TRUE) %>% head(30)
    
    ggplot(sprzed_autor, aes(x = reorder(Author, -Reviews), y = Reviews,
    fill = Author)) + geom_bar(stat = "identity") + guides(fill="none") + 
      ylab("Ilość recenzji") +
      theme_gdocs() +
      theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
            axis.title.x = element_blank())
  })
  
  output$najlepsza_ksiazka <- renderPrint({
    najl_ksiazka <- amazon %>% filter(Year == input$rok) %>% arrange(desc(Reviews)) %>% head(3)
    xd <- paste(najl_ksiazka$Author[1], najl_ksiazka$Name[1], najl_ksiazka$Reviews[1], sep = " - ")
    xd2 <- paste(najl_ksiazka$Author[2], najl_ksiazka$Name[2],najl_ksiazka$Reviews[2], sep = " - ")
    xd3 <- paste(najl_ksiazka$Author[3], najl_ksiazka$Name[3],najl_ksiazka$Reviews[3], sep = " - ")
    print(xd)
    print(xd2)
    print(xd3)
  })
  
  bestsellers_data_12_22_2 <- reactive({amazon %>% arrange(Name, -Reviews) %>% 
    filter(duplicated(Name) == FALSE & duplicated(Author) == FALSE)%>%
    filter(User.Rating >= input$min_oc) %>%
    arrange(desc(User.Rating)) %>%
    select(Name, Author, User.Rating)})
  
  output$ksiazki_oceny <- DT::renderDataTable(
    expr = DT::datatable(bestsellers_data_12_22_2(),
    caption = htmltools::tags$caption( style = 'caption-side: top;
                                      text-align: center; color:black;
                                      font-size:200% ;'
                                      ,'Najlepiej oceniane książki w latach 2009-2022.'))

  )
  
  output$outputText1 <- renderText({ 
    paste("Zbiór danych zawiera po 50 najlepiej sprzedających się książek na platformie Amazon
          w latach 2009-2022.")
  })
  
  output$boxplot <- renderPlot({
    
    ggplot(amazon, aes_string(x = "Genre",y = input$os_y), color_string = input$os_y)  + 
             geom_boxplot(color="#525120", fill="#61CC04") + theme_gdocs()
    
  })
  
  output$barplot <- renderPlot({
    ggplot(amazon,aes_string(x="Year", fill = "Genre"))+
      geom_bar(position = "dodge")+
      scale_fill_manual(values = c("#525120", "#61CC04"))+
      scale_x_continuous(breaks = seq(2009, 2022, by=1)) + 
      theme_gdocs() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

  })
  
  output$gestosc <- renderPlot({
      ggplot(amazon, aes_string(x=input$in_gestosc, y= "after_stat(density)"))+
      geom_histogram(fill="#00AE6E", color="#000000", bins = input$liczba_hist)+
      geom_density(alpha = 0.3, fill = "#61CC04") + theme_gdocs()
    
  })
}


shinyApp(ui, server)

library(shiny)
library(tidyverse)
library(DT)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(shinythemes)
library(moments)
library(readr)

# Wczytanie danych

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv"
meteorites <- read_csv(url, show_col_types = FALSE)

# Konwersja roku

meteorites$year <- suppressWarnings(as.numeric(substr(meteorites$year,1,4)))

# Nazwy kolumn

colnames(meteorites) <- c("Nazwa", "ID", "Typ", "Klasa", "Masa_g", "Upadek", "Rok", "Szer_geo", "Dl_geo", "Pozycja")

# Współrzędne

meteorites <- meteorites %>% mutate(
  Szer_geo = as.numeric(Szer_geo),
  Dl_geo   = as.numeric(Dl_geo)
)

# Opisy współrzędnych

meteorites <- meteorites %>% mutate(
  Szer_geo_opis = case_when(
    !is.na(Szer_geo) & Szer_geo > 0  ~ paste0(round(Szer_geo,4),"°N"),
    !is.na(Szer_geo) & Szer_geo < 0  ~ paste0(abs(round(Szer_geo,4)),"°S"),
    TRUE ~ NA_character_
  ),
  Dl_geo_opis = case_when(
    !is.na(Dl_geo) & Dl_geo > 0  ~ paste0(round(Dl_geo,4),"°E"),
    !is.na(Dl_geo) & Dl_geo < 0  ~ paste0(abs(round(Dl_geo,4)),"°W"),
    TRUE ~ NA_character_
  )
)

# Filtr lat

meteor_filtered <- meteorites %>% filter(!is.na(Rok) & Rok >= 1900 & Rok <= 2000)
meteor_map <- meteor_filtered %>% filter(!is.na(Szer_geo) & !is.na(Dl_geo) & !is.na(Masa_g))

# Grupy mas

meteor_map <- meteor_map %>% mutate(
  Masa_grupa = cut(
    Masa_g,
    breaks = quantile(Masa_g, probs = seq(0, 1, 0.2), na.rm = TRUE),
    labels = c("Bardzo małe","Małe","Średnie","Duże","Bardzo duże"),
    include.lowest = TRUE
  )
)
pal_group <- colorFactor(palette = brewer.pal(5,"YlOrRd"), domain = meteor_map$Masa_grupa)

# UI

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Analiza Meteorów NASA"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("rok", "Wybierz zakres lat:",
                  min = min(meteor_filtered$Rok, na.rm=TRUE),
                  max = max(meteor_filtered$Rok, na.rm=TRUE),
                  value = c(min(meteor_filtered$Rok, na.rm=TRUE), max(meteor_filtered$Rok, na.rm=TRUE)),
                  step = 1),
      selectInput("typ", "Typ meteoru:",
                  choices = c("Wszystkie", "Znalezione" = "Found", "Upadłe" = "Fell"),
                  selected = "Wszystkie"),
      selectInput("klasa", "Klasa meteoru:",
                  choices = c("Wszystkie", sort(unique(meteor_filtered$Klasa)) ),
                  selected = "Wszystkie")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Tabela",
                 DTOutput("tabela")
        ),
        tabPanel("Mapa",
                 leafletOutput("mapa", height = 600)
        ),
        tabPanel("Wykresy",
                 br(),
                 h4("Liczba meteorów w dekadach"),
                 p("Wykres pokazuje, ile meteorów odnotowano w każdej dekadzie."),
                 plotlyOutput("wykres_liczba", height = "300px"),
                 br(),
                 h4("Top 10 klas meteorów"),
                 p("10 najczęściej występujących klas meteorytów w danych."),
                 plotlyOutput("wykres_top10", height = "300px"),
                 br(),
                 h4("Średnia masa meteorytów w dekadach"),
                 p("Pokazuje trend zmian średniej masy meteorów na przestrzeni dekad."),
                 plotlyOutput("wykres_srednia", height = "300px"),
                 br(),
                 h4("Średnia masa meteorytów według klasy"),
                 p("Przedstawia średnią masę meteorytów w każdej klasie, co daje porównanie między klasami."),
                 plotlyOutput("wykres_masy", height = "300px")
        ),
        tabPanel("Statystyki",
                 br(),
                 h3("Statystyki opisowe", style="color:#2C3E50;"),
                 tableOutput("statystyki")
        ),
        tabPanel("Informacje",
                 br(),
                 h3("Wprowadzenie", style="color:#2C3E50;"),
                 p("W raporcie przedstawiono analizę meteorów NASA z lat 1900–2000."),
                 
                 h3("Autorzy projektu", style="color:#2C3E50;"),
                 p("Paulina Papież i Julia Bugielska"),
                 
                 h3("Ciekawostki", style="color:#2C3E50;"),
                 tags$ul(
                   tags$li("Najcięższy meteoryt w danych ważył ponad 100 000 g."),
                   tags$li("Najwięcej meteorów pochodzi z klasy L6."),
                   tags$li("Najwięcej przypadków odnotowano w latach 60."),
                   tags$li("Większość meteorów to obiekty typu 'Found'.")
                 ),
                 
                 h3("Wnioski", style="color:#2C3E50;"),
                 tags$ul(
                   tags$li("Liczba obserwacji meteorów różni się między dekadami."),
                   tags$li("Średnia masa meteorów jest zmienna i zależy od klasy."),
                   tags$li("Rozmieszczenie geograficzne jest nierównomierne."),
                   tags$li("Dane NASA pozwalają na pogłębioną analizę typów i klas meteorytów.")
                 )
                 
        )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  filtrowane <- reactive({
    df <- meteor_filtered
    df <- df %>% filter(Rok >= input$rok[1] & Rok <= input$rok[2])
    if(input$typ != "Wszystkie") df <- df %>% filter(Upadek == input$typ)
    if(input$klasa != "Wszystkie") df <- df %>% filter(Klasa == input$klasa)
    df
  })
  
  output$tabela <- renderDT({
    datatable(
      filtrowane() %>% select(Nazwa, Klasa, Masa_g, Rok, Szer_geo_opis, Dl_geo_opis, Pozycja),
      colnames = c("Nazwa","Klasa","Masa (g)","Rok","Szerokość","Długość","Lokalizacja"),
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip',
                     columnDefs = list(list(className = 'dt-left', targets = "_all"))),
      style = "bootstrap"
    ) %>%
      formatStyle(columns = c("Nazwa","Klasa","Masa_g","Rok","Szer_geo_opis","Dl_geo_opis","Pozycja"),
                  `text-align` = "left")
  })
  
  output$mapa <- renderLeaflet({
    leaflet(meteor_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~Dl_geo, lat = ~Szer_geo,
        color = ~pal_group(Masa_grupa),
        radius = ~pmin(sqrt(Masa_g)/300, 12),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        popup = ~paste0("<b>Nazwa:</b> ", Nazwa,
                        "<br><b>Klasa:</b> ", Klasa,
                        "<br><b>Masa:</b> ", formatC(Masa_g, format="d", big.mark=" "), " g",
                        "<br><b>Rok:</b> ", Rok,
                        "<br><b>Szerokość:</b> ", Szer_geo_opis,
                        "<br><b>Długość:</b> ", Dl_geo_opis)
      ) %>%
      addLegend("bottomright", pal = pal_group, values = ~Masa_grupa, title="Grupy mas")
  })
  
  output$wykres_liczba <- renderPlotly({
    decade_counts <- filtrowane() %>%
      mutate(Rok_grupa = Rok - Rok %% 10) %>%
      group_by(Rok_grupa) %>% summarise(Liczba=n())
    plot_ly(decade_counts, x=~Rok_grupa, y=~Liczba, type='bar',
            text=~Liczba, textposition='auto',
            marker=list(color=~Liczba, colorscale='Oranges'))
  })
  
  output$wykres_top10 <- renderPlotly({
    top10 <- filtrowane() %>%
      group_by(Klasa) %>% summarise(Liczba=n()) %>%
      arrange(desc(Liczba)) %>% slice_head(n=10)
    plot_ly(top10, x=~Liczba, y=~reorder(Klasa, Liczba), type='bar',
            orientation='h', text=~Liczba, textposition='auto',
            marker=list(color=~Liczba, colorscale='Purples'))
  })
  
  output$wykres_srednia <- renderPlotly({
    mass_trend <- filtrowane() %>%
      mutate(Rok_grupa = Rok - Rok %% 10) %>%
      group_by(Rok_grupa) %>% summarise(Srednia = mean(Masa_g, na.rm=TRUE))
    plot_ly(mass_trend, x=~Rok_grupa, y=~Srednia, type='scatter', mode='lines+markers')
  })
  
  output$wykres_masy <- renderPlotly({
    class_mean <- filtrowane() %>% group_by(Klasa) %>% summarise(Srednia = mean(Masa_g, na.rm=TRUE))
    plot_ly(class_mean, x=~Klasa, y=~Srednia, type='bar', text=~round(Srednia,1),
            textposition='auto', marker=list(color=~Srednia, colorscale='Viridis')) %>%
      layout(xaxis=list(title="Klasa meteorytu"), yaxis=list(title="Średnia masa (g)"))
  })
  
  output$statystyki <- renderTable({
    df <- filtrowane()
    tibble(
      `Min` = min(df$Masa_g, na.rm=TRUE),
      `Max` = max(df$Masa_g, na.rm=TRUE),
      `Mediana` = median(df$Masa_g, na.rm=TRUE),
      `Średnia` = mean(df$Masa_g, na.rm=TRUE),
      `Odchylenie std` = sd(df$Masa_g, na.rm=TRUE),
      `Skośność` = moments::skewness(df$Masa_g, na.rm=TRUE),
      `Kurtoza` = moments::kurtosis(df$Masa_g, na.rm=TRUE)
    )
  })
}

shinyApp(ui, server)


##Gjenståande:
## Kartet bør vere større
## Svalbard manglar
## Tildeltkolonna vert utan formatering mellomrom og kr) når når biletet vert smalt grunna responsivitet
## Informasjon om lasting av app (Shiny) eller kart (Highcharts)

library(shiny)
library(highcharter)
library(jsonlite)
library(readxl)
library(dplyr)
library(DT)

kart<-"https://raw.githubusercontent.com/Tilskot/Kart/master/Kommunar%202019.geojson"

kart2<-fromJSON(kart, simplifyVector = FALSE)

data<-read_excel("Kommuneliste 2019.xlsx") %>%
  mutate(kommunenummer=as.numeric(kommunenr),
         Mottakar=namn_ledd,
         Tildelt=tildelt,
         Orgnr=orgnr_ledd) %>%
  select(kommunenummer,Mottakar,Tildelt,Orgnr,kommune,fylkenr) %>%
  arrange(desc(Tildelt)) %>%
  mutate(Tildelt=round(Tildelt)) # Rundar her sidan responsive i DT ikkje formaterer når biletet er smalt

største<-data %>%
  group_by(kommunenummer) %>%
  top_n(n = 1, wt = Tildelt) %>%
  mutate(størst=Mottakar) %>%
  select(kommunenummer,størst)

aggregert<-data %>%
  group_by(kommunenummer,kommune, fylkenr) %>%
  summarise(Log=log(sum(Tildelt)),
            Sum=round(sum(Tildelt))) %>%
  ungroup() %>%
  left_join(største,by='kommunenummer') %>%
  mutate(kommunenummer=as.character(kommunenummer))

series <- aggregert %>%
  group_by(name = fylkenr) %>%
  do(data = list_parse(.)) %>%
  ungroup()

shinyServer(function(input, output) {
  
  output$kart<-renderHighchart(
    
    ## Plottar kartet
    highchart(type = "map") %>% 
      hc_plotOptions(map = list(
        allAreas = FALSE,
        joinBy = c("kommunenummer"),
        mapData = kart2),
        series=list(
          point=list(events=list(click=JS("function(event) {Shiny.onInputChange('hcClicked', this.kommunenummer);Shiny.onInputChange('hcKlikk', this.kommunenummer);}"))))) %>% 
      hc_add_series_list(series) %>%
      hc_legend(enabled=FALSE) %>%
      hc_mapNavigation(enabled=TRUE) %>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.point.kommune + '</b><br/>' + 'Totalt tildelt: ' + Highcharts.numberFormat(this.point.Sum,0) + ' kroner'+ '<br/>' + 'Største mottakar: ' +this.point.størst;}")) %>%
      hc_chart(showLoading=TRUE)
  )
  
  output$tabell<-renderDataTable({
    
    tabelldata<-filter(data,kommunenummer==input$hcKlikk)
    
    datatable(tabelldata[,c("Orgnr","Mottakar","Tildelt","kommune")],
              extensions = 'Responsive',
              rownames = FALSE,
              options = list(
                dom = 'ftp',
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Norwegian-Nynorsk.json'))) %>%
      formatCurrency('Tildelt', mark = "\U00A0",digits = 0, currency=' kr', before=FALSE)
  })
  
  ## Størrelsen på boksen kan også settast i ui.R
  dataModal <- function(failed = FALSE) {
    modalDialog(
      DT::dataTableOutput('tabell'),
      easyClose = TRUE,
      size="l",
      footer = actionButton("hcClicked","Tilbake til kart")
    )
  }
  
  ## Dette er ikkje ei god løysing, men den verkar å fungere her. actionbutton i dataModal genererer ein ny input$hcClicked ved kvart trykk. Denne får verdien NULL slik at observeEvent vert endra.
  observeEvent(input$hcClicked,{
    if (input$hcClicked %in% data$kommunenummer){
      showModal(dataModal())
    } else {
      removeModal()
    }
    
  })
  
})
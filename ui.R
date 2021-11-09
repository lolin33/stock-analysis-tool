library(shiny)
library(shinycssloaders)
library(shinythemes)
library(plotly)

#Definition der UI mit fluid-Layout für shiny-Anwendung speichern
ui <- fluidPage(
  #Festlegen des Fenster-Titels mittels titlePanel-Funktion
  titlePanel("Aktien-Tool v1.0"),
  
  #Zuweisen eines shinythemes zum theme-Parameter der fluidPage-Funktion
  theme = shinytheme("united"),
  
  #Mittels sidebarLayout-Funktion das Layout der Fluid-Page auf Sidebar-Layout festlegen
  sidebarLayout(
    #Definition der Sidebar mittels sidebarPanel-Funktion
    sidebarPanel(
      #Festlegen des obersten Elements der Sidebar auf ein Texteingabefeld mittels textInput-Funktion
      textInput(
        #Zuweisen eines String zu Parameter inputId
        inputId = "stockId",
        
        #Zuweisen eines String zu Paramter label für Beschriftung des Eingabefelds
        label = "Tickersymbol der Aktie auf yahoo! Finance",
        
        #Zuweisen eines Strings zu Parameter value als initialer Wert bei Anwendungsstart
        value = "ADN1.DE"
      ),
      
      #Festlegen des zweiten Sidebar-Elements per conditionPanel-Funktion
      conditionalPanel(#Zuweisen einer input-Id als String zu condition-Paramter; dieses Panel wird angezeigt wenn die Bedingung wahr ist
        condition = "input.tab==1",
        
        #Festlegen des Inhalts des Conditional-Panels auf ein Auswahl-Menü per selectInput-Funktion
        #Parameter 1: Festlegen der Id des Select-Inputs auf selected
        #Parameter 2: Festlegen der Beschriftung des Auswahlmenüs auf Datenauswahl:
        #Parameter 3: Festlegen der Auswahloptionen per Vektor mit Strings
        selectInput(
          "selected",
          "Datenauswahl:",
          c(
            "GuV",
            "Bilanz",
            "Cashflow",
            "Kralicek-Quicktest",
            "Weitere Kennzahlen"
          )
        )),
      
      #Definition eines zweiten, alternativen Conditional-Panel als zweites Element in der Anzeige der Sidebar
      conditionalPanel(
        #Zuweisen einer input-Id als String zu condition-Paramter; dieses Panel wird angezeigt wenn die Bedingung wahr ist
        condition = "input.tab==2",
        
        #Erstes Element dieses Conditional Panels wird im Server gerendert. Die Funktion uiOutput hat als Parameter die Id des anzugeigenden Elements
        #Hier wird eine Datumsauswahl über die uiOutput-Funktion aus dem output-Element des Servers abgerufen.
        uiOutput("dateRange"),
        
        #Erzeugen einer Checkbox für Anzeige der 50-Tage-Linie als zweites Element des conditionalPanels
        checkboxInput(
          #Festlegen der input-Id auf rollMean50Show
          inputId = "rollMean50Show",
          #Festlegen der Beschriftung der Checkbox; strong-Funktion erzeugt fette Schrift
          label = strong("50-Tage-Linie"),
          #Festlegen des initialen Zustands bei Anwendungsstart; FALSE bedeutet nicht angekreutzt
          value = FALSE
        ),
        
        #Analog zu erster Checkbox für 200-Tage-Linie
        checkboxInput(
          inputId = "rollMean200Show",
          label = strong("200-Tage-Linie"),
          value = FALSE
        ),
        
        #Analog zu erster Checkbox für angepassten Verlauf des DAX
        checkboxInput(
          inputId = "daxShow",
          label = strong("Angep. Verlauf des DAX"),
          value = FALSE
        ),
        
        #Analog zu erster Checkbo für angepassten Verlauf des S&P 500
        checkboxInput(
          inputId = "snpShow",
          label = strong("Angep. Verlauf des S&P 500"),
          value = FALSE
        )
      )
    ),
    
    #Festlegen des Main-Panels des sidebarLayouts für die Hauptanzeige der Anwendung
    mainPanel(
      #Festlegen des Inhalts des Main-Panels auf ein Tabset-Panel
      tabsetPanel(
        #Festlegen der Id des Tabset-Panels
        id = "tab",
        
        #Erzeugen eines Tab-Panels für Tabset-Panel
        tabPanel(#Beschriftung des Tab-Panels
          "Fundamentalanalyse",
          
          #Festlegen der Position des Tab-Panels an erste Stelle
          value = 1,
          
          #try-Catch-Block für Fehlerbehandlung bei Problemen
          tryCatch(
            #Festlegen des auszuführenden Ausdrucks-Blocks
            expr = {
              #tableOutput-Funktion stellt Tabelle aus output-Element des Server dar
              #Parameter outputId: festlegen auf fundamentals; anhand der Id werden Daten aus output-Element abgerufen
              #Weitergeben des tableOutputs an Funktion withSpinner die während der Berechnungszeit des Servers
              #eine farbige Ladeanimation anzeigt; Farbe der Ladeanimation wird festgelegt mit Parameter color
              tableOutput(outputId = "fundamentals") %>% withSpinner(color = "#0dc5c1")
            },
            #Behandlung von auftretenden errors
            error = function(error) {
              #Anzeigen des Texts aus dem output-Element des Servers anhand der Id fundamentals
              #Text wird bei errors schon im Server erzeugt
              textOutput(outputId = "fundamentals")
              
            },
            #Behandlung von auftretenden warnings
            warning = function(warning) {
              #Anzeigen des Texts aus dem output-Element des Servers anhand der Id fundamentals
              #Text wird bei warnings schon im Server erzeugt
              textOutput(outputId = "fundamentals")
            }
          )),
        
        #Erzeugen eines weiteren Tab-Panels für Tabset-Panel
        tabPanel(#Beschriftung des Tab-Panels
          "Chartanalyse",
          
          #Festlegen der Position des Tab-Panels an erste Stelle
          value = 2,
          
          #try-Catch-Block für Fehlerbehandlung bei Problemen
          tryCatch(
            #Festlegen des auszuführenden Ausdrucks-Blocks
            expr = {
              #plotlyOutput-Funktion stellt Plotly-Diagramm aus output-Element des Server dar
              #Parameter outputId: festlegen auf lineplot; anhand der Id werden Daten aus output-Element abgerufen
              #Weitergeben des plotlyOutputs an Funktion withSpinner die während der Berechnungszeit des Servers
              #eine farbige Ladeanimation anzeigt; Farbe der Ladeanimation wird festgelegt mit Parameter color
              plotlyOutput(outputId = "lineplot") %>% withSpinner(color = "#0dc5c1")
            },
            #Behandlung von auftretenden errors
            error = function(error) {
              #Anzeigen des Texts aus dem output-Element des Servers anhand der Id lineplot
              #Text wird bei errors schon im Server erzeugt
              textOutput(outputId = "lineplot")
              
            },
            warning = function(warning) {
              #Anzeigen des Texts aus dem output-Element des Servers anhand der Id lineplot
              #Text wird bei warnings schon im Server erzeugt
              textOutput(outputId = "lineplot")
            }
          ))
      ),
      #Erzeugen eine HTML dividers für Anzeige von Informationen am unteren Rand des main-Panels
      tags$div(
        #Erzeugen einer Leerzeile für mehr Abstand zu Darstellung der Daten
        tags$br(),
        #Funktion htmlOutput erzeugt Ausgabe anhand der Daten des output-Element des Servers mit Id desc
        htmlOutput(outputId = "desc"),
        #Erzeugen eines HTML-Anker-Elements für Anzeige eines Hyperlink-Elements
        tags$a(
          #Festlegen des Links für HTML-Anker-Element
          href = "https://finance.yahoo.com/lookup/",
          #Festlegen des Textes der den Hyperlink enthält
          "Quelle der Daten: yahoo! Finance",
          #Festlegen der Öffnung des Hyperlinks in neuem Tab/Fenster
          target = "_blank"
        )
      )
    )
  )
)

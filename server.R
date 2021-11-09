library(dplyr)
library(ggplot2)
library(plotly)
library(rvest)
library(stringr)
library(zoo)

#Definition des Servers für die Datenverarbeitung und Verarbeitung des User-Inputs der shiny-Anwendung
#Server-Funktion hat die Parameter input für Eingaben des Benutzers, output für Datenausgabe des Servers und session wird benötigt um die Datumsauswahl zu aktualisieren
server <- function(input, output, session) {
  #Speichern der aktuellen Zeit als UNIX-Timestamp im Objekt currentUNIXTime
  currentUNIXTime <- as.numeric(as.POSIXct(Sys.Date()))
  
  #########################################
  #### URLs für den Download der Daten ####
  #########################################
  
  #Speichern der Funktion für Rückgabe der Query für Kurs-Daten des DAX im Objekt daxUrl
  daxUrl <- reactive({
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des aktuellen UNIX-Timestamps in DAX-URL mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://query1.finance.yahoo.com/v7/finance/download/%%5EGDAXI?period1=0&period2=%s&interval=1d&events=history&includeAdjustedClose=true",
        currentUNIXTime
      )
    )
  })
  
  #Speichern der Funktion für Rückgabe der Query für Kurs-Daten des DAX im Objekt daxUrl
  snpUrl <- reactive({
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des aktuellen UNIX-Timestamps in S&P 500-URL mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://query1.finance.yahoo.com/v7/finance/download/%%5EGSPC?period1=0&period2=%s&interval=1d&events=history&includeAdjustedClose=true",
        currentUNIXTime
      )
    )
  })
  
  #Speichern der Funktion für Rückgabe der Query für Kurs-Daten der aktuellen Aktie per stockId im Objekt dynamicUrl
  dynamicUrl <- reactive({
    #Mit der Funktion req wird sichergestellt, dass das Element stockId im input-Objekt existiert
    req(input$stockId)
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des Aktien-Symbols (stockId) und des aktuellen UNIX-Timestamps in Query mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://query1.finance.yahoo.com/v7/finance/download/%s?period1=0&period2=%s&interval=1d&events=history&includeAdjustedClose=true",
        input$stockId,
        currentUNIXTime
      )
    )
  })
  
  #Speichern der Funktion für Rückgabe der URL zur GuV der aktuellen Aktie per stockId im Objekt dynamicUrl_financials
  dynamicUrl_financials <- reactive({
    #Mit der Funktion req wird sichergestellt, dass das Element stockId im input-Objekt existiert
    req(input$stockId)
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des Aktien-Symbols (stockId) in URL mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://de.finance.yahoo.com/quote/%s/financials?p=%s",
        input$stockId,
        input$stockId
      )
    )
  })
  
  #Speichern der Funktion für Rückgabe der URL zur Bilanz der aktuellen Aktie per stockId im Objekt dynamicUrl_balanceSheet
  dynamicUrl_balanceSheet <- reactive({
    #Mit der Funktion req wird sichergestellt, dass das Element stockId im input-Objekt existiert
    req(input$stockId)
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des Aktien-Symbols (stockId) in URL mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://de.finance.yahoo.com/quote/%s/balance-sheet?p=%s",
        input$stockId,
        input$stockId
      )
    )
  })
  
  #Speichern der Funktion für Rückgabe der URL zur cashflow-Rechnung der aktuellen Aktie per stockId im Objekt dynamicUrl_cashflow
  dynamicUrl_cashflow <- reactive({
    #Mit der Funktion req wird sichergestellt, dass das Element stockId im input-Objekt existiert
    req(input$stockId)
    #Rückgabe des Objekts durch return
    return(
      #Einfügen des Aktien-Symbols (stockId) in URL mit der sprintf-Funktion und dem Platzhalter %s
      sprintf(
        "https://de.finance.yahoo.com/quote/%s/cash-flow?p=%s",
        input$stockId,
        input$stockId
      )
    )
  })
  
  #Objekt cache speichert zur Laufzeit relevante Daten zur aktuellen Aktie, damit diese je Aktie nur einmal heruntergeladen werden müssen
  cache <-
    reactiveValues(
      dax = NULL,
      snp = NULL,
      financials = NULL,
      balanceSheet = NULL,
      cashflow = NULL
    )
  
  #Objekt zum Speichern des Start- und Enddatums für die Chartanalyse
  minMaxDates <- reactiveValues()
  #Objekt zum Speichern der Checkbox-Einstellungen der Chartanalyse
  config <- reactiveValues()
  
  #Speichern der gerenderten Datumsauswahl im output-Objekt
  output$dateRange <- renderUI({
    #Erzeugen einer Datumsauswahl mit Start- und Enddatum
    dateRangeInput(
      #Id des Input-Objekts
      "Date",
      #Beschriftung der Datumsauswahl
      "Wählen Sie einen Zeitraum:",
      #Initiales Festlegen des Startdatums auf Datum des ersten Werts des Kursverlaufs der aktuellen Aktie
      start =
        minMaxDates$min,
      #Initiales Festlegen des Startdatums auf Datum des letzten Werts des Kursverlaufs der aktuellen Aktie
      end =
        minMaxDates$max,
      #Festlegen des minimalen Wertes der Datumsauswahl auf Datum des ersten Werts des Kursverlaufs der aktuellen Aktie
      min =
        minMaxDates$min,
      #Festlegen des maximalen Wertes der Datumsauswahl auf Datum des letzten Werts des Kursverlaufs der aktuellen Aktie
      max =
        minMaxDates$max,
      #Festlegen des Datumsformats
      format = "yyyy-mm-dd"
    )
    
  })
  
  
  ###########################################################################
  #### Observer für bestimmte Events oder Änderungen von reactive-Values ####
  ###########################################################################
  
  #Aufruf der Funktionn für Download der Kursdaten für S&P 500 & DAX, wenn diese noch nicht geladen wurden und das Tab gewechselt wird
  #Erzeugen eines Observers auf das input-Event des Tabwechsels mit auszuführendem Code-Block
  observeEvent(input$tab, {
    #Die Bedingungen von req setzen voraus, dass die Daten von DAX und S&P 500 noch nicht im cache-Objekt gespeichert wurden
    #Wenn eine der beiden Bedingungen falsch ist, dann wird der Codeblock verlassen und die untenstehenden Funktionen nicht aufgerufen
    req(is.null(cache$dax))
    req(is.null(cache$snp))
    
    #Aufrufen der Funktion welche die Daten des DAX herunterlädt und im cache-Objekt speichert
    historicalDataDax()
    #Aufrufen der Funktion welche die Daten des S&P 500 herunterlädt und im cache-Objekt speichert
    historicalDataSnp()
  })
  
  #Bei Eingabe eines neuen Datums wird der DateRangeInput der Chartanalyse geupdated
  #Erzeugen eines Observer auf das Event der Änderung der Datumsauswahl mit auszuführendem Codeblock
  observeEvent(input$Date, {
    #Aufrufen der Funktion für das aktualisieren der Datumsauswahl
    #Parameter 1 ist die aktuelle Session
    #Paramter 2 ist die Id des Date-Range-Inputs, welcher aktualisiert werden soll
    #Änderungen betreffen an dieser Stelle die korrekte Anzeige in der UI
    updateDateRangeInput(session, "Date")
  })
  
  #Bei Eingabe eines neuen Datums wird validiert, dass Start- und Enddatum vorhanden sind und dass das Startdatum nicht nach dem Enddatum liegt
  #Bei Änderungen der Checkboxen in der Chartanalyse wird die Änderung auch in die Config-Variable übernommen
  #Erzeugen eines Observer der auf Änderungen der in ihm verwendeten reactiveValues wartet
  observe({
    #Bedingung von req überprüft, ob Objekt der Datumsauswahl vorhanden ist
    req(input$Date)
    
    #Prüfen ob sowohl Start- als auch Enddatum angegebn wurden
    validate(need(
      #Prüfbedingung
      !is.na(input$Date[1]) &!is.na(input$Date[2]),
      #Fehlermeldung, welche angezeigt wird, wenn Prüfbedingung False ist
      "Error: Bitte Start- und Enddatum angeben."
    ))
    
    #Prüfen ob Zeitraum valide ist, also ob das Enddatum auch wirklich nach dem Startdatum liegt
    validate(
      need(
        #Prüfbedingung
        input$Date[1] < input$Date[2],
        #Fehlermeldung, welche angezeigt wird, wenn Prüfbedingung False ist
        "Error: Das Startdatum muss vor dem Enddatum liegen."
      )
    )
    
    #Speichern des Zustands der 50-Tage-Checkbox im entsprechenden Element des cache-Objekts, um darauf zugreifen zu können
    config$rollMean50Show <- input$rollMean50Show
    
    #Speichern des Zustands der 200-Tage-Checkbox im entsprechenden Element des cache-Objekts, um darauf zugreifen zu können
    config$rollMean200Show <- input$rollMean200Show
    
    #Speichern des Zustands der DAX-Checkbox im entsprechenden Element des cache-Objekts, um darauf zugreifen zu können
    config$daxShow <- input$daxShow
    
    #Speichern des Zustands der S&P 500-Checkbox im entsprechenden Element des cache-Objekts, um darauf zugreifen zu können
    config$snpShow <- input$snpShow
  })
  
  #Bei Auswahl des Quicktest werden benötigte Daten vorgeladen und in der Cache-Variablen gespeichert
  #Erzeugen eines Observers auf das Event der Auswahl eines Elements des Auswahlmenüs der Fundamentalanalyse
  observeEvent(input$selected, {
    #Bedingung prüft, ob selected-Element des input-Objekts existiert und dem String Kralicek-Quicktest entspricht
    req(input$selected,
        input$selected == "Kralicek-Quicktest")
    
    #Ausführen der Funktionn für das Herunterladen und Speichern im entprechenden Element der Cache-Variable von GuV, Bilanz und cashflow-Rechnung der aktuellen Aktie
    #Dieser Schritt erfolgt, da die Daten im Kralicek-Quicktest benötigt werden und durch das Vorabladen die Verfügbarkeit der Daten sichergestellt wird
    financials()
    balanceSheet()
    cashflow()
  })
  
  #Wenn die Aktie geändert wird und dabei der Quicktest bereits ausgewählt ist werden die benötigten Daten geladen und in die cache-Variable gespeichert
  #Erzeugen eines Observers auf das Event der Eingabe in das Input-Feld mit der Id stockId
  observeEvent(input$stockId, {
    #Bedingung prüft, ob selected-Element des input-Objekts existiert und dem String Kralicek-Quicktest entspricht
    req(input$selected,
        input$selected == "Kralicek-Quicktest")
    #Ausführen der Funktionn für das Herunterladen und Speichern im entprechenden Element der Cache-Variable von GuV, Bilanz und cashflow-Rechnung der aktuellen Aktie
    #Dieser Schritt erfolgt, da die Daten im Kralicek-Quicktest benötigt werden und durch das Vorabladen die Verfügbarkeit der Daten sichergestellt wird
    financials()
    balanceSheet()
    cashflow()
    
    #Aufrufen der Funktion für Berechnung und Speicherung der Werte des Kralicek-Quicktest im output-Objekt
    quickTest()
  })
  
  #Bei Auswahl des Punkts weitere Kennzahlen werden die benötigten Daten geladen und in die cache-Variable gespeichert
  #Erzeugen eines Observers auf das Event der Auswahl eines Elements des Auswahlmenüs der Fundamentalanalyse
  observeEvent(input$selected, {
    #Bedingung prüft, ob selected-Element des input-Objekts existiert und dem String Weitere Kennzahlen entspricht
    req(input$selected, input$selected == "Weitere Kennzahlen")
    
    #Ausführen der Funktionn für das Herunterladen und Speichern im entprechenden Element der Cache-Variable von GuV und Bilanz der aktuellen Aktie
    #Dieser Schritt erfolgt, da die Daten für die Berechnung der weiteren Kennzahlen benötigt werden und durch das Vorabladen die Verfügbarkeit der Daten sichergestellt wird
    financials()
    balanceSheet()
  })
  
  #Wird die Aktie geändert und ist dabei der Punkt weitere Kennzahlen ausgewählt werden die benötigten Daten geladen und in der cache-Variablen gespeichert
  #Erzeugen eines Observers auf das Event der Eingabe in das Input-Feld mit der Id stockId
  observeEvent(input$stockId, {
    #Bedingung prüft, ob selected-Element des input-Objekts existiert und dem String Weitere Kennzahlen entspricht
    req(input$selected, input$selected == "Weitere Kennzahlen")
    
    #Ausführen der Funktionn für das Herunterladen und Speichern im entprechenden Element der Cache-Variable von GuV und Bilanz der aktuellen Aktie
    #Dieser Schritt erfolgt, da die Daten für die Berechnung der weiteren Kennzahlen benötigt werden und durch das Vorabladen die Verügbarkeit der Daten sichergestellt wird
    financials()
    balanceSheet()
    
    #Aufrufen der Funktion für Berechnung und Speicherung der Werte für den Menü-Punkt weitere Kennzahlen im output-Objekt
    benchmarks()
  })
  
  ########################################################################################
  #### Funktionen für das Herunterladen und Speichern der Daten im cache-Objekt ##########
  ########################################################################################
  
  #Durch Aufruf dieser Funktion werden die Kursdaten des DAX geladen und im DAX-Element des cache-Objekts gespeichert
  historicalDataDax <- reactive({
    #Herunterladen der DAX-Daten über Link aus daxUrl-Funktion
    #try-Catch-Block für Fehlerbehandlung bei Problemen
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Auslesen der DAX-Daten im csv-Format über die URL der daxUrl-Funktion
        read.csv(daxUrl(), sep = ",")
      },
      
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      #Rückgabe von null bei Warnungen
      warning = function(w) {
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bei dem Herunterladen der DAX-Daten ist ein Fehler aufgetreten. Bitte versuchen Sie es später erneut."
      )
    )
    
    #Umwandlung der Spaltenwerte auf numerische Werte
    download[["Open"]] <- as.numeric(download[["Open"]])
    download[["High"]] <- as.numeric(download[["High"]])
    download[["Low"]] <- as.numeric(download[["Low"]])
    download[["Close"]] <- as.numeric(download[["Close"]])
    download[["Adj.Close"]] <- as.numeric(download[["Adj.Close"]])
    download[["Volume"]] <- as.numeric(download[["Volume"]])
    
    #Umwandlung der Datumsspalte als Date
    download$Date <- as.Date(download$Date)
    
    #In den Daten sind NA an Tagen bei denen die Aktien Werte haben; damit die dataframes dennoch die gleiche Länge haben werden hier NAs mit dem vorangehenden Wert ersetzt
    #download-Objekt wird an na.locf-Funktion übergeben, welche NA-Werte durch die vorgehenden Werte ersetzt
    download %>% na.locf()
    
    #download-Objekt wird im dax-Element des cache-Objekts gespeichert
    cache$dax <- download
  })
  
  #Durch Aufruf dieser Funktion werden die Kursdaten des S&P 500 geladen und im S&P 500-Element des cache-Objekts gespeichert
  historicalDataSnp <- reactive({
    #Herunterladen der S&P 500-Daten über Link aus snpUrl-Funktion
    #try-Catch-Block für Fehlerbehandlung bei Problemen
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Auslesen der S&P 500-Daten im csv-Format über die URL der snpUrl-Funktion
        read.csv(snpUrl(), sep = ",")
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bei dem Herunterladen der S&P 500-Daten ist ein Fehler aufgetreten. Bitte versuchen Sie es später erneut."
      )
    )
    
    #Umwandlung der Spaltenwerte auf numerische Werte
    download[["Open"]] <- as.numeric(download[["Open"]])
    download[["High"]] <- as.numeric(download[["High"]])
    download[["Low"]] <- as.numeric(download[["Low"]])
    download[["Close"]] <- as.numeric(download[["Close"]])
    download[["Adj.Close"]] <- as.numeric(download[["Adj.Close"]])
    download[["Volume"]] <- as.numeric(download[["Volume"]])
    
    #Umwandlung der Datumsspalte als Date
    download$Date <- as.Date(download$Date)
    
    #In den Daten sind NA an Tagen bei denen die Aktien Werte haben; damit die dataframes dennoch die gleiche Länge haben werden hier NAs mit dem vorangehenden Wert ersetzt
    #download-Objekt wird an na.locf-Funktion übergeben, welche NA-Werte durch die vorgehenden Werte ersetzt
    download %>% na.locf()
    
    #download-Objekt wird im snp-Element des cache-Objekts gespeichert
    cache$snp <- download
  })
  
  ##################################################################################################
  #### Bei den folgenden Funktionen werden die Daten auch schon direkt als Tabelle gerendert, ######
  #### damit die Funktionen direkt für den Output in der UI genutzt werden können ##################
  ##################################################################################################
  
  #Herunterladen der GuV der Aktie und Rendern der Tabelle als financials-Funktion
  #vgl. stackoverflow: https://stackoverflow.com/questions/58315274/r-web-scraping-yahoo-finance-after-2019-change
  financials <- renderTable({
    #try-Catch-Block für Fehlerbehandlung bei Problemen
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Herunterladen der GuV über Link aus dynamicUrl_financials-Funktion
        html <- read_html(dynamicUrl_financials())
        
        #Suchen der entsprechenden HTML-Knoten der GuV-Tabelle
        nodes <- html %>% html_nodes(".fi-row")
        
        #Erzeugen eines NULL-Objekts für Speichern des Dataframes der GuV
        df = NULL
        
        #Iteration über gefundenen Knoten
        for (i in nodes) {
          #Speichern der Spaltennamen und den Werten der Spalten
          r <-
            list(i %>% html_nodes("[title],[data-test='fin-col']") %>% html_text())
          
          #Zusammenfügen der Spalten zu einem Data Frame
          df <- rbind(df,
                      as.data.frame(
                        matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE),
                        stringsAsFactors = FALSE
                      ))
        }
        
        #Titel der Spalten nach Daten mit Format dd-mm-yyyy durchsuchen und passende Werte speichern
        matches <-
          str_match_all(
            html %>% html_node('#Col1-1-Financials-Proxy') %>% html_text(),
            '\\d{1,2}\\.\\d{1,2}\\.\\d{4}'
          )
        
        #Header des Data Frame analog zu Spaltennnamen benennen; dabei die gefundenen Daten einsetzen
        headers <- c('Aufschlüsselung', 'TTM', matches[[1]][, 1])
        
        #Vektor mit Header-Namen zu Dataframe hinzufügen
        names(df) <- headers
        
        #Zuweisen des fertigen Dataframes zu download-Objekt für Rückgabe durch Funktion
        download <- df
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bitte stellen Sie sicher, dass es sich um ein gültiges Tickersymbol handelt. Sie können dies unten bei der Quelle überprüfen."
      )
    )
    
    #download-Objekt in financials-Element des cache-Objekts speichern für Zugriff aus anderen Funktionen
    cache$financials <- download
    
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      #Infotext zur GuV-Tabelle in UI anzeigen
      renderText({
        #Infotext
        paste(
          "Die obenstehenden Daten sind in tsd-EUR angegeben und basieren auf den Daten der aktuellsten Veröffentlichung aus der untenstehenden Quelle.",
          "TTM steht für Trailing 12 Months und bezieht sich auf die vergangenen 12 Kalendermonate.",
          sep = "</br>"
        )
      })
    
    #Rückgabe des download-Objekts mit der Tabelle an Funktion
    return(download)
  },
  #Zellenausrichtung auf rechtsbündig stellen, damit die Dezimalstellen der Zahlen besser verglichen werden können
  align = "r")
  
  #Herunterladen der Bilanz der Aktie und Rendern der Tabelle als balanceSheet-Funktion
  #vgl. stackoverflow: https://stackoverflow.com/questions/58315274/r-web-scraping-yahoo-finance-after-2019-change
  balanceSheet <- renderTable({
    #try-Catch-Block für Fehlerbehandlung bei Problemen
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Herunterladen der Bilanz über Link aus dynamicUrl_balanceSheet-Funktion
        html <- read_html(dynamicUrl_balanceSheet())
        
        #Suchen der entsprechenden HTML-Knoten der Bilanz-Tabelle
        nodes <- html %>% html_nodes(".fi-row")
        #Erzeugen eines NULL-Objekts für Speichern des Dataframes der Bilanz
        df = NULL
        
        #Iteration über gefundenen Knoten
        for (i in nodes) {
          #Speichern der Spaltennamen und den Werten der Spalten
          r <-
            list(i %>% html_nodes("[title],[data-test='fin-col']") %>% html_text())
          #Zusammenfügen der Spalten zu einem Data Frame
          df <-
            rbind(df,
                  as.data.frame(
                    matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE),
                    stringsAsFactors = FALSE
                  ))
        }
        #Titel der Spalten nach Daten mit Format dd-mm-yyyy durchsuchen und passende Werte speichern
        matches <-
          str_match_all(
            html %>% html_node('#Col1-1-Financials-Proxy') %>% html_text(),
            '\\d{1,2}\\.\\d{1,2}\\.\\d{4}'
          )
        
        #Header des Data Frame analog zu Spaltennamen benennen; dabei die gefundenen Daten einsetzen
        headers <- c('Aufschlüsselung', matches[[1]][, 1])
        
        #Vektor mit Header-Namen zu Dataframe hinzufügen
        names(df) <- headers
        
        #Zuweisen des fertigen Dataframes zu download-Objekt für Rückgabe durch Funktion
        download <- df
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bitte stellen Sie sicher, dass es sich um ein gültiges Tickersymbol handelt. Sie können dies unten bei der Quelle überprüfen."
      )
    )
    
    #download-Objekt in balanceSheet-Element des cache-Objekts speichern für Zugriff aus anderen Funktionen
    cache$balanceSheet <- download
    
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      #Infotext zur Bilanz-Tabelle in UI anzeigen
      renderText({
        #Infotext
        paste(
          "Die obenstehenden Daten sind in tsd-EUR angegeben und basieren auf den Daten der aktuellsten Veröffentlichung aus der untenstehenden Quelle.",
          "TTM steht für Trailing 12 Months und bezieht sich auf die vergangenen 12 Kalendermonate.",
          sep = "</br>"
        )
      })
    #Rückgabe des download-Objekts mit der Tabelle an Funktion
    return(download)
  },
  #Zellenausrichtung auf rechtsbündig stellen, damit die Dezimalstellen der Zahlen besser verglichen werden können
  align = "r")
  
  
  #Herunterladen der Cashflow-Rechnung der Aktie und Rendern der Tabelle als cashflow-Funktion
  #vgl. stackoverflow: https://stackoverflow.com/questions/58315274/r-web-scraping-yahoo-finance-after-2019-change
  cashflow <- renderTable({
    #try-Catch-Block für Fehlerbehandlung bei Problemen
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Herunterladen der Bilanz über Link aus dynamicUrl_cashflow-Funktion
        html <- read_html(dynamicUrl_cashflow())
        
        #Suchen der entsprechenden HTML-Knoten der Bilanz-Tabelle
        nodes <- html %>% html_nodes(".fi-row")
        #Erzeugen eines NULL-Objekts für Speichern des Dataframes der Cashflow-Rechnung
        df = NULL
        
        #Iteration über gefundenen Knoten
        for (i in nodes) {
          #Speichern der Spaltennamen und den Werten der Spalten
          r <-
            list(i %>% html_nodes("[title],[data-test='fin-col']") %>% html_text())
          #Zusammenfügen der Spalten zu einem Data Frame
          df <-
            rbind(df,
                  as.data.frame(
                    matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE),
                    stringsAsFactors = FALSE
                  ))
        }
        #Titel der Spalten nach Daten mit Format dd-mm-yyyy durchsuchen und passende Werte speichern
        matches <-
          str_match_all(
            html %>% html_node('#Col1-1-Financials-Proxy') %>% html_text(),
            '\\d{1,2}\\.\\d{1,2}\\.\\d{4}'
          )
        #Header des Data Frame analog zu Spaltennamen benennen; dabei die gefundenen Daten einsetzen
        headers <- c('Aufschlüsselung', 'TTM', matches[[1]][, 1])
        #Vektor mit Header-Namen zu Dataframe hinzufügen
        names(df) <- headers
        
        #Zuweisen des fertigen Dataframes zu download-Objekt für Rückgabe durch Funktion
        download <- df
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bitte stellen Sie sicher, dass es sich um ein gültiges Tickersymbol handelt. Sie können dies unten bei der Quelle überprüfen."
      )
    )
    
    #download-Objekt in cashflow-Element des cache-Objekts speichern für Zugriff aus anderen Funktionen
    cache$cashflow <- download
    
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      #Infotext zur Cashflow-Rechnungs-Tabelle in UI anzeigen
      renderText({
        #Infotext
        paste(
          "Die obenstehenden Daten sind in tsd-EUR angegeben und basieren auf den Daten der aktuellsten Veröffentlichung aus der untenstehenden Quelle.",
          "TTM steht für Trailing 12 Months und bezieht sich auf die vergangenen 12 Kalendermonate.",
          sep = "</br>"
        )
      })
    #Rückgabe des download-Objekts mit der Tabelle an Funktion
    return(download)
  },
  #Zellenausrichtung auf rechtsbündig stellen, damit die Dezimalstellen der Zahlen besser verglichen werden können
  align = "r")
  
  #Funktion quicktest für Berechnung der Kennzahlen und deren Beurteilung nach Kralicek-Quicktest
  quickTest <- renderTable({
    #Objekt calculatedValues für Speichern der Werte
    calculatedValues <-
      #tryCatch-Block für Fehlerbehandlung
      tryCatch(
        #Festlegen des auszuführenden Code-Blocks
        expr = {
          #Laden der GuV, Bilanz und Cashflow-Rechnung aus dem cache-Objekt
          financialsDf <- cache$financials
          balanceSheetDf <- cache$balanceSheet
          cashflowDf <- cache$cashflow
          
          #Eigenkapital aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          ek <-
            as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Summe Eigenkapitalpositionen", 2]), fixed = TRUE))
          
          #Gesamtkapital aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          gk <-
            as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Gesamtverbindlichkeiten und Eigenkapitalpositionen", 2]), fixed = TRUE))
          
          #Berechnung der Eigenkapitalquote
          ekQuote <- (ek / gk) * 100
          
          #Berechnung des Fremdkapitals
          fk <- gk - ek
          
          #Liquide Mittel aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          liquideMittel <-
            as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Cash (gesamt)", 2]), fixed = TRUE))
          
          #Freier Cashflow aus Cashflow-Rechnung lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          cashflow <-
            as.numeric(gsub(".", "", as.character(cashflowDf[cashflowDf$Aufschlüsselung == "Freier Cashflow", 3][2]), fixed = TRUE))
          
          #Berechnung der Schuldentilgungsdauer
          tilgungsdauer <- (fk - liquideMittel) / cashflow
          
          #Fremdkapital-Zinsen aus GuV lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          fkZinsen <-
            as.numeric(gsub(".", "", as.character(financialsDf[financialsDf$Aufschlüsselung == "Zinsaufwand", 3]), fixed = TRUE))
          
          #Gewinn vor Steuern aus GuV lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          ergVorSt <-
            as.numeric(gsub(".", "", as.character(financialsDf[financialsDf$Aufschlüsselung == "Gewinn vor Steuern", 3]), fixed = TRUE))
          
          #EBIT=Gewinn vor Steuern + Zinsaufwand berechnen
          ebit <- ergVorSt + fkZinsen
          
          #Finanzergebnis=Gewinn Vor Steuern - EBIT berechnen
          finanzergeb <- ergVorSt - ebit
          
          #Ergebnis der gewöhnlichen Geschäftstätigkeit=EBIT + Finanzergebnis berechnen
          egt <- ebit + finanzergeb
          
          #Gesamtkapitalrentabilität=((EGT+FK-Zinsen)/GK)*100 berechnen
          rentabilität <- ((egt + fkZinsen) / gk) * 100
          
          #Betriebsleistung aus GuV lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
          betriebsleistung <-
            as.numeric(gsub(".", "", as.character(financialsDf[financialsDf$Aufschlüsselung == "Operativer Gewinn/Verlust", 3]), fixed = TRUE))
          
          #Cashflow-Leistungsrate=cashflow/Betriebsleistung*100 berechnen
          cashflowLeistung <- (cashflow / betriebsleistung) * 100
          
          #Beurteilung der EK-Quote nach Kralicek-Skala
          if (ekQuote < 0) {
            ekQuoteBeurt <- "insolvenzgefährdet"
          } else if (ekQuote <= 10) {
            ekQuoteBeurt <- "schlecht"
          } else if (ekQuote > 10 && ekQuote <= 20) {
            ekQuoteBeurt <- "mittel"
          } else if (ekQuote > 20 && ekQuote <= 30) {
            ekQuoteBeurt <- "gut"
          } else if (ekQuote > 30) {
            ekQuoteBeurt <- "sehr gut"
          }
          
          #Beurteilung der Schuldentilgungsdauer nach Kralicek-Skala
          if (tilgungsdauer >= 30) {
            tilgungsdauerBeurt <- "insolvenzgefährdet"
          } else if (tilgungsdauer < 30 && tilgungsdauer >= 12) {
            tilgungsdauerBeurt <- "schlecht"
          } else if (tilgungsdauer < 12 && tilgungsdauer >= 5) {
            tilgungsdauerBeurt <- "mittel"
          } else if (tilgungsdauer < 5 && tilgungsdauer >= 3) {
            tilgungsdauerBeurt <- "gut"
          } else if (tilgungsdauer < 3) {
            tilgungsdauerBeurt <- "sehr gut"
          }
          
          #Beurteilung der GK-Rentabilität nach Kralicek-Skala
          if (rentabilität < 0) {
            rentabilitätBeurt <- "insolvenzgefährdet"
          } else if (rentabilität <= 8) {
            rentabilitätBeurt <- "schlecht"
          } else if (rentabilität > 8 && rentabilität <= 12) {
            rentabilitätBeurt <- "mittel"
          } else if (rentabilität > 12 &&
                     rentabilität <= 15) {
            rentabilitätBeurt <- "gut"
          } else if (rentabilität > 15) {
            rentabilitätBeurt <- "sehr gut"
          }
          
          #Beurteilung der Cashflow-Leistungsrate nach Kralicek-Skala
          if (cashflowLeistung < 0) {
            cashflowLeistungBeurt <- "insolvenzgefährdet"
          } else if (cashflowLeistung <= 5) {
            cashflowLeistungBeurt <- "schlecht"
          } else if (cashflowLeistung > 5 &&
                     cashflowLeistung <= 8) {
            cashflowLeistungBeurt <- "mittel"
          } else if (cashflowLeistung > 8 &&
                     cashflowLeistung <= 10) {
            cashflowLeistungBeurt <- "gut"
          } else if (cashflowLeistung > 10) {
            cashflowLeistungBeurt <- "sehr gut"
          }
          
          #Vektor mit Namen der Kennzahlen erstellen
          Kennzahl <-
            c(
              "Eigenkapitalquote (%)",
              "Schuldentilgungsdauer in Jahren",
              "Gesamtkapitalrentabilität (%)",
              "Cashflow-Leistungsrate (%)"
            )
          
          #Vektor mit entsprechenem Ergebnis der Berechnungen erstellen
          Ergebnis <-
            c(ekQuote,
              tilgungsdauer,
              rentabilität,
              cashflowLeistung)
          
          #Vektor mit entsprechender Beurteilung der Ergebnisse erstellen
          Beurteilung <-
            c(ekQuoteBeurt,
              tilgungsdauerBeurt,
              rentabilitätBeurt,
              cashflowLeistungBeurt)
          
          #Zusammenführen der Vektoren als Data Frame für Darstellung in UI und speichern im Objekt calculatedValues
          calculatedValues <-
            data.frame(Kennzahl, Ergebnis, Beurteilung)
        },
        error = function(e) {
          #Rückgabe von null bei Errors
          return(NULL)
        },
        warning = function(w) {
          #Rückgabe von null bei Warnungen
          return(NULL)
        }
      )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob calculatedValues-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(calculatedValues),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Die Berechnung konnte nicht durchgeführt werden. Möglicherweise fehlen benötigte Werte in der GuV, der Bilanz oder der cashflow-Rechnung Wenn die GuV, die Bilanz und die cashflow-Rechnung auch nicht angzeigt werden, handelt es sich wahscheinlich um ein falsches Tickersymbol. "
      )
    )
    
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      renderText({
        #Infotext als HTML-Code um durch Paragraph-Tag einen Abstand zu erhalten
        HTML(
          "<p>Beurteilungsskala: sehr gut - gut - mittel - schlecht - insolvenzgefährdet.</p>
        Die obenstehende Tabelle basiert auf dem Aktien-Quicktest von Kralicek und soll eine Auskunft über die Kapitalkraft, die Verschuldung, die Rendite und die finanzielle Leistungsfähigkeit des Unternehmens geben.
        Die Berechnungen basieren auf den Daten der aktuellsten Veröffentlichung aus der untenstehenden Quelle.",
        )
      })
    #Rückgabe des calculatedValues-Objekts mit der Tabelle an Funktion
    return(calculatedValues)
  })
  
  #Funktion benchmark für Berechnung der Kennzahlen für Menüpunkt Weitere Kennzahlen
  benchmarks <- renderTable({
    #Objekt calculatedValues für Speichern der Werte und try-catch-Block für Fehlerbehandlung
    calculatedValues <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Laden der GuV und Bilanz aus dem cache-Objekt
        financialsDf <- cache$financials
        balanceSheetDf <- cache$balanceSheet
        
        #Eigenkapital aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        ek <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Summe Eigenkapitalpositionen", 2]), fixed = TRUE))
        #Gesamtkapital aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        gk <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Gesamtverbindlichkeiten und Eigenkapitalpositionen", 2]), fixed = TRUE))
        #Berechnung des Fremdkapitals
        fk <- gk - ek
        
        #Gewinn vor Steuern aus GuV lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        gewinn <-
          as.numeric(gsub(".", "", as.character(financialsDf[financialsDf$Aufschlüsselung == "Gewinn vor Steuern", 3]), fixed = TRUE))
        
        #Umlaufvermögen aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        uv <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Aktuelle Anlagen (gesamt)", 2]), fixed = TRUE))
        #Anlagevermögen aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        av <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Summe Anlagevermögen", 2]), fixed = TRUE))
        
        #Kurzfristige Verbindlichkeiten aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        kurzfrVerbind <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Aktuelle Verbindlichkeiten (gesamt)", 2]), fixed = TRUE))
        #Gesamte Verbindlichkeiten aus Bilanz lesen und als numerische Werte anstelle von tausender-Punkt-Trennung darstellen
        gesVerbind <-
          as.numeric(gsub(".", "", as.character(balanceSheetDf[balanceSheetDf$Aufschlüsselung == "Verpflichtungen (gesamt)", 2]), fixed = TRUE))
        
        #Berechnung der Liquidität 3. Grades=(UV/kurzfr. Verbindl.)
        currentRatio <-
          (uv / kurzfrVerbind) * 100
        
        #Berechnung des Verschuldungsgrads=FK/EK
        verschuldungsGrad <- fk / ek * 100
        
        #Berechnung der Eigenkapitalrentabilität aka Return on Equity=(Gewinn/EK)*100
        roe <-
          (gewinn / ek) * 100  
        #Berechnung der Eigenkapitaldeckung=EK/AV
        coverage <- ek / av * 100
        
        #Erstellen eine Vektors mit den Bezeichnungen der Kennzahlen
        Kennzahl <-
          c(
            "Liquidität 3. Grades",
            "Verschuldungsgrad",
            "Eigenkapitalrentabilität",
            "Eigenkapitaldeckung"
          )
        
        #Erstellen eines Vektors mit den Ergebnissen der Berechnung
        Ergebnis <-
          c(currentRatio, verschuldungsGrad, roe, coverage)
        
        #Erstellen eines Vektors mit Kontextbeschreibungen zu den Kennzahlen
        Kontext <-
          c(
            "Deckung der kurzfristig fälligen Schulden durch kurzfristig liquidierbares Umlaufvermögen",
            "Verhältnis des Eigenkapitals zum Fremdkapital",
            "Gewinn vor Steuern im Verhältnis zum Eigenkapital",
            "Deckung des Anlagevermögens durch Eigenkapital"
          )
        
        #Zusammenführen der Vektoren zu Dataframe
        df <- data.frame(Kennzahl, Ergebnis, Kontext)
        
        #Speichern des Objekts df im calculatedValues-Objekt
        calculatedValues <- df
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob calculatedValues-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        !is.null(calculatedValues),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Die Berechnung konnte nicht durchgeführt werden. Möglicherweise fehlen benötigte Werte in der GuV, der Bilanz oder der Cashflow-Rechnung. Wenn die GuV, die Bilanz und die Cashlow-Rechnung auch nicht angzeigt werden, handelt es sich wahrscheinlich um ein falsches Tickersymbol. "
      )
    )
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      #Infotext
      renderText(
        "Alle obenstehenden Zahlen sind in % angegeben.
                Die Berechnungen basieren auf den Daten der aktuellsten Veröffentlichung aus der untenstehenden Quelle."
      )
    #Rückgabe des calculatedValues-Objekts mit der Tabelle an Funktion
    return(calculatedValues)
  })
  
  ###############################################################################
  #### Aggregation und Anpassung der Daten für das Diagramm der Chartanalyse ####
  ###############################################################################
  #Funktion historicalData für Rückgabe der Daten für Chartanalyse
  historicalData <- reactive({
    #try-catch-Block für Fehlerbehandlung
    download <- tryCatch(
      #Festlegen des auszuführenden Code-Blocks
      expr = {
        #Lesen der historschen Kursdaten der Aktie über URL von dynamicUrl-Funktion
        read.csv(dynamicUrl(), sep = ",")
      },
      error = function(e) {
        #Rückgabe von null bei Errors
        return(NULL)
      },
      warning = function(w) {
        #Rückgabe von null bei Warnungen
        return(NULL)
      }
    )
    
    #Ausgabe einer Fehlermeldung bei Warnungen oder Errors
    #validate-Funktion prüft, ob download-Objekt nicht null ist, also ob der Download erfolgreich war
    validate(
      need(
        #Prüfbedingung
        is.data.frame(download),
        #Fehlermeldung wenn Prüfbedingung false ist
        "Bitte stellen Sie sicher, dass es sich um ein gültiges Tickersymbol handelt. Sie können dies unten bei der Quelle überprüfen."
      )
    )
    
    #Umwandlung der Daten in Date-Format
    download$Date <- as.Date(download$Date)
    
    #Bestimmen des frühesten Datums in Daten
    minMaxDates$min <- min(download$Date)
    
    #Bestimmen des spätesten Datums in Daten
    minMaxDates$max <- max(download$Date)
    
    #Berechnung des gleitenden Durchschnitts über 200 Tage
    download$rollMean200 <-
      #rollmean-Funktion berechnet den gleitenden Durchschnitt
      #Paramter 1: Daten für die Berechnung
      #Parameter 2: Fensterlänge des gl. Durchschnitts (hier: 200 Werte)
      #Parameter 3: Auffüllen mit NAs für Beibehaltung der Länge
      #Parameter 4: Berechnete Werte nach "rechts" also an Enddatum des Betracghtungszeitraums schieben
      rollmean(download$Adj.Close,
               200,
               fill = NA,
               align = "right")
    
    #Berechnung des gleitenden Durchschnitts über 50 Tage
    download$rollMean50 <-
      #rollmean-Funktion berechnet den gleitenden Durchschnitt
      #Paramter 1: Daten für die Berechnung
      #Parameter 2: Fensterlänge des gl. Durchschnitts (hier: 50 Werte)
      #Parameter 3: Auffüllen mit NAs für Beibehaltung der Länge
      #Parameter 4: Berechnete Werte nach "rechts" also an Enddatum des Betracghtungszeitraums schieben
      rollmean(download$Adj.Close,
               50,
               fill = NA,
               align = "right")
    
    #Filtern der Daten anhand der Datumseingabe in UI
    #if-Anweisung stellt sicher, dass Benutzer eigene Datumseingabe vorgenommen hat
    if (!is.null(input$Date[1]) && !is.null(input$Date[2])) {
      #Herausfiltern aller Werte vor Startdatum und nach Enddatum
      download <- download %>% filter(Date >= input$Date[1] &
                                        Date <= input$Date[2])
    }
    
    #Kursdaten von DAX und S&P 500 aus cache-Objekt lesen
    daxDf <- cache$dax
    snpDf <- cache$snp
    
    #Berechnung des Faktors für Anpassen des Kursverlaufs
    #Wenn DAX-Daten länger als Aktie-Daten, dann DAX-Daten entpsrechend auf gleiche Länge bringen und Faktor dann anhand der ersten Werte bestimmen
    #Wenn DAX-Daten kürzer als Aktie-Daten, dann DAX-Daten mit NAs auffüllen und Faktor anhand der ersten gemeinsamen Daten berechnen
    if (nrow(download) < nrow(daxDf)) {
      #Länge der des DAX-Werte-Vektors anpassen; Tail funktion schneidet überhängende Daten ab
      download$dax <- daxDf$Adj.Close %>% tail(nrow(download))
      #Berechnung des Faktors anhand der ersten Werte
      faktor <- download$dax[1] / download$Adj.Close[1]
      
    } else if (nrow(download) > nrow(daxDf)) {
      #Auffüllen der DAX-Werte mit NAs so oft wie DAX-Werte-Vektor kürzer ist als Aktie-Werte-Vektor
      download$dax <-
        c(rep(NA, nrow(download) - nrow(daxDf)), daxDf$Adj.Close)
      #Faktor anhand des ersten Tages berechnen wo DAX keine NAs hat; Bestimmung anhand der Differenz der Reihenanzahl
      faktor <-
        download$dax[nrow(download) - nrow(daxDf) + 1] / download$Adj.Close[nrow(download) - nrow(daxDf) +
                                                                              1]
    }
    
    #Werte des Dax mit Faktor anpassen um vergleichbaren Verlauf zu erhalten
    adjustedValuesDax <- download$dax / faktor
    #Angepasste Dax-Werte zu Data Frame hinzufügen
    download$dax <- adjustedValuesDax
    
    #Berechnung des Faktors für Anpassen des Kursverlaufs
    #Wenn S&P 500-Daten länger als Aktie-Daten, dann S&P 500-Daten entpsrechend auf gleiche Länge bringen und Faktor dann anhand der ersten Werte bestimmen
    #Wenn S&P 500-Daten kürzer als Aktie-Daten, dann S&P 500-Daten mit NAs auffüllen und Faktor anhand der ersten gemeinsamen Daten berechnen
    if (nrow(download) < nrow(snpDf)) {
      #Länge der des S&P 500-Werte-Vektors anpassen; Tail funktion schneidet überhängende Daten ab
      download$snp <- snpDf$Adj.Close %>% tail(nrow(download))
      #Berechnung des Faktors anhand der ersten Werte
      faktor <- download$snp[1] / download$Adj.Close[1]
      
    } else if (nrow(download) > nrow(snpDf)) {
      #Auffüllen der S&P 500-Werte mit NAs so oft wie S&P 500-Werte-Vektor kürzer ist als Aktie-Werte-Vektor
      download$snp <-
        c(rep(NA, nrow(download) - nrow(snpDf)), snpDf$Adj.Close)
      #Faktor anhand des ersten Tages berechnen wo S&P 500 keine NAs hat; Bestimmung anhand der Differenz der Reihenanzahl
      faktor <-
        download$snp[nrow(download) - nrow(snpDf) + 1] / download$Adj.Close[nrow(download) - nrow(snpDf) +
                                                                              1]
    }
    #Werte des S&P 500 mit Faktor anpassen um vergleichbaren Verlauf zu erhalten
    adjustedValuesSnp <- download$snp / faktor
    #Angepasste S&P 500-Werte zu Data Frame hinzufügen
    download$snp <- adjustedValuesSnp
    
    #Speichern des Infotexts in desc-Element des output-Objekts
    output$desc <-
      #Infotext
      renderText(
        "Die 50- und 200-Tage-Linie stellen den gleitenden Durchschnitt dar und können für die entsprechende Interpretation (vgl. 2.2.2 Chartanalyse) genutzt werden.
                                    Die Kurse des DAX und des S&P 500 sind an den Wert der Aktie zum Startdatum angepasst und dienen als Vergleich für die Kurs-Entwicklung.
                                    Das Start- und Enddatum können entsprechend angepasst werden, wenn der Vergleich ab einem anderen Startdatum erfolgen soll.
                                    Die Grafik basiert auf Daten aus der unten stehenden Quelle und ist von deren Qualität abhängig."
      )
    #Rückgabe des download-Objekts mit angepassten Daten an Funktion
    return(download)
  })
  
  
  ################################
  #### Output-Elemente für UI ####
  ################################
  
  #Plotly-Diagram für Chartanalyse in lineplot-Element des output-Objekts speichern
  output$lineplot <-
    #Erzeugen des Lineplots mittels renderPlotly-Funktion
    renderPlotly({
      #Speichern des ggplots in Objekt für schrittweises hinzufügen weiterer Schichten
      plotHistoricalData <-
        #Ggplot mit historischen Daten erstellen; y-Achse: Kurswert, x-Achse: Datum
        ggplot(
          #Festlegen der Daten auf Historische Kursdaten aus der historicalData-Funktion
          data = historicalData(),
          #Festlegen der x- und y-Achs-Werte sowieso zuweisen einer Farbe
          aes(
            x = Date,
            y = Adj.Close,
            group = 1,
            color = "Kursverlauf"
          )
        ) +
        #Festlegen der Achsenbeschriftungen
        xlab("Datum") + ylab("Angep. Tagesschlusskurs") + labs(title = "Kursverlauf und Indikatoren", color = "Legende") +
        #Festlegen der Liniendicke
        geom_line(lwd = 0.5)
      
      #Festlegen der Farben für Linien und Beschriftung in der Legende
      plotHistoricalData = plotHistoricalData + scale_color_manual(
        values = c(
          "Kursverlauf" = "black",
          "Gl. 200-Tage-Durchschnitt" = "red",
          "Gl. 50-Tage-Durchschnitt" = "green",
          "Angep. Verlauf des DAX" = "purple",
          "Angep. Verlauf des S&P 500" = "orange"
        )
      )
      
      #Wenn Checkbox für 50-Tage-Linie angekreuzt ist, wird 50-Tage-Linie angezeigt
      if (isTRUE(config$rollMean50Show)) {
        #Hinzufügen einer neuen Schicht zu plotHistoricalData mit Graph für 50-Tage-Linie
        plotHistoricalData = plotHistoricalData + geom_line(aes(
          x = Date,
          y = rollMean50,
          group = 1,
          color = "Gl. 50-Tage-Durchschnitt"
        ),
        lwd = 0.5)
      }
      
      #Wenn Checkbox für 200-Tage-Linie angekreuzt ist, wird 200-Tage-Linie angezeigt
      if (isTRUE(config$rollMean200Show)) {
        #Hinzufügen einer neuen Schicht zu plotHistoricalData mit Graph für 200-Tage-Linie
        plotHistoricalData = plotHistoricalData + geom_line(aes(
          x = Date,
          y = rollMean200,
          group = 1,
          color = "Gl. 200-Tage-Durchschnitt"
        ),
        lwd = 0.5)
      }
      
      #Wenn Checkbox für DAX angekreuzt ist, wird DAX-Linie angezeigt
      if (isTRUE(config$daxShow)) {
        #Hinzufügen einer neuen Schicht zu plotHistoricalData mit Graph für DAX-Linie
        plotHistoricalData = plotHistoricalData + geom_line(aes(
          x = Date,
          y = dax,
          group = 1,
          color = "Angep. Verlauf des DAX"
        ),
        lwd = 0.5)
      }
      
      #Wenn Checkbox für S&P 500 angekreuztist , wird S&P 500-Linie angezeigt
      if (isTRUE(config$snpShow)) {
        #Hinzufügen einer neuen Schicht zu plotHistoricalData mit Graph für S&P 500-Linie
        plotHistoricalData = plotHistoricalData + geom_line(aes(
          x = Date,
          y = snp,
          group = 1,
          color = "Angep. Verlauf des S&P 500"
        ),
        lwd = 0.5)
      }
      
      #Auf Abschluss und Speicherung der Daten warten
      req(plotHistoricalData)
      
      #Daten ausgeben mit maximaler höhe von 400p
      plotHistoricalData %>% ggplotly(max_height = 400)
    })
  
  #Je nach Auswahl im Drop-Down-Menü wir die entsprechende Tabelle gerendert
  #Speichern der anzuzeigenden Daten im fundamentals-Element des output-Objekts
  output$fundamentals <- reactive({
    #Anhand des Wertes des selected-Elements des input-Objekt wird die entsprechende Funktion aufgerufen
    if (input$selected == "GuV") {
      financials()
    } else if (input$selected == "Bilanz") {
      balanceSheet()
    } else if (input$selected == "Cashflow") {
      cashflow()
    } else if (input$selected == "Kralicek-Quicktest") {
      quickTest()
    } else if (input$selected == "Weitere Kennzahlen") {
      benchmarks()
    }
  })
}
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Definizioni e librerie preliminari

library(ggplot2)
library(jsonlite)
library(curl)
Regioni<-c("Italia","Piemonte","Valle d'Aosta","Lombardia","Veneto","Friuli Venezia Giulia",
           "Liguria","Emilia Romagna","Toscana","Umbria","Marche",
           "Lazio","Abruzzo","Molise","Campania","Puglia","Basilicata",
           "Calabria","Sicilia","Sardegna","P.A. Trento","P.A. Bolzano")
file1<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json"
file2<-"https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json"
ItalyDataFrame<-fromJSON(file1)
RegionDataFrame<-fromJSON(file2)


# Schema

ui <- fluidPage(

    # Application title
    titlePanel("Italy - CoViD19 Gestione dei casi"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Gestione della popolazione degli infetti da coronavirus in Italia"),
            h5("Selezionare un'area geografica."),
            selectInput("region","Regione:",Regioni,selected="Italia"),
            h5("Elaborazione su dati protezione civile (https://github.com/pcm-dpc/COVID-19)"),
            h5("Per info: info@ltlogics.it")
        )
        ,

        # Show the relevant plot
        mainPanel(
           plotOutput("stackchartPlot")
        )
    )
)

# Funzioni di servizio per disegnare i grafici

RegionChart<-function(RegionName,mydata) {
    days<-c()
    patients<-c()
    management<-c()
    # Creating base date to count days
    daybase="2020-02-23 18:00:00"
    ndaybase=strptime(daybase,"%Y-%m-%d %H:%M:%S")
    # Preparing subset with relevant state
    rdf=subset(mydata,denominazione_regione==RegionName)
    # now let us crunch data
    for (i in seq(nrow(rdf))) {
        days<-c(days,rep(difftime(strptime(rdf$data[i],"%Y-%m-%d %H:%M:%S"),
                                  daybase,
                                  units="days"),4))
        p1=rdf$isolamento_domiciliare[i]
        p2=rdf$ricoverati_con_sintomi[i]
        p3=rdf$terapia_intensiva[i]
        if (is.na(p1)|is.na(p2)|is.na(p3)) {
            patients<-c(patients,c(0,0,0,1))
        } else if (p1+p2+p3==0) {
            patients<-c(patients,c(0,0,0,1))
        } else {
            patients<-c(patients,c(p1/(p1+p2+p3),
                                   p2/(p1+p2+p3),
                                   p3/(p1+p2+p3),0 ))
        }
        management<-c(management,c("Dom","Hosp","ICU","None"))
    }
    mydf<-data.frame(days,patients,management)
    p<-ggplot(mydf, aes(x=days, y=patients, fill=management)) + 
        geom_area(alpha=0.6,size=1,position="stack", colour="black", show.legend=TRUE) +
        scale_y_continuous(labels=scales::percent) +
        xlab("Giorni dal 23 Febbraio") +
        ylab("% dai casi attivi") +
        ggtitle(paste("CoViD patient management - ",RegionName)) +
        scale_fill_manual(values = c("#00FF00FF", "#EFC000FF","#FF0000FF","#FF00FFFF"))+
        theme(legend.position = 'bottom')
    return(p)
}
ItalyChart<-function(mydata) {
    days<-c()
    patients<-c()
    management<-c()
    daybase="2020-02-23 18:00:00"
    ndaybase=strptime(daybase,"%Y-%m-%d %H:%M:%S")
    for (i in seq(nrow(mydata))) {
        days<-c(days,rep(difftime(strptime(mydata$data[i],"%Y-%m-%d %H:%M:%S"),
                                  daybase,
                                  units="days"),3))
        p1=mydata$isolamento_domiciliare[i]
        p2=mydata$ricoverati_con_sintomi[i]
        p3=mydata$terapia_intensiva[i]
        patients<-c(patients,c(p1/(p1+p2+p3),
                               p2/(p1+p2+p3),
                               p3/(p1+p2+p3) ))
        management<-c(management,c("Dom","Hosp","ICU"))
    }
    mydf<-data.frame(days,patients,management)
    p<-ggplot(mydf, aes(x=days, y=patients, fill=management)) + 
        geom_area(alpha=0.6,size=1,position="stack", colour="black", show.legend=TRUE) +
        scale_y_continuous(labels=scales::percent) +
        xlab("Giorni dal 23 Febbraio") +
        ylab("% dei casi attivi") +
        ggtitle("CoViD patient management - Italy") +
        scale_fill_manual(values = c("#00FF00FF", "#EFC000FF","#FF0000FF"))+
        theme(legend.position = 'bottom')
    return(p)
}

# Define server logic required to draw the chart
server <- function(input, output) {

    output$stackchartPlot <- renderPlot({
        if (input$region=="Italia") {
            p<-ItalyChart(ItalyDataFrame)
        } else {
            p<-RegionChart(input$region,RegionDataFrame)
        }
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

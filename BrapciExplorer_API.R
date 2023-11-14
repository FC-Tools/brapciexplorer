#BIBLIOTECAS

#instale se necessário

#install.packages("httr")
#install.packages("jsonlite") 
#install.packages("dplyr")
#install.packages("igraph")
#install.packages("visNetwork")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("flexdashboard")
#install.packages("fresh")
#install.packages("shinycssloaders")
#install.packages("ggplot2")

#Carregando bibliotecas

library(httr)
library(jsonlite) 
library(dplyr)
library(igraph)
library(visNetwork)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(fresh)
library(shinycssloaders)
library(ggplot2)

#Tema

theme_aas <-create_theme(
  adminlte_color(
    light_blue = "#d2d6de"
  ),
  adminlte_sidebar(
    width = "230px",
    dark_bg = "#c3c9d3",
    dark_hover_bg = "#000000",
    dark_color = "#fff"
  ),
  adminlte_global(
    content_bg = "#fff",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

#USER INTERFACE

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(disable = TRUE),
    
    dashboardSidebar(
      
      div(tags$style(HTML('.main-sidebar {padding-top:10px;}'))),
      
      title = div(class='text-center', img(src="brapciex2.png", width = 180, align = "centrer")),
      
      textInput("query_aux", "Busca:", "Insira um termo de busca",),
      
      selectInput("ano_1", "Ano inicial:", c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971","1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")),
      
      selectInput("ano_2", "Ano final:", c("2024","2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991", "1990", "1989", "1988", "1987", "1986", "1985", "1984", "1983", "1982", "1981", "1980", "1979", "1978", "1977", "1976", "1975", "1974", "1973", "1972","1971", "1970", "1969", "1968", "1967", "1966", "1965", "1964", "1963", "1962", "1961", "1960")),
      
      actionButton("runmodel", "Buscar",
                   style=
                     "background-color:#0062cc;
color:#fff;
margin-top:30px;
width:200px"
      )),
    
    dashboardBody(use_theme(theme_aas),
                  
                  tags$head(
                    tags$style(HTML('.container-fluid {padding-right: 0px;padding-left: 0px}')),
                    tags$style(HTML('.content{ min-height: 680px; }'))),
                  
                  fluidRow(column(
                    
                    width = 12, div(id = "content"),
                    
                    tabsetPanel(
                      
                      tabPanel(title=h5("Rede de coautorias",style='color:black;'),
                               
                               conditionalPanel("input.runmodel!=0",
                                                column(h3(textOutput("erro"),
                                                          style="background-color:#99c794;
width:300px;
margin-top:30px;
margin-bottom:15px;
padding:10px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align: center;"),
                                                       width = 12,
                                                       
                                                       withSpinner(visNetworkOutput("PlotCoupling", width = "100%", height = 450)),
                                                       
                                                       downloadButton("dl_pajek", "Download Pajek (.net)",
                                                                      style="background-color:#0062cc;color:#fff;"),
                                                       
                                                       downloadButton("dl_mtcx_coaut", "Download Matriz (.txt)",
                                                                      style="background-color:#0062cc;color:#fff;"),
                                                       
                                                       downloadButton("dl_net", "Download Rede (.html)",
                                                                      style="background-color:#0062cc;color:#fff;")))),
                      
                      tabPanel(title=h5("Autores mais produtivos",style='color:black;'),
                               
                               column(h3(textOutput("aut"),
                                         style= "background-color:#99c794;
width:150px;
margin-top:30px;
margin-bottom:15px;
margin-right:20px;
padding:5px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align: left;
float:left"),
                                      
                                      h3(textOutput("aut1"),
                                         style="background-color:#c594c5;
width:150px;
margin-top:30px;
margin-bottom:15px;
margin-right:20px;
padding:5px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align:left;
float:left"),
                                      
                                      h3(textOutput("aut2"),
                                         style="background-color:#5fb3b3;
width:180px;
margin-top:30px;
margin-bottom:15px;
margin-right:20px;
padding:5px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align:left;
float:left"),
                                      
                                      width = 12, dataTableOutput(outputId = "DataFrameAut"),
                                      
                                      downloadButton("dl_autores", "Download Data (.txt)",
                                                     style="background-color:#0062cc; color:#fff; margin-top:20px;"))),
                      
                      
                      tabPanel(title=h5("Fontes mais produtivas",style='color:black;'),
                               
                               column(h3(textOutput("period"),
                                         style="background-color:#99c794;
width:150px;
margin-top:30px;
margin-bottom:15px;
margin-right:20px;
padding:5px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align: left;
float:left"),
                                      
                                      h3(textOutput("period1"),
                                         style= "background-color:#c594c5;
width:180px;
margin-top:30px;
margin-bottom:15px;
margin-right:20px;
padding:5px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align:left;
float:left"),
                                      
                                      width = 12, dataTableOutput(outputId = "DataFrameFontes"),
                                      downloadButton("dl_fontes", "Download Data (.txt)",
                                                     style="background-color:#0062cc;
color:#fff;
margin-top:20px;"))),
                      
                      tabPanel(title=h5("Anos mais produtivos",style='color:black;'),
                               h3(textOutput("anomed"),
                                  style="background-color:#c594c5;
width:300px;
margin-top:15px;
margin-bottom:15px;
margin-left:25px;
padding:10px;
border-radius:5px;
font-size:16px;
color:#fff;
text-align: left;"),
                               
                               plotOutput(outputId = "barplotanos", width = "100%", height=450),
                               downloadButton("dl_anos", "Download Data (.txt)",
                                              style="background-color:#0062cc;
color:#fff;
margin-top:15px;
margin-bottom:5px;
margin-left:20px")),
                      
                    ))))))

#SERVER

server <- function(input, output){
  
  observe({
    
    input$runmodel
    
    if (input$runmodel==0)
      return()
    
    else
      
      isolate({
        
        r<-reactive({input$query_aux})
        
        req(r())
        
      })
    
    query<-isolate(r())
    query_1<-gsub(" ","+",query)
    y1<-input$ano_1
    y2<-input$ano_2
    
#BUSCA PELO TOTAL    
    
    page1<-paste0("https://cip.brapci.inf.br/api/brapci/search/v1?q=", query_1, "&di=", y1, "&df=", y2, "&start=") 
    
    data<-read_json(page1)
    total_busca<-data$total # Se o total for zero, erro de busca
    s<-seq(0, total_busca, 10)
    i<-c()
    
#LOOP PARA URL
    
    
if (total_busca!=0) { 
      
page1<-paste0("https://cip.brapci.inf.br/api/brapci/search/v1?q=", query_1, "&di=", y1, "&df=", y2, "&start=", s[i])
  


for (i in 1:length(s)) {
    
page1[i]<-paste0("https://cip.brapci.inf.br/api/brapci/search/v1?q=", query_1, "&di=", y1, "&df=", y2, "&start=", s[i])
    
} 



#Extração dos anos

anos_loop<-character()

for (i in 1:length(page1)){
  
  extract<-fromJSON(page1[i])
  corpus<-extract$works  
  anos<-corpus$year
  
  anos_loop<-append(anos, anos_loop)
}

#Extração dos periódicos

periodicos_loop<-character()

for (i in 1:length(page1)){
  
  extract<-fromJSON(page1[i])
  corpus_p<-extract$works
  corpus_list<-as.list(corpus_p)
  periodicos<-corpus_list$data$JOURNAL
  
  periodicos_loop<-append(periodicos, periodicos_loop)
  
}

#Extração das Autorias

fonte_loop<-character()

for (i in 1:length(page1)){
  
  extract<-fromJSON(page1[i])
  corpus_a<-extract$works
  corpus_list_a<-as.list(corpus_a)
  autores<-corpus_list_a$data$AUTHORS
  
  fonte_loop<-append(autores, fonte_loop)
  
}

#Autores, Periodicos e anos mais produtivos

#Autores

autores_prod <- (unlist(strsplit(fonte_loop, "; "))) #Autores

prod<-as.data.frame(table(autores_prod)) #Producao por autor

prod<-prod[order(prod$Freq,decreasing=TRUE),]

colnames(prod)[1]<-"Autores"
colnames(prod)[2]<-"Frequência"

prod_autores<-na.omit(prod[1:10,]) #10 autores mais produtivos

colnames(prod_autores)[1]<-"Autores"
colnames(prod_autores)[2]<-"Frequência"


#Periodicos

periodicos_prod<-(unlist(strsplit(periodicos_loop, ",")))

periodicos_prod<-as.data.frame(table(periodicos_loop)) #Producao por autor

periodicos_prod<-periodicos_prod[order(periodicos_prod$Freq,decreasing=TRUE),]

colnames(periodicos_prod)[1]<-"Fontes"
colnames(periodicos_prod)[2]<-"Frequência"

prod_periodicos<-na.omit(periodicos_prod[1:10,]) #10 periódicos mais produtivos

colnames(prod_periodicos)[1]<-"Fontes"
colnames(prod_periodicos)[2]<-"Frequência"


#Anos

anos_prod <- (unlist(strsplit(anos_loop, " "))) #Anos

anos_prod<-table(anos_prod)

anos_prod<-as.data.frame(anos_prod) #Producao por ano

anos_prod<-na.omit(anos_prod)

colnames(anos_prod)[1]<-"Anos"
colnames(anos_prod)[2]<-"Frequência"

anos_prod<-anos_prod[order(anos_prod$Anos,decreasing=F),]

prod_anos<-na.omit(anos_prod[1:15,]) #15 anos mais produtivos

colnames(prod_anos)[1]<-"Anos"
colnames(prod_anos)[2]<-"Frequência"


#ORGANIZACAO PARA REDE DE COAUTORIAS

b4<-strsplit(as.character(fonte_loop), split = "; " , fixed = FALSE)

b5<-as.data.frame(do.call(cbind, b4))

b6<-stack(b5)

#MATRIZES

mtx<-table(stack(b5))

mtx[mtx>1]<-1

mtx_coaut<-mtx%*%t(mtx) #matriz de coautorias

diag(mtx_coaut)<-0

mtx_coaut_df<-as.data.frame(mtx_coaut) #converte em contagem pareada


#REDE IGRAPH

rede_coaut<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")

mtx_adj_coaut_df<-tibble::rownames_to_column(mtx_coaut_df, " ")

#REDE VISNETWORK

vis_coaut<-toVisNetworkData(rede_coaut)
node_coaut<-data.frame("id"=vis_coaut$nodes$id, "label"=vis_coaut$nodes$label)
links_coaut<-as.data.frame(vis_coaut$edges)

if (length(vis_coaut$edges)==0){
  
  vis<-visNetwork(node_coaut, links_coaut)
  
} else {


colnames(links_coaut)[3]<-'width'

#REDE DE COAUTORIAS

#CONSTRUI A REDE VIS

vis<-visNetwork(node_coaut, links_coaut) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")    

}

#VISUALIZA O TOTAL DE RESULTADOS ENCONTRADOS

output$erro<-renderText({paste0("Total de documentos encontrados: ", total_busca)})      


#VISUALIZA A REDE DE COAUTORIA
    
output$PlotCoupling <- renderVisNetwork({
      
input$runmodel
      
vis})    

#VISUALIZA O GRAFICO DE FREQUÊNCIA DOS ANOS

output$barplotanos <- renderPlot({
  ggplot(prod_anos, aes(x = Frequência, y = Anos)) +
    geom_bar(stat ="identity" , fill = "#99c794") +
    coord_flip()+
    theme(text=element_text(size=20), panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black", size = 0.5))+
    labs(x = NULL, y = NULL)
})

output$anomed<-renderText({paste0("Média de documentos por ano: ", round(mean(anos_prod$Frequência),2)) }) #indicador de media de documentos por ano

#OUTPUS TABELAS DE FREQUENCIA

#OUTPUT ABA AUTORES

output$DataFrameAut <- renderDataTable(prod_autores, options = list(searching = FALSE, paging = FALSE, dom = 't')) #options = list REMOVE A BUSCA

output$aut<-renderText({paste0("Total de autores distintos: ", dim(prod)[1])})

output$aut2<-renderText({paste0("Média de documentos por autor: ", round(mean(prod$Frequência),2)) })

output$aut1<-renderText({paste0("Índice de Coautoria: ", round(sum(prod$Frequência)/total_busca,2)) })

#OUTPUT ABA FONTES

output$DataFrameFontes <- renderDataTable(prod_periodicos, options = list(searching = FALSE, paging = FALSE, dom = 't'))

output$period<-renderText({paste0("Total de fontes distintas: ", dim(periodicos_prod)[1])})

output$period1<-renderText({paste0("Média de documentos por fonte: ", round(mean(periodicos_prod$Frequência),2)) })


#DOWNLOADS

#OUTPUT DOWNLOAD AUTORES

output$dl_autores <- downloadHandler(
  filename = function(){
    paste("Autores", "txt", sep=".")
  },
  content = function(file){
    write.table(prod, file, sep="\t", row.names = F, col.names = TRUE)
    
  })


#OUTPUT DOWNLOAD FONTES

output$dl_fontes <- downloadHandler(
  filename = function(){
    paste("Fontes de Publicação", "txt", sep=".")
  },
  content = function(file){
    write.table(periodicos_prod, file, sep="\t", row.names = F, col.names = TRUE)
    
  })

#OUTPUT ANOS

output$dl_anos <- downloadHandler(
  filename = function(){
    paste("Anos de publicacao", "txt", sep=".")
  },
  content = function(file){
    write.table(anos_prod, file, sep="\t", row.names = F, col.names = TRUE)
    
  })

#OUTPUT DOWNLOAD MATRIZ DA REDE

output$dl_mtcx_coaut <- downloadHandler(
  filename = function(){
    paste("Matriz de Coautoria", "txt", sep=".")
  },
  content = function(file){
    write.table(mtx_adj_coaut_df, file, sep="\t", row.names = F, col.names = TRUE)
    
  })

#OUTPUT DOWNLOAD PAJEK

rede_paj<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")

if (length(vis_coaut$edges)==0){
  
  V(rede_paj)$id <- as.character(unique(row.names(mtx_coaut)))
  E(rede_paj)$weight<-0

}

else {

  E(rede_paj)$weight<-links_coaut$width
V(rede_paj)$id <- as.character(unique(row.names(mtx_coaut)))

}

#OUT PUT DOWNLOAD REDE: PAJEK

output$dl_pajek <- downloadHandler(
  
  filename = function(){
    paste("Rede de Coautoria_Pajek", "net", sep=".")
  },
  
  content = function(file){
    write.graph(rede_paj, file, format = "pajek")
  })

#DOWNLOAD IMAGEM: HTML

output$dl_net <- downloadHandler(
  
  filename = function(){
    paste("Rede de Coautoria_img", "html", sep=".")
  },
  
  content = function(file){
    visSave(graph=vis, file = file)
  })


} else {
      
      output$erro<-renderText({paste("Nada localizado para esta busca")})
      output$PlotCoupling <- renderVisNetwork({})  
      
    }
    
####
})
  
}

#Rodar o BRAPCI Explorer

shinyApp(ui, server)    

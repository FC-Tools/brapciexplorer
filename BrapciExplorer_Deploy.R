#Esta versão do BRAPCI Explorer utiliza o método de web scraping para extração de dados da BRAPCI (Base de Dados Referencial de Artigos de Periódicos em Ciência da Informação)

#Instale os pacotes se necessário

#install.packages(rvest)
#install.packages(dplyr)
#install.packages(igraph)
#install.packages(visNetwork)
#install.packages(shiny)
#install.packages(shinydashboard)
#install.packages(flexdashboard)
#install.packages(fresh)
#install.packages(shinycssloaders)
#install.packages(ggplot2)

#Bibliotecas utilizadas

library(rvest)
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


ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(disable = TRUE),
    
    dashboardSidebar(
      
      div(tags$style(HTML('.main-sidebar {padding-top:10px;}'))),
      
      title = div(class='text-center', img(src="brapciex2.png", width = 180, align = "centrer")),
      
      textInput("query_aux", "Busca:", "Insira um termo de busca",),
      
      
      selectInput("campo", "Campo:", c(Todos = "1", Autores= "2", Título = "3", "Palavras-chave" = "4", "Resumo" = "5", "Texto Completo" ="6")),
      
      selectInput
      ("ano_1", "Ano inicial:", c("1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")),
      
      selectInput("ano_2", "Ano final:", c("2024","2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991", "1990", "1989", "1988", "1987", "1986", "1985", "1984", "1983", "1982", "1981", "1980", "1979", "1978", "1977", "1976", "1975", "1974", "1973", "1972")),
      
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
    k<-input$campo
    y1<-input$ano_1
    y2<-input$ano_2
    
    i<-1
    
    page1<-paste0("https://brapci.inf.br/index.php/res/?q=", query_1, "&type=", k, "&year_s=", y1, "&year_e=", y2, "&p=", i)
    
    fonte<-read_html(page1[1])
    
    #verifica possível erro de busca
    
    localiza_erro<- fonte %>%
      html_nodes(".alert-warning") %>%
      html_text2()
    
    if (length(localiza_erro)==0) {
      
      total<- fonte %>%
        html_nodes(".col-4") %>%
        html_text2()
      
      total<-as.numeric(gsub("\\D", "", total[1])) #total de artigos
      
      i<-1
      
      page1<-paste0("https://brapci.inf.br/index.php/res/?q=", query_1, "&type=", k, "&year_s=", y1, "&year_e=", y2, "&p=", i)
      
      for (i in 1:(ceiling(total/20))){
        
        page1[i]<-paste0("https://brapci.inf.br/index.php/res/?q=", query_1, "&type=", k, "&year_s=", y1, "&year_e=", y2, "&p=", i)
        
        #Extracao dos periodicos
        
      }
      
      periodicos_loop<-character()
      
      for (i in 1:(ceiling(total/20))){
        
        extract<-read_html(page1[i])%>%
          html_nodes(".isPubishIn") %>%
          html_text2()
        
        periodicos_loop<-append(extract, periodicos_loop)
        
      }
      
      #Extracao Autorias
      
      fonte_loop<-character()
      
      for (i in 1:(ceiling(total/20))){
        
        extract<-read_html(page1[i])%>%
          html_nodes("i") %>%
          html_text2()
        
        fonte_loop<-append(extract, fonte_loop)
        
        
      }
      
      
      #Extracao Anos
      
      anos_loop<-character()
      
      for (i in 1:(ceiling(total/20))){
        
        extract<-read_html(page1[i])%>%
          html_nodes(".col-1") %>%
          html_text2()
        
        anos_loop<-append(extract, anos_loop)
        
      }
      
      #Autores, Periodicos e anos mais produtivos
      
      #Autores
      
      autores <- (unlist(strsplit(fonte_loop, "; "))) #Autores
      
      prod<-as.data.frame(table(autores)) #Producao por autor
      
      prod<-prod[order(prod$Freq,decreasing=TRUE),]
      
      colnames(prod)[1]<-"Autores"
      colnames(prod)[2]<-"Frequência"
      
      prod_autores<-na.omit(prod[1:10,]) #10 autores mais produtivos
      
      colnames(prod_autores)[1]<-"Autores"
      colnames(prod_autores)[2]<-"Frequência"
      
      #Periodicos
      
      periodicos<-(unlist(strsplit(periodicos_loop, ",")))
      
      periodicos<-as.data.frame(table(periodicos_loop)) #Producao por autor
      
      periodicos<-periodicos[order(periodicos$Freq,decreasing=TRUE),]
      
      colnames(periodicos)[1]<-"Fontes"
      colnames(periodicos)[2]<-"Frequência"
      
      prod_periodicos<-na.omit(periodicos[1:10,]) #10 autores mais produtivos
      
      colnames(prod_periodicos)[1]<-"Fontes"
      colnames(prod_periodicos)[2]<-"Frequência"
      
      #ANOS
      
      anos <- (unlist(strsplit(anos_loop, " "))) #Anos
      
      anos_aux<-table(anos)
      
      anos<-as.data.frame(table(anos)) #Producao por ano
      
      anos<-na.omit(anos)
      
      colnames(anos)[1]<-"Anos"
      colnames(anos)[2]<-"Frequência"
      
      anos<-anos[order(anos$Anos,decreasing=TRUE),]
      
      prod_anos<-na.omit(anos[1:15,]) #15 anos mais produtivos
      
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
      
      # mtx_coaut_tb<-as.table(mtx_coaut) #coautoria
      
      mtx_coaut_df<-as.data.frame(mtx_coaut) #converte em contagem pareada
      
      #Rede igraph
      
      rede_coaut<-graph_from_adjacency_matrix(mtx_coaut, weighted = T, mode = "undirected")
      
      mtx_adj_coaut_df<-tibble::rownames_to_column(mtx_coaut_df, " ")
      
      vis_coaut<-toVisNetworkData(rede_coaut)
      node_coaut<-data.frame("id"=vis_coaut$nodes$id, "label"=vis_coaut$nodes$label)
      links_coaut<-as.data.frame(vis_coaut$edges)
      colnames(links_coaut)[3]<-'width'
      
      
      #REDE DE COAUTORIAS
      
      vis_coaut<-toVisNetworkData(rede_coaut)
      
      #CONSTRUI A REDE VIS
      
      vis<-visNetwork(node_coaut, links_coaut) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr")
      
      
      #VISUALIZA O TOTAL DE RESULTADOS ENCONTRADOS
      
      output$erro<-renderText({paste0("Total de documentos encontrados: ", total)})
      
      #VISUALIZA A REDE DE COAUTORIA
      
      output$PlotCoupling <- renderVisNetwork({
        
        input$runmodel
        
        vis
        
      })
      
      
      #VISUALIZA O GRAFICO DE FREQUÊNCIA DOS ANOS
      
      output$barplotanos <- renderPlot({
        ggplot(prod_anos, aes(x = Frequência, y = Anos)) +
          geom_bar(stat ="identity" , fill = "#99c794") +
          coord_flip()+
          theme(text=element_text(size=20), panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black", size = 0.5))+
          labs(x = NULL, y = NULL)
      })
      
      output$anomed<-renderText({paste0("Média de documentos por ano: ", round(mean(anos$Frequência),2)) }) #indicador de media de documentos por ano
      
      #OUTPUS TABELAS DE FREQUENCIA
      
      #OUTPUT ABA AUTORES
      
      output$DataFrameAut <- renderDataTable(prod_autores, options = list(searching = FALSE, paging = FALSE, dom = 't')) #options = list REMOVE A BUSCA
      
      output$aut<-renderText({paste0("Total de autores distintos: ", dim(prod)[1])})
      
      output$aut2<-renderText({paste0("Média de documentos por autor: ", round(mean(prod$Frequência),2)) })
      
      output$aut1<-renderText({paste0("Índice de Coautoria: ", round(sum(prod$Frequência)/total,2)) })
      
      #OUTPUT ABA FONTES
      
      output$DataFrameFontes <- renderDataTable(prod_periodicos, options = list(searching = FALSE, paging = FALSE, dom = 't'))
      
      output$period<-renderText({paste0("Total de fontes distintas: ", dim(periodicos)[1])})
      
      output$period1<-renderText({paste0("Média de documentos por fonte: ", round(mean(periodicos$Frequência),2)) })
      
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
          write.table(periodicos, file, sep="\t", row.names = F, col.names = TRUE)
          
        })
      
      #OUTPUT ANOS
      
      output$dl_anos <- downloadHandler(
        filename = function(){
          paste("Anos de publicacao", "txt", sep=".")
        },
        content = function(file){
          write.table(anos, file, sep="\t", row.names = F, col.names = TRUE)
          
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
      E(rede_paj)$weight<-links_coaut$width
      V(rede_paj)$id <- as.character(unique(row.names(mtx_coaut)))
      
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
      
    }
    
    else {
      
      output$erro<-renderText({paste("Nada localizado para esta busca")})
      output$PlotCoupling <- renderVisNetwork({})
      
    }
    
  })
  
}

#Rodar o BRAPCI Explorer

shinyApp(ui, server)

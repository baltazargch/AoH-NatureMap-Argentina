library(tidyverse)
library(scrypt)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(leaflet)
library(raster)
library(DTedit)
library(rdrop2)
# library(rgdal)
library(sf)
library(plotly)
library(scales)

ui <- dashboardPage(title = 'Área de Hábitat - Applicación NatureMap Argentina', 
                    dashboardHeader(title = 'Área de Hábitat NatureMap', 
                                    titleWidth = 280, dropdownMenuOutput("avanceEspecies")),
                    dashboardSidebar(disable = F, width = 230,
                                     sidebarMenuOutput('sidebar')), 
                    dashboardBody(
                      tags$head(
                        tags$style(
                          HTML(
                            ".skin-blue .main-header .logo { background-color: #3c8dbc; }
                        .skin-blue .main-header .logo:hover { background-color: #3c8dbc; }
                        h3 { font-size: 20px; }
                        h4 { font-size: 18px; }
                        .main-sidebar { font-size: 20px; }
                        .shiny-notification { position:fixed; top: calc(50%); 
                        left: calc(50% - 50px); width: 400px; font-size: 20px;font-style: italic; }"
                          )
                        )
                      ),
                      tabItems(
                        tabItem(tabName = 'tabLogin',
                                column(12, useShinyalert(), useShinyjs(),
                                       div(id='all', align="center", style='height: 100%;',
                                           actionButton('anfibios', label = 'Anfibios', 
                                                        icon = icon('frog'), 
                                                        style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #ACD43B;
                                          color: white;'),
                                          actionButton('reptiles', label = 'Reptiles', 
                                                       icon = icon('dragon'), 
                                                       style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #60E841;
                                          color: white;'),br(),
                                          actionButton('mamiferos', label = 'Mamíferos', 
                                                       icon = icon('paw'), 
                                                       style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #45D16F;
                                          color: white;'),
                                          actionButton('aves', label = 'Aves', icon = icon('dove'), 
                                                       style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #7AD1BB;
                                          color: white;'), br(), 
                                          actionButton('peces', label ='Peces', icon = icon('fish'),  
                                                       style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #2CB2D1;
                                          color: white;'), 
                                          actionButton('plantas', label = 'Plantas', 
                                                       icon = icon('tree'), 
                                                       style='padding:4px; font-size:150%; 
                                          height: 200px; width: 40%; background: #2C77D1;
                                          color: white;') 
                                       )
                                )
                        )),
                      tags$div(uiOutput('body'))
                    ),
                    skin = 'blue')


server <- function(input, output, session){
  appServer <- eval(parse(file = 'R/server.R'))
  sidebarUI <- parse(file='R/sidebar_aoh.R')
  boudyUI <- parse(file='R/body_aoh.R')
  
  observeEvent(input$empezar, {
    removeModal()
    removeUI(selector = "div:has(> #shiny-tab-tabLogin)", multiple = T)
  })
  
  output$sidebar <- renderMenu({
    if(!is.null(input$empezar) && input$empezar > 0){
      eval(sidebarUI)
    } else {
      sidebarMenu(id='tabs',
                  menuItem('Inicio de sesión',  selected = TRUE,
                           tabName = 'tabLogin',startExpanded = TRUE, 
                           icon=icon('sign-in-alt')))
    }
  })
  
  output$body <- renderUI({
    if(!is.null(input$empezar) && input$empezar > 0){
      appServer(input, output, session)
      eval(boudyUI)
    }
  })
  
  # crds <- readRDS('data/credentials.rds') #itworks
  source('R/udf/drop_read_rds.R')
  token <- readRDS('info/token.rds')
  crds <- drop_read_crds(dtoken=token)
  
  lastActive <- reactiveValues(id = character(), 
                               type = character(), 
                               user = character(), 
                               species = character())
  
  lapply(
    X = c('anfibios', 'reptiles', 'mamiferos', 'aves', 'peces', 'plantas'),
    FUN = function(i){
      observeEvent(input[[i]], priority = 1, {
        if (input[[i]] > 0) {
          lastActive$id = i   
        }
      })
    }
  )
  
  observeEvent(input$anfibios | input$reptiles | input$mamiferos | 
                 input$aves | input$peces | input$plantas, ignoreInit = T, ignoreNULL = T, 
               priority = 0, {
                 showModal(
                   modalDialog(title = 'Autentificación de usuario: ',
                               tagList(
                                 textInput('UserIN',label = 'Usuario:',
                                           placeholder = 'Ingrese el nombre de usuario', 
                                           width = '80%'), br(), 
                                 passwordInput('PwIN',label = 'Contraseña:',
                                               placeholder = 'Ingrese su contraseña', 
                                               width = '80%')),
                               footer = tagList(
                                 actionButton('login', 'Iniciar sesión'),modalButton("Cerrar")),
                               size = 'm')
                 )
               })
  
  observeEvent(input$login, ignoreInit = T, ignoreNULL = T, priority = 1, {
    
    if(input$PwIN == '' | input$UserIN == ''){
      shinyalert('Campos incompletos', 'Por favor, indique usuario y contraseña.', type='error')
    } else {
      user <- input$UserIN
      pw <- input$PwIN
      grp <- lastActive$id
      
      userok <- ifelse(user %in% crds$user, TRUE, FALSE)
      
      if(!userok){
        shinyalert('Usuario incorrecto', 'Por favor verifique el nombre de usuario 
                   y vuelva a intentarlo', type = 'error')
      } else if(userok){
        grpok <- ifelse(grp  %in% strsplit(crds$groups[ match(user, crds$user)], ',')[[1]], 
                        TRUE, FALSE)
        pwok <- verifyPassword(crds$pw[ match(user, crds$user) ], pw)
        
        if(!grpok){
          shinyalert('Grupo incorrecto', 'El usuario seleccionado no tiene acceso a este grupo', 
                     type = 'error')
        } else if(grpok & !pwok){
          shinyalert('Contraseña incorrecta', 'La contraseña es incorrecta. Vuelva a intentarlo.', 
                     type = 'error')
        } else if(grpok & pwok){
          lastActive$type <<- crds$type[crds$user == user]
          lastActive$user <<- user
          if(crds$type[crds$user == user] == 'invitado'){
            lastActive$species <<- crds$perm_sp[crds$user == user] %>% 
            parse(text = .) %>% 
            eval(.)
          }
          shinyalert('¡Usuario autenticado!',
                     text = tagList(h4('Da click en ', strong('Empezar'),
                                       'y esperá a que todo esté listo...'),
                                    actionBttn('empezar', 'Empezar', style = 'jelly', 
                                               color='success')), 
                     showConfirmButton = F,immediate = T,
                     html = TRUE)
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)

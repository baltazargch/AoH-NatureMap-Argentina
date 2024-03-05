library(tidyverse)
library(scrypt)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinyFiles)
library(leaflet)
library(ggplot2)
library(shinyjs)
library(raster)
library(DTedit)
library(rdrop2)
library(dplyr)
# library(rgdal)
library(sf)
library(plotly)
library(scales)

tabItems(
  # Inicio -----------------------------------------------------------------------------------------------
  tabItem(tabName = "tabInicio",
          fluidRow(
            column(width = 2, shinyjs::hidden(actionButton('switchtab', 'Switch tab')),
                   box(width = NULL, title = 'Datos generales', solidHeader = T,
                       status = 'info', uiOutput('infoFam') %>% withSpinner(), 
                       uiOutput('infoGen'),
                       uiOutput('infoEsp')),
                   actionBttn('demo', 'Tutoriales', icon = icon('play'), 
                              color = 'success', style = 'jelly', 
                              size = 'lg', block = T), uiOutput('demoPlay')
            ),
            column(width = 10,
                   box(title = "Instructivo",  width=NULL, solidHeader = T, status = 'primary',
                       h3("Esta es una aplicación sencilla pensada para mantener ordenada la información sobre AOH y poder revisar ",
                          "especie por especie por parte de los expertos. Este interfaz permite que por cada pestaña los expertos ",
                          "puedan ver, editar y descargar la información por Familia, de manera compacta y ordenada En las pestañas ", 
                          "siguientes van a encontrar:"),
                       fluidRow(
                         column(width =8,
                                br(),  
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('paw'), "Conservación"), solidHeader = T, 
                                    status = 'info', 
                                    h3("Se muestran las tablas según se seleccione del menú una familia. En dichas tablas ",
                                       "los expertos podrán revisar y editar categorías de amenaza nacional y global y endemismo. ",
                                       "En esta pestaña podrán guardar la información, marcar la familia como revisada o descargar ", 
                                       'la información.')
                                ), 
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('tree'), 'Hábitats'), solidHeader = T, 
                                    status = 'info', 
                                    h3("En esta pestaña los expertos podrán ver los datos de las especies según vayan seleccionando una familia ", 
                                       "el menú superior. Especie por especie se mostrará en la izquierda la tabla editable donde están ", 
                                       "los hábitats", em("disponibles"), "para la especie. A la derecha verán un mapa con dos botones arriba. ", 
                                       em("Dibujar rango"), "añadirá el rango al mapa para visualizar. ", em("Dibujar hábitat"), 
                                       "añadirá los hábitats", strong("en la tabla actual"), "para la especie. ",
                                       "Si el experto considera que se debe modificar alguno deberá usar los botones 'Nuevo' o 'Eliminar'. ", 
                                       "En caso de 'Nuevo', podrán agregar hábitats de una lista desplegable de los hábitats ", 
                                       strong("disponibles dentro del rango. "),'Al editar la tabla podrán presionar de nuevo', em('Dibujar hábitat'),
                                       ' y se actualizará el mapa. ', "Para esta versión se utilizan las coberturas de GlobCover 2019. ",
                                       'Además, los expertos deberán indicar si la especie es apta, marginal o no apta para habitar forestaciones',
                                       ". Los expertos podrán descargar la información, guardar, marcar como revisada y visualizar cambios.",
                                       "Los datos se guardarán localmente. ")
                                ),
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('map-pin'), 'Elevación'), solidHeader = T,
                                    status = 'info', 
                                    h3("La idea de esta pestaña es igual que la anterior. Los expertos podrán ver y modificar, si hace falta, los ", 
                                       "límites altitudinales. Notar que para cada especie se muestran el máximo y mínimo disponible", strong("dentro del rango. "), 
                                       "Cualquier dato por fuera de estos hará que la aplicación use el disponible y no el editado. Igualmente,", 
                                       " cualquier carácter no númerico generará un error. El procedimiento de descargar y subir infoormación es igual el anterior.")
                                ),
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('cog'), 'Visor y restaurador'), 
                                    solidHeader = T, status = 'info', 
                                    h3('Una vez modificada la o las especies, estas desaparecerán del menú de selección en cada pesta. ', 
                                       'En esta pestaña, los expertos podrán explorar los cambios realizados hasta el momento. Ya sea por ', 
                                       'categoría (Conservación, Hábitat o Elevación) o en general. También brinda la oportunidad de ', 
                                       'cambiar el estado de una especie de revisada a no revisada. De esta manera, en la siguiente sesión ',
                                       'la especie volverá aparecer en todas las pestañas para ser revisada de nuevo o corregida.')
                                ), 
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('globe-americas'), 'Área de Hábitat'), 
                                    solidHeader = T, status = 'info', 
                                    h3('Esta pestaña es de exploración únicamente y es experimental. El experto podrá calcular el AOH, ',
                                       'a partir de los datos guardados y visualizar el mismo. Esto solo para las especies marcadas ', 
                                       'como revisadas para TODAS las categorías. En esta pestaña el AOH se calcula del cruce de información ', 
                                       'de Hábitats y Elevación, sin embargo, el producto final tendrá incorporada información de huella humana ', 
                                       'y de forestaciones (para las especies marcadas como no aptas para este tipo de coberturas). Además ', 
                                       'los expertos podrán ver información adicional del método en la subpestaña Notas AOH.')
                                ),
                                box(collapsible = T, collapsed = T, width = NULL, title = span(icon('table'), 'Resumen sesión'), solidHeader = T,
                                    status = 'info', 
                                    h3("Acá podrás ver datos resumen y avance del proceso, como por ejemplo, cuántas especies se corrigieron", 
                                       " hasta el momento en el total de sesiones. ", 
                                       strong('Una vez llegado a esta pestaña, la información',
                                              ' resumen no varía si el experto decide volver a editar. 
                                                                    PERO LOS CAMBIOS SI SERÁN GUARDADOS AL SUBIR INFORMACIÓN.'))
                                )
                                
                         ), 
                         column(width =4, 
                                br(), br(),br(), br(),
                                infoBox(width = NULL, title = '¡Expande las pestañas!', value = 'Hay información útil', 
                                        color = 'red', icon = icon('plus')))
                       )
                   )
            )
          ) 
  ),
  # tabConservación --------------------------------------------------------------------------------------
  tabItem(tabName = "tabCon",
          fluidRow(
            box(width = 9, title = 'Listado especies y conservación', status = 'primary', solidHeader = T, 
                fluidRow(column(width =8, uiOutput("familia") %>% withSpinner()),
                         column(width =4, 
                                infoBox(width = NULL, title = 'Para editar o eliminar: ', 
                                        value = 'Selecciona primero la fila y luego presiona los botones', color = 'red', 
                                        icon = icon('lightbulb')))
                ),
                uiOutput("grupo_seleccionado"), 
                downloadButton("downloadConGrupo", "Descargar grupo"),
                br(),
                column(width = 3, br()),
                actionBttn('uploadCon', label ='Confirmar cambios', icon = icon('upload'), style = 'jelly', 
                           color='success', size='lg'), 
                actionBttn('marcarCon', 'Marcar familia como revisada', icon = icon('check'), 
                           style = 'pill', color='warning', size='lg')),
            column(width= 3, 
                   box(width = NULL,  title = 'Tips', status = 'info', solidHeader = T,
                       infoBox(width = NULL,'1. Escoger familia', 'Podrás cambiar de familia a editar en cada pestaña'),
                       infoBox(width = NULL,'2. Editar o eliminar', '¡Fijarse abajo de la tabla, hay más páginas!'), 
                       infoBox(width = NULL,'3. Descarga o copia', 'Revisa que los cambios se están generando'), 
                       infoBox(width = NULL,'4. Guarda en tu PC', 'Aunque al final se subirán, es recomendable tener una copia')
                   )
            )
          )
  ), 
  # tabHábitats ------------------------------------------------------------------------------------------
  tabItem(tabName="tabHabs",useShinyjs(),
          fluidRow(
            box(width = 4,height = 800, title = 'Tabla de hábitats por especie', status = 'primary', solidHeader = T, 
                fluidRow(column(width=4,uiOutput('habsFamilia')), 
                         column(width=6,uiOutput("especietab3"))),
                fluidRow(column(width = 12, 
                                box(width = NULL, title = span(icon('database'), 'Información disponible'),
                                    htmlOutput('showInfo'), collapsible = T, collapsed = F, solidHeader = T, status = 'info'))),
                uiOutput("datos_especie"),br(),
                fluidRow(
                  column(width=12,
                         downloadButton("downloadHabsEspecie", "Descargar especie"), 
                         downloadButton("downloadHabsGrupo", "Descargar grupo"), 
                         actionBttn('uploadHabs', label ='Confirmar cambios', icon = icon('upload'), style = 'jelly', 
                                    color='success', size='md'))), 
                fluidRow(
                  br(),
                  column(width = 12, offset=1,
                         actionBttn('marcarForest', 'Decidir forestaciones', icon = icon('tree'), 
                                    style = 'jelly', color='warning', size='md'),
                         actionBttn('marcarHabs', '¿Especie revisada?', icon = icon('check'), 
                                    style = 'jelly', color='danger', size='md')
                  )
                )
            ),
            box(width = 8, height = 800, title = 'Mapa dinámico: rangos y hábitats', status = 'success', solidHeader = T,useShinyalert(),
                fluidRow(
                  column(7, 
                         div(style = "height:50px;",
                             actionBttn("dibujarR", "Rango", style='jelly', color='primary', size='sm'),
                             actionBttn("dibujarH", "Hábitats", style='jelly', color='success', size='sm'),
                             actionBttn("dibujarF", "Forestaciones", style='jelly', color='warning', size='sm'),
                             actionBttn("kml", "+ kml", style='jelly', color='royal', size='sm'),
                             actionBttn("dibujarP", "Borrar todo", style='jelly', color='danger', size='sm')
                         )),
                  column(2, offset = 0, div(style = "height:50px;", h4(strong('Subir archivo: ')))),
                  column(3, offset = 0,
                         div(style = "height:50px;",
                             fileInput('shapes', label = NULL, multiple = F, accept = '.kml',
                                       buttonLabel='Explorar', width= '100%')))),
                # fluidRow(br()),
                leafletOutput("mapadatos", height = 680) %>% withSpinner()
            )
          )
  ), 
  # tabElevación -----------------------------------------------------------------------------------------
  tabItem(tabName = "tabElev",useShinyjs(),
          fluidRow(
            box(width = 4, title = 'Tabla de elevación por especie', status = 'primary', solidHeader = T, 
                fluidRow(column(width=4, uiOutput('habsFamiliaE')), 
                         column(width=6, uiOutput("especietab4"))),
                fluidRow(column(width = 12, 
                                box(width = NULL, title = span(icon('database'), 'Información disponible'),
                                    htmlOutput('elevDisp'), collapsible = T, collapsed = F, solidHeader = T,
                                    status = 'info'))),
                useShinyalert(),
                uiOutput("datos_especie_elev"),
                # downloadButton("downloadElevEspecie", "Descargar especie"), 
                downloadButton("downloadElevGrupo", "Descargar grupo"), 
                br(),
                br(),
                column(width=12, offset = 1,
                       actionBttn('uploadElev', label ='Confirmar cambios', icon = icon('upload'), style = 'jelly', 
                                  color='success', size='md'), 
                       actionBttn('marcarElev', '¿Especie revisada?', icon = icon('check'), 
                                  style = 'jelly', color='danger', size='md'))
            ), 
            box(width = 8, height = 800, title = 'Mapa dinámico: elevación', status = 'success', solidHeader = T, useShinyalert(),
                fluidRow(
                  column(7, 
                         div(style = "height:50px;",
                             actionBttn("dibujarE", "Elevación", style='jelly', color='primary', size='sm'),
                             actionBttn("kmlElev", "+ kml", style='jelly', color='royal', size='sm'))),
                  column(2, offset = 0, div(style = "height:50px;", h4(strong('Subir archivo: ')))),
                  column(3, div(style = "height:50px;",
                                fileInput('shapesElev', label = NULL, multiple = F, accept = '.kml',
                                          buttonLabel='Explorar', width= '100%')))),
                leafletOutput("mapadatosE", height = 680) %>% withSpinner()
            )
          )
  ),
  # tabResumen -------------------------------------------------------------------------------------------
  tabItem(tabName = "tabResumen", #shinyalert(),
          fluidRow(
            column(9,
                   tabBox(title = tagList(icon('chart-bar'), 'Avance revisiones'), 
                          height = 750, width = NULL, 
                          tabPanel("Conservación",  plotlyOutput('resumenCon', height = 600)), 
                          tabPanel("Hábitats",  plotlyOutput('resumenHabs', height = 600)), 
                          tabPanel("Elevación",  plotlyOutput('resumenElev', height = 600)), 
                          tabPanel("Eliminaciones", uiOutput('resumenDelete')))
            ), 
            column(3,
                   box(width=NULL, title = "Finalizar", status = 'success', solidHeader = F, useShinyalert(),
                       h4('Si le diste click en', strong('Guardar cambios ó revisar especie'),
                          'en las páginas anteriores, los cambios han sido guardados. ',br(),div(), 
                          'Si no lo has hecho aún, antes de finalizar, ve a las pestañas y dale click en el botón verde para guardarlos. ',
                          'Pero recuerda que hay que hacerlo por cada especie revisada/editada. ',
                          'Si le das finalizar o cierras la aplicación SIN haber guardado cambios ',
                          strong('SE PERDERÁN TODOS LOS CAMBIOS.')),
                       column(8, offset = 2,
                              useShinyalert()
                              # conditionalPanel("false", downloadButton("downloadData")),
                              ))
            )
          )
          
  ), #final tabItem tab Resuemn
  
  # tabVisual --------------------------------------------------------------------------------------------
  tabItem(tabName = "tabVisor", useShinyalert(),
          fluidRow(
            column(3,
                   box(width = NULL, title = 'Restaurador de especies', status = 'info', 
                       solidHeader = T, 
                       radioButtons('categoriaRev', label = 'Categoría:', 
                                    choices = c('Conservacion', 'Habitats', 'Elevacion', 
                                                'Todas'), selected = NULL),
                       uiOutput('familiaRev'), 
                       uiOutput('especieRev'), 
                       actionBttn('restaurar', 'Desmarcar revisado', style = 'jelly', 
                                  size = 'md', color = 'danger'))), 
            column(9, 
                   tabBox(title = 'Visualizador de cambios en especies',width = NULL, 
                          tabPanel(title = 'Conservación', uiOutput('conscons')), 
                          tabPanel(title = 'Hábitats & Elevación', 
                                   h2('Datos de elevación guardados:'), 
                                   DTOutput('elevRev'), 
                                   br(),
                                   h2('Datos de forestaciones guardados:'),
                                   DTOutput('forestRev'),
                                   br(), 
                                   h2('Datos de hábitats guardados:'),
                                   DTOutput('habsRev'))))
          )
  ), 
  # tabAOH -----------------------------------------------------------------------------------------------
  tabItem(tabName = "tabAOH", useShinyalert(), 
          tabBox(title = 'Calculador y visualizador de AOH', width=12, height = 800,
                 tabPanel(title = 'AOH', 
                          fluidRow(column(2, uiOutput('familiaAOH', inline = T)),
                                   column(2, uiOutput('especieAOH', inline = T)),
                                   column(2, br(),
                                          actionBttn('makeAOH', 'Calcular AOH', icon = icon('play'),
                                                     style = 'jelly', color = 'royal'))
                          ),
                          fluidRow(br(),
                                   column(4, plotOutput('plotRH')),
                                   column(4, plotOutput('plotRE')), 
                                   column(4, plotOutput('plotAOH'))
                          )
                 ),
                 tabPanel(title = 'Notas AOH', 
                          h2(strong('Método: ')), 
                          h4('El Área de Hábitat es calculado con base en la información ', 
                             'más actualizada de la especie respecto a tres fuentes principales ', 
                             'de información: 1) su distribución actual, 2) los hábitats aptos ', 
                             'para su supervivencia (e.g., alimentación, reproducción), y 3) ', 
                             'la franja altitudinal a la que se encuentra, por ejemplo, por límites ', 
                             'en su fisiología. La metodlogía sigue a ', 
                             a('Brooks et al. (2019)', 
                               href='https://www.sciencedirect.com/science/article/pii/S0169534719301892', 
                               target='_blank'), ' y las fuentes de datos son detalladas a continuación.'), 
                          br(), 
                          h2(strong('Fuente de datos: ')),
                          h4('Para esta app las siguientes fuentes de datos fueron empleadas: '), 
                          h4(strong('- Rangos de especies: '), 'Los rangos de las especies fueron ', 
                             'obtenidos mediante el esfuerzo de la ', 
                             a('Categorización de los Mamíferos de Argentina', 
                               href= 'http://cma.sarem.org.ar/', target='_blank'), ' y el MAyDS a través', 
                             ' del proyecto NatureMap.'), 
                          h4(strong('- Cobertura del suelo: '), 'la capa de cobertura de suelo usada en ', 
                             'esta app corresponde al mapa de coberturas globales del ', 
                             a('Copernicus Global Land Cover ver. 3.0.1', 
                               href='https://www.mdpi.com/2072-4292/12/6/1044', 
                               target='_blank'),'(disponible para visualización ', a('aquí', 
                                                                                     href='https://lcviewer.vito.be/', 
                                                                                     target='_blank'), ').',
                             'Originalmente esta capa está a 100m de resolución. ',
                             'Para su uso, esta misma fue procesada ', 
                             'a una resolución de 1 km² mediante el método de nearest neighbor.'),
                          h4(strong('- Modelo de elevación del suelo: '), 'la capa de elevación es derivada ',
                             'de ', a('SRTM ver. 4', href='http://srtm.csi.cgiar.org/', target='_blank'), '. ', 
                             'Originalmente esta capa está a 90m de resolución. Para su uso, esta misma fue ', 
                             'procesada a una resolución de 1km² mediante el método de interpolación bilineal.')
                 )
          )
  ), 

# Invitados ----------------------------------------------------------------------------------------------
  tabItem(tabName = "tabInvita2", 
          fluidRow( 
                box(
                  width = 4, title = 'Crear credenciales para nuevo invitado', solidHeader = T,
                  status = 'primary',
                  h4('A continuación, agregue la información en cada casilla según corresponda. ',
                     'Al presionar en el botón ', strong('Generar credenciales'), 'y si toda la ',
                     'información es validada (no tiene caracteres no aceptados), aparecerá ',
                     'la información completa para enviar a su invitado: usuario, contraseña y ',
                     'link de acceso. La cual podrá copiar y enviar a su invitado. Una vez pasados ',
                     'los días de acceso las credenciales se invalidarán.'),
                  br(),
                  textInput(inputId = 'invitado_in', label = 'Nombre de usuario invitado:',
                            placeholder = 'e.g., mamiferos-invitado'),
                  uiOutput('checkopts_invitado'),
                  numericInput(inputId = 'invitado_dias', label = 'Duración de permiso (en días):',
                               min = 1, max = 30, step = 1, value = 1),
                  h5(strong('Permisos específicos: ')), br(), 
                  uiOutput('permiso_familia'), uiOutput('permiso_especie'),
                  actionBttn(inputId = 'invitado_crds', 'Generar credenciales', icon = icon('user-check'),
                             block = T, color = 'success', style = 'jelly')
                    )
                )
          )
)

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
library(plotly)
library(scales)
library(dplyr)
# library(rgdal)
library(shiny)
library(shinybusy)
library(sf)

lapply(list.files('R/udf/', pattern = '.R$', full.names = T), source)

# SERVER -----------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  shinybusy::show_modal_spinner(text = 'Cargando la aplicación del grupo...', 
                                spin = 'fulfilling-bouncing-circle',
                                color = '#DD5568')
  
  # Globals --------------------------------------------------------------------------------------------
  local <- F
  options(DT.options = list(pageLength = 60))
  
  forest <- read_sf('eco/forestaciones.shp')
  st_crs(forest) <- 4326
  
  eco <- read_sf('eco/arg_ecorregiones_01_simpli.shp')
  eco <- eco %>% 
    filter(Ecorregion != c("Mar Argentino", 'Antartida'))
  st_crs(eco) <- 4326
  
  e <- raster::extent(c(xmin=-73.58999, xmax=-53.66536, ymin=-55.04876, ymax=-21.78415))
  
  rasPl <- raster::raster(
    nrows=3703, 
    ncols=2218, 
    resolution = c(0.008983153, 0.008983153), 
    crs='+proj=longlat +datum=WGS84 +no_defs',
    ext=e, vals=NA)
  
  crs(rasPl) <- '+proj=longlat +datum=WGS84 +no_defs'
  
  token <- readRDS('info/token.rds')
  # Groupals -------------------------------------------------------------------------------------------
  grp_app <- lastActive$id
  # grp_app <- 'mamiferos'
  if(local){
    dir.list <- list(
      spat = 'data/sp_obj.rds',
      habs = paste0('info/', grp_app, '/habitatsRef.csv'), 
      spps = paste0('info/',grp_app,'/editados/referenciaTable.csv'), 
      orig = list(cobs = paste0('info/',grp_app,'/coberturasTable.csv'),
                  cons = paste0('info/',grp_app,'/conservacionTable.csv'), 
                  elev = paste0('info/',grp_app,'/elevacionTable.csv')), 
      edit = list(cobs = paste0('info/',grp_app,'/editados/modified_habitatsTable.csv'), 
                  cons = paste0('info/',grp_app,'/editados/modified_conservationTable.csv'),
                  elev = paste0('info/',grp_app,'/editados/modified_elevationTable.csv'), 
                  fore = paste0('info/',grp_app,'/editados/modified_forestationsTable.csv'))
    )
  } else {
    dir.list <- list(
      spat = 'data/sp_obj.rds',
      habs = paste0('NatureMap/info/', grp_app, '/habitatsRef.csv'), 
      spps = paste0('NatureMap/info/',grp_app,'/editados/referenciaTable.csv'), 
      orig = list(cobs = paste0('/NatureMap/info/',grp_app,'/coberturasTable.csv'),
                  cons = paste0('NatureMap/info/',grp_app,'/conservacionTable.csv'), 
                  elev = paste0('NatureMap/info/',grp_app,'/elevacionTable.csv')), 
      edit = list(cobs = paste0('NatureMap/info/',grp_app,'/editados/modified_habitatsTable.csv'), 
                  cons = paste0('NatureMap/info/',grp_app,'/editados/modified_conservationTable.csv'),
                  elev = paste0('NatureMap/info/',grp_app,'/editados/modified_elevationTable.csv'), 
                  fore = paste0('NatureMap/info/',grp_app,'/editados/modified_forestationsTable.csv'))
    )
  }
  
  upload.dir <- paste0('NatureMap/info/', grp_app, '/editados/')
  
  # df <- aoh_table_read(dir.list$edit$cons, local = local, is.cons = T, dtoken=token)
  # class(as_tibble(df))
  #TODO forestaciones
  # if(!file.exists(dir.list$edit$fore)){
  #   write.csv(data.frame(Familia=NA, Especie=NA, Forestaciones=NA), 
  #             dir.list$edit$fore, row.names = F)
  # }
  
  if(grp_app == 'mamiferos'){
    eco_habs_elev <- read.csv('info/mamiferos/tabla_sarem_ecohabselev.csv')
  }
  
  totalhabs <- read.csv(paste0('info/',grp_app,'/coberturasTable.csv'))
  
  valsHabs <- read.csv(paste0('info/', grp_app, '/habitatsRef.csv'))
  
  filePathCons <- file.path(tempdir(), 'modified_conservationTable.csv')
  filePathHabs <- file.path(tempdir(),'modified_habitatsTable.csv')
  filePathElev <- file.path(tempdir(), 'modified_elevationTable.csv')
  filePathRefe <- file.path(tempdir(), 'referenciaTable.csv')
  filePathFore <- file.path(tempdir(), 'modified_forestationsTable.csv')
  
  lvlGlo <- c('', 'LC', 'NT', 'VU', 'EN', 'CR','EW', 'EX', 'DD', 'NoIUCN')
  
  if(grp_app == 'anfibios'){
    lvlPro <- c('', 'LC', 'NT', 'VU', 'EN', 'CR','EW', 'EX', 'DD', 'Ninguno')
    lvlNac <- c('', 'VU', 'EP', 'AM', 'NA', 'IC', 'NE')
  }
  
  icons.grp <- list(
    fam = 'database', 
    gen = 'dna', 
    spp = switch (grp_app,
                  anfibios = 'frog', reptiles = 'dragon', 
                  mamiferos = 'otter', aves = 'earlybirds', 
                  peces = 'fish', plantas = 'pagelines'
    )
  )
  
  # Demo -----------------------------------------------------------------------------------------------
  observeEvent(input$demo, once = F, {
    output$demoPlay <- renderUI({
      showModal(
        modalDialog(
          title = 'Videos demo del uso cada solapa de la AppOH',
          fluidRow(
            column(
              12,
              actionLink('demoCon', '1. Demo Conservación (~1 min)'), br(), 
              actionLink('demoHab', '2. Demo Hábitats (~4 min)'), br(), 
              actionLink('demoEle', '3. Demo Elevación (~2 min)'),  br(),
              actionLink('demoRes', '4. Demo Visualizador y restaurador (~2 min)'), br(),
              actionLink('demoAOH', '5. Demo AOH (~1 min)'), br()
            )
          ), 
          size = 's')
      )
    })
  })  
  
  observeEvent(input$demoCon, once = F, {
    output$demoPlay <- renderUI({
      showModal(
        modalDialog(
          title = 'Videos demo del uso de la solapa Conservación',
          tags$video(
            id="video2", type = "video/mp4",
            src = "demo.mp4",
            controls = "controls", width="100%", height="100%"
          ), 
          size = 'l')
      )
    })
  }) 
  
  # Load Data ------------------------------------------------------------------------------------------
  df <- aoh_table_read(dir.list$edit$cons, local = local, is.cons = T, dtoken=token)
  Coberturas <- aoh_table_read(dir.list$edit$cobs, local = local, is.cons = F, dtoken=token)
  elevacion <- aoh_table_read(dir.list$edit$elev, local = local, is.cons = F, dtoken=token)
  ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
  
  output$infoFam <- renderUI({
    nFaml <- as.character(length(unique(df$Familia)))
    valueBox(width = NULL, 'Familias', nFaml, color = 'olive', icon=icon(icons.grp$fam))
    
  })
  output$infoGen <- renderUI({
    generos <- sapply(strsplit(df$Especie, ' '), function(x) {x[1]}, simplify = T)
    nGen <- as.character(length(unique(generos)))
    valueBox(width = NULL, 'Géneros', nGen, color = 'green', icon=icon(icons.grp$gen))    
  })
  output$infoEsp <- renderUI({
    nEsp <- as.character(length(unique(df$Especie)))
    valueBox(width = NULL, 'Especies', nEsp, color = 'teal', icon=icon(icons.grp$spp))
  })
  
  if (lastActive$type == 'invitado'){
    df <- subset(df, Especie %in% lastActive$species)
    Coberturas <- subset(Coberturas, Especie %in% lastActive$species)
    elevacion <- subset(elevacion, Especie %in% lastActive$species)
    ref_sp <- subset(ref_sp,Especie %in% lastActive$species)
  }
  allFamily <- unique(df$Familia)
  
  df1 <- split(df, df$Familia)
  
  Coberturas <- Coberturas[Coberturas$Especie  %in% df$Especie,]
  df2 <- split(Coberturas, Coberturas$Especie)
  
  df3 <- split(elevacion, elevacion$Especie)
  
  advance.data <- data.frame(
    from = c('conservación', 'hábitat', 'elevación'), 
    n = NROW(ref_sp), 
    revisadas = c(apply(ref_sp[,c('rev_con', 'rev_hab', 'rev_ele')], 2, function(col){
      round(length((which(col == TRUE))) / NROW(ref_sp) * 100, 2)
    })),
    color = c("red", "green", "orange")
  )
  
  output$avanceEspecies <- renderMenu({
    msgs <- apply(advance.data, 1, function(row) {
      taskItem(value= as.numeric(row[['revisadas']]), text = row[["from"]], color = row[["color"]])
    })
    
    dropdownMenu(type = "tasks", badgeStatus = 'success', 
                 .list = msgs)
  })
  
  write.csv(df, filePathCons, row.names = F, na='')
  write.csv(Coberturas, filePathHabs, row.names = F)
  write.csv(elevacion, filePathElev, row.names = F)
  # write.csv(elevacion, filePathElev, row.names = F) 
  #TODO forestaciones
  
  observeEvent(input$tabs, {
    if(input$tabs == 'tabCon' & all(ref_sp$rev_con)){
      shinyalert(html = T, 
                 title = '¡Todas las especies revisadas para Conservación!', 
                 text = span(tagList(icon('beer'), 'Sale una birra')))
    } else if (input$tabs == 'tabHabs' & all(ref_sp$rev_hab)) {
      shinyalert(html = T, 
                 title = '¡Todas las especies revisadas para Hábitats!', 
                 text = span(tagList(icon('beer'),icon('beer'), 'Salen dos birras')))
    } else if (input$tabs == 'tabElev' & all(ref_sp$rev_ele)){
      shinyalert(html = T, 
                 title = '¡Todas las especies revisadas para Elevación!', 
                 text = span(tagList(icon('beer'),icon('beer'), icon('beer'),
                                     'Salen tres birras')))
    }
  })
  # Conservation ---------------------------------------------------------------------------------------
  # 1. Inputs ------------------------------------------------------------------------------------------
  output$familia <- renderUI({
    choicFam <- allFamily[allFamily %in% ref_sp$Familia[ref_sp$rev_con == FALSE]]
    selectInput(inputId = "dataset", label = "Familia:",
                choices = sort(unique(choicFam)),
                selected = sort(unique(choicFam))[1], width = 250)
  })
  
  # 2. Table -------------------------------------------------------------------------------------------
  dt1_results <- list()
  
  lapply(names(df1), function(i) {
    
    df1[[i]] <- df1[[i]] %>% 
      filter(Especie  %in% ref_sp$Especie[ref_sp$rev_con == FALSE]) %>% 
      mutate(Especie = as.factor(Especie)) %>% 
      mutate(Nacional = factor(Nacional, levels = sort(unique(Nacional)), exclude = NULL)) %>% 
      mutate(Global = factor(Global, levels = lvlGlo, exclude = NULL)) %>% 
      mutate(Endemismo = factor(Endemismo, levels = c(0,1,NA)))
    
    dt1_results[[i]] <- DTedit::dtedit(
      input, output,
      name = paste0("tableConFiltered", i),
      thedata = df1[[i]],
      view.cols = c('Especie', 'Nacional', 'Global', 'Endemismo'),
      edit.cols = c('Nacional', 'Global', 'Endemismo'),
      input.types = c(Nacional = 'selectInput', Global='selectInput', 
                      Endemismo = 'selectInput'), 
      title.edit = paste0('Editando datos para ', input$dataset),
      selectize = F,
      show.insert = F, 
      show.copy = F,
      callback.update =  cons.update.callback,
      callback.delete = cons.delete.callback,
      datatable.options = list(dom = 'ftiS', 
                               rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
                               columnDefs = list(list(className = 'dt-center', targets = c(1,2,3))), 
                               scrollY = 300, scrollCollapse = TRUE )
    )  
    
    observeEvent(dt1_results[[i]]$thedata, ignoreInit = TRUE, {
      df1[[i]] <<- dt1_results[[i]]$thedata
    })
  })
  
  observeEvent(input$dataset, priority = 0, {
    output$grupo_seleccionado <- renderUI({
      uiOutput(paste0("tableConFiltered", input$dataset))
    })
  })
  
  output$downloadConGrupo <- downloadHandler(
    filename = function() {
      paste0(input$dataset,"_corregido_listado.csv")
    },
    content = function(file) {
      tbldwnld <- df1[[input$dataset]]
      write.csv(tbldwnld, file, row.names = FALSE)
    })
  
  
  # 3. Upload ------------------------------------------------------------------------------------------
  observeEvent(input$uploadCon, {
    
    modif <- df1[[input$dataset]]
    
    orig <- aoh_table_read(dir.list$edit$cons, local = local, is.cons = T, dtoken=token)
    
    orig <- orig[!orig$Familia == input$dataset, ]
    orig <- rbind(orig, modif)
    
    aoh_table_write(orig, dir.list$edit$cons, 
                    local = local, is.cons = T, 
                    dummy.dir = filePathCons, 
                    path = upload.dir,
                    dtoken=token)
    
    if (any(ref_sp$f_borrado_cons, ref_sp$f_editado_cons, ref_sp$rev_con)) {
      
      reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
      
      ref_new <- ref_sp %>% 
        filter(f_borrado_cons == T | f_editado_cons == T | rev_con == T)
      cols <- c('f_borrado_cons', 'f_editado_cons', 'rev_con')
      reforig[reforig$ID  %in%  ref_new$ID, cols] <- ref_new[,cols]
      
      aoh_table_write(reforig, dir.list$spps, local = local, is.cons = F,
                      dummy.dir = filePathRefe,
                      path = upload.dir,
                      dtoken = token)
      shinyalert("¡Datos guardados!",  "La información ha sido guardada correctamente", 
                 type = "success") 
    }
  })
  
  # 4. Mark --------------------------------------------------------------------------------------------
  
  observeEvent(input$marcarCon, {
    
    ref_rev <- subset(ref_sp, Familia == input$dataset)
    ref_rev$rev_con <- TRUE
    
    reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
    
    ref_save <- rbind(ref_rev, reforig[reforig$Familia != input$dataset,])
    
    aoh_table_write(ref_save, dir.list$spps, local = local, is.cons = F,
                    dummy.dir = filePathRefe,
                    path = upload.dir,
                    dtoken = token)
    
    shinyjs::click('uploadCon')
    
    shinyalert("¡Finalizado!",  "La Familia ha sido marcada como revisada", type = "success") 
  })
  # Habitats -------------------------------------------------------------------------------------------
  # 1. Inputs ------------------------------------------------------------------------------------------
  output$habsFamilia <- renderUI({
    choicFam <- allFamily[allFamily  %in% ref_sp$Familia[ref_sp$rev_hab == FALSE]]
    selectInput(inputId = "habsFamilia1", label = "Familia:",
                choices = sort(unique(choicFam)),
                selected = sort(unique(choicFam))[1], width = '100%')
  })
  
  observeEvent(input$habsFamilia1, { 
    output$especietab3 <- renderUI({
      especies <- df[df$Especie  %in% ref_sp$Especie[ref_sp$rev_hab == F],]
      selectInput("User", "Especie:", 
                  choices = sort(especies$Especie[especies$Familia == input$habsFamilia1]), 
                  selected = sort(especies$Especie[especies$Familia == input$habsFamilia1])[1], 
                  width = '100%')
    })
    
  })
  
  
  # 2. User data ---------------------------------------------------------------------------------------
  map <- reactive({
    if(!is.null(input$shapes)){
      map <- sf::st_read(input$shapes$datapath)
      map
    }
  })
  
  
  # 3. External info -----------------------------------------------------------------------------------
  if (grp_app == 'mamiferos') {
    output$showInfo <- renderUI({
      tabla_sp <- eco_habs_elev[eco_habs_elev$especie == input$User,2:7]
      href <- paste0("https://cma.sarem.org.ar/es/especie-nativa/", 
                     tolower(gsub(" ", "-", input$User)))
      url <- a(input$User, href=href, target='_blank')
      msg <- p(strong('Ecorregiones confirmadas: '), 
               ifelse(tabla_sp$eco_con == '', 'Ninguna',tabla_sp$eco_con), 
               br(),
               strong('Ecorregiones dudosas: '), 
               ifelse(tabla_sp$eco_dud == '', 'Ninguna', tabla_sp$eco_dud), 
               br(), 
               strong('Hábitats confirmados: '), 
               ifelse(tabla_sp$hab_con == '', 'Ninguno', tabla_sp$hab_con),
               br(), 
               strong('Hábitats dudosos: '), 
               ifelse(tabla_sp$hab_dud == '', 'Ninguno', tabla_sp$hab_dud), 
               br(), 
               strong('Tolerancia hábitats antropizados: '), tabla_sp$tha, 
               br(),
               tagList(strong('Ficha SAREM de:'), url))
    })
  } else {
    output$showInfo <- renderUI({ 
      h4('No se ha registrado información para este grupo')
    })
  }
  
  # 4. Table -------------------------------------------------------------------------------------------
  dt_results <- list()
  
  lapply(names(df2), function(i) {
    
    valsR <- unique(totalhabs[totalhabs$Especie == i, "Cobertura"])
    
    df2[[i]] <- df2[[i]] %>% 
      filter(Especie  %in% ref_sp$Especie[ref_sp$rev_hab == FALSE]) %>% 
      mutate(Cobertura = factor(Cobertura, levels = unique(Cobertura, valsR))) %>% 
      mutate(Idoneidad = factor(Idoneidad, levels = c('No apto', 'Apto', 'Marginal')))
    
    
    dt_results[[i]] <- DTedit::dtedit(
      input, output,
      name = paste0("tableFiltered", i),
      thedata = df2[[i]],
      view.cols = c('Cobertura', 'Idoneidad'),
      edit.cols = c('Cobertura', 'Idoneidad'),
      edit.label.cols = c('Coberturas dentro del rango', 'Idoneidad'),
      input.types = c(Cobertura = 'selectInput', Idoneidad='selectInput'),
      title.edit = paste0("Editando datos de ", i),
      title.add = paste0("Agregando datos a ", i),
      title.delete = paste0("Eliminando datos de ", i),
      selectize = F,
      show.insert = T,
      show.copy = F,
      callback.update = habs.update.callback,
      callback.delete = habs.delete.callback,
      callback.insert = habs.insert.callback,
      datatable.options = list(rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
                               scrollY = 175, scrollCollapse = TRUE, 
                               dom = 'tS')
    )
    
    observeEvent(dt_results[[i]]$thedata, ignoreInit = TRUE, {
      df2[[i]] <<- dt_results[[i]]$thedata
    })
  }
  )
  
  observeEvent(input$User, priority = 0, {
    output$datos_especie <- renderUI({
      uiOutput(paste0("tableFiltered", input$User))
    })
  })
  
  output$downloadHabsGrupo <- downloadHandler(
    filename = function() {
      paste0(input$habsFamilia1,"_corregido_Coberturas.csv")
    },
    content = function(file) {
      tbldwnld <- do.call(rbind, df2)
      tbldwnld <- tbldwnld[tbldwnld$Familia == input$habsFamilia1,]
      tbldwnld <- tbldwnld[!is.na(tbldwnld$Cobertura),]
      tbldwnld <- tbldwnld[, c(5,1,2,3,4)]
      
      write.csv(tbldwnld, file, row.names = FALSE)
    })
  
  output$downloadHabsEspecie <- downloadHandler(
    filename = function() {
      paste0(input$User,"_corregido_Coberturas.csv")
    },
    content = function(file) {
      tbldwnld <- df2[[input$User]]
      tbldwnld <- tbldwnld[!is.na(tbldwnld$Cobertura),]
      tbldwnld <- tbldwnld[, c(5,1,2,3,4)]
      
      write.csv(tbldwnld, file, row.names = FALSE)
    })
  
  # 5. Upload ------------------------------------------------------------------------------------------
  observeEvent(input$uploadHabs, {
    habs <- do.call(rbind, df2)
    habs <- habs[ habs$Familia == input$habsFamilia1, ]
    
    orig <- aoh_table_read(dir.list$edit$cobs, local = local, dtoken=token)
    
    orig <- orig[!orig$Familia == input$habsFamilia1, ]
    
    orig <- rbind(orig, habs)
    
    aoh_table_write(orig, dir.list$edit$cobs, local = local, is.cons = F, 
                    dummy.dir = filePathHabs, 
                    path = upload.dir,
                    dtoken=token)
    
    if (any(ref_sp$f_borrado_habs, ref_sp$f_editado_habs, ref_sp$f_nuevo_habs, ref_sp$rev_hab)) {
      reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
      
      ref_new <- ref_sp %>% 
        filter(f_borrado_habs == T, f_editado_habs == T, f_nuevo_habs == T, rev_hab == T)
      
      cols <- c('f_borrado_habs', 'f_editado_habs', 'f_nuevo_habs', 'rev_hab')
      reforig[reforig$ID  %in%  ref_new$ID, cols ] <- ref_new[ , cols ]
      
      aoh_table_write(reforig, dir.list$spps, local = local, is.cons = F,
                      dummy.dir = filePathRefe,
                      path = upload.dir,
                      dtoken = token)
      
      shinyalert("¡Cambios guardados!",  "La información ha sido guardada correctamente", 
                 type = "success") 
    }
    
  })
  
  # 6. Mark --------------------------------------------------------------------------------------------
  observeEvent(input$marcarForest,{
    sp <- input$User
    data.sp <- subset(df, Especie == sp)[,c(1,2)]
    
    df.forest <- aoh_table_read(dir.list$edit$fore, local = local, dtoken=token)
    
    if (!is.na(df.forest$Especie) && sp %in% df.forest$Especie) {
      shinyalert('¡Advertencia!', 
                 paste('La especie ya tiene esta información y fue marcado como', 
                       df.forest$Forestaciones[df.forest$Especie == sp]),
                 type = 'warning')
    } else {
      shinyalert(html = TRUE, inputId = 'ok.ff',
                 text = tagList(
                   icon('tree'),
                   h3('Definir idoneidad de cobertura forestaciones para la especie'), 
                   selectInput(inputId = 'ff', label = 'Idoneidad', 
                               choices = c('No apto', 'Apto', 'Marginal'), 
                               selected = 'No apto'))
      )
      
      observeEvent(input$ok.ff, once = T, ignoreInit = T, ignoreNULL = T,{
        data.sp$Forestaciones <- input$ff
        
        aoh_table_write(table = rbind(na.omit(df.forest), data.sp), 
                        file = dir.list$edit$fore, local = local, is.cons = F, 
                        dummy.dir = filePathFore, path=upload.dir, dtoken=token)
      })
    }
  })
  
  observeEvent(input$marcarHabs, {
    
    ref_rev <- subset(ref_sp, Especie == input$User)
    df.forest <- aoh_table_read(dir.list$edit$fore, local = local, dtoken=token)
    
    if (!any(ref_rev$f_borrado_habs, ref_rev$f_editado_habs, ref_rev$f_nuevo_habs)) {
      shinyalert("¡Error!",  "No se han editado datos de la especie. No se puede marcar como revisada. 
                 Por favor editar datos, confirmar cambios y marcar como revisada luego de esto.", 
                 type = "error") 
    } else if (!input$User %in% df.forest$Especie) {
      shinyalert('¡Error!', 'La especies no ha sido revisada para forestaciones. 
                 Por favor primero editar en "Decidir forestaciones"',
                 type = 'error')
    } else {
      ref_rev <- subset(ref_sp, Especie == input$User)
      ref_rev$rev_hab <- TRUE
      
      reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
      
      ref_save <- rbind(ref_rev, reforig[reforig$Especie != input$User,])
      
      aoh_table_write(ref_save, dir.list$spps, local = local, is.cons = F,
                      dummy.dir = filePathRefe,
                      path = upload.dir,
                      dtoken = token)
      
      shinyjs::click('uploadHabs')
      
      shinyalert("¡Finalizado!",  "La Especie ha sido marcada como revisada", type = "success") 
    }
  })
  # 7. Map ---------------------------------------------------------------------------------------------
  mapBase <- leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels,
                     options = providerTileOptions(noWrap = TRUE)) %>% 
    addPolygons(data= eco, weight=1, fill = T, fillOpacity = 0.01, #color=T,
                color = wesanderson::wes_palette('Darjeeling1',n = NROW(eco), 
                                                 type = 'continuous'),
                group = "Ecorregion",popup = ~Ecorregion, 
                highlightOptions=highlightOptions(bringToFront=T, sendToBack=T, 
                                                  fillOpacity = 0.2)) %>% 
    flyTo(-70, -45, 5) 
  
  output$mapadatos <- renderLeaflet(mapBase)
  
  counters <- reactiveValues(dibujarR = 0, 
                             dibujarH = 0, 
                             dibujarF = 0)
  
  observeEvent(input$dibujarR, {
    
    sp <- input$User
    
    spOK <- readRDS('data/sp_obj.rds')$NombreCientifico
    
    if(spOK == sp){
      obj <- readRDS('data/sp_obj.rds')
    } else {
      shinybusy::show_modal_spinner(text = 'Descargando info de la especie...', 
                                    spin = 'fulfilling-bouncing-circle',
                                    color = '#DD5568')
      rdrop2::drop_download(
        paste0('NatureMap/datos_espaciales/', sp, '.rds'),
        local_path = 'data/sp_obj.rds',
        dtoken=token, 
        overwrite = T)
      
      obj <- readRDS('data/sp_obj.rds')
      remove_modal_spinner()
    }
    
    y <- obj$Rango
    st_crs(y) <- 4326
    
    boundbbx <-unname(sf::st_bbox(y))
    
    leafletProxy("mapadatos", session) %>%
      addPolygons(data= y, fill=TRUE, fillOpacity = 0) %>%
      flyToBounds(boundbbx[1], 
                  boundbbx[2], 
                  boundbbx[3], 
                  boundbbx[4])
  })
  
  observeEvent(input$dibujarH, {
    
    sp <- input$User
    
    spOK <- readRDS('data/sp_obj.rds')$NombreCientifico
    if(spOK == sp){
      obj <- readRDS('data/sp_obj.rds')
    } else {
      
      shinybusy::show_modal_spinner(text = 'Descargando info de la especie...', 
                                    spin = 'fulfilling-bouncing-circle',
                                    color = '#DD5568')
      
      rdrop2::drop_download(
        paste0('NatureMap/datos_espaciales/', sp, '.rds'),
        local_path = 'data/sp_obj.rds',
        dtoken=token, 
        overwrite = T)
      
      obj <- readRDS('data/sp_obj.rds')
      remove_modal_spinner()
    }
    
    newDT <- df2[[input$User]]
    
    if (!all(is.na(newDT$Idoneidad))) {
      obj$RxHxE$Cobs[obj$RxHxE$Cobs  %in% newDT$Codigo[newDT$Idoneidad == 'No apto'] ] <- NA
    }
    
    obj$RxHxE$Cobs[!obj$RxHxE$Cobs  %in% newDT$Codigo] <- NA
    y <- obj$Rango
    st_crs(y) <- 4326
    
    boundbbx <- unname(sf::st_bbox(y))
    
    rasSp <- rasPl
    rasSp[obj$RxHxE$cells] <-  obj$RxHxE$Cobs
    rasSp <- crop(rasSp, extent(y)+0.02)
    rasSp <- projectRasterForLeaflet(rasSp, 'ngb')
    
    rasSp <- ratify(rasSp)
    rat <- levels(rasSp)[[1]]
    rat$name <- replace.values(valsHabs$value, valsHabs$Habitat, rat$ID)
    levels(rasSp) <- rat
    
    colores <- replace.values(valsHabs$value, valsHabs$color, levels(rasSp)[[1]]$ID)
    
    pal <-colorFactor(palette = colores,alpha = TRUE,
                      domain = values(rasSp), 
                      na.color = NA)
    
    leafletProxy("mapadatos", session) %>% 
      clearImages() %>% 
      clearControls() %>% 
      addRasterImage(rasSp, colors = pal, project = F, opacity = 1) %>% 
      addLegend(pal = pal, values = values(rasSp), title = 'Hábitats', 
                labFormat = labelFormat( transform = function(x) {
                  levels(rasSp)[[1]]$name[which(levels(rasSp)[[1]]$ID == x)]
                }), opacity = 1) %>% 
      flyToBounds(boundbbx[1], 
                  boundbbx[2], 
                  boundbbx[3], 
                  boundbbx[4])
  })
  
  observeEvent(input$dibujarF, {
    
    counters$dibujarF <-  counters$dibujarF + 1
    
    layerID <- paste('forest', 1:NROW(forest))
    layerIDlg <- paste('forest', 1:NROW(forest), 'lg')
    
    if (counters$dibujarF %% 2 != 0) {
      
      shinybusy::show_modal_spinner(text = 'Por favor espere, cargando información...', 
                                    spin = 'fulfilling-bouncing-circle',
                                    color = '#DD5568')
      colFores <- colorFactor(palette = '#FF00C5', 
                              domain = 'Macizos forestales')
      leafletProxy("mapadatos", session) %>%
        addPolygons(data=forest, weight=0.5, fill=T, fillOpacity = 0.5, layerId = layerID,
                    color=c('#FF00C5'), popup = 'Plantaciones forestales') %>% 
        addLegend(pal=colFores, position = "bottomright", values = 'Macizos forestales', 
                  title = 'Forestaciones', opacity = 1, layerId = layerIDlg)
      remove_modal_spinner()
    } else {
      leafletProxy("mapadatos", session) %>%
        removeShape(layerID) %>% 
        removeControl(layerIDlg)
    }
  })
  
  observeEvent(input$dibujarP, {
    leafletProxy("mapadatos", session) %>%
      clearShapes() %>%
      clearImages() %>%
      clearControls() %>%
      clearMarkerClusters() %>%
      clearMarkers() %>% 
      addPolygons(data= eco,weight=1,  fill = T, fillOpacity = 0.01,
                  color = wesanderson::wes_palette('Darjeeling1',n = NROW(eco), 
                                                   type = 'continuous'),
                  group = "Ecorregion",popup = ~Ecorregion, 
                  highlightOptions=highlightOptions(bringToFront=T, sendToBack=T, 
                                                    fillOpacity = 0.4))
    
  })
  
  observeEvent(input$kml, {
    if (!is.null(map())) {
      mapXY <- st_zm(map(), drop = TRUE, what = "ZM")
      leafletProxy("mapadatos", session) %>%
        addMarkers(data = as.data.frame(st_coordinates(mapXY)), 
                   lng = ~X, lat = ~Y, 
                   popup = paste('Fuente:',input$shapes$name), 
                   clusterOptions = markerClusterOptions())
    } else {
      shinyalert('¡Advertencia!', 'No se ha subido ningún archivo kml', type = 'warning')
    }
  })
  
  # Elevation ------------------------------------------------------------------------------------------
  # 1. Inputs ------------------------------------------------------------------------------------------
  output$habsFamiliaE <- renderUI({
    choicFam <- allFamily[allFamily  %in% ref_sp$Familia[ref_sp$rev_ele == FALSE]]
    selectInput(inputId = "habsFamilia1E", label = "Familia:",
                choices = sort(unique(choicFam)),
                selected = sort(unique(choicFam))[1], width = '100%')
  })
  
  observeEvent(input$habsFamilia1E, {
    output$especietab4 <- renderUI({
      especies <- df[df$Especie  %in% ref_sp$Especie[ref_sp$rev_ele == F],]
      selectInput("User1", "Especie:", 
                  choices = sort(especies$Especie[especies$Familia == input$habsFamilia1E]), 
                  selected = sort(especies$Especie[especies$Familia == input$habsFamilia1E])[1], 
                  width = '100%')
    })
  })
  
  
  # 2. User data ---------------------------------------------------------------------------------------
  mapElev <- reactive({
    if(!is.null(input$shapesElev)){
      map1 <- sf::st_read(input$shapesElev$datapath)
      map1
    }
  })
  
  # 3. External info -----------------------------------------------------------------------------------
  observeEvent(input$User1, {
    if (grp_app == 'mamiferos') {
      output$elevDisp <- renderUI({
        tablaElev <- df3[[input$User1]] 
        tablaElev <- tablaElev[ ,2:3]
        tablaElev$Inf_SAREM <- eco_habs_elev[eco_habs_elev$especie == input$User1, 8]
        tablaElev$Sup_SAREM <- eco_habs_elev[eco_habs_elev$especie == input$User1, 9]
        href <- paste0("https://cma.sarem.org.ar/es/especie-nativa/", 
                       tolower(gsub(" ", "-", input$User1)))
        url <- a(input$User1, href=href, target='_blank')
        msg <- p(strong('El rango de elevación disponible es: '), 
                 tablaElev[1,1], '-', tablaElev[1,2], ' msnm.', 
                 br(),
                 strong('El rango reportado en SAREM para la especie es: '),
                 tablaElev[1,3], '-', tablaElev[1,4], ' msnm.', 
                 br(), 'Si desea confirmar en la ficha SAREM siga este ',
                 tagList('link', url))
      })
    } else {
      output$elevDisp <- renderUI({
        h4('No se ha registrado información para este grupo')
      })
    }
  })
  
  # 4. Table -------------------------------------------------------------------------------------------
  dt3_results <- list()
  
  lapply(names(df3), function(i) {
    
    df3[[i]] <- df3[[i]] %>% 
      filter(Especie  %in% ref_sp$Especie[ref_sp$rev_ele == FALSE])
    
    dt3_results[[i]] <- DTedit::dtedit(
      input, output,
      name = paste0("elevFiltered", i),
      thedata = df3[[i]],
      view.cols = c('Especie', 'InferiorApto', 'SuperiorApto'),
      edit.cols = c('InferiorApto', 'SuperiorApto'),
      edit.label.cols = c('Nuevo límite inferior', 'Nuevo límite superior'),
      input.types = c(InferiorApto = 'numericInput', SuperiorApto='numericInput'),
      title.edit = paste0("Editando datos para ", i),
      show.insert = F,
      show.delete = F,
      show.copy = F,
      callback.update = elev.update.callback,
      datatable.options = list(rowCallback = JS("function(r,d) {$(r).attr('height', '30px')}"), 
                               dom='t'))
    
    observeEvent(dt3_results[[i]]$thedata, ignoreInit = TRUE, {
      df3[[i]] <<- dt3_results[[i]]$thedata
    })
  }
  )
  
  observeEvent(input$User1, priority = 0, {
    output$datos_especie_elev <- renderUI({
      uiOutput(paste0("elevFiltered", input$User1))
    })
  })
  
  output$downloadElevGrupo <- downloadHandler(
    filename = function() {
      paste0(input$habsFamilia1E,"_corregido_elev" , ".csv")
    },
    content = function(file) {
      tbldwnld <- do.call(rbind, df3)
      tbldwnld <- tbldwnld[tbldwnld$Familia == input$habsFamilia1E,]
      write.csv(tbldwnld, file, row.names = FALSE)
    })
  
  # output$downloadElevEspecie <- downloadHandler(
  #   filename = function() {
  #     paste0(input$User1,"_corregido_elev" , ".csv")
  #   },
  #   content = function(file) {
  #     tbldwnld <- df3[[input$User1]]
  #     write.csv(tbldwnld, file, row.names = FALSE)
  #   })
  
  
  # 5. Upload ------------------------------------------------------------------------------------------
  observeEvent(input$uploadElev, {
    elev <- do.call(rbind, df3)
    elev <- elev[ elev$Familia == input$habsFamilia1E, ]
    
    orig <- aoh_table_read(dir.list$edit$elev, local = local, dtoken=token)
    
    orig <- orig[!orig$Familia == input$habsFamilia1E, ]
    orig <- rbind(orig, elev)
    
    aoh_table_write(orig, dir.list$edit$elev, local = local, is.cons = F, 
                    dummy.dir = filePathElev, 
                    path = upload.dir,
                    dtoken=token)
    
    if (any(ref_sp$f_editado_elev, ref_sp$rev_ele)) {
      reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
      
      ref_new <- ref_sp %>% 
        filter(f_editado_elev == T, rev_ele ==T)
      cols <- c('f_editado_elev', 'rev_ele')
      reforig[reforig$ID  %in%  ref_new$ID, cols ] <- ref_new[, cols ]
      
      aoh_table_write(reforig, dir.list$spps, local = local, is.cons = F,
                      dummy.dir = filePathRefe,
                      path = upload.dir,
                      dtoken = token)
    }
    
    shinyalert("¡Cambios guardados!",  "La información ha sido guardada correctamente", 
               type = "success") 
    
  })
  
  # 6. Mark --------------------------------------------------------------------------------------------
  observeEvent(input$marcarElev, {
    ref_rev <- subset(ref_sp, Especie == input$User1)
    
    if (!ref_rev$f_editado_elev) {
      shinyalert("¡Error!",  "No se han editado datos de la especie. No se puede marcar como revisada. 
                 Por favor editar datos, confirmar cambios y marcar como revisada luego de esto.", 
                 type = "error") 
    } else {
      ref_rev <- subset(ref_sp, Especie == input$User1)
      ref_rev$rev_ele <- TRUE
      
      reforig <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
      
      ref_save <- rbind(ref_rev, reforig[reforig$Especie != input$User1,])
      
      aoh_table_write(ref_save, dir.list$spps, local = local, is.cons = F,
                      dummy.dir = filePathRefe,
                      path = upload.dir,
                      dtoken = token)
      
      shinyjs::click('uploadElev')
      
      shinyalert("¡Finalizado!",  "La Especie ha sido marcada como revisada", type = "success") 
    }
  })
  
  # 7. Map ---------------------------------------------------------------------------------------------
  mapBaseE <- leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)) %>% 
    flyTo(-70, -45, 5)
  
  output$mapadatosE <- renderLeaflet(mapBaseE)
  
  observeEvent(input$dibujarE, {
    # input <- list(); input$User1 = 'Abrocoma budini'
    
    spOK <- readRDS('data/sp_obj.rds')$NombreCientifico
    
    if(spOK == input$User1){
      obj <- readRDS('data/sp_obj.rds')
    } else {
      shinybusy::show_modal_spinner(text = 'Descargando info de la especie...', 
                                    spin = 'fulfilling-bouncing-circle',
                                    color = '#DD5568')
      rdrop2::drop_download(
        paste0('NatureMap/datos_espaciales/', input$User1, '.rds'),
        local_path = 'data/sp_obj.rds',
        dtoken=token, 
        overwrite = T)
      
      obj <- readRDS('data/sp_obj.rds')
      shinybusy::remove_modal_spinner()
    }
    
    inf <- ifelse(is.na(df3[[input$User1]]$InferiorApto), df3[[input$User1]]$limiteInferior,
                  df3[[input$User1]]$InferiorApto)
    sup <- ifelse(is.na(df3[[input$User1]]$SuperiorApto), df3[[input$User1]]$limiteSuperior, 
                  df3[[input$User1]]$SuperiorApto)
    
    obj$RxHxE$Elev[obj$RxHxE$Elev < inf] <- NA
    obj$RxHxE$Elev[obj$RxHxE$Elev > sup] <- NA
    
    y <- obj$Rango
    st_crs(y) <- 4326
    
    boundbbx <- unname(sf::st_bbox(y))
    
    rasSp <- rasPl
    rasSp[obj$RxHxE$cells] <-  obj$RxHxE$Elev
    rasSp <- crop(rasSp, extent(y)+0.02)
    rasSp <- projectRasterForLeaflet(rasSp, 'bilinear')
    
    pal <-colorNumeric(palette = hcl.colors(15,'Heat', rev=T), 
                       domain =  obj$RxHxE$Elev, 
                       na.color = NA)
    
    leafletProxy("mapadatosE", session) %>% 
      clearShapes() %>% 
      clearImages() %>% 
      clearControls() %>% 
      addPolygons(data = y, fill=F) %>% 
      addRasterImage(rasSp, colors = pal, project = F, opacity = 1) %>% 
      addLegend(pal=pal, values = obj$RxHxE$Elev, title = 'Elevación (m)', opacity = 1) %>%  
      flyToBounds(boundbbx[1], 
                  boundbbx[2], 
                  boundbbx[3], 
                  boundbbx[4])
  })
  
  observeEvent(input$kmlElev, {
    if(!is.null(mapElev())){
      mapXY <- st_zm(mapElev(), drop = TRUE, what = "ZM")
      leafletProxy("mapadatosE", session) %>%
        addMarkers(data = as.data.frame(st_coordinates(mapXY)), 
                   lng = ~X, lat = ~Y, 
                   popup = paste('Fuente:',input$shapes$name), 
                   clusterOptions = markerClusterOptions())
    } else {
      shinyalert('¡Advertencia!', 'No se ha subido ningún archivo kml', type = 'warning')
    }
  })
  
  # Visual & restaurador -------------------------------------------------------------------------------
  # 1. Inputs ------------------------------------------------------------------------------------------
  output$familiaRev <- renderUI({
    # input <- list(); input$categoriaRev <- 'Elevacion'
    cat <- switch (input$categoriaRev,
                   Todas = 'Todas', 
                   Conservacion = 'rev_con',
                   Habitats = 'rev_hab', 
                   Elevacion = 'rev_ele'
    )
    if(cat == 'Todas'){
      familias <- ref_sp %>% 
        filter(rev_con == T | rev_hab == T | rev_ele == T)
    } else {
      familias <- ref_sp[ which(ref_sp[[cat]]), ] 
    }
    
    selectInput('famRev', label = 'Familia:', 
                choices = sort(unique(familias$Familia)), 
                selected = sort(unique(familias$Familia))[1])
  })  
  
  output$especieRev <- renderUI({
    cats <- switch (input$categoriaRev,
                    Todas = 'Todas', 
                    Conservacion = 'rev_con',
                    Habitats = 'rev_hab', 
                    Elevacion = 'rev_ele'
    )
    if(cats == 'Todas'){
      fam <- ref_sp %>% 
        filter(rev_con == T | rev_hab == T | rev_ele == T)
    } else {
      fam <- ref_sp[ which(ref_sp[[cats]]), ] 
    }
    
    Especies <- fam$Especie[fam$Familia == input$famRev]
    
    selectInput('spRev', label = 'Especie:', 
                choices = sort(unique(Especies)), 
                selected = sort(unique(Especies))[1])
  })  
  
  # 2. Unmark ------------------------------------------------------------------------------------------
  observeEvent(input$restaurar, {
    cats <- switch (input$categoriaRev,
                    Todas = 'Todas', 
                    Conservacion = 'rev_con',
                    Habitats = 'rev_hab', 
                    Elevacion = 'rev_ele'
    )
    #TODO forestaciones
    ref <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
    
    if(cats == 'Todas'){
      ref[ref$Especie == input$spRev, c('rev_con', 'rev_hab', 'rev_ele')] <- FALSE
    } else {
      ref[ref$Especie == input$spRev, cats] <- FALSE
    }
    
    ref_sp <<- ref
    
    aoh_table_write(ref_save, dir.list$spps, local = local, is.cons = F,
                    dummy.dir = filePathRefe,
                    path = upload.dir,
                    dtoken = token)
    
    shinyalert('¡Especie desmarcada!', 
               paste('La especie aparecerá de nuevo en la categoría seleccionada o', 
                     'en todas las pestañas si la opción "Todas" fue seleccionada', 
                     'y aparecerá con la información editada.', 
                     'Para ver los cambios deberá reiniciar la app', collapse = ' '), 
               type= 'success')
  })
  
  # 3. Outputs -----------------------------------------------------------------------------------------
  output$conscons <- renderUI({ tagList(DTOutput('consRev')) })
  
  observeEvent(input$famRev, ignoreNULL = T, {
    
    dt.render <- aoh_table_read(dir.list$edit$cons, local = local, is.cons = T, dtoken=token)
    ref <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
    
    cats <- switch (input$categoriaRev,
                    Todas = 'Todas', 
                    Conservacion = 'rev_con',
                    Habitats = 'rev_hab', 
                    Elevacion = 'rev_ele')
    
    dt.render <- subset(dt.render, Familia == input$famRev)
    
    if (cats=='Todas') {
      cols <- c('rev_con', 'rev_hab', 'rev_ele')
      dt.render <- dt.render[ dt.render$Especie  %in% ref$Especie[ ref[,] == T ] , cols ]
    } else {
      dt.render <- dt.render[ dt.render$Especie  %in% ref$Especie[ ref[[cats]] == T ] , ]
    }
    
    output$consRev <- renderDT(dt.render[,1:5], rownames=FALSE,
                               options = list(scrollY = 450, 
                                              scrollCollapse = TRUE, 
                                              dom = 'tiS'))
  })
  
  observeEvent(input$spRev, ignoreNULL = T, {
    
    ref <- aoh_table_read(dir.list$spps, local = local, dtoken=token)
    
    dt.render.habs <- aoh_table_read(dir.list$edit$cobs, local = local, is.cons = F, dtoken=token)
    dt.render.elev <- aoh_table_read(dir.list$edit$elev, local = local, is.cons = F, dtoken=token)
    dt.render.fore <- aoh_table_read(dir.list$edit$fore, local = local, is.cons = F, dtoken=token)
    
    dt.render.habs <- dt.render.habs %>% 
      filter(Especie  %in% ref$Especie[ref$rev_hab == T])
    
    output$habsRev <- renderDT(subset(dt.render.habs, Especie == input$spRev)[,c(5,1,2,4)],
                               rownames=FALSE,
                               options = list(scrollY = 450, 
                                              scrollCollapse = TRUE, 
                                              dom = 'tiS'))
    
    dt.render.elev <- dt.render.elev %>% 
      filter(Especie  %in% ref$Especie[ref$rev_ele == T])
    
    output$elevRev <- renderDT(subset(dt.render.elev, Especie == input$spRev)[,c(6,1:5)],
                               rownames=FALSE,
                               options = list(dom = 'tiS'))
    
    if (!input$spRev  %in% dt.render.fore$Especie) {
      dt.render.fore <- cbind(Especie=input$spRev,
                              Idoneidad='No editado')
    } else {
      dt.render.fore <- subset(dt.render.fore, Especie == input$spRev)
    }
    
    output$forestRev <- renderDT(dt.render.fore, rownames=FALSE,
                                 options = list(dom = 'tiS') )
    
  })
  
  # Process AOH ----------------------------------------------------------------------------------------
  # 1. Inputs ------------------------------------------------------------------------------------------
  output$familiaAOH <- renderUI({
    familiasAOH <- ref_sp %>% 
      filter(rev_con == T & rev_hab == T & rev_ele == T) %>% 
      select(Familia) %>% arrange() %>% as.vector()
    
    selectInput('UserAOH', 'Familia:', choices = familiasAOH$Familia, 
                selected = familiasAOH$Familia[1], width = '100%')
  })
  
  output$especieAOH <- renderUI({
    dtFULL <- ref_sp %>% 
      filter(rev_con == T & rev_hab == T & rev_ele == T)
    
    EspeciesAOH <- sort(dtFULL$Especie[dtFULL$Familia  == input$UserAOH])
    
    selectInput('UserAOHsp', 'Especie:', choices = EspeciesAOH, 
                selected = EspeciesAOH[1], width = '100%')
  })
  
  # 2. Outputs -----------------------------------------------------------------------------------------
  notify <- function(msg, id = NULL) {
    showNotification(msg, id = id, duration = NULL, closeButton = FALSE, 
                     type = 'message')
  }
  
  observeEvent(input$makeAOH, {
    
    id <- notify("Leyendo información...(0%)")
    on.exit(removeNotification(id), add = TRUE)
    
    sp <- input$UserAOHsp
    
    spOK <- readRDS('data/sp_obj.rds')$NombreCientifico
    if(spOK == sp){
      obj <- readRDS('data/sp_obj.rds')
    } else {
      rdrop2::drop_download(
        paste0('NatureMap/datos_espaciales/', sp, '.rds'),
        local_path = 'data/sp_obj.rds',
        dtoken=token, 
        overwrite = T)
      
      obj <- readRDS('data/sp_obj.rds')
    }
    
    range <- obj$Rango
    st_crs(range) <- 4326
    
    if(sum(as.numeric(st_area(st_transform(range, 5346)))) * 1e-06 > 10e06) {
      notify("El área de la especie es mayor a 10.000.000 km²...", id = id)
      Sys.sleep(2)
      notify("El el proceso puede demorar bastante...", id = id)
      Sys.sleep(2)
    }
    
    habitat <- obj$RxHxE$Cobs
    cells <- obj$RxHxE$cells
    
    notify("Traduciendo boludeces...(5%)", id = id)
    
    newDT <- aoh_table_read(dir.list$edit$cobs, local = local, is.cons = F, dtoken=token)
    
    newDT <- subset(newDT, Especie == sp)
    
    if(!all(is.na(newDT$Idoneidad))){
      habitat[habitat  %in% newDT$Codigo[newDT$Idoneidad == 'No apto'] ] <- NA
    }
    
    habitat[!habitat  %in% newDT$Codigo] <- NA
    
    notify("Calculando el cubo inverso del infinito...(10%)", id = id)
    
    rasSp <- rasPl
    rasSp[cells] <-  habitat
    
    rasSp <- mask(crop(rasSp, range), range)
    habitat <- rasSp
    habitat[!is.na(habitat[])] <- 1
    habitat[is.na(habitat[])] <- 0
    
    notify("Robando datos personales y de banco...(20%)", id = id)
    
    output$plotRH <- renderPlot({
      range %>% 
        ggplot()+
        geom_sf(data = range, fill='gray', color='dark gray', lwd=0.6)+
        coord_sf()+
        geom_tile(data = as.data.frame(habitat, xy=T), 
                  aes(x=x, y=y, fill=as.factor(layer)), alpha=0.8)+
        scale_fill_manual(values = c(NA, 'forest green'))+
        theme_void() +
        labs(title= 'Hábitats aptos para la especie', 
             caption = 'Basado en los datos editados por los expertos. 
           Este mapa no toma aún en cuenta las forestaciones.') +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5))
    }, height = 550)
    
    notify("Arreglando la economía...(40%)", id = id)
    
    newDT <- aoh_table_read(dir.list$edit$elev, local = local, is.cons = F, dtoken=token)
    
    newDT <- subset(newDT, Especie == sp)
    
    inf <- ifelse(is.na(newDT$InferiorApto), newDT$limiteInferior,
                  newDT$InferiorApto)
    sup <- ifelse(is.na(newDT$SuperiorApto), newDT$limiteSuperior, 
                  newDT$SuperiorApto)
    
    obj$RxHxE$Elev[obj$RxHxE$Elev < inf] <- NA
    obj$RxHxE$Elev[obj$RxHxE$Elev > sup] <- NA
    
    notify("Falló el sistema. Al menos se intentó...(50%)", id = id)
    
    rasSp <- rasPl
    
    rasSp[obj$RxHxE$cells] <-  obj$RxHxE$Elev
    rasSp <- mask(crop(rasSp, range), range)
    
    elevacion <- rasSp
    elevacion[!is.na(elevacion[])] <- 1 
    elevacion[is.na(elevacion[])] <- 0 
    
    notify("Encontrando problemas donde no hay...(60%)", id = id)
    
    output$plotRE <- renderPlot({
      range %>% 
        ggplot()+
        geom_sf(data = range, fill='gray', color='dark gray', lwd=0.6)+
        coord_sf()+
        geom_tile(data = as.data.frame(elevacion, xy=T), 
                  aes(x=x, y=y, fill=as.factor(layer)), alpha=0.8)+
        scale_fill_manual(values = c(NA, 'darkorange3'))+
        theme_void() +
        labs(title= 'Elevación apta para la especie', 
             caption = 'Basado en los datos editados por los expertos.') +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5))
    }, height = 550)
    
    notify("Invalidando la información guardada...(80%)", id = id)
    
    aoh.sp <- habitat
    aoh.sp[elevacion[] == 0] <- 0 
    
    output$plotAOH <- renderPlot({
      range %>% 
        ggplot()+
        geom_sf(data = range, fill='gray', color='dark gray', lwd=0.6)+
        coord_sf()+
        geom_tile(data = as.data.frame(aoh.sp, xy=T), 
                  aes(x=x, y=y, fill=as.factor(layer)), alpha=0.8)+
        scale_fill_manual(values = c(NA, 'dodgerblue3'))+
        theme_void() +
        labs(title= 'Área de Hábitat', 
             caption = 'Basado en el cruce de datos de elevación y hábitat.
             Aún no incluye datos de forestaciones. 
             La resolución actual es 1km, pero el producto final será a 100m.') +
        theme(legend.position = 'none', 
              plot.title = element_text(hjust = 0.5))
    }, height = 550)
    
    notify("Imposible de invalidar. Excelente info...(100%)", id = id)
  })
  
  # Summary --------------------------------------------------------------------------------------------
  output$resumenCon <- renderPlotly({
    ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
    
    ref_sp %>% 
      select(Familia, rev_con) %>% 
      group_by(Familia) %>% 
      mutate(totalFam = length(Familia)) %>%  ungroup() %>% 
      mutate(rev_con = factor(ifelse(rev_con, 'Revisado', 'No revisado'),
                              levels = c('No revisado', 'Revisado'))) %>% 
      group_by(Familia, rev_con, totalFam) %>%
      count(Familia) %>% 
      mutate(tooltip = paste(Familia, 'tiene', 
                             paste0(round(n/totalFam, 2)*100, '%'), 
                             tolower(rev_con))) %>% 
      ggplot(aes(x=Familia, y=n,fill= Familia, alpha=rev_con, text=tooltip)) +
      geom_bar(aes(fill= Familia), stat = "identity", position = "fill") +
      scale_fill_discrete(guide = 'none')+
      scale_y_continuous(labels = scales::percent_format(), guide = 'none')+
      scale_alpha_discrete(name='Estado (por transparencia)',
                           labels= if(all(ref_sp$rev_con)) {
                             'Revisado'
                           } else if(all(ref_sp$rev_con == FALSE)){
                             'No revisado'
                           } else {
                             c('No revisado', 'Revisado')
                           },
                           range = c(ifelse(all(ref_sp$rev_con == FALSE), 0.4, 
                                            ifelse(all(ref_sp$rev_con), 1, 0.4)),
                                     ifelse(all(ref_sp$rev_con == FALSE), 0.4, 1)), 
                           guide = 'none') +
      theme_light()+
      theme(
        axis.text.x= element_text(hjust = 1, angle = 60),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = 'none' ) -> fig
    ggplotly(fig, tooltip='tooltip')
  })
  
  output$resumenHabs <- renderPlotly({
    ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
    
    ref_sp %>% 
      select(Familia, rev_hab) %>% 
      group_by(Familia) %>% 
      mutate(totalFam = length(Familia)) %>%  ungroup() %>% 
      mutate(rev_hab = factor(ifelse(rev_hab, 'Revisado', 'No revisado'),
                              levels = c('No revisado', 'Revisado'))) %>% 
      group_by(Familia, rev_hab, totalFam) %>%
      count(Familia) %>% 
      mutate(tooltip = paste(Familia, 'tiene', 
                             paste0(round(n/totalFam, 2)*100, '%'), 
                             tolower(rev_hab))) %>% 
      ggplot(aes(x=Familia, y=n,fill= Familia, alpha=rev_hab, text=tooltip)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_discrete(guide = 'none') +
      scale_y_continuous(labels = scales::percent_format(), guide = 'none')+
      scale_alpha_discrete(name='Estado (por transparencia)',
                           
                           labels= if(all(ref_sp$rev_hab)) {
                             'Revisado'
                           } else if(all(ref_sp$rev_hab == FALSE)){
                             'No revisado'
                           } else {
                             c('No revisado', 'Revisado')
                           },
                           range = c(ifelse(all(ref_sp$rev_hab == FALSE), 0.4, 
                                            ifelse(all(ref_sp$rev_hab), 1, 0.4)),
                                     ifelse(all(ref_sp$rev_hab == FALSE), 0.4, 1)), 
                           guide='none') +
      theme_light()+
      theme(
        axis.text.x= element_text(hjust = 1, angle = 60),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = 'none') -> fig1
    ggplotly(fig1, tooltip='tooltip')
  })
  
  output$resumenElev <- renderPlotly({
    ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
    
    ref_sp %>% 
      select(Familia, rev_ele) %>% 
      group_by(Familia) %>% 
      mutate(totalFam = length(Familia)) %>%  ungroup() %>% 
      mutate(rev_ele = factor(ifelse(rev_ele, 'Revisado', 'No revisado'),
                              levels = c('No revisado', 'Revisado'))) %>% 
      group_by(Familia, rev_ele, totalFam) %>%
      count(Familia) %>% 
      mutate(tooltip = paste(Familia, 'tiene', 
                             paste0(round(n/totalFam, 2)*100, '%'), 
                             tolower(rev_ele))) %>% 
      ggplot(aes(x=Familia, y=n,fill= Familia, alpha=rev_ele, text=tooltip)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_discrete(guide = 'none') +
      scale_y_continuous(labels = scales::percent_format(), guide = 'none')+
      scale_alpha_discrete(name='Estado (por transparencia)',
                           
                           labels= if(all(ref_sp$rev_ele)) {
                             'Revisado'
                           } else if(all(ref_sp$rev_ele == FALSE)){
                             'No revisado'
                           } else {
                             c('No revisado', 'Revisado')
                           },
                           range = c(ifelse(all(ref_sp$rev_ele == FALSE), 0.4, 
                                            ifelse(all(ref_sp$rev_ele), 1, 0.4)),
                                     ifelse(all(ref_sp$rev_ele == FALSE), 0.4, 1)),
                           guide='none') +
      theme_light()+
      theme(
        axis.text.x= element_text(hjust = 1, angle = 60),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = 'none') -> fig2
    ggplotly(fig2, tooltip='tooltip')
  })
  
  output$resumenDelete <- renderUI({
    tagList(
      fluidRow(column(5, DT::dataTableOutput("ConservacionResu")),
               column(2, br()),
               column(5, DT::dataTableOutput("HabitatsResu"))
      )
    )
  })
  
  output$ConservacionResu <- DT::renderDataTable({
    
    ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
    
    ref_sp_resu <- ref_sp$Especie[ref_sp$f_borrado_cons  == TRUE]
    
    DT::datatable(cbind('Conservación' = ref_sp_resu), 
                  fillContainer = T,
                  rownames = FALSE, options = list(paging = FALSE, 
                                                   scrollY = 200, scrollCollapse = TRUE, 
                                                   dom = 'ti'))
  })
  
  output$HabitatsResu <- DT::renderDataTable({
    ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
    
    ref_sp_resu <- ref_sp$Especie[ref_sp$f_borrado_habs == TRUE]
    
    DT::datatable(cbind('Hábitats_de' = ref_sp_resu),
                  rownames = FALSE, 
                  fillContainer = T,
                  options = list(paging = FALSE, 
                                 scrollY = 180, scrollCollapse = TRUE, 
                                 dom = 'ti'))
  })
  
  # Make invitado ---------------------------------------------------------------------------------------
  user_type <- lastActive$type
  if(user_type  %in% c('admin', 'experto')){
    output$tabInvitados <- renderUI({
      sdm <- menuItem(text = 'Agregar invitado', icon=icon('network-wired'))
      sdm$children[[1]]$attribs$href <- "#shiny-tab-tabInvita2"
      sdm$children[[1]]$attribs$`data-toggle` <- "tab"
      sdm$children[[1]]$attribs$`data-value` <- "tabInvita2"
      sdm
    })
    
    if(user_type == 'admin'){
      output$checkopts_invitado <- renderUI({
        
        checkboxGroupInput(inputId = 'invitado_grupos', 'Grupos que podrá editar:',
                           choices = c('anfibios', 'reptiles', 'mamiferos',
                                       'aves', 'peces', 'plantas'), inline=T)
      })
    } else {
      output$checkopts_invitado <- renderUI({
        
        checkboxGroupInput(inputId = 'invitado_grupos', 'Permiso a editar grupos:',
                           choices = lastActive$id, selected = lastActive$id)
      })
    }
  }
  
  output$permiso_familia <- renderUI({
    selectInput('perm_fam_given', 'Familia(s): ', choices = sort(allFamily), multiple=T)
  })
  
  sps <- reactive({
    if(!is.null(input$perm_fam_given)) {
      sps <- sort(unique(df$Especie[df$Familia  %in% input$perm_fam_given]))
    } else {
      sps <- sort(unique(df$Especie))
    }
    sps
  })
  
  output$permiso_especie <- renderUI({
      selectInput('perm_sp_given', 'Especie(s): ', choices = sps(), multiple=T)
  })
  
  observeEvent(input$invitado_crds, {
    newUser <- input$invitado_in
    groups <- input$invitado_grupos
    type <- 'invitado'
    daysPermit <- input$invitado_dias
    oldUsers <- crds$user
    
    if (daysPermit > 30 | daysPermit < 1) {
      shinyalert('¡Error!', 
                 'Los días de permiso deben estar entre un intervalo de 1 día mínimo y 30 días máximo.',
                 type='error')
    } else if (any(oldUsers == newUser)) {
      shinyalert('¡Error!', 
                 'El usuario especificado ya existe. Por favor elija otro nombre.',
                 type='error')
    } else if (is.null(input$invitado_grupos)) {
      shinyalert('¡Error!', 
                 'Debe especificar algún grupo a dar permiso para revisar', 
                 type='error')
    } else if (is.null(input$invitado_in)) {
      shinyalert('¡Error!', 
                 'Debe especificar un nombre de usuario', 
                 type='error')
    } else if (is.null(input$perm_fam_given)){
      shinyalert('¡Error!', 
                 'Debe especificar al menos una familia para dar acceso', 
                 type='error')
    } else {
      date.limit <- Sys.Date() +  daysPermit
      rnd_smp <- c(1:9,letters,LETTERS,"!", "§", "$", "%", "&", "*")
      newPW <- paste(sample(rnd_smp,8),collapse="")
      if (is.null(input$perm_sp_given)){
      perm_sp <-  df %>% 
        filter(Familia %in% input$perm_fam_given) %>% select(Especie) %>% as.vector()
      perm_sp <- paste(perm_sp, collapse = ',')
      } else {
        perm_sp <- paste(input$perm_sp_given, collapse = ',')
      }
      
      catch <- try(create_credentials(
        user = newUser,
        pw = newPW,
        groups = groups,
        type = type,
        date.limit = date.limit, 
        perm_sp = perm_sp
      ))
      
      if(class(catch) == 'try-error'){
        shinyalert('¡Error!', 
                   'Se produjo un error al crear las credenciales. Por favor contacte a Baltazar 
                   (baltazargch@gmail.com)', type='error')
      } else {
        source('R/udf/drop_read_rds.R')
        upload_credentials()
        
        shinyalert('¡Credenciales creadas correctamente!', 
                   text = tagList(
                     div(style="text-align: left",
                         h4('Se han creado las credenciales de manera exitosa:\n', 
                            'Verifique la información, y si es correcto copiela y', 
                            'envíela al nuevo usuario. \n\n'), br(),
                         h4(strong('Usuario       :   '), newUser, br(), 
                            strong('Clave         :   '), newPW, br(), 
                            strong('Grupos        :   '), paste(groups, collapse = ', '), br(), 
                            strong('Link de acceso:'),'https://baltazargch.shinyapps.io/master_AOH/',
                            br(),
                            strong('Permisos:   '), type, br(),br(), 
                            strong('Familias             :'),
                            paste(input$perm_fam_given, collapse = ', '), br(), br(),
                            strong('Especies             :'),
                            paste(input$perm_sp_given, collapse = ', '), br(),  br(),
                            strong('Fecha de creación    : '), as.character(Sys.Date()), br(), 
                            strong('Fecha de vencimiento : '), as.character(Sys.Date() +  daysPermit)
                         )
                     )
                   ), html = T)
      }
    }
  })
  
  # Final ----------------------------------------------------------------------------------------------
  
  observeEvent(input$stop, {
    
    shinyalert('¡Sesión finalizada!',
               'En la carpeta "reportes/", del DropBox se ha guardado un backup 
               de los datos hasta ahora editados',
               type = 'success', showConfirmButton = F, 
               closeOnEsc = F, closeOnClickOutside = F, 
               showCancelButton = F)
    
    aoh_create_report(grp_app = grp_app, dir.list = dir.list, token = token, local = local)
    
    stopApp()
  })
  
  observeEvent(input$switchtab, ignoreInit = F, ignoreNULL = F, {
    updateTabItems(session, "tabs", "tabCon")
    updateTabItems(session, "tabs", "tabInicio")
    
    if(lastActive$type == 'invitado'){
      
      vals <- c("marcarCon", 'marcarForest', 'marcarHabs', 'marcarElev', 'stop', 'restaurar')
      vals <- paste0("#",vals)
      
      removeUI(
        selector = vals,
        multiple = T
      )
    }
  })
  
  session$onSessionEnded(function() { stopApp() })
  
  
  # Funcs edit:tables ----------------------------------------------------------------------------------
  cons.update.callback <- function(data, olddata, row) {
    
    gr <- data[row,]$Familia[1]
    orig <- df1[[gr]]
    
    actualizar <- which(orig$Especie == data[row, ]$Especie)
    
    actualizar_ref <- which(ref_sp$Especie == data[row,]$Especie)
    
    ref_sp[actualizar_ref, ]$f_editado_cons <<- TRUE
    ref_sp[actualizar_ref, ]$rev_con <<- TRUE
    dumm <- data[row,]
    dumm[] <- lapply(dumm, as.character)
    
    dumm$f_edit <- TRUE
    orig[actualizar,] <- dumm
    df1[[gr]] <<- orig
    
    orig <- orig %>% 
      mutate(Especie = as.factor(Especie)) %>% 
      mutate(Nacional = factor(Nacional, levels = sort(unique(Nacional)), exclude = NULL)) %>% 
      mutate(Global = factor(Global, levels = lvlGlo, exclude = NULL)) %>% 
      mutate(Endemismo = factor(Endemismo, levels = c(0,1,NA)))
    
    return(orig)
  } #funcionando
  cons.delete.callback <- function(data, row) {
    gr <- data[row, ]$Familia[1]
    
    actualizar_ref <- which(ref_sp$Especie == data[row,]$Especie)
    ref_sp[actualizar_ref, ]$f_borrado_cons <<- TRUE
    
    orig <- df1[[gr]]
    orig <- orig[!orig$Especie == data[row,]$Especie, ]
    
    df1[[gr]] <<- orig
    
    orig <- orig %>% 
      mutate(Especie = as.factor(Especie)) %>% 
      mutate(Nacional = factor(Nacional, levels = sort(unique(Nacional)), exclude = NULL)) %>% 
      mutate(Global = factor(Global, levels = lvlGlo, exclude = NULL)) %>% 
      mutate(Endemismo = factor(Endemismo, levels = c(0,1,NA)))
    return(orig)
    
  } #funcionando
  
  habs.delete.callback <- function(data, row) {
    sp <- data[row, ]$Especie
    
    actualizar_ref <- which(ref_sp$Especie == sp)
    ref_sp[actualizar_ref, ]$f_borrado_habs <<- TRUE
    
    orig <- df2[[sp]]
    orig <- orig[!orig$Codigo == data[row,]$Codigo, ]
    
    df2[[sp]] <<- orig
    
    valsR <- unique(totalhabs[totalhabs$Especie == sp, "Cobertura"])

    orig$Cobertura <- factor(orig$Cobertura, levels = unique(c(orig$Cobertura, valsR)))
    orig$Idoneidad <- factor(orig$Idoneidad, levels = c('No apto', 'Apto', 'Marginal'))
    
    return(orig)
  } #funcionando
  habs.update.callback <- function(data, olddata, row) {
    sp <- data[row,]$Especie
    orig <- df2[[sp]]
    
    actualizar_ref <- which(ref_sp$Especie == sp)
    ref_sp[actualizar_ref, ]$f_editado_habs <<- TRUE
    
    actualizar <- which(orig$Codigo == data[row,]$Codigo)
    
    dumm <- data[row,]
    dumm[] <- lapply(dumm, as.character)
    
    dumm$f_edit <- TRUE
    
    orig[actualizar,] <- dumm
    
    df2[[sp]] <<- orig
    
    valsR <- unique(totalhabs[totalhabs$Especie == sp, "Cobertura"])

    orig$Cobertura <- factor(orig$Cobertura, levels = unique(c(orig$Cobertura, valsR)))
    orig$Idoneidad <- factor(orig$Idoneidad, levels = c('No apto', 'Apto', 'Marginal'))
    
    return(orig)
  } #funcionando
  habs.insert.callback <- function(data, row){
    data[row,]$Especie <- sp <- data$Especie[1]
    data[row,]$Codigo <- replace.values(valsHabs$Habitat, valsHabs$value, data[row,]$Cobertura)
    
    data[row,]$f_edit <- FALSE
    data[row,]$f_delete <- FALSE
    data[row,]$f_new <- TRUE
    
    df2[[sp]] <<- rbind(df2[[sp]], data[row,]) 
    
    actualizar_ref <- which(ref_sp$Especie == sp)
    ref_sp[actualizar_ref, ]$f_nuevo_habs <<- TRUE
    
    orig <- df2[[sp]]
    valsR <- unique(totalhabs[totalhabs$Especie == sp, "Cobertura"])

    orig$Cobertura <- factor(orig$Cobertura, levels = unique(c(orig$Cobertura, valsR)))
    orig$Idoneidad <- factor(orig$Idoneidad, levels = c('No apto', 'Apto', 'Marginal'))
    return(orig)
  } #funcionando
  
  elev.update.callback <- function(data, olddata, row) {
    sp <- data[row,]$Especie
    orig <- df3[[sp]]
    
    actualizar_ref <- which(ref_sp$Especie == sp)
    ref_sp[actualizar_ref, ]$f_editado_elev <<- TRUE
    
    dumm <- data[row,]
    dumm[] <- lapply(dumm, as.character)
    
    dumm$f_edit <- TRUE
    dumm[,2:5] <- as.numeric(dumm[,2:5])
    orig[1,] <- dumm
    
    if(!is.na(orig$InferiorApto) & orig$InferiorApto < orig$limiteInferior){
      shinyalert('¡Advertencia!', 
                 'El límite inferior apto es menor que el disponible. Se usará el mínimo disponbile', 
                 type = 'warning')
      orig$InferiorApto <- orig$limiteInferior
    }
    if(!is.na(orig$SuperiorApto) & orig$SuperiorApto > orig$limiteSuperior){
      shinyalert('¡Advertencia!', 
                 'El límite superior apto es mayor que el disponible. Se usará el máximo disponbile', 
                 type = 'warning')
      orig$SuperiorApto <- orig$limiteSuperior
    }
    
    df3[[sp]] <<- orig
    
    return(orig)
  } #funcionando
  
  
  remove_modal_spinner()
}

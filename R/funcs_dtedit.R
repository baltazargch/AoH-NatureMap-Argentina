cons.update.callback <- function(data, olddata, row) {
  
  gr <- data[row,]$Familia[1]
  orig <- df1[[gr]]
  
  actualizar <- which(orig$Especie == data[row, ]$Especie)
  
  actualizar_ref <- which(ref_sp$Especie == data[row,]$Especie)
  
  ref_sp[actualizar_ref, ]$f_editado_cons <<- TRUE
  ref_sp[actualizar_ref, ]$rev_con <<- TRUE
  # print(ref_sp[actualizar_ref,])
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
  # valsR <- replace.values(valsHabs$value, valsHabs$Habitat, valsR)
  
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
  # valsR <- replace.values(valsHabs$value, valsHabs$Habitat, valsR)
  
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
  # valsR <- replace.values(valsHabs$value, valsHabs$Habitat, valsR)
  
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
    shinyalert('¡Advertencia!', 'El límite inferior apto es menor que el disponible. Se usará el mínimo disponbile', 
               type = 'warning')
    orig$InferiorApto <- orig$limiteInferior
  }
  if(!is.na(orig$SuperiorApto) & orig$SuperiorApto > orig$limiteSuperior){
    shinyalert('¡Advertencia!', 'El límite superior apto es mayor que el disponible. Se usará el máximo disponbile', 
               type = 'warning')
    orig$SuperiorApto <- orig$limiteSuperior
  }
  
  df3[[sp]] <<- orig
  
  return(orig)
} #funcionando
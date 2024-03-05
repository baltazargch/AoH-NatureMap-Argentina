create_credentials <- function(user, pw, groups = c('anfibios', 'reptiles', 'mamiferos', 
                                                    'aves', 'peces', 'plantas'), 
                               type = c('experto', 'invitado', 'admin'), 
                               date.limit = NULL, perm_sp = NULL) {
  require(scrypt)
  require(lubridate)
  stopifnot(!missing(user), !missing(pw), !missing(groups), !missingArg(type), 
            length(user) == 1, length(pw) == 1, all(type  %in% c('experto', 'invitado', 'admin')))
  if(type != 'admin') {
    stopifnot(!identical(groups, c('anfibios', 'reptiles', 'mamiferos', 
                                  'aves', 'peces', 'plantas')))
  } else if (type == 'admin'){
    groups <- c('anfibios', 'reptiles', 'mamiferos', 
               'aves', 'peces', 'plantas')
  }
  
  if(type == 'invitado') {
    permits <- 'restringido'
    perm_sp <- paste(perm_sp, collapse = ',')
  } else {
    permits <- perm_sp <- 'todos'
  }
  
  if(type != 'invitado' & is.null(date.limit)){
    date.limit <- Inf
  } else if (type == 'invitado' & is.null(date.limit)){
    stop('La fecha límite debe especificarse y no debe ser más de 1 mes de duración desde la fecha actual')
  } else if (type != 'invitado' & !is.null(date.limit)){
    message('El argumento de fecha se ignorará porque el tipo de usuario no es invitado')
    date.limit <- Inf
  } else if (type == 'invitado' & !is.null(date.limit)){
    # date.limit <- '2020-11-20'
    prs.date <- lubridate::parse_date_time(date.limit, 'ymd')
    
    today <- lubridate::parse_date_time(Sys.Date(), 'ymd')
    
    date.diff <- as.Date(prs.date, format="%Y-%m-%d") > as.Date(today, format="%Y-%m-%d")
    
    size <- as.numeric(as.Date(prs.date, format="%Y-%m-%d") - as.Date(today, format="%Y-%m-%d")) > 30
    
    if(is.na(prs.date)) {
      stop('La fecha debe estar en formato yyyy-mm-dd (e.g., 2020-11-05)')
    } else if (!date.diff){
      stop('El tiempo de acceso es menor a 1 día. Revisar la fecha')
    } else if (size) {
      stop('El tiempo de acceso es mayor al máximo permitido (30 días). Revisar la fecha')
    } else {
      date.limit <- as.character(prs.date)
    }
  }
  
  dt_credentials <- readRDS('data/credentials.rds')
  
  new_credentials <- data.frame(
    user = user, 
    pw = hashPassword(pw), 
    groups = paste(groups, sep = ' ', collapse = ','), 
    type = type, 
    permits = permits, 
    date.limit = date.limit, 
    perm_sp = perm_sp)
  
  dt_credentials <- rbind(dt_credentials, new_credentials)
  
  saveRDS(dt_credentials, 'data/credentials.rds')
}

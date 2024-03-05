aoh_create_report <- function(grp_app, dir.list, token, local){
  report.dir <- paste0('/NatureMap/reportes/',grp_app,'/', Sys.Date())
  flag.dir <- rdrop2::drop_exists(report.dir, dtoken = token)
  
  if(!flag.dir){
    rdrop2::drop_create(report.dir, dtoken = token)
    version <- '_v1'
  } else if(flag.dir){
    ver.list <- drop_dir(report.dir, dtoken = token)
    version <- paste0('_v', (NROW(ver.list)/5) + 1)
  }
  
  Conservacion <- aoh_table_read(dir.list$edit$cons, local = local, is.cons = T, dtoken=token)
  Coberturas <- aoh_table_read(dir.list$edit$cobs, local = local, is.cons = F, dtoken=token)
  elevacion <- aoh_table_read(dir.list$edit$elev, local = local, is.cons = F, dtoken=token)
  ref_sp <- aoh_table_read(dir.list$spps, local = local, is.cons = F, dtoken=token)
  forest <- aoh_table_read(dir.list$edit$fore, local = local, is.cons = F, dtoken=token)
  
  aoh_table_write(table = Conservacion, file = dir.list$edit$cons, local = F, 
                  is.cons = T, 
                  dummy.dir = paste0(tempdir(), '/conservacion', version, '.csv' ), 
                  path=report.dir, dtoken=token)
  
  aoh_table_write(table = Coberturas, file = dir.list$edit$cobs, local = F, 
                  is.cons = F, 
                  dummy.dir = paste0(tempdir(), '/habitats', version, '.csv' ), 
                  path=report.dir, dtoken=token)
  
  aoh_table_write(table = elevacion, file = dir.list$edit$elev, local = F, 
                  is.cons = F, 
                  dummy.dir = paste0(tempdir(), '/elevacion', version, '.csv' ), 
                  path=report.dir, dtoken=token)
  
  aoh_table_write(table = ref_sp, file = dir.list$spps, local = F, 
                  is.cons = F, 
                  dummy.dir = paste0(tempdir(), '/referencia', version, '.csv' ), 
                  path=report.dir, dtoken=token)
  
  aoh_table_write(table = forest, file = dir.list$edit$fore, local = F, 
                  is.cons = F, 
                  dummy.dir = paste0(tempdir(), '/forestaciones', version, '.csv' ), 
                  path=report.dir, dtoken=token)
}
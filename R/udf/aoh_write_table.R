aoh_table_write <- function(table = NULL, file=NULL, local=TRUE, is.cons=FALSE, dummy.dir='',...){
  stopifnot(class(table)[1]  %in% c('data.frame', 'tbl_df', 'tbl'), !is.null(local), !is.null(is.cons), !is.null(file))
  if(dummy.dir == '' && !local) stop('Si no es loca, debe de haber un "dummy.dir" válido')
  require(rdrop2)
  
  if (local && is.cons) {
    write.csv(table, file, na='', row.names = F)
  } else if (local && !is.cons) {
    write.csv(table, file, row.names = F)
  } else if (!local && is.cons) {
    write.csv(table, dummy.dir, row.names = F, na='')
    drop_upload(dummy.dir, ...)
  } else if (!local && !is.cons) {
    write.csv(table, dummy.dir, row.names = F)
    drop_upload(dummy.dir, ...)
  }
}
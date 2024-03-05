aoh_table_read <- function(file=NULL, local=TRUE, is.cons=FALSE, ...){
  stopifnot(!is.null(file), !is.null(local), !is.null(is.cons)) 
  require(rdrop2)
  if (local && is.cons) {
    df <- read.csv(file, na='')
  } else if (local && !is.cons) {
    df <- read.csv(file, na='')
  } else if (!local && is.cons) {
    df <- rdrop2::drop_read_csv(file, na='', ...)
  } else if (!local && !is.cons) {
    df <- rdrop2::drop_read_csv(file, ...)
  }
  return(df)
}

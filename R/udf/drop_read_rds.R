drop_read_crds <- function(file = '/NatureMap/data/credentials.rds', 
                          dest = 'data/credentials.rds', 
                          dtoken = NULL, ...) {
  require(rdrop2)
  drop_download(file, local_path = dest, overwrite = TRUE, dtoken = dtoken)
  readRDS(dest, ...)
}


upload_credentials <- function(file ='data/credentials.rds', 
                               dest = '/NatureMap/data'){
  require(rdrop2)
  drop_upload(file, dest)
}

# token <- readRDS('info/token.rds')
# # upload_credentials()
# crds <- drop_read_crds(dtoken = token)


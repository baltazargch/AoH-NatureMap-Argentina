# source('R/udf/create_credentials.R')
# grupos <-  c('anfibios', 'reptiles', 'mamiferos', 'aves', 'peces', 'plantas')
# 
# original_credentials <- list(
#   users = list('baltazar', 'gabriel', 'javier', 'sebastian', 'kini', 'diego', 'ambiente'),
#   pws = list('tool', 'natmapaoh', 'natmapaoh', 'natmapaoh', 'natmapaoh', 'natmapaoh', 'natmapaoh'),
#   groups = list(grupos, grupos, c('anfibios', 'reptiles'), 'mamiferos', 'aves', 'mamiferos', 'mamiferos'),
#   types = list('admin', 'admin', 'experto','experto','experto','experto','experto')
# )
# 
# crds <- data.frame(
#   user = NA,
#   pw = NA,
#   groups = NA,
#   type = NA,
#   permits = NA,
#   date.limit = NA
# )
# 
# saveRDS(na.omit(crds), 'data/credentials.rds')
# 
# for (i in seq_along(original_credentials$users)) {
#   create_credentials(user = original_credentials$users[[i]],
#                      pw= original_credentials$pws[[i]],
#                      groups = original_credentials$groups[[i]],
#                      type = original_credentials$types[[i]])
# }
# 
# crds <- readRDS('data/credentials.rds')
# token <- readRDS('info/token.rds')
# 
# 

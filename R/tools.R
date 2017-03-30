# creat directory if doesn't exist
# dic, completed directory
validateAndCreatDir <- function(dir){
  if(!dir.exists(dir))
    dir.create(dir)
}

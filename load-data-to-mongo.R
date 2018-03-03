save_data_mongodb <- function(data) {
    db <- mongo(collection = TABLE_NAME, url = sprintf("mongodb://%s:%s@%s/%s", 
        options()$mongodb$username, options()$mongodb$password, 
        options()$mongodb$host, DB_NAME))
    data <- as.data.frame(t(data))
    db$insert(data)
}

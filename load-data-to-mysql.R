save_data_mysql <- function(data) {
    db <- dbConnect(MySQL(), dbname = DB_NAME, host = options()$mysql$host, 
        port = options()$mysql$port, user = options()$mysql$user, 
        password = options()$mysql$password)
    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')", TABLE_NAME, 
        paste(names(data), collapse = ", "), paste(data, collapse = "', '"))
    dbGetQuery(db, query)
    dbDisconnect(db)
}

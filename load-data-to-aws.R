save_data_s3 <- function(data) {
    data <- paste0(paste(names(data), collapse = ","), "\n", 
        paste(unname(data), collapse = ","))
    file_name <- paste0(paste(get_time_human(), digest(data, 
        algo = "md5"), sep = "_"), ".csv")
    put_object(file = charToRaw(data), object = file_name, bucket = s3_bucket_name)
}

require(stringi)

generate_passwords <- function(n, len){
stri_rand_strings(n, length=len, pattern="[A-Za-z0-9!;:'<>@]")
}

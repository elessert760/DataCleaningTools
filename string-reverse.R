
# Doing these just for fun because I saw that this is a common task 
# when learning a new language and wanted to give it a shot

string_reverser <- function(x = "string"){
  
  x <- as.character(x) 
  len <- strsplit(x, split= "")
  paste(len[[1]][length(len[[1]]):1], collapse = "")

}


palindrome_checker <- function(.x = "palindrome"){
  
  .x <- as.character(.x)
  
  if (.x == string_reverser(.x)){ print(paste(.x, " is a palindrome"))
  }  else { 
    print(paste(.x, " is NOT a palindrome"))
  }
}


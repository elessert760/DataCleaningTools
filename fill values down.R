# Assume you have a data frame named houston and that if CCN is empty then the record is meant to be
# assigned to the instance above where CCN is filled in

for (p in colnames(houston)) {
  houston[,p] <- houston[, p] %>% trimws()
}

houston <- houston %>% unique()


change <- which(houston[,"CCN"] != "")

for (p in colnames(houston)) {
  for (i in change) {
    if (houston[i,p] == "" | is.na(houston[i,p])) {
      houston[i, p] <- "Not Provided"
    }
  }
}

for (j in colnames(houston)) {
  for (i in 1:nrow(houston)) {
    if (houston[i, j] == "" | is.na(houston[i,j])) {
      houston[i, j] <- houston[i - 1 , j]
    }
  }
}

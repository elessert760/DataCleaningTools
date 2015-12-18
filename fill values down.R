# Assume you have a data frame named df and that if "column" is empty then the record is meant to be
# assigned to the instance above where "column" is filled in

for (p in colnames(df)) {
  df[,p] <- df[, p] %>% trimws()
}

df <- df %>% unique()


change <- which(df[,"column"] != "" | is.na(df[,"column"]))

for (p in colnames(df)) {
  for (i in change) {
    if (df[i,p] == "" | is.na(df[i,p])) {
      df[i, p] <- "Not Provided"
    }
  }
}

for (j in colnames(df)) {
  for (i in 1:nrow(df)) {
    if (df[i, j] == "" | is.na(df[i,j])) {
      df[i, j] <- df[i - 1 , j]
    }
  }
}



#hh:mm:ss
df$time <- sapply(strsplit(df$time,":"),
       function(x) {
           x <- as.numeric(x)
           x[1]+x[2]+x[3]/60
       }
)

#mm:ss
df$time <- sapply(strsplit(df$time,":"),
       function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
       }
)

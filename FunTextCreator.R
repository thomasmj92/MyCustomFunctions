# Function to convert a lowercase string of text to an 
# alternating-case "Spongebob Meme" style string

fun.text.creator <- function(my.text) {
  location <- c(1:nchar(my.text))
  chars <- c(rep(0, nchar(my.text)))
  
  for (i in 1:nchar(my.text)) {
    chars[i] <- substr(my.text, i, i)
  }
  
  df <- as.data.frame(cbind(location, chars), stringsAsFactors = FALSE)
  
  for(i in 1:(nrow(df)) %/% 2) {
    df$chars[i*2] <- toupper(df$chars[i*2])
  }
  
  paste(df$chars, collapse = "")
  
}
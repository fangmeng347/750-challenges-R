# install packages if not already in

# load packages
library(magrittr)
library(tm)
library(SnowballC)



placements <- rawText %>% 
              parseText(rawText) %>%
              extractWords %>% 
              computeWeighting(text) %>% 
              compute_word_extents %>% 
              order_words(ordering_strategy) %>% 
              place_words
display_word_cloud(placements, output_format, output_target)

# reads in 2 types of sources, returns a string with no internal structure
# requirement: raw_text is either a url or a .txt file (already in the directory)
parseText = function(rawText) {
 # text <- readLines("frenchMagazine.txt")
  # for testing
  rawTextURL = paste0("http://www.sthda.com/sthda/RDoc/example-files/",
                     "martin-luther-king-i-have-a-dream-speech.txt")
  rawTextURL1 = "http://www.stat.cmu.edu/~hseltman/601/.index.html"
  
  txt = readLines(rawText)
  # if it's a .html
  # first get rid of the <...>, then get rid of the character reference &#...
  txt2 = gsub("<[[:space:]]*[/]?[a-zA-Z].*?>", "", txt)
  txt3 = gsub("&#?\\w+;", "", txt2)
  
  text = Corpus(VectorSource(txt3))
  inspect(text)
  return(text)
}

extractWords = function(text) {
  # replace special characters "/", "@", "\\|" with whitespace
  toSpace = content_transformer(function (x , pattern) gsub(pattern, " ", x))
  text = tm_map(text, toSpace, "/")
  text = tm_map(text, toSpace, "@")
  text = tm_map(text, toSpace, "\\|")
  
  # convert the text to lower case
  text = tm_map(text, content_transformer(tolower))
  # remove numbers
  text = tm_map(text, removeNumbers)
  # remove punctuations
  text = tm_map(text, removePunctuation)
  # remove extra white spaces
  text = tm_map(text, stripWhitespace)
  # remove stopwords in english
  text = tm_map(text, removeWords, stopwords("english"))
  # text stemming
  text = tm_map(text, stemDocument)
}

computeWeighting(text) {
  doc <- TermDocumentMatrix(text)
  m <- as.matrix(doc)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word=names(v), freq=v)
  head(d, 10)
}







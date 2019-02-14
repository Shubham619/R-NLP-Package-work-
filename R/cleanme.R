#'  @title  Cleans the text
#'
#'  @description It cleans the text such as package remove punctuation, Remove Special characters, Remove stop words, Remove HTML Tags,Convert all the characters into small letters and use the SnowballStemmer to stem the words
#'
#'  @param 'x'
#'
#'  @return x
#'
#'  @examples cleanme('x')
#'
#'  @export



cleanme <- function(x)
{

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  x<- replace_abbreviation(x)
  x<- replace_ordinal(x)
  x<- replace_symbol(x)
  x<-tolower(x)
  x = gsub("[[:punct:]]", " ", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove html links
  x = gsub("http\\w+", "", x)
  x<-stripWhitespace(x)
  x<-removeWords(x,c(stopwords("en")))


  x<-stem_snowball(x, algorithm = "en")
  x<-gsub("<.*?>", "", x)
  x<- str_trim(x,side=c("both","left","right"))
  x<-stem_snowball(x, algorithm = "en")
  #x<-corpus::text_tokens(x,stemmer="en")
  #x<- tm:re,replace_number(x)
  return(x)





}

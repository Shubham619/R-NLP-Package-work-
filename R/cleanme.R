#'  @title  Cleans the text,load ml and nlp packages
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

  req_packages<-c("tidyverse","tidyr","readr","dplyr","tm","qdap","tidyverse","tidytext","ngram","tm.plugin.webmining","corpus","textclean")


  check.and.install.Package<-function(package_name){
    if(!package_name%in%installed.packages()){
      install.packages(package_name)
    }
  }

  for(i in req_packages){
    check.and.install.Package(as.character(i))
  }


  lapply(req_packages, require, character.only = TRUE)
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

ml_mode<-function(){
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  req_packages<-c("tidyverse","tidyr","readr","dplyr","tm","qdap","tidyverse","tidytext","ngram","tm.plugin.webmining","corpus",
                  "ggplot2","textclean","caret","shiny","lubridate","glmnet","randomForest","xtable","xlsx","readxl","zoo","xts","Rcpp","data.table","purrr","rio","quantmod","magrittr","flexdashboard","flexdashboardPlus","plumber","reticulate","broom","C50","earth",
                  "gbm","glmnet","mlr","FactoMineR","caTools","naniar","datasets","yardstick","psych","xgboost","tree","Rtsne","Amelia","fuzzywuzzyR","stringi","R.oo","rminer","text2vec","utf8","NLP","SnowballC","quanteda","MonkeyLearn","syuzhet","koRpus")


  check.and.install.Package<-function(package_name){
    if(!package_name%in%installed.packages()){
      install.packages(package_name)
    }
  }

  for(i in req_packages){
    check.and.install.Package(as.character(i))
  }


  lapply(req_packages, require, character.only = TRUE)
}

nlp_mode<-function(){
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  req_packages<-c("tidyverse","tidyr","readr","dplyr","tm","qdap","tidyverse","tidytext","ngram","tm.plugin.webmining","corpus",
                  "ggplot2","textclean","caret","readxl","rminer","text2vec","utf8","NLP","SnowballC","quanteda","MonkeyLearn","syuzhet","koRpus")


  check.and.install.Package<-function(package_name){
    if(!package_name%in%installed.packages()){
      install.packages(package_name)
    }
  }

  for(i in req_packages){
    check.and.install.Package(as.character(i))
  }


  lapply(req_packages, require, character.only = TRUE)




}




EDA_mode<-function(){
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  req_packages<-c("xlsx","Amelia","ggplot2","tidyverse","shiny","plotly","wordcloud","wordcloud2","leaflet","corrplot","gganimate","r2d3","misc3d","rgl")


  check.and.install.Package<-function(package_name){
    if(!package_name%in%installed.packages()){
      install.packages(package_name)
    }
  }

  for(i in req_packages){
    check.and.install.Package(as.character(i))
  }


  lapply(req_packages, require, character.only = TRUE)




}


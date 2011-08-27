#' make wordcloud from your clipboard
#' 
#' make wordcloud from your clipboard
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param type specific part of speech you want (You have to need input by Japanese Kanji character)
#' @param min minimum frequency of word
#' @param \dots optional argument to "wordcloud"
#' @return NULL(word-cloud image)
#' @note %% ~~further notes~~
#' @author %% Ichikawa Daisuke
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' clpcloud(min=3)
#' 


clpcloud <-
function(type=NULL, min=1, ...){
  require(RMeCab)
  require(wordcloud)
  require(plyr)
  require(RColorBrewer)
  if( .Platform$OS.type=="unix"){
    txt <- read.delim(pipe("pbpaste"), as.is=TRUE, header=FALSE)  	
  }else{
    txt <- read.delim("clipboard", as.is=TRUE, header=FALSE)
  }
  res <- unlist(apply(txt,1,RMeCabC, mypref=1))
  res <- res[!(attr(res, "names") %in% c("助詞", "助動詞", "記号", "接頭詞","連体詞"))]
  if(is.null(type)){
  	res <- res
  } else {
  	res <- res[attr(res, "names") %in% type]
  }
  item <- data.frame(hinsi=attr(res, "names"), word=res, stringsAsFactors=FALSE)
  res <- ddply(item, .(hinsi, word), summarise, count=sum(!is.na(word)))
  if(is.null(type)){
    res$num <- as.numeric(as.factor(res$hinsi))
    pal <- data.frame(num=1:length(unique(res$hinsi)), 
                      pal=brewer.pal(length(unique(res$hinsi)),"Set1"),
                      stringsAsFactors=FALSE)
    res <- merge(res, pal)
    wordcloud2(res$word, res$count, min.freq=min, colors=res$pal, ...)
  } else {
    pal <- brewer.pal(9,"BuGn")[5:9]
    wordcloud(res$word, res$count, min.freq=min, colors=pal, ...)
  }
}


library(tm)
library(data.table)
library(tidytext)
library(tidyverse)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(reshape2)


tdm<-function(data){
  corpus<-VCorpus(VectorSource(data$stemmedwords))
  tdm.all<-TermDocumentMatrix(tm_map(corpus, stemDocument))
  tdm.tidy=tidy(tdm.all)
  tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
  return(tdm.overall)
}


wordcloudplot<-function(tdm_data){
  tdm.overall<-tdm_data
  wordcloud(tdm.overall$term, tdm.overall$`sum(count)`,
            scale=c(5,0.5),
            max.words=200,
            min.freq=1,
            random.order=FALSE,
            rot.per=0.3,
            use.r.layout=T,
            random.color=FALSE)
}
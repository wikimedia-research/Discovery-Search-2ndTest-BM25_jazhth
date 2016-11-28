library(jiebaR)
library(ropencc)
library(feather)

#thwiki
queries_th <- searches[searches$wiki=="thwiki", c("page_id","query")]
save(queries_th, file="data/queries_th.RData")
system("scp data/queries_th.RData chelsyx@stat2:~/")
# run on stat2
load("queries_th.RData")
output <- lapply(queries_th$query, function(query){
  query <- gsub("[[:punct:]]", " ", query)
  curl_string <- paste0("curl -XPOST http://elastic2020.codfw.wmnet:9200/thwiki_content/page/_mtermvectors -d '{
        \"docs\": [{
                        \"doc\": { \"title\": \"",query,
                        "\" },
                        \"fields\": [\"title\", \"title.plain\"],
                        \"positions\": true,
                        \"offsets\": false,
                        \"term_statistics\": false,
                        \"field_statistics\": false
  }]
  }'")
  results <- system(curl_string, intern = TRUE)
  results <- jsonlite::fromJSON(results,simplifyVector=F)
  tokens <- names(results$docs[[1]]$term_vectors$title$terms)
  return(tokens)
})
names(output) <- queries_th$page_id
save(output, file="tokens_th.RData")
#local
system("scp chelsyx@stat2:~/tokens_th.RData data/")
#filter stop words
load("data/tokens_th.RData")
tokens_th <- output; rm(output)
stopword_th <- jsonlite::fromJSON("resource/th.json")
tokens_th <- filter_segment(tokens_th,stopword_th)

#zhwiki
queries_zh <- searches[searches$wiki=="zhwiki", c("page_id","query")]
queries_zh$query <- gsub("[[:punct:]]", " ", queries_zh$query)
stopword_zh <- jsonlite::fromJSON("resource/zh.json")
ccst = converter(S2T)
stopword_zh <- union(ccst[stopword_zh], stopword_zh)
mixseg = worker(bylines = TRUE)
tokens_zh <- mixseg[queries_zh$query]
tokens_zh <- filter_segment(tokens_zh,stopword_zh)
names(tokens_zh) <- queries_zh$page_id

#jawiki
queries_ja <- searches[searches$wiki=="jawiki", c("page_id","query")]
write_feather(queries_ja, "data/queries_ja.feather")
tokens_ja <- jsonlite::fromJSON("data/tokens_ja.json")
stopword_ja <- jsonlite::fromJSON("resource/ja.json")
tokens_ja <- filter_segment(tokens_ja,stopword_ja)
tokens_ja <- lapply(tokens_ja, function(x) gsub(" +","",x)) # strip space before/after string


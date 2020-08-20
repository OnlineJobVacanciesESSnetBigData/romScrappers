# ANOFM 
# API
library(httr)
library(jsonlite)

set_config(use_proxy("http://proxy.insro.local", 8080))


localitati_url <- "https://www.anofm.ro/dmxConnect/api/oferte_bos/selectLocalitate.php?agentie=ANOFM"
localitati <- GET(localitati_url)
localitati <- content(localitati, type = "text")
localitati <- fromJSON(localitati, simplifyDataFrame = TRUE)
localitati <- unlist(localitati)
localitati <- trimws(localitati)



nr_joburi_url <- "https://www.anofm.ro/dmxConnect/api/oferte_bos/statiaticilmv.php?agentie=ANOFM"
nr_joburi <- GET(nr_joburi_url)
nr_joburi <- content(nr_joburi, type = "text")
nr_joburi <- fromJSON(nr_joburi, simplifyDataFrame = TRUE)
nr_joburi <- nr_joburi[[1]]
colnames(nr_joburi)[1:2] <- c("numar oferte", "numar posturi")



lim <- 8
id_joburi <- paste0("https://www.anofm.ro/dmxConnect/api/oferte_bos/oferte_bos_query2L.php?offset=&cauta=&select=ANOFM&limit=", lim, "&localitate=")
id_joburi <- GET(id_joburi)
id_joburi <- content(id_joburi, type = "text")
id_joburi <- fromJSON(id_joburi)

lim <- id_joburi$lmv$total
id_joburi <- paste0("https://www.anofm.ro/dmxConnect/api/oferte_bos/oferte_bos_query2L.php?offset=&cauta=&select=ANOFM&limit=", lim, "&localitate=")
id_joburi <- GET(id_joburi)
id_joburi <- content(id_joburi, type = "text")
id_joburi <- fromJSON(id_joburi)
id_joburi <- id_joburi$lmv$data

# posted_job_id = 1:10
url_job <- "https://www.anofm.ro/dmxConnect/api/oferte_bos/detalii_lmv.php?id_lmv="
job_description <- sapply(id_joburi$POSTED_JOBS_ID, function(x){
  job_desc <- GET(paste0(url_job,"%20", trimws(x)))
  job_desc <- content(job_desc, type = "text")
  job_desc <- fromJSON(job_desc, simplifyDataFrame = TRUE)
  anofm_jobs <- rbind(anofm_jobs, job_desc[[1]])
}) 

anofm <- do.call('rbind', job_description)
write.csv(anofm, "anofm_30_08.csv", row.names = FALSE)

# Posturi gov
library(RSelenium)
library(rvest)
library(parallel)
library(doParallel)
library(foreach)


url_gov <- "http://posturi.gov.ro/"

rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                       port = 4444L,
                       browserName = "firefox")

###################################################################################################
extractText <- function(x, css){
  x <- x %>% html_node(css) %>% html_text()
  return(x)
}

extractAttr <- function(x, css, attr){
  x <- x %>% html_nodes(css) %>% html_attr(attr)
  return(x)
}


##################################################################################################

rmdSel$open()
rmdSel$navigate(url_gov)
job_links <- list()

no_pages <- read_html(rmdSel$getPageSource()[[1]]) %>% html_node("div.pagination-s>span") %>% html_text() %>% grep(pattern = "(?<=/)", perl = TRUE, ignore.case = TRUE, value = TRUE)
no_pages <- as.integer(strsplit(no_pages, split= " / ")[[1]][2])

url_pag_no <- "http://posturi.gov.ro/page/"


cl <- detectCores() - 1
cl <- makeCluster(cl)
registerDoParallel(cl)


clusterEvalQ(cl,{
  library(RSelenium)
  library(rvest)
  library(xml2)
  rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                         port = 4444L,
                         browserName = "firefox")
  
})

gov_res <- foreach(x=1:no_pages) %dopar%{
  rmdSel$open()
  rmdSel$navigate(paste0(url_pag_no, x, "/"))
  links <- read_html(rmdSel$getPageSource()[[1]])  
  links <- extractAttr(links,  "strong > a", "href")
  jobs <- list()
  for(i in links){
    rmdSel$navigate(i)
    source_page <- read_html(rmdSel$getPageSource()[[1]])
    nume_job <- extractText(source_page, "div > h2.title") %>% trimws()
    data_publicare <- extractText(source_page, "div.alignright") %>% trimws()
    nume_angajator <- extractText(source_page, "div.details > ul >li:nth-child(1)") %>% trimws()
    tip_angajator <- extractText(source_page, "div.details > ul >li:nth-child(2)") %>% trimws()
    tip_post <- extractText(source_page, "div.details > ul >li:nth-child(3)") %>% trimws()
    judet <- extractText(source_page, "div.details > ul >li:nth-child(4)") %>% trimws()
    durata_contract <- extractText(source_page, "div.details > ul >li:nth-child(5)") %>% trimws()
    nivel_post <- extractText(source_page, "div.details > ul >li:nth-child(5)") %>% trimws()
    termen_expirare <- extractText(source_page, "div.details > ul >li:nth-child(6)") %>% trimws()
    detalii <- extractText(source_page, "div.section.single > div.section_content") %>% trimws()
  jobs[[length(jobs) + 1]] <- list(nume_job = nume_job, data_publicare = data_publicare, nume_angajator = nume_angajator,
                                   tip_angajator = tip_angajator, tip_post = tip_post, judet = judet, durata_contract = durata_contract,
                                   nivel_post = nivel_post, termen_expirare = termen_expirare, detalii = detalii)
  }
  rmdSel$close()
  jobs
}

# clusterEvalQ(cl,{
#   rmdSel$close()
# })

stopImplicitCluster()

gov_jobs <- unlist(gov_res, recursive = FALSE)
gov_jobs <- lapply(gov_jobs, as.data.frame, stringAsFactors =FALSE)
gov_jobs <- do.call("rbind", gov_jobs)
gov_jobs$data_colectare <- Sys.Date()
gov_jobs$sursa <- "posturi.gov.ro"
write.csv(gov_jobs, "gov_jobs.csv", row.names = FALSE)

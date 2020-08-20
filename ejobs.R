# ejobs
    library(RSelenium)
    library(jsonlite)
    library(rvest)
    library(doParallel)
    library(parallel)
    library(foreach)
    
    
    ejobs_site <- "https://www.ejobs.ro/sitemap"
    
    
    
    rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                           port = 4444L,
                           browserName = "firefox")
    
    
    rmdSel$open()
    rmdSel$navigate(ejobs_site)
    tip_job <- read_html(rmdSel$getPageSource()[[1]])
    tip_job_categ_nume <- tip_job %>% html_nodes("a.ejblue12") %>% html_text()
    tip_job_categ_link <- tip_job %>% html_nodes("a.ejblue12") %>% html_attr("href")
    tip_job_categ <- cbind(tip_job_categ_nume, tip_job_categ_link)
    # categoriile de job-uri se afla de la randul 384-441
    
    # selectie anul/luna curenta
    links <- grep(pattern = "/2019/mai", tip_job_categ[,2], value = TRUE, ignore.case = TRUE)
    
    days <- lapply(links, function(x){
      rmdSel$navigate(x)
      days <- rmdSel$findElements("css", "tr:nth-child(1) > td > a.ejblue12")
      days <- sapply(days, function(x){
      res <-   x$getElementAttribute(attrName = "href")
      })
      days
    })
    
    days <- unlist(days)
    
    extractText <- function(x, css){
      x <- read_html(x) %>% html_node(css) %>% html_text()
      return(x)
    }
    
    extractAttr <- function(x, css, attr){
      x <- read_html(x) %>% html_nodes(css) %>% html_attr(attr)
      return(x)
    }
    
    
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
    
    ejobs_date <- foreach(x = 1:length(days), .errorhandling = "pass") %dopar% {
      rmdSel$open()
      rmdSel$navigate(days[x])
      pages <- extractAttr(rmdSel$getPageSource()[[1]], "span.ejvblack12 > a", "href")
      pages <- length(pages) + 1
      ejobs_list <- list()
      for(i in 1:pages){
        rmdSel$navigate(paste0("https://www.ejobs.ro/arhiva/2019/mai/",x,"/Pagina_", i, ".html"))
        jobs <- extractAttr(rmdSel$getPageSource()[[1]], "td > table.ejvblack12 > tbody > tr > td > a", "href")
        for(j in 1:length(jobs)){
          rmdSel$navigate(jobs[j])
          try({
          nume_job <- extractText(rmdSel$getPageSource()[[1]], "h1.jobad-title")
          firma <- extractText(rmdSel$getPageSource()[[1]], ".jobad-company-hero")
          numar_posturi <- extractText(rmdSel$getPageSource()[[1]], "div.jobad-facts.hidden-phone")
          data_publicare <- extractText(rmdSel$getPageSource()[[1]], "span[itemprop = 'datePosted']")
          data_expirare <- extractText(rmdSel$getPageSource()[[1]], "div.jobad-dates.hidden-phone")
          criteriu_limba <- extractText(rmdSel$getPageSource()[[1]], "ul[data-singular='limba straina']")
          criteriu_nivel_cariera <- extractText(rmdSel$getPageSource()[[1]], "ul[itemprop = 'experienceRequirements']")
          criteriu_nivel_studii <- extractText(rmdSel$getPageSource()[[1]], "ul[data-target = 'studii']")
          criteriu_tip_job <- extractText(rmdSel$getPageSource()[[1]], "ul[itemprop = 'employmentType']")
          criteriu_oras <- extractText(rmdSel$getPageSource()[[1]], "ul[data-singular = 'oras']")
          criteriu_departament <- extractText(rmdSel$getPageSource()[[1]], "ul[itemprop = 'occupationalCategory']")
          criteriu_domeniu <- extractText(rmdSel$getPageSource()[[1]], "ul[itemprop = 'industry']")
          criteriu_salariu <- extractText(rmdSel$getPageSource()[[1]], "ul.Criteria__List > li:last-child")
          descriere_integrala <- extractText(rmdSel$getPageSource()[[1]], "section.jobad-content.grid_18 > div.jobad-content-block") 
          }, silent = TRUE)
          ejobs_list[[length(ejobs_list) + 1]] <- list(nume_job = nume_job, firma = firma, numar_posturi = numar_posturi,
                                  data_publicare = data_publicare, data_expirare = data_expirare, 
                                  criteriu_limba = criteriu_limba, criteriu_nivel_cariera = criteriu_nivel_cariera,
                                  criteriu_nivel_studii = criteriu_nivel_studii, criteriu_tip_job = criteriu_tip_job,
                                  criteriu_oras = criteriu_oras, criteriu_departament = criteriu_departament,
                                  criteriu_domeniu = criteriu_domeniu, criteriu_salariu = criteriu_salariu,
                                  descriere_integrala = descriere_integrala)
        }
      }
    } 
    
    clusterEvalQ(cl, {
      rmdSel$close()
    })
    
    stopImplicitCluster()


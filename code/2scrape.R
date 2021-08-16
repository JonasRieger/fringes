library(batchtools)
library(data.table)
library(urltools)

makeRegistry("scrape", packages = c("urltools", "data.table"))

id = c("AT", "CH", "DK", "ESP", "FR", "GER", "IT", "NL")

fun = function(i){
  message("prep_bot.R")
  token = readLines(file.path("scraping_articles", "tokenpro.txt"))
  source(file.path("scraping_articles", "diffbot_function.R"))
  starttime = Sys.time()
  message(i)
  
  url = readRDS(file.path("data", i, "url.rds"))
  url_expand_table = readRDS(file.path("data", i, "url_expand_table.rds"))
  
  url = merge(url, url_expand_table, by = "url_new")
  url$url_new_expanded[is.na(url$url_status_code)] = url$url_new[is.na(url$url_status_code)]
  url[, favorite_sum := sum(as.integer(favorite_count))+length(favorite_count), by = url_new_expanded]
  url[, retweet_sum := sum(as.integer(retweet_count))+length(retweet_count), by = url_new_expanded]
  ### url_cores:
  parsed = url_parse(gsub("@", "", url$url_new_expanded))
  url_core = character(length(url$url_new_expanded))
  inactive = logical(length(url$url_new_expanded))
  
  domain = parsed$domain
  google_replace = grepl("google", parsed$domain) & grepl("\\.", parsed$path)
  if(any(google_replace)){
    url_core[google_replace] = sapply(strsplit(parsed$path[google_replace], "\\."),
                                      function(x) ifelse(grepl("/", x[2]), tail(strsplit(x[1], "/")[[1]], 1), x[2]))
    inactive[google_replace] = TRUE
    domain = domain[!inactive]
  }
  
  suffix = suffix_extract(domain)
  www_replace = !is.na(suffix$domain) & suffix$domain == "www"
  if(any(www_replace)){
    url_core[which(!inactive)[www_replace]] = sapply(strsplit(domain[www_replace], "\\."), function(x) x[2])
    inactive[which(!inactive)[www_replace]] = TRUE
  }
  na_replace = is.na(suffix$domain)
  if(any(na_replace)){
    url_core[which(!inactive)[na_replace]] = suffix[na_replace, "host"]
    inactive[which(!inactive)[na_replace]] = TRUE
  }
  url_core[!inactive] = suffix[!www_replace & !na_replace,"domain"]
  
  url[, url_core := NULL]
  url[, url_core := url_core]
  ###
  url[, retweet_sum_core := sum(as.integer(retweet_count))+length(retweet_count), by = url_core]
  
  saveRDS(url, file.path("data", i, "urlExpanded.rds"))
  
  to_scrape = url[!(url_core %in% c("twitter", "instagram", "youtube", "youtu")) &
                    (retweet_sum > 9 | favorite_sum > 9) &
                    !is.na(url_new_expanded), unique(url_new_expanded)]
  
  message(length(to_scrape), " URLs")
  system.time(botdata <- do.call(rbind, lapply(to_scrape, diffbot, token = token)))
  
  saveRDS(botdata, file = file.path("data", i, "botdata.rds"))
  
  message(difftime(Sys.time(), starttime, units = "hours"), " hours")
  
  gc(gcinfo(TRUE))
}

jobs = batchMap(fun = fun, id)

submitJobs(jobs, resources = list(walltime = 14*24*60*60, memory = 8192))

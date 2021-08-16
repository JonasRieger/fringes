library(longurl)
library(data.table)
library(batchtools)

makeRegistry("expand", packages = c("longurl", "data.table"))

id = c("AT", "CH", "DK", "ESP", "FR", "GER", "IT", "NL", "UK")

fun = function(i){
  message("expand.R")
  message(i)
  starttime = Sys.time()
  
  load(file.path("data", "RData", paste0(i, ".Rdata")))
  obj = as.data.table(get(i))
  rm(list = i)
  dir.create(file.path("data", i))
  saveRDS(obj, file.path("data", i, "raw.rds"))
  
  obj = obj[!is.na(obj$urls_expanded_url)]
  obj = obj[!duplicated(status_id, fromLast = TRUE)]
  obj[, urls_expanded_url := lapply(urls_expanded_url, unique)]
  tmp = unlist(obj$urls_expanded_url)
  obj = obj[rep(seq_len(.N), times = lengths(urls_expanded_url)),]
  obj[, url_new := tmp]
  saveRDS(obj, file.path("data", i, "url.rds"))
  
  to_expand = unique(obj$url_new)
  
  a = Sys.time()
  tmp = rbindlist(lapply(to_expand, function(x){
    o = try(expand_urls(x), silent = TRUE)
    if(class(o)[1] == "try-error") o = data.table(orig_url = x)
    o
  }), fill = TRUE)
  b = Sys.time()
  message(difftime(b, a, units = "hours"), " hours")
  
  expanded_url = tmp$expanded_url
  status_code = tmp$status_code
  
  a = Sys.time()
  if(any(is.na(status_code))){
    a = Sys.time()
    tmp = rbindlist(lapply(to_expand[is.na(status_code)], function(x){
      o = try(expand_urls(x), silent = TRUE)
      if(class(o)[1] == "try-error") o = data.table(orig_url = x)
      o
    }), fill = TRUE)
    b = Sys.time()
    expanded_url[is.na(status_code)] = tmp$expanded_url
    status_code[is.na(status_code)] = tmp$status_code
  }
  b = Sys.time()
  message(difftime(b, a, units = "hours"), " hours")
  
  
  url_expand_table = data.table(
    url_new = to_expand,
    url_new_expanded = expanded_url,
    url_status_code = status_code)
  
  saveRDS(url_expand_table, file = file.path("data", i, "url_expand_table.rds"))
  
  message(difftime(Sys.time(), starttime, units = "hours"), " hours")
  
  gc(gcinfo(TRUE))
}

batchMap(fun = fun, id)

submitJobs(resources = list(walltime = 48*60*60, memory = 4096))
# Some jobs failed because of memory, so there were also 8GB and 16GB jobs.
submitJobs(findErrors(), resources = list(walltime = 48*60*60, memory = 8192))
submitJobs(findErrors(), resources = list(walltime = 48*60*60, memory = 16384))
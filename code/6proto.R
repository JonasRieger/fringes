library(ldaPrototype)

for(i in c("AT", "CH", "DK", "ESP", "FR", "GER", "IT", "NL", "UK")){
  message(rep("#", nchar(i)+35*2), "\n", rep("#", nchar(i)+35*2), "\n",
          rep("#", 34), " ", i, " ", rep("#", 34), "\n",
          rep("#", nchar(i)+35*2), "\n", rep("#", nchar(i)+35*2), "\n")
  dir.create(file.path("data", i, "proto"))
  for(K in seq(20, 75, 5)){
    message(rep("#", 34), " K: ", K, " ", rep("#", 34))
    starttime = Sys.time()
    lda = readRDS(file.path("data", i, "lda", paste0(K, ".rds")))
    proto = getPrototype(lda, pm.backend = "socket", ncpus = 4)
    time = as.numeric(difftime(Sys.time(), starttime, units = "hours"))
    message(round(time, 2), " hours")
    saveRDS(proto, file.path("data", i, "proto", paste0(K, ".rds")))
  }
}

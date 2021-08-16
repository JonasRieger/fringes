library(ldaPrototype)
library(batchtools)

reg = makeExperimentRegistry(file.dir = "Batch", packages = "ldaPrototype")

sapply(seq(20, 75, 5),
  function(K) addProblem(paste0("AT", K), data = list(i = "AT", K = K, ncpus = 4)))
sapply(seq(20, 40, 5),
  function(K) addProblem(paste0("CH", K), data = list(i = "CH", K = K, ncpus = 4)))
sapply(seq(45, 75, 5),
  function(K) addProblem(paste0("CH", K), data = list(i = "CH", K = K, ncpus = 2)))
sapply(seq(20, 45, 5),
  function(K) addProblem(paste0("DK", K), data = list(i = "DK", K = K, ncpus = 2)))
sapply(seq(50, 75, 5),
  function(K) addProblem(paste0("DK", K), data = list(i = "DK", K = K, ncpus = 1)))
sapply(seq(20, 55, 5),
  function(K) addProblem(paste0("ESP", K), data = list(i = "ESP", K = K, ncpus = 1)))
sapply(seq(60, 75, 5),
  function(K) addProblem(paste0("ESP", K), data = list(i = "ESP", K = K, ncpus = 2)))
sapply(seq(20, 40, 5),
  function(K) addProblem(paste0("FR", K), data = list(i = "FR", K = K, ncpus = 1)))
sapply(seq(45, 75, 5),
  function(K) addProblem(paste0("FR", K), data = list(i = "FR", K = K, ncpus = 2)))
sapply(seq(20, 40, 5),
  function(K) addProblem(paste0("GER", K), data = list(i = "GER", K = K, ncpus = 2)))
sapply(seq(45, 75, 5),
  function(K) addProblem(paste0("GER", K), data = list(i = "GER", K = K, ncpus = 1)))
sapply(seq(20, 75, 5),
  function(K) addProblem(paste0("IT", K), data = list(i = "IT", K = K, ncpus = 1)))
sapply(seq(20, 75, 5),
  function(K) addProblem(paste0("NL", K), data = list(i = "NL", K = K, ncpus = 1)))
sapply(seq(20, 40, 5),
  function(K) addProblem(paste0("UK", K), data = list(i = "UK", K = K, ncpus = 2)))
sapply(seq(45, 75, 5),
  function(K) addProblem(paste0("UK", K), data = list(i = "UK", K = K, ncpus = 1)))

addAlgorithm("LDARepAlgo",
  fun = function(job, data, instance, seed, ...){
    i = data$i
    K = data$K
    ncpus = data$ncpus
    starttime = Sys.time()
    message("### ", i, " Topics: ", K, " ###")
    
    docs = readRDS(file.path("data", i, "docs.rds"))
    vocab = readRDS(file.path("data", i, "vocab.rds"))
    
    if(ncpus > 1){
      lda = LDARep(docs, vocab, K = K, pm.backend = "socket", ncpus = ncpus)
    }else{
      lda = LDARep(docs, vocab, K = K)
    }
    saveRDS(lda, file.path("data", i, "lda", paste0(K, ".rds")))
    
    gc(verbose = TRUE, reset = TRUE)
    
    time = as.numeric(difftime(Sys.time(), starttime, units = "hours"))
    message(round(time, 2), " hours")
    
    return(time)
  })

addExperiments()

ids = getJobTable()[, .(job.id, problem)]
ids[, K := as.integer(gsub("[A-Z]", "", problem))]
ids[, i := gsub("[0-9]", "", problem)]

ids[i == "AT", walltime := 2*60*60]
ids[i == "AT", memory := 32*1024]
ids[i == "AT", ncpus := 4]

ids[i == "CH" & K < 41, walltime := 2*60*60]
ids[i == "CH" & K < 41, ncpus := 4]
ids[i == "CH" & K > 41, walltime := 8*60*60]
ids[i == "CH" & K > 41, ncpus := 2]
ids[i == "CH", memory := 32*1024]

ids[i == "DK" & K < 46, walltime := 8*60*60]
ids[i == "DK" & K < 46, ncpus := 2]
ids[i == "DK" & K > 46, walltime := 48*60*60]
ids[i == "DK" & K > 46, ncpus := 1]
ids[i == "DK", memory := 32*1024]

ids[i == "ESP" & K < 56, walltime := 48*60*60]
ids[i == "ESP" & K < 56, ncpus := 1]
ids[i == "ESP" & K < 56, memory := 32*1024]
ids[i == "ESP" & K > 56, walltime := 48*60*60]
ids[i == "ESP" & K > 56, ncpus := 2]
ids[i == "ESP" & K > 56, memory := 64*1024]

ids[i == "FR" & K < 41, walltime := 48*60*60]
ids[i == "FR" & K < 41, ncpus := 1]
ids[i == "FR" & K < 41, memory := 32*1024]
ids[i == "FR" & K > 41, walltime := 48*60*60]
ids[i == "FR" & K > 41, ncpus := 2]
ids[i == "FR" & K > 41, memory := 64*1024]

ids[i %in% c("GER", "UK") & K < 41, walltime := 48*60*60]
ids[i %in% c("GER", "UK") & K < 41, ncpus := 2]
ids[i %in% c("GER", "UK") & K < 41, memory := 64*1024]
ids[i %in% c("GER", "UK") & K > 41, walltime := 7*24*60*60]
ids[i %in% c("GER", "UK") & K > 41, ncpus := 1]
ids[i %in% c("GER", "UK") & K > 41, memory := 40*1024]

ids[i %in% c("IT", "NL"), walltime := 48*60*60]
ids[i %in% c("IT", "NL"), memory := 32*1024]
ids[i %in% c("IT", "NL"), ncpus := 1]

ids[, problem := NULL]
ids[, K := NULL]
ids[, i := NULL]

submitJobs(ids)

library(ldaPrototype)
library(tosca)
library(data.table)
library(ggplot2)
library(ineq)
library(utf8)
library(cividis)
library(ggrepel)
library(ggpubr)

countries = c("AT", "CH", "DK", "ESP", "FR", "GER", "IT", "NL")
countries_long = c("Austria", "Switzerland", "Denmark", "Spain", "France", "Germany", "Italy", "Netherlands")
names(countries_long) = countries

pop.parties = c("VOX", "FPÖ", "SVP", "DF", "RN", "AfD", "LN", "PVV")
#pop.parties = PRRPs
int.parties = c("FPÖ", "SVP", "DF", "VOX", "LN")
alt.parties = c("RN", "AfD", "PVV")

pdf(file.path("data", "plots.pdf"), width = 12, height = 6.5)

for(i in countries){
  #dir.create(file.path("data", i, "tables"))
  
  ### Read RDS Files
  raw = readRDS(file.path("data", i, "raw.rds"))
  url = readRDS(file.path("data", i, "urlExpanded.rds"))
  clean = readRDS(file.path("data", i, "cleanprep.rds"))
  ids = names(readRDS(file.path("data", i, "docs.rds")))
  clean = filterID(clean, ids)
  party_match_lda = clean$meta$party_short[match(ids, clean$meta$id)]
  url_match_lda = clean$meta$url_core[match(ids, clean$meta$id)]
  protos = lapply(list.files(file.path("data", i, "proto")), function(x)
    readRDS(file.path("data", i, "proto", x)))
  names(protos) = sapply(protos, function(x) getK(getLDA(x)))
  
  ### Read CSV Files
  ## Party Colors
  party_col = fread(file.path("data", i, "parties_col.csv"), encoding = "UTF-8")
  ## Politicians
  polit = merge(fread(file.path("data", i, "politicians.csv"),
                      na.strings = "", encoding = "UTF-8"),
                party_col, by = "party_short", all.x = TRUE)
  polit[, screen_name :=  gsub("@", "", twitter_handle)]
  polit[is.na(col), party_short := "Misc."]
  party_tab = polit[, .(.N,
                        col = unique(col)), by = party_short]
  setkey(party_tab, N)
  levels = c("Misc.", setdiff(party_tab$party_short, "Misc."))
  polit[, party_short := factor(party_short, levels = levels)]
  
  ## Politicians in LDA
  polit_lda = as.data.table(clean$meta)
  polit_lda[, user_nobs := .N, by = name.y]
  polit_lda = merge(
    unique(polit_lda[, c("name.y", "party_short", "party_full", "party_grouping",
                         "account_created_at", "verified", "followers_count",
                         "friends_count", "statuses_count", "user_nobs")]),
    party_col, by = "party_short", all.x = TRUE)
  polit_lda[, N := .N, by = party_short]
  polit_lda[is.na(col), party_short := "Misc."]
  party_tab_lda = polit_lda[, .(.N,
                                col = unique(col),
                                date = as.Date(max(account_created_at))), by = party_short]
  setkey(party_tab_lda, N)
  levels_lda = party_tab_lda$party_short
  polit_lda[, party_short := factor(party_short, levels = levels_lda)]
  
  ### Data Manipulation
  raw = merge(raw, polit[!is.na(screen_name)], by = "screen_name")
  url = merge(url, polit[!is.na(screen_name)], by = "screen_name")
  url[, url_core := as.factor(url_core)]
  url[, party_short := droplevels(party_short)]
  n_party = uniqueN(url$party_short)
  size = sapply(
    split(lengths(clean$text),
          clean$meta$party_short[match(names(clean$text), clean$meta$id)]),
    sum)
  
  ### EDA
  ## Twitter Accounts per Party
  print(ggplot(polit_lda, aes(account_created_at, color = party_short)) +
          geom_step(aes(N = N, y = ..y.. * N), stat = "ecdf") +
          scale_color_manual("Party",
                             breaks = rev(levels_lda),
                             values = rev(party_tab_lda[match(levels_lda, party_short), col])) +
          xlab("") + ylab("Number of Twitter Accounts") +
          xlim(as.POSIXct(c("2007-06-01", "2020-07-15"), tz = "UTC")) +
          labs(title = countries_long[i]))
  
  ## Stacked Barplots (seats, accounts, tweets, urls, texts) per Party
  seats_bar = data.table(stat = "seats",
                         party_short = polit$party_short)
  accounts_bar = data.table(stat = "accounts",
                            party_short = polit[!is.na(twitter_handle), party_short])
  tweets_bar = data.table(stat = "tweets",
                          party_short = raw$party_short)
  urls_bar = data.table(stat = "URLs",
                        party_short = url$party_short)
  texts_bar = data.table(stat = "texts",
                         party_short = factor(clean$meta$party_short,
                                              levels = levels))
  dt_bar = rbind(seats_bar, accounts_bar, tweets_bar, urls_bar, texts_bar)
  dt_bar[, stat := factor(stat, rev(c("seats", "accounts", "tweets", "URLs", "texts")))]
  dt_bar_flat = dt_bar[, .N, by = c("stat", "party_short")]
  dt_bar_flat[, rel := N/sum(N), by = "stat"]
  fwrite(dt_bar_flat, file.path("data", i, "tables", "eda_seats_accounts_tweets_urls_texts.csv"))
  
  print(ggplot(dt_bar_flat, aes(y = stat, x = rel, fill = party_short)) +
          geom_bar(position = "stack", stat = "identity") +
          scale_color_manual("Party",
                             breaks = rev(levels),
                             values = rev(party_tab[match(levels, party_short), col]),
                             aesthetics = "fill") +
          xlab("Share") + ylab("")  +
          scale_y_discrete(
            labels = c("seats" = paste0("seats\n(", dt_bar[stat == "seats", .N], ")"),
                       "accounts" = paste0("accounts\n(", dt_bar[stat == "accounts", .N], ")"),
                       "tweets" = paste0("tweets\n(", dt_bar[stat == "tweets", .N], ")"),
                       "URLs" = paste0("URLs\n(", dt_bar[stat == "URLs", .N], ")"),
                       "texts" = paste0("texts\n(", dt_bar[stat == "texts", .N], ")"))) +
          labs(title = countries_long[i]))
  
  ### Analysis
  ## Defining Variables
  # P1 as Matrix per Topic and Party 
  gini_party_lda_list = lapply(names(protos), function(K){
    a = as.data.table(t(getDocument_sums(getLDA(protos[[K]]))))
    a = a/rowSums(a) # Normalization per Text
    a[, party_short := party_match_lda]
    b = a[, lapply(.SD, sum), by = party_short]
    tmp = b$party_short
    b[, party_short := NULL]
    b = as.data.table(t(b))
    colnames(b) = tmp
    
    rel1 = b[, lapply(.SD, function(x) x/sum(x))]
    rel2 = rel1/rowSums(rel1)
    
    l = as.matrix(rel2)
    l[l > 1/n_party] = 0.5 + (l[l > 1/n_party] - 1/n_party) / (n_party-1) / 2 * n_party
    l[l <= 1/n_party] = l[l <= 1/n_party] / 2 * n_party
    rel2 = as.data.table(l)
    
    gini = rel2[, lapply(.SD, Gini, corr = TRUE)]
    
    dt = data.table(K = as.integer(K), party_short = as_utf8(names(gini)), gini = as.numeric(gini))
    dt = cbind(dt, t(rel2)[dt$party_short,])
    colnames(dt)[(ncol(dt)-as.integer(K)+1):ncol(dt)] = paste0("T", seq_len(as.integer(K)), ".",
                                                               topWords(getTopics(getLDA(protos[[K]]))))
    dt
  })
  names(gini_party_lda_list) = sapply(protos, function(x) getK(getLDA(x)))
  # P1) Topic Insularity per Party
  gini_party_lda = rbindlist(lapply(gini_party_lda_list, function(x) x[,1:3]))
  fwrite(gini_party_lda, file.path("data", i, "tables", "P1_gini_party_lda.csv"))
  # P1 as Matrix per Topic and Party (for one specific K)
  K = "30"
  melted = melt(gini_party_lda_list[[K]][,-c("K", "gini")],
                id.vars = "party_short", variable.name = "topic", value.name = "Score")
  fwrite(melted, file.path("data", i, "tables", "attention.csv"))
  
  # Helper table for:
  # P2) Media/Source Insularity per Party,
  # P3) Media/Source Diversity per Party,
  # M1) Party Insularity per Media/Source
  url_core_party = url[, table(party_short), by = url_core]
  url_core_party[, party_short := rep(levels(url$party_short), length.out = .N)]
  url_core_party[, N_url := sum(V1), by = url_core]
  url_core_party[, N := V1]
  url_core_party[, V1 := NULL]
  url_core_party[, rel_party := N/sum(N), by = party_short]
  url_core_party[, rel_core := rel_party/sum(rel_party), by = url_core]
  url_core_party[rel_core > 1/n_party,
                 rel_core_centered := 0.5 + (rel_core - 1/n_party) / (n_party-1) / 2 * n_party , by = url_core]
  url_core_party[rel_core <= 1/n_party,
                 rel_core_centered := rel_core / 2 * n_party , by = url_core]
  fwrite(url_core_party, file.path("data", i, "tables", "P2_P3_M1_url_core_party.csv"))
  
  gini_party = url_core_party[N_url > 50, .( # take all URLs into account that are shared more than 50 times
    gini_abs = Gini(N, corr = TRUE), # falsified/misleading
    gini_norm = Gini(rel_core, corr = TRUE),
    gini_centered = Gini(rel_core_centered, corr = TRUE), # P2) Media/Source Insularity per Party,
    N_urls = sum(N > 0),
    rel_urls = mean(N >0)), by = party_short] # P3) Media/Source Diversity per Party,
  gini_party = merge(gini_party, data.table(party_short = names(size), size = as.numeric(size)))
  fwrite(gini_party, file.path("data", i, "tables", "P2_P3_gini_party.csv"))
  
  # M1) Party Insularity per Media/Source
  gini_url = url_core_party[, .(gini_centered = Gini(rel_core, corr = TRUE),
                                max_party = party_short[which.max(rel_core_centered)],
                                rel_party = rel_party[which.max(rel_core_centered)],
                                N_party = N[which.max(rel_core_centered)],
                                N_url = N_url[which.max(rel_core_centered)]),
                            by = url_core]
  fwrite(gini_url, file.path("data", i, "tables", "M1_gini_url.csv"))
  
  # M2 as Matrix per Topic and Media
  gini_url_lda_list = lapply(names(protos), function(K){
    a = as.data.table(t(getDocument_sums(getLDA(protos[[K]]))))
    a = a/rowSums(a) # Normalization per Text
    a[, url_core := url_match_lda]
    b = a[, lapply(.SD, sum), by = url_core]
    tmp = b$url_core
    b[, url_core := NULL]
    N_url_core = rowSums(b)
    b = as.data.table(t(b))
    colnames(b) = tmp
    
    rel1 = b[, lapply(.SD, function(x) x/sum(x))]
    rel2 = rel1/rowSums(rel1)
    
    l = as.matrix(rel2)
    l[l > 1/ncol(rel2)] = 0.5 + (l[l > 1/ncol(rel2)] - 1/ncol(rel2)) / (ncol(rel2)-1) / 2 * ncol(rel2)
    l[l <= 1/ncol(rel2)] = l[l <= 1/ncol(rel2)] / 2 * ncol(rel2)
    rel2 = as.data.table(l)
    
    gini = rel2[, lapply(.SD, Gini, corr = TRUE)]
    
    dt = data.table(K = as.integer(K), url_core = names(gini), gini = as.numeric(gini), N_url = N_url_core)
    dt = cbind(dt, t(rel2)[dt$url_core,])
    colnames(dt)[(ncol(dt)-as.integer(K)+1):ncol(dt)] = paste0("T", seq_len(as.integer(K)), ".",
                                                               topWords(getTopics(getLDA(protos[[K]]))))
    dt
  })
  names(gini_url_lda_list) = sapply(protos, function(x) getK(getLDA(x)))
  # M2) Topic Insularity per Media/Source
  gini_url_lda = rbindlist(lapply(gini_url_lda_list, function(x) x[,1:4]))
  fwrite(gini_url_lda, file.path("data", i, "tables", "M2_gini_url_lda.csv"))
  
  ## Plots
  # Topic Insularity per Party and Number of Topics
  levs = gini_party_lda[K == max(K), party_short[order(gini, decreasing = TRUE)]]
  print(ggplot(gini_party_lda, aes(x = K, y = gini, group = party_short, color = party_short)) +
          geom_line() + geom_point() +
          ylab("Topic Insularity") +
          scale_color_manual("Party",
                             breaks = levs,
                             values = party_tab_lda[match(levs, party_short), col]) +
          labs(title = countries_long[i]))
  
  # Topic Insularity per Party and Party "Size"
  gini_party_lda_size = merge(gini_party_lda, data.table(party_short = names(size), size = as.numeric(size)))
  z = merge(gini_party_lda_size[, .(y = max(gini), x = unique(size)), by = party_short],
            party_tab_lda, "party_short")
  print(ggplot(gini_party_lda_size, aes(x = size, y = gini, color = party_short)) +
          geom_point() +
          scale_color_manual("Party",
                             breaks = levs,
                             values = party_tab_lda[match(levs, party_short), col]) +
          geom_label_repel(data = gini_party_lda_size[, .(gini = max(gini), size = unique(size)), by = party_short],
                           aes(x = size, y = gini, color = party_short, label = party_short),
                           fill = "lightgrey",
                           box.padding = 1) +
          ylim(c(min(gini_party_lda_size$gini)*0.9, max(gini_party_lda_size$gini)*1.1)) +
          xlab("Number of Assignments") + ylab("Topic Insularity") + theme(legend.position = "none") +
          labs(title = countries_long[i]))
  
  # Topic Insularity, Media Insularity per Media
  o = gini_url_lda[, .(gini = mean(gini), N_url = unique(round(N_url))), by = url_core]
  setkey(o, N_url)
  o = merge(tail(o, 35), gini_url, "url_core")
  o[gini_centered < 0.75, max_party := "other"]
  print(ggplot(o, aes(x = gini, y = gini_centered, label = url_core, size = log(N_url.x), color = max_party)) +
          geom_point() +
          geom_label_repel(fill = "lightgrey") + geom_point() +
          xlab("Topic Insularity") + ylab("Media Insularity") +
          scale_color_manual(breaks = c(levs, "other", "Misc."),
                             values = c(party_tab_lda[match(levs, party_short), col], "grey45", "grey45")) +
          theme(legend.position = "none") + scale_size(range = c(2,5)) +
          labs(title = countries_long[i]))
  
  # Topic Insularity, Media Insularity and Size per Party
  m = merge(
    merge(gini_party, gini_party_lda[, .(gini = mean(gini)), by = party_short], "party_short"),
    party_col)
  print(ggplot(m, aes(x = gini, y = gini_centered, color = party_short, label = party_short, size = size)) +
          geom_point() +
          geom_label_repel(fill = "lightgrey") + geom_point() +
          xlab("Topic Insularity") + ylab("Media Insularity") +
          scale_color_manual("Party",
                             breaks = levs,
                             values = party_tab_lda[match(levs, party_short), col]) +
          theme(legend.position = "none") +
          labs(title = countries_long[i]))
  print(ggplot(gini_party, aes(x = rel_urls, y = gini_centered, color = party_short, label = party_short, size = size)) +
          geom_point() +
          geom_label_repel(fill = "lightgrey") + geom_point() +
          xlab("Share of URLs") + ylab("Media Insularity") +
          scale_color_manual("Party",
                             breaks = levs,
                             values = party_tab_lda[match(levs, party_short), col]) +
          theme(legend.position = "none") +
          labs(title = countries_long[i]))
  
  # Heatmap of Topic Attention per Party
  print(ggplot(melted, aes(x = topic, y = party_short, fill = Score)) + geom_tile() +
          xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_gradientn(
            colors = cividis(500),
            values = unique(c(seq(0, 0.5, length.out = 251),
                              seq(0.5, 0.7, length.out = 171),
                              seq(0.7, 1, length.out = 80))))+
          labs(fill = "Attention\nScore") +
          scale_y_discrete(labels = paste0(sort(unique(melted$party_short)), "\n(",
                                           round(gini_party_lda[K == as.integer(.GlobalEnv$K), gini[order(party_short)]], 3), ")")) +
          labs(title = countries_long[i]))
  
  # Principal Component Analysis
  setkey(melted, party_short, topic)
  mat = do.call(rbind, split(melted$Score, melted$party_short))
  pca = as.data.frame(prcomp(mat)$x)
  pca$party_short = rownames(pca)
  pca = as.data.table(pca)
  
  print(ggplot(pca, aes(x = PC1, y = PC2, color = party_short, label = party_short)) +
          geom_point() +
          geom_label_repel(fill = "lightgrey") + geom_point() +
          scale_color_manual("Party",
                             breaks = levs,
                             values = party_tab_lda[match(levs, party_short), col]) +
          theme(legend.position = "none") +
          labs(title = countries_long[i]))
  
  ### Top Words K = 30
  lda = getLDA(protos[[K]])
  topwords = topWords(getTopics(getLDA(protos[[K]])), 50)
  prop = round(rowSums(getTopics(lda)) / sum(getTopics(lda)) * 100, 4)
  out = rbind(prop, topwords)
  colnames(out) = paste("Topic", seq_len(as.integer(K)))
  row.names(out) = c("Proportion (%)", 1:50)
  write.csv(out, file.path("data", i, paste0("topwords", K, ".csv")))
}

### Country Variables
# L1) Media Trust
# L2) Polarisation
extern = fread(file.path("data", "extern.csv"), dec = ",")
extern$Vote_Share_Timbro_2020 = as.numeric(gsub(",", ".", extern$Vote_Share_Timbro_2020))
extern[, Country := countries_long[country]]

# L3) Topic Insularity
gini_party_lda = rbindlist(lapply(countries, function(x){
  tab = fread(file.path("data", x, "tables", "P1_gini_party_lda.csv"), encoding = "UTF-8")
  tab[, Country := countries_long[x]]
}))

# L4) Media/Source Insularity
gini_party = rbindlist(lapply(countries, function(x){
  tab = fread(file.path("data", x, "tables", "P2_P3_gini_party.csv"), encoding = "UTF-8")
  tab[, Country := countries_long[x]]
}))

# L5) Party Insularity per Media/Source
gini_url = rbindlist(lapply(countries, function(x){
  tab = fread(file.path("data", x, "tables", "M1_gini_url.csv"), encoding = "UTF-8")
  tab[, Country := countries_long[x]]
}))

# L6) Topic Insularity per Media
gini_url_lda = rbindlist(lapply(countries, function(x){
  tab = fread(file.path("data", x, "tables", "M2_gini_url_lda.csv"), encoding = "UTF-8")
  tab[, Country := countries_long[x]]
}))

# L7) Topic Diversity
diversity = rbindlist(lapply(countries, function(x){
  protos = lapply(list.files(file.path("data", x, "proto")), function(file)
    readRDS(file.path("data", x, "proto", file)))
  names(protos) = sapply(protos, function(x) getK(getLDA(x)))
  data.table(
    Country = countries_long[x],
    K = as.integer(names(protos)),
    gini = sapply(protos, function(x) Gini(rowSums(getDocument_sums(getLDA(x))))))
}))

# Party Colors
party_col = rbindlist(lapply(countries, function(x){
  dt = fread(file.path("data", x, "parties_col.csv"), encoding = "UTF-8")
  dt[, Country := countries_long[x]]
}))
party_col[, party_Country := paste0(party_short, Country)]


####
print(ggplot(gini_party_lda[, .(gini = mean(gini)), by = c("party_short", "Country")],
             aes(y = gini, x = Country, fill = Country)) +
        geom_violin() + geom_point() +
        ylab("Topic Insularities per Party"))

print(ggplot(gini_party, aes(y = gini_centered, x = Country, fill = Country)) +
        geom_violin() + geom_point() +
        ylab("Media Insularities per Party"))

abc = gini_url_lda[, .(mean = mean(gini), sd = sd(gini)), by = Country]
abc[, ymin := mean - sd]
abc[, ymax := mean + sd]

gini_url_lda[, .(mean = mean(gini), sd = sd(gini)), by = Country]
print(ggplot() +
        geom_violin(data = gini_url_lda, aes(y = gini, x = Country, fill = Country)) +
        ylab("Topic Insularity per Media") + 
        geom_errorbar(data = abc, aes(x = Country, ymin = ymin, ymax = ymax)) +
        geom_point(data = abc, aes(x = Country, y = mean)))

print(ggplot(diversity, aes(x = K, y = gini, color = Country)) + geom_line() +
        ylab("Topic Diversity"))
print(ggplot(diversity, aes(x = Country, y = gini, fill = Country)) + geom_violin() +
        xlab("") + ylab("Topic Diversity"))


u = merge(gini_party_lda[, .(gini = mean(gini)), by = c("party_short", "Country")],
          gini_party, by = c("party_short", "Country"))
print(ggplot(u, aes(x = gini, y = gini_centered, color = Country, label = party_short)) +
        geom_point() +
        geom_label_repel(fill = "lightgrey") + geom_point() +
        xlab("Topic Insularity") + ylab("Media Insularity") +
        theme(legend.position = "none"))
print(ggplot(u[party_short %in% c("VOX", "FPÖ", "SVP", "DF", "RN", "AfD", "LN", "M5S", "PVV")],
             aes(x = gini, y = gini_centered, label = party_short, color = Country)) +
        geom_point() +
        geom_label_repel(fill = "lightgrey") + geom_point() +
        xlab("Topic Insularity") + ylab("Media Insularity") +
        theme(legend.position = "none"))
print(ggplot(u, aes(x = gini, y = gini_centered, color = Country, label = party_short)) +
        geom_point() +
        geom_label_repel(fill = "lightgrey") + geom_point() +
        xlab("Topic Insularity") + ylab("Media Insularity") +
        theme(legend.position = "none") + 
        facet_wrap(~Country)) 

u[, size_rel := size/sum(size), by = Country]

tmp = copy(u)
tmp[, source := gini_centered]
tmp[, source_country := mean(source), by = Country]
tmp[, source_rel := source - source_country]
tmp[, topic := gini]
tmp[, topic_country := mean(topic), by = Country]
tmp[, topic_rel := topic - topic_country]

tmp = tmp[, c("Country", "source_country", "topic_country",
              "party_short", "source", "source_rel", "topic", "topic_rel",
              "N_urls", "rel_urls", "size", "size_rel")]
setkeyv(tmp, c("Country", "party_short"))

fwrite(tmp, file.path("data", "stats.csv"))

p = u[, .(topic = gini-mean(gini), media = gini_centered-mean(gini_centered),
          party_short = party_short), by = Country]
w = merge(p, u, c("party_short", "Country"))
w[, party_Country := paste0(party_short, Country)]

print(ggplot(w,
             aes(x = topic, y = media, label = party_short, color = party_Country,
                 size = size_rel)) +
        geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        geom_point(key_glyph = "rect") +
        geom_label_repel(fill = "lightgrey", key_glyph = "point") +
        geom_point() +
        scale_size(range = c(1,4)) +
        labs(size = "Share of\nAssignments") +
        xlab("Deviation from Mean Topic Insularity") +
        ylab("Deviation from Mean Media Insularity") +
        facet_wrap(~Country) + theme(legend.position = "none") + 
        scale_color_manual(breaks = party_col$party_Country,
                           values = party_col$col))

print(ggplot(w[size_rel > 0.095],
             aes(x = topic, y = media, label = party_short, color = Country,
                 size = size_rel, fill = Country)) +
        geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        geom_point(key_glyph = "rect") +
        geom_label_repel(fill = "lightgrey", key_glyph = "point") +  geom_point() +
        scale_size(range = c(2,6)) +
        labs(size = "Share of\nAssignments\nper Country") +
        xlab("Deviation from Mean Topic Insularity per Country") +
        ylab("Deviation from Mean Source Insularity per Country"))

w[, shape := "Others"]
w[party_short %in% int.parties, shape := "Neg.-integr. RWP"]
w[party_short %in% alt.parties, shape := "Non-integr. RWP"]
w[, shape := factor(shape, levels = c("Others", "Neg.-integr. RWP", "Non-integr. RWP"))]

print(ggplot(w[size_rel > 0.095],
             aes(x = topic, y = media, label = party_short, color = Country,
                 size = size_rel, fill = Country, shape = shape)) +
        geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
        geom_point(key_glyph = "rect") +
        geom_label_repel(fill = "lightgrey", key_glyph = "point") +  geom_point() +
        scale_size(range = c(2,6)) +
        labs(size = "Share of\nAssignments\nper Country",
             shape = "Integration") +
        xlab("Deviation from Mean Topic Insularity per Country") +
        ylab("Deviation from Mean Source Insularity per Country"))

g = melt(extern)

print(ggplot(g, aes(x = country, y = value, color = Country, label = Country)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("") +
        theme(legend.position = "none"))

f = merge(u[, .(topic = mean(gini), media = mean(gini_centered)), by = Country],
          g, by = "Country")

print(ggplot(f, aes(x = topic, y = value, color = Country, label = Country)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Mean Topic Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))
print(ggplot(f, aes(x = media, y = value, color = Country, label = Country)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Mean Media Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))

f.party = merge(w[party_short %in% pop.parties], g, by = "Country", allow.cartesian = TRUE)

print(ggplot(f[, .(topic = unique(topic), media = unique(media)), by = "Country"],
             aes(x = topic, y = media, color = Country, label = Country)) +
        geom_label_repel() + geom_point() +
        xlab("Mean Topic Insularity per Party") + ylab("Mean Media Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 0.5, label.y.npc = 1, hjust = 0.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 0.5, label.y.npc = 0.9, hjust = 0.5))
print(ggplot(w,
             aes(x = gini, y = gini_centered, color = Country, label = party_short)) +
        geom_label_repel() + geom_point() +
        xlab("Topic Insularity per Party") + ylab("Media Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 0, label.y.npc = 1, hjust = 0) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 0, label.y.npc = 0.9, hjust = 0))
print(ggplot(w,
             aes(x = topic, y = media, color = Country, label = party_short)) +
        geom_label_repel() + geom_point() +
        xlab("Deviation from Mean Topic Insularity per Country") + ylab("Deviation from Mean Media Insularity per Country") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 0, label.y.npc = 1, hjust = 0) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 0, label.y.npc = 0.9, hjust = 0))
print(ggplot(f.party[, .(gini = unique(gini), gini_centered = unique(gini_centered), Country = unique(Country)), by = "party_short"],
             aes(x = gini, y = gini_centered, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        xlab("Topic Insularity per Party") + ylab("Media Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 0, label.y.npc = 1, hjust = 0) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 0, label.y.npc = 0.9, hjust = 0))
print(ggplot(f.party[, .(topic = unique(topic), media = unique(media), Country = unique(Country)), by = "party_short"],
             aes(x = topic, y = media, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        xlab("Deviation from Mean Topic Insularity per Country") + ylab("Deviation from Mean Media Insularity per Country") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 0, label.y.npc = 1, hjust = 0) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 0, label.y.npc = 0.9, hjust = 0))

print(ggplot(f.party, aes(x = gini, y = value, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Topic Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))
print(ggplot(f.party, aes(x = gini_centered, y = value, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Media Insularity per Party") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))

print(ggplot(f.party, aes(x = topic, y = value, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Deviation from Mean Topic Insularity per Country") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))
print(ggplot(f.party, aes(x = media, y = value, color = Country, label = party_short)) +
        geom_label_repel(size = 2.3) + geom_point() +
        facet_wrap(~variable, scales = "free") + xlab("Deviation from Mean Media Insularity per Country") +
        theme(legend.position = "none") +
        stat_cor(aes(label = paste0("Pearson: ", ..r.., "(", ..p..,")")), color = "black", method = "pearson",
                 label.x.npc = 1, label.y.npc = 1, hjust = 1, size = 2.5) +
        stat_cor(aes(label = paste0("Spearman: ", ..r.., "(", ..p..,")")), color = "black", method = "spearman",
                 label.x.npc = 1, label.y.npc = 0.9, hjust = 1, size = 2.5))

## Correlation 
# Media Insularity
cor1 = merge(merge(f.party[, .(
  pearson = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
  spearman = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
  by = variable],
  f.party[!Country %in% c("Germany", "Netherlands"), .(
    pearson.wo.GERNL = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNL = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
    by = variable]),
  f.party[!Country %in% c("Germany", "Netherlands", "Spain"), .(
    pearson.wo.GERNLESP = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNLESP = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
    by = variable])
cor1$type = "media"

# Deviation Media Insularity
cor2 = merge(merge(f.party[, .(
  pearson = cor(value, media, method = "pearson", use = "complete.obs"),
  spearman = cor(value, media, method = "spearman", use = "complete.obs")),
  by = variable],
  f.party[!Country %in% c("Germany", "Netherlands"), .(
    pearson.wo.GERNL = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNL = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
    by = variable]),
  f.party[!Country %in% c("Germany", "Netherlands", "Spain"), .(
    pearson.wo.GERNLESP = cor(value, media, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNLESP = cor(value, media, method = "spearman", use = "complete.obs")),
    by = variable])
cor2$type = "deviation media"

# Topic Insularity
cor3 = merge(merge(f.party[, .(
  pearson = cor(value, gini, method = "pearson", use = "complete.obs"),
  spearman = cor(value, gini, method = "spearman", use = "complete.obs")),
  by = variable],
  f.party[!Country %in% c("Germany", "Netherlands"), .(
    pearson.wo.GERNL = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNL = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
    by = variable]),
  f.party[!Country %in% c("Germany", "Netherlands", "Spain"), .(
    pearson.wo.GERNLESP = cor(value, gini, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNLESP = cor(value, gini, method = "spearman", use = "complete.obs")),
    by = variable])
cor3$type = "topic"

# Deviation Topic Insularity
cor4 = merge(merge(f.party[, .(
  pearson = cor(value, topic, method = "pearson", use = "complete.obs"),
  spearman = cor(value, topic, method = "spearman", use = "complete.obs")),
  by = variable],
  f.party[!Country %in% c("Germany", "Netherlands"), .(
    pearson.wo.GERNL = cor(value, gini_centered, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNL = cor(value, gini_centered, method = "spearman", use = "complete.obs")),
    by = variable]),
  f.party[!Country %in% c("Germany", "Netherlands", "Spain"), .(
    pearson.wo.GERNLESP = cor(value, topic, method = "pearson", use = "complete.obs"),
    spearman.wo.GERNLESP = cor(value, topic, method = "spearman", use = "complete.obs")),
    by = variable])
cor4$type = "deviation topic"

cor = rbind(cor1, cor2, cor3, cor4)

cor[, pearson.diff.GERNL := pearson - pearson.wo.GERNL]
cor[, spearman.diff.GERNL := spearman - spearman.wo.GERNL]
cor[, pearson.diff.GERNLESP := pearson - pearson.wo.GERNLESP]
cor[, spearman.diff.GERNLESP := spearman - spearman.wo.GERNLESP]


fwrite(cor, file.path("data", "cor.csv"))

dev.off()

# Correlations...
k = u[, .(pearson = cor(gini, gini_centered, method = "pearson"), spearman = cor(gini, gini_centered, method = "spearman")), by = Country]

# Party Top URLs
url_party = rbindlist(lapply(countries, function(x){
  tab = fread(file.path("data", x, "tables", "P2_P3_M1_url_core_party.csv"), encoding = "UTF-8")
  tab[, Country := countries_long[x]]
}))

# Top Media per Party
url_party[, insularity_media := Gini(rel_core_centered, corr = TRUE), by = c("url_core", "Country")]
setorderv(url_party, "N", -1)
for(party in pop.parties){
  d = head(url_party[party_short == party,], 100)
  d[, insularity_party := rel_core_centered]
  d[, c("url_core", "N_url", "N", "rel_party", "insularity_party", "insularity_media")]
  fwrite(d[, c("url_core", "N_url", "N", "rel_party", "insularity_party", "insularity_media")],
         file.path("data", "party_media", paste0(party, ".csv")))
}

# Top insulariest Media per Country
q = gini_url[N_url > 50, ]
q[, insularity_media := gini_centered]
setorderv(q, c("insularity_media", "N_url"), -1)
for(country in unique(q$Country)){
  fwrite(q[Country == country, c("url_core", "insularity_media", "N_url", "max_party", "N_party", "rel_party")],
         file.path("data", "media", paste0(country, ".csv")))
}

library(quanteda)
library(data.table)
library(readtext)
library(dplyr)
file_names = list.files("./files",pattern = ".txt")
profanity = read.csv("profanity.txt",header = F,stringsAsFactors = F)
profanity = profanity$V1
load("final.Rdata")
for(i in 3:11){
        #load("main.Rdata")
        file_names = list.files("./sorted_data_acl/books",pattern = ".txt")
        #profanity = read.csv("profanity.txt",header = F,stringsAsFactors = F)
        #profanity = profanity$V1
        data = readtext(paste("./sorted_data_acl/books/",file_names[i],sep = ""))
        data = gsub("<|>|/","",data)
        data = gsub("(f|ht)tp(s?)"," ",data)
        data = gsub("@[^\\s]+"," ",data)
        data = gsub('[^ -~]', '', data)
        data = gsub('-|:|;|_', ' ', data)
        data = gsub('#', '', data)
        corpus_main = corpus(data)
        tokens1 = tokens(corpus_main,remove_punct = T,remove_numbers = T,remove_url = T,what = "word",remove_symbols = T)
        tokens1 = tokens_remove(tokens1,pattern = profanity)
        tok_grams2 = tokens_ngrams(tokens1,n = 2)
        tok_grams3 = tokens_ngrams(tokens1,n = 3)
        tok_grams4 = tokens_ngrams(tokens1,n = 4)
        tok_grams5 = tokens_ngrams(tokens1,n = 5)
        df1 = dfm(tokens1,tolower=T)
        df1 = dfm_trim (df1,min_termfreq = 2)
        df2 = dfm(tok_grams2,tolower=T)
        df2 = dfm_trim (df2,min_termfreq = 2)
        df3 = dfm(tok_grams3,tolower=T)
        df3 = dfm_trim (df3,min_termfreq = 2)
        df4 = dfm(tok_grams4,tolower=T)
        df4 = dfm_trim (df4,min_termfreq = 2)
        df5 = dfm(tok_grams5,tolower=T)
        df5 = dfm_trim (df5,min_termfreq = 2)
        #df1 = dfm_sort(df,decreasing = T,margin = "features")
        df1 = textstat_frequency(df1)
        dt1 = as.data.table(df1)
        dt1 = dt1[,1:2]
        names(dt1)[2] = file_names[i]
        df2 = textstat_frequency(df2)
        dt2 = as.data.table(df2)
        dt2 = dt2[,1:2]
        names(dt2)[2] = file_names[i]
        df3 = textstat_frequency(df3)
        dt3 = as.data.table(df3)
        dt3 = dt3[,1:2]
        names(dt3)[2] = file_names[i]
        df4 = textstat_frequency(df4)
        dt4 = as.data.table(df4)
        dt4 = dt4[,1:2]
        names(dt4)[2] = file_names[i]
        df5 = textstat_frequency(df5)
        dt5 = as.data.table(df5)
        dt5 = dt5[,1:2]
        names(dt5)[2] = file_names[i]
        setkey(dt1,feature)
        setkey(dt2,feature)
        setkey(dt3,feature)
        setkey(dt4,feature)
        setkey(dt5,feature)
        setkey(dt_bii,feature)
        setkey(dt_trii,feature)
        setkey(dt_quadd,feature)
        setkey(dt_pentt,feature)
        setkey(dt_unii,feature)
        dt_unii = merge(dt_unii,dt1,all = T)
        dt_bii = merge(dt_bii,dt2,all = T)
        dt_trii = merge(dt_trii,dt3,all = T)
        dt_quadd = merge(dt_quadd,dt4,all = T)
        dt_pentt = merge(dt_pentt,dt5,all = T)
        #save(dt_uni,dt_bi,dt_tri,dt_quad,dt_pent,file = "main.Rdata")
        #rm(list = ls())
        #gc()
}

dt_unii[is.na(dt_unii)] = 0
dt_bii[is.na(dt_bii)] = 0
dt_trii[is.na(dt_trii)] = 0
dt_quadd[is.na(dt_quadd)] = 0
dt_pentt[is.na(dt_pentt)] = 0
dt_uni$freq = rowSums(dt_uni[,2:12])
dt_uni = dt_uni[,-c(2:12)]
dt_uni = arrange(dt_uni,desc(freq))
dt_bi$freq = rowSums(dt_bi[,2:12])
dt_bi = dt_bi[,-c(2:12)]
dt_bi = arrange(dt_bi,desc(freq))
dt_tri$freq = rowSums(dt_tri[,2:12])
dt_tri = dt_tri[,-c(2:12)]
dt_tri = arrange(dt_tri,desc(freq))
dt_quad$freq = rowSums(dt_quad[,2:12])
dt_quad = dt_quad[,-c(2:12)]
dt_quad = arrange(dt_quad,desc(freq))
dt_pent$freq = rowSums(dt_pent[,2:12])
dt_pent = dt_pent[,-c(2:12)]
dt_pent = arrange(dt_pent,desc(freq))
dt_uni = as.data.table(dt_uni)
dt_bi = as.data.table(dt_bi)
dt_tri = as.data.table(dt_tri)
dt_quad = as.data.table(dt_quad)
dt_pent = as.data.table(dt_pent)


dt_bi$pred = sapply(dt_bi$feature,function(x) unlist(strsplit(x,"_",fixed = T))[2])
dt_tri$pred = sapply(dt_tri$feature,function(x) unlist(strsplit(x,"_",fixed = T))[3])
dt_quad$pred = sapply(dt_quad$feature,function(x) unlist(strsplit(x,"_",fixed = T))[4])
dt_pent$pred = sapply(dt_pent$feature,function(x) unlist(strsplit(x,"_",fixed = T))[5])
dt_bi$base = sapply(dt_bi$feature,function(x) unlist(strsplit(x,"_",fixed = T))[1])
dt_tri$base = sapply(dt_tri$feature, function(x) paste(unlist(strsplit(x,"_",fixed = T))[1],unlist(strsplit(x,"_",fixed = T))[2],sep = "_"))
dt_quad$base = sapply(dt_quad$feature, function(x) paste(unlist(strsplit(x,"_",fixed = T))[1],unlist(strsplit(x,"_",fixed = T))[2],unlist(strsplit(x,"_",fixed = T))[3],sep = "_"))
dt_pent$base = sapply(dt_pent$feature, function(x) paste(unlist(strsplit(x,"_",fixed = T))[1],unlist(strsplit(x,"_",fixed = T))[2],unlist(strsplit(x,"_",fixed = T))[3],unlist(strsplit(x,"_",fixed = T))[4],sep = "_"))


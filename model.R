load("corpus.Rdata")
dt_uni = uni
dt_quad = quad
dt_bi = bi
dt_tri = tri
dt_pent = pent
rm(uni,bi,tri,quad,pent)
library(data.table)
dt_uni = as.data.table(dt_uni)
dt_bi = as.data.table(dt_bi)
dt_tri = as.data.table(dt_tri)
dt_quad = as.data.table(dt_quad)
dt_pent = as.data.table(dt_pent)
gc()
predictions = function(input){
        input = tolower(input)
        if(substr(input,nchar(input),nchar(input))==" ")
        {
                input = substr(input,1,nchar(input)-1)
        }
        my_base = gsub(" ","_",input)
        nwords = length(unlist(strsplit(input," ")))
        if(nwords>4)
        {
                l = nwords-3
                u = nwords
                words = unlist(strsplit(my_base,"_"))
                words = words[l:u]
                my_base = paste(words,collapse = "_")
                nwords = 4
                
        }
        if(nwords==4)
        {
                ans = pred_pent(my_base,5)
        }
        if(nwords==3)
        {
                ans = pred_quad(my_base,5)
        }
        if(nwords==2)
        {
                ans = pred_tri(my_base,5)
        }
        if(nwords==1)
        {
                ans = pred_bi(my_base,5)
        }
        ans
}
pred_pent = function(base4,count,ignore = NULL){
        matrix = dt_pent[base == base4]
        matrix = matrix[!matrix$pred%in%ignore]
        if(nrow(matrix)>=count)
        {
                predictions = matrix[1:count]$pred
                ans = predictions
        }
        else if(nrow(matrix)<count)
        {
                predictions = NULL
                if(nrow(matrix!=0))
                        predictions = matrix[1:nrow(matrix)]$pred
                base3= paste(unlist(strsplit(base4,"_"))[2],unlist(strsplit(base4,"_"))[3],unlist(strsplit(base4,"_"))[4],sep = "_")
                ignore = predictions
                answer = pred_quad(base3,count-nrow(matrix),ignore)
                ans= c(predictions,answer)
        }
        ans
}
pred_quad = function(base3,count,ignore = NULL){
        matrix = dt_quad[base == base3]
        matrix = matrix[!matrix$pred%in%ignore]
        if(nrow(matrix)>=count)
        {
                predictions = matrix[1:count]$pred
                ans = predictions
        }
        else if(nrow(matrix)<count)
        {
                predictions = NULL
                if(nrow(matrix!=0))
                        predictions = matrix[1:nrow(matrix)]$pred
                base2= paste(unlist(strsplit(base3,"_"))[2],unlist(strsplit(base3,"_"))[3],sep = "_")
                ignore = predictions
                answer = pred_tri(base2,count-nrow(matrix),ignore)
                ans= c(predictions,answer)
        }
        ans
}
pred_tri = function(base2,count,ignore = NULL){
        matrix = dt_tri[base == base2]
        matrix = matrix[!matrix$pred%in%ignore]
        if(nrow(matrix)>=count)
        {
                predictions = matrix[1:count]$pred
                ans = predictions
        }
        else if(nrow(matrix)<count)
        {
                predictions = NULL
                if(nrow(matrix!=0))
                        predictions = matrix[1:nrow(matrix)]$pred
                base1= unlist(strsplit(base2,"_"))[2]
                ignore = c(ignore,predictions)
                answer = pred_bi(base1,count-nrow(matrix),ignore)
                ans= c(predictions,answer)
        }
        else if(nrow(matrix)==0)
        {
                answer = pred_uni(count,ignore)
                ans = answer
        }
        ans
}
pred_bi = function(base1,count,ignore = NULL){
        matrix = dt_bi[base == base1]
        matrix = matrix[!matrix$pred%in%ignore]
        if(nrow(matrix)>=count)
        {
                predictions = matrix[1:count]$pred
                ans = predictions
        }
        else if(nrow(matrix)<count)
        {
                predictions = NULL
                if(nrow(matrix!=0))
                        predictions = matrix[1:nrow(matrix)]$pred
                ignore = c(ignore,predictions)
                answer = pred_uni(count-nrow(matrix),ignore)
                ans = c(predictions,answer)
        }
        ans
}
pred_uni = function(count,ignore = NULL){
        matrix = dt_uni[1:50,]
        matrix = matrix[!matrix$feature%in%ignore]
        predictions = matrix[1:count]$feature
        ans = predictions
        ans
}

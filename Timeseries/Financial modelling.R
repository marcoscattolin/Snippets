library(quantmod)


symbols <- c("FTSEMIB.MI","^FTSE","^GDAXI","^FCHI","^NDX","^STOXX50E","^GSPTSE","^BVSP")
clean.symbols <- str_extract(symbols,"[A-Z0-9]+")
symblist <- list()
        
for (i in 1:length(symbols)){
        symblist[[i]] <- getSymbols(symbols[i], src = "yahoo", auto.assign=FALSE)
}
rm(i)

#merge dei dataframe
returns <- lapply(symblist, function(x) periodReturn(x, period = "daily"))
returns <- Reduce(function(...) merge(..., all=T), returns)
data <- data.frame(returns)
colnames(data) <- c(clean.symbols)
data <- data[complete.cases(data),]


#itera lungo le combinazioni 2 a 2
comb <- combn(length(data),2)
cross.correlations <- list()
for(k in 1:length(comb[1,])){
        i <- comb[1,k]
        j <- comb[2,k]
        cross.correlations[[k]] <- ccf(ts(data[,i]),ts(data[,j]), lag.max = 5)
        label <- paste(clean.symbols[i],clean.symbols[j],sep ="  ")
        if (k == 1){
                acf.values <- data.frame(cross.correlations[[1]]$lag,cross.correlations[[1]]$acf)
                colnames(acf.values) <- c("lag", label)
        } else {
                acf.values <- data.frame(acf.values,cross.correlations[[k]]$acf)
                colnames(acf.values) <- c(colnames(acf.values[,1:k]), label)
        }
}
rm(i,j,k,label)

#se esiste un valore significativo presente su lag negativo,
#allora il primo segnale predice il secondo

plot(cross.correlations[[20]])


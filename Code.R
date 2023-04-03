library(ggplot2)

df <- read.csv("df.csv")


p <- ggplot(data = df, aes(x=pos, y=cov)) + geom_area(aes(fill=chr))
p + facet_wrap(~ chr, ncol=1)





Chr2_cov <- sample(1.3e+07,7000)
Chr2 <- data.frame(Cov=Chr2_cov,fil=1)
pl <- qplot(Cov,fil,data=Chr2,geom="pointrange",ymin=0,ymax=1.1,xlab="Chromosome 2",ylab="-",alpha=I(1/50))
print(pl)




VeryLongVector <- sample(500,1e+07,replace=TRUE)

movAv <- function(vector,n){
    chops <- as.integer(length(vector)/n)
    count <- 0
    pos <- 0
    Cov <-0
    pos[1:chops] <- 0
    Cov[1:chops] <- 0
    for(c in 1:chops){
        tmpcount <- count + n
        tmppos <- median(count:tmpcount)
        tmpCov <- mean(vector[count:tmpcount])
        pos[c] <- tmppos
        Cov[c] <- tmpCov
        count <- count + n
    }

    result <- data.frame(pos=pos,cov=Cov)
    return(result)
}

Chr2 <- movAv(VeryLongVector,10000)
qplot(pos,cov,data=Chr2,geom="line")

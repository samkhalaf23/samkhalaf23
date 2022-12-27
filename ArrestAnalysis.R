rm(list=ls())
#part1
library(ISLR2)
data(USArrests)
dim(USArrests)
states= row.names(USArrests)
sd.data = scale(USArrests)
sd(sd.data[,1])
data.dist= dist(sd.data)
plot(hclust(data.dist), labels = states, main='Complete Linkage', xlab='', sub='', ylab='')
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, states)
par(mfrow = c(1, 1))
#pdf("hc_gene4.pdf", height=5, width=12)
plot(hc.out, labels = states)
cutree(hc.out, 4)
abline(h = 4, col = 'red')
#dev.off()
hc.out
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)
#
#Part2
set.seed(1)
n = 20 
m = 100 
x= matrix(rnorm(n*m), ncol = m)
x[, 1:50] = x[, 1:50]+0.5
fund.mini = Fund[, 1:m]
t.test(fund.mini[, 1], mu = 0)
p.values = rep(0,m)

fund.pvalue = rep(0, m)
for (i in 1:m){
  fund.pvalue[i] = t.test(fund.mini[, i], mu = 0)$p.value
fund.pvalue
for (i in 1:100)
  p.values[i] = t.test(x[, i], mu = 0)$p.value
}
decision = rep("Do not reject H0", 100)
decision[p.values < .05] = "Reject H0"
table(decision, factor(true, levels = c("H0 is True", "H0 is False")))

hist(fund.pvalue)
##part2
sum(p.values <0.5)
p.adjust(fund.pvalue, method = 'bonferroni')

p.adjust(fund.pvalue, method = "holm")

setwd('C:/Users/Allen/Dropbox/Classes_Senior/Psci 418-401/Final Project')
rm(list=ls())

da = read.csv('Model experiment-table.csv', skip=6)
names(da) = c('trial','N','delta','sigma','step','refugees','idps')

da$ratio = da$refugees / da$idps

da = da[order(da$trial),]
rownames(da) = da$trial
da = da[,-1]
attach(da)

Ns = unique(N)
deltas = unique(delta)
sigmas = unique(sigma)

### Boxplots
# tikz('test.tex', width=3.25, height=3.25)
for(D in 1:length(deltas)){
  for(S in c(1,3)){
    pdf( paste0('plot-D',deltas[D],'-Sigma',sigmas[S],'.pdf'), width=6.5, height=5)
    par(mar=c(2.5,2.5,1,2))
    boxplot(ratio~N, data=da[(da$sigma==sigmas[S] & da$ratio < Inf & !is.na(da$ratio) & da$N <= 5 & da$delta==deltas[D]),], outline=F, las=1, boxwex=0.6)
    abline(h=1, lty='dotted')
    dev.off()
  }
}

mean.da = data.frame()
for (i in 1:length(Ns)){
  for (j in 1:length(deltas)){
    for (k in 1:length(sigmas)){
      mean.ratio = mean( ratio[(N==Ns[i] & delta==deltas[j] & sigmas==sigmas[k] & ratio < Inf & !is.nan(ratio))] )
      mean.da = rbind(mean.da, c(Ns[i],deltas[j],sigmas[k],mean.ratio))
    }
  }
}
detach(da)
rm(list=ls()[ls()!='mean.da'])
names(mean.da) = c('N','delta','sigma','mean.ratio')

mean.da = mean.da[(mean.da$N <=5),]

x = unique(mean.da$delta)
Ns = unique(mean.da$N)
deltas = unique(mean.da$delta)
colors = c('blue','red','green','black')

##################### Delta on x-axis

plot(mean.da$delta, mean.da$mean.ratio, type='n', las=1, cex.axis=.8, xlab="Network Locality (low: more local)", ylab="Refugee-to-IDP Ratio", main="")
for (l in 1:length(Ns)){
  lines(x, mean.da$mean.ratio[(mean.da$N==Ns[l] & mean.da$sigma==4)], type='b')
}
abline(h=1.0, lty='dotted')



#################### N on x-axis

x = unique(mean.da$N)

plot(mean.da$N, mean.da$mean.ratio, type='n', las=1, cex.axis=.8, xlab="Number of Connections (Relocation Options) per Household", ylab="Refugee-to-IDP Ratio", main="Influence of Network Structure on Displacement")
grid(nx=NULL, ny=NULL)
for (l in 1:length(deltas)){
  lines(x, mean.da$mean.ratio[(mean.da$delta==deltas[l] & mean.da$sigma==1)], type='b', col=colors[l])
}
abline(h=1.0, lty='dotted')
## add legend
legend(locator(1), legend=c('Max connection dist.=2','d=6','d=10','d=14'), col=c('red','green','blue','black'), lty=c(1,1,1,1), cex=0.8)

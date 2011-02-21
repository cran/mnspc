print.kResults<-function(all.results.sarl1, k.results)
{

p.k<-all.results.sarl1$k.1
p.h<-all.results.sarl1$h.1
p.arl0<-all.results.sarl1$arl0.1
p.arl1<-all.results.sarl1$arl1.1

h<-k.results$h
k<-k.results$k
arl1<-k.results$arl1
arl0<-k.results$arl0.actual

nrowin<-length(p.h)

.C("printkResults", p.k=as.double(p.k),p.h=as.double(p.h), p.arl0=as.double(p.arl0), pr.arl1=as.double(p.arl1), k=as.double(k), h=as.double(h), arl0=as.double(arl1), arl1=as.double(arl0), nrowin=as.integer(nrowin))

}

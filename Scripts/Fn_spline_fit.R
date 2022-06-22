##------------------------


# SCRIPT NAME: Fn_Spline

# AUTHOR: Jon Walter

# DATE CREATED: 2022-04-14


##------------------------
spline.fit<-function(distmat,zmat,nresamp=1000,quantiles=c(0,0.01,0.025,0.05,0.1,0.5,0.9,0.95,0.975,0.99,1)){
  triang<-lower.tri(distmat)
  distmat<-distmat
  xemp<-distmat[triang]
  yemp<-zmat[triang]
  dfs=sqrt(nrow(distmat))
  out<-list()
  
  emp.spline<-smooth.spline(xemp,yemp,df=dfs)
  out$emp.spline<-emp.spline
  
  resamp.splines<-matrix(NA, nrow=nresamp, ncol=length(emp.spline$y))
  for(ii in 1:nresamp){
    shuffle<-sample(1:nrow(distmat), size=nrow(distmat), replace=TRUE)
    xres<-distmat[shuffle,shuffle][triang]
    yres<-zmat[shuffle,shuffle][triang]
    drop.NaNs<-!is.na(yres)
    xres<-xres[drop.NaNs]; yres<-yres[drop.NaNs]
    yres<-yres[!(xres==0)]
    xres<-xres[!(xres==0)]
    res.spline<-smooth.spline(xres,yres,df=dfs)
    resamp.splines[ii,]<-predict(res.spline, x=emp.spline$x)$y
  }
  out$resamp.splines<-resamp.splines
  out$spline.quantiles<-apply(resamp.splines,2,quantile,probs=quantiles)
  return(out)
}
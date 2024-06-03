#' For plotting the magnitude of values in \code{coh_tv} objects against time and timesecale
#' 
#'
#' @param object An object of class \code{coh_tv}
#' @param zlims z axis limits. If specified, must encompass the range of 
#' \code{Mod(get_values(object))}. Default NULL uses this range.
#' @param neat Logical. Should timescales with no values be trimmed?
#' @param colorfill Color spectrum to use, set through colorRampPalette. Default value NULL 
#' produces jet colors from Matlab.
#' @param sigthresh Significance threshold(s). Numeric vector with values between 0 and 1. 
#' Typically 0.95, 0.99, 0.999, etc. Contours are plotted at these values.
#' @param colorbar Logical. Should a colorbar legend be plotted?
#' @param title Title for the top of the plot.
#' @param filename Filename (without extension), for saving as pdf. Default value NA saves no 
#' file and uses the default graphics device.
#' @param ... Additional graphics parameters passed to \code{image} (\code{graphics} package) 
#' if \code{colorbar==FALSE}, or to \code{image.plot} (\code{fields} package) if 
#' \code{colorbar==TRUE} (for \code{tts} objects) 
#' 
#' @details Based on plotting methods for wavelet phasor mean field.
#' 
#' @author Thomas Anderson, \email{anderstl@@gmail.com}, Jon Walter, \email{jaw3es@@virginia.edu}; Lawrence 
#' Sheppard, \email{lwsheppard@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}
#' 
#' @references 
#' Sheppard, L.W., et al. (2016) Changes in large-scale climate alter spatial synchrony of aphid 
#' pests. Nature Climate Change. DOI: 10.1038/nclimate2881
#' 
#' Sheppard, LW et al. (2019) Synchrony is more than its top-down and climatic parts: interacting 
#' Moran effects on phytoplankton in British seas. Plos Computational Biology 15, e1006744. doi: 10.1371/journal.pcbi.1006744
#' 
#' @seealso \code{\link{tts}}, \code{\link{wt}}, \code{\link{wmf}}, \code{\link{wpmf}}, \code{\link{coh}},
#' \code{\link{wlmtest}}, \code{\link{plotphase}}, \code{\link{bandtest}}, \code{\link{plotrank}},
#' \code{browseVignettes("wsyn")}
#' 
#' @examples
#' times<-1:100
#' dat1<-matrix(rnorm(1000),10,100)
#' dat2<-matrix(rnorm(1000),10,100)
#' dat1<-cleandat(dat1,times,1)$cdat
#' dat2<-cleandat(dat2,times,1)$cdat
#' norm<-"powall"
#' sigmethod<-"fast"
#' nrand<-100 #for real applications use larger nrand
#' res<-coh_tv(dat1,dat2,times,norm,sigmethod,nrand)
#' plotmag.coh_tv(res)
#' 
#' @export
#' @importFrom fields image.plot
#' @importFrom graphics image axis par plot lines text
#' @importFrom grDevices colorRampPalette pdf dev.off
#' @importFrom stats quantile
#' 
plotmag.coh_tv<-function(object,zlims=NULL,neat=TRUE,colorfill=NULL,sigthresh=0.95,colorbar=TRUE,title=NULL,filename=NA,...)
{
  require(wsyn)
  wav<-Mod(object$coher)
  times<-object$times
  timescales<-object$timescales
  signif<-object$signif
  
  if (any(sigthresh>=1 | sigthresh<=0))
  {
    stop("Error in plotmag.coh_tv: inappropriate value for sigthresh")
  }
  if(is.null(zlims)){
    zlims<-range(wav,na.rm=T)
  }else
  {
    rg<-range(wav,na.rm=T)
    if (rg[1]<zlims[1] || rg[2]>zlims[2])
    {
      stop("Error in plotmag.coh_tv: zlims must encompass the z axis range of what is being plotted")
    }
  }
  if(neat){
    inds<-which(!is.na(colMeans(wav,na.rm=T)))
    wav<-wav[,inds]
    timescales<-timescales[inds]
    if (!identical(signif,NA))
    {
      signif[[3]]<-signif[[3]][,,inds]
    }
  }
  if(is.null(colorfill)){
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill<-grDevices::colorRampPalette(jetcolors)
  }
  ylocs<-pretty(timescales,n=8)
  xlocs<-pretty(times,n=8)
  
  if (!is.na(filename))
  {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  if (!colorbar)
  {
    graphics::image(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
                    ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }else
  {
    fields::image.plot(x=times,y=log2(timescales),z=wav,xlab="Time",zlim=zlims,
                       ylab="Timescale",axes=F,col=colorfill(100),main=title,...)
    graphics::axis(1,at = xlocs,labels=xlocs)
    graphics::axis(2,at = log2(ylocs),labels = ylocs)
  }
  if (!all(is.na(signif)))
  {
    graphics::par(new=T)
    if (signif[[1]] %in% c("fft","aaft"))
    {
      
      graphics::contour(x=times,y=log2(timescales),z=signif[[4]],levels=sigthresh,
                        drawlabels=F,lwd=2,xaxs="i",xaxt="n",yaxt="n",xaxp=c(0,1,5),
                        las = 1,frame=F)
    }
  }
  if (!is.na(filename))
  {
    grDevices::dev.off()
  }
}

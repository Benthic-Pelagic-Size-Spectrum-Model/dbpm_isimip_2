getExportRatio<-function(sphy,lphy,sst,depth){
  ptotal=sphy+lphy
  plarge <- lphy/ptotal
  psmall <- sphy/ptotal
  er <- (exp(-0.032*sst)*((0.14*psmall)+(0.74*(plarge)))+(0.0228*(plarge)*(depth*0.004)))/(1+(depth*0.004))
  return(er)
}


GetPPIntSlope<-function(sphy,lphy,mmin=10^-14.25, mmid=10^-10.184,mmax=10^-5.25,depth,output="slope") {
  
  #  need to convert sphy and lphy from mol C / m^2 to g C / m^2
  
  sphy= (sphy*12.0107)
  lphy= (lphy*12.0107)
  
  # it's depth integrated units are /m^-2 and need to divide my depth if using depth integrated inputs  
  
  sphy= sphy/min(depth,100)
  lphy= lphy/min(depth,100)
  
  
  #mmin=10^-14.25 gww, approx 0.2 ESD
  #mmid=10^-10.184 gww, approax 5 ESD - division between small and large phytoplankton in GFDL-Topaz
  #mmax=10^-5.25 gww, approx 200 ESD
  
  
  #from Appendix of Barnes 2010 JPR, used in Woodworth-Jefcoats et al. 2013 GCB.
  
  #the scaling of biomass with body mass can be described as B=aM^b
  
  #the exponent b (also equivalent to the slope b in a log B vs log M relationship) can be assumed:
  # 0.25 (results in N ~ M^-3/4) or 0 (results in N ~ M^-1)
  # most studies seem to suggest N~M^-1, so can assume that and test sensitivity of our results to this assumption.
  
  #Calculate a and b in log B (log10 abundance) vs. log M (log10 gww)
  
  midsmall<-log10((mmin+mmid)/2) #in log10 gww
  midlarge<-log10((mmid+mmax)/2) #in log10 gww
  
  
  small<-log10((sphy*10)/10^midsmall)       #convert to log10 (gww/size class median size) for log10 abundance
  large<-log10((lphy*10)/10^midlarge)  #convert to log10 (gww/size class median size) for log10 abundance
  
  b<-(small-large)/(midsmall-midlarge)
  
  a = large - b*midlarge  #a is really log10(a), same a when small, midsmall are used
  # pp<-c(a,b) # log10(a) will equal to the log10 density of particles at mass = 1 g and at log10 (mass)=0
  # a could be used directly to replace 10^pp in sizemodel()
  if (output =="slope") return(b)
  if (output =="intercept") return(a)
  
}







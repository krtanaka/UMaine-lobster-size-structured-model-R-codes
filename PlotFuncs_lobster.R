
sizebins=report$Size_bins[-1]-0.5
Nyear=report$N_year

#windows(record=T)
#X11(,type="cairo")

# Weight-Length -----------------------------------------------------------
PlotWL = function() {
  
  Year_end = report$EndYear
  Year_begin = report$BeginYear
  year = seq(Year_begin,Year_end,1)
  
  plot_nrow = ceiling(Nyear/6); plot_ncol=6
  yaxis = rep(1,plot_nrow)
  
  for(pl in 1:plot_nrow) {
    
    yaxis[pl] = yaxis[1]+(pl-1)*plot_ncol
    
  }
  
  xaxis = seq(Nyear-plot_ncol+1,Nyear,1)
  
  par(mfrow=c(plot_nrow,plot_ncol))
  
  par(mar = c(0,0,0,0), oma = c(4, 4, 4, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
  for (yi in 1:Nyear) {
    
    y=report$Weight_length[yi,]
    
    plot(sizebins,y,col='Black',type='b', cex = 0.5, 
         pch=16, axes=F, ylim=c(0,10000), xlim = c(0,300))
    
    mtext(paste(year[yi]),side=3,line=-1,adj=0.1,cex=0.6)
    
    if(yi %in% yaxis) 
      axis(2,col='gray',at=seq(0,9000,5000))
    
    if(yi %in% xaxis) 
      axis(1,col='gray',at=sizebins[-1])
    
    box(col='gray')
  }
  
  mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
  mtext('Mean Weight (g)',side=2,outer=T,line=2.2)
  mtext(paste('Length-Weight Relationships'),side=3,outer=T,line=2.2)
  
}

PlotWL_n = function(year) {
  
  par(mar = c(4,4,1,1))
  
  Year = year-report$BeginYear+1
  
  ind = 0;
  itemp = Year
  Maintemp = paste("Year",year)
  
  y = report$Weight_length[itemp,]
  plot(sizebins, y, 
       xlab = "Carapace Length (mm)", 
       ylab = "Mean Weight (g)", 
       type='o', axes = F, 
       col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab=1.3, cex.axis=1.3)
  axis(1); axis(2); box(bty = "L")
  
}

# Maturity-Length ---------------------------------------------------------
PlotML = function() {
  
  Year_end=report$EndYear; Year_begin=report$BeginYear
  year = seq(Year_begin,Year_end,1)
  
  plot_nrow = ceiling(Nyear/6); plot_ncol=6
  yaxis = rep(1,plot_nrow)
  
  for (pl in 1:plot_nrow) {
    
    yaxis[pl] = yaxis[1]+(pl-1)*plot_ncol
    
  }
  
  xaxis = seq(Nyear-plot_ncol+1,Nyear,1)
  
  par(mfrow = c(plot_nrow,plot_ncol))
  
  par(mar = c(0.5,0.5,0.5,0.5), oma = c(4, 4, 4, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
  for (yi in 1:Nyear) {
    
    y = report$Maturity_length[yi,]
    plot(sizebins,y,col = 'Black',type = 'b', pch = 16,axes = F,ylim = c(0,1),cex = 0.5)
    mtext(paste(year[yi]),side = 3,line = -1,adj = 0.1,cex = 0.6)
    
    if(yi %in% yaxis)
      axis(2,col = 'gray',at = seq(0,1,0.2))
    
    if(yi %in% xaxis)
      axis(1,col = 'gray',at = sizebins[-1])
    box(col = 'gray')
    
  }
  mtext('Carapace Length (mm)',side = 1,outer = T,line = 2.2)
  mtext('Frequency',side = 2,outer = T,line = 2.2)
  mtext(paste('Maturity'),side = 3,outer = T,line = 2.2)
}

PlotML_n = function(year) {
  
  par(mar = c(4,4,1,1))
  Year = year-report$BeginYear+1
  
  Maintemp = paste("Year", year)
  
  y = report$Maturity_length[Year, ]
  
  plot(sizebins, y, xlab = "Carapace Length (mm)", ylab = "Maturity", 
       type='o', axes = F, 
       col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab=1.3, cex.axis=1.3)
  axis(1); axis(2); box(bty = "L")
  
}

# Growth matrix,specify the year, season and how many years for gr --------
Plot_GM_tb = function(n_tb,iYearNum) {
  
  plot_nrow = 1; plot_ncol=n_tb
  par(mfrow=c(plot_nrow,plot_ncol))
  
  par(mar = c(0, 0, 0, 0), 
      oma = c(4, 4, 0.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
  for (i in 1:n_tb) {
    
    PlotGM(i,iYearNum)
    
  }
  mtext('Year',side=1,outer=T,line=2.2)
  mtext('Carapace Length (mm)',side=2,outer=T,line=2.2)
  # mtext(paste('Growth of a cohort with the growth matrices of year'),side=3,outer=T,line=2.2)
}

PlotGM=function(yearblock,iYearNum) {
  
  season=report$Time_step
  nsizebin=length(sizebins)
  Year=yearblock
  
  if(season>1)
  {
    itemp=season*nsizebin*(Year-1)
    GM=report$Growth[(itemp+1):(itemp+nsizebin*season),]
    Maintemp=paste("Growth of a cohort with the Growth matrices of year",Year)
    
    BRPNAS<-rep(0,nsizebin)
    BRPNASData<-matrix(NA,ncol=nsizebin,nrow=iYearNum)
    Recruitment<-1000
    RecPrjRatio<-report$Recrut_Sizebin_ratio
    BRPS<-exp(-report$Natural_Mortality[(season*(Year-1)+1):(season*(Year-1)+season),])
    
    for (i in 1:(iYearNum-1))
    {
      for (j in 1:season)
      {
        iL=nsizebin*(j-1)
        if(i==1&&j==1)
          BRPNASData[1,]<- BRPNAS <- BRPNAS+Recruitment*RecPrjRatio;
        NASTemp<-BRPNAS*BRPS[j,];
        BRPNAS<-NASTemp%*%GM[(iL+1):(iL+nsizebin),]
        BRPNASData[i+1,]<-BRPNAS;
      }
    }
    
    iYear=seq(1,iYearNum,1);RRRData=NULL;RMainData=c();
    
    for( i in 1:iYearNum)
    {
      
      RRData<-BRPNASData[i,]
      RRData<-RRData/sum(RRData)
      RMainData[i]<-sum(RRData*sizebins)
      for(j in 1:nsizebin)
      {
        SizeNum<-RRData[j]*1000  
        if( SizeNum<1)
          next;
        XXX1<-rep(i,SizeNum)
        XXX2<-rep(j,SizeNum)
        RRRDataTemp<-cbind(iYear[XXX1],sizebins[XXX2])
        RRRData<-rbind(RRRData,RRRDataTemp)
      }
    }
    
  } else {
    
    itemp=nsizebin*(Year-1)
    GM=report$Growth[(itemp+1):(itemp+nsizebin),]
    Maintemp=paste("Growth of a cohort with the Growth matrix of year",Year)
    
    BRPNAS<-rep(0,nsizebin)
    BRPNASData<-matrix(NA,ncol=nsizebin,nrow=iYearNum)
    Recruitment<-1e5
    RecPrjRatio<-report$Recrut_Sizebin_ratio
    BRPS<-exp(-report$Natural_Mortality[Year,])
    
    for( i in 1:(iYearNum-1))
    {
      if(i==1)
        BRPNASData[1,]<- BRPNAS <- BRPNAS+Recruitment * RecPrjRatio;
      #BRPNASData<-rbind(BRPNASData,BRPNAS);
      NASTemp<-BRPNAS*BRPS;
      BRPNAS<-NASTemp%*%GM
      BRPNASData[i+1,]<-BRPNAS;
    } 
    iYear=seq(1,iYearNum,1);RRRData=NULL;RMainData=c();
    
    for( i in 1:iYearNum)
    {
      
      RRData<-BRPNASData[i,]
      RRData<-RRData/sum(RRData)
      RMainData[i]<-sum(RRData*sizebins)
      for(j in 1:nsizebin)
      {
        SizeNum<-RRData[j]*1000  
        if( SizeNum<1)
          next;
        XXX1<-rep(i,SizeNum)
        XXX2<-rep(j,SizeNum)
        RRRDataTemp<-cbind(iYear[XXX1],sizebins[XXX2])
        RRRData<-rbind(RRRData,RRRDataTemp)
      }
    }
  }
  boxplot(RRRData[,2]~factor(RRRData[,1]),xlab="Year",ylab="CL(mm)", pch = 20, axes = F, 
          cex.lab=1.0, cex.axis=1.0, cex.main=1.0, cex.sub=1.0)
  #stripchart(RRRData[,2]~factor(RRRData[,1]),method='jitter',add=T,vertical=T,pch=1)
  lines(1:iYearNum,RMainData,type="l",col="blue",lwd=3)
  axis(1); axis(2); box(bty = "L")
  
}

# Selectivity of fisheries ------------------------------------------------
Plot_sel_f = function() {
  
  par(mfrow = c(2,2), 
      mar = c(2,2,2,2), 
      oma = c(4, 4, 0.5, 0.5), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  PlotSelF(2013,1,1)
  axis(1); axis(2); box(bty = "L")
  # axis(1,col = 'black',at = sizebins[-1])
  
  PlotSelF(2013,2,1)
  axis(1); axis(2); box(bty = "L")
  # axis(1,col = 'black',at = sizebins[-1])
  
  PlotSelF(2013,3,1)
  axis(1); axis(2); box(bty = "L")
  # axis(1,col = 'black',at = sizebins[-1])
  
  PlotSelF(2013,4,1)
  axis(1); axis(2); box(bty = "L")
  # axis(1,col = 'black',at = sizebins[-1])
  
  mtext('Carapace Length (mm)',side = 1,outer = T,line = 2.2)
  mtext('Selectivity',side = 2,outer = T,line = 2.2)
  # mtext(paste('Estimated fleet selectivity'),side = 3,outer = T,line = 2.2)
  
}

PlotSelF = function(year,season,fishery) {
  
  Season = report$Time_step
  nsizebin = length(sizebins)
  Year = year-report$BeginYear+1
  
  if (Season>1) {
    
    itemp = Nyear*Season*(fishery-1)+Nyear*(season-1)
    
    # Maintemp = paste(year,";Season",season,";Fishery",fishery,sep = '')
    Maintemp = paste0(year,"; Season ",season)
    
    
  } else {
    
    itemp = Nyear*(fishery-1)
    
    if (year == 1987)
      fishery = 2
    
    if (year>2000)
      fishery = fishery+1
    
    Maintemp = paste("Selectivity time block",fishery)
  }
  
  sel = report$Fleet_selectivity[itemp+Year,]
  
  plot(sizebins,sel,xlab = "",ylab = "",
       cex = 1,col = 'black',type = 'b',lwd = 1,pch = 16,
       cex.lab = 1.3, cex.axis = 1.3, cex.main = 1.3, cex.sub = 1.3,axes = FALSE)
  
  mtext(Maintemp,side = 3,line = -2,adj = 0.8,cex = 0.8)
}

# Selectivity of surveys --------------------------------------------------
PlotSelS = function(year,survey) {
  
  par(mar =c(4, 4, 4, 0.5), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  nsizebin=length(sizebins)
  Year=year-report$BeginYear+1
  
  itemp=Nyear*(survey-1)+Nyear
  if(survey == 1) Maintemp="VTS Q3"
  if(survey == 2) Maintemp="NEFSC Q2"
  if(survey == 3) Maintemp="NEFSC Q4"
  if(survey == 4) Maintemp="MA Q2"
  if(survey == 5) Maintemp="MA Q4"
  if(survey == 6) Maintemp="MENH Q2"
  if(survey == 7) Maintemp="MENH Q4"
  
  # Maintemp=paste("Survey",survey)
  
  sel=report$Index_selectivity[itemp,]
  plot(sizebins,sel,
       xlab="CL (mm)", 
       ylab="Selectivity",
       main=Maintemp,
       cex=1.1, axes = F, 
       col='black',type='o',
       lwd=2,pch=16,
       cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3)
  axis(1); axis(2);box(bty = "L")
}

# Fishing mortality -------------------------------------------------------
Plot_f = function() {
  
  Year = seq(report$BeginYear,report$EndYear,1)
  Season = report$Time_step
  f = report$Fishing_Mortality
  
  write.csv(f, paste0("C:/Users/Kisei/Desktop/", "F_Mortality_", HSI, ".csv"))
  
  par(mfrow=c(1,4), 
      mar = c(2, 2, 2, 1), 
      oma = c(2, 2, 0.5, 0.5), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  PlotF(1,1)
  # PlotF(1,2)
  # PlotF(1,3)
  # PlotF(1,4)
  
  # mtext('Year',side=1,outer=T,line=2.2)
  # mtext('Fishing mortality',side=2,outer=T,line=2.2)
  # mtext(paste('Estimated fishing mortality'),side=3,outer=T,line=2.2)
  
}

PlotF = function(fishery,season) {
  
  Year = seq(report$BeginYear,report$EndYear,1)
  Season = report$Time_step
  f = report$Fishing_Mortality
  
  if (Season>1) {
    
    for (i in 1:Season) {
      
      Maintemp = paste("Season",i)
      itemp = Season*(fishery-1)
      ftemp = f[itemp+i,]
      
      plot(Year,ftemp,
           xlab="Year",
           ylab="",
           type='o',
           col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
           cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3,
           ylim=c(0,1.2),
           axes=F)
      
      axis(1); axis(2); box(col='black', bty = "L")
      mtext(Maintemp,side=3,line=-2,adj=0.1,cex=1.0)
      
      if (i %in% c(1))
      # axis(2,col='black',at=seq(0,1.2,0.05))
      mtext("Fishing mortality", 2, outer = 2)

      # axis(2,col='black',at=seq(0,max(f),0.05))
      
      # if (i %in% c(1:4))
      #   axis(1,col='black',at=Year)
      
    }
    
  } else {
    
    Maintemp = paste('Fishery',fishery)
    
    plot(Year,f[fishery,],xlab="Year",ylab="F",type='b',
         pch=16,cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3)
    
  }
  
}

CompareF=function(type){
  
  if(type == 1){
  
  location = "C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/data/"
  F_HSI_N = t(read.csv(paste0(location, "F_Mortality_n.csv"), header = T))
  F_HSI_Y = t(read.csv(paste0(location, "F_Mortality_y.csv"), header = T))
  
  F_HSI_N = F_HSI_N[2:31,]
  F_HSI_Y = F_HSI_Y[2:31,]
  
  F_HSI_N = apply(F_HSI_N, 1, sum)
  F_HSI_Y = apply(F_HSI_Y, 1, sum)
  
  Year = c(1984:2013)
  
  par(mfrow=c(1,1), 
      mar = c(2, 4, 1, 1), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  plot(Year,F_HSI_N,
       ylab = "Fishing mortality",
       xlab = "",type = 'o',
       # ylim=c(0,1.2),
       axes=F,
       col = 'blue',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  box(bty = "L"); axis(2); axis(1)
  
  lines(Year,F_HSI_Y,type = 'o',
        col = 'red',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  legend("topleft", cex = 1, lwd = 2,
         legend=c("Base Model","Environmetnal Model"),
         lty=c(1,2),
         col = c("blue", "red"),
         bty = "n")
  
  }
  
  if(type == 2){
  
  location = "C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/data/"
  F_HSI_N = t(read.csv(paste0(location, "F_Mortality_n.csv"), header = T))
  F_HSI_Y = t(read.csv(paste0(location, "F_Mortality_y.csv"), header = T))
  
  colnames(F_HSI_N) = c("Q1", "Q2", "Q3", "Q4")
  colnames(F_HSI_Y) = c("Q1", "Q2", "Q3", "Q4")
  
  F_HSI_N = F_HSI_N[2:31,]
  F_HSI_Y = F_HSI_Y[2:31,]
  
  F_HSI_N = apply(F_HSI_N, 1, sum)
  F_HSI_Y = apply(F_HSI_Y, 1, sum)
  
  Year = c(1984:2013)
  
  f1 = cbind(Year, F_HSI_N[,1], F_HSI_Y[,1])  
  f2 = cbind(Year, F_HSI_N[,2], F_HSI_Y[,2])  
  f3 = cbind(Year, F_HSI_N[,3], F_HSI_Y[,3])  
  f4 = cbind(Year, F_HSI_N[,4], F_HSI_Y[,4])  
  
  par(mfrow=c(1,4), 
      mar = c(4, 4, 4, 1), 
      oma = c(2, 1, 0.5, 1), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  plot(Year,f1[,2],
       ylab = "Fishing mortality",
       xlab = "",type = 'o',
       ylim=c(0,1.2),
       axes=F,
       col = 'blue',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  box(bty = "L")
  axis(2); axis(1)
  lines(Year,f1[,3],type = 'o',
        col = 'red',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  legend(x=1982,
         y=1.2, cex = 1, lwd = 2,
         legend=c("Base Model","Environmetnal Model"),
         lty=c(1,2),
         col = c("blue", "red"),
         bty = "n")
  
  plot(Year,f2[,2],
       ylab = "",
       xlab = "",type = 'o',
       ylim=c(0,1.2), 
       axes=F,
       col = 'blue',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  box(bty = "L")
  axis(2); axis(1)
  lines(Year,f2[,3],type = 'o',
        col = 'red',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  plot(Year,f3[,2],
       ylab = "",
       xlab = "",type = 'o',
       ylim=c(0,1.2), 
       axes=F,
       col = 'blue',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  box(bty = "L")
  axis(2); axis(1)
  lines(Year,f3[,3],type = 'o',
        col = 'red',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  plot(Year,f4[,2],
       ylab = "",
       xlab = "",type = 'o',
       ylim=c(0,1.2), 
       axes=F,
       col = 'blue',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  box(bty = "L")
  axis(2); axis(1)
  lines(Year,f4[,3],type = 'o',
        col = 'red',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  par(mfrow = c(1,1))
  
  }
  
}

# Natural mortality -------------------------------------------------------
PlotM=function(year)
{
  par(mfrow=c(1,1))
  par(mar = c(4, 4, 1, 0.5))
  
  Season=report$Time_step
  M=report$Natural_Mortality
  Year=year-report$BeginYear+1
  par(mfrow=c(1,1))
  if (Season>1)
  {
    itemp=Season*(Year-1)
    m=M[(itemp+1),]
    Maintemp=paste("Seasonal M; Year",year)
    plot(sizebins,m,xlab="Length (mm)",ylab="M",main=Maintemp,type='l',lwd=3,
         col='blue',cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3)
  }else
  {
    Maintemp=paste("Year",year)
    m=M[Year,]
    plot(sizebins,m,xlab="Length (mm)",ylab="M",main=Maintemp,type='l',lwd=3,
         col='blue',cex.lab=1.3, cex.axis=1.3, cex.main=1.3, cex.sub=1.3)
  }  
}


# Recruitment -------------------------------------------------------------
PlotR=function(plot){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  R = report$recruitment_Pred
  Rbar = report$Mean_Recruitment
  Rdev = report$recruitment_log_Dev
  RdevSD = report$recruitment_log_DevSD2

  if(report$SexAtSizeLamda>0){
    
    SSB = report$Spawning_stock_Biomass
    
  }else{
    
    SSB = report$Spawning_stock_Biomass_input
    
  }
  
  par(mfrow = c(1,1), mar = c(4,4,1,1), mgp = c(2, 0.6, 0))
  
  ssb_r = data.frame(SSB, R)
  ssb_r$Year = as.factor(1984:2013)
  
  library(colorRamps)
  color <- with(ssb_r, data.frame(Year = levels(Year), color = matlab.like(30))) 
  ssb_r = merge(ssb_r, color, all = T)
  
  fit = data.frame(report$ fit$ names, report$ fit$ est, report$ fit$ std) 
  fit = fit[which(fit$report.fit.names == "sdr_vRecruitmentP"),]
  
  ci = 1.96*fit$report.fit.std
  

  if(plot == 1){
    plot(Year,R,
         ylab = "Recruitment (hundred thousands)",
         xlab = "Year",type = 'o',
         ylim = c(min(R)*0.8,max(R)*1.2),
         col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed', axes = F, 
         cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
    arrows(Year, R-ci, Year, R+ci, code = 3, angle = 90, length = 0.05, col = "black")
    axis(1); axis(2); box(bty = "L")
  }
  
  if(plot == 2){
  plot(Year,Rdev,
       xlab = "Year",
       ylab = "Log recruitment deviation",type = 'o',
       ylim = c(-2,2),
       col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed', axes = F,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  abline(h = 0,col = "gray",lwd = 1.5)
  axis(1); axis(2); box(bty = "L")
  
  }

  if(plot == 3){
  plot(ssb_r$SSB,ssb_r$R,
       type = 'n',
       xlab = "Spawning biomass (mt)",
       ylab = "Recruits (hundred thousands)",
       # xlim = c(20000, 65000), 
       # ylim = c(100,1500),
       # col = ssb_r$color, 
       # cex = 0.001, pch = 20, lwd = 1.5, lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, axes = F, cex.sub = 1.5)
  text(x = ssb_r$SSB, y = ssb_r$R, labels  = ssb_r$Year)
  axis(1); axis(2); box(bty = "L")
  
  }
  
  par(mfrow = c(1,1))
  r = cbind(Year,R)
  write.csv(r, paste0("C:/Users/Kisei/Desktop/", "Recruitment_", HSI, ".csv"))
}

CompareR=function(i){
  
  library(readr)
  
  if(i == 1){
    location = "C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/data/"
    R_HSI_N = read.csv(paste0(location, "Recruitment_n.csv"), header = T)
    R_HSI_Y = read.csv(paste0(location, "Recruitment_y.csv"), header = T)
    R = cbind(R_HSI_N, R_HSI_Y[2])  
    colnames(R) = c("Year", "R_without_HSI", "R_with_HSI")
  }
  
  if(i == 2){
    location = paste0("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_",HSI,"/")  
    R_HSI_N = read_csv(paste0(location, "Recruitment_", HSI, ".csv"))
    HSI_2 = gsub("n_", "y_", HSI)
    location_2 = gsub("n_", "y_", location)
    R_HSI_Y = read_csv(paste0(location_2, "Recruitment_", HSI_2, ".csv"))
    R = data.frame(R_HSI_N$Year,R_HSI_N$R, R_HSI_Y$R)  
    colnames(R) = c("Year", "R_without_HSI", "R_with_HSI")
  }
  
  par(mfrow=c(1,1), 
      mar = c(2,4,1,1),
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  plot(R$Year,R$R_with_HSI,
       ylab = "Recruits (thousands)",
       xlab = "",type = 'o',
       col = 'red',cex = 1.5,pch = 20,lwd = 2,lty = 'dashed', axes = F, 
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  lines(R$Year,R$R_without_HSI,
        type = 'o',
        ylim = c(0,1800),
        col = 'blue',cex = 1.5,pch = 20,lwd = 2,lty = 'dashed',
        cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  
  legend("topleft", cex = 1, lwd = 2,
         legend=c("Base Model","Environmetnal Model"),
         lty=c(1,2),
         col = c("blue", "red"),
         bty = "n")
  axis(1); axis(2); box(bty = "L")

}

PlotR_HSI=function(d){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  R = report$recruitment_Pred
  Rbar = report$Mean_Recruitment
  Rdev = report$recruitment_log_Dev
  RdevSD = report$recruitment_log_DevSD2
  
  hsi = read.csv("C:/Users/Kisei/Google Drive/HSI_values.csv", header = T)
  hsi = hsi[1:30,]
  colnames(hsi) = c("Year", 
                    "SP_SSB", 
                    "FL_SSB",
                    "SP_R", "SP_R_3yrs_avg", "SP_R_5yrs_avg", 
                    "FL_R", 
                    "SP_GOMGBK",
                    "SP_R_GOM_GBK_unweighted",
                    "SP_R_GOM_GBK_weighted", 
                    "bottom_temp", 
                    "bottom_salt")
  
  if(d == 1){
    hsi = hsi$SP_SSB 
  }
  if(d == 2){
    hsi = hsi$FL_SSB 
  }
  if(d == 3){
    hsi = hsi$SP_R 
  }
  if(d == 4){
    hsi = hsi$SP_R_3yrs_avg 
  }
  if(d == 5){
    hsi = hsi$SP_R_5yrs_avg 
  }
  if(d == 6){
    hsi = hsi$FL_R 
  }
  if(d == 7){
    hsi = hsi$SP_GOMGBK 
  }
  if(d == 8){
    hsi = hsi$SP_R_GOM_GBK_unweighted 
  }
  if(d == 9){
    hsi = hsi$SP_R_GOM_GBK_weighted 
  }
  if(d == 10){
    hsi = hsi$bottom_temp 
  }
  if(d == 11){
    hsi = hsi$bottom_salt 
  }
  
  Rdev_z = as.vector(scale(report$recruitment_log_Dev))
  
  if(report$SexAtSizeLamda>0){
    
    SSB = report$Spawning_stock_Biomass
    
  } else {
    
    SSB = report$Spawning_stock_Biomass_input
    
  }
  
  par(mfrow = c(1,2))
  par(mar = c(0, 5, 0, 1.1), oma = c(4, 1, 1, 2))
  
  plot(Year,Rdev_z,
       xlab = "Year",
       ylab = "Centered value",
       ylim = c(-3,3),
       type = 'o',
       col = 'blue',
       cex = 1.5,
       pch = 20, 
       lwd = 2,
       cex.lab = 1.5,
       cex.axis = 1.5)
  
  lines(Year, hsi,
        type = 'o',
        col = 'red',
        cex = 1.5,
        pch = 20, 
        lwd = 2,
        cex.lab = 1.5,
        cex.axis = 1.5,
        lty = 'dashed')
  
  if(d %in% c(1:3, 6:9)){
    legend(x=1982,
           y=3.3, cex = 1.5, lwd = 2,
           legend=c("Recruitment Deviation","Habitat Suitability"),
           lty=c(1,2),
           col = c("blue", "red"),
           bty = "n")
  }
  
  if(d == 4){
    legend(x=1982,
           y=3.3, cex = 1.5, lwd = 2,
           legend=c("Recruitment Deviation","Habitat Suitability 3-year Avg"),
           lty=c(1,2),
           col = c("blue", "red"),
           bty = "n")
  }
  
  if(d == 5){
    legend(x=1982,
           y=3.3, cex = 1.5, lwd = 2,
           legend=c("Recruitment Deviation","Habitat Suitability 5-year Avg"),
           lty=c(1,2),
           col = c("blue", "red"),
           bty = "n")
  }
  
  
  if(d == 10){
    legend(x=1982,
           y=3.3, cex = 1.5, lwd = 2,
           legend=c("Recruitment Deviation","Bottom Temperature"),
           lty=c(1,2),
           col = c("blue", "red"),
           bty = "n")
  }
  
  if(d == 11){
    legend(x=1982,
           y=3.3, cex = 1.5, lwd = 2,
           legend=c("Recruitment Deviation","Bottom Salinity"),
           lty=c(1,2),
           col = c("blue", "red"),
           bty = "n")
  }
  
  ccf(Rdev_z, hsi, 
      ylab = "Cross-correlation", 
      ylim = c(-0.7, 0.7),
      lwd = 2, cex.lab = 1.5, cex.axis = 1.5,
      # xlab = NA,
      main = "")
  
}

# SSB ---------------------------------------------------------------------
PlotSSB = function(){
  
  par(mar = c(0, 0, 0, 0))
  Year=seq(report$BeginYear,report$EndYear,1)
  
  if(report$SexAtSizeLamda>0){
    SSB=report$Spawning_stock_Biomass
  }else{
    SSB=report$Spawning_stock_Biomass_input
  }
  
  fit = data.frame(report$ fit$ names, report$ fit$ est, report$ fit$ std) 
  fit = fit[which(fit$report.fit.names == "sdr_vSSB"),]
  
  ci = 1.96*fit$report.fit.std
  
  par(mfrow = c(1,1), mar = c(4,4,1,1), mgp = c(2, 0.6, 0))
  
  plot(Year,SSB,xlab="Year",ylab="Spawning biomass (t)",type='o',axes = F, 
       ylim = c(min(SSB)*0.8,max(SSB)*1.2),
       col = 'black',cex = 1.5,pch = 20,lwd = 1.5,lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5)
  arrows(Year, SSB-ci, Year, SSB+ci, code = 3, angle = 90, length = 0.05, col = "black")
  axis(1); axis(2); box(bty = "L")
  
}


# Abundance at the beginning of the year ----------------------------------
PlotAbun = function() {
  
  library(lattice)
  Abun = report$Abundance_at_Size;Abun[,1:2] = 0
  Season = report$Time_step
  Year = seq(report$BeginYear,report$EndYear,1)
  
  AbunB = matrix(NA,ncol = 3,nrow = length(Year)*length(sizebins))
  itemp = 1
  
  for (i in Year)
  {
    iL = i-report$BeginYear+1
    
    for (j in 1:length(sizebins))
    {
      if (Season>1)
      {
        AbunB[itemp,]=c(i,sizebins[j],Abun[(iL-1)*Season+1,j])
        itemp=itemp+1
      }else
      {
        AbunB[itemp,]=c(i,sizebins[j],Abun[iL,j])
        itemp=itemp+1
      }
      
    }
  }
  AbunB<-as.data.frame(AbunB)    
  names(AbunB)=c("Year","CL","Abun")
  
  plot = xyplot(CL ~ Year,
                data = AbunB,
                layout = c(1,1),
                par.strip.text = list(cex = 1.3),
                as.table = T,
                subscript = T,
                z = AbunB[,3],
                main = paste("Beginning of year expected numbers at length"),
                scales = list(y = list(cex = 1.3), x = list(cex = 1.3)),
                panel = panel.bubble.for.R,
                scalem = 4,
                ylab = list(label = "Carapace Length (mm)",cex = 1.3),
                xlab = list(label = "Year", cex = 1.3))
  print(plot)  
}

# Bubble function ---------------------------------------------------------
panel.bubble.for.R<-function(x,y,z,subscripts,scalem=4,...){
  #panel.grid(col="lightgrey",lwd=1,h=-1,v=-1)
  cex<-abs(z) # size of points depends on the absolute value of z and is consistent across panels
  cex<-scalem*sqrt(cex/max(cex))
  flagplus<- z>=0
  panel.xyplot(x[flagplus],y[flagplus],pch=16,
               # col="gray",
               cex=cex[flagplus])
  panel.xyplot(x[!flagplus],y[!flagplus],pch=1,
               # col="gray",
               cex=cex[!flagplus])
}


# Survey Length Comp ------------------------------------------------------
PlotSLC <- function(){
  
  N_syear=Nyear
  DimIndexIndex=report$DimIndexIndex
  
  if(is.vector(DimIndexIndex)){
    
    N_index=1
    DimIndexIndex=matrix(DimIndexIndex,nrow = 1,ncol = length(DimIndexIndex))
    
  }else{
    
    N_index=nrow(DimIndexIndex)
  }
  
  Year=seq(report$BeginYear,report$EndYear,1)
  s_length=c();ind=c()
  
  ind_year = matrix(NA,nrow=N_index,ncol=2)
  
  ind[1] = 1
  
  # par(mfrow=c(6,30))
  
  for (s in 1:N_index){
    
    s_length[s] = sum(!is.na(DimIndexIndex[s,]))
    #SurveyESS_Obs[s,1:s_length[s]] = report$Survey_ESSinput[s,][1:s_length[s]]
    ind1 = c(DimIndexIndex[s,][1:s_length[s]])
    ind[s+1] = ind[s] + s_length[s]
    #Scomp_Obs[c(ind1),,s] = report$Survey_Comp_Obs[c(),]
    ind_year[s,] = c(Year[c(ind1)][1],Year[c(ind1)][s_length[s]])
    
    plot_nrow = ceiling(s_length[s]/6) 
    plot_ncol = 6
    yaxis = rep(1,plot_nrow)
    
    for(pl in 1:plot_nrow){
      
      yaxis[pl] = yaxis[1]+(pl-1)*plot_ncol
      
    }
    
    if (plot_nrow >1){
      
      xaxis = seq(s_length[s]-plot_ncol+1,s_length[s],1)
      
    }else{
      
      xaxis = seq(1,s_length[s],1)
      
    }
    
    par(mfrow=c(plot_nrow,plot_ncol))
    
    par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
    par(tcl = -0.25)
    par(mgp = c(2, 0.6, 0))
    
    down_pred = (s-1)*N_syear+ind_year[s,1]-report$BeginYear+1
    up_pred = down_pred+s_length[s]-1
    
    Scomp_Obs = report$Survey_Comp_Obs[ind[s]:(ind[s+1]-1),]
    Scomp_Pred = report$Survey_Comp_Pred[down_pred:up_pred,]
    
    year_plot = seq(ind_year[s,1],ind_year[s,2],1)
    
    for (yi in 1:s_length[s]){
      
      plot(sizebins,Scomp_Obs[yi,],type='l',ylim=c(0,0.25),axes=FALSE,lwd=1,col='gray')
      x=c(sizebins,rev(sizebins))
      y=c(rep(0,length(sizebins)),rev(Scomp_Obs[yi,]))
      polygon(x,y,col="gray",border = NA)
      mtext(paste(year_plot[yi]),side=3,line=-1,adj=0.1,cex=0.6)
      lines(sizebins,Scomp_Pred[yi,],col="black",lwd=2)
      #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
      if(yi %in% yaxis)
        axis(2,col='gray',at=seq(0,0.20,0.02))
      if(yi %in% xaxis)
        axis(1,col='gray',at=sizebins[-1])
      box(col='gray')
    }
    mtext('Length (mm)',side=1,outer=T,line=2.2)
    mtext('Proportion',side=2,outer=T,line=2.2)
    mtext(paste('Survey-',s,';black line = Pred',sep=''),side=3,outer=T,line=2.2)
  }
  
}

PlotSLC_n <- function(){
  
  N_syear=Nyear
  DimIndexIndex=report$DimIndexIndex
  
  if(is.vector(DimIndexIndex)){
    
    N_index=1
    DimIndexIndex=matrix(DimIndexIndex,nrow = 1,ncol = length(DimIndexIndex))
    
  }else{
    
    N_index=nrow(DimIndexIndex)
    
  }
  
  Year=seq(report$BeginYear,report$EndYear,1)
  s_length=c();ind=c()
  
  ind_year = matrix(NA,nrow=N_index,ncol=2)
  
  ind[1] = 1
  
  par(mfrow=c(7,30))
  par(mar = c(0, 0, 1, 0), oma = c(4, 4, 4, 5))
  
  for (s in 1:N_index){
    
    s_length[s] = sum(!is.na(DimIndexIndex[s,])) #number of survey years
    #SurveyESS_Obs[s,1:s_length[s]] = report$Survey_ESSinput[s,][1:s_length[s]]
    ind1 = c(DimIndexIndex[s,][1:s_length[s]])
    ind[s+1] = ind[s] + s_length[s]
    #Scomp_Obs[c(ind1),,s] = report$Survey_Comp_Obs[c(),]
    ind_year[s,] = c(Year[c(ind1)][1],Year[c(ind1)][s_length[s]]) #first and last year of a survey
    
    down_pred = (s-1)*N_syear+ind_year[s,1]-report$BeginYear+1
    up_pred = down_pred+s_length[s]-1
    year_plot = seq(1984,2013,1)
    
    
    if(s == 1){
      
      Scomp_Obs = report$Survey_Comp_Obs[ind[s]:(ind[s+1]-1),]
      Scomp_Pred = report$Survey_Comp_Pred[down_pred:up_pred,]
      
      missing_1 = matrix(0, ncol = 35, nrow = 22)
      missing_2 = matrix(0, ncol = 35, nrow = 1)
      
      Scomp_Obs = rbind(missing_1, Scomp_Obs, missing_2)
      Scomp_Pred = rbind(missing_1, Scomp_Pred, missing_2)
      
      for (i in 1:30){
        
        plot(sizebins,Scomp_Obs[i,],
             type = 'l', ylim = c(0, 0.25), xlim = c(53,200),
             axes=FALSE,lwd=1,col='gray', main = year_plot[i],cex.main=1.2)
        
        x=c(sizebins,rev(sizebins))
        y=c(rep(0,length(sizebins)),rev(Scomp_Obs[i,]))
        
        polygon(x,y,col="gray",border = NA)
        
        lines(sizebins,Scomp_Pred[i,],col="black",lwd=2)
        
        #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
        if(i == 1){
          
          axis(2,col='gray', at = 0.1)
          
        }
        
        if(i == 30){
          
          mtext(paste0("Survey_#", s),side=3, line=-2.5,adj=-1)
          
        }
        
        box(col='gray')
        
      }
      
    }else if(s == 6){
      
      Scomp_Obs = report$Survey_Comp_Obs[ind[s]:(ind[s+1]-1),]
      Scomp_Pred = report$Survey_Comp_Pred[down_pred:up_pred,]
      
      missing_1 = matrix(0, ncol = 35, nrow = 17)
      
      Scomp_Obs = rbind(missing_1, Scomp_Obs)
      Scomp_Pred = rbind(missing_1, Scomp_Pred)
      
      for (i in 1:30){
        
        plot(sizebins,Scomp_Obs[i,],
             type = 'l', ylim = c(0, 0.25), xlim = c(53,200),
             axes=FALSE,lwd=1,col='gray')
        
        x=c(sizebins,rev(sizebins))
        y=c(rep(0,length(sizebins)),rev(Scomp_Obs[i,]))
        
        polygon(x,y,col="gray",border = NA)
        
        lines(sizebins,Scomp_Pred[i,],col="black",lwd=2)
        
        #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
        
        if(i == 1){
          
          axis(2,col='gray', at = 0.1)
          
        }
        
        if(i == 30){
          
          mtext(paste0("Survey_#", s),side=3, line=-2.5,adj=-1)
          
        }
        
        box(col='gray')
        
      }
      
    }else if(s == 7){
      
      Scomp_Obs = report$Survey_Comp_Obs[ind[s]:(ind[s+1]-1),]
      Scomp_Pred = report$Survey_Comp_Pred[down_pred:up_pred,]
      
      missing_1 = matrix(0, ncol = 35, nrow = 16)
      
      Scomp_Obs = rbind(missing_1, Scomp_Obs)
      Scomp_Pred = rbind(missing_1, Scomp_Pred)
      
      for (i in 1:30){
        
        plot(sizebins,Scomp_Obs[i,],
             type = 'l', ylim = c(0, 0.25), xlim = c(53,200),
             axes=FALSE,lwd=1,col='gray')
        
        x=c(sizebins,rev(sizebins))
        y=c(rep(0,length(sizebins)),rev(Scomp_Obs[i,]))
        
        polygon(x,y,col="gray",border = NA)
        
        lines(sizebins,Scomp_Pred[i,],col="black",lwd=2)
        axis(1, col = 'gray', 
             # at = sizebins[1:20]
             at = c(90)
             
        )
        #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
        
        if(i == 1){
          
          axis(2,col='gray', at = 0.1)
          
        }
        
        if(i == 30){
          
          mtext(paste0("Survey_#", s),side=3, line=-2.5,adj=-1)
          
        }
        
        box(col='gray')
        
      }
      
    }else{
      
      Scomp_Obs = report$Survey_Comp_Obs[ind[s]:(ind[s+1]-1),]
      Scomp_Pred = report$Survey_Comp_Pred[down_pred:up_pred,]
      
      for (i in 1:30){
        
        plot(sizebins,Scomp_Obs[i,],
             type = 'l', ylim = c(0, 0.25), xlim = c(53,200),
             axes=FALSE,lwd=1,col='gray')
        
        x=c(sizebins,rev(sizebins))
        y=c(rep(0,length(sizebins)),rev(Scomp_Obs[i,]))
        
        polygon(x,y,col="gray",border = NA)
        
        lines(sizebins,Scomp_Pred[i,],col="black",lwd=2)
        
        #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
        
        if(i == 1){
          
          axis(2,col='gray', at = 0.1)
          
        }
        
        if(i == 30){
          
          mtext(paste0("Survey_#", s),side=3, line=-2.5,adj=-1)
          
        }
        
        box(col='gray')
        
      }
      
    }
    
    mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
    mtext('Proportion',side=2,outer=T,line=2.2)
    mtext("Year",side=3,outer=T,line=2.2)
  }
  
}

# Survey Length Comp aggregated -------------------------------------------
PlotSLCA=function(){
  
  survey=nrow(report$Survey_Comp_Pred)/report$N_year
 
   par(mfrow=c(2,4), 
      mar = c(2,2,2,2), #space around each plot
      oma = c(4,4,1,1), #space around the whole plotss
      tcl = -0.25, 
      mgp = c(2, 0.6, 0) #space between axis and axis label
      )
  
  Year=seq(report$BeginYear,report$EndYear,1)
  
  for (i in 1:survey) {
    
    if(i==1){
      SurveyESS_Obs=rep(0,report$N_year)
      SurveyESS_Pred=rep(0,report$N_year)
      Scomp_Obs=matrix(0,nrow=report$N_year,ncol=35)
      Scomp_Pred=matrix(0,nrow=report$N_year,ncol=35)
      
      Scomp_Obs=report$Survey_Comp_Obs[1:7,] #Ventless 2006-2012
      Scomp_Pred=report$Survey_Comp_Pred[23:30,]
      SurveyESS_Obs=report$Survey_ESSinput[1,1:7]
      SurveyESS_Pred=round(report$Survey_ESSpred[1,1:7])
      
      iL=0
      for(j in 1:nrow(Scomp_Obs))
      {
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
      
    }
    
    if(i==2){
      
      #Year=seq(1984,2013,1)
      SurveyESS_Obs=rep(0,report$N_year)
      SurveyESS_Pred=rep(0,report$N_year)
      Scomp_Obs=matrix(0,nrow=report$N_year,ncol=35)
      Scomp_Pred=matrix(0,nrow=report$N_year,ncol=35)
      
      Scomp_Obs=report$Survey_Comp_Obs[8:37,]
      Scomp_Pred=report$Survey_Comp_Pred[31:60,]
      SurveyESS_Obs=report$Survey_ESSinput[2,1:30]
      SurveyESS_Pred=round(report$Survey_ESSpred[2,1:30])
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    if(i==3){
      
      SurveyESS_Obs=rep(0,report$N_year)
      SurveyESS_Pred=rep(0,report$N_year)
      Scomp_Obs=matrix(0,nrow=report$N_year,ncol=35)
      Scomp_Pred=matrix(0,nrow=report$N_year,ncol=35)
      
      Scomp_Obs=report$Survey_Comp_Obs[38:67,]
      Scomp_Pred=report$Survey_Comp_Pred[61:90,]
      SurveyESS_Obs=report$Survey_ESSinput[2,1:30]
      SurveyESS_Pred=round(report$Survey_ESSpred[2,1:30])
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    if(i==4){
      
      SurveyESS_Obs = rep(0,report$N_year)
      SurveyESS_Pred = rep(0,report$N_year)
      Scomp_Obs = matrix(0,nrow = report$N_year,ncol = 35)
      Scomp_Pred = matrix(0,nrow = report$N_year,ncol = 35)
      
      Scomp_Obs = report$Survey_Comp_Obs[68:97,]
      Scomp_Pred = report$Survey_Comp_Pred[91:120,]
      SurveyESS_Obs = report$Survey_ESSinput[4,1:30]
      SurveyESS_Pred = round(report$Survey_ESSpred[4,1:30])
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    if(i==5){
      
      SurveyESS_Obs=rep(0,report$N_year)
      SurveyESS_Pred=rep(0,report$N_year)
      Scomp_Obs=matrix(0,nrow=report$N_year,ncol=35)
      Scomp_Pred=matrix(0,nrow=report$N_year,ncol=35)
      
      Scomp_Obs=report$Survey_Comp_Obs[98:127,]
      Scomp_Pred=report$Survey_Comp_Pred[121:150,]
      SurveyESS_Obs=report$Survey_ESSinput[5,1:30]
      SurveyESS_Pred=round(report$Survey_ESSpred[5,1:30])
      #N_syear=length(Year)
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    if(i==6){
      
      SurveyESS_Obs = rep(0,report$N_year)
      SurveyESS_Pred = rep(0,report$N_year)
      Scomp_Obs = matrix(0,nrow = report$N_year,ncol = 35)
      Scomp_Pred = matrix(0,nrow = report$N_year,ncol = 35)
      
      Scomp_Obs[18:30,] = report$Survey_Comp_Obs[128:140,]
      Scomp_Pred[18:30,] = report$Survey_Comp_Pred[168:180,]
      SurveyESS_Obs[18:30] = report$Survey_ESSinput[6,18:30]
      SurveyESS_Pred[18:30] = round(report$Survey_ESSpred[6,18:30])
      
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    if(i==7){
      
      SurveyESS_Obs=rep(0,report$N_year)
      SurveyESS_Pred=rep(0,report$N_year)
      Scomp_Obs=matrix(0,nrow=report$N_year,ncol=35)
      Scomp_Pred=matrix(0,nrow=report$N_year,ncol=35)
      
      Scomp_Obs[17:30,]=report$Survey_Comp_Obs[141:154,]
      Scomp_Pred[17:30,]=report$Survey_Comp_Pred[197:210,]
      SurveyESS_Obs[17:30]=report$Survey_ESSinput[7, 17:30]
      SurveyESS_Pred[17:30]=round(report$Survey_ESSpred[7, 17:30])
      
      
      iL=0
      
      for(j in 1:nrow(Scomp_Obs)){
        
        if(sum(Scomp_Obs[j,]>0))
          iL=iL+1
      }
    }
    
    #Scomp_Obs_temp=report$Survey_Comp_Obs[(Nyear*(i-1)+1):(Nyear*(i-1)+Nyear),]
    Scomp_Obs_A=apply(Scomp_Obs,2,sum)/iL
    #Scomp_Pred_temp=report$Survey_Comp_Pred[(Nyear*(i-1)+1):(Nyear*(i-1)+Nyear),]
    Scomp_Pred_A=apply(Scomp_Pred,2,sum)/iL
    
    SurveyESS_Obs=sum(SurveyESS_Obs)
    SurveyESS_Pred=sum(SurveyESS_Pred)
    
    plot(sizebins,Scomp_Obs_A,type='l',ylim=c(0,0.3),axes=FALSE,lwd=1,col="gray")
    x=c(sizebins,rev(sizebins))
    y=c(rep(0,length(sizebins)),rev(Scomp_Obs_A))
    polygon(x,y,col="gray",border=NA)
    
    if(i == 1) legend('topright',"VTS Q3",cex=1.2,bty='n')
    if(i == 2) legend('topright',"NEFSC Q2",cex=1.2,bty='n')
    if(i == 3) legend('topright',"NEFSC Q4",cex=1.2,bty='n')
    if(i == 4) legend('topright',"MA Q2",cex=1.2,bty='n')
    if(i == 5) legend('topright',"MA Q4",cex=1.2,bty='n')
    if(i == 6) legend('topright',"MENH Q2",cex=1.2,bty='n')
    if(i == 7) legend('topright',"MENH Q4",cex=1.2,bty='n')
    
    # rp=vector('expression',2)
    # rp[1]=substitute(expression(N==Value),list(Value=SurveyESS_Obs))[2]
    # rp[2]=substitute(expression(effN==Value),list(Value=SurveyESS_Pred))[2]
    # legend('topright',legend=rp,bty='n',y.intersp=0.8)
    
    lines(sizebins,Scomp_Pred_A,col='black',lwd=2)
    
    # if(i %in% c(1,5))
      axis(2)
    # axis(2,col='gray',at=seq(0,0.3,0.1))
    
    # if(i %in% c(5:7))
      axis(1)
    # axis(1,col='gray',at=sizebins)
    
    box(bty = "L")
    
  }
  mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
  mtext('Proportion',side=2,outer=T,line=2.2)
  # mtext(paste('Length Comp,','aggregated across time by survey,',' black line = Pred'),
  #       side=3,outer=T,line=2.2)
}

# Survey index ------------------------------------------------------------
PlotSI=function(){
  
  # if(n>2){
  #   par(mfrow=c(2,2))
  # }else{
  #   par(mfrow=c(1,2))
  # }
  
  par(mfrow=c(2,4), 
      mar = c(3,3,1,1), 
      oma = c(2,2,1,1))
  
  # survey=1
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  SI_Obs[23:29]=report$Survey_Index_Obs[1,1:7]
  SI_Pred[23:29]=report$Survey_Index_Pred[1,23:29]
  SI_Obs = SI_Obs[23:29]
  SI_Pred = SI_Pred[23:29]
  Year = Year[23:29]
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='VTS Q3')
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],
         SI_Obs,
         pch=16,col='red',
         cex=1.0)
  axis(1); axis(2); box(bty = "L")
  legend('topleft',c('Obs','Pred'),pch=c(16,16),
         col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=2
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  SI_Obs[1:30]=report$Survey_Index_Obs[2,1:30]
  SI_Pred[1:30]=report$Survey_Index_Pred[2,1:30]
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='NEFSC Q2')
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  axis(1); axis(2); box(bty = "L")
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=3
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  
  SI_Obs[1:30]=report$Survey_Index_Obs[3,1:30]
  SI_Pred[1:30]=report$Survey_Index_Pred[3,1:30]
  
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='NEFSC Q4')
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  axis(1); axis(2); box(bty = "L")
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=4
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  SI_Obs[1:30]=report$Survey_Index_Obs[4,1:30]
  SI_Pred[1:30]=report$Survey_Index_Pred[4,1:30]
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='MA Q2')
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  axis(1); axis(2); box(bty = "L")
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=5
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  SI_Obs[1:30]=report$Survey_Index_Obs[5,1:30]
  SI_Pred[1:30]=report$Survey_Index_Pred[5,1:30]
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='MA Q4')
  axis(1); axis(2); box(bty = "L")
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=6
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
  SI_Obs[18:30]=report$Survey_Index_Obs[6,1:13]
  SI_Pred[18:30]=report$Survey_Index_Pred[6,18:30]
  SI_Obs = SI_Obs[18:30]
  SI_Pred = SI_Pred[18:30]
  Year = Year[18:30]
  min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='MENH Q2')
  axis(1); axis(2); box(bty = "L")
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  # survey=7
  Year=seq(report$BeginYear,report$EndYear,1)
  SI_Obs=rep(0,report$N_year)
  SI_Pred=rep(0,report$N_year)
    SI_Obs[17:30]=report$Survey_Index_Obs[7,1:14]
  SI_Pred[17:30]=report$Survey_Index_Pred[7,17:30]
    SI_Obs = SI_Obs[17:30]
  SI_Pred = SI_Pred[17:30]
  Year = Year[17:30]
    min_ylim = min(min(SI_Obs, na.rm = T),min(SI_Pred, na.rm = T))*0.5
  max_ylim = max(max(SI_Obs, na.rm = T),max(SI_Pred, na.rm = T))*1.1  
  plot(Year,SI_Pred,
       # ylab=paste('Index',sep=''),
       ylab = "", xlab = "",
       type='l',lwd=2, axes = F,
       col='black',cex=1.1,
       ylim=c(min_ylim,max_ylim),
       main='MENH Q4')
  axis(1); axis(2); box(bty = "L")
  #legend(2014,15,paste('Survey',survey),bty='n')
  points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.0)
  # legend('topleft',c('Obs','Pred'),pch=c(16,16),
  #        col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  
  mtext('Year',side=1,outer=T)
  mtext('Survey Index',side=2, outer=T)

}

PlotSIESS=function()
{
  par(mfrow=c(3,1))
  par(mar = c(4, 4, 1, 0.5))
  survey=1
  
  Year=seq(1984,2008,1)
  SI_Obs=log(report$Survey_Index_Obs[1,][1:25])
  SI_Pred=log(report$Survey_Index_Pred[1,][1:25])
  SIESS_input=report$Survey_ESSinput[1,][8:25]
  SIESS_pred=report$Survey_ESSpred[1,][8:25]
  scaler=1.3
  
  plot(SIESS_input[1:length(SI_Obs)],SIESS_pred[1:length(SI_Obs)],type='p',pch=16,
       xlab='Input effective sample size',ylab='Estimated effective sample size')
  legend('topleft',paste('Survey',survey),bty='n')  
  #lines(SIESS_input,SIESS_input)
  
  survey=2
  
  Year=seq(1984,2013,1)
  SI_Obs=log(report$Survey_Index_Obs[2,])
  SI_Pred=log(report$Survey_Index_Pred[2,])
  SIESS_input=report$Survey_ESSinput[2,]
  SIESS_pred=report$Survey_ESSpred[2,]
  scaler=1.3
  
  plot(SIESS_input[1:length(SI_Obs)],SIESS_pred[1:length(SI_Obs)],type='p',pch=16,
       xlab='Input effective sample size',ylab='Estimated effective sample size')
  legend('topleft',paste('Survey',survey),bty='n') 
  #lines(SIESS_input,SIESS_input)
  
  survey=3
  
  Year=seq(2009,2012,1)
  SI_Obs=log(report$Survey_Index_Obs[3,][1:4])
  SI_Pred=log(report$Survey_Index_Pred[3,][26:29])
  SIESS_input=report$Survey_ESSinput[3,][1:4]
  SIESS_pred=report$Survey_ESSpred[3,][26:29]
  scaler=2
  
  plot(SIESS_input[1:length(SI_Obs)],SIESS_pred[1:length(SI_Obs)],type='p',pch=16,
       xlab='Input effective sample size',ylab='Estimated effective sample size')
  legend('topleft',paste('Survey',survey),bty='n')   
  #lines(SIESS_input,SIESS_input)
  
  #if(survey==4)
  
  #Year=seq(2009,2012,1)
  #SI_Obs=report$Survey_Index_Obs[4,][1:4]
  #SI_Pred=report$Survey_Index_Pred[4,][26:29]
  #SIESS_input=report$Survey_ESSinput[4,][1:4]
  #SIESS_pred=report$Survey_ESSpred[4,][26:29]
  
  
  #Year=seq(report$BeginYear,report$EndYear,1)
  #SI_Obs=report$Survey_Index_Obs#[survey,]
  #SI_Pred=report$Survey_Index_Pred[1:length(SI_Obs)]#[survey,]
  #SIESS_input=report$Survey_ESSinput#[survey,]
  #SIESS_pred=report$Survey_ESSpred[1:length(SI_Obs)]#[survey,]
  
  
  #plot(Year,SI_Pred,ylab='Survey Index',type='b',pch=16,
  #col='black',cex=1.5,ylim=c(0,scaler*max(SI_Obs, na.rm = T)),
  #main=paste('Survey',survey))
  #points(Year[1:length(SI_Obs)],SI_Obs,pch=16,col='red',cex=1.3)
  #legend('topleft',c('Obs','Pred'),pch=c(16,16),col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
  #plot(SIESS_input[1:length(SI_Obs)],SIESS_pred[1:length(SI_Obs)],type='p',pch=16,
  #xlab='Observed sample size',ylab='Effective sample size')
  #lines(SIESS_input,SIESS_input)
}


# Total Catch -------------------------------------------------------------
PlotTC = function() {
  
  par(mfrow=c(1,1), 
      mar = c(1,4,1,1), 
      oma = c(1, 1.5, 0.1, 0.1), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  bioORnum = report$BiomassORNum
  
  if (sum(bioORnum)==0) {
    
    ylab=paste('Catch (million)')
    
  }else{
    
    ylab='Catch (mt)'
  }
  
  total_catch_obs=apply(report$Catch_Obs,2,sum)
  total_catch_pred=apply(report$Catch_Pred,2,sum)
  
  ylim=max(max(total_catch_obs),max(total_catch_pred))*1.2
  
  Year=seq(report$BeginYear,report$EndYear,1)
  
  plot(Year,
       total_catch_pred,type='l',
       lwd=2,col='black',
       xlab='Year',
       ylab=ylab,
       # main='Total catch',
       axes = F,
       # ylim=c(0,ylim),
       # ylim=c(0,80000), 
       cex.lab = 1.5)
  
  points(Year,total_catch_obs,pch=20,col='red',cex=1.5)
  axis(1); axis(2); box(bty = "L")
  legend('topleft',c('Observation','Prediction'),
         lty=c(NA,1),pch=c(20,NA),lwd=c(NA,3),cex=1.0,
         col=c('red','black'),bty='n',y.intersp=0.8,xjust=1)
  
}

# Fishery-specific Catch --------------------------------------------------
Plot_f_c = function(){
  
  par(mfrow=c(2,2), 
      mar = c(2, 0.5, 2, 2), 
      oma = c(2, 4, 0.5, 0), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  PlotC(1) # plot fishery-specific catch
  
}

PlotC = function(fishery){
  
  Season = report$Time_step
  Year = seq(report$BeginYear,report$EndYear,1)
  bioORnum = report$BiomassORNum
  
  if (sum(bioORnum) == 0) {
    
    ylab=paste('Catch (million)')
    
  } else {
    
    ylab='Catch (mt)'
    
  }
  
  if (Season>1) {
    
    ylim = ceiling(max(max(report$Catch_Obs),max(report$Catch_Pred)*1.2))
    
    for (i in 1:Season) {
      
      total_catch_obs=report$Catch_Obs[Season*(fishery-1)+i,]
      total_catch_pred=report$Catch_Pred[Season*(fishery-1)+i,]
      
      plot(Year,total_catch_pred,
           type='l',
           lwd=3,
           ylab = "",
           col='black',axes=F,
           # ylim=c(0,ylim),
           ylim = c(0,45000))
      points(Year,total_catch_obs,pch=20,col='red',cex=1.5)
      # legend('topleft',paste('Fishery',fishery,'Season',i),bty='n',cex=1.2)
      legend('topleft',paste('Season',i),bty='n',cex=1.2)
      
            # if (i==4) legend('bottomright',c('Pred','Obs'), 
      #                  lty=c(NA,1), pch=c(20,NA), lwd=c(NA,2), 
      #                  cex=1, col=c('red','black'),bty='n',
      #                  y.intersp=0.8,xjust=1)
      
      if (i == 1)
        mtext(paste(ylab),side=2,outer=T,line=2.2)
        # axis(2,col='black',at=seq(0,ylim,5000))

      # if (i %in% c(1:4))
      #   axis(1,col='black',at=Year)
      
      axis(1); axis(2); box(col='black', bty = "L")
      
    }
    
    # mtext('Year',side=1,outer=T,line=2.2)
    # mtext(paste(ylab),side=2,outer=T,line=2.2)
    
    #mtext(paste('Seasonal catch,','fishery',fishery),side=3,outer=T,line=2.2,cex=1.5)
    
  } else   {
    
    par(mfrow=c(fishery,1))
    par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
    par(tcl = -0.25)
    par(mgp = c(2, 0.6, 0))
    ylim=ceiling(max(max(report$Catch_Obs),max(report$Catch_Pred)))
    for (i in 1:fishery)
    {
      total_catch_obs=report$Catch_Obs[i,]
      total_catch_pred=report$Catch_Pred[i,]
      plot(Year,total_catch_obs,type='l',lwd=3,col='black',axes=T,ylim=c(0,ylim))
      points(Year,total_catch_pred,pch=20,col='red',cex=1.5)
      legend('top',paste('Fishery',i),bty='n',cex=1.5)
      if (i==1)
        legend('topleft',c('Pred','Obs'),lty=c(NA,1),pch=c(20,NA),lwd=c(NA,3),cex=1,
               col=c('red','black'),bty='n',y.intersp=0.8,xjust=-1)
      if (i %in% seq(1,fishery,1))
        axis(2,col='gray',at=seq(0,ylim,50))
      if(i==fishery)
        axis(1,col='gray',at=Year)
      box(col='gray')
    }
    mtext('Year',side=1,outer=T,line=2.2)
    mtext(paste(ylab),side=2,outer=T,line=2.2)
    mtext(paste('Total catch by fleet'),side=3,outer=T,line=2.2,cex=1.5)
  }    
}

# Catch comp  -------------------------------------------------------------
PlotCC = function(fishery,season){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  Season = report$Time_step
  par(mar = c(1,1,0,1), 
      oma = c(4, 4, 4, 0.5), tcl = -0.25, mgp = c(2, 0.6, 0))
  
  if (Season>1) {
    
    temp = Nyear * Season * (fishery-1) + Nyear * (season-1)
    temp1 = Season * (fishery-1) + season
    Ccomp_Obs = report$Catch_Comp_Obs[(temp + 1):(temp + Nyear),]
    Ccomp_Pred = report$Catch_Comp_Pred[(temp + 1):(temp + Nyear),]
    CatchESS_Obs = report$Catch_ESSinput[temp1,]
    CatchESS_Pred = report$Catch_ESSpred[temp1,]
    
    if (Nyear<25) {
      
      par(mfrow=c(5,5))
      temp=(Nyear%/%5*(5-1)+Nyear%%5):(Nyear%/%5*(5-1)+5)
      yaxis=c(1,6,11,16,21)
      xaxis=c(21,22,23,24,25,temp)
      
    }
    
    if (Nyear>25) {
      
      par(mfrow=c(5,6))
      yaxis=c(1,7,13,19,25,31)
      xaxis=c(25,26,27,28,29,30)
      
    }
    
    if (Nyear>30) {
      
      par(mfrow=c(6,7))
      yaxis=c(1,7,13,19,25,31)
      xaxis=c(28,29,30,31,32,33)
      
    }
    
    if (fishery==1&season==1) {
      
      # Ccomp_Obs = Ccomp_Obs[1:Nyear,]
      # Ccomp_Pred = Ccomp_Pred[1:Nyear,]
      n_year = Nyear
      # Year = seq(1984,2000,1)
      
    }
    
    if (fishery==1&season==2) {
      
      # Ccomp_Obs = Ccomp_Obs[1:Nyear,]
      # Ccomp_Pred = Ccomp_Pred[1:Nyear,]
      n_year = Nyear
      # Year = seq(2000,report$EndYear,1)
      
    }
    
    if (fishery==1&season==3) {
      
      # Ccomp_Obs = Ccomp_Obs[17:Nyear,]
      # Ccomp_Pred = Ccomp_Pred[17:Nyear,]
      n_year = Nyear
      # Year = seq(2000,report$EndYear,1)
      
    }
    
    if (fishery==1&season==4) {
      
      # Ccomp_Obs = Ccomp_Obs[17:Nyear,]
      # Ccomp_Pred = Ccomp_Pred[17:Nyear,]
      n_year = Nyear
      # Year = seq(2000,report$EndYear,1)
      
    }
    
    yaxis = yaxis
    xaxis = xaxis
    
    for (i in 1:n_year) {
      
      plot(sizebins,Ccomp_Obs[i,],
           type = 'l',
           ylim = c(0,1),
           axes = F,
           lwd = 1,
           ylab = "", 
           xlab = "",
           col = 'gray')
      x = c(sizebins,rev(sizebins))
      y = c(rep(0,length(sizebins)),rev(Ccomp_Obs[i,]))
      polygon(x,y,col = "gray",border = NA)
      mtext(paste(Year[i]),side = 3,line = -1,adj = 0.1,cex = 1)
      #legend('topleft',paste(Year[i]),cex = 1.1,bty = 'n')
      #rp = vector('expression',2)
      #rp[1] = substitute(expression(N == Value),list(Value = CatchESS_Obs[i]))[2]
      #rp[2] = substitute(expression(effN == Value),list(Value = CatchESS_Pred[i]))[2]
      #legend('topright',legend = rp,bty = 'n',y.intersp = 0.8,yjust = 1)
      lines(sizebins,Ccomp_Pred[i,],col = 'black',lwd = 2)
      #text(x = 27,y = 0.15,paste("effN = ",SurveyESS_Pred[i]),cex = 1)
      
      if(i %in% yaxis)
        # axis(2,col='gray',at=seq(0,1,0.2))
        axis(2)
      
      if(i %in% xaxis)
        axis(1)
      # axis(1,col='gray',at=sizebins)
      
      box(bty = "L")
      
    }
    
    mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
    mtext('Proportion',side=2,outer=T,line=2.2)
    # mtext(paste('Length Comp,','fishery',fishery,'season',season,',black line=Pred'),
    #       side=3,outer=T,line=2.2)
    mtext(paste('Season',season),
          side=3,outer=T,line=2.2)
    
  } else {
    
    temp=Nyear*(fishery-1)
    Ccomp_Obs=report$Catch_Comp_Obs[(temp+1):(temp+Nyear),]
    Ccomp_Pred=report$Catch_Comp_Pred[(temp+1):(temp+Nyear),]
    CatchESS_Obs=report$Catch_ESSinput[fishery,]
    CatchESS_Pred=report$Catch_ESSpred[fishery,]
    
    if (Nyear<25) {
      
      par(mfrow=c(5,5))
      temp=(Nyear%/%5*(5-1)+Nyear%%5):(Nyear%/%5*(5-1)+5)
      yaxis=c(1,6,11,16,21)
      xaxis=c(21,22,23,24,25,temp)
      
    }
    
    if (Nyear>25) {
      
      par(mfrow=c(5,6))
      yaxis=c(1,7,13,19,25,31)
      xaxis=c(25,26,27,28,29,30)
      
    }
    
    for (i in 1:Nyear) {
      
      plot(sizebins,Ccomp_Pred[i,],type='l',ylim=c(0,1),axes=FALSE)
      x=c(sizebins,rev(sizebins))
      y=c(rep(0,length(sizebins)),rev(Ccomp_Pred[i,]))
      polygon(x,y,col="gray")
      mtext(paste(Year[i]),side=3,line=-1,adj=0.1,cex=0.6)
      #legend('topleft',paste(Year[i]),cex=1.1,bty='n')
      rp=vector('expression',2)
      rp[1]=substitute(expression(N==Value),list(Value=CatchESS_Obs[i]))[2]
      rp[2]=substitute(expression(effN==Value),list(Value=CatchESS_Pred[i]))[2]
      legend('topright',legend=rp,bty='n',y.intersp=0.8,yjust=1)
      lines(sizebins,Ccomp_Obs[i,],col='red')
      #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
      
      if(i %in% yaxis)
        axis(2,col='gray',at=seq(0,0.8,0.05))
      if(i %in% xaxis)
        axis(1,col='gray',at=sizebins)
      
      box(col='gray')
      
    }
    
    mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
    mtext('Proportion',side=2,outer=T,line=2.2)
    # mtext(paste('Length Comp,','fishery',fishery,',red line=Obs'),
    #       side=3,outer=T,line=2.2)
    
  }
  
}

PlotCC_n = function(){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  
  Season = report$Time_step
  
  par(mfrow=c(4,30), 
      mar = c(1,0.5,1,0.5), 
      oma = c(4, 4, 0.5, 0.5), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  fishery = 1
  
  yaxis=c(1,31,61,91)
  xaxis=c(91:120)
  
  for (j in 1:4){
    
    season = j
    
    temp = Nyear*Season*(fishery-1)+Nyear*(season-1)
    temp1 = Season*(fishery-1)+season
    Ccomp_Obs = report$Catch_Comp_Obs[(temp+1):(temp+Nyear),]
    Ccomp_Pred = report$Catch_Comp_Pred[(temp+1):(temp+Nyear),]
    CatchESS_Obs = report$Catch_ESSinput[temp1,]
    CatchESS_Pred = report$Catch_ESSpred[temp1,]
    
    n_year = Nyear
    
    yaxis = yaxis
    xaxis = xaxis
    
    for (i in 1:n_year) {
      
      plot(sizebins,
           Ccomp_Obs[i,],
           type='l',
           ylim=c(0,0.5), 
           xlim = c(63,150),
           axes=F,
           lwd=0.5,
           col='gray')
      x=c(sizebins,rev(sizebins))
      y=c(rep(0,length(sizebins)),rev(Ccomp_Obs[i,]))
      
      polygon(x,y,col="gray",border=NA)
      
      mtext(paste(Year[i]),side=3,line=-1,adj=0.1,cex=0.6)
      
      #legend('topleft',paste(Year[i]),cex=1.1,bty='n')
      
      #rp=vector('expression',2)
      
      #rp[1]=substitute(expression(N==Value),list(Value=CatchESS_Obs[i]))[2]
      
      #rp[2]=substitute(expression(effN==Value),list(Value=CatchESS_Pred[i]))[2]
      
      #legend('topright',legend=rp,bty='n',y.intersp=0.8,yjust=1)
      
      lines(sizebins,Ccomp_Pred[i,],col='black',lwd=2)
      
      #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
      
      if(i %in% yaxis)
        axis(2)
      # axis(2, col = 'gray', at = c(0.5))
      
      if(j == 4)
        axis(1)
      # axis(1, col = 'gray', at = c(80, 120))
      
      box(bty = "L")
    }
    
    mtext('Caparace Length (mm)',side=1,outer=T,line=2.2)
    mtext('Proportion',side=2,outer=T,line=2.2)
    # mtext('Length Composition by season, 1984-2013, black line = Prediction', side=3,outer=T,line=2.2)
  }
  
}

# Catch comp aggregated ---------------------------------------------------
PlotCCA = function(Nfishery){
  
  par(mfrow=c(Nfishery,1), 
      mar = c(0, 0, 0, 0), 
      oma = c(4, 4, 0.5, 0.5),
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  Season=report$Time_step
  
  for (i in 1:Nfishery){
    
    if (Season>1) {
      
      Ccomp_Obs_temp=report$Catch_Comp_Obs[(Nyear*Season*(i-1)+1):(Nyear*Season*(i-1)+Nyear*Season),]
      Ccomp_Obs_A=apply(Ccomp_Obs_temp,2,sum)/(report$N_year*Season)
      
      Ccomp_Pred_temp=report$Catch_Comp_Pred[(Nyear*Season*(i-1)+1):(Nyear*Season*(i-1)+Nyear*Season),]
      Ccomp_Pred_A=apply(Ccomp_Pred_temp,2,sum)/(report$N_year*Season)
      
      CatchESS_Obs=sum(report$Catch_ESSinput[(Season*(i-1)+1):(Season*(i-1)+1),])
      CatchESS_Pred=round(sum(report$Catch_ESSpred[(Season*(i-1)+1):(Season*(i-1)+1),]),1)
      
    } else {
        
      Ccomp_Obs_temp=report$Catch_Comp_Obs[(Nyear*(i-1)+1):(Nyear*(i-1)+Nyear),]
      Ccomp_Obs_A=apply(Ccomp_Obs_temp,2,sum)/report$N_year
      
      Ccomp_Pred_temp=report$Catch_Comp_Pred[(Nyear*(i-1)+1):(Nyear*(i-1)+Nyear),]
      Ccomp_Pred_A=apply(Ccomp_Pred_temp,2,sum)/report$N_year
      
      CatchESS_Obs=sum(report$Catch_ESSinput[i,])
      CatchESS_Pred=round(sum(report$Catch_ESSpred[i,]),1)
    }
    
    plot(sizebins, Ccomp_Obs_A, type = 'l', 
         ylim = c(0, max(Ccomp_Obs_A)), 
         axes = F, 
         lwd = 1, 
         col = "gray")
    x = c(sizebins, rev(sizebins))
    y = c(rep(0, length(sizebins)), rev(Ccomp_Obs_A))
    polygon(x, y, col = "gray", border = NA)
    # legend('topleft', paste("Fishery", i), cex = 1.2, bty = 'n')
    # rp = vector('expression', 2)
    # rp[1] = substitute(expression(N == Value), list(Value = CatchESS_Obs))[2]
    # rp[2] = substitute(expression(effN == Value), list(Value = CatchESS_Pred))[2]
    # legend('topright', legend = rp, bty = 'n', y.intersp = 0.8)
    lines(sizebins, Ccomp_Pred_A, col = 'black', lwd = 2)
    abline(v = 82.6, col = 2, lwd = 3)
    abline(v = 128, col = 4, lwd = 3)    
    legend("topright", c("Minimum legal size", "Maximum legal size"), fill= c("red", "blue"), bty = "n")
    
    axis(2)
    # axis(2, col = 'gray', at = seq(0, 0.5, 0.05))
    
    if(i == Nfishery)
      axis(1)
    # axis(1, col = 'gray', at = sizebins)
    
    box(bty = "L")
    
  }
  mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
  mtext('Proportion',side=2,outer=T,line=2.2)
  # mtext(paste('Catch Length Comp,','aggregated across time by fleet',',black line=Pred'), side=3,outer=T,line=2.2)
  
}


# Sex COMP ----------------------------------------------------------------
PlotSexComp = function(season,ylim_scaler){
  
  Year=seq(report$BeginYear,report$EndYear,1)
  Season=season
  
  if(Nyear<25){
    par(mfrow=c(5,5))
    temp=(Nyear%/%5*(5-1)+Nyear%%5):(Nyear%/%5*(5-1)+5)
    yaxis=c(1,6,11,16,21)
    xaxis=c(21,22,23,24,25,temp)
  }
  
  if (Nyear>25){
    par(mfrow=c(6,5))
    yaxis=c(1,6,11,16,21,26)
    xaxis=c(26,27,28,29,30)
  }
  
  par(mar = c(0.5, 0, 0, 0.5), oma = c(4, 4, 0.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
  Abun=report$Abundance_at_Size[(Nyear*(Season-1)+1):(Nyear*Season),]
  
  if(season==1){Abun[,1:2]=0}
  
  Non_female=(1-report$FemaleProp_at_Size_obs)*Abun
  Female=report$FemaleProp_at_Size_obs*Abun
  
  ylim=ylim_scaler*max(Abun)
  
  for (i in 1:Nyear){
    
    plot(sizebins,Non_female[i,],col='lightblue',type='l',ylim=c(0,ylim),axes=FALSE)
    xx=c(sizebins,rev(sizebins))
    yy=c(rep(0,length(sizebins)),rev(Non_female[i,]))
    polygon(xx, yy, col='lightblue',border=NA)  
    mtext(paste(Year[i]),side=3,line=-1,adj=0.1,cex=0.6)    
    yy2 <- c(Non_female[i,], rev(Female[i,]) + rev(Non_female[i,])) 
    polygon(xx, yy2, col='lightpink',border=NA) 
    if(i %in% yaxis)
      axis(2,col='gray',at=seq(0,ylim,50))
    if(i %in% xaxis)
      axis(1,col='gray',at=sizebins)
    box(col='gray')
  }
  mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
  mtext('Abundance (million)',side=2,outer=T,line=2.2)
  # mtext(paste('Numbers at stage and size'),side=3,outer=T,line=2.2)
}



# Sex COMP Fit ------------------------------------------------------------
PlotFfit=function(){
  
  Year=seq(report$BeginYear,report$EndYear,1)
  
  Non_female=report$FemaleProp_at_Size
  Non_female_obs=report$FemaleProp_at_Size_obs
  
  Scomp_Obs=Non_female_obs
  Scomp_Pred=Non_female
  
  if (Nyear<25){
    par(mfrow=c(5,5))
    temp=(Nyear%/%5*(5-1)+Nyear%%5):(Nyear%/%5*(5-1)+5)
    yaxis=c(1,6,11,16,21)
    xaxis=c(21,22,23,24,25,temp)
  }
  
  if (Nyear>25){
    par(mfrow=c(6,5))
    yaxis=c(1,6,11,16,21,26)
    xaxis=c(26,27,28,29,30)
  }
  
  par(mar = c(0.5, 0, 0, 0.5),
      oma = c(4, 4, 4, 0.5), 
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  for (i in 1:Nyear){
    
    plot(sizebins,Scomp_Pred[i,],type='n',ylim=c(0,1),axes=FALSE)
    x=c(sizebins,rev(sizebins))
    y=c(rep(0,length(sizebins)),rev(Scomp_Pred[i,]))
    # polygon(x,y,col="gray")
    mtext(paste(Year[i]),side=3,line=-1,adj=0.1,cex=0.6)
    #legend('topleft',paste(Year[i]),cex=1.1,bty='n')
    #rp=vector('expression',2)
    #rp[1]=substitute(expression(N==Value),list(Value=SurveyESS_Obs[i]))[2]
    #rp[2]=substitute(expression(effN==Value),list(Value=SurveyESS_Pred[i]))[2]
    #legend('topright',legend=rp,bty='n',y.intersp=0.8,yjust=1)
    lines(sizebins,Scomp_Obs[i,],col='red', lwd = 2)
    #text(x=27,y=0.15,paste("effN=",SurveyESS_Pred[i]),cex=1)
    if(i %in% yaxis)
      axis(2,col='gray',at=seq(0,1,0.5))
    if(i %in% xaxis)
      axis(1,col='gray',at=sizebins)
    box(col='gray')
    
  }
  
  mtext('Carapace Length (mm)',side=1,outer=T,line=2.2)
  mtext('Proportion',side=2,outer=T,line=2.2)
  # mtext(paste('Proportion of sex for a give size,','red line=Obs'), side=3,outer=T,line=2.2)
}

# L50 ---------------------------------------------------------------------
PlotLfifty = function() {
  
  par(mfrow = c(1,1), mar = c(4, 4.5, 2, 0.5))

  Year = seq(report$BeginYear,report$EndYear,1)
  L50 = report$Lfifty
  #par(mfrow = c(2,1))
  plot(Year,L50,xlab = "Year",
       ylab = "Size (mm)",type = 'o',
       col = 'red',
       cex = 2,
       pch = 16,
       lty = 'dashed',
       cex.lab = 1.3, 
       cex.axis = 1.3, 
       cex.main = 1.3, 
       cex.sub = 1.3,
       main = 'L50')
}

# Stock status ------------------------------------------------------------
PlotStockStatus=function() {
  
  par(mfrow=c(1,1), mar = c(4,4,1,1))
  Year=seq(report$BeginYear,report$EndYear,1)
  f=report$Fishing_Mortality 
  N=report$Abundance_at_Size[1:Nyear,]
  W=report$Weight_length
  B=apply(N*W,1,sum)
  R=report$Mean_Recruitment
  
  F=apply(f,2,sum)
  Fmax=report$Fmax
  F0.1=report$F0.1
  Fmsy=report$FMSY
  F30=report$F30SPR
  Bmsy=report$Bmsy
  
  SSB=report$Spawning_stock_Biomass
  SSBmsy=report$SSBmsy
  
  xlim=1*max(SSB,SSBmsy)
  ylim=max(F)
  #ylim=1*max(F,Fmax,F0.1,Fmsy,F30)
  
  status = data.frame(Year, SSB, F)
  
  plot(SSB,F,
       type='l',
       col='gray', 
       pch = 20, 
       xlim=c(xlim*0.3,xlim*1.1),
       ylim=c(min(F)-0.1,ylim+0.1),
       # xlim=c(20000, 70000),
       # ylim=c(0.5,1.5),
       xlab='Spawning Stock Biomass (t)',
       ylab='Fishing Mortality')
  
  text(x = status$SSB, y = status$F, labels  = status$Year, pos = 4)
  
  # abline(h=Fmsy,col='red',lwd=2)
  # abline(v=Bmsy,col='blue',lwd=2)
  # text(0.9*xlim,Fmsy,'Fmsy')
  # text(Bmsy,0.9*ylim,'Bmsy')
  # text(1700,0.1,'2013')
}


# Stage specific biomass --------------------------------------------------
PlotSpB = function(season){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  W = report$Weight_length
  Abun = report$Abundance_at_Size[(Nyear*(season-1)+1):(Nyear*season),]
  
  Non_female = apply((1-report$FemaleProp_at_Size_obs)*Abun*W,1,sum)
  Female = apply(report$FemaleProp_at_Size_obs*Abun*W,1,sum)
  
  Data = matrix(NA,ncol = length(Year),nrow = 2)
  Data[1,] = Non_female
  Data[2,] = Female
  
  # par(xpd = T, mar = par()$mar+c(2,2,1,1))
  par(mar = c(2,5,1,0.5) + 0.1)
  
  
  Abun = report$Abundance_at_Size[(Nyear*(4-1)+1):(Nyear*4),]
  Non_female = apply((1-report$FemaleProp_at_Size_obs)*Abun*W,1,sum)
  Female = apply(report$FemaleProp_at_Size_obs*Abun*W,1,sum)
  D = matrix(NA,ncol = length(Year),nrow = 2)
  D[1,] = Non_female
  D[2,] = Female
  max_y = max(D)
  
  barplot(Data, 
          main="", 
          ylab="", 
          # ylim = c(0,1.5*max_y),
          ylim = c(0,600000),
          col=topo.colors(2), 
          space=0.1, cex.axis=1, las=1,
          names.arg=Year, cex=1,border=NA) 
  legend('topright', paste0("Season ", season), cex=1.5, bty='n')
  
  if(season == 1){
  legend('topleft', c('Males','Females'), cex=1.5, fill=topo.colors(2),bty='n')
  mtext('Biomass (t)',side=2,outer=T,line=-1.5)
  }
  
}
PlotSpB_n = function(){
  
  Year = seq(report$BeginYear,report$EndYear,1)
  W = report$Weight_length
  
  Abun_1 = report$Abundance_at_Size[(Nyear*(1-1)+1):(Nyear*1),]
  Abun_2 = report$Abundance_at_Size[(Nyear*(2-1)+1):(Nyear*2),]
  Abun_3 = report$Abundance_at_Size[(Nyear*(3-1)+1):(Nyear*3),]
  Abun_4 = report$Abundance_at_Size[(Nyear*(4-1)+1):(Nyear*4),]
  
  Abun = Abun_1+Abun_2+Abun_3+Abun_4
  
  Non_female = apply((1-report$FemaleProp_at_Size_obs)*Abun*W,1,sum)
  Female = apply(report$FemaleProp_at_Size_obs*Abun*W,1,sum)
  
  Data = matrix(NA,ncol = length(Year),nrow = 2)
  Data[1,] = Non_female
  Data[2,] = Female
  max_y = max(Data)
  
  par(mfrow = c(1,1), mar = c(4,6,1,0.5))
  barplot(Data, 
          main="", 
          ylab="", 
          axes = F,
          ylim = c(0,2*max_y),
          # ylim = c(0,1900000),
          col=topo.colors(2), 
          space=0.1, cex.axis=1, las=1,
          names.arg=Year, cex=1,border=NA)
  axis(2, at = seq(0, 2*max_y, 200000), las = 1)
  
  legend('topleft', c('Males','Females'), cex=1.5, fill=topo.colors(2),bty='n')
  mtext('Biomass (t)',side=2,outer=T,line=-1.5)
}
# Retro ---------------------------------------------------------------
PlotRo_scale = function(beginYear,referenceYear,endYear,timestep) { 
 
   par(mar = c(4,3,1,1), 
       oma = c(3, 3, 2, 0.5), 
       mgp = c(2, 0.6, 0))
  
  Nyear=endYear-beginYear+1
  Year=seq(beginYear,endYear,1)
  Nrow=endYear-referenceYear+1
  Nthcol=Nyear-(endYear-referenceYear)
  SSB=matrix(NA,ncol=Nyear,nrow=Nrow)
  SSB_s=matrix(NA,ncol=Nyear,nrow=Nrow)
  Recruit=matrix(NA,ncol=Nyear,nrow=Nrow)
  ER=matrix(NA,ncol=Nyear,nrow=Nrow)
  Recruit_s=matrix(NA,ncol=Nyear,nrow=Nrow)
  ER_s=matrix(NA,ncol=Nyear,nrow=Nrow)
  
  par(mfrow=c(2,2))
  for (i in 1:Nrow){
    
    filename=paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", HSI, "/NSLSAP01_",beginYear,"_",endYear+1-i,"_",timestep,".rep",sep='')
    
    if(report$SexAtSizeLamda>0){
      SSB[i,1:(Nyear+1-i)]=read.rep(filename)$Spawning_stock_Biomass
    }else{
      SSB[i,1:(Nyear+1-i)]=read.rep(filename)$Spawning_stock_Biomass_input
    }
    SSB_s[i,1:(Nyear+1-i)]=SSB[i,1:(Nyear+1-i)]/SSB[1,1:(Nyear+1-i)]
    if (i==1){
      
      plot(Year,
           SSB_s[i,],
           type='l',
           lty=1,
           col=1,
           xlab='Year',
           lwd=2,
           ylab='Estiamte/Terminal year estimate',
           ylim = c(min(SSB_s[i,], na.rm = T)*-0.1, 
                    max(SSB_s[i,], na.rm = T)*2),
           main="SSB")
      points(Year[Nyear],SSB_s[i,Nyear],pch=16)
      
    }else{
      lines(Year,SSB_s[i,],lty=1,col=1,lwd=2)
      points(Year[Nyear-i+1],SSB_s[i,Nyear-i+1],pch=16)
    }
  }
  
  for (i in 1:Nrow){
    
    filename=paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", HSI, "/NSLSAP01_",beginYear,"_",endYear+1-i,"_",timestep,".rep",sep='')
    
    Recruit[i,1:(Nyear+1-i)]=read.rep(filename)$recruitment_Pred
    Recruit_s[i,1:(Nyear+1-i)]=Recruit[i,1:(Nyear+1-i)]/Recruit[1,1:(Nyear+1-i)]
    
    if (i==1){
      
      plot(Year,
           Recruit_s[i,],
           type = 'l',
           lty = 1,
           col = 1,
           xlab = 'Year',
           lwd = 2,
           ylab = 'Estiamte/Terminal year estimate',
           ylim = c(min(Recruit_s[i,], na.rm = T)*-0.1, 
                    max(Recruit_s[i,], na.rm = T)*2),
           main = "Recruitment")
      points(Year[Nyear],Recruit_s[i,Nyear],pch = 16)
      
    }else{
      
      lines(Year,Recruit_s[i,],lty = 1,col = 1,lwd = 2)
      points(Year[Nyear-i+1],Recruit_s[i,Nyear-i+1],pch=16)
      
    }
  }
  
  for (i in 1:Nrow){
    
    filename=paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", HSI, "/NSLSAP01_",beginYear,"_",endYear+1-i,"_",timestep,".rep",sep='')
    
    Prop_female=read.rep(filename)$FemaleProp_at_Size_obs
    
    catch1=read.rep(filename)$Catch_Pred[1,]*read.rep(filename)$Catch_Comp_Pred[1:(Nyear+1-i),]
    catch2=read.rep(filename)$Catch_Pred[2,]*read.rep(filename)$Catch_Comp_Pred[(Nyear+2-i):(2*(Nyear+1-i)),]
    catch3=read.rep(filename)$Catch_Pred[3,]*read.rep(filename)$Catch_Comp_Pred[(2*(Nyear+1-i)+1):(3*(Nyear+1-i)),]
    
    TCB=apply(read.rep(filename)$Weight_length[1:(Nyear+1-i),]*(catch1+catch2+catch3)*Prop_female,1,sum)
    
    TBF=read.rep(filename)$Abundance_at_Size[1:(Nyear+1-i),]*Prop_female*read.rep(filename)$Weight_length[1:(Nyear-i+1),]
    
    Exploitation_rate_TB=TCB/apply(TBF,1,sum)
    
    ER[i,1:(Nyear+1-i)]=Exploitation_rate_TB
    ER_s[i,1:(Nyear+1-i)]=ER[i,1:(Nyear+1-i)]/ER[1,1:(Nyear+1-i)]
    
    if (i==1){
      
      plot(Year,
           ER_s[i,],
           type='l',
           lty=1,
           col=1,
           xlab='Year',
           lwd=2,
           ylab='Estiamte/Terminal year estimate',
           ylim = c(min(ER_s[i,], na.rm = T)*-0.1, 
                    max(ER_s[i,], na.rm = T)*2),
           main="Exploitation rate")
      points(Year[Nyear],ER_s[i,Nyear],pch=16)
      
    }else{
      
      lines(Year,ER_s[i,],lty=1,col=1,lwd=2)
      points(Year[Nyear-i+1],ER_s[i,Nyear-i+1],pch=16)
      
    }
  }
  
}

PlotRo = function(beginYear, referenceYear, endYear, timestep){
  
  Nyear = endYear - beginYear + 1
  Year = seq(beginYear,endYear, 1)
  Nrow = endYear - referenceYear + 1
  Nthcol = Nyear - (endYear - referenceYear)
  SSB = matrix(NA, ncol = Nyear, nrow = Nrow)
  Recruit = matrix(NA, ncol = Nyear, nrow = Nrow)
  ER = matrix(NA, ncol = Nyear, nrow = Nrow)
  
  cl <- rainbow(Nrow)
  
  par(mfrow=c(3,1), 
      #par(tcl = -0.25)
      mgp = c(2, 0.6, 0),
      mar = c(4, 3, 1, 1), 
      oma = c(3, 3, 2, 0.5))
  
  #SSB
  for (i in 1:Nrow) {
    
    filename = paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", 
                     HSI, "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
    
    if(report$SexAtSizeLamda>0){
      
      SSB[i,1:(Nyear+1-i)] = read.rep(filename)$Spawning_stock_Biomass
      
    }else{
      
      SSB[i,1:(Nyear+1-i)] = read.rep(filename)$Spawning_stock_Biomass_input

    }
    
    if (i==1) {
      
      plot(Year, SSB[i,],
           type='l',
           # ylim = c(min(SSB[i,], na.rm = T)*0.1, max(SSB[i,], na.rm = T)*1.1),
           ylim = c(0, 80000),
           xlab='Year',
           ylab='Spawning biomass (t)')
      points(Year[Nyear],SSB[i,Nyear],pch=16)

    }else{
      
      lines(Year,SSB[i,],lty=1, col = cl[i])
      points(Year[Nyear-i+1], SSB[i,Nyear-i+1], pch=16, col = cl[i])
      
    }
  }
  
  Mohn_SSB=sum((SSB[,Nthcol]-SSB[Nrow,Nthcol])/SSB[,Nthcol])
  legend("topleft", paste0("Mohn = ", round(Mohn_SSB, 2)), bty = "n", cex = 1.5)
  print(Mohn_SSB)

  #R
  for (i in 1:Nrow){
    
    filename = paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", 
                     HSI, "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
    
    Recruit[i,1:(Nyear+1-i)]=read.rep(filename)$recruitment_Pred
    
    if (i==1){
      
      plot(Year,Recruit[i,],type='l',lty=1,col=1,
           # ylim = c(min(Recruit[i,], na.rm = T)*0.1, max(Recruit[i,], na.rm = T)*1.1),
           ylim = c(0, 2000),
           xlab='Year',
           ylab='Recruitment')
      points(Year[Nyear],Recruit[i,Nyear],pch=16)
      
    }else{
      
      lines(Year,Recruit[i,],lty=1,col = cl[i])
      points(Year[Nyear-i+1],Recruit[i,Nyear-i+1], pch=16, col = cl[i])

    }
  }
  
  Mohn_R=sum((Recruit[,Nthcol]-Recruit[Nrow,Nthcol])/Recruit[,Nthcol])
  legend("topleft", paste0("Mohn = ", round(Mohn_R, 2)), bty="n", cex = 1.5)
  print(Mohn_R)
  
  #Exploitation Rate
  for (i in 1:Nrow){
    
    filename=paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_",  
                   HSI,  "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
    
    Prop_female=read.rep(filename)$FemaleProp_at_Size_obs[1:(Nyear+1-i),]
    
    catch1=read.rep(filename)$Catch_Pred[1,]*read.rep(filename)$Catch_Comp_Pred[1:(Nyear+1-i),] 
    catch2=read.rep(filename)$Catch_Pred[2,]*read.rep(filename)$Catch_Comp_Pred[(Nyear+2-i):(2*(Nyear+1-i)),]
    catch3=read.rep(filename)$Catch_Pred[3,]*read.rep(filename)$Catch_Comp_Pred[(2*(Nyear+1-i)+1):(3*(Nyear+1-i)),]
    catch4=read.rep(filename)$Catch_Pred[4,]*read.rep(filename)$Catch_Comp_Pred[(3*(Nyear+1-i)+1):(4*(Nyear+1-i)),]
    
    TCB = apply(read.rep(filename)$Weight_length[1:(Nyear+1-i),]*(catch1+catch2+catch3+catch4)*Prop_female,1,sum)
    TBF = read.rep(filename)$Abundance_at_Size[1:(Nyear+1-i),]*Prop_female*read.rep(filename)$Weight_length[1:(Nyear-i+1),]
    Exploitation_rate_TB = TCB/apply(TBF,1,sum)
    
    ER[i,1:(Nyear+1-i)]=Exploitation_rate_TB
    
    if (i==1) {
      
      plot(Year,ER[i,],
           type='l',
           lty=1,col=1,
           xlab='Year',
           ylab='Exploitation rate (%)',
           ylim=c(min(ER[i,], na.rm = T)*0.1, max(ER[i,], na.rm = T)*1.1))
           # ylim = c(0, 100))
      
      points(Year[Nyear],ER[i,Nyear],pch=16)
      
    }else{
      
      lines(Year,ER[i,],lty=1,col=1)
      points(Year[Nyear-i+1],ER[i,Nyear-i+1],pch=16)
    }
  }
  
  Mohn_ER=sum((ER[,Nthcol]-ER[Nrow,Nthcol])/ER[,Nthcol])
  legend("topleft", paste0("Mohn = ", round(Mohn_ER, 2)), bty="n", cex = 1.5)
  print(Mohn_ER)
  
}

PlotRo_n = function(beginYear, referenceYear, endYear, timestep, type){
  
  Nyear = endYear - beginYear + 1
  Year = seq(beginYear,endYear, 1)
  Nrow = endYear - referenceYear + 1
  Nthcol = Nyear - (endYear - referenceYear)
  SSB = matrix(NA, ncol = Nyear, nrow = Nrow)
  Recruit = matrix(NA, ncol = Nyear, nrow = Nrow)
  ER = matrix(NA, ncol = Nyear, nrow = Nrow)
  Fmort = matrix(NA, ncol = Nyear, nrow = Nrow)
  
  
  par(mfrow=c(1,1), mar = c(4,5,2,2))
  
  library(icesAdvice)
  
  #SSB
  if (type == 1){
    
    for (i in 1:Nrow) {
      
      filename = paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", 
                       HSI, "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
      
      if(report$SexAtSizeLamda>0){
        SSB[i,1:(Nyear+1-i)] = read.rep(filename)$Spawning_stock_Biomass
      }else{
        SSB[i,1:(Nyear+1-i)] = read.rep(filename)$Spawning_stock_Biomass_input
      }
      
    }
    
    mSSB = data.frame(t(SSB))
    colnames(mSSB) = c("Base", paste0("-", 1:(Nrow-1)))
    rownames(mSSB) = Year
    mohn(mSSB, details = T, plot = T, pch = 26, axes = F, lwd = 2, ylim = c(0, 80000))
    axis(1, cex.axis = 1.5); axis(2, cex.axis = 1.5)
    title(xlab="Year", ylab="SSB", cex.lab = 1.5)
    box( bty = "L")
    ssb_rho = mohn(mSSB)
    legend("topleft", paste0("Mohn = ", round(ssb_rho, 4)), bty = "n", cex = 1.5)
    # lines(as.numeric(rownames(mSSB)), mSSB$Base, lwd=4)
    print(mohn(mSSB, details = T))
    
  }
  
  #R
  if (type == 2){
    
    for (i in 1:Nrow){
      filename = paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", 
                       HSI, "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
      Recruit[i,1:(Nyear+1-i)]=read.rep(filename)$recruitment_Pred
    }
    
    mR = data.frame(t(Recruit))
    colnames(mR) = c("Base", paste0("-", 1:(Nrow-1)))
    rownames(mR) = Year
    mohn(mR, details = T, plot = T, pch = 26, axes = F, lwd = 2, ylim = c(0, 2000))
    axis(1, cex.axis = 1.5); axis(2, cex.axis = 1.5)
    title(xlab="Year", ylab="Recruitment", cex.lab = 1.5)
    box(bty = "L")
    r_rho = mohn(mR)
    legend("topleft", paste0("Mohn = ", round(r_rho, 4)), bty = "n", cex = 1.5)
    # lines(as.numeric(rownames(mR)), mR$Base, lwd=4)
    print(mohn(mR, details = T))
    
  }
  
  #Exploitation Rate
  if (type == 3){
   
     for (i in 1:Nrow){
      
      filename=paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_",  
                     HSI,  "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')
      
      rep = read.rep(filename)
      
      Prop_female=rep$FemaleProp_at_Size_obs[1:(Nyear+1-i),]
      ABUN = rep$Abundance_at_Size[1:(Nyear+1-i),]
      
      catch1=rep$Catch_Pred[1,]*rep$Catch_Comp_Pred[1:(Nyear+1-i),] 
      catch2=rep$Catch_Pred[2,]*rep$Catch_Comp_Pred[(Nyear+2-i):(2*(Nyear+1-i)),]
      catch3=rep$Catch_Pred[3,]*rep$Catch_Comp_Pred[(2*(Nyear+1-i)+1):(3*(Nyear+1-i)),]
      catch4=rep$Catch_Pred[4,]*rep$Catch_Comp_Pred[(3*(Nyear+1-i)+1):(4*(Nyear+1-i)),]
      
      # TCB = apply(rep$Weight_length[1:(Nyear+1-i),]*(catch1+catch2+catch3+catch4)*Prop_female,1,sum)
      # TBF = rep$Abundance_at_Size[1:(Nyear+1-i),]*Prop_female*rep$Weight_length[1:(Nyear-i+1),]
      # Exploitation_rate_TB = TCB/apply(TBF,1,sum)
      
      TCB = apply(rep$Weight_length[1:(Nyear+1-i),]*(catch1+catch2+catch3+catch4),1,sum)
      TB = apply(rep$Weight_length[1:(Nyear+1-i),]*ABUN,1,sum)
      Exploitation_rate_TB=TCFB/TB
      
      ER[i,1:(Nyear+1-i)]=Exploitation_rate_TB
      
    }
    
    mER = data.frame(t(ER))
    colnames(mER) = c("Base", paste0("-", 1:(Nrow-1)))
    rownames(mER) = Year
    mohn(mER, details = T, plot = T, pch = 26, axes = F,  lwd = 2, ylim = c(0, 150))
    axis(1, cex.axis = 1.5); axis(2, cex.axis = 1.5)
    title(xlab="Year", ylab="Exploitation Rate (%)", cex.lab = 1.5)
    box(bty = "L")
    er_rho = mohn(mER)
    legend("topleft", paste0("Mohn = ", round(er_rho, 4)), bty = "n", cex = 1.5)
    # lines(as.numeric(rownames(mER)), mER$Base, lwd=4)
    print(mohn(mER, details = T))

  }
  
  #F
  if (type == 4){
    
    for (i in 1:Nrow){
      filename = paste("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_", 
                       HSI, "/NSLSAP01_", beginYear, "_", endYear+1-i, "_", timestep, ".rep", sep = '')

      Fmort[i,1:(Nyear+1-i)] = apply(read.rep(filename)$Fishing_Mortality, 2, sum)
      
      
    }
    
    mF = data.frame(t(Fmort))
    colnames(mF) = c("Base", paste0("-", 1:(Nrow-1)))
    rownames(mF) = Year
    mohn(mF, details = T, plot = T, pch = 26, axes = F, lwd = 2, ylim = c(0,2))
    axis(1, cex.axis = 1.5); axis(2, cex.axis = 1.5)
    title(xlab="Year", ylab="Fishing Mortality", cex.lab = 1.5); box(bty = "L")
    f_rho = mohn(mF)
    legend("topleft", paste0("Mohn = ", round(f_rho, 4)), bty = "n", cex = 1.5)
    # lines(as.numeric(rownames(mF)), mF$Base, lwd=4)
    print(mohn(mF, details = T))
    
  }
  
}

# Get likelihoods ---------------------------------------------------------
Getlike=function(){
  
  Total = report$likelihood_total
  TCatch = report$likelihood_tcatch
  PCatch = report$likelihood_pcatch
  Index = report$likelihood_index
  PINdex = report$likelihood_pindex
  Recruit = report$likelihood_Recruit
  PFeMal = report$likelihood_pfemal
  
  Likelihood_1 = data.frame(rbind(Total, TCatch, PCatch, Recruit, PFeMal))
  colnames(Likelihood_1) = "Likelihood"
  
  Likelihood_2 = data.frame(rbind(Index, PINdex))
  colnames(Likelihood_2) = c("Ventless Q3", "NEFSC Q2", "NEFSC Q4", "MA Q2", "MA Q4",  "MENH Q2", "MENH Q4")
  
  print(Likelihood_1)
  print(Likelihood_2)
  
  write.csv(Likelihood_1, "C:/Users/Kisei/Desktop/Likelihood_1.csv")
  write.csv(Likelihood_2, "C:/Users/Kisei/Desktop/Likelihood_2.csv")
  
  
}


# exploitation rate -------------------------------------------------------
PlotER = function(){
  
  options(scipen=999)
  
  ABUN = report$Abundance_at_Size[1:Nyear,]
  
  TN = apply(ABUN,1,sum) #total number by year
  Exploitation_rate_TN = apply(report$Catch_Pred,2,sum)/TN
  
  F_mort = apply(report$Fishing_Mortality[,1:Nyear], 2, sum)
  
  #female exploitation rate
  Prop_female=report$FemaleProp_at_Size_obs
  catch1=report$Catch_Pred[1,]*report$Catch_Comp_Pred[1:30,]*Prop_female
  catch2=report$Catch_Pred[2,]*report$Catch_Comp_Pred[31:60,]*Prop_female
  catch3=report$Catch_Pred[3,]*report$Catch_Comp_Pred[61:90,]*Prop_female
  catch4=report$Catch_Pred[4,]*report$Catch_Comp_Pred[91:120,]*Prop_female
  TCFB = apply(report$Weight_length*(catch1+catch2+catch3+catch4),1,sum) #total female catch in biomass
  TCFN = apply(catch1+catch2+catch3+catch4,1,sum) #total female catch in number
  TNF = ABUN*Prop_female #total female number
  TBF = ABUN*Prop_female*report$Weight_length #total female biomass
  TBNF = ABUN*(1-Prop_female)*report$Weight_length #total non-female biomass
  Exploitation_rate_TFB = TCFB/apply(TBF,1,sum)
  
  catch1 = report$Catch_Pred[1,]*report$Catch_Comp_Pred[1:30,]
  catch2 = report$Catch_Pred[2,]*report$Catch_Comp_Pred[31:60,]
  catch3 = report$Catch_Pred[3,]*report$Catch_Comp_Pred[61:90,]
  catch4 = report$Catch_Pred[4,]*report$Catch_Comp_Pred[91:120,]
  
  TCB = apply(report$Weight_length*(catch1+catch2+catch3+catch4),1,sum)
  TB = apply(report$Weight_length*ABUN,1,sum)
  Exploitation_rate_TB = TCFB/TB
  
  Exploitation_rate_CTB_TBF = TCB/apply(TBF,1,sum)
  Exploitation_rate_CTN_TNF = apply(report$Catch_Pred,2,sum)/apply(TNF,1,sum)
  Exploitation_rate_CTFB_TBF = Exploitation_rate_TFB
  
  x = report$BeginYear:report$EndYear
  my.settings <- list(par.main.text = list(font = 1, just = "left", x = grid::unit(5, "mm")))
  library(lattice)
  
  ER<-xyplot(Exploitation_rate_TN+Exploitation_rate_TFB+Exploitation_rate_TB~x,
             t = c("l","l","l"),
             lty=c(1,2,3),
             xlab=list(label='Year',cex=1.3),
             ylab=list(label='Exploitation rate (%)',cex=1.3),
             col.line = c('red', 'green',"blue"),lwd=5,
             scales=list(cex=1.3),
             par.settings=my.settings,
             main="red~TN; green~TFB; blue~TB")
  
  # ER<-xyplot(Exploitation_rate_TN+Exploitation_rate_TB~x,
  #            t=c("l","l"),lty=c(1,2),
  #            xlab=list(label='Year',cex=1.3),
  #            ylab=list(label='Exploitation rate (%)',cex=1.3),
  #            ylim = c(0,100),
  #            col.line = c('red',"blue"),lwd=2,
  #            scales=list(cex=1.3),par.settings=my.settings,
  #            main="red~Total Number; blue~Total Biomass")
  
  par(mar = c(4,4,1,1),
      tcl = -0.25, 
      mgp = c(2, 0.6, 0))
  
  plot(Exploitation_rate_TN~x, type = "o",
       ylab = 'Exploitation rate (%)',
       xlab = 'Year',
       ylim = c(0, 100), axes = F, 
       col = 'blue', cex = 1.5, pch = 20, lwd = 2, lty = 'dashed',
       cex.lab = 1.5, cex.axis = 1.5)
  
  lines(Exploitation_rate_TB~x, type = "o",
        col = 'red',cex = 1.5,pch = 20,lwd = 2,lty = 'dashed')
  
  #lines(Exploitation_rate_TFB~x, type = "o",
  # col = 'green',cex = 1.5,pch = 20,lwd = 2,lty = 'dashed')
  
  legend('topleft',c('Abundance','Biomass'),lwd=c(3,3),cex=1.0,
         col=c('red','blue'),bty='n',y.intersp=0.8,xjust=1)
  
  axis(1); axis(2); box(bty = "L")
  
  # Exploitation_rate_TN = data.frame(x,Exploitation_rate_TN)
  # Exploitation_rate_TN$Exploitation_Rate = "Total Number"
  # colnames(Exploitation_rate_TN) = c("Year", "Rate", "Type")
  # 
  # Exploitation_rate_TB = data.frame(x, Exploitation_rate_TB)
  # Exploitation_rate_TB$Exploitation_Rate = "Total Biomass"
  # colnames(Exploitation_rate_TB) = c("Year", "Rate", "Type")
  # 
  # Exploitation_rate_TFB = data.frame(x, Exploitation_rate_TFB)
  # Exploitation_rate_TFB$Exploitation_Rate = "Total Female Biomass"
  # colnames(Exploitation_rate_TFB) = c("Year", "Rate", "Type")
  # 
  # exp = rbind(Exploitation_rate_TB, Exploitation_rate_TN, Exploitation_rate_TFB)
  # exp = rbind(Exploitation_rate_TB, Exploitation_rate_TN)
  # 
  # library(ggplot2)
  # library(ggthemes)
  # 
  # ER = ggplot(exp, aes(Year, Rate, color = Type))+ 
  #   geom_line(size = 2) +
  #   xlab("Year") +
  #   ylab("Explotation Rate (%)") +
  #   # geom_smooth(size = 2, span = 0.5) +
  #   theme(text = element_text(size=20)) +
  #   scale_x_continuous(breaks=seq(1984, 2013)*1) +
  #   scale_color_manual("", values=c("blue", "red")) +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 20)) +
  #   theme(legend.position="top") +
  #   geom_rangeframe(colour = "black", lwd = 2) + 
  #   theme(panel.background = element_rect(fill = "white", colour = "white")) 
  # 
  # #print(c(Exploitation_rate_CTN_TNF[30],Exploitation_rate_CTB_TBF[30],Exploitation_rate_TFB[30]))
  # Bmsy=report$Bmsy
  # ratio=TB/Bmsy
  # print(ER)
}

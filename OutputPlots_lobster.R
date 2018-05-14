# choose model output -----------------------------------------------------

HSI = "n"

filename=paste0("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/lobster_b_",HSI,"/NSLSAP01")  
source("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/reptoRlist_lobster.r")
report<-read.admb(filename)

source("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/PlotFuncs_lobster.r")

# source("C:/Users/Kisei/Google Drive/Research/Size_Model/Programs/SaveOutputs.R")

# pdf("plots.pdf",width=16,height=6)
par(mfrow = c(1,2))
PlotWL_n(2013) # plot length-weight relationships
PlotML_n(2013) # plot maturity

Plot_GM_tb(1,50) # visualize growth matrices (user defines the number of year-specific time-blocks for growth and how many year to grow)

Plot_sel_f() # plot fleet selectivity
Plot_f() # plot fishing mortality
PlotTC() # plot total catch
Plot_f_c() # plot fishery-specific catch

PlotCC(1,1) # plot fishery-specific catch composition; specify fishery and season
PlotCC(1,2)
PlotCC(1,3)
PlotCC(1,4)
PlotCC_n()
PlotCCA(1) # plot aggregated fishery catch composition

par(mfrow = c(2,4))
PlotSelS(2000,1) # plot survey selectivity; specify year and survey
PlotSelS(2000,2)
PlotSelS(2000,3)
PlotSelS(2000,4)
PlotSelS(2000,5)
PlotSelS(2000,6)
PlotSelS(2000,7)

PlotSLC_n() # plot survey size composition
PlotSLCA() # plot aggregated survey size composition
PlotSI() # plot survey index;n= # of indices
PlotM(1990)

PlotR(1)
PlotR(2)
PlotR(3)

PlotR_HSI(1)#SP_SSB
PlotR_HSI(2)#FL_SSB
PlotR_HSI(3)#SP_R
PlotR_HSI(4)#SP_R_3year
PlotR_HSI(5)#SP_R_5year
PlotR_HSI(6)#FL_R
PlotR_HSI(7)#SP_R_GOM_GBK
PlotR_HSI(8)#SP_R_GOM_GBK_unweighted
PlotR_HSI(9)#SP_R_GOM_GBK_weighted
PlotR_HSI(10)#bottom temp
PlotR_HSI(11)#bottom salt

PlotSSB()
PlotAbun()

par(mfrow = c(2,2))
PlotSpB(1)
PlotSpB(2)
PlotSpB(3)
PlotSpB(4)

PlotSpB_n()

PlotSexComp(1,1.2)

PlotFfit()

PlotLfifty()
PlotER()

CompareR(1) #1 = basecase for manuscript, 2 = senstivity analysis
CompareF(1)
PlotR_HSI(3)

PlotStockStatus()

PlotRo_n(1984,2006,2013,4,1)
PlotRo_n(1984,2006,2013,4,2)
PlotRo_n(1984,2006,2013,4,4) # 3 = Exloitation rate, 4 = F

# PlotRo_scale(1984,2006,2013,4)
Getlike()



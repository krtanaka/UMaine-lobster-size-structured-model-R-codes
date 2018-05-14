jpeg(paste0("C:/Users/Kisei/Desktop/WL&ML-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
par(mfrow = c(1,2))
PlotWL_n(2013) # plot length-weight relationships
PlotML_n(2013) # plot maturity
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/GM_tb-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
Plot_GM_tb(1,50) # visualize growth matrices (user defines the number of year-specific time-blocks for growth and how many year to grow)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/sel_f-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
Plot_sel_f() # plot fleet selectivity
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/f-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
Plot_f() # plot fishing mortality
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/TC-", HSI, ".jpg"), width = 7, height = 7, res = 500, units = "in")
PlotTC() # plot total catch
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/f_c-", HSI, ".jpg"), width = 7, height = 7, res = 500, units = "in")
Plot_f_c() # plot fishery-specific catch
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/CC_1-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotCC(1,1) # plot fishery-specific catch composition; specify fishery and season
dev.off()
jpeg(paste0("C:/Users/Kisei/Desktop/CC_2-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotCC(1,2)
dev.off()
jpeg(paste0("C:/Users/Kisei/Desktop/CC_3-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotCC(1,3)
dev.off()
jpeg(paste0("C:/Users/Kisei/Desktop/CC_4-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotCC(1,4)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/CCA-", HSI, ".jpg"), width = 7, height = 4, res = 500, units = "in")
PlotCCA(1) # plot aggregated fishery catch composition
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/CC_n-", HSI, ".jpg"), width = 20, height = 8, res = 500, units = "in")
PlotCC_n() # plot aggregated fishery catch composition
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SelS-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
par(mfrow = c(2,4))
PlotSelS(2000,1) # plot survey selectivity; specify year and survey
PlotSelS(2000,2)
PlotSelS(2000,3)
PlotSelS(2000,4)
PlotSelS(2000,5)
PlotSelS(2000,6)
PlotSelS(2000,7)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SLC-", HSI, ".jpg"), width = 20, height = 10, res = 500, units = "in")
PlotSLC_n() # plot survey size composition
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SLCA-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotSLCA() # plot aggregated survey size composition
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SI-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotSI() # plot survey index;n= # of indices
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/M-", HSI, ".jpg"), width = 4, height = 4, res = 500, units = "in")
PlotM(2000)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/R1-", HSI, ".jpg"), width = 10, height = 5, res = 500, units = "in")
PlotR(1)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/R2-", HSI, ".jpg"), width = 10, height = 5, res = 500, units = "in")
PlotR(2)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/R3-", HSI, ".jpg"), width = 10, height = 10, res = 500, units = "in")
PlotR(3)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SSB-", HSI, ".jpg"), width = 8, height = 5, res = 500, units = "in")
PlotSSB()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Abun-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotAbun()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SpB-", HSI, ".jpg"), width = 8, height = 8, res = 500, units = "in")
par(mfrow = c(2,2))
PlotSpB(1)
PlotSpB(2)
PlotSpB(3)
PlotSpB(4)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SpB_", HSI, ".jpg"), width = 8, height = 8, res = 500, units = "in")
PlotSpB_n()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/SexComp-", HSI, ".jpg"), width = 10, height = 6, res = 500, units = "in")
PlotSexComp(1,1.2)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Ffit-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotFfit()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Lfifty-", HSI, ".jpg"), width = 4, height = 4, res = 500, units = "in")
PlotLfifty()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/ER-", HSI, ".jpg"), width = 16, height = 8, res = 500, units = "in")
PlotER()
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/R-", HSI, ".jpg"), width = 12, height = 5, res = 500, units = "in")
PlotR_HSI(3) #3 = SP_R
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/CompareR.jpg"), width = 5, height = 5, res = 500, units = "in")
CompareR(1) 
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/CompareF.jpg"), width = 5, height = 5, res = 500, units = "in")
CompareF(1) 
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/StockStatus-", HSI, ".jpg"), width = 8, height = 8, res = 500, units = "in")
PlotStockStatus()
dev.off()

# jpeg(paste0("C:/Users/Kisei/Desktop/Rho_", HSI, ".jpg"), width = 8, height = 8, res = 500, units = "in")
# PlotRo(1984,2006,2013,4)
# dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Rho_SSB_", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotRo_n(1984,2006,2013,4,1)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Rho_R_-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotRo_n(1984,2006,2013,4,2)
dev.off()

jpeg(paste0("C:/Users/Kisei/Desktop/Rho_ER_-", HSI, ".jpg"), width = 8, height = 4, res = 500, units = "in")
PlotRo_n(1984,2006,2013,4,4)
dev.off()

# jpeg(paste0("C:/Users/Kisei/Desktop/Ro_scale-", HSI, ".jpg"), width = 8, height = 8, res = 500, units = "in")
# PlotRo_scale(1984,2006,2013,4)
# dev.off()
Getlike()


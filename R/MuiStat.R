# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#使用案例
# result = MuiStat(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "box",plottype = "mui")
# result[[1]]


###-----------------------------------走一个流程--正态检验---方差齐性---是否方差检验如果多个变量实现这个自动化过程，应该怎么办？

# num = c(4,5,6)
# data = data_wt
# method_cv = "leveneTest"
# i = 1
# sig_show  = "abc"
# ncol = 2
MuiStat = function(data = data_wt,num = c(4,5,6),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 2,plot = "bar",plottype = "mui"){
  Mytheme <- theme_bw()+

    # scale_fill_manual(values = mi, guide = guide_legend(title = NULL))+
    theme(

      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),

      plot.title = element_text(vjust = -8.5,hjust = 0.1),
      axis.title.y =element_text(size = 20,face = "bold",colour = "black"),
      axis.title.x =element_text(size = 24,face = "bold",colour = "black"),
      axis.text = element_text(size = 20,face = "bold"),
      axis.text.x = element_text(colour = "black",size = 14),
      axis.text.y = element_text(colour = "black",size = 14),
      legend.text = element_text(size = 15,face = "bold"),
      legend.position = "none"#是否删除图例

    )

  data_wt = data
  norCv = MuiNorCV(data = data_wt,num = c(4,5,6),method_cv = "leveneTest")
  norCv

  # 将符合正态分布和方差齐性的分为一组，将两者有一个不符合的分为一组；
  AA = c()
  BB = c()
  for (i in 1:length(num)) {

    if (norCv[,"cor"][i] == TRUE & norCv[,"CV"][i] == TRUE) {
      num[i]
      AA = c(AA,num[i])
      AA

    }

    if (norCv[,"cor"][i] == FALSE | norCv[,"CV"][i] == FALSE) {
      num[i]
      BB = c(BB,num[i])
      BB

    }

  }

  # AA
  # BB



  resultAA = MuiaovMcomper(data = data_wt,num = AA,method_Mc = method_Mc )
  resultAA

  resultBB = MuiKwWlx(data = data_wt,num = BB)
  resultBB

  resultall = cbind(resultAA,resultBB)
  resultall



  num = c(AA,BB)
  if (plottype == "single") {
    if (plot == "bar") {
      plot = MuiPlotresultBar(data = data_wt,num = num,result = resultall,sig_show = sig_show )
      p = "Folder"
    }

    if (plot == "box") {
      plot = MuiPlotresultBox(data = data_wt,num = num,result = resultall,sig_show = sig_show)
      p = "Folder"
    }
  }



  if (plottype == "mui") {
    if (plot == "bar") {
      result1 = FacetMuiPlotresultBar(data = data_wt,num = num,result = resultall,sig_show =sig_show,ncol = ncol )
      p = result1[[1]]
    }

    if (plot == "box") {
      result1 = FacetMuiPlotresultBox(data = data_wt,num = num,result = resultall,sig_show =sig_show,ncol =ncol )
      p = result1[[1]]

    }


  }

  return(list(p, aov = AA,wlx = BB,table =resultall ))


}

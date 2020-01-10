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


# # #使用案例
# result = MuiKwWlx(data = data_wt,num = c(4:6))
# result
# # #结果直接输出到文件夹中
# MuiPlotresultBar(data = data_wt,num = c(4:6),result = result ,sig_show ="line")


###-----------多组数据出图柱状图
# num = c(4:6)
# N =4
MuiPlotresultBar = function(data = data_wt,num = c(4:6),result = result,sig_show ="abc" ){
  data_wt = data
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

  for (N in num) {
    print(N)
    name = colnames(data_wt[N])

    as = result[match( name,colnames(result))]
    colnames(as) = "groups"
    as$group = row.names(as)

    PlotresultBar = aovMuiBarPlot(data = data_wt, i= N,sig_show =sig_show,result = as)
    p = PlotresultBar[[1]]
    name = colnames(data_wt[N])
    p
    if (dim(as)[1]>3){	p=p+theme(axis.text.x=element_text(angle=45,vjust=1, hjust=1))}
    path = "./Muibar/"
    dir.create(path)
    FileName <- paste(path,name,"_bar", ".pdf", sep = "_")
    ggsave(FileName, p, width = 8, height = 8)

  }
}

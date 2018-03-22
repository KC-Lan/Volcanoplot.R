#R_火山圖(雙變數上色,輸出PNG & PDF)


library(ggplot2)

input_file=read.csv("C:/Users/file location.csv")
cutoff_log10v=1
cutoff_log2fc=0.585
neg_cutoff_log2fc=-cutoff_log2fc
output_fg="Resultplot"
x_axis_max=3
y_axis_max=5

log2FC=input_file[3]
log10ES=input_file[2]



#ifelse(test, yes, no)
threshold <- as.factor(ifelse(abs(log2FC) >=cutoff_log2fc & log10ES >= cutoff_log10v,
ifelse(log2FC>= cutoff_log2fc,'Up','Down'),'Not'))


png(file= paste(output_fg,".png"),width=12,height=12,unit="cm", res=175)
g = ggplot(data=input_file, aes(x=log2FC, y=log10ES, color=threshold)) +
  scale_color_manual(values=c("blue","#4f4f4f","red")) +
    geom_point(alpha=0.8, size=1.3) + #alpha=0~1 (0:transparent,1:opaque)
  geom_vline(xintercept=c(-0.585,0.585),color="Red",linetype="dashed") +
  geom_hline(yintercept=1,color="Blue",linetype="dashed") +
  theme(legend.position = "none") +
  xlim(c(-(x_axis_max), x_axis_max)) + ylim(c(0, y_axis_max)) +
  xlab("log2 Fold Change") + ylab("log10 Expression Value")
  
g
dev.off()

pdf(file= paste(output_fg,".pdf"),width=800,height=600,paper='a4r')
g1 = ggplot(data=input_file, aes(x=log2FC, y=log10ES, color=threshold)) +
  scale_color_manual(values=c("blue","#4f4f4f","red")) +
  geom_point(alpha=1.0, size=2.5) + #alpha=0~1 (0:transparent,1:opaque)
  geom_vline(xintercept=c(-0.585,0.585),color="Red",linetype="dashed",size=1) +
  geom_hline(yintercept=1,color="Blue",linetype="dashed",size=1) +
  theme(legend.position = "none") +
  xlim(c(-(x_axis_max), x_axis_max)) + ylim(c(0, y_axis_max)) +
  xlab("log2 Fold Change") + ylab("log10 Expression Value")
  
g1
dev.off()

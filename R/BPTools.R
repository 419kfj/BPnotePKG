#
# iOSアプリのBPnoteのバックアップデータを図示するためのfunctions
#
# 2018-03-31 にBPTools.Rとして分離
#
# (c) kazuo fujimoto kazuo.fujimoto2007@gmail.com

#
# read BP csv and consitute to data frame
#

read_BP <- function(fname){
  bp <- read.csv(fname,fileEncoding = "sjis")
  #bp <- read_csv(fname,locale=locale(encoding="CP932"))

  bp[,1] <- as.Date(bp[,1]) # これで日付をDateオブジェクトとして扱える。
  names(bp)[c(3,4,7,8)] <- c("朝収縮期","朝拡張期","夜収縮期","夜拡張期")

  return(bp)
  #bp %>% mutate(日付=ymd(日付)) %>%
  #bp %>% mutate(日付=as.Date(bp$日付)) %>% dplyr::rename("朝収縮期"="朝最高血圧(mmHg)",
  #                                            "朝拡張期"="朝最低血圧(mmHg)",
  #                                            "夜収縮期"="晩最高血圧(mmHg)",
  #                                            "夜拡張期"="晩最低血圧(mmHg)",
  #                                            "朝脈拍"="朝脈拍(拍/分)",
  #                                            "晩脈拍"="晩脈拍(拍/分)",
  #                                            "体重"="体重(Kg)") -> .bp
}



#
# 散布図
#

drawbp <- function(.bp,rtitle){
  #  qplot(jitter(.bp[,4]),jitter(.bp[,3]),xlim=c(60,105),ylim=c(100,165),
  #  	col=2,main=rtitle,ylab="収縮期血圧 mmHg",xlab="拡張期血圧 mmHg")

  plot(jitter(.bp[,4]),jitter(.bp[,3]),xlim=c(60,105),ylim=c(100,165),
       pch=16,col=2,main=rtitle,ylab="収縮期血圧 mmHg",xlab="拡張期血圧 mmHg")
  lines(c(85,85),c(90,130))
  lines(c(50,85),c(130,130))
  points(jitter(.bp[,8]),jitter(.bp[,7]),xlim=c(60,105),ylim=c(100,165),
         pch=16,col=4)
  legend(90,120,c("起床時血圧","就寝前血圧"),col=c(2,4),pch=16,cex=0.7)
}

# 最終日、期間を指定し、散布図およびBoxPlotを描画する。

drbp2 <- function(end.d,period, bp){
  start.d <- as.Date(end.d) - period #'2012-08-01'
  title <- paste("From ",start.d, " to ", end.d)
  cond <- (bp[,1] >= start.d) & (bp[,1] <= end.d)
  .bp <- bp[cond,]
  (nrow(.bp))
  drawbp(.bp, title)

  boxplot(.bp[,c(3,4,7,8)],main =title,ylim=c(60,160),ylab="mmHg")
  abline(h=130,lty=2)
  abline(h=85,lty=2)
}

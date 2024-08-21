gg.gauge <- function(pos, sgn, breaks=c(0,30,70,100)) {
  require(ggplot2)
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  get.dirarrow <- function(pos, sgn) {
      th <- pi * (1 - c(pos + sgn*2, pos + sgn*2, pos + sgn*4) / 100)
      rr <- c(.75, .85, .8)
      data.frame(x=rr*cos(th), y=rr*sin(th))
  }
  ggplot()+
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_polygon(data=get.dirarrow(pos, sgn),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=round(pos),vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())
}

get.gauge <- function(all.ts) {
    all.level <- c()
    all.level.lag <- c()
    for (name in names(all.ts)) {
        cat.level <- c()
        cat.level.lag <- c()
        for (ii in 1:length(all.ts[[name]])) {
            cat.level <- c(cat.level, calc.level(all.ts[[name]][[ii]]))
            cat.level.lag <- c(cat.level.lag, calc.level(all.ts[[name]][[ii]][-length(all.ts[[name]][[ii]])]))
        }

        all.level <- c(all.level, mean(cat.level))
        all.level.lag <- c(all.level.lag, mean(cat.level.lag))
    }

    level <- mean(all.level)
    level.lag <- mean(all.level.lag)

    gg.gauge(level * 100, sign(level - level.lag))
}

## Assume that higher is better
calc.level <- function(ts) {
    tail(rank(ts) - .5, 1) / length(ts)
}


df <- data.frame(year=c(2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007),
                 netloss.M=c(21077, 20570, 20842, 20529, 20265, 19952, 19696, 18500, 17661), #http://www.lotteriinspektionen.se/sv/Om-oss/Statistik/Omsattning-och-behallning-av-spel/Testsida/,
                 p.play=c(0.58, 0.6, 0.61, 0.62, 0.61, 0.62, 0.63, 0.65, 0.67), # http://www.folkhalsomyndigheten.se/documents/statistik-uppfoljning/enkater-undersokningar/nationella-folkhalsoenkaten/resultat-2015/spelvanor-regionala-resultat-2015.xls
                 population=c(7197776, 7273402, 7347057, 7424838, 7489322,7542245, 7592081, 7644548, 7709612)) # http://www.statistikdatabasen.scb.se/sq/10169


df$per.player <- with(df, netloss.M*1000000/(population * p.play))

library(ggplot2)
library(scales)

ggplot(df, aes(x=year, y=per.player)) +
  geom_point(size=2) +
  geom_line(size=1) +
  annotate("text", label="* Personer 16-84 år som uppskattas ha\nspelat under de senaste 12 månaderna", x=2014, y=200) +
  scale_y_continuous(label=comma, limits=c(0, max(df$per.player)))+
  labs(x="", y="", title="Medelförlust, efter vinster, per spelare* och år\n") +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        plot.title = element_text(size=18))
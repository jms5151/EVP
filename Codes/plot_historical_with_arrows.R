x<-subset(modMaxes, County=="Kisumu"|County=="Kwale")

norm <- subset(models2, County == "Kisumu" & Category == "Normal")
el <- subset(models2, County == "Kisumu" & Category == "El_nino")
la <- subset(models2, County == "Kisumu" & Category == "La_nina")

ggplot(norm, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("1993-10-09"), xend=as.Date("1993-10-09"), y=10000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))

ggplot(el, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("2015-12-17"), xend=as.Date("2015-12-17"), y=2000000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))

ggplot(la, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("1999-11-19"), xend=as.Date("1999-11-19"), y=2000000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))



norm <- subset(models2, County == "Kwale" & Category == "Normal")
el <- subset(models2, County == "Kwale" & Category == "El_nino")
la <- subset(models2, County == "Kwale" & Category == "La_nina")

ggplot(norm, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("1993-10-06"), xend=as.Date("1993-10-06"), y=6000000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))

ggplot(el, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("2015-08-28"), xend=as.Date("2015-08-28"), y=10000000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))

ggplot(la, aes(Date, Mtot)) +
  geom_line() +
  ylab('Estimated mosquito abundance') +
  xlab('Month') +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  geom_segment(aes(x=as.Date("1999-09-20"), xend=as.Date("1999-09-20"), y=10000000, yend=0), size=2, col='red', arrow = arrow(length = unit(0.5, "cm")))


# shinyApp(ui = ui, server = server)

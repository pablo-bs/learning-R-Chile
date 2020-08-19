##Ggplot tipos de líneas

lt <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
lt_names <- c("1 blank", "2 solid", "3 dashed", "4 dotted", "5 dotdash", "6 longdash", "7 twodash")

d <- data.frame(lt, lt_names)

ggplot() + scale_x_continuous(name = "", limits = c(0, 1), breaks = NULL) + 
  scale_linetype_identity() + 
  geom_segment(data = d, mapping = aes(x = 0, xend = 1, y = lt_names, yend = lt_names, linetype = lt)) + 
  labs(y = "") + 
  theme(axis.text.y = element_text(face = "bold", color = "black"))

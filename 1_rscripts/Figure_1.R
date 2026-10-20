library(ggplot2)
# Daten für die beiden Verteilungen erzeugen

# Neue Parameter
mean_A <- -5
sd_A <- 1

mean_B <- 5
sd_B <- 1


# Daten erzeugen
x <- seq(-10, 10, length.out = 500)

df <- rbind(
  data.frame(
    x = x,
    dichte = dnorm(x, mean = mean_A, sd = sd_A),
    Group = "A"
  ),
  data.frame(
    x = x,
    dichte = dnorm(x, mean = mean_B, sd = sd_B),
    Group = "B"
  )
)

df$dichte[df$dichte < 0.001] <- NA


# Positionen automatisch berechnen
A_points <- c(
  mean_A - sd_A,
  mean_A,
  mean_A + sd_A
)

B_points <- c(
  mean_B - sd_B,
  mean_B,
  mean_B + sd_B
)

p1 <- 
  ggplot(df, aes(x=x, y=dichte, color=Group, fill=Group)) +
  geom_line(linewidth=1.2) +
  geom_area(alpha=.3, position="identity") +
  
  # SD-Balken unten A
  geom_segment(
    aes(x=A_points[1], xend=A_points[3],
        y = 0.075,yend = 0.075),
    color="#F8766D",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_segment(
    aes(x=B_points[1], xend=B_points[3],
        y=0.075,yend= 0.075),
    color="#00BFC4",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # Tick marks A
  geom_segment(
    data=data.frame(x=A_points),
    aes(x=x,xend=x,y=0.065,yend=0.085),
    color="#F8766D",
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  # Tick marks B
  geom_segment(
    data=data.frame(x=B_points),
    aes(x=x,xend=x,y= 0.065,yend= 0.085),
    color="#00BFC4",
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  # Beschriftungen A
  annotate("text",
           x=A_points,
           y=0.050,
           label=c("-1SD","M","+1SD"),
           color="#F8766D",
           fontface="bold.italic",
           size=4) +
  
  # Beschriftungen B
  annotate("text",
           x=B_points,
           y=0.050,
           label=c("-1SD","M","+1SD"),
           color="#00BFC4",
           fontface="bold.italic",
           size=4) +
  
  geom_segment(
    data=data.frame(x=c(mean_A,mean_B)),
    aes(x=x,xend=x,y=0.410,yend=0.430),
    color="black",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # Between variance
  geom_segment(
    aes(x=mean_A,xend=mean_B,
        y=.42,yend=.42),
    color = "black",
    linewidth = 1
  ) +
  
  theme_minimal() +
  #coord_cartesian(
  #  xlim = c(-10, 10),
  #  ylim = (c(0,0.43)), 
  #  clip = 'off') +
  theme(
    #plot.margin = unit(c(1,1,1,1), "lines"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(color = "Prejudice Target Group",
       fill = "Prejudice Target Group")

p1



# Neue Parameter
mean_C <- -2
sd_C <- 3

mean_D <- 2
sd_D <- 3


# Daten erzeugen
g <- seq(-10, 10, length.out = 500)

dfp2 <- rbind(
  data.frame(
    x = g,
    dichte = dnorm(g, mean = mean_C, sd = sd_C),
    Group = "A"
  ),
  data.frame(
    x = g,
    dichte = dnorm(g, mean = mean_D, sd = sd_D),
    Group = "B"
  )
)

dfp2$dichte[dfp2$dichte < 0.001] <- NA


# Positionen automatisch berechnen
C_points <- c(
  mean_C - sd_C,
  mean_C,
  mean_C + sd_C
)

D_points <- c(
  mean_D - sd_D,
  mean_D,
  mean_D + sd_D
)

p2 <-
  ggplot(dfp2, aes(x=x, y=dichte, color=Group, fill=Group)) +
  geom_line(linewidth=1.2) +
  geom_area(alpha=.3, position="identity") +
  
  # SD-Balken unten A
  geom_segment(
    aes(x=C_points[1], xend=C_points[3],
        y = 0.04,yend = 0.04),
    color="#F8766D",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_segment(
    aes(x=D_points[1], xend=D_points[3],
        y=0.035,yend= 0.035),
    color="#00BFC4",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # Tick marks A
  geom_segment(
    data=data.frame(x=C_points),
    aes(x=x,xend=x,y=0.035,yend=0.045),
    color="#F8766D",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # Tick marks B
  geom_segment(
    data=data.frame(x=D_points),
    aes(x=x,xend=x,y= 0.030,yend= 0.040),
    color="#00BFC4",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # Beschriftungen A
  annotate("text",
           x=C_points,
           y=0.055,
           label=c("-1SD","M","+1SD"),
           color="#F8766D",
           fontface="bold.italic",
           size=4) +
  
  # Beschriftungen B
  annotate("text",
           x=D_points,
           y=0.02,
           label=c("-1SD","M","+1SD"),
           color="#00BFC4",
           fontface="bold.italic",
           size=4) +
  
  geom_segment(
    data=data.frame(x=c(mean_C,mean_D)),
    aes(x=x,xend=x,y=0.145,yend=0.155),
    color="black",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  
  # Between variance
  geom_segment(
    aes(x=mean_C,xend=mean_D,
        y=.15,yend=.15),
    color = "black",
    linewidth = 1
  ) +
  
  # Achse
  geom_segment(
    aes(x=min(x)+1,y=-.02,
        xend=max(x)-1,yend=-.02),color = "black",
    arrow=arrow(length=unit(.4,"cm"),
                type="closed")
  ) +
  
  annotate(
    "text",
    x=0,
    y=-.05,
    label="Latent Valence Dimension",
    fontface="bold",
    size=5
  ) +
  theme_minimal() +
  #coord_cartesian(
  #  xlim = c(-10, 10),
  #  ylim = (c(0,0.17)), 
  #  clip = 'off') +
  theme(
    #plot.margin = unit(c(1,1,3,1), "lines"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ) +
  labs(color = "Prejudice Target Group",
       fill = "Prejudice Target Group")
p2

library(ggpubr)
ggarrange(p1, p2, ncol=1, nrow=2, 
          common.legend = TRUE, 
          legend="bottom",
          labels = "AUTO")

ggsave(
  last_plot(), filename = "fig1.png",
  height = 4,width = 8
)



library(ggplot2)
library(patchwork)   # ersetzt ggpubr::ggarrange

# Okabe-Ito-Farben: farbenblind-tauglich und druckfreundlich
group_cols <- c(A = "#F8766D", B = "#0072B2")

# Ein Panel: zwei Normalverteilungen + SD-Marker + Abstand der Mittelwerte
plot_targets <- function(means, sds, x_axis = FALSE) {
  grp  <- names(group_cols)
  peak <- dnorm(0, sd = min(sds))   # alle Positionen relativ zur Kurvenhöhe
  
  # Dichten nur im Bereich M ± 4 SD (ersetzt den NA-Trick)
  dens <- do.call(rbind, lapply(1:2, function(i) {
    x <- seq(means[i] - 4 * sds[i], means[i] + 4 * sds[i], length.out = 400)
    data.frame(Group = grp[i], x = x, y = dnorm(x, means[i], sds[i]))
  }))
  
  # -1SD / M / +1SD je Gruppe; A oben (Label darüber), B unten (Label darunter)
  marks <- data.frame(
    Group = rep(grp, each = 3),
    x     = c(means[1] + c(-1, 0, 1) * sds[1],
              means[2] + c(-1, 0, 1) * sds[2]),
    y     = rep(c(0.24, 0.12) * peak, each = 3),
    label = c("-1SD", "M", "+1SD")
  )
  marks$tick_lo <- marks$y - 0.025 * peak
  marks$tick_hi <- marks$y + 0.025 * peak
  marks$label_y <- marks$y + ifelse(marks$Group == grp[1], 1, -1) * 0.07 * peak
  
  bar_y <- 1.08 * peak   # Höhe des Balkens zwischen den Mittelwerten
  
  p <- ggplot(dens, aes(x, y, colour = Group, fill = Group)) +
    geom_area(alpha = .3, linewidth = 1.2, position = "identity") +
    
    # SD-Balken, Ticks, Beschriftungen
    geom_line(data = marks, linetype = "dashed", linewidth = 1, show.legend = FALSE) +
    geom_segment(data = marks, aes(xend = x, y = tick_lo, yend = tick_hi),
                 linewidth = 1, show.legend = FALSE) +
    geom_text(data = marks, aes(y = label_y, label = label),
              fontface = "bold.italic", size = 3.5, show.legend = FALSE) +
    
    # Abstand der Mittelwerte (Between-Varianz) + Hilfslinien zu den Mittelwerten
    annotate("segment", x = means, xend = means, y = 0, yend = bar_y,
             linetype = "dotted", colour = "grey50") +
    annotate("segment", x = means, xend = means,
             y = bar_y - 0.03 * peak, yend = bar_y + 0.03 * peak, linewidth = 1) +
    annotate("segment", x = means[1], xend = means[2],
             y = bar_y, yend = bar_y, linewidth = 1) +
    
    scale_colour_manual(values = group_cols, aesthetics = c("colour", "fill"),
                        name = "Prejudice Target Group") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(xlim = c(-10, 10)) +   # gleiche x-Skala in beiden Panels
    theme_void(base_size = 12)
  
  if (x_axis) {
    p <- p +
      labs(x = "Latent Valence Dimension") +
      theme(
        axis.line.x  = element_line(arrow = arrow(length = unit(.3, "cm"), type = "closed")),
        axis.title.x = element_text(face = "bold", margin = margin(t = 6))
      )
  }
  p
}

p1 <- plot_targets(means = c(-5, 5), sds = c(1, 1))
p2 <- plot_targets(means = c(-2, 2), sds = c(3, 3), x_axis = TRUE)

fig1 <- p1 / p2 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold"))

fig1

ggsave("fig1.png", fig1, width = 8, height = 5, dpi = 300, bg = "white")

######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# Construct horizontal bar chart of top ten DRGs.

freq.us.top10 <- freq.us[1:10,]
freq.us.top10$idx <- factor(freq.us.top10$drg,
                            levels = rev(freq.us$drg[1:10]),
                            labels = as.character(c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)))
freq.us.top10$label <- sapply(paste(" ", freq.us.top10$drg, sep = ""), strtrim.ellipsis, n = 52, USE.NAMES = FALSE)

gg <- ggplot() +
    geom_bar(data = freq.us.top10,
             aes(x = idx, y = discharges / 3),
             stat = "identity",
             color = "pink",
             fill = "pink",
             width = 0.75) +
    coord_flip() +
    # Annotate with DRG.
    geom_text(data = freq.us.top10,
              fontface = "bold", size = 5,
              aes(x = idx, y = 0, hjust = 0, label = label)) +
    # General style and visuals.
    theme_bw() +
    scale_y_continuous(labels = scales::comma, limits = c(0, 500000)) +
    labs(title = "Top 10 Inpatient Procedures in the United States") +
    labs(x = "Top 10 Ranking", y = "# of Discharges per Year") +
    theme(legend.position = "none",
          aspect.ratio = 0.75,
          plot.title = element_text(lineheight = 0.8, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold", color = "black"),
          axis.text.y = element_text(size = 12, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black", vjust = -0.5),
          axis.title.y = element_text(size = 12, face = "bold", color = "black", vjust = -0.5))

gg
svg("./cms-top10.svg", height = 6, width = 8)
gg
dev.off()
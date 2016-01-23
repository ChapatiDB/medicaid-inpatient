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

# Construct boxplot of top ten DRG costs and annotate with costs at best 4 US hospitals.

drg.us.top10 <- drg.us[drg.us$drg %in% freq.us$drg[1:10],]
drg.us.top10$idx <- factor(drg.us.top10$drg,
                           levels = freq.us$drg[1:10],
                           labels = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

# Top four hospitals in 2015/2016 as per U.S. News and World Report article here:
# http://health.usnews.com/health-news/best-hospitals/articles/2015/07/21/best-hospitals-2015-16-an-overview
#   1 - MASSACHUSETTS GENERAL HOSPITAL (id = 220071)
#   2 - MAYO CLINIC METHODIST HOSPITAL (id = 240061)
#   3 - JOHNS HOPKINS HOSPITAL, THE (id = 210009)
#   4 - RONALD REAGAN UCLA MEDICAL CENTER (id = 50262) [tied with Johns Hopkins]
# Data frame of top 10 DRGs in the US at top 4 hospitals.
drg.best4 <- drg.us.top10[drg.us.top10$id %in% c(220071, 240061, 210009, 50262),]

# Shorten names so legend isn't so wide.
xlat <- data.frame(name = c("MASSACHUSETTS GENERAL HOSPITAL",
                            "MAYO CLINIC METHODIST HOSPITAL",
                            "JOHNS HOPKINS HOSPITAL, THE",
                            "RONALD REAGAN UCLA MEDICAL CENTER"),
                   name1 = c("Massachusetts General",
                             "Mayo Clinic",
                             "Johns Hopkins",
                             "Ronald Reagan UCLA"),
                   stringsAsFactors = FALSE)
drg.best4 <- left_join(drg.best4, xlat, by = "name")

# Data frame of most expensive locations, i.e. maximum outlier, for top 10 DRGs in the US.
# Compute max value's percentage of mean value.
mymax <- function(df) {
    # Get maximum value.
    retval <- as.data.frame(df[which(df$avg.charge == max(df$avg.charge)),])
    # Construct label.
    retval$label <- pct(100 * (max(df$avg.charge) / median(df$avg.charge)), digits = 0)
    retval
}
drg.us.top10.max <- as.data.frame(drg.us.top10 %>% group_by(drg) %>% do(mymax(.)))

gg <- ggplot() +
    geom_boxplot(data = drg.us.top10,
                 aes(x = idx, y = avg.charge)) +
    # Add best hospital data.
    geom_point(data =  drg.best4,
               aes(x = idx, y = avg.charge, color = name1),
               shape = 18, size = 5) +
    # Color maximum outlier red.
    geom_point(data = drg.us.top10.max,
               aes(x = idx, y = avg.charge),
               shape = 16, color = "red", size = 3) +
    # Annotate maximum outliers with percentage of median.
    geom_text(data = drg.us.top10.max,
              aes(x = idx, y = avg.charge, hjust = 0.5, vjust = -1, label = label),
              color = "red", size = 4) +
    # General style and visuals.
    scale_color_brewer(palette = "Set1", name = "Hospital Name") +
    scale_y_continuous(labels = scales::dollar, limits = c(0, 250000)) +
    labs(title = "Cost Ranges of Top 10 Procedures") +
    labs(x = "Procedure as Top 10 Ranking", y = "Provider's Average Charge for Services") +
    theme(plot.title = element_text(lineheight = 0.8, face = "bold"),
          axis.text.x = element_text(size = 12, face = "bold", color = "black"),
          axis.text.y = element_text(size = 12, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black", vjust = -0.75),
          axis.title.y = element_text(size = 12, face = "bold", color = "black", vjust = -0.75),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, face = "bold", color = "black"),
          plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "in"),
          panel.margin = unit(c(0, 0, 0, 0), "in"))

gg
svg("./cms-ranges.ggplot.svg", height = 8, width = 12)
gg
dev.off()


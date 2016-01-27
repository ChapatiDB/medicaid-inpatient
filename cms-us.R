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

# Construct US map with states colored by most frequent procedure.

# Determine most frequent drg for each state.
tmp <- as.data.frame(summarise(group_by(drg.us, state, drg),
                               discharges = sum(discharges)))
most.frequent <- function(df) {
    df[which(df$discharges == max(df$discharges)),]
}
drg.top.by.state <- tmp %>% group_by(state) %>% do(most.frequent(.))
drg.top.by.state <- as.data.frame(drg.top.by.state)

# Simplify what will eventually be legend labels.
xlat <- data.frame(drg = c("CHRONIC OBSTRUCTIVE PULMONARY DISEASE",
                           "HEART FAILURE & SHOCK",
                           "MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY",
                           "SEPTICEMIA OR SEVERE SEPSIS",
                           "SIMPLE PNEUMONIA & PLEURISY"),
                   Procedure = c("Lung disease",
                                 "Heart failure",
                                 "Joint replacement",
                                 "Infection",
                                 "Pneunomia"),
                   stringsAsFactors = FALSE)
drg.top.by.state <- left_join(drg.top.by.state, xlat, by = "drg")

# Read US states map shapefile.
usa.sp <- readOGR(dsn = "./states_map", layer = "states")
usa.sp@data$id = rownames(usa.sp@data)
usa.map <- fortify(usa.sp, region = "id")

# Annotate map data with fill info so we can color by procedure.
usa.sp@data$state <- as.character(usa.sp@data$state)
usa.sp@data <- left_join(usa.sp@data, drg.top.by.state[,c("state", "Procedure"),], by = "state")

# Merge fill variables, etc. with polydon data.
usa.map = left_join(usa.map, usa.sp@data, by = "id")

# Make the plot.
gg <- ggplot() +
    geom_map(data = usa.map, map = usa.map,
             aes(x = long, y = lat, map_id = id, group = group, fill = Procedure),
             color = "black") +
    coord_equal() +
    theme_bw() +
    scale_fill_brewer(palette = "Pastel1") +
    labs(title = "Most Frequent Inpatient Procedure by State") +
    theme(plot.title = element_text(lineheight = 0.8, face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(size = 12, face = "bold", color = "black"),
          legend.text = element_text(size = 12, face = "bold", color = "black"))

gg
svg("./cms-us.svg", height = 5, width = 9)
gg
dev.off()


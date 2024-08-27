# Install Packages
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("dplyr")

# Load the Libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Load the Data
Trackman_Data <- read_excel("C:/Users/grant/OneDrive/Desktop/TrackmanData.xlsx")

# Filter the Data
selected_rows <- Trackman_Data %>% 
  filter(PitcherTeam == "WIL") %>%
  rename(px = PlateLocSide, pz = PlateLocHeight, type = PitchCall) %>%
  filter(!is.na(px) & !is.na(pz) & !is.na(type) & type != "FoulBall" & type != "InPlay" & type != "StrikeSwinging" & type != "HitByPitch")

# Define the Shape of Home Plate(Still Needs Work)
home_plate <- data.frame(
  x = c(-0.5, 0.5, 0.25, -0.25, -0.5),
  y = c(1, 1, 1.4, 1.4, 1)
)

# Define the Strike Zone
strike_zone <- selected_rows %>%
  filter(px >= -0.8 & px <= 0.8 & pz >= 1.6 & pz <= 3.4)

# Number of Called Strikes in Defined Strike Zone
strikes_in_strike_zone <- strike_zone %>%
  filter(type == "StrikeCalled") %>%
  nrow()

total_in_strike_zone <- nrow(strike_zone)
percentage_strikes_called <- (strikes_in_strike_zone / total_in_strike_zone) * 100

cat("Percentage of balls in the strike zone that were called strikes:", percentage_strikes_called, "%\n")

# Create the Plot
plot <- ggplot(selected_rows, aes(x = px, y = pz, color = type)) +
  geom_point(size = 3) +
  geom_rect(aes(xmin = -0.8, xmax = 0.8, ymin = 1.6, ymax = 3.4), 
            fill = NA, color = "black", linetype = "solid") +   # Add strike zone
  geom_polygon(data = home_plate, aes(x = x, y = y), fill = NA, color = "black") +
  labs(title = "Catcher Framing", x = "Horizontal Location", y = "Vertical Location") +
  theme_minimal() +
  scale_color_manual(values = c("StrikeCalled" = "red", "BallCalled" = "blue")) +
  xlim(-3, 3) +   
  ylim(0, 5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 0.4, y = 5, label = paste0("Percentage: ", 
                                                    round(percentage_strikes_called, 2), "%"), 
           color = "black", size = 4, hjust = 0)

# Display Plot
print(plot)

# Save Plot
ggsave("Catcher_Framing.png", plot = plot, width = 8, height = 6, dpi = 300)









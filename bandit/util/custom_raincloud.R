custom_rc <- function (data_2x2, colors = (c("dodgerblue", "darkorange", "dodgerblue", 
                                "darkorange")), fills = (c("dodgerblue", "darkorange", "dodgerblue", 
                                                           "darkorange")), line_color = "gray", line_alpha = 0.3, size = 1.5, 
          alpha = 0.6, spread_x_ticks = TRUE) 
{
  if (spread_x_ticks == TRUE) {
    figure_2x2 <- ggplot(data = data_2x2) + geom_line(data = data_2x2 %>% 
                                                        dplyr::filter(x_axis %in% c("1", "2")), aes(x = jit, 
                                                                                                    y = y_axis, group = id), color = line_color, alpha = alpha) + 
      geom_line(data = data_2x2 %>% dplyr::filter(x_axis %in% 
                                                    c("4", "5")), aes(x = jit, y = y_axis, group = id), 
                                                                  color = line_color, alpha = alpha) + 
      
      geom_line(data = data_2x2 %>% dplyr::filter(x_axis %in% 
                                                    c("7", "8")), aes(x = jit, y = y_axis, group = id), 
                color = line_color, alpha = alpha) + 
      
      geom_point(data = data_2x2 %>% 
                dplyr::filter(x_axis == "1"), aes(x = jit, y = y_axis), 
              color = colors[1], fill = fills[1], size = size, 
              alpha = alpha) + geom_point(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                                              "2"), aes(x = jit, y = y_axis), color = colors[1], 
                                          fill = fills[1], size = size, alpha = alpha) + geom_point(data = data_2x2 %>% 
                                                                                                      dplyr::filter(x_axis == "4"), aes(x = jit, y = y_axis), 
                                                                                                    color = colors[2], fill = fills[2], size = size, 
                                                                                                    alpha = alpha) + geom_point(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                                                                                                                                    "5"), aes(x = jit, y = y_axis), color = colors[2], 
                                                                                                                                                                          fill = fills[2], size = size, alpha = alpha) + 
      #additional two scatters
      geom_point(data = data_2x2 %>% 
                   dplyr::filter(x_axis == "7"), aes(x = jit, y = y_axis), 
                 color = colors[3], fill = fills[3], size = size, 
                 alpha = alpha) + geom_point(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                                                 "8"), aes(x = jit, y = y_axis), color = colors[3], 
                                             fill = fills[3], size = size, alpha = alpha) +
      
      
      
      geom_half_violin(data = data_2x2 %>% 
                         dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis), 
                       color = colors[1], fill = fills[1], position = position_nudge(x = 0), #-0.35
                       side = "l", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                       dplyr::filter(x_axis == "2"), aes(x = x_axis, y = y_axis), 
                                                                     color = colors[1], fill = fills[1], position = position_nudge(x = 0), 
                                                                     side = "r", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                                                                     dplyr::filter(x_axis == "4"), aes(x = x_axis, y = y_axis), 
                                                                                                                   color = colors[2], fill = fills[2], position = position_nudge(x = 0), 
                                                                                                                   side = "l", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                                                                                                                   dplyr::filter(x_axis == "5"), aes(x = x_axis, y = y_axis), 
                                                                                                                                                                 color = colors[2], fill = fills[2], position = position_nudge(x = 0), 
                                                                                                                                                                 side = "r", alpha = alpha)+
      #additional two densities
      geom_half_violin(data = data_2x2 %>% 
                           dplyr::filter(x_axis == "7"), aes(x = x_axis, y = y_axis), 
                         color = colors[3], fill = fills[3], position = position_nudge(x = 0), 
                         side = "l", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                         dplyr::filter(x_axis == "8"), aes(x = x_axis, y = y_axis), 
                                                                       color = colors[3], fill = fills[3], position = position_nudge(x = 0), 
                                                                       side = "r", alpha = alpha)+
      
      geom_half_boxplot(data = data_2x2 %>% 
                          dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis), 
                        color = "black", fill = fills[1], position = position_nudge(x = 0.05), #-0.3
                        side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                        width = 0.2, alpha = alpha) + geom_half_boxplot(data = data_2x2 %>% 
                                                                          dplyr::filter(x_axis == "2"), aes(x = x_axis, y = y_axis), 
                                                                        color = "black", fill = fills[1], position = position_nudge(x = -0.05), 
                                                                        side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                                                                        width = 0.2, alpha = alpha) + geom_half_boxplot(data = data_2x2 %>% 
                                                                                                                          dplyr::filter(x_axis == "4"), aes(x = x_axis, y = y_axis), 
                                                                                                                        color = "black", fill = fills[2], position = position_nudge(x = 0.05), 
                                                                                                                        side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                                                                                                                        width = 0.2, alpha = alpha) + geom_half_boxplot(data = data_2x2 %>% 
                                                                                                                                                                          dplyr::filter(x_axis == "5"), aes(x = x_axis, y = y_axis), 
                                                                                                                                                                        color = "black", fill = fills[2], position = position_nudge(x = -0.05), 
                                                                                                                                                                        side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                                                                                                                                                                        width = 0.2, alpha = alpha) +
    #additional two boxplots
    geom_half_boxplot(data = data_2x2 %>% 
                          dplyr::filter(x_axis == "7"), aes(x = x_axis, y = y_axis), 
                        color = "black", fill = fills[3], position = position_nudge(x = 0.05), 
                        side = "l", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                        width = 0.2, alpha = alpha) + geom_half_boxplot(data = data_2x2 %>% 
                                                                          dplyr::filter(x_axis == "8"), aes(x = x_axis, y = y_axis), 
                                                                        color = "black", fill = fills[3], position = position_nudge(x = -0.05), 
                                                                        side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                                                                        width = 0.2, alpha = alpha)
  }
  else if (spread_x_ticks == FALSE) {
    figure_2x2 <- ggplot(data = data_2x2) + geom_point(data = data_2x2 %>% 
                                                         dplyr::filter(x_axis == "1"), aes(x = jit, y = y_axis), 
                                                       color = colors[1], fill = fills[1], size = size, 
                                                       alpha = alpha) + geom_point(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                                                                                       "1.01"), aes(x = jit, y = y_axis), color = colors[2], 
                                                                                   fill = fills[2], size = size, alpha = alpha) + geom_point(data = data_2x2 %>% 
                                                                                                                                               dplyr::filter(x_axis == "2"), aes(x = jit, y = y_axis), 
                                                                                                                                             color = colors[3], fill = fills[3], size = size, 
                                                                                                                                             alpha = alpha) + geom_point(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                                                                                                                                                                             "2.01"), aes(x = jit, y = y_axis), color = colors[4], 
                                                                                                                                                                         fill = fills[4], size = size, alpha = alpha) + geom_line(data = data_2x2 %>% 
                                                                                                                                                                                                                                    dplyr::filter(x_axis %in% c("1", "2")), aes(x = jit, 
                                                                                                                                                                                                                                                                                y = y_axis, group = id), color = colors[1], alpha = alpha) + 
      geom_line(data = data_2x2 %>% dplyr::filter(x_axis %in% 
                                                    c("1.01", "2.01")), aes(x = jit, y = y_axis, 
                                                                            group = id), color = colors[2], alpha = alpha) + 
      geom_half_boxplot(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                            "1"), aes(x = x_axis, y = y_axis), color = colors[1], 
                        fill = fills[1], position = position_nudge(x = -0.30), 
                        side = "l", outlier.shape = NA, center = TRUE, 
                        errorbar.draw = FALSE, width = 0.2, alpha = alpha) + 
      geom_half_boxplot(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                            "1.01"), aes(x = x_axis, y = y_axis), color = colors[2], 
                        fill = fills[2], position = position_nudge(x = -0.30), 
                        side = "l", outlier.shape = NA, center = TRUE, 
                        errorbar.draw = FALSE, width = 0.2, alpha = alpha) + 
      geom_half_boxplot(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                            "2"), aes(x = x_axis, y = y_axis), color = colors[3], 
                        fill = fills[3], position = position_nudge(x = 0.30), 
                        side = "r", outlier.shape = NA, center = TRUE, 
                        errorbar.draw = FALSE, width = 0.2, alpha = alpha) + 
      geom_half_boxplot(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                            "2.01"), aes(x = x_axis, y = y_axis), color = colors[4], 
                        fill = fills[4], position = position_nudge(x = 0.30), 
                        side = "r", outlier.shape = NA, center = TRUE, 
                        errorbar.draw = FALSE, width = 0.2, alpha = alpha) + 
      geom_half_violin(data = data_2x2 %>% dplyr::filter(x_axis == 
                                                           "1"), aes(x = x_axis, y = y_axis), color = colors[1], 
                       fill = fills[1], position = position_nudge(x = -0.30), 
                       side = "l", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                       dplyr::filter(x_axis == "1.01"), aes(x = x_axis, 
                                                                                                            y = y_axis), color = colors[2], fill = fills[2], 
                                                                     position = position_nudge(x = -0.50), side = "l", 
                                                                     alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                                                         dplyr::filter(x_axis == "2"), aes(x = x_axis, y = y_axis), 
                                                                                                       color = colors[3], fill = fills[3], position = position_nudge(x = 0.40), 
                                                                                                       side = "r", alpha = alpha) + geom_half_violin(data = data_2x2 %>% 
                                                                                                                                                       dplyr::filter(x_axis == "2.01"), aes(x = x_axis, 
                                                                                                                                                                                            y = y_axis), color = colors[4], fill = fills[4], 
                                                                                                                                                     position = position_nudge(x = 0.49), side = "r", 
                                                                                                                                                     alpha = alpha)
  }
  return(figure_2x2)
}

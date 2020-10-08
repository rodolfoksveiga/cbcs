invisible({
  pkgs = c('dplyr', 'forcats', 'ggplot2', 'jsonlite',
           'purrr', 'RJSONIO')
  lapply(pkgs, library, character.only = TRUE)
})

# plot sobol total index
PlotST = function(result_path, problem_path, output_dir) {
  vars = problem_path %>%
    read_json() %>%
    pluck('names') %>%
    unlist()
  plot = result_path %>%
    fromJSON() %>%
    keep(names(.) %in% c('S1', 'ST')) %>%
    as.data.frame() %>%
    mutate(Variable = as.factor(vars)) %>%
    arrange(ST) %>%
    mutate(Variable = fct_inorder(Variable)) %>%
    ggplot() +
    geom_bar(aes(x = Variable, y = ST, fill = Variable),
             stat = 'identity', colour = 'black', show.legend = FALSE) +
    coord_flip() +
    scale_fill_brewer(palette = 'Paired') +
    theme(panel.background = element_rect(colour = 'black',
                                          fill = 'white', size = 0.5),
          panel.grid.major = element_line(size = 0.25, colour = 'black'),
          panel.grid.minor = element_line(size = 0.25, colour = 'black'),
          axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14, colour = 'black'),
          axis.text.y = element_text(size = 14, colour = 'black', angle = 30))
  plot_name = 'sobol_barplot'
  WritePlot(plot, plot_name, output_dir)
}

# define characteristics to save the plot
WritePlot = function(plot, plot_name, output_dir) {
  # plot: plot variable
  # plot_name: file name (without extension)
  # output_dir: output directory
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# application ####
output_dir = './plot_table/'
PlotST('./result/sobol_analysis.json', './result/sobol_problem.json', output_dir)

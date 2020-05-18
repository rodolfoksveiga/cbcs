# load libraries ####
library('brnn')
library('caret')
library('dplyr')
library('shiny')

# load model ####
load('ghr_sobol_brnn_model.RData')

# base functions ####
# create dummy variables
CreateDummies = function(data) {
  data$hvac.split = ifelse(data$hvac == 'split', 1, 0)
  data$hvac.vrf = ifelse(data$hvac == 'vrf', 1, 0)
  data$afn.max = ifelse(data$afn == 'max', 1, 0)
  data$afn.min = ifelse(data$afn == 'min', 1, 0)
  data$boundaries.adiabatic = ifelse(data$boundaries == 'adiabatic', 1, 0)
  data$boundaries.outdoors = ifelse(data$boundaries == 'outdoors', 1, 0)
  data = select(data, hvac.split, hvac.vrf, envelope, lights,
                shgc, afn.max, afn.min, azimuth, boundaries.adiabatic,
                boundaries.outdoors, ghr)
  return(data)
}
# pre-process and predict data
PredictData = function(data, pp_model, model) {
  dummy_data = CreateDummies(data)
  dummy_data = predict(pp_model, dummy_data)
  prediction = predict(model, dummy_data) %>%
    round(1)
  if (nrow(data) == 1) {
    return(prediction)
  } else {
    data = cbind(data, prediction)
    return(data)
  }
}

# shiny server ####
shinyServer(
  function(input, output) {
    output$prediction = renderText({
      data = data.frame('hvac' = input$hvac, 'envelope' = as.numeric(input$envelope),
                        'lights' = input$lights, 'shgc' = input$shgc,
                        'afn' = input$afn, 'azimuth' = input$azimuth,
                        'boundaries' = input$boundaries, 'ghr' = input$ghr)
      PredictData(data, pp_model, model)
    })
    
    output$construction = renderTable({
      if (input$envelope == 2.5) {
        data.frame('Paredes' = c('Gesso (1 cm)', 'Concreto (10 cm)',
                                 'Argamassa (2 cm)', 'Placa de granito (2 cm)'),
                   'Cobertura' = c('Forro (2 cm)', 'Câmara de ar',
                                   'Câmara de ar', 'Telha de fibrocimento escura (8 mm)'),
                   row.names = c('Camada interna', 'Camada 2', 'Camada 3', 'Camada externa'))
      } else {
        data.frame('Paredes' = c('Gesso (1 cm)', 'Tijolo cerâmico (1.34 cm)',
                                 'Câmara de ar', 'Tijolo cerâmico (1.34 cm)',
                                 'Argamassa (2.5 cm)'),
                   'Cobertura' = c('Laje nervurada de EPS (15 cm)', 'Câmara de ar',
                                   'Laje nervurada de EPS (7.5 cm)', 'Câmara de ar',
                                   'Telha de fibrocimento clara (8 mm)'),
                   row.names = c('Camada interna', 'Camada 2', 'Camada 3',
                                 'Camada 4', 'Camada externa'))
      }
    }, align = 'c', striped = T, bordered = T, rownames = T)
    
    data = reactive({
      file = input$file
      if (is.null(file)) {
        return()
      } else {
        file$datapath %>%
          read.csv(sep = input$sep) %>%
          PredictData(pp_model, model)
      }
    })
    
    output$table = renderTable({
      if (is.null(data())) {
        return()
      } else {
        head(data(), input$nrow)
      }
    }, align = 'c', striped = T, bordered = T, rownames = T)
    
    output$download = downloadHandler(
      filename = 'predictions.csv', 
      content = function(file) {
        write.csv(data(), file, sep = input$sep, row.names = F)
      },
      contentType = 'csv'
    )
  }
)

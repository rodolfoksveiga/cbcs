# load libraries ####
library('shiny')
library('shinythemes')

# shiny user interface ####
shinyUI(
  fluidPage(
    theme = shinytheme('yeti'),
    headerPanel(
      h1('Predição do consumo energético de agências bancárias', align = 'center')
    ),
    sidebarPanel(
      conditionalPanel(
        condition = 'input.tabselected == 1',
        h2('Insira os dados de entrada manualmente', align = 'center'),
        numericInput('ghr', '1. Graus Horas de Resfriamento da cidade [°C]:',
                     min = 3495, max = 71394, value = 3495),
        selectInput('hvac', '2. Sistema de condicionamento artificial do ar:',
                    choices = list('Splitão' = 'split', 'VRF' = 'vrf')),
        selectInput('afn', '3. Trocas de ar segundo a lotação da agência bancária [m³/(s.pessoa]:',
                    choices = list('0 (lotação mínima)' = 'min',
                                   '0.0075 (lotação máxima)' = 'max')),
        selectInput('boundaries', '4. Exposição das fachadas:',
                    choices = list('Apenas a fachada de entrada exposta' = 'adiabatic',
                                   'Todas as fachadas expostas' = 'outdoors')),
        selectInput('envelope', '5. Transmitância térmica da envoltória [W/(m².K)]:',
                    choices = c('2.5 (alta inércia térmica)' = 2.5,
                                '3.7 (baixa inércia térmica)' = 3.7)),
        sliderInput('azimuth', '6. Orientação do edifício [°]:',
                    min = 0, max = 345, step = 15, value = 0),
        sliderInput('lights', '7. Densidade de potência de iluminação [W/m²]:',
                    min = 10, max = 24, step = 0.5, value = 10),
        sliderInput('shgc', '8. Fator solar dos vidros:',
                    min = 0.3, max = 0.7, step = 0.01, value = 0.3),
        helpText(h4('Ajuste todos os campos de entrada para gerar os dados de saída.', align = 'center'))
      ),
      conditionalPanel(
        condition = 'input.tabselected == 2',
        h2('Insira os dados de entrada através de um arquivo no formato CSV', align = 'center'),
        fileInput('file', '1. Upload do arquivo CSV:'),
        radioButtons('sep', '2. Tipo de separador:', selected = ',',
                     choices = list('Vírgula' = ',', 'Ponto e vírgula' = ';',
                                    'Tab' = '\t', 'Espaço' = ' ')),
        sliderInput('nrow', '3. N° de observações exibidas na tabela de saída:',
                    min = 5, max = 100, value = 50),
        downloadButton('download', 'Download das predições'),
        helpText(h4('O tamanho máximo permitido para o arquivo CSV é de 50 Mb.', align = 'center'))
      )
    ),
    mainPanel(
      tabsetPanel(
        type = 'tab', id = 'tabselected',
        tabPanel(
          'Interativo', value = 1,
            h2(strong('Valor da predição'), align = 'center'),
            h2(textOutput('prediction'), align = 'center'),
            br(),
            h3('Materiais utilizados na envoltória', align = 'center'),
            h5('Valores equivalentes utilizados no EnergyPlus', align = 'center'),
            h4('Área do edifício = 600 m²', align = 'center'),
            tableOutput('construction')
        ),
        tabPanel(
          'CSV', value = 2,
          h2(strong('Resultados das predições'), align = 'center'),
          tableOutput('table'),
          h3('ATENÇÃO', align = 'center'),
          h4('Os arquivos em CSV devem conter as seguintes colunas: "hvac", "envelope", "lights", 
             "shgc", "afn", "azimuth", "boundaries" e "ghr".', align = 'center'),
          h4(em('Caso essas instruções não sejam seguidas, é possível que os resultados gerados 
                estejam incorretos.'), align = 'center'),
          h3('O USO DESSA PLATAFORMA É DE TOTAL RESPONSABILIDADE DO USUÁRIO!', align = 'center')
        )
      )
    )
  )
)
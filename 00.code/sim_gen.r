# load libraries
pkgs = c('stringr')
lapply(pkgs, library, character.only = T)


# temp
seed_path = '/home/rodox/00.git/02.commercial_model/01.seed/seed_split_bank.epJSON'
seed = rjson::fromJSON(file = seed_path)

ApplyObjects = function(object, fields, values) {
  object = mapply(function(x, y) x = y, fields, values, SIMPLIFY = FALSE)
  names(object) = fields
  return(object)
}
# seed$'Construction'$'Exterior Wall' =
#   ApplyObjects(seed$'Construction'$'Exterior Wall',
#                list('idf_max_fields', 'outside_layer', 'layer_2', 'layer_3', 'layer_4', 'layer_5'),
#                list(6, 'argamassa', 'tijolo ceramico', 'Camara parede', 'tijolo ceramico', 'gesso'))

seed$'Construction'[paste('Exterior', c('Wall', 'Roof'))] =
  mapply(ApplyObjects, seed$'Construction'[paste('Exterior', c('Wall', 'Roof'))],
         list(list('idf_max_fields', 'outside_layer', 'layer_2', 'layer_3', 'layer_4', 'layer_5')),
         list(list(6, 'argamassa', 'tijolo ceramico',
                   'Camara parede', 'tijolo ceramico', 'gesso'),
              list(6, 'fibrocimento telha branca', 'Camara telhado',
                   'laje nervurada EPS', 'vazio', 'EPS laje nervurada')),
         SIMPLIFY = FALSE)

DefConstruction = function(envelope, group) {
  objects = paste('Exterior', c('Wall', 'Roof'))
  fields = c('idf_max_fields', 'outside_layer', 'layer_2', 'layer_3', 'layer_4')
  if (envelope == 2.5) {
    values = list(list(5, 'placa granito', 'argamassa assent', 'Concreto parede', 'gesso'),
                  list(5, 'fibrocimento telha', 'Camara telhado', 'vazio', 'forro'))
  } else if (envelope == 3.7) {
    fields = c(fields, 'layer_5')
    values = list(list(6, 'argamassa', 'tijolo ceramico',
                       'Camara parede', 'tijolo ceramico', 'gesso'),
                  list(6, 'fibrocimento telha branca', 'Camara telhado',
                       'laje nervurada EPS', 'vazio', 'EPS laje nervurada'))
  }
  group = mapply(ApplyObjects, objects, fields, values, SIMPLIFY = FALSE)
  return(seed)
}





envelope = 2.5 | 3.7

if (envelope == 2.5) {
  seed$'Construction'$'Exterior Roof'$'idf_max_fields' = 5,
  seed$'Construction'$'Exterior Roof'$'outside_layer' = 'fibrocimento telha'
  seed$'Construction'$'Exterior Roof'$'layer_2' = 'Camara telhado'
  seed$'Construction'$'Exterior Roof'$'layer_3' = 'vazio'
  seed$'Construction'$'Exterior Roof'$'layer_4' = 'forro'
  
  seed$'Construction'$'Exterior Wall'$'idf_max_fields' = 5,
  seed$'Construction'$'Exterior Wall'$'outside_layer' = 'placa granito'
  seed$'Construction'$'Exterior Wall'$'layer_2' = 'argamassa assent'
  seed$'Construction'$'Exterior Wall'$'layer_3' = 'Concreto parede'
  seed$'Construction'$'Exterior Wall'$'layer_4' = 'gesso'
} else if (envelope == 3.7) {
  seed$'Construction'$'Exterior Roof'$'idf_max_fields' = 6,
  seed$'Construction'$'Exterior Roof'$'outside_layer' = 'fibrocimento telha branca'
  seed$'Construction'$'Exterior Roof'$'layer_2' = 'Camara telhado'
  seed$'Construction'$'Exterior Roof'$'layer_3' = 'laje nervurada EPS'
  seed$'Construction'$'Exterior Roof'$'layer_4' = 'vazio'
  seed$'Construction'$'Exterior Roof'$'layer_5' = 'EPS laje nervurada'
  
  seed$'Construction'$'Exterior Wall'$'idf_max_fields' = 6,
  seed$'Construction'$'Exterior Wall'$'outside_layer' = 'argamassa'
  seed$'Construction'$'Exterior Wall'$'layer_2' = 'tijolo ceramico'
  seed$'Construction'$'Exterior Wall'$'layer_3' = 'Camara parede'
  seed$'Construction'$'Exterior Wall'$'layer_4' = 'tijolo ceramico'
  seed$'Construction'$'Exterior Wall'$'layer_5' = 'gesso'
}

lights

seed$'FILL'$
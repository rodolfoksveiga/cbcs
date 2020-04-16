# load libraries ####
pkgs = c('stringr')
lapply(pkgs, library, character.only = T)

# base functions ####
ApplyObject = function(object, fields, values) {
  names = names(object)
  object = append(object, mapply(function(x, y) x = y, fields,
                                 values, SIMPLIFY = FALSE))
  names(object) = c(names, fields)
  return(object)
}

# sample functions ####
# envelope: 2.5 or 3.7
DefEnvelope = function(envelope, objects) {
  fields = list('idf_max_fields', 'outside_layer', 'layer_2', 'layer_3', 'layer_4')
  if (envelope == 2.5) {
    values = list(list(5, 'placa granito', 'argamassa assent', 'Concreto parede', 'gesso'),
                  list(5, 'fibrocimento telha', 'Camara telhado', 'vazio', 'forro'))
  } else if (envelope == 3.7) {
    fields = append(fields, 'layer_5')
    values = list(list(6, 'argamassa', 'tijolo ceramico',
                       'Camara parede', 'tijolo ceramico', 'gesso'),
                  list(6, 'fibrocimento telha branca', 'Camara telhado',
                       'laje nervurada EPS', 'vazio', 'EPS laje nervurada'))
  } else {
    stop('Invalid thermal transmittance!')
  }
  objects = mapply(ApplyObject, objects, list(fields), values, SIMPLIFY = FALSE)
  return(objects)
}

# lights: 10 to 24
DefLights = function(lights, objects) {
  if (lights >= 10 & lights <= 24) {
    objects = mapply(ApplyObject, objects, 'watts_per_zone_floor_area',
                     lights, SIMPLIFY = FALSE)
  } else {
    stop('Invalid lighting density!')
  }
  return(objects)
}

# shgc: 0.3 to 0.7
  # PRECISO TESTAR A TRANSMITÂNCIA VISIVEL COM DIFERENTES VALORES
DefSHGC = function(shgc, object) {
  if (shgc >= 0.3 & shgc <= 0.7) {
    object = ApplyObject(object, 'solar_heat_gain_coefficient', shgc)
  } else {
    stop('Invalid SHGC!')
  }
  return(object)
}

# afn: 'max' or 'min'
  # PRECISO TESTAR A FLOW_RATE_PER_PERSON = 0 COM SCHEDULE_NAME = HVAC | ALWAYS OFF
DefAFN = function(afn, objects) {
  fields = list('schedule_name', 'flow_rate_per_person')
  if (afn == 'max') {
    values = list('HVAC', 0.0075)
  } else if (afn == 'min') {
    values = list('Always Off', 0)
  } else {
    stop('Invalid AFN capacity!')
  }
  objects = mapply(ApplyObject, objects, MoreArgs = list(fields, values),
                   SIMPLIFY = FALSE)
  return(objects)
}

# azimuth: 0 to 360
DefAzimuth = function(azimuth, object) {
  if (azimuth >= 0 & azimuth <= 360) {
    object = ApplyObject(object, 'north_axis', azimuth)
  }
  else {
    stop('Invalid azimuth angle!')
  }
  return(object)
}

# boundaries: 'adiabatic' or 'outdoors'
DefBoundaries = function(boundaries, objects) {
  fields = list('outside_boundary_condition', 'sun_exposure', 'wind_exposure')
  if (boundaries == 'adiabatic') {
    values = list('Adiabatic', 'NoSun', 'NoWind')
  } else if (boundaries == 'outdoors') {
    values = list('Outdoors', 'SunExposed', 'WindExposed')
  } else {
    stop('Invalid boundary condition!')
  }
  objects = mapply(ApplyObject, objects, MoreArgs = list(fields, values),
                   SIMPLIFY = FALSE)
  return(objects)
}

# main function ####
SimGen = function(seed_path, envelope, lights, shgc, afn, azimuth, boundaries) {
  # load seed file
  seed = rjson::fromJSON(file = seed_path)
  # envelope
  components = paste('Exterior', c('Wall', 'Roof'))
  seed$'Construction'[components] =
    DefEnvelope(envelope, seed$'Construction'[components])
  # lights
  seed$'Lights'[c(1:5)] =
    DefLights(lights, seed$'Lights'[c(1:5)])
  # shgc
  seed$'WindowMaterial:SimpleGlazingSystem'$'SHGC 01' =
    DefSHGC(shgc, seed$'WindowMaterial:SimpleGlazingSystem'$'SHGC 01')
  # afn
  seed$'ZoneVentilation:DesignFlowRate'[c(1:5)] =
    DefAFN(afn, seed$'ZoneVentilation:DesignFlowRate'[c(1:5)])
  # azimuth
  seed$'Building'$'AgenciaBancaria' =
    DefAzimuth(azimuth, seed$'Building'$'AgenciaBancaria')
  # boundaries
  surfs = c('4', '6', '8', '10', '11', '14', '16', '18',
            '20', '23', '24', '26', '26A', '28', '31', '32')
  surfs = paste('Surface', surfs)
  seed$'BuildingSurface:Detailed'[surfs] =
    DefBoundaries('adiabatic', seed$'BuildingSurface:Detailed'[surfs])
  # write the 'epJSON' file
  jsonlite::write_json(seed, paste0(dirname(seed_path), '/', 'test.epJSON'),
                       pretty = T, auto_unbox = T)
}

# application ####
# temp
# seed_path = '/home/rodox/00.git/02.commercial_model/01.seed/seed_split_bank.epJSON'
seed_path = '/home/rodox/00.git/02.commercial_model/01.seed/seed_vrf_bank.epJSON'

seed = SimGen(seed_path, 3.7, 17, 0.5, 'max', 270, 'outdoors')





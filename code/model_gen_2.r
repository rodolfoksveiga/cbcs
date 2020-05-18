# load libraries ####
pkgs = c('jsonlite', 'parallel', 'stringr')
lapply(pkgs, library, character.only = T)

# base functions ####
ApplyModelGen = function(sample_path, output_dir) {
  sample = read.csv(sample_path, stringsAsFactors = F)
  mcmapply(ModelGen, sample$seed, sample$envelope, sample$lights,
           sample$shgc, sample$afn, sample$azimuth, sample$boundaries,
           sample$case, output_dir, mc.cores = detectCores())
}

ApplyObject = function(object, fields, values) {
  names = names(object)
  object = append(object, mapply(function(x, y) x = y, fields,
                                 values, SIMPLIFY = FALSE))
  names(object) = c(names, fields)
  return(object)
}

# model edition functions ####
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
DefSHGC = function(shgc, object) {
  if (shgc >= 0.3 & shgc <= 0.7) {
    object = ApplyObject(object, 'solar_heat_gain_coefficient', shgc)
  } else {
    stop('Invalid SHGC!')
  }
  return(object)
}

# afn: 'max' or 'min'
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

# atm: 1 to 10
  # consumption was defined as 142 watts, at first
DefATM = function(atm, object, consumption = 142) {
  object = ApplyObject(object, 'design_level', atm*consumption)
  return(object)
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
ModelGen = function(seed_path, envelope, lights, shgc, afn, atm,
                    azimuth, boundaries, model_name, output_dir) {
  # load seed file
  seed = read_json(file = seed_path)
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
  # atm
  seed$'ElectricEquipment'$'EQUIP_CO_RE_ATM_EQUIP' =
    DefATM(atm, seed$'ElectricEquipment'$'EQUIP_CO_RE_ATM_EQUIP')
  # azimuth
  seed$'Building'$'AgenciaBancaria' =
    DefAzimuth(azimuth, seed$'Building'$'AgenciaBancaria')
  # boundaries
  surfs = c('4', '6', '8', '10', '11', '14', '16', '18',
            '20', '23', '24', '26', '26A', '28', '31', '32')
  surfs = paste('Surface', surfs)
  seed$'BuildingSurface:Detailed'[surfs] =
    DefBoundaries(boundaries, seed$'BuildingSurface:Detailed'[surfs])
  # write the 'epJSON' file
  model_path = paste0(output_dir, model_name, '.epJSON')
  write_json(seed, model_path, pretty = T, auto_unbox = T)
  print(model_path)
}

# application ####
ApplyModelGen('/home/rodox/00.git/02.commercial_model/00.code/sample.csv',
              '/home/rodox/00.git/02.commercial_model/02.model/')

# ModelGen('/home/rodox/00.git/02.commercial_model/01.seed/seed_split_bank.epJSON',
#          3.7, 15, 0.3, 'max', 90, 'outdoors', 'test1', '~/Desktop/test/')
# ModelGen('/home/rodox/00.git/02.commercial_model/01.seed/seed_split_bank.epJSON',
#          3.7, 20, 0.3, 'max', 90, 'outdoors', 'test2', '~/Desktop/test/')
# ModelGen('/home/rodox/00.git/02.commercial_model/01.seed/seed_vrf_bank.epJSON',
#          3.7, 15, 0.3, 'max', 90, 'outdoors', 'test3', '~/Desktop/test/')
# ModelGen('/home/rodox/00.git/02.commercial_model/01.seed/seed_vrf_bank.epJSON',
#          3.7, 20, 0.3, 'max', 90, 'outdoors', 'test4', '~/Desktop/test/')

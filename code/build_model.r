# base functions ####
# define fields values for an specific object
AddFields = function(object, fields, values) {
  names = names(object)
  object = c(object, mapply(function(x, y) x = y, fields, values, SIMPLIFY = FALSE))
  names(object) = c(names, fields)
  return(object)
}
# define range
DefRange = function(bound, gap) {
  mult = (bound[2] - bound[1])*gap
  inter = c(bound[1] - mult, bound[2] + mult)
  return(inter)
} 
# scale vertex
ScaleVertex = function(coord, multiplier) coord*multiplier
# scale surface
ScaleSurf = function(vertex, multiplier) {
  vertex[1:2] = lapply(vertex[1:2], ScaleVertex, multiplier)
  return(vertex)
}
# scale an object
ScaleObject = function(object, type, multiplier) {
  axis = c('x', 'y')
  if (type == 'zone') {
    coords = paste0(axis, '_origin')
    object[coords] = lapply(object[coords], ScaleVertex, multiplier)
  } else if (type == 'surf') {
    object$vertices = lapply(object$vertices, ScaleSurf, multiplier)
  } else {
    coords = paste0('vertex_', 1:4, '_', rep(axis, each = 4), '_coordinate')
    object[coords] = lapply(object[coords], ScaleVertex, multiplier)
  }
  return(object)
}

# model editing functions ####
# afn: 'smart', 'min', 'inter', 'max' [adm]
DefAFN = function(object, tag, area, people, afn, factors) {
  fields = c('schedule_name', 'design_flow_rate_calculation_method')
  zone = str_extract(tag, '(?<=ren_).*')
  if (afn == 'smart') {
    fields = c(fields, 'flow_rate_per_person')
    sch = paste0('sch_ocup_', str_remove(zone, '\\d'))
    values = list(sch, 'Flow/Person', 0.0075)
  } else if (afn %in% c('min', 'inter', 'max')) {
    fields = c(fields, 'design_flow_rate')
    sch = paste0('sch_hvac_', ifelse(grepl('atm_clientes', zone), 'atm_clientes', 'caixas_admin'))
    values = list(sch, 'Flow/Zone')
    np = people$people_per_zone_floor_area*area
    flow_rate = (np*factors[1] + area*factors[2])/1000
    values = append(values, flow_rate)
  } else {
    stop('Invalid air flow rate!')
  }
  object = AddFields(object, fields, values)
  return(object)
}
# area: 200 to 1200 [m²]
DefArea = function(group, type, area) {
  inter = DefRange(c(200, 1200), 0.02)
  if (area >= inter[1] & area <= inter[2]) {
    group = lapply(group, ScaleObject, type, sqrt(area/600))
  }
  else {
    stop('Invalid area!')
  }
  return(group)
}
# atm: 5 to 20 [m²/atm]
# consumption was defined as 142 watts, at first
DefATM = function(atm, object, consumption = 142) {
  inter = DefRange(c(5, 20), 0.02)
  if (atm >= inter[1] & atm <= inter[2]) {
    dens = (1/atm)*(130/20)*consumption
    object = AddFields(object, 'watts_per_zone_floor_area', dens)
  } else {
    stop('Invalid ATM density!')
  }
  return(object)
}
# azimuth: 0 to 360 [°]
DefAzimuth = function(azimuth, object) {
  inter = DefRange(c(0, 360), 0.02)
  if (azimuth >= inter[1] & azimuth <= inter[2]) {
    object = AddFields(object, 'north_axis', 180 + azimuth)
  }
  else {
    stop('Invalid azimuth angle!')
  }
  return(object)
}
# boundaries: 'adiabatic' or 'outdoors' [adm]
DefBoundaries = function(boundaries, objects) {
  fields = list('outside_boundary_condition', 'sun_exposure', 'wind_exposure')
  if (boundaries == 'adiabatic') {
    values = list('Adiabatic', 'NoSun', 'NoWind')
  } else if (boundaries == 'outdoors') {
    values = list('Outdoors', 'SunExposed', 'WindExposed')
  } else {
    stop('Invalid boundary condition!')
  }
  objects = mapply(AddFields, objects, MoreArgs = list(fields, values), SIMPLIFY = FALSE)
  return(objects)
}
# cop: 3 to 6 [w/w]
DefCOP = function(objects, cop, hvac) {
  inter = DefRange(c(3, 6), 0.02)
  if (cop >= inter[1] & cop <= inter[2]) {
    if (hvac == 'split') {
      fields = ifelse(grepl('Heating', names(objects)), 'heating', 'cooling')
      fields = paste0('gross_rated_', fields, '_cop')
      objects = mapply(AddFields, objects, fields, cop, SIMPLIFY = FALSE)
    } else {
      fields = paste0('gross_rated_', c('heating', 'cooling'), '_cop')
      objects = lapply(objects, AddFields, fields, cop)
    }
  } else {
    stop('Invalid COP!')
  }
  return(objects)
}
# envelope: 'heavy' or 'light' [adm]
DefEnvelope = function(envelope, objects) {
  fields = list('idf_max_fields', 'outside_layer', 'layer_2', 'layer_3', 'layer_4', 'layer_5')
  if (envelope == 'heavy') {
    fields = list(fields[1:2], fields[1:4])
    values = list(list(2, 'concreto_10cm_0.7'),
                  list(4, 'telha_fibrocimento_0.7', 'camara_cob', 'concreto_10cm_0.7'))
  } else if (envelope == 'light') {
    fields = list(fields[1:6], fields[1:5])
    values = list(list(6, 'argamassa_0.3', 'tijolo_vazado_0.3', 'camara_par',
                       'tijolo_vazado_0.3', 'argamassa_0.3'),
                  list(5, 'telha_fibrocimento_0.3', 'camara_cob',
                       'la_vidro_5cm_0.3', 'concreto_10cm_0.3'))
  } else {
    stop('Invalid envelope!')
  }
  objects = mapply(AddFields, objects, fields, values, SIMPLIFY = FALSE)
  return(objects)
}
# lights: 10 to 24
DefLights = function(lights, objects) {
  inter = DefRange(c(10, 24), 0.02)
  if (lights >= inter[1] & lights <= inter[2]) {
    objects = mapply(AddFields, objects, 'watts_per_zone_floor_area', lights, SIMPLIFY = FALSE)
  } else {
    stop('Invalid lighting density!')
  }
  return(objects)
}
# shgc: 0.3 to 0.7
DefSHGC = function(shgc, object) {
  inter = DefRange(c(0.3, 0.7), 0.02)
  if (shgc >= inter[1] & shgc <= inter[2]) {
    object = AddFields(object, 'solar_heat_gain_coefficient', shgc)
  } else {
    stop('Invalid SHGC!')
  }
  return(object)
}
# pick up summer and winter design days by weather
PickDesignDays = function(inmet) {
  ddys = c('Ann Clg .4% Condns DP=>MDB', 'Ann Htg 99.6% Condns DB')
  ddys = inmet[ddys]
  return(ddys)
}

# main function ####
BuildModel = function(seed_path, afn, area, atm, azimuth, boundaries,
                      cop, envelope, lights, shgc, model_path) {
  # load seed file
  seed = read_json(seed_path)
  # afn
  areas = c(admin = 144, atm_clientes = 130, caixas1 = 150, caixas2 = 150)*area/600
  factors = list(min = c(3.8, 0.3), inter = c(4.8, 0.4), max = c(5.7, 0.5))
  factors = factors[[afn]]
  group = seed$'ZoneVentilation:DesignFlowRate'
  seed$'ZoneVentilation:DesignFlowRate' = mapply(DefAFN, group, names(group), areas, seed$People,
                                                 MoreArgs = list(afn, factors), SIMPLIFY = FALSE)
  # area
  groups = c('Zone', 'BuildingSurface:Detailed', 'FenestrationSurface:Detailed')
  seed[groups] = mapply(DefArea, seed[groups], c('zone', 'surf', 'fen'), area)
  # atm
  seed$'ElectricEquipment'$'equip_atm_equip' =
    DefATM(atm, seed$'ElectricEquipment'$'equip_atm_equip')
  # azimuth
  seed$'Building'$'agencia_bancaria' = DefAzimuth(azimuth, seed$'Building'$'agencia_bancaria')
  # boundaries
  surfs = c('4', '6', '8', '10', '11', '14', '16', '18',
            '20', '23', '24', '26', '26a', '28', '31', '32')
  surfs = paste0('surf_', surfs)
  seed$'BuildingSurface:Detailed'[surfs] =
    DefBoundaries(boundaries, seed$'BuildingSurface:Detailed'[surfs])
  # cop
  hvac = seed_path %>% basename() %>% str_extract('(?<=seed_).*(?=\\.json)')
  if (hvac == 'split') {
    groups = paste0('Coil:', c('Heat', 'Cool'), 'ing:DX:SingleSpeed')
  } else {
    groups = 'AirConditioner:VariableRefrigerantFlow'
  }
  seed[groups] = lapply(seed[groups], DefCOP, cop, hvac)
  # envelope
  components = paste0(c('par', 'cob'), '_ext')
  seed$'Construction'[components] = DefEnvelope(envelope, seed$'Construction'[components])
  # lights
  seed$'Lights' = DefLights(lights, seed$'Lights')
  seed$'Exterior:Lights'$'ilum_ext'$design_level = 1020 + 12.3*sqrt(area/600)*15
  # shgc
  seed$'WindowMaterial:SimpleGlazingSystem'$'vidro_temperado' =
    DefSHGC(shgc, seed$'WindowMaterial:SimpleGlazingSystem'$'vidro_temperado')
  # write the 'epJSON' file
  write_json(seed, model_path, pretty = TRUE, auto_unbox = TRUE)
}
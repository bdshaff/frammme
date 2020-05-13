mmm_load_submodels = function(nmp){
  
  PI = mmm_load_submodel(nmp = nmp, submodel = "PI")
  KBA = mmm_load_submodel(nmp = nmp, submodel = "KBA")
  PC = mmm_load_submodel(nmp = nmp, submodel = "PC")
  OAO = mmm_load_submodel(nmp = nmp, submodel = "OAO")
  SRC = mmm_load_submodel(nmp = nmp, submodel = "SRC")
  MA = mmm_load_submodel(nmp = nmp, submodel = "MA")
  
  return(list(PI = PI,
              KBA = KBA,
              PC = PC,
              OAO = OAO,
              SRC = SRC,
              MA = MA))
  
}
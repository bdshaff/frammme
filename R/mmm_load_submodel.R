mmm_load_submodel = function(nmp, submodel){

  if(nmp %in% c("ttn", "nv", "fro")){
    segment = "truck"
  }else if(nmp %in% c("alt", "lef", "sen", "ver", "max")){
    segment = "sedan"
  }else if(nmp %in% c("arm", "mur", "rge", "rgs", "pth")){
    segment = "suv"
  }

  existing_submodels = stringr::str_extract(list.files(here::here("output","SubModels",submodel)),"^.*(?=-)")

  if(nmp %in% existing_submodels){
    MODEL = readRDS(here::here("output","SubModels",submodel,str_c(nmp,"-",submodel,".Rds")))
    message(str_c(nmp,"-",submodel,".Rds"))
  }else if(segment %in% existing_submodels){
    MODEL = readRDS(here::here("output","SubModels",submodel,str_c(segment,"-",submodel,".Rds")))
    message(str_c(segment,"-",submodel,".Rds"))
  }else{
    MODEL = readRDS(here::here("output","SubModels",submodel,str_c("all-",submodel,".Rds")))
    message(str_c("all-",submodel,".Rds"))
  }
  return(MODEL)
}

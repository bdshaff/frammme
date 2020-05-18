nmp_dispatch = function(NMP){
  if(NMP == "TTN"){
    NAMEPLATE = "Titan"
    nmp = "ttn"
  }else if(NMP == "VER"){
    NAMEPLATE = "Versa"
    nmp = "ver"
  }else if(NMP == "ALT"){
    NAMEPLATE = "Altima"
    nmp = "alt"
  }else if(NMP == "ARM"){
    NAMEPLATE = "Armada"
    nmp = "arm"
  }else if(NMP == "FRO"){
    NAMEPLATE = "Frontier"
    nmp = "fro"
  }else if(NMP == "LEF"){
    NAMEPLATE = "LEAF"
    nmp = "lef"
  }else if(NMP == "RGE"){
    NAMEPLATE = "Rogue"
    nmp = "rge"
  }else if(NMP == "RGS"){
    NAMEPLATE = "RogueSport"
    nmp = "rgs"
  }else if(NMP == "MUR"){
    NAMEPLATE = "Murano"
    nmp = "mur"
  }else if(NMP == "MAX"){
    NAMEPLATE = "Maxima"
    nmp = "max"
  }else if(NMP == "PTH"){
    NAMEPLATE = "Pathfinder"
    nmp = "pth"
  }else if(NMP == "SEN"){
    NAMEPLATE = "Sentra"
    nmp = "sen"
  }else if(NMP == "NV"){
    NAMEPLATE = "NV"
    nmp = "nv"
  }else if(NMP == "ALL"){
    NAMEPLATE = "ALL"
    nmp = "all"
  }else{
    print(NMP)
    message("need to configure")
  }
  return(list(NAMEPLATE = NAMEPLATE,
              nmp = nmp,
              NMP = NMP))
}

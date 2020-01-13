library(xml2)
library(tidyverse)

TypeInvoice <- function(str1){
  num=substr(i, nchar(i)-2, nchar(i))  
  return(num)
}


LecturaDatos=function(Direccion){
  setwd(Direccion)
  archivos=list.files()
  Base=0
  for(i in archivos){
    if(TypeInvoice(i)=="xml"){
      
      x=tryCatch(read_xml(i),error=function(e) e, warning=function(w) w)
      
      if(is(x, "error")){
        print(paste("La lectura del archivo xml ", i, " no fue exitosa, revisar el contenido del mismo"))
      }else{
        
        #Encabezado
        
        Encabezado_carcateri=tryCatch(xml_attrs(xml_find_all(x, "//cfdi:Comprobante"), ns = character())[[1]],error=function(e) e, warning=function(w) w)
        if(is(Encabezado_carcateri,"error")){
          serie_nomina=folio_nomina=formaPago=version_nomina=fecha_nomina=subtotal=moneda=total=LugarExpedicion=NA
        }else{
          names(Encabezado_carcateri)=tolower(names(Encabezado_carcateri))
          serie_nomina=as.character(Encabezado_carcateri["serie"]) #1 a 25 alfanumerico
        }
        
        
        
        if(length(Base)==1){
          Base=data.frame(i,serie_nomina,folio_nomina,formaPago,version_nomina,fecha_nomina,subtotal,moneda,total,LugarExpedicion,rfc_emisor,nombre_emisor,regimnfiscal,rfc_receptor,nombre_receptor,usocdfi_receptor,ClaveProdServ,cantidad,claveunidad,descripcion,importe,descuento,version_nomina_2,TipoNomina,FechaPago,fechainicialpago,fechafinalpago,numdiaspagados,totalpercepciones,totaldeducciones,totalotrospagos,RegistroPatronal,numseguridadsocial,fechainiciorellaboral,antiguedad,TipoContrato,tiporegimen,numempleado,puesto,riesgopuesto,periodicidadpago,banco,salariobasecotapor,salariodiariointegrado,claveentfed,totalgravado,totalexento,totalsueldos,totalotrasdeducciones,totalimpuestosretenidos)
        }else{
          base=data.frame(i, serie_nomina,folio_nomina,formaPago,version_nomina,fecha_nomina,subtotal,moneda,total,LugarExpedicion,rfc_emisor,nombre_emisor,regimnfiscal,rfc_receptor,nombre_receptor,usocdfi_receptor,ClaveProdServ,cantidad,claveunidad,descripcion,importe,descuento,version_nomina_2,TipoNomina,FechaPago,fechainicialpago,fechafinalpago,numdiaspagados,totalpercepciones,totaldeducciones,totalotrospagos,RegistroPatronal,numseguridadsocial,fechainiciorellaboral,antiguedad,TipoContrato,tiporegimen,numempleado,puesto,riesgopuesto,periodicidadpago,banco,salariobasecotapor,salariodiariointegrado,claveentfed,totalgravado,totalexento,totalsueldos,totalotrasdeducciones,totalimpuestosretenidos)
          Base=rbind(Base, base)
        }
        
        nombres=c("serie_nomina","folio_nomina","formaPago","version_nomina","fecha_nomina","subtotal","moneda","total","LugarExpedicion","rfc_emisor","nombre_emisor","regimnfiscal","rfc_receptor","nombre_receptor","usocdfi_receptor","ClaveProdServ","cantidad","claveunidad","descripcion","importe","descuento","version_nomina_2","TipoNomina","FechaPago","fechainicialpago","fechafinalpago","numdiaspagados","totalpercepciones","totaldeducciones","totalotrospagos","RegistroPatronal","numseguridadsocial","fechainiciorellaboral","antiguedad","TipoContrato","tiporegimen","numempleado","puesto","riesgopuesto","periodicidadpago","banco","salariobasecotapor","salariodiariointegrado","claveentfed","totalgravado","totalexento","totalsueldos","totalotrasdeducciones","totalimpuestosretenidos")
        rm(list = nombres)
      }
    }else{
      print(paste("El arhivo ", i, " no es un formato compatible"))
    }
  }    
  nombres_user=c("Nombre Factura", "Serie","Folio","FormaPago","Version CFDI","Fecha","Subtotal","Moneda","Total","LugarExpedicion","rfc_emisor","nombre_emisor","RegimenFiscal","rfc_receptor","nombre_receptor","Uso de CFDI","ClaveProdServ","Cantidad","ClaveUnidad","Descripci�n","Importe","Descuento","Version Nomina","TipoNomina","FechaPago","Fecha Inicial Pago","FechaFinalPago","NumDiasPagados","Total Percepciones","Total Feducciones","Total Otros Pagos","Registro Patronal","Num Seguridad Social","Fecha Inicio Rel Laboral","Antiguedad","Tipo Contrato","Tipo Regimen","Num Empleado","Puesto","Riesgo Puesto","Periodicidad Pago","Banco","Salario Base Cotapor","Salario Diario Integrado","Clave Ent Fed","Total Gravado","Total Exento","Total Sueldos","Total Otras Deducciones","Total Impuestos Retenidos")
  return(Base)
  write.csv(Base, file="Facturas.csv")
    
  }
  
  tiempo=proc.time()
  Base=LecturaDatos("C:/Users/suma12/Desktop/Facturas_nomina/resumen")
  proc.time()-tiempo
  
  tail(Base, 2)
  
  View(Base)
  
  
  
  rm(list=ls())
  
  
VFVen = function() {
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Calcular el valor futuro de la anualidad vencida
  VF = A * (((1 + tasaI)^T - 1) / tasaI)
  
  cat("El valor futuro de la anualidad vencida es: $", round(VF, 4), "\n")
}

pagoVFVen = function() {
  cat("Ingresa el valor futuro conocido (VF) ")
  VF = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (T): ")
  T <- as.numeric(readline(prompt = ""))
  
  A = VF / (((1 + tasaI)^T -1) / tasaI)
  
  cat("La anualidad vencida es: $", round(A, 4), "\n")
}

## TASA NECESARIA
tasaVFVen = function() {
  VF = 3038688.17
  t = 960
  A = 500
  
  objective_function = function(r) {
    A * ((1 + r) ** t - 1) / r
  }
  
  result = optimize(
    f = function(r) abs(VF - objective_function(r)),
    interval = c(0, 0.1),
    tol = 1e-6
  )
  
  res = result$minimum * 100 * 24
  print(paste("El valor de la tasa de interés es ", round(res, digits = 2), "%"))
}



periodosVFVen = function() 
{
  cat("Ingresa el valor futuro conocido (VF): ")
  VF = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la anualidad vencida (A): ")
  A = as.numeric(readline(prompt = ""))
  
  # Calcular el número de periodos (T)
  T = log(((VF*tasaI)/A)+1)/log(1+tasaI)
  
  cat("El número de periodos necesario es: ", round(T, 4), "\n")
}


##  VALOR ACTUAL 


VAVen = function() 
{
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaÌ = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Calcular el Valor Actual de la anualidad vencida
  VA = A * ((1 - (1 + tasaI)^(-T)) / tasaI)
  
  cat("El Valor Actual de la anualidad vencida es: $", round(VA, 4), "\n")
}

pagoVAVen = function() 
{
  cat("Ingresa el Valor Actual (VA): ")
  VA = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Calcular la anualidad para el Valor Actual
  A = VA * (tasaI / (1 - (1 + tasaI)^(-T)))
  
  cat("La anualidad para el Valor Actual es: $", round(A, 4), "\n")
}

## TASA NECESARIA PARA ANUALIDAD vencida
tasaVAVen = function(VA, t, A) 
{
  VA = 19349.6
  t =48
  A = 450
  objective_function = function(r) 
  {
    A * (1 - (1 + r) ** -t) / r
  }
  
  dif = 5000000
  
  result = optimize(
    f = function(r) abs(VA - objective_function(r)),
    interval = c(0, 0.1),
    tol = 1e-6)
  
  return(result$minimum * 100 * 24)  ## el 24 por la division de la tasa por los periodos
  
  res = tasaVAVen(VA, t, A)
  print(paste("El valor de la tasa de interés es ", round(res, digits = 2),"%"))
}





periodosVAVen = function() 
{
  cat("Ingresa el Valor Actual (VA): ")
  VA = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  # Calcular el número de periodos para el Valor Actual
  T = -log(1 - (VA * tasaI) / A) / log(1 + tasaI)
  
  cat("El número de periodos necesario es: ", round(T, 4), "\n")
}





## SEGUNDO MENU ANUALIDADES ANTICIPADAS

VFAnt = function() {
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  VF = A * (((1 + tasaI)^T - 1) / tasaI) * (1 + tasaI)
  
  cat("El valor futuro de la anualidad anticipada es: $", round(VF, 4), "\n")
}

pagoVFAnt = function() {
  cat("Ingresa el valor futuro de la anualidad anticipada (VF): ")
  VF = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Corrección en la fórmula
  A = VF*tasaI / ((((1 + tasaI)^T - 1)) * (1 + tasaI))
  
  cat("La anualidad anticipada es: $", round(A, 4), "\n")
}

### TASA NECESARIA 
tasaVFAnt = function(VF, t, A) 
{
  VF = 3048184.07
  t = 960
  A = 500
  objective_function = function(r) 
  {
    A * ((1 + r) * ((1 + r) ** t - 1) / r)
  }
  
  result = optimize(
    f = function(r) abs(VF - objective_function(r)),
    interval = c(0, 0.1),
    tol = 1e-6)
  
  return(result$minimum * 100 * 24) ## el 24 por la division de la tasa por los periodos
  res = tasaVFAnt(VF, t, A)
  print(paste("El valor de la tasa de interés es ", round(res, digits = 2)))
}


periodosVFAnt = function() {
  cat("Ingresa el Valor Actual (VA): ")
  VF = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasa = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  # Corrección en la fórmula
  T = log(1+(VF/(A*(1+tasaI)))*tasaI)/log(1+tasaI)
  
  cat("El número de periodos necesario es: ", round(T, 4), "\n")
}



## PARA EL VALOR ACTUAL

VAAnt = function() 
{
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Calcular el Valor Actual de la anualidad anticipada
  VA = A * ((1 - (1 + tasaI)^(-T)) / tasaI) * (1 + tasaI)
  
  cat("El Valor Actual de la anualidad anticipada es: $", round(VA, 4), "\n")
}

pagoVAAnt = function() 
{
  cat("Ingresa el Valor Actual (VA): ")
  VA = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  # Calcular la anualidad para el Valor Actual en anualidad anticipada
  A = VA / ((1 - (1 + tasaI)^(-T)) / tasaI) / (1 + tasaI)
  
  cat("La anualidad para el Valor Actual es: $", round(A, 4), "\n")
}

## calculo de la tasa
tasaVAAnt = function(VA, t, A) 
{
  VA = 19438.28
  t = 48
  A = 450
  objective_function = function(r) 
  {
    A * (1 - (1 + r) ** -t) / r / (1 + r)
  }
  
  dif = 5000000
  
  result = optimize(
    f = function(r) abs(VA - objective_function(r)),
    interval = c(0, 0.1),
    tol = 1e-6)
  
  
  return(result$minimum * 100 * 24)  ## el 24 por la division de la tasa por los periodos
  
  res = tasaVAAnt(VA, t, A)
  print(paste("El valor de la tasa de interés es ", round(res, digits = 2)))
}



periodosVAAnt = function() 
{
  cat("Ingresa el Valor Actual (VA): ")
  VA = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  T = -log(1-(VA/(A*(1+tasaI))*tasaI))/log(1+tasaI)
  
  cat("El número de periodos necesario es: ", round(T, 4), "\n")
}



## diferidas con pagos vencidos

VADif = function() {
  cat("Ingresa el pago anual (A): ")
  A = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  tasaI = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos diferidos (D): ")
  D = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número total de periodos (n): ")
  n = as.numeric(readline(prompt = ""))
  
  VAD = (A * ((1 - (1 + tasaI)^-D) / tasaI)) / (1 + tasaI)^n
  
  cat("El Valor Actual con anualidades diferidas y pagos vencidos es: $", round(VAD, 4), "\n")
}

pagoVFDif = function() {
  cat("Ingresa el Valor Actual (VA): ")
  VA = as.numeric(readline(prompt = ""))
  
  cat("Ingresa la tasa de interés anual (como decimal): ")
  Interes = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número de periodos diferidos (D): ")
  dif = as.numeric(readline(prompt = ""))
  
  cat("Ingresa el número total de periodos (n): ")
  T = as.numeric(readline(prompt = ""))
  
  tasaII = (1 + Interes)^(1/24) - 1
  nP = T * 24
  VAD = VAVen(A, tasaI, T)
  A = VAD / ((1 - (1 + tasaII)^-nP) / tasaII) * (1 + tasaII)
  
  cat("El monto del pago anual con anualidades diferidas y pagos vencidos es: $", round(A, 4), "\n")
}


anualidadesVencidas = function(opcion) {
  cat("Has seleccionado Anualidades Vencidas.\n")
  cat("Selecciona una opción:
    para valor futuro conodido: 
    1: valor futuro
    2: anualidad para el valor futuro
    3: tasa necesaria
    4: numero de peridodos 
    para valor actual o presente conocido: 
    5: valor actual
    6: anualidad para el valor actual
    7: tasa necesaria
    8: numero de periodos \n")
  if (opcion == 1) {
    VFVen()
  } else if (opcion == 2) {
    pagoVFVen()
  } else if (opcion == 3) {
    tasaVFVen()
  } else if (opcion == 4) {
    periodosVFVen()
  } else if (opcion == 5) {
    VAVen()
  } else if (opcion == 6) {
    pagoVAVen()
  } else if (opcion == 7) {
    tasaVAVen()
  } else if (opcion == 8) {
    periodosVAVen()
  } else {
    cat("Opción no válida. Por favor, selecciona un número entre 1 y 8.\n")
  }
}

# Cálculos para anualidades anticipadas
anualidadesAnticipadas = function(opcion) {
  cat("Has seleccionado Anualidades Anticipadas.\n")
  cat("Selecciona una opción:
    para valor futuro conodido: 
    1: valor futuro
    2: anualidad para el valor futuro
    3: tasa necesaria
    4: numero de peridodos 
    para valor actual o presente conocido: 
    5: valor actual
    6: anualidad para el valor actual
    7: tasa necesaria
    8: numero de periodos \n")
  if (opcion == 1) {
    VFAnt()
  } else if (opcion == 2) {
    pagoVFAnt()
  } else if (opcion == 3) {
    tasaVFAnt()
  } else if (opcion == 4) {
    periodosVFAnt()
  } else if (opcion == 5) {
    VAAnt()
  } else if (opcion == 6) {
    pagoVAAnt()
  } else if (opcion == 7) {
    tasaVAAnt()
  } else if (opcion == 8) {
    periodosVAAnt()
  } else {
    cat("Opción no válida. Por favor, selecciona un número entre 1 y 8.\n")
  }
}


# Cálculos para anualidades diferidas con pagos vencidos
anualidadesDiferidas = function(opcion) {
  cat("Has seleccionado la opcion de anualidades diferidas con pagos vencidos.\n")
  cat("Selecciona una opción: 
    1: Para el valor actual.
    2: para el monto de pagos A.\n")
  if (opcion == 1) {
    return(VADif())
  } else if (opcion == 2) {
    return(pagoVFDif())
  } else {
    cat("Opción no válida. Por favor, selecciona un número entre 1 y 2.\n")
  }
}


# Menú principal
menu_principal = function() {
  opcion = 0
  
  while (opcion != 4) {
    cat("Menú Principal:\n")
    cat("1. Anualidades Vencidas\n")
    cat("2. Anualidades Anticipadas\n")
    cat("3. Anualidades diferidas con pagos vencidos\n")
    cat("4. Salir\n")
    
    opcion = as.numeric(readline("Selecciona una opción: "))
    
    if (opcion == 1) {
      anualidadesVencidas(as.numeric(readline(prompt = "Selecciona una opción para Anualidades Vencidas: ")))
    } else if (opcion == 2) {
      anualidadesAnticipadas(as.numeric(readline(prompt = "Selecciona una opción para Anualidades Anticipadas: ")))
    } else if (opcion == 3) {
      anualidadesDiferidas(as.numeric(readline(prompt = "Selecciona una opción para Anualidades Diferidas: ")))
    } else if (opcion == 4) {
      cat("Saliendo del programa. Adiós.\n")
    } else {
      cat("Opción no válida. Inténtalo de nuevo.\n")
    }
  }
}


# Llamamos a la función del menú principal para iniciar el programa
menu_principal()
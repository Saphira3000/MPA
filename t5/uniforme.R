uniforme = function(n, semilla, a, c, m) {
    datos = numeric()
    x = semilla
    while (length(datos) < n) {
        x = (a * x + c) %% m
        datos = c(datos, x)
    }
    return(datos / (m - 1))
}

#summary(uniforme(5000, 27))

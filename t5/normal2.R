normal2 = function(mu, sigma, u) {
    #u = runif(2);
    z0 = sqrt(-2 * log(u[1])) * cos(2 * pi * u[2]);
    z1 = sqrt(-2 * log(u[1])) * sin(2 * pi * u[2]);
    datos = c(z0, z1)
    return (sigma * datos + mu);
}
#cat(gaussian(0, 1), '\n')




generate t6 = (time == 6)
generate t12 = (time == 12)
generate armt6 = arm*t6
generate armt12 = arm*t12
summarize y0
scalar mean_y0 = r(mean)
generate y0c = y0 - mean_y0

mixed y y0c t6 t12 arm armt6 armt12  || id:, var noconst residuals(unstr, t(time))

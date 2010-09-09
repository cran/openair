quickText <- function(text, auto.text = TRUE){

#the lookup table version

	###return if auto.text false
	if(!auto.text) {return(ans <- text)}

#setup string to evaluate
	ans <- paste("expression(paste('", text, " ", sep = "")

#lookup tables
	##format gsub("text","replacement",ans)

#nox variants
	ans <- gsub("NO2","' 'NO' [2] * '", ans)
	ans <- gsub("no2","' 'NO' [2] * '", ans)
	ans <- gsub("NOX","' 'NO' [X] * '", ans)
	ans <- gsub("nox","' 'NO' [X] * '", ans)
	ans <- gsub("NOx","' 'NO' [X] * '", ans)

# to CAPS
	ans <- gsub("co ","' 'CO ' '", ans)
        ans <- gsub("co,","' 'CO,' '", ans)
	ans <- gsub("no ","' 'NO ' '", ans)
        ans <- gsub("no,","' 'NO,' '", ans)
	ans <- gsub("nmhc","' 'NMHC' '", ans)
	ans <- gsub("ws","' 'wind spd.' '",
			ans)
	ans <- gsub("wd","' 'wind dir.' '", ans)
	ans <- gsub("rh ","' 'relative humidity' '", ans)
	ans <- gsub("cl ","' 'cloud cover' '", ans)
	ans <- gsub("temp","' 'temperature' '", ans)

#pm10 variants
	ans <- gsub("PM10","' 'PM' [10] * '", ans)
	ans <- gsub("pm10","' 'PM' [10] * '", ans)

#pm1
	ans <- gsub("pm1","' 'PM' [1] * '", ans)
	ans <- gsub("PM1","' 'PM' [1] * '", ans)

#pmc, pmcoarse variants
	ans <- gsub("pmc","' 'PM' [coarse] * '", ans)
	ans <- gsub("pmcoarse","' 'PM' [coarse] * '", ans)
	ans <- gsub("PMc","' 'PM' [coarse] * '", ans)
	ans <- gsub("PMcoarse","' 'PM' [coarse] * '", ans)

#pmf, pmfine, etc
	ans <- gsub("pmf","' 'PM' [fine] * '", ans)
	ans <- gsub("pmfine","' 'PM' [fine] * '", ans)
	ans <- gsub("PMf","' 'PM' [fine] * '", ans)
	ans <- gsub("PMfine","' 'PM' [fine] * '", ans)


#pm2.5 variants
	ans <- gsub("PM2.5","' 'PM' [2.5] * '", ans)
	ans <- gsub("pm2.5","' 'PM' [2.5] * '", ans)
	ans <- gsub("pm25","' 'PM' [2.5] * '", ans)
	ans <- gsub("PM2.5","' 'PM' [2.5] * '", ans)
	ans <- gsub("PM25","' 'PM' [2.5] * '", ans)
	ans <- gsub("pm25","' 'PM' [2.5] * '", ans)

#O3 variants
	ans <- gsub("O3","' 'O' [3] * '", ans)
	ans <- gsub("o3","' 'O' [3] * '", ans)
	ans <- gsub("ozone","' 'O' [3] * '", ans)

#CO2 variants
	ans <- gsub("CO2","' 'CO' [2] * '", ans)
	ans <- gsub("co2","' 'CO' [2] * '", ans)

#so2 variants
	ans <- gsub("SO2","' 'SO' [2] * '", ans)
	ans <- gsub("so2","' 'SO' [2] * '", ans)

#H2S variants
	ans <- gsub("H2S","' 'H' [2] * 'S''", ans)
	ans <- gsub("h2s","' 'H' [2] * 'S''", ans)

#CH4 variants
ans <- gsub("CH4","' 'CH' [4] * '", ans)
ans <- gsub("ch4","' 'CH' [4] * '", ans)


#degreesC variants
#did not include oC, 0C, oc, etc, because I think they are going to be commonplace
	ans <- gsub("dgrC", "' * degree * 'C' '", ans)
	ans <- gsub("degreeC", "' * degree * 'C' '", ans)
	ans <- gsub("degreesC", "' * degree * 'C' '", ans)
	ans <- gsub("degrees", "' * degree *'", ans)

#ug/m3, mg/m3, ng/m3 variants
	ans <- gsub("ug/m3", "' * mu * 'g m' ^-3 *'", ans)
	ans <- gsub("ug.m-3", "' * mu * 'g m' ^-3 *'", ans)
	ans <- gsub("ug m-3", "' * mu * 'g m' ^-3 *'", ans)
	ans <- gsub("ugm-3", "' * mu * 'g m' ^-3 *'", ans)
	ans <- gsub("mg/m3", "' * 'm' * 'g m' ^-3 *'", ans)
	ans <- gsub("mg.m-3", "' * 'm' * 'g m' ^-3 *'", ans)
	ans <- gsub("mg m-3", "' * 'm' * 'g m' ^-3 *'", ans)
	ans <- gsub("mgm-3", "' * 'm' * 'g m' ^-3 *'", ans)
	ans <- gsub("ng/m3", "' * 'n' * 'g m' ^-3 *'", ans)
	ans <- gsub("ng.m-3", "' * 'n' * 'g m' ^-3 *'", ans)
	ans <- gsub("ng m-3", "' * 'n' * 'g m' ^-3 *'", ans)
	ans <- gsub("ngm-3", "' * 'n' * 'g m' ^-3 *'", ans)

#m/s variants
	ans <- gsub("m/s", "' 'm s' ^-1 *'", ans)
	ans <- gsub("m.s-1", "' 'm s' ^-1 *'", ans)
	ans <- gsub("m s-1", "' 'm s' ^-1 *'", ans)

#emissions, speeds
	ans <- gsub("g/km", "' 'g km' ^-1 *'", ans)
	ans <- gsub("g/s", "' 'g s' ^-1 *'", ans)
	ans <- gsub("km/hr", "' 'km hr' ^-1 *'", ans)
        ans <- gsub("km/h", "' 'km hr' ^-1 *'", ans)
	ans <- gsub("km/hour", "' 'km hr' ^-1 *'", ans)

        ans <- gsub("km/hr/s", "' 'km hr' ^-1 ' s' ^-1 *'", ans)
        ans <- gsub("km/h/s", "km hr' ^-1 * ' s' ^-1 *'", ans)
	ans <- gsub("km/hour/s", "' 'km hr' ^-1 ' s' ^-1 *'", ans)

# R2
	ans <- gsub("r2", "R' ^2 *'", ans)
	ans <- gsub("R2", "R' ^2 *'", ans)


#tidy up output
	ans <- paste(ans, "'))", sep = "")
#ans <-substr(ans,21,21)

	if (substr(ans, 21, 21) == "*") {a <- ans
		ans <- paste(substr(a, 1, 20), substr(a, 22, nchar(a)), sep = "")}

	if (substr(ans,(nchar(ans) - 8),(nchar(ans) - 6)) == "] *")
	{a <- ans
		ans <- paste(substr(a, 1, (nchar(a) - 7)), substr(a, (nchar(a) - 5),
						nchar(a)),sep="")}

	ans <- gsub("''", "", ans)
	ans <- gsub("' '", "", ans)
	ans <- gsub("\\*  \\*", "~", ans)

	if (substr(ans, (nchar(ans) - 2), (nchar(ans) - 2)) == "*")
	{a <- ans
		ans <- paste(substr(a, 1, (nchar(a) - 2)), " ' ' ",
				substr(a, (nchar(a) - 1), nchar(a)), sep = "")}

#output
	if (inherits(try(eval(parse(text = ans)), TRUE), "try-error") == FALSE)
	{ ans <- eval(parse(text=ans)) } else { ans <- text }

}

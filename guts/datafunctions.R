#v 0.91

#actual data functions for argoslook
gDEFAULTCLON <- -75
gDEFAULTCLAT <- 35.6
g_lastzoom <- 4
g_lastlon <- gDEFAULTCLON
g_lastlat <- gDEFAULTCLAT

nitro <- function(alpha) {
	rgb(153/255, 0, 56/255, alpha)
}

carbon <- function(alpha) {
	rgb(0, 94/255, 153/255, alpha)
}

neonemph <- function(alpha) {
	rgb(0, 240/255, 181/255, alpha)
}

getdepths <- function(dat) {
	dese <- which(dat$What != "Message")
	-dat$DepthMax[dese]
}

gettimes <- function(dat) {
	dese <- which(dat$What != "Message")
	as.POSIXct(dat$Start[dese], tz = "UTC")
}

plotdives <- function(ddat, adat) {
	dese <- which(ddat$what != "Message")
	dat <- ddat[dese, ]
	times <- as.POSIXct(dat$startposix, tz = "UTC")
	
	zeroday <- as.POSIXct(format(times[1], "%Y-%m-%d"), tz = "UTC")
	lastday <- as.POSIXct(format(as.Date(times[length(times)]) + 1, "%Y-%m-%d"), tz = "UTC")
	
	axistimes <- seq(zeroday, lastday, by = "day")
	axistimeslabs <- format(axistimes, "%d%h%Y")
	
	times_to_average <- cbind(as.POSIXct(dat$startposix, tz = "UTC"),
		as.POSIXct(dat$endposix, tz = "UTC"))
	averaged_times <- apply(times_to_average, 1, mean)
	times <- as.POSIXct(averaged_times, origin = "1970-01-01", tz = "UTC")
	par(oma = c(2, 0, 0, 0), las = 1)
	plot(times, dat$dep, pch = 16, cex = .5, bty = 'n', xlab = "", ylab = "", axes = FALSE, ylim = c(min(dat$dep), max(dat$dep)))
	axis(2, at = c(0, min(ddat$dep, na.rm = TRUE)), label = NA, tcl = 0)
	axis(2, at = seq(0, min(ddat$dep, na.rm = TRUE), by = -250))
	axis.POSIXct(1, at = axistimes, labels = axistimeslabs, las = 2, tcl = -0.9, mgp = c(0, 1.5, 0.75))
	# axis.POSIXct(1, at = halfdayaxistimes, labels = NA, mgp = c(0, 0, 0.75))
	if(!is.null(adat)) {
		badies <- which(adat$locationquality %in% c("", "A", "B", "Z"))
		argostimes <- as.POSIXct(adat$date, tz = "UTC")
		axis.POSIXct(1, at = argostimes[badies], labels = NA, tcl = 1.25, col = neonemph(1), col.axis = nitro(1), mgp = c(0, 0, 0.75))
		axis.POSIXct(1, at = argostimes[-badies], labels = NA, tcl = 1.25, mgp = c(0, 0, 0.75))
		axis.POSIXct(1, at = argostimes, labels = NA, tcl = 0, mgp = c(0, 0, 0.75))
	}
	getgaps <- calculategaps(ddat)
	rect(
		as.POSIXct(getgaps$gaptimes_start, tz = "UTC"), min(ddat$dep, na.rm = TRUE),
		as.POSIXct(getgaps$gaptimes_end, tz = "UTC"), 0,
		col = nitro(.25), border = NA
	)
		
	curtime <- Sys.time()
	curtime_UTC <- format(curtime, tz = "UTC")
	prev48hour <- as.POSIXct(curtime_UTC, tz = "UTC") - 60*60*48
	abline(v = prev48hour, lty = 2, col = "purple")
}

plotlegend <- function(isdive, isargos) {
	plot(0, 0, bty = 'n', axes = FALSE, xlab = "", ylab = "", type = 'n')
	
	if(isdive) {
		leglab <- c("behavior messages", "gaps in behavior data", "48 hour cutoff")
		legpch <- c(16, 15, NA)
		legcol <- c("black", nitro(.5), "purple")
		leglty <- c(NA, NA, 2)
	
		if(isargos) {
			leglab <- c(leglab, "argos messages; qflag = (0, 1, 2, 3)", "argos messages; qflag = (nofix, A, B, Z)")
			legpch <- c(legpch, 15, 15)
			leglty <- c(leglty, NA, NA)
			legcol <- c(legcol, "black", neonemph(1))
		}	

		legend("topleft", leglab, pch = legpch, col = legcol, lty = leglty, bty = 'n')
	}
}

tableargos <- function(adat, qflags_checkboxes) {
	adat <- adat[rev(1:nrow(adat)), ]
	if(!is.null(qflags_checkboxes)) {
		qflag <- adat$locationquality
		dese <- which(qflag %in% qflags_checkboxes)
		adat <- adat[dese, ]
	}
	adat
}
 
plotargos <- function(adat, qflags_checkboxes, zoom) {
	argnona <- adat
	
	if(length(which(is.na(adat$longitude))) > 0) {
		argnona <- adat[-which(is.na(adat$longitude)), ]
	}
	
	xx <- argnona$longitude
	yy <- argnona$latitude
	qflag <- argnona$locationquality
	data.df <- data.frame(xx, yy, qflag)
	
	if(!is.null(qflags_checkboxes)) {
		dese <- which(qflag %in% qflags_checkboxes)
		data.df <- data.df[dese, ]
	}
	
	data.df[, 'latest'] <- "NO"
	data.df[nrow(data.df), 'latest'] <- "YES"
	
	if(zoom[2] == gDEFAULTCLON & zoom[3] == gDEFAULTCLAT) {	
		g_map <- switch(zoom[1], {mapzoom5}, {mapzoom7}, {mapzoom8}, {mapzoom9})
		g_lastzoom <<- zoom[1]
		g_lastlon <<- zoom[2]
		g_lastlat <<- zoom[3]
	} else if(zoom[1] != g_lastzoom | zoom[2] != g_lastlon | zoom[3] != g_lastlat) {
		g_map <<- get_map(location = c(lon = zoom[2], lat = zoom[3]), zoom = c(5, 7, 8, 9)[zoom[1]], maptype = "sat")
		g_lastzoom <<- zoom[1]
		g_lastlon <<- zoom[2]
		g_lastlat <<- zoom[3]
	}
	
	map <- ggmap(g_map) + geom_point(aes(x = xx, y = yy, colour = qflag, shape = latest), data = data.df)
	
	if(nrow(data.df) > 4) {
		lastfive <- (nrow(data.df) - 4):nrow(data.df)
		map <- map + geom_path(arrow = arrow(), aes(x = xx, y = yy), data = data.df[lastfive, ], color = "white")
	}
	
	map
}

plotdepdur <- function(ddat, baselinesp, baselinedep) {
	dese <- which(ddat$what == "Dive")
	dat <- ddat[dese, ]
	sur <- ddat[which(ddat$what == "Surface"), ]
	
	if(baselinesp != "other" & !is.null(baselinesp)) {
		if(baselinesp == "zca") {
			baselinenum <- 1
		} else if(baselinesp == "gma") {
			baselinenum <- 2
		}
		
		baseline <- base[[baselinenum]]
		basedive <- baseline[which(baseline$what == "Dive"), ]
		basesurf <- baseline[which(baseline$what == "Surface"), ]
		
		#if baselinesp is zca baselinedep is actually duration in minutes
		#if baselinesp is gma baselinedep is depth in meters 
		if(!is.null(baselinedep) && !is.na(baselinedep)) {
			if(baselinenum == 2) {
				basedive <- basedive[which(-basedive$dep > baselinedep), ]
			} else if(baselinenum == 1) {
				basedive <- basedive[which(basedive$dur > baselinedep*60), ]
			}
		}
		
		par(las = 1)
		lay <- matrix(c( 1, 1, 2, 2,
						 1, 1, 2, 2,
						 3, 3, 4, 4, 
						 3, 3, 4, 4,
						 0, 5, 5, 0,
						 0, 5, 5, 0), 6, 4, byrow = TRUE)
		layout(lay)
		
		hist(dat$dur/60, nclass = 20, ylab = "", xlab = "dive duration (minutes)", main = "", col = carbon(.5))
		a <- density(dat$dur/60)
		b <- density(basedive$dur/60)
		plot(c(a$x, b$x), c(a$y, b$y), type = 'n', xlab = "dive duration (minutes)", ylab = "", bty = "n")
		lines(a, lty = 1, col = carbon(.5))
		lines(b, lty = 1, col = nitro(.5))
		legend("topright", c("current tag", "baseline tags"), lty = c(1, 1), col = c(carbon(.5), nitro(.5)), bty = 'n')
		
		hist(-dat$dep, nclass = 20, ylab = "", xlab = "depth (m)", main = "", col = carbon(.5))
		a <- density(-dat$dep)
		b <- density(-basedive$dep)
		plot(c(a$x, b$x), c(a$y, b$y), type = 'n', xlab = "depth (m)", ylab = "", bty = "n")
		lines(a, lty = 1, col = carbon(.5))
		lines(b, lty = 1, col = nitro(.5))
		legend("topright", c("current tag", "baseline tags"), lty = c(1, 1), col = c(carbon(.5), nitro(.5)), bty = 'n')
			
		hist(sur$dur/60, nclass = 20, ylab = "", xlab = "surface duration (minutes)", main = "", col = carbon(.5))
		# a <- density(sur$dur/60)
		# b <- density(basesurf$dur/60)
		# plot(c(a$x, b$x), c(a$y, b$y), type = 'n', xlab = "surface duration (minutes)", ylab = "", bty = "n")
		# lines(a, lty = 1, col = carbon(.5))
		# lines(b, lty = 1, col = nitro(.5))
		# legend("topright", c("current tag", "baseline tags"), lty = c(1, 1), col = c(carbon(.5), nitro(.5)), bty = 'n')
	} else {
		par(las = 1)
		lay <- matrix(c( 1, 1, 2, 2,
						 1, 1, 2, 2,
						 0, 3, 3, 0,
						 0, 3, 3, 0), 4, 4, byrow = TRUE)
						 
		layout(lay)
		hist(dat$dur/60, nclass = 20, ylab = "", xlab = "dive duration (minutes)", main = "", col = carbon(.5))
		hist(-dat$dep, nclass = 20, ylab = "", xlab = "depth (m)", main = "", col = carbon(.5))
		hist(sur$dur/60, nclass = 20, ylab = "", xlab = "surface duration (minutes)", main = "", col = carbon(.5))
	}
}

plotdepdurtab <- function(ddat) {
	dives <- ddat[which(ddat$what == "Dive"), ]
	surfs <- ddat[which(ddat$what == "Surface"), ]
	
	parameters <- c("dive duration (min)", "dive depth (m)", "surface dur (min)")
	lowerquant <- c(quantile(dives$dur/60, .025), quantile(-dives$dep, .025), quantile(surfs$dur/60, .025))
	median <- c(quantile(dives$dur/60, .5), quantile(-dives$dep, .5), quantile(surfs$dur/60, .5))
	upperquant <- c(quantile(dives$dur/60, .975), quantile(-dives$dep, .975), quantile(surfs$dur/60, .975))
	# sds <- c(sd(dives$dur/60), sd(-dives$dep), sd(surfs$dur/60))
	ns <- c(length(dives$dur/60), length(dives$dur), length(surfs$dur))
	sums <- c(sum(dives$dur/60), NA, sum(surfs$dur/60))
	
	data.frame(parameter = parameters, q0.025 = round(lowerquant, 2), q0.5 = round(median, 2), q0.975 = round(upperquant), n = ns, sum = round(sums, 2))
}

makegapstatstab <- function(ddat) {
	parameter <- c("mean gap length (hours)", "sd gap length (hours)", "n gaps", "sum of gaps (hours)", "total ellapsed time on tag", "proportion gaps")
	getgaps <- calculategaps(ddat)

	data.frame(parameter, "current tag" = round(c(mean(getgaps$gaps, na.rm = TRUE),
		sd(getgaps$gaps, na.rm = TRUE),
		length(which(!is.na(getgaps$gaps))),
		sum(getgaps$gaps, na.rm = TRUE),
		getgaps$totalellapsedtime,
		sum(getgaps$gaps, na.rm = TRUE) / getgaps$totalellapsedtime), 2))
}

prepargos <- function(arg) {
	names(arg) <- tolower(names(arg))
	dates <- paste(arg$date, "UTC", sep = " ")
	arg$date <- dates
	arg[order(arg$date), ]
	arg[, c('ptt', 'date', 'longitude', 'latitude', 'locationquality', 'satellite', 'power')]
}

prepbehavior <- function(bev) {
	names(bev) <- tolower(names(bev))
	starts <- paste(bev$start, "UTC", sep = " ")
	ends <- paste(bev$end, "UTC", sep = " ")
	bev[, 'startposix'] <- as.character(starts)
	bev[, 'endposix'] <- as.character(ends)
	bev <- bev[order(starts), ]
	depth <- -bev$depthmax
	depth[which(bev$what == "Surface")] <- 0
	bev[, 'dep'] <- depth
	bev[, 'dur'] <- apply(bev[, c('durationmin', 'durationmax')], 1, mean)
	behavior <- bev[, c('what', 'startposix', 'endposix', 'dep', 'dur')]
	behavior
}

calculategaps <- function(ddat) {
	gaps <- vector()
	gaptimes_start <- vector()
	gaptimes_end <- vector()
	totalellapsedtime <- NA
	
	dese <- which(ddat$what == "Message")
	message_start <- ddat[dese, 'startposix']
	message_end <- ddat[dese, 'endposix']
	
	intervals_ends <- as.character(message_end)
	intervals_starts <- as.character(message_start)
	
	intervals_ends <- as.POSIXct(intervals_ends, tz = "UTC")
	intervals_starts <- as.POSIXct(intervals_starts, tz = "UTC")
	
	en <- length(intervals_ends)
	
	gaps <- as.numeric(
		difftime(intervals_starts[2:en], 
		intervals_ends[1:(en-1)],
		unit = "hour"))
		
	gaptimes_start <- intervals_ends[1:(en-1)]
	gaptimes_end <- intervals_starts[2:en]
	
	smallgaps <- which(gaps < 2/60)
	biggaps <- which(gaps >= 2/60)
	if(length(smallgaps) > 0) {
		gaps[smallgaps] <- NA
		gaptimes_start[smallgaps] <- NA
		gaptimes_end[smallgaps] <- NA
	}
	totalellapsedtime <- as.numeric(difftime(intervals_ends[en], intervals_starts[1], unit = "hour"))
	
	# stretchid <- vector()
	# count <- 1
	# i <- 1
	# while(i <= length(gaptimes_start)) {
		# while(!(i %in% biggaps) & i <= length(gaptimes_start)) {
			# stretchid[i] <- count
			# i <- i + 1
		# }
		# if(i <= length(gaptimes_start)) {
			# stretchid[i] <- count
			# count <- count + 1
			# i <- i + 1
		# }
	# }
	
	list(gaps = gaps, gaptimes_start = gaptimes_start, gaptimes_end = gaptimes_end, totalellapsedtime = totalellapsedtime)
}

leafplotargos <- function(adat, qflags) {
		datapoints <- data.frame(lon = adat$longitude, lat = adat$latitude, qflag = as.character(adat$locationquality), date = adat$date)
		if(length(which(is.na(datapoints$lon))) > 0) {
			datapoints <- datapoints[-which(is.na(datapoints$lon)),]
		}
		dese <- which(datapoints$qflag %in% qflags)
		datapoints <- datapoints[dese, ]
		n <- nrow(datapoints)
		dropback <- 4
		if(n < 4) dropback = 0
		radius <- rep(2, n)
		radius[n] <- 5
	
		pal <- colorFactor(c(rgb(.5, .5, .5), rgb(0, .5, .75), rgb(.5, .5, 1),  rgb(0, 0, 0), rgb(1, 1, 1), rgb(1, 1, 1), rgb(1, 1, 1)), domain = c("Z", "A", "B", "0", "1", "2", "3"))
		leaflet(datapoints) %>%
	    addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
	    addCircleMarkers(data = datapoints[, 1:2],
	    	popup = paste("qflag = ", datapoints$qflag, "<br>", datapoints$date, "<br>(", round(datapoints$lat, 4), ", ", round(datapoints$lon, 4), ")", sep = ""),
	      	radius = radius,
	      	color = pal(datapoints$qflag)
	     ) %>%
		addLegend("bottomright", pal = pal, values = ~qflag, opacity = 1) %>%
	    addPolylines(lng = ~lon[(n-dropback):n], lat = ~lat[(n-dropback):n], weight = 2, color = "white")
}
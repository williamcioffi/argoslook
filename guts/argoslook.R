#v 0.91
require(leaflet)
require(shiny)
# require(ggmap)
source("guts/datafunctions.R")
#load("guts/mapzoomdat_small")
gm_baseline <- read.table("guts/allgms.csv", header = TRUE, sep = ',')
zc_baseline <- read.table("guts/allzcs.csv", header = TRUE, sep = ',')
zc_baseline <- zc_baseline[-which(zc_baseline$dur > 10^5), ]
base <- list(zc_baseline, gm_baseline)

rm(list = c("gm_baseline", "zc_baseline"))

ui <- fluidPage(
	sidebarLayout(
		sidebarPanel(
			actionButton("quit", "quit"),
			hr(),
			# p("use forms below to upload behavior and argos files exported from DAP processor."),
			# fileInput("sourcedir", "",
				# accept = c("text/csv", "text/coma-separate-values", "text/plain",
				# ".csv"), multiple = TRUE, width = "225"),
			
			fileInput("file1", "choose behavior csv:",
				accept = c(
				"text/csv",
				"text/comma-separated-values,text/plain",
				".csv"), width = "225"),
			fileInput("file2", "choose argos csv:",
				accept = c(
				"text/csv",
				"text/comma-separated-values,text/plain",
				".csv"), width = "225"),
				uiOutput("tagselect"),
			selectInput("basesp", "baseline species", c("beakers", "pdubs", "other"), width = "150px"),
			width = 3),

		mainPanel(
			tabsetPanel(
				tabPanel("dive plot",
					# actionButton("prev", "prev day"),
					# actionButton("nxt", "next day"), 
					hr(),
					plotOutput("plot"),
					plotOutput("legend")),
				tabPanel("gap stats",
					tableOutput("gapstatstable")),
				tabPanel("dive stats",
					uiOutput("baseline_mindep"),
					plotOutput("divedepdur", height = "800px"),
					tableOutput("divestatstable")),
				# tabPanel("pos table",
					# uiOutput("qflags2"),
					# tableOutput("postable")),
				tabPanel("pos plot (leaflet)",
					p("click on points for time/lat/lon; big point it latest"),
					uiOutput("qflags3"),
					leafletOutput("leafmap", height = "600px", width = "600px"),
					tableOutput("postable")
				)))))
				# tabPanel("pos plot (ggmap)",
					# #div(style = "display: inline-block; width: 150px", 
					# div(style = "display: inline-block; padding: 0px 20px 0px 0px; vertical-align:top; width = 300px;", uiOutput("qflags")),
					# # div(style = "display: inline; width = 20px;", HTML("<br>")),
					# div(style = "display: inline-block; padding: 0px 20px 0px 0px; vertical-align:top; width = 75px;", numericInput("centerlon", "center lon", value = "-75.0", step = 0.25, width = "75px")),
					# div(style = "display: inline-block; padding: 0px 20px 0px 0px; vertical-align:top; width = 75px;", numericInput("centerlat", "center lat", value = "35.6", step = 0.25, width = "75px")),
					# div(style = "display: inline-block; vertical-align:top; width = 150px;", sliderInput("zoom", "zoom", min = 1, max = 4, value = 4, width = "150px")),
					# sidebarLayout(position = "right", 
						# sidebarPanel(
							# uiOutput("qflags"),
							# sliderInput("zoom", "zoom:", min = 1, max = 4, value = 4, width = "150px"),
							# div(style = "display: inline-block; padding: 0px 20px 0px 0px; vertical-align:top; width = 75px;", numericInput("centerlon", "lon:", value = "-75.0", step = 0.25, width = "75px")),
							# div(style = "display: inline-block; padding: 0px 20px 0px 0px; vertical-align:top; width = 75px;", numericInput("centerlat", "lat:", value = "35.6", step = 0.25, width = "75px")),
							# div(style = "display: inline;", HTML("<p>preloaded maps are centered on (-75, 35.6)</p>")),
							# width = 3),
							
						# mainPanel(plotOutput("posplot", width = "100%", height = "500px"), tableOutput("postable2"))
					#)
				#)))))

server <- function(input, output) {
	#reactive data functions
	sourcefiles <- reactive({
		behaviorfname <- input$file1
		argosfname <- input$file2
		if(is.null(behaviorfname) & is.null(argosfname)) return(NULL)
		
		behavior <- NULL
		argos <- NULL
		bevtaglabels <- NULL
		argtaglabels <- NULL
		taglabels <- vector()
		
		if(!is.null(behaviorfname)) {
			behavior <- read.table(behaviorfname$datapath, header = TRUE, sep = ',')
			behavior$Start 		<- paste(strptime(behavior$Start, 	format = "%H:%M:%S %d-%b-%Y"), "UTC")
			behavior$End 			<- paste(strptime(behavior$End, 		format = "%H:%M:%S %d-%b-%Y"), "UTC")
			bevtaglabels <- paste(behavior$DeployID,behavior $Ptt, sep = " ")
			taglabels <- c(taglabels, bevtaglabels)
		}
		
		if(!is.null(argosfname)) {
			argos <- read.table(argosfname$datapath, header = TRUE, sep = ',')
			argos$Date      <- paste(strptime(argos$Date, format = "%H:%M:%S %d-%b-%Y"), "UTC")
			argtaglabels <- paste(argos$DeployID, argos$Ptt, sep = " ")
			taglabels <- c(taglabels, argtaglabels)	
		}
		taglabels <- unique(taglabels)
		list(behavior = behavior, argos = argos, taglabels = taglabels, argtaglabels = argtaglabels, bevtaglabels = bevtaglabels)
	})
	
	output$tagselect <- renderUI({
		sources <- sourcefiles()
		if(is.null(sources)) return(NULL)
		selectInput("tagselectinput", "select tag:", sort(sources$taglabels))
	})
		
	selecttag <- reactive({
		selection <- input$tagselectinput
		if(is.null(selection)) return(NULL)
		
		sources <- sourcefiles()
		behavior <- sources$behavior
		argos <- sources$argos
		
		if(!is.null(sources$bevtaglabels)) {
			desebev <- which(sources$bevtaglabels == selection)
			behavior <- behavior[desebev, ]
			if(nrow(behavior) == 0) behavior <- NULL
		}
		if(!is.null(sources$argtaglabels)) {
			desearg <- which(sources$argtaglabels == selection)
			argos <- argos[desearg, ]
			if(nrow(argos) == 0) argos <- NULL
		}
		
		list(behavior = behavior, argos = argos)
	})
	
	argosdat <- reactive({
		tag <- selecttag()
		if(is.null(tag$argos)) return(NULL)
		arg <- tag$argos
		prepargos(arg)})
		
	divedat <- reactive({
		tag <- selecttag()
		if(is.null(tag$behavior)) return(NULL)
		bev <- tag$behavior
		prepbehavior(bev)})
	
	getzoom <- reactive({
		c(input$zoom, input$centerlon, input$centerlat)
	})
	
	baselinesp <- reactive({
		sp <- input$basesp
		out <- "other"
		if(sp == "beakers") {
			out <- "zca"
		} else if(sp == "pdubs") {
			out <- "gma"
		}
		out})
		
	baselinedep <- reactive({
		input$mindep
	})
	
	observeEvent(input$quit, {
		stopApp(0)})
	
	#plot and table output function
	output$gapstatstable  <- renderTable({
		ddat <- divedat()
		if(is.null(ddat)) return(NULL)
		makegapstatstab(ddat)})
		
	output$divedepdur	  <- renderPlot({
		ddat <- divedat()
		if(is.null(ddat)) return(NULL)
		plotdepdur(ddat, baselinesp(), baselinedep())})
		
	output$timebtwndive   <- renderPlot({})
	output$divestatstable <- renderTable({
		ddat <- divedat()
		if(is.null(ddat)) return(NULL)
		plotdepdurtab(ddat)})
	
	output$plot <- renderPlot({
		ddat <- divedat()
		adat <- argosdat()
		if(is.null(ddat)) {
			g_diveplotlive <- FALSE
			return(NULL)
		}
		plotdives(ddat, adat)#, tagon = makeGMT(tagondate(), tagontime()), adat)
		g_diveplotlive <- TRUE})
		
  	output$legend <- renderPlot({
  		plotlegend(isdive = !is.null(divedat()), isargos = !is.null(argosdat()))})
  	
  	output$baseline_mindep <- renderUI({
  		blsp <- baselinesp()
  		if(blsp == "zca") {
  			numericInput("mindep", "min. duration (minutes) for dives (baseline data):", 33, min = 0, max = 1000, step = 1)  		
  		} else if(blsp == "gma") {
  			numericInput("mindep", "min. depth for dives (baseline data):", 75, min = 0, max = 5000, step = 10)  
  		} else {
  			textOutput("nobaseline")
  		}})
  		
  		output$nobaseline <- renderText({"no baseline data"})
  	
  	output$qflags3 <- renderUI({
  		adat <- argosdat()
  		if(is.null(adat)) return(NULL)
  		argnona <- adat[which(!is.na(adat$longitude)), ]
  		qflags <- as.character(argnona$locationquality)
		uqflags <- unique(qflags)
		checkboxGroupInput("qflags_checkboxes3", "qflags:", uqflags, selected = uqflags, inline = TRUE)})

  	
  	output$qflags <- renderUI({
  		adat <- argosdat()
  		if(is.null(adat)) return(NULL)
  		argnona <- adat[which(!is.na(adat$longitude)), ]
  		qflags <- as.character(argnona$locationquality)
		uqflags <- unique(qflags)
		checkboxGroupInput("qflags_checkboxes", "qflags:", uqflags, selected = uqflags, inline = TRUE)})
  	
  	output$qflags2 <- renderUI({
  		adat <- argosdat()
  		if(is.null(adat)) return(NULL)
  		argnona <- adat
  		qflags <- as.character(argnona$locationquality)
		uqflags <- unique(qflags)
		checkboxGroupInput("qflags_checkboxes2", "qflags:", uqflags, selected = uqflags, inline = TRUE)})
  	
  	# output$dateslider <- renderUI({
  		# adat <- argosdat()
  		# if(is.null(adat)) return(NULL)
  		# argona <- adat[which(!is.na(adat$longitude)), ]
  		# times <- as.POSIXct(argona$date, tz = "GMT")
  		# range <- times[c(1, length(times))]
  		# sliderInput("daterange", "date range:", min = min(times), max = max(times), value = range)
  	# })
		
	output$leafmap <- renderLeaflet({
		adat <- argosdat()
		if(is.null(adat)) return(NULL)
		leafplotargos(adat, input$qflags_checkboxes3)
	})
	
	
		
	output$postable <- renderTable({
		adat <- argosdat()
		if(is.null(adat)) return(NULL)
		tableargos(adat, input$qflags_checkboxes3)}, digits = 3)
		
	# output$postable2 <- renderTable({
		# adat <- argosdat()
		# if(is.null(adat)) return(NULL)
		# tableargos(adat, input$qflags_checkboxes)})
	
	# output$posplot <- renderPlot({
		# adat <- argosdat()
		# if(is.null(adat)) return(NULL)
		# plotargos(adat, input$qflags_checkboxes, getzoom())})
		
	output$argosuploaded <- reactive({
		return(!is.null(argosdat()))})
		
	outputOptions(output, 'argosuploaded', suspendWhenHidden=FALSE)

}
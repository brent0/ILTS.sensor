To Install

	Make sure you have the devtools package. Also you may need to install dependancies if error arise.
 
	library(devtools)
	install_github("jae0/netmensuration")
	install_github("brent0/ILTS.sensor") 
	install_github("brent0/ILTS.sensor", INSTALL_opts=c("--no-multiarch"))

To Develop

	In RStudio go to file -> new project -> version controll 
	enter this url https://github.com/brent0/ILTS.sensor.git
	let me know so I can add your git account as contributor or you can fork the code for your own project.

To Avoid entering project variables in the dialog prompts, set the following in your Rprofile.site file

	oracle.snowcrab.server = "ptran"
    oracle.snowcrab.user = "your oracle username"
    oracle.snowcrab.password = "your oracle password"
    bio.datadirectory.ilts = file.path( "C:", "bio.data", "ilts") replace with desired path


To start determining bottom touchdown, or to redo any previously determined sets:

	ilts.format.merge(update = TRUE, user = "brent" ) Set user to whatever you like.

To stop just click the top right x in the plot window at any time

To continue determining bottom touchdown, picking up any uncompleted sets:

	ilts.format.merge(update = FALSE, user = "brent" ) user the same as above whatever you like.

These call will create and add to:

	clicktouchdown_brent.csv AND iltsStats_brent.RDATA
	The clicktouchdown file will contain rows with start and end times by set
	The iltsStats file can be loaded with R and contains a list of detailed stats for each set
	
	
	
ERRORS

	trip 100054290 set 11
	trip 100054290 set 31
	trip L09092019 set 4
	trip L12092019 set 8
	trip L17092019 set 7
	trip L18092019 set `3
	
	100054269 set 16 did plot before did when testing expanded plot
	L04092019 set 4  did plot before did when testing expanded plot
	
	      
	

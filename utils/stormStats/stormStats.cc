// *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
// ** Copyright UCAR (c) 1990 - 2016                                         
// ** University Corporation for Atmospheric Research (UCAR)                 
// ** National Center for Atmospheric Research (NCAR)                        
// ** Boulder, Colorado, USA                                                 
// ** BSD licence applies - redistribution and use in source and binary      
// ** forms, with or without modification, are permitted provided that       
// ** the following conditions are met:                                      
// ** 1) If the software is modified to produce derivative works,            
// ** such modified software should be clearly marked, so as not             
// ** to confuse it with the version available from UCAR.                    
// ** 2) Redistributions of source code must retain the above copyright      
// ** notice, this list of conditions and the following disclaimer.          
// ** 3) Redistributions in binary form must reproduce the above copyright   
// ** notice, this list of conditions and the following disclaimer in the    
// ** documentation and/or other materials provided with the distribution.   
// ** 4) Neither the name of UCAR nor the names of its contributors,         
// ** if any, may be used to endorse or promote products derived from        
// ** this software without specific prior written permission.               
// ** DISCLAIMER: THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS  
// ** OR IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED      
// ** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.    
// *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
///////////////////////////////////////////////////////////////
// stormStats.cc
//
// stormStats object
//
// The One, RAP, NCAR
// P.O.Box 3000, Boulder, CO, 80307-3000, USA
//
// Major changes for revised XML file format (rjp 19 May 2006)
//
// MODIFIED from Storms2Xml2 BY T. RAUPACH, UNIBE, 2018-2019.
//
///////////////////////////////////////////////////////////////
//
// stormStats produces CSV output from TITAN storm files.
//
///////////////////////////////////////////////////////////////

#include "stormStats.hh"
#include "StormFileReader.hh"

#include <cerrno>
#include <vector>
#include <string>

#include <toolsa/file_io.h>
#include <toolsa/pmu.h>
#include <toolsa/DateTime.hh>
#include <toolsa/pjg_flat.h>
#include <dsserver/DsLdataInfo.hh>
#include <titan/DsTitan.hh>
#include <titan/TitanComplexTrack.hh>
#include <titan/TitanSimpleTrack.hh>
#include <titan/TitanTrackEntry.hh>

// Constructor.
stormStats::stormStats(int argc, char **argv) {

  // Status flag.
  _progName = "stormStats";
  ucopyright((char *) _progName.c_str());

  // Get command line arguments.
  if (_args.parse(argc, argv, _progName)) {
    cerr << "ERROR: " << _progName << endl;
    cerr << "Problem with command line args" << endl;
    return;
  }
  
  // Get TDRP params.
  if (_params.loadFromArgs(argc, argv, _args.override.list,
			   &_paramsPath)) {
    cerr << "ERROR: " << _progName << endl;
    cerr << "Problem with TDRP parameters" << endl;
    return;
  }

  // Init process mapper registration.
  PMU_auto_init((char *) _progName.c_str(),
		_params.Instance,
		PROCMAP_REGISTER_INTERVAL);

  // Make sure the output directory exists.
  if (ta_makedir_recurse(_params.outDir)){
    cerr << "Failed to create output directory : ";
    cerr << _params.outDir << " - exiting." << endl;
    exit(-1);
  }
  
  return;
}

// Destructor
stormStats::~stormStats() {}

// Main run function.
int stormStats::Run() {

  // Just loop through and process each input file.
  for(int i = 0; i < _args.inputFiles.size(); i++)
    _run(_args.inputFiles[i]);  
  
  return(0);
}

// Output all storm tracks from an input file.
int stormStats::_run(string infile) {

  if (_params.debug) {
    cerr << "Processing file: " << infile << endl;
  }

  StormFileReader reader;
  reader.setDebug(_params.debug);
  reader.open(infile);

  // Get storm file parameters. 
  const storm_file_params_t &storm_params = reader.stormFile.params();

  // Use the last scan time minus one hour to make the output file name.
  const time_t recTime = reader.stormFile.header().end_time - 3600;
  date_time_t fileTime;
  fileTime.unix_time = recTime;
  uconvert_from_utime(&fileTime);
  char outfile[100];
  sprintf(outfile, "tracks_%04d%02d%02d.csv",       
	  fileTime.year, fileTime.month, fileTime.day);
  
  if (_params.debug) {
    cerr << "  Complex tracks found: ";
    cerr << reader.trackFile.header().n_complex_tracks << endl;
  }
  
  // Loop through complex tracks.
  for(int c = 0; c < reader.trackFile.header().n_complex_tracks; c++) {

    // Get complex track ID.
    int complex_num = reader.trackFile.complex_track_nums()[c];
    
    // Read in complex track properties.
    reader.trackFile.ReadComplexParams(complex_num, true);

    // Get complex track information.
    const complex_track_params_t &ComplexParams = reader.trackFile.complex_params();
   
    // Loop through this complex track's simple tracks.
    for(int s = 0; s < ComplexParams.n_simple_tracks; s++) {

      // Get number of simple tracks.
      int simple_num = reader.trackFile.simples_per_complex()[complex_num][s];

      //      cerr << "Track " << complex_num << "/" << simple_num << endl;
      
      if(reader.trackFile.RewindSimple(simple_num)) {
	cerr << "Error rewinding simple track " << complex_num << "/" << simple_num << endl;
	exit(0);
      }

      // Get the simple track parameters.
      const simple_track_params_t &SimpleParams = reader.trackFile.simple_params();

      // Loop through track entries.
      for(int e = 0; e < SimpleParams.duration_in_scans; e++) {
	// Full list of track properties that can be accessed can be found in
	// lrose-core/codebase/libs/titan/src/include/titan/:
	// -- track_v5.h ({simple|complex}_track params_t)
	// -- storm_v5.h (storm_file_scan_header_t and
	//                storm_file_global_props_t)

	// Read this entry.
	if(reader.trackFile.ReadEntry()) {
	  cerr << "Error reading entry in track " << complex_num <<
	    "/" << simple_num << endl;
	  exit(0);
	}

	// Get entry properties.
	const track_file_entry_t &trackFileEntry = reader.trackFile.entry();

	// Read the scan properties.	
	if(reader.stormFile.ReadScan(trackFileEntry.scan_num,
				     trackFileEntry.storm_num) ||
	   reader.stormFile.ReadProps(trackFileEntry.storm_num)) {
	  cerr << "Error reading scan properties for track " << complex_num <<
	    "/" << simple_num << endl;
	  exit(0);
	}

	// Get scan and global track properties.
	const storm_file_scan_header_t &scan = reader.stormFile.scan();
	const storm_file_global_props_t &Gp = reader.stormFile.gprops()[trackFileEntry.storm_num];

	// Write header if this is the first track.
	if(c == 0 && s == 0 && e == 0)
	  _openOutput(outfile, scan);
	
	// Complex track parameters.
	fprintf(outFile, "%d,", SimpleParams.complex_track_num);  // stormID
	fprintf(outFile, "%d,", ComplexParams.n_simple_tracks);   // numTracks
	fprintf(outFile, "%d,", ComplexParams.duration_in_secs);  // stormDuration
	
	// Simple track parameters.
	fprintf(outFile, "%d,", SimpleParams.simple_track_num);   // trackID
	
	// Get trackEntryTime.  
	const time_t trackEntryTime = trackFileEntry.time;
	date_time_t tracktime;
	tracktime.unix_time = trackEntryTime;
	uconvert_from_utime(&tracktime);
	fprintf(outFile, "%04d-%02d-%02dT%02d:%02d:%02d,",       
		tracktime.year, tracktime.month, tracktime.day,
		tracktime.hour, tracktime.min, tracktime.sec);    // timestamp
	
	// History information. History in seconds is affected by motion smoothing.
	// history_in_secs from SimpleParams is only for the last timestamp in the
	// track.
	fprintf(outFile, "%d,", trackFileEntry.history_in_secs);  // history
	fprintf(outFile, "%d,", trackFileEntry.history_in_scans); // historyInScans	

	// Get originTime.
	date_time_t origintime;
	origintime.unix_time = trackFileEntry.time_origin;
	uconvert_from_utime(&origintime);
	fprintf(outFile, "%04d-%02d-%02dT%02d:%02d:%02d,",       
		origintime.year, origintime.month, origintime.day,
		origintime.hour, origintime.min, origintime.sec); // time_origin
	
	fprintf(outFile, "%d,", SimpleParams.duration_in_secs);   // duration
	fprintf(outFile, "%d,", SimpleParams.nparents);           // nParents
	fprintf(outFile, "%d,", SimpleParams.nchildren);          // nChildren

	// Projection area parameters.
	fprintf(outFile, "%g,", Gp.proj_area_centroid_x);         // xCoord
	fprintf(outFile, "%g,", Gp.proj_area_centroid_y);         // yCoord
	fprintf(outFile, "%g,", Gp.proj_area_major_radius);       // major
	fprintf(outFile, "%g,", Gp.proj_area_minor_radius);       // minor
	fprintf(outFile, "%g,", Gp.proj_area_orientation);        // orientation
	fprintf(outFile, "%g,", Gp.proj_area);                    // projArea

	// Movement parameters.
	fprintf(outFile, "%g,", trackFileEntry.dval_dt.smoothed_speed);     // speed
	fprintf(outFile, "%g,", trackFileEntry.dval_dt.smoothed_direction); // dir

	// Storm cell properties.
	fprintf(outFile, "%g,", Gp.area_mean);                   // meanArea
	fprintf(outFile, "%g,", Gp.volume);                      // volume
	fprintf(outFile, "%g,", Gp.top);                         // top
	fprintf(outFile, "%g,", Gp.dbz_max);                     // maxZ
	fprintf(outFile, "%g,", Gp.dbz_mean);                    // meanZ
	fprintf(outFile, "%g,", Gp.ht_of_dbz_max);               // maxZHeight
	fprintf(outFile, "%g,", Gp.vil_from_maxz);               // VIL
	
	// Hail properties. In TITAN parameters, HAIL_METRICS must be set to NEXRAD_HDA.
	fprintf(outFile, "%g,", Gp.add_on.hda.poh);             // POH
	fprintf(outFile, "%g,", Gp.add_on.hda.shi);             // SHI
	fprintf(outFile, "%g,", Gp.add_on.hda.posh);            // POSH
	fprintf(outFile, "%g,", Gp.add_on.hda.mehs);            // MEHS
	
	// fprintf(outFile, "%d,", Gp.add_on.hail_metrics.FOKRcategory);         // stormCat
	// fprintf(outFile, "%g,", Gp.add_on.hail_metrics.waldvogelProbability); // hailProb
	// fprintf(outFile, "%g,", Gp.add_on.hail_metrics.vihm);                 // vertHailMass
	// fprintf(outFile, "%g,", Gp.add_on.hail_metrics.hailMassAloft);        // hailMassAloft

	// _writeStormRuns(Gp);
	
	// Parent IDs.
	const int *parents = SimpleParams.parent;
	if(SimpleParams.nparents == 0)
	  fprintf(outFile, "NA,");
	else {
	  fprintf(outFile, "\"");
	  for (int i=0; i < SimpleParams.nparents; i++){
	    if(i == SimpleParams.nparents-1)
	      fprintf(outFile, "%d", parents[i]);
	    else
	      fprintf(outFile, "%d ", parents[i]);
	  }
	  fprintf(outFile, "\",");
	}
	
	// Child IDs.
	const int *children = SimpleParams.child;
	if(SimpleParams.nchildren == 0)
	  fprintf(outFile, "NA,");
	else {
	  fprintf(outFile, "\"");
	  for (int i=0; i < SimpleParams.nchildren; i++){
	    if(i == SimpleParams.nchildren-1)
	      fprintf(outFile, "%d", children[i]);
	    else
	      fprintf(outFile, "%d ", children[i]);
	  }
	  fprintf(outFile, "\",");
	}
	
	// Threshold settings.
	fprintf(outFile, "%g,", storm_params.low_dbz_threshold); // lowdBZThreshold
	fprintf(outFile, "%g,", storm_params.min_storm_size);    // minStormSize
	
	// Polygon points.
	// _writePolygonPoints(storm_params, scan, Gp);
	
	// Storm area coordinates.
	_writeStormRuns(reader.stormFile, trackFileEntry.storm_num);
	
	// Finish the line.
	fprintf(outFile, "\n");
	
      } // track entry loop.
    } // simple track loop.
  } // complex track loop.
	
  _closeOutput();
  return(0);
}

// Routine to open the output file and write header.
int stormStats::_openOutput(string outname, const storm_file_scan_header_t &scan) {

  if (_params.debug) {
    cerr << "  Output file: " << outname << endl;
  }
  
  // Make sure the output directory exists.
  if (ta_makedir_recurse(_params.outDir)){
    int errNum = errno;
    cerr << "ERROR - Failed to create output directory : ";
    cerr << _params.outDir << endl;
    cerr << "  " << strerror(errNum) << endl;
    return(-1);
  }

  sprintf(outRelFileName, "%s", outname.c_str());
  sprintf(outAbsFilePath,"%s/%s", _params.outDir, outRelFileName);
  unlink(outAbsFilePath);
  outFile = fopen(outAbsFilePath,"w");

  if (outFile == NULL){
    cerr << "Failed to create " << outAbsFilePath << endl;
    return(-1);
  }

  // Projection information.
  char proj_type[10];
  if(scan.grid.proj_type == 0) {
    sprintf(proj_type, "latlong");
  } else if(scan.grid.proj_type == 8) {
    sprintf(proj_type, "flat");
  } else {
    cerr << "Projection type is not flat or latlong." << endl;
    return(-1);
  }
  
  fprintf(outFile, "TITAN tracking results.\n\n");
  fprintf(outFile, "Projection definition:\n");
  fprintf(outFile, "unitx, unity, unitz, dx, dy, minx, miny, origin_lat, origin_long, proj_type\n");
  fprintf(outFile, "%s,%s,%s,%g,%g,%g,%g,%g,%g,%s\n\n",
	  scan.grid.unitsx, scan.grid.unitsy, scan.grid.unitsz, scan.grid.dx, scan.grid.dy,
	  scan.grid.minx, scan.grid.miny, scan.grid.proj_origin_lat, scan.grid.proj_origin_lon,
	  proj_type);

  // Header information.
  fprintf(outFile, "Header descriptions:\n");
  fprintf(outFile, "stormID:\t\t complex storm ID.\n");
  fprintf(outFile, "numTracks:\t\t number of simple sub-tracks.\n");
  fprintf(outFile, "stormDuration [s]:\t total duration of storm.\n");
  fprintf(outFile, "trackID:\t\t simple/subtrack track number.\n");
  fprintf(outFile, "timestamp:\t\t UTC timestamp.\n");
  fprintf(outFile, "history [s]:\t\t time since earliest storm branch.\n");
  fprintf(outFile, "historyInScans [s]:\t\t scans since earliest storm branch.\n");
  fprintf(outFile, "originTime:\t\t UTC timestamp of earliest detection.\n");
  fprintf(outFile, "duration [s]:\t\t simple track duration.\n");
  fprintf(outFile, "nParents:\t\t number of parent tracks.\n");
  fprintf(outFile, "nChildren:\t\t number of child tracks.\n");
  fprintf(outFile, "x [grid units]:\t\t projected area centroid x-coord.\n");
  fprintf(outFile, "y [grid units]:\t\t projected area centroid y-coord.\n");
  fprintf(outFile, "major [grid units]:\t\t ellipse major axis length.\n");
  fprintf(outFile, "minor [grid units]:\t\t ellipse minor axis length.\n");
  fprintf(outFile, "orientation [deg]:\t ellipse orientation from N.\n");
  fprintf(outFile, "projArea [km2]:\t\t area of projected area.\n");
  fprintf(outFile, "speed [km/h]:\t\t storm movement (smoothed) speed.\n");
  fprintf(outFile, "dir [deg]:\t\t storm movement direction from N.\n");
  fprintf(outFile, "meanArea [km2]:\t\t mean storm area.\n");
  fprintf(outFile, "volume [km3]:\t\t storm volume.\n");
  fprintf(outFile, "top [km]:\t\t storm top height.\n");
  fprintf(outFile, "maxZ [dBZ]:\t\t maximum reflectivity.\n");
  fprintf(outFile, "meanZ [dBZ]:\t\t mean reflectivity.\n");
  fprintf(outFile, "maxHeight [km]:\t\t height of maximum reflectivity.\n");
  fprintf(outFile, "VIL [kg/m2]:\t\t Vertically integrated liquid.\n");
  fprintf(outFile, "POH [perc]:\t\t Probability of hail (Waldvogel POH).\n");
  fprintf(outFile, "SHI [J/(ms)]:\t\t Severe hail index (SHI).\n");
  fprintf(outFile, "POSH [perc]:\t\t Probability of severe hail (POSH).\n");
  fprintf(outFile, "MEHS [mm]:\t\t Maximum estimated hail size (MEHS).\n");
  fprintf(outFile, "parents:\t\t list of parent track IDs (space separated).\n");
  fprintf(outFile, "children:\t\t list of child track IDs (space separated).\n");
  fprintf(outFile, "lowdBZThreshold [dBZ]:\t low dBZ threshold used.\n");
  fprintf(outFile, "minStormSize [km3]:\t minimum storm size threshold used.\n");
  fprintf(outFile, "numLines:\t\t number of (y) lines in the cell.\n");
  fprintf(outFile, "stormRuns:\t storm outline runs in grid coords (x,y,n).\n");

  // Header line:
  fprintf(outFile, "stormID,numTracks,stormDuration,trackID,timestamp,");
  fprintf(outFile, "history,historyInScans,originTime,duration,nParents,nChildren,");
  fprintf(outFile, "xCoord,yCoord,major,minor,orientation,projArea,speed,dir,meanArea,");
  fprintf(outFile, "volume,top,maxZ,meanZ,maxHeight,VIL,POH,SHI,POSH,MEHS,");
  fprintf(outFile, "parents,children,lowdBZThreshold,minStormSize,numLines,stormRuns\n");
  
  return(0);
}

// Close the output file.
void stormStats::_closeOutput(){
  fclose(outFile);
  DsLdataInfo ldata(_params.outDir);
  ldata.setWriter("stormStats");
  ldata.setDataFileExt("csv");
  ldata.setDataType("csv");
  ldata.setRelDataPath(outRelFileName);
  ldata.write(outTime);
}

void stormStats::_writePolygonPoints(const storm_file_params_t &storm_params,
				     const storm_file_scan_header_t &scan,
				     const storm_file_global_props_t &props) {
  // For a given storm, write its polygon outline lat and lon
  // coordinates to the output file.

  double range;
  double xPoint;
  double yPoint;
  
  // Polygon information, taken from draw_titan.cc in Rview.
  // deg. azimuth delta between polygon points (pos = counterclockwise).
  double poly_delta_az = storm_params.poly_delta_az * DEG_TO_RAD;

  // deg. azimuth from T.N. (north?) for first polygon points.
  double theta = storm_params.poly_start_az * DEG_TO_RAD;
  int n_sides = storm_params.n_poly_sides;
  double dx = scan.grid.dx;
  double dy = scan.grid.dy;

  // Debug info:
  // sides << "Storm file parameters: polygon delta az: " << poly_delta_az << ", ";
  // cerr << "theta: " << theta << ", n_poly: " << n_sides << endl;
  // cerr << "Storm scan parameters: dx: " << dx << ", dy: " << dy << endl;

  // X points.
  fprintf(outFile, "\"");
  for(int side = 0; side < n_sides; side++) {
    // proj_area_polygon[n_sides] gives the length of each ray from the
    // centroid to the polygon vertices, in numbers of grid points.
    range = props.proj_area_polygon[side];
    xPoint = props.proj_area_centroid_x + (range * sin(theta) * dx);

    fprintf(outFile, "%g", xPoint);
    if(side != n_sides-1)
      fprintf(outFile, " ");
	    
    theta += poly_delta_az;
  }
  fprintf(outFile, "\",");

  // Y points.
  theta = storm_params.poly_start_az * DEG_TO_RAD;
  fprintf(outFile, "\"");
  for(int side = 0; side < n_sides; side++) {
    // proj_area_polygon[n_sides] gives the length of each ray from the
    // centroid to the polygon vertices, in numbers of grid points.
    range = props.proj_area_polygon[side];
    yPoint = props.proj_area_centroid_y + (range * cos(theta) * dy);

    fprintf(outFile, "%g", yPoint);
    if(side != n_sides-1)
      fprintf(outFile, " ");

    theta += poly_delta_az;
  }
  fprintf(outFile, "\"");
}

void stormStats::_writeStormRuns(TitanStormFile &stormFile, int storm_number) {
  // Read storm run information for a certain storm number and write the coordinates of the
  // storm to file.

  // Read the run (storm coordinate) information.
  if(stormFile.ReadProjRuns(storm_number))
    cerr << "Error reading run information for storm number" <<
      storm_number << endl;

  const storm_file_global_props_t &Gp = stormFile.gprops()[storm_number];

  // Write out number of lines/runs in this cell.
  fprintf(outFile, "%d,", Gp.n_proj_runs);
  
  // Loop through all runs for this storm. Output space-separated triplets of x,y,n where x and y
  // give a grid coordinate and n is the number of pixels in the run in the positive x direction,
  // from that point.
  fprintf(outFile, "\"");
  for(int i=0; i < Gp.n_proj_runs; i++) {
    const storm_file_run_t run = stormFile.proj_runs()[i];    

    if(run.iz != 0) {
      printf("Expected run.iz == 0.");
      exit(0);
    }

    // This code converts to real coordinates from grid-coordinates. No longer used.
    // double xCoord = stormFile.scan().grid.minx + (stormFile.scan().grid.dx * (double) run.ix);
    // double yCoord = stormFile.scan().grid.miny + (stormFile.scan().grid.dy * (double) run.iy); 
    // double distX = stormFile.scan().grid.dx * (double) run.n; 

    fprintf(outFile, "%d,%d,%d", run.ix, run.iy, run.n);

    if(i != Gp.n_proj_runs-1)
      fprintf(outFile, " ");
  }
  fprintf(outFile, "\"");
}

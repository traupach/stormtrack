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
//
// Nc2Mdv object
//
// Mike Dixon, RAP, NCAR, P.O.Box 3000, Boulder, CO, 80307-3000, USA
//
// Oct 2008
//
// MODIFIED from NcGeneric2Mdv BY T. RAUPACH, UNIBE, 2018-2019.
//
///////////////////////////////////////////////////////////////
//
// Nc2Mdv reads data in NetCDF format, and converts to MDV.
//
////////////////////////////////////////////////////////////////

#include <toolsa/toolsa_macros.h>
#include <toolsa/umisc.h>
#include <toolsa/pmu.h>
#include <toolsa/Path.hh>
#include <Mdv/MdvxField.hh>
#include <dsserver/DsLdataInfo.hh>
#include "Nc2Mdv.hh"
using namespace std;

const fl32 Nc2Mdv::_missingFloat = -9999.0;

// Constructor.
Nc2Mdv::Nc2Mdv(int argc, char **argv) {
  isOK = true;
  _input = NULL;

  // Set program name.
  _progName = "Nc2Mdv";
  ucopyright((char *) _progName.c_str());

  // Get command line args.
  if (_args.parse(argc, argv, _progName)) {
    cerr << "ERROR: " << _progName << endl;
    cerr << "Problem with command line args" << endl;
    isOK = false;
    return;
  }

  // Get TDRP params.
  _paramsPath = (char *) "unknown";
  if (_params.loadFromArgs(argc, argv, _args.override.list,
			   &_paramsPath)) {
    cerr << "ERROR: " << _progName << endl;
    cerr << "Problem with TDRP parameters" << endl;
    isOK = false;
  }

  // Check that the file list is set.
  if (_args.inputFileList.size() == 0) {
    cerr << "ERROR: Nc2Mdv" << endl;
    cerr << "  You must use -f to specify files on the command line."
	 << endl;
    _args.usage(_progName, cerr);
    isOK = false;
  }

  // Init process mapper registration.
  PMU_auto_init((char *) _progName.c_str(),
		_params.instance,
		_params.reg_interval);

  // Initialize the data input object.
  if (_params.debug) {
    for (int ii = 0; ii < (int) _args.inputFileList.size(); ii++) {
      cerr << "Adding path: " << _args.inputFileList[ii] << endl;
    }
  }

  _input = new DsInputPath(_progName,
			   _params.debug >= Params::DEBUG_VERBOSE,
			   _args.inputFileList);
}

// Destructor.
Nc2Mdv::~Nc2Mdv() {
  PMU_auto_unregister();
  if (_input) {
    delete _input;
  }
}

// Run.
int Nc2Mdv::Run() {
  int iret = 0;
  PMU_auto_register("Run");
  
  // Loop until end of data.
  char *inputPath;
  while((inputPath = _input->next()) != NULL) {
    PMU_auto_register("Reading file");
    ta_file_uncompress(inputPath);
    if (_processFile(inputPath)) {
      cerr << "ERROR = Nc2Mdv::Run" << endl;
      cerr << "  Processing file: " << inputPath << endl;
      iret = -1;
    }
  }
  
  return iret;
}

// Process a single file.
int Nc2Mdv::_processFile(const char *input_path) {
  PMU_auto_register("Processing file");

  if (_params.debug) {
    cerr << "Processing file: " << input_path << endl;
  }

  // Open the file.
  if (_openNc3File(input_path)) {
    cerr << "ERROR - Nc2Mdv::_processFile" << endl;
    cerr << "  File path: " << input_path << endl;
    return -1;
  }

  // Print out NetCDF info for debugging.
  if (_params.debug >= Params::DEBUG_EXTRA)
    _printFile(*_ncFile);

  // Check that this is a valid file.
  if (_loadMetaData()) {
    cerr << "ERROR - Nc2Mdv::_processFile" << endl;
    cerr << "  File has invalid data" << endl;
    cerr << "  File: " << input_path << endl;
    return -1;
  }

  if(_params.debug >= Params::DEBUG_EXTRA)
    cerr << "There are " << _nTimes << " times to loop through." << endl;
  
  // Loop through times.
  for(int itime = 0; itime < _nTimes; itime++) {
    if(_params.debug >= Params::DEBUG_EXTRA) 
      cerr << "Processing time " << itime+1 << "..." << endl;
    
    // Create output Mdvx file object.
    DsMdvx mdvx;
    if(_params.debug >= Params::DEBUG_VERBOSE)
      mdvx.setDebug(true);
    
    // Set master header.
    if(_setMasterHeader(mdvx, itime))
      return -1;
    
    // Add the data fields.
    if (_addDataFields(mdvx, itime))
      return -1;
    
    // Write output file.
    if(_params.debug) 
      cerr << "Writing file to url: " << _params.output_url << endl;

    if(mdvx.writeToDir(_params.output_url)) {
      cerr << "ERROR - Nc2Mdv" << endl;
      cerr << "  Cannot write file to url: " << _params.output_url << endl;
      cerr << mdvx.getErrStr() << endl;
      return -1;
    }
    
    if(_params.debug)
      cerr << "Wrote output file: " << mdvx.getPathInUse() << endl;
  } 

  return 0;
}

// Initialize the input projection.
void Nc2Mdv::_initInputProjection() {
  double midLon = _minx + _nx * _dx / 2.0;
  _inputProj.initLatlon(midLon);
  return;
}

// Open netcdf file. Returns 0 on success, -1 on failure.
int Nc2Mdv::_openNc3File(const string &path) {
  if(_ncFile) {
    _ncFile->close();
    delete _ncFile;
  }

  _ncFile = new Nc3File(path.c_str(), Nc3File::ReadOnly);

  // Check that constructor succeeded.
  if(!_ncFile->is_valid()) {
    cerr << "ERROR - Nc2Mdv::_openNc3File" << endl;
    cerr << "  Opening file, path: " << path << endl;
    return 1;
  }
  
  // Change the error behavior of the netCDF C++ API by creating an Nc3Error object. Until it is
  // destroyed, this Nc3Error object will ensure that the netCDF C++ API silently returns error
  // codes on any failure, and leaves any other error handling to the calling program.
  _ncErr = new Nc3Error(Nc3Error::silent_nonfatal);

  if (_params.debug) {
    cerr << "Opened input file: " << path << endl;
  }
  
  return 0;
}

// Close netcdf file if open. Remove error object if it exists.
void Nc2Mdv::_closeNc3File() {
  if (_ncFile) {
    _ncFile->close();
    delete _ncFile;
    _ncFile = NULL;
  }

  if (_ncErr) {
    delete _ncErr;
    _ncErr = NULL;
  }
}

// Check that this is a valid file. Returns 0 on success, -1 on failure.
int Nc2Mdv::_loadMetaData() {
  // Check dimensions.
  _timeDim = _ncFile->get_dim(_params.netcdf_dim_time);
  if (_timeDim == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  time dimension missing: " << _params.netcdf_dim_time << endl;
    return -1;
  }
  _nTimes = _timeDim->size();

  if (strcmp(_params.netcdf_dim_z, "none") == 0) {
    _zDim = NULL;
    _nz = 1;
  } else {
    _zDim = _ncFile->get_dim(_params.netcdf_dim_z);
    if (_zDim == NULL) {
      cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
      cerr << "  Z dimension missing: " << _params.netcdf_dim_z << endl;
      return -1;
    }
    _nz = _zDim->size();
  }
  
  _yDim = _ncFile->get_dim(_params.netcdf_dim_y);
  if (_yDim == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  Y dimension missing: " << _params.netcdf_dim_y << endl;
    return -1;
  }
  _ny = _yDim->size();
  
  _xDim = _ncFile->get_dim(_params.netcdf_dim_x);
  if (_xDim == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  Z dimension missing: " << _params.netcdf_dim_x << endl;
    return -1;
  }
  _nx = _xDim->size();

  // variables
  
  if (strcmp(_params.netcdf_var_base_time, "none") == 0) {
    _baseTimeVar = NULL;
  } else {
    _baseTimeVar = _ncFile->get_var(_params.netcdf_var_base_time);
    if (_baseTimeVar == NULL) {
      cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
      cerr << "  base time var variable missing" << _params.netcdf_var_base_time << endl;
      return -1;
    }
  }
  
  _timeOffsetVar = _ncFile->get_var(_params.netcdf_var_time_offset);
  if (_timeOffsetVar == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  time offset variable missing" << _params.netcdf_var_time_offset << endl;
    return -1;
  }

  if (_zDim == NULL) {
    _zVar = NULL;
  } else {
    _zVar = _ncFile->get_var(_params.netcdf_var_z);
    if (_zVar == NULL) {
      cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
      cerr << "  z variable missing" << _params.netcdf_var_z << endl;
    return -1;
    }
  }

  _yVar = _ncFile->get_var(_params.netcdf_var_y);
  if (_yVar == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  y variable missing" << _params.netcdf_var_y << endl;
    return -1;
  }

  _xVar = _ncFile->get_var(_params.netcdf_var_x);
  if (_xVar == NULL) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  x variable missing" << _params.netcdf_var_x << endl;
    return -1;
  }

  // Z coord values

  cerr << "_nz is " << _nz << endl;
  _zArray = (float *) _zArray_.alloc(_nz);
  if (_zVar == NULL) {
    _zArray[0] = 0.0;
  } else {
    if (_zVar->get(_zArray, _nz) == 0) {
      cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
      cerr << "  Cannot get z coords from var: " << _params.netcdf_var_z << endl;
      return -1;
    }
  
    // convert to km
    double zScale = 1.0;
    Nc3Att *zUnits = _zVar->get_att("units");
    if (zUnits != NULL) {
      string units = zUnits->as_string(0);
      if (units == "m" || units == "meters") {
        zScale = 0.001;
      }
      delete zUnits;
    }
    if (zScale != 1.0) {
      for (int ii = 0; ii < _zDim->size(); ii++) {
        _zArray[ii] *= zScale;
      }
    }
  }

  // Y coord values
  _yArray = (float *) _yArray_.alloc(_yDim->size());
  if (_yVar->get(_yArray, _yDim->size()) == 0) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  Cannot get y coords from var: " << _params.netcdf_var_y << endl;
    return -1;
  }

  // convert to km

  double yScale = 1.0;
  Nc3Att *yUnits = _yVar->get_att("units");
  if (yUnits != NULL) {
    string units = yUnits->as_string(0);
    if (units == "m" || units == "meters") {
      yScale = 0.001;
    }
    delete yUnits;
  }
  if (yScale != 1.0) {
    for (int ii = 0; ii < _yDim->size(); ii++) {
      _yArray[ii] *= yScale;
    }
  }

  // X coord values

  _xArray = (float *) _xArray_.alloc(_xDim->size());
  if (_xVar->get(_xArray, _xDim->size()) == 0) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  Cannot get x coords from var: " << _params.netcdf_var_x << endl;
    return -1;
  }

  // convert to km

  double xScale = 1.0;
  Nc3Att *xUnits = _xVar->get_att("units");
  if (xUnits != NULL) {
    string units = xUnits->as_string(0);
    if (units == "m" || units == "meters") {
      xScale = 0.001;
    }
    delete xUnits;
  }
  if (xScale != 1.0) {
    for (int ii = 0; ii < _xDim->size(); ii++) {
      _xArray[ii] *= xScale;
    }
  }
  
  // attributes
  
  Nc3Att *source = _ncFile->get_att("source");
  if (source != NULL) {
    _source = source->as_string(0);
    delete source;
  } else {
    _source = _params.data_set_source;
  }

  // data set info

  Nc3Att *history = _ncFile->get_att("history");
  if (history != NULL) {
    _history = history->as_string(0);
    delete history;
  } else {
    _history = _params.data_set_info;
  }

  // reverse the Y array?

  _yIsReversed = false;
  if (_ny >= 2) {
    int midNy = _ny / 2;
    double dyOrig = (_yArray[midNy+1] - _yArray[midNy]);
    if (dyOrig < 0) {
      _yIsReversed = true;
    }
  }

  if (_yIsReversed) {
    TaArray<float> tmpArray_;
    float *tmpArray = tmpArray_.alloc(_ny);
    memcpy(tmpArray, _yArray, _ny * sizeof(float));
    for (int ii = 0, jj = _ny - 1; ii < _ny; ii++, jj--) {
      _yArray[ii] = tmpArray[jj];
    }
  }

  // set up geometry, for now assuming constant dx and dy

  _minz = _zArray[0];
  _miny = _yArray[0];
  _minx = _xArray[0];

  _maxz = _zArray[_nz-1];
  _maxy = _yArray[_ny-1];
  _maxx = _xArray[_nx-1];

  if (_nz < 2) {
    _dz = 1.0;
  } else {
    _dz = (_zArray[_nz - 1] - _zArray[0]) / (_nz - 1.0);
  }
  
  if (_ny < 2) {
    _dy = 1.0;
  } else {
    _dy = (_yArray[_ny - 1] - _yArray[0]) / (_ny - 1.0);
  }
  
  if (_nx < 2) {
    _dx = 1.0;
  } else {
    _dx = (_xArray[_nx - 1] - _xArray[0]) / (_nx - 1.0);
  }

  _nxValid = _nx;
  _nyValid = _ny;
  _ixValidStart = 0;
  _ixValidEnd = _nx - 1;
  _iyValidStart = 0;
  _iyValidEnd = _ny - 1;

  // check we have a constant deltax and y

  _dxIsConstant = _checkDxIsConstant();
  _dyIsConstant = _checkDyIsConstant();
  
  // Assume latlon coords, find the valid end indices for the lat/lon coords.
  if (_findValidLatLonLimits()) {
    cerr << "ERROR - Nc2Mdv::_loadMetaData" << endl;
    cerr << "  Bad lat/lon values, cannot process" << endl;
    return -1;
  }
  
  if (_params.debug >= Params::DEBUG_VERBOSE) {
    cerr << "Input geometry:" << endl;
    cerr << "  nz: " << _nz << endl;
    cerr << "  ny: " << _ny << endl;
    cerr << "  nx: " << _nx << endl;
    cerr << "  minz: " << _minz << endl;
    cerr << "  miny: " << _miny << endl;
    cerr << "  minx: " << _minx << endl;
    cerr << "  dz: " << _dz << endl;
    cerr << "  dy: " << _dy << endl;
    cerr << "  yIsReversed: " << _yIsReversed << endl;
    cerr << "  dx: " << _dx << endl;
    cerr << "  dxIsConstant: " << (_dxIsConstant?"Y":"N") << endl;
    cerr << "  dyIsConstant: " << (_dyIsConstant?"Y":"N") << endl;
  }
  
  _initInputProjection();

  return 0;
}

// Set the master header from the NCF file. 
// Returns 0 on success, -1 on failure
int Nc2Mdv::_setMasterHeader(DsMdvx &mdvx, int itime) {
  mdvx.clearMasterHeader();
  
  // Process time.
  time_t baseTimeUtc = 0;
  if (_baseTimeVar) {
    baseTimeUtc = _baseTimeVar->as_int(0);
  } else {
    DateTime btime(_params.base_time_string);
    baseTimeUtc = btime.utime();
  }

  bool offsetIsDays = false;
  Nc3Att *timeUnits = _timeOffsetVar->get_att("units");
  if (timeUnits != NULL) {
    string units = timeUnits->as_string(0);
    if (units.find("day") != string::npos) {
      offsetIsDays = true;
    }
    delete timeUnits;
  }
  
  int timeOffsetSecs = 0;
  if (offsetIsDays) {
    double timeOffsetDays = _timeOffsetVar->as_double(itime);
    timeOffsetSecs = (int) (timeOffsetDays * 86400.0 + 0.5);
  } else {
    timeOffsetSecs = _timeOffsetVar->as_int(itime);
  }
  _validTime = baseTimeUtc + timeOffsetSecs;
  mdvx.setValidTime(_validTime);

  if (_params.debug) {
    cerr << "  Found data set at time: " << DateTime::strm(_validTime) << endl;
  }
  
  // Set metadata.
  mdvx.setDataCollectionType(Mdvx::DATA_MEASURED);
  mdvx.setDataSetName(_params.data_set_name);
  mdvx.setDataSetSource(_source.c_str());
  mdvx.setDataSetInfo(_history.c_str());

  return 0;
}

// Add the data fields to the MDV file.
// Returns 0 on success, -1 on failure.
int Nc2Mdv::_addDataFields(DsMdvx &mdvx, int itime) {
  for (int ivar = 0; ivar < _ncFile->num_vars(); ivar++) {
    Nc3Var *var = _ncFile->get_var(ivar);

    if(_params.debug >= Params::DEBUG_EXTRA)
      cerr << "  Looking at variable with name: " << var->name() << endl;
    
    if (var->get_dim(0) != _timeDim) {
      if(_params.debug >= Params::DEBUG_EXTRA)
	cerr << "  Zeroth dimension is not " << _params.netcdf_dim_time << ", skipping!" << endl;
      continue;
    }

    if (_zDim) {
      if (var->num_dims() < 4) {
        continue;
      }
      if (var->get_dim(1) != _zDim) {
        continue;
      }
      if (var->get_dim(2) != _yDim) {
        continue;
      }
      if (var->get_dim(3) != _xDim) {
        continue;
      }
    } else {
      if (var->num_dims() < 3) {
        continue;
      }
      if (var->get_dim(1) != _yDim) {
        continue;
      }
      if (var->get_dim(2) != _xDim) {
        continue;
      }
    }

    if (_params.debug) {
      cerr << "  Found valid variable:" << endl;
      cerr << "    Name: " << var->name() << endl;
      cerr << "    Is valid: " << var->is_valid() << endl;
      cerr << "    N dims: " << var->num_dims() << endl;
    }

    _addDataField(var, mdvx, itime);
  } 
  
  return 0;
}


// Add the data field.
// Returns 0 on success, -1 on failure
int Nc2Mdv::_addDataField(Nc3Var *var, DsMdvx &mdvx, int itime) {

  Nc3Att *missingAtt = var->get_att("missing_value");
  if (missingAtt == NULL) {
    missingAtt = var->get_att("_FillValue");
    if (missingAtt == NULL) {
      cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
      cerr << "  Cannot find missing_value of _FillValue attribute" << endl;
      cerr << "  field name: " << var->name() << endl;
      return -1;
    }
  }

  // Set npoints, allocate float values array.
  int npts = _nz * _ny * _nx;

  TaArray<float> vals_;
  float *vals = vals_.alloc(npts);

  // set current location based on time

  if (_zDim) {
    var->set_cur(itime, 0, 0, 0);
  } else {
    var->set_cur(itime, 0, 0);
  }

  if (var->type() == NC_FLOAT) {

    TaArray<float> fvals_;
    float *fvals = fvals_.alloc(npts);
    
    // read data

    int iret = 0;
    if (_zDim) {
      iret = var->get(fvals, 1, _nz, _ny, _nx);
    } else {
      iret = var->get(fvals, 1, _ny, _nx);
    }

    if (iret == 0) {
      cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
      cerr << "  Cannot get data from input netcdf variable" << endl;
      cerr << "  field name: " << var->name() << endl;
      cerr << _ncErr->get_errmsg() << endl;
      return -1;
    }

    // save data
    
    float missing = missingAtt->as_float(0);
    for (int ii = 0; ii < npts; ii++) {
      if (fvals[ii] == missing) {
        vals[ii] = _missingFloat;
      } else {
        vals[ii] = fvals[ii];
      }
    }

  } else if (var->type() == NC_DOUBLE) {
    
    TaArray<double> dvals_;
    double *dvals = dvals_.alloc(npts);

    // read data

    int iret = 0;
    if (_zDim) {
      iret = var->get(dvals, 1, _nz, _ny, _nx);
    } else {
      iret = var->get(dvals, 1, _ny, _nx);
    }
    if (iret == 0) {
      cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
      cerr << "  Cannot get data from input netcdf variable" << endl;
      cerr << "  field name: " << var->name() << endl;
      cerr << _ncErr->get_errmsg() << endl;
      return -1;
    }

    // save data
    
    double missing = missingAtt->as_double(0);
    for (int ii = 0; ii < npts; ii++) {
      if (dvals[ii] == missing) {
        vals[ii] = _missingFloat;
      } else {
        vals[ii] = dvals[ii];
      }
    }

  } else {

    // for int fields, we need scale and offset

    double scale = 1.0;
    Nc3Att *scaleAtt = var->get_att("scale");
    if (scaleAtt == NULL) {
      scaleAtt = var->get_att("scale_factor");
    }
    if (scaleAtt == NULL) {
      cerr << "WARNING - Nc2Mdv::_addDataField" << endl;
      cerr << "  Cannot get scale for integer variable" << endl;
      cerr << "  field name: " << var->name() << endl;
      cerr << "  Setting scale to 1.0" << endl;
    } else {
      scale = scaleAtt->as_double(0);
      delete scaleAtt;
    }
      
    double offset = 0.0;
    Nc3Att *offsetAtt = var->get_att("offset");
    if (offsetAtt == NULL) {
      if (_params.debug) {
        cerr << "WARNING - Nc2Mdv::_addDataField" << endl;
        cerr << "  Cannot get offset for integer variable" << endl;
        cerr << "  field name: " << var->name() << endl;
        cerr << "  setting to 0" << endl;
      }
    } else {
      offset = offsetAtt->as_double(0);
      delete offsetAtt;
    }
    
    if (var->type() == NC_INT) {
      
      TaArray<int> ivals_;
      int *ivals = ivals_.alloc(npts);
      
      // read data
      
      int iret = 0;
      if (_zDim) {
        iret = var->get(ivals, 1, _nz, _ny, _nx);
      } else {
        iret = var->get(ivals, 1, _ny, _nx);
      }
      if (iret == 0) {
        cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
        cerr << "  Cannot get data from input netcdf variable" << endl;
        cerr << "  field name: " << var->name() << endl;
        cerr << _ncErr->get_errmsg() << endl;
        return -1;
      }

      // save data

      int missing = missingAtt->as_int(0);
      for (int ii = 0; ii < npts; ii++) {
        if (ivals[ii] == missing) {
          vals[ii] = _missingFloat;
        } else {
          vals[ii] = ivals[ii] * scale + offset;
        }
      }

    } else if (var->type() == NC_SHORT) {
      
      TaArray<short> svals_;
      short *svals = svals_.alloc(npts);
      
      // read data
      
      int iret = 0;
      if (_zDim) {
        iret = var->get(svals, 1, _nz, _ny, _nx);
      } else {
        iret = var->get(svals, 1, _ny, _nx);
      }
      if (iret == 0) {
        cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
        cerr << "  Cannot get data from input netcdf variable" << endl;
        cerr << "  field name: " << var->name() << endl;
        cerr << _ncErr->get_errmsg() << endl;
        return -1;
      }

      // save data

      short missing = missingAtt->as_short(0);
      for (int ii = 0; ii < npts; ii++) {
        if (svals[ii] == missing) {
          vals[ii] = _missingFloat;
        } else {
          vals[ii] = svals[ii] * scale + offset;
        }
      }

    } else if (var->type() == NC_BYTE) {
      

      TaArray<ncbyte> bvals_;
      ncbyte *bvals = bvals_.alloc(npts);
      TaArray<unsigned char> uvals_;
      unsigned char *uvals = uvals_.alloc(npts);
      
      // read data
      
      int iret = 0;
      if (_zDim) {
        iret = var->get(bvals, 1, _nz, _ny, _nx);
      } else {
        iret = var->get(bvals, 1, _ny, _nx);
      }
      if (iret == 0) {
        cerr << "ERROR - Nc2Mdv::_addDataField" << endl;
        cerr << "  Cannot get data from input netcdf variable" << endl;
        cerr << "  field name: " << var->name() << endl;
        cerr << _ncErr->get_errmsg() << endl;
        return -1;
      }
      memcpy(uvals, bvals, npts);

      // save data
      
      ncbyte missing = missingAtt->as_ncbyte(0);
      for (int ii = 0; ii < npts; ii++) {
        if (bvals[ii] == missing) {
          vals[ii] = _missingFloat;
        } else {
          if (_params.treat_ncbyte_as_unsigned) {
            vals[ii] = (int) uvals[ii] * scale + offset;
          } else {
            vals[ii] = (int) bvals[ii] * scale + offset;
          }
        }
      }

    } // if (var->type() == NC_INT)

  } // if (var->type() == NC_FLOAT)

  // free up attribute

  delete missingAtt;

  // reverse y order if it was in reverse order in the file

  if (_yIsReversed) {

    TaArray<float> tmpVals_;
    float *tmpVals = tmpVals_.alloc(npts);
    memcpy(tmpVals, vals, npts * sizeof(float));

    int nptsPlane = _ny * _nx;

    for (int iz = 0; iz < _nz; iz ++) {
      int planeOffset = iz * nptsPlane;
      for (int iy = 0; iy < _ny; iy ++) {
        float *src = tmpVals + planeOffset + _nx * (_ny - 1 - iy);
        float *dest = vals + planeOffset + _nx * iy;
        memcpy(dest, src, _nx * sizeof(float));
      } // iy
    } // iz
    
  }
  
  // get field name and units

  string fieldName(var->name());

  string units;
  Nc3Att *unitsAtt = var->get_att("units");
  if (unitsAtt != NULL) {
    units = unitsAtt->as_string(0);
    delete unitsAtt;
  }

  string longName(fieldName);
  Nc3Att *longNameAtt = var->get_att("long_name");
  if (longNameAtt != NULL) {
    longName = longNameAtt->as_string(0);
    delete longNameAtt;
  }

  // create the field from the netcdf array
  MdvxField *field = _createMdvxField(fieldName, longName, units,
				      _nx, _ny, _nz,
				      _minx, _miny, _minz,
				      _dx, _dy, _dz,
				      vals);
  // add to Mdvx, which takes over ownership
  mdvx.addField(field);
  return 0;
}

// Create an Mdvx field
MdvxField *Nc2Mdv::_createMdvxField(const string &fieldName,
				    const string &longName,
				    const string &units,
				    int nx, int ny, int nz,
				    double minx, double miny, double minz,
				    double dx, double dy, double dz,
				    const float *vals) {
  // Check max levels.

  if (nz > MDV_MAX_VLEVELS) {
    nz = MDV_MAX_VLEVELS;
  }

  // set up MdvxField headers
  Mdvx::field_header_t fhdr;
  MEM_zero(fhdr);
  
  _inputProj.syncToFieldHdr(fhdr);

  fhdr.compression_type = Mdvx::COMPRESSION_NONE;
  fhdr.transform_type = Mdvx::DATA_TRANSFORM_NONE;
  fhdr.scaling_type = Mdvx::SCALING_NONE;
  
  fhdr.native_vlevel_type = Mdvx::VERT_TYPE_Z;
  fhdr.vlevel_type = Mdvx::VERT_TYPE_Z;
  fhdr.dz_constant = false;
  fhdr.data_dimension = 3;

  fhdr.bad_data_value = _missingFloat;
  fhdr.missing_data_value = _missingFloat;
  
  fhdr.encoding_type = Mdvx::ENCODING_FLOAT32;
  fhdr.data_element_nbytes = sizeof(fl32);
  fhdr.volume_size = nx * ny * nz * sizeof(fl32);
  
  fhdr.nx = nx;
  fhdr.ny = ny;
  fhdr.nz = nz;

  fhdr.grid_minx = minx;
  fhdr.grid_miny = miny;
  fhdr.grid_minz = minz;

  fhdr.grid_dx = dx;
  fhdr.grid_dy = dy;
  fhdr.grid_dz = dz;
  
  Mdvx::vlevel_header_t vhdr;
  MEM_zero(vhdr);
  
  for (int ii = 0; ii < nz; ii++) {
    vhdr.type[ii] = Mdvx::VERT_TYPE_Z;
    vhdr.level[ii] = _zArray[ii];
  }

  // create MdvxField object
  // converting data to encoding and compression types

  MdvxField *field = new MdvxField(fhdr, vhdr, vals);
  field->convertType
    ((Mdvx::encoding_type_t) _params.output_encoding_type,
     (Mdvx::compression_type_t) _params.output_compression_type);

  // set names etc
  
  field->setFieldName(fieldName.c_str());
  field->setFieldNameLong(longName.c_str());
  field->setUnits(units.c_str());
  field->setTransform("");

  return field;

}
  
// Print data in file.
void Nc2Mdv::_printFile(Nc3File &ncf) {
  cout << "ndims: " << ncf.num_dims() << endl;
  cout << "nvars: " << ncf.num_vars() << endl;
  cout << "ngatts: " << ncf.num_atts() << endl;
  Nc3Dim *unlimd = ncf.rec_dim();
  if (unlimd != NULL) {
    cout << "unlimd: " << unlimd->size() << endl;
  }
  
  // dimensions

  TaArray<Nc3Dim *> dims_;
  Nc3Dim **dims = dims_.alloc(ncf.num_dims());
  for (int idim = 0; idim < ncf.num_dims(); idim++) {
    dims[idim] = ncf.get_dim(idim);

    cout << endl;
    cout << "Dim #: " << idim << endl;
    cout << "  Name: " << dims[idim]->name() << endl;
    cout << "  Length: " << dims[idim]->size() << endl;
    cout << "  Is valid: " << dims[idim]->is_valid() << endl;
    cout << "  Is unlimited: " << dims[idim]->is_unlimited() << endl;
    
  } // idim
  
  cout << endl;

  // global attributes

  cout << "Global attributes:" << endl;

  for (int iatt = 0; iatt < ncf.num_atts(); iatt++) {
    cout << "  Att num: " << iatt << endl;
    Nc3Att *att = ncf.get_att(iatt);
    _printAtt(att);
    delete att;
  }

  // loop through variables

  TaArray<Nc3Var *> vars_;
  Nc3Var **vars = vars_.alloc(ncf.num_vars());
  for (int ivar = 0; ivar < ncf.num_vars(); ivar++) {

    vars[ivar] = ncf.get_var(ivar);
    cout << endl;
    cout << "Var #: " << ivar << endl;
    cout << "  Name: " << vars[ivar]->name() << endl;
    cout << "  Is valid: " << vars[ivar]->is_valid() << endl;
    cout << "  N dims: " << vars[ivar]->num_dims();
    Nc3Dim *vdims[vars[ivar]->num_dims()];
    if (vars[ivar]->num_dims() > 0) {
      cout << ": (";
      for (int ii = 0; ii < vars[ivar]->num_dims(); ii++) {
	vdims[ii] = vars[ivar]->get_dim(ii);
	cout << " " << vdims[ii]->name();
	if (ii != vars[ivar]->num_dims() - 1) {
	  cout << ", ";
	}
      }
      cout << " )";
    }
    cout << endl;
    cout << "  N atts: " << vars[ivar]->num_atts() << endl;
    
    for (int iatt = 0; iatt < vars[ivar]->num_atts(); iatt++) {

      cout << "  Att num: " << iatt << endl;
      Nc3Att *att = vars[ivar]->get_att(iatt);
      _printAtt(att);
      delete att;

    } // iatt

    cout << endl;
    _printVarVals(vars[ivar]);
    
  } // ivar
  
}

// Print an attribute
void Nc2Mdv::_printAtt(Nc3Att *att) {

  cout << "    Name: " << att->name() << endl;
  cout << "    Num vals: " << att->num_vals() << endl;
  cout << "    Type: ";
  
  Nc3Values *values = att->values();

  switch(att->type()) {
    
  case nc3NoType: {
    cout << "No type: ";
  }
  break;
  
  case nc3Byte: {
    cout << "BYTE: ";
    unsigned char *vals = (unsigned char *) values->base();
    for (long ii = 0; ii < att->num_vals(); ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Char: {
    cout << "CHAR: ";
    char vals[att->num_vals() + 1];
    MEM_zero(vals);
    memcpy(vals, values->base(), att->num_vals());
    cout << vals;
  }
  break;
  
  case nc3Short: {
    cout << "SHORT: ";
    short *vals = (short *) values->base();
    for (long ii = 0; ii < att->num_vals(); ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Int: {
    cout << "INT: ";
    int *vals = (int *) values->base();
    for (long ii = 0; ii < att->num_vals(); ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Float: {
    cout << "FLOAT: ";
    float *vals = (float *) values->base();
    for (long ii = 0; ii < att->num_vals(); ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Double: {
    cout << "DOUBLE: ";
    double *vals = (double *) values->base();
    for (long ii = 0; ii < att->num_vals(); ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  }
  
  cout << endl;

  delete values;

}

    
///////////////////////////////
// print variable values

void Nc2Mdv::_printVarVals(Nc3Var *var)

{

  int nprint = var->num_vals();
  if (nprint > 100) {
    nprint = 100;
  }

  Nc3Values *values = var->values();

  cout << "  Variable vals:";
  
  switch(var->type()) {
    
  case nc3NoType: {
  }
  break;
  
  case nc3Byte: {
    cout << "(byte)";
    unsigned char *vals = (unsigned char *) values->base();
    for (long ii = 0; ii < nprint; ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Char: {
    cout << "(char)";
    char str[nprint + 1];
    MEM_zero(str);
    memcpy(str, values->base(), nprint);
    cout << " " << str;
  }
  break;
  
  case nc3Short: {
    cout << "(short)";
    short *vals = (short *) values->base();
    for (long ii = 0; ii < nprint; ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Int: {
    cout << "(int)";
    int *vals = (int *) values->base();
    for (long ii = 0; ii < nprint; ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Float: {
    cout << "(float)";
    float *vals = (float *) values->base();
    for (long ii = 0; ii < nprint; ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  case nc3Double: {
    cout << "(double)";
    double *vals = (double *) values->base();
    for (long ii = 0; ii < nprint; ii++) {
      cout << " " << vals[ii];
    }
  }
  break;
  
  }
  
  cout << endl;

  delete values;

}

// Check if dx is constant.
bool Nc2Mdv::_checkDxIsConstant() {
  if (_nx < 3) {
    return true;
  }

  for (int ix = 0; ix < _nx - 1; ix++) {
    double dx = _xArray[ix+1] - _xArray[ix];
    double diff = fabs(dx - _dx);
    double diffFraction = diff / fabs(_dx);
    if (diffFraction > 0.001) {
      return false;
    }
  }

  return true;
}

// Check if dy is constant.
bool Nc2Mdv::_checkDyIsConstant() {
  if (_ny < 3) {
    return true;
  }

  for (int iy = 0; iy < _ny - 1; iy++) {
    double dy = _yArray[iy+1] - _yArray[iy];
    double diff = fabs(dy - _dy);
    double diffFraction = diff / fabs(_dy);
    if (diffFraction > 0.001) {
      return false;
    }
  }
  
  return true;
}

//////////////////////////////////////////////////////////////////////////
// FInd the limits of valid lat/lon values if the x,y locations are
// supplied in latitude/longitude coords
//
// Returns 0 on success, -1 on failure

int Nc2Mdv::_findValidLatLonLimits()

{
  
  // sanity check

  if (_nx < 5 || _ny < 5) {
    cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
    cerr << "  Grid too small to deduce limits accurately" << endl;
    cerr << "  nx: " << _nx << endl;
    cerr << "  ny: " << _ny << endl;
    _ixValidStart = 0;
    _ixValidEnd = _nx - 1;
    _iyValidStart = 0;
    _iyValidEnd = _ny - 1;
    return -1;
  }
  
  // find mid pt

  int midIx = _nx / 2;
  int midIy = _ny / 2;

  // set origin

  double centerLon = _xArray[midIx];
  double centerLat = _yArray[midIy];

  if (_params.debug) {
    cerr << "  Found data center lon: " << centerLon << ", ";
    cerr << "data center lat: " << centerLat << "." << endl;
  }

  // get starting deltas at center of grid

  double dLon0 =  fabs(_xArray[midIx+1] - _xArray[midIx-1]) / 2.0;
  double dLat0 =  fabs(_yArray[midIy+1] - _yArray[midIy-1]) / 2.0;

  // move out from the grid center, looking for big jumps in the delta
  // and stop there

  _ixValidStart = 0;
  double dLonPrev = dLon0;
  for (int ix = midIx; ix > 0; ix--) {
    double lon0 =  _xArray[ix];
    double lon1 =  _xArray[ix-1];
    double dLon =  fabs(lon0 - lon1);
    double dd = fabs(dLon - dLonPrev);
    double ddFrac = dd / dLonPrev;
    if (fabs(lon0) > 180.0 || lon0 == 0.0 || ddFrac > 1.0) {
      // bad jump, stop here
      _ixValidStart = ix;
      cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
      cerr << "   Bad longitude jump, ix, lon0, lon1: "
           << ix << ", " << lon0 << ", " << lon1 << endl;
      break;
    }
    dLonPrev = dLon;
  } // ix

  _ixValidEnd = _nx - 1;
  dLonPrev = dLon0;
  for (int ix = midIx; ix < _nx - 1; ix++) {
    double lon0 =  _xArray[ix];
    double lon1 =  _xArray[ix+1];
    double dLon =  fabs(_xArray[ix+1] - _xArray[ix]);
    double dd = fabs(dLon - dLonPrev);
    double ddFrac = dd / dLonPrev;
    if (fabs(lon0) > 360.0 || lon0 == 0.0 || ddFrac > 1.0) {
      // bad jump, stop here
      _ixValidEnd = ix;
      cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
      cerr << "   Bad longitude jump, ix, lon0, lon1: "
           << ix << ", " << lon0 << ", " << lon1 << endl;
      break;
    }
    dLonPrev = dLon;
  } // ix

  _iyValidStart = 0;
  double dLatPrev = dLat0;
  for (int iy = midIy; iy > 0; iy--) {
    double lat0 =  _yArray[iy];
    double lat1 =  _yArray[iy-1];
    double dLat =  fabs(_yArray[iy] - _yArray[iy-1]);
    double dd = fabs(dLat - dLatPrev);
    double ddFrac = dd / dLatPrev;
    if (fabs(lat0) > 90.0 || lat0 == 0.0 || ddFrac > 1.0) {
      // big jump, stop here
      _iyValidStart = iy;
      cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
      cerr << "   Bad latitude jump, iy, lat0, lat1: " 
           << iy << ", " << lat0 << ", " << lat1 << endl;
      break;
    }
    dLatPrev = dLat;
  } // iy

  _iyValidEnd = _ny - 1;
  dLatPrev = dLat0;
  for (int iy = midIy; iy < _ny - 1; iy++) {
    double lat0 =  _yArray[iy];
    double lat1 =  _yArray[iy+1];
    double dLat =  fabs(_yArray[iy+1] - _yArray[iy]);
    double dd = fabs(dLat - dLatPrev);
    double ddFrac = dd / dLatPrev;
    if (fabs(lat0) > 90.0 || lat0 == 0.0 || ddFrac > 1.0) {
      // big jump, stop here
      _iyValidEnd = iy;
      cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
      cerr << "   Bad latitude jump, iy, lat0, lat1: " 
           << iy << ", " << lat0 << ", " << lat1 << endl;
      break;
    }
    dLatPrev = dLat;
  } // iy

  _nxValid = _ixValidEnd - _ixValidStart + 1;
  _nyValid = _iyValidEnd - _iyValidStart + 1;

  if (_nxValid < 5 || _nyValid < 5) {
    cerr << "ERROR - Nc2Mdv::_findValidLatLonLimits()" << endl;
    cerr << "  Valid grid too small to process" << endl;
    cerr << "  nxValid: " << _nxValid << endl;
    cerr << "  nyValid: " << _nyValid << endl;
    return -1;
  }

  return 0;
  
}


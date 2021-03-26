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
//////////////////////////////////////////////////////////
// Args.cc
//
// Command line arguments
//
// Mike Dixon, RAP, NCAR, P.O.Box 3000, Boulder, CO, 80307-3000, USA
//
// Oct 2007
//
// MODIFIED BY T. RAUPACH, UNIBE, 2018-2019.
//
/////////////////////////////////////////////////////////////

#include "Args.hh"
#include "Params.hh"
#include <cstring>
#include <toolsa/umisc.h>
using namespace std;

Args::Args() { TDRP_init_override(&override); }
Args::~Args() { TDRP_free_override(&override); }

int Args::parse(int argc, char **argv, string &prog_name) {

  char tmp_str[256];
  
  // loop through args
  for (int i =  1; i < argc; i++) {

    // Help/usage.
    if (!strcmp(argv[i], "--") ||
	!strcmp(argv[i], "-h") ||
	!strcmp(argv[i], "-help") ||
	!strcmp(argv[i], "-man")) {
      
      usage(prog_name, cout);
      exit(0);

    } else if (!strcmp(argv[i], "-debug")) {
      
      sprintf(tmp_str, "debug = TRUE;");
      TDRP_add_override(&override, tmp_str);
      
    } else if (!strcmp(argv[i], "-if") ||
               !strcmp(argv[i], "-f")) {
      if (i < argc - 1) {
	// load up file list vector. Break at next arg which
	// starts with -
	for (int j = i + 1; j < argc; j++) {
	  if (argv[j][0] == '-') {
	    break;
	  } else {
	    inputFiles.push_back(argv[j]);
	  }
	}
      }
    }
  }

  if(inputFiles.size() == 0) {
    cerr << "ERROR: problem with command line arguments." << endl;
    usage(prog_name, cout);
    exit(0);
  }
  
  return(0);
}

void Args::usage(string &prog_name, ostream &out) {
  out << "Usage: " << prog_name << " [options as below] -f files\n"
      << "options:\n"
      << "       [ --, -h, -help, -man ] produce this list.\n"
      << "       [ -debug ] print debug messages\n"
      << endl;

  Params::usage(out);
}

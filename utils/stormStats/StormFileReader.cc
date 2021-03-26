// Simplest possible TITAN storm file reader.
// Based on code in lrose-core.
// MODIFIED BY T. RAUPACH, UNIBE, 2018-2019.

#include "StormFileReader.hh"

StormFileReader::StormFileReader() {}

StormFileReader::~StormFileReader() {
  stormFile.CloseFiles();
  trackFile.CloseFiles();
}

int StormFileReader::open(string infile) {
  string trackPath = infile.substr(0, infile.size()-4) + ".th5";
  string stormPath = infile.substr(0, infile.size()-4) + ".sh5";

  if(_debug) {
    cerr << "  Storm header file: " << stormPath << endl;
    cerr << "  Track header file: " << trackPath << endl;
  }

  // Open storm file.
  if(stormFile.OpenFiles("r", stormPath.c_str()) ||
     stormFile.LockHeaderFile("r") ||
     stormFile.ReadHeader()) {
    cerr << "Error reading storm file." << endl;
    cerr << stormFile.getErrStr() << endl;
    exit(0);
  }
  
  // Open track file.
  if(trackFile.OpenFiles("r", trackPath.c_str()) ||
     trackFile.LockHeaderFile("r") ||
     trackFile.ReadHeader()) {
    cerr << "Error reading track file." << endl;
    cerr << trackFile.getErrStr() << endl;
    exit(0);
  }
}

void StormFileReader::setDebug(bool value) {
  _debug = value;
}


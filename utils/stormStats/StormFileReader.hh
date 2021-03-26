// Simplest possible TITAN storm file reader.
// Based on code in lrose-core.
// MODIFIED BY T. RAUPACH, UNIBE, 2018-2019.

#ifndef StormFileReader_H
#define StormFileReader_H

#include <iostream>
#include <vector>
#include <titan/TitanStormFile.hh>
#include <titan/TitanTrackFile.hh>
#include <titan/TitanComplexTrack.hh>
using namespace std;

class StormFileReader {

public:
  StormFileReader();
  ~StormFileReader();
  int open(string infile);
  void setDebug(bool value);
  TitanTrackFile trackFile;
  TitanStormFile stormFile;
  
private:
  bool _debug = false;
};

#endif

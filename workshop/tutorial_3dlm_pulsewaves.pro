Pro tutorial_3DLM_pulsewaves

  ; init the object
;  plsObj = pulsewavestools(inputfile = 'C:\Users\moi\Documents\GitHub\Magazine\data\pulsewaves\Bertholdstein - Q560_9996619 - 150528_134250.pls')
  plsObj = pulsewavestools(inputfile = '/Users/antoine/Git/Magazine/data/pulsewaves/Bertholdstein - Q560_9996619 - 150528_134250.pls')
  
  ; gettting info from the object
  pulse = plsObj.getPulses(0)
  waves = plsObj.readWaves()

  ; looking at the pulse record
  help, pulse

  ; interacting with the waveform object


  ; converting pls/wvs files into a points cloud
  dum = plsObj.toPointsCloud(/SIMPLE, /CSV)

  ; generate rasters from epc files
  dum = plsObj.toRaster(/INTENSITYOUT, /INTENSITYIN, /ELEVATION, /RANGE, /SACNANGLE)
  ; extract trajectory



  ;dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
  ;dum = plsObj.getAnchors()
  ;write_csv, 'pulsewaves_coord.csv',transpose(dum.xyz())





End
Pro tutorial_3DLM_pulsewaves

  ; init the object
;  plsObj = pulsewavestools(inputfile = 'C:\Users\moi\Documents\GitHub\Magazine\data\pulsewaves\Bertholdstein - Q560_9996619 - 150528_134250.pls')
  plsObj = pulsewavestools(inputfile = '/Users/moi/Github/Magazine/data/pulsewaves/Bertholdstein - Q560_9996619 - 150528_134250.pls')
  
  ; gettting info from the object, 
  ; pulse 230 -> 1 outgoing sampling, 1 return sampling 
  ; pulse 2456 -> 1 outgoing sampling, 2 return samplings
  pulse = plsObj.getPulses(2456)
  waves = plsObj.readWaves(/NO_PLOT)
  help, waves
  
  ; selecting the segment we want to explore
  wave = waves[1]
  
  ; looking at the pulse record
  help, pulse

  ; MANUAL interacting with the waveform object
  
    ; print the origine of the waveform - LAS, PLS, ELSE
    print, wave.originFileType()
    
    ; print the manufacturer of the waveform
    print, wave.manufacturer()
    
    ; get the duration from anchor - time offset
    dfa = wave.durationFromAnchor()
    
    ; get the number of samples in the waveform 
    nsamples = wave.n()
    
    ; get the intensity values
    int = wave.wave()
    
    ; get the look-up table
    lut = wave.lut()
    
    ; get the "not a number" value for this type of waveform
    nan = wave.getNanValue()
    
    ; plot the waveform
    dum = wave.plotwave()
    
    ; find the peaks in the waveform
    ppoints = wave.findPoint(/THRES, /NOSMOOTH, /ADD_TAIL)
    dum = wave.plotwave(/POINTSOVERPLOT)
    
    
  ; destroying object

 
;  ; AUTOMATIC PROCESSING OF THE FILES
;
;  ; converting pls/wvs files into a points cloud
;  dum = plsObj.toPointsCloud(/SIMPLE, /CSV)
;
;  ; generate rasters from epc files - we generate all the raster within one command - but can be split-up into multiple commands
;  dum = plsObj.toRaster(/INTENSITYOUT, /INTENSITYIN, /ELEVATION, /RANGE, /SACNANGLE)
;  
;  ; extract trajectory
;  dum = plsObj.toVector(/TRAJECTORY)






End
Pro tutorial_3DLM_pulsewaves

; init the object
plsObj = pulsewavestools(inputfile ='/Users/antoine/Git/Magazine/data/pulsewaves/Bertholdstein - Q560_9996619 - 150528_134250.pls');'/Users/antoine/Processing_Temp_Folder/Vivian/Pulsewaves/Channel_2/L2-1-M01-S1-C2_r.pls')

; gettting info from the object
pulse = plsObj.getPulses(0)
waves = plsObj.readWaves()

; looking at the pulse record
help, pulse

; interacting with the waveform object


; converting pls/wvs files into a points cloud
dum = plsObj.toPointsCloud(/SIMPLE, /CSV)

; generate rasters from epc files
dum = plsObj.toRaster(/INTENSITYOUT, /ELEVATION)
; extract trajectory



;dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
;dum = plsObj.getAnchors()
;write_csv, 'pulsewaves_coord.csv',transpose(dum.xyz())





End




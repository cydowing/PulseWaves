fileArr = File_search('/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves', '*.pls')

; Extracting the line number and scanner number
lineNumb = Strmid(File_basename(fileArr), 9, 1)
scanNumb = Strmid(File_basename(fileArr), 13, 1)

openw, wLun, '/Users/antoine/Desktop/pulsewaves_information.csv', /APPEND, /GET_LUN

;for i = 1,6 do begin
  i = 3

  ; We will consider sc1 one as the reference for searching into the other data set
  sc1Index = Where(lineNumb eq i and scanNumb eq 3)
  pls = pulsewavestools(INPUTFILE = fileArr[sc1Index], /NO_VLR)
  dum = pls.readPulses(/ALL)
  
  nRef = pls.getHeaderProperty(/NPULSES)
  nLoop = 1UL

  ;  While k lt nRef do begin
  While nLoop lt nRef do begin

    plsd = pls.getPulses(nLoop)
    ray = pls.computePulses(INDEX = nLoop, /UNIT, /NO_PLOT)
    openw, wLun, '/Users/antoine/Desktop/pulsewaves_information.csv', /APPEND, /GET_LUN
    if nLoop eq 1 then Printf, wLun, FORMAT='(%"%s")', fileArr[sc1Index]
    Printf, wLun, FORMAT='(%"%i,%f,%f,%f,%f")', plsd.gpsTime, *(ray.getDurAnchor())
    free_lun, wLun
    
    nLoop += 1UL
      
  Endwhile
  

;Endfor



End
Pro processing_multispectral_quickndirty

; Getting all the PLs files
fileArr = file_search('/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves', '*.pls')

; Extracting the line number and scanner number
lineNumb = STRMID(file_basename(fileArr), 9, 1)
scanNumb = STRMID(file_basename(fileArr), 13, 1)

for i = 1,6 do begin
;i = 1
  
  ; We will consider sc1 one as the reference for searching into the other data set
  sc1Index = where(lineNumb eq i and scanNumb eq 1)
  pls1 = pulsewavestools(INPUTFILE = fileArr[sc1Index], /NO_VLR)
  
  sc2Index = Where(lineNumb eq i and scanNumb eq 2)
  pls2 = pulsewavestools(INPUTFILE = fileArr[sc2Index], /NO_VLR)
  
  sc3Index = Where(lineNumb eq i and scanNumb eq 3)
  pls3 = pulsewavestools(INPUTFILE = fileArr[sc3Index], /NO_VLR)
  
;  dum = pls1.computePulses(/UNIT)
  ray2 = pls2.computePulses(/UNIT)
  ray3 = pls3.computePulses(/UNIT)
  
  nRef = pls1.getHeaderProperty(/NPULSES)
  nLoop = 1UL
  
;  While k lt nRef do begin
  While nLoop lt 10 do begin
    
    ray1 = pls1.computePulses(INDEX = nLoop, /UNIT, /NO_PLOT)
    d12 = ray1.findSimilarRay(ray2)
    simRay2 = pls2.computePulses(INDEX = d12, /UNIT, /NO_PLOT)
    d13 = ray1.findSimilarRay(ray3)
    simRay3 = pls3.computePulses(INDEX = d13, /UNIT, /NO_PLOT)
    
    nLoop += 1UL
  Endwhile
;  trajPls1 = pls1.getTrajectory()
;  trajPls2 = pls2.getTrajectory()
;  trajPls3 = pls3.getTrajectory()
;  write_csv, 'traj_' + strcompress(string(i),/remove_all) + '1.csv',transpose(trajPls1.xyz())
;  write_csv, 'traj_' + strcompress(string(i),/remove_all) + '2.csv',transpose(trajPls2.xyz())
;  write_csv, 'traj_' + strcompress(string(i),/remove_all) + '3.csv',transpose(trajPls3.xyz())
  
  dimPls1 = pls1.getNumberOfPoints()
  dimPls2 = pls2.getNumberOfPoints()
  dimPls3 = pls3.getNumberOfPoints()
  
  print, dimPls1, dimPls2, dimPls3
endfor

End
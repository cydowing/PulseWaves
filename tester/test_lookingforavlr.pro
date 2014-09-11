Pro test_lookingforAVLR

  result = file_search('/Users/antoine/IDLWorkspace83/PulseWaves/data/', '*.pls', COUNT = n)
  
  for i=0, n-1 do begin
  
    plsObj = obj_new('pulsewaves', inputFile = result[i], /QUIET)
    res = plsObj.getHeaderProperty(/NAVLRECORDS)
    print, result[i], res
    
  endfor
  
End
Pro processing_multispectral_quickndirty_01

; Getting all the PLs files
fileArr = file_search('/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves', '*.pls')

; Extracting the line number and scanner number
lineNumb = STRMID(file_basename(fileArr), 9, 1)
scanNumb = STRMID(file_basename(fileArr), 13, 1)
flag = 0B

for j = 1, 3 do begin
  for i = 0, 6 do begin
;  for j = 1, 3 do begin
  
    ; We will consider sc1 one as the reference for searching into the other data set
    sc1Index = where(lineNumb eq i and scanNumb eq j)
    pls1 = pulsewavestools(INPUTFILE = fileArr[sc1Index], /NO_VLR, /NO_HEADER, /QUIET)
    
    nRef = pls1.getHeaderProperty(/NPULSES)
    nLoop = 1UL

     While nLoop lt nRef do begin
;    While nLoop lt 10 do begin    
    
;      ; Establish error handler. When errors occur, the index of the
;      ; error is returned in the variable Error_status:
      Catch, Error_status
;      
;      ;This statement begins the error handler:
      IF Error_status NE 0 THEN BEGIN
        nLoop += 1UL
        if nLoop ge nRef then return
        Catch, /CANCEL
      ENDIF

      ; Compute the pulse
      pulse = pls1.computePulses(INDEX = nLoop, /UNIT, /NO_PLOT)
      
      if size(pulse,/TYPE) eq 11 then begin
        ; Get the last segment
        value = pulse.getLastSegment()
        peaks = pointocator(value.Int, thres = -157., /NOSMOOTH, /ADD_TAIL)
        if (*(peaks.rawuppoints)) ne !NULL then begin
          savedIntensity = (value.int)((*(peaks.rawuppoints))[-1])
          savedCoordinates = (value.coor).extractPoint( (*(peaks.rawuppoints))[-1] )
        
;          ; Progressive result printing
;          if flag eq 0 then begin
;            x = savedCoordinates.x()
;            y = savedCoordinates.y()
;            z = savedCoordinates.z()
;            i = savedIntensity
;
;            name = 'band_' + Strcompress(String(j), /remove_all) + '.csv'
;            Openw, rLun, name, /get_lun
;            Printf, rLun, x, y, z, i
;            Close, rLun
;
;            flag += 1B
;          endif else begin
;            x = savedCoordinates.x()
;            y = savedCoordinates.y()
;            z = savedCoordinates.z()
;            i = savedIntensity
;
;            name = 'band_' + Strcompress(String(j), /remove_all) + '.csv'
;            Openw, rLun, name, /get_lun, /APPEND
;            Printf, rLun, x, y, z, i
;            Close, rLun
;          endelse

;             Concatenation for final print
          if flag eq 0 then begin
            x = savedCoordinates.x()
            y = savedCoordinates.y()
            z = savedCoordinates.z()
            i = savedIntensity
            flag += 1B
          endif else begin
            x = [x,savedCoordinates.x()]
            y = [y,savedCoordinates.y()]
            z = [z,savedCoordinates.z()]
            i = [i,savedIntensity]
          endelse
          
        endif
        
      endif
      nLoop += 1UL
    Endwhile

  endfor
  
  name = 'band_' + strcompress(string(j), /remove_all) + '.csv'
  write_csv, name,transpose(x),transpose(y),transpose(z),transpose(i)
  
endfor

End
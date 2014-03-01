; docformat = 'rst'
;+
; This is an IDL implementation of the Open Source
; PulseWaves format created by Martin Isenburg Creator
; of LASTools and LASZip.
;
; :Category:
; 	READER, WRITER, WAVEFORM
;
; :Return:
; 	If any, what is the output of this method
;
;	:Uses:
;		plsObj = obj_new('pulsewaves', inputfile = '/Path/To/PLS/File')
;
;	:Example:
;		A quick example on how to use this method
;
; :History:
; 	September 2013
; 	 -First implementation
;
; :Author:
;   Antoine Cottin
;-
Pro pulsewaves__define

void = { pulsewaves, $
  plsFilePath   : "",$
  plsHeader     : ptr_new(),$
  plsvlrarray   : ptr_new(),$
  plspulserec   : ptr_new(),$
  plsavlrarray  : ptr_new(),$
  out           : obj_new() $
}

End



;+
;
; Create an instance of the PulseWaves Class Object.
;
; :Categories:
;   GENERAL
;
; :Returns:
;   Return an instance of the PulseWaves Class Object.
;
; :Uses:
;   plsObj = obj_new('pulsewaves', inputFile = '/Path/To/The/File.pls')
;       
; :Keywords:
;    inputfile: in, required, type=string
;     This is the fully qualified path to the file
;    
;  :Author:
;     Antoine Cottin
;
;  :History:
;     -01/03/2014: Creation
;
;-
Function pulsewaves::init, inputfile = file

  Compile_opt idl2
  
  ; Checking the type of file, pls or wvs, and find the other accordingly
  
  ; Initialazing data members
  self.plsHeader = ptr_new(self.initplsheader())
  ;self.plspulserec = ptr_new(self.initpulserecord())
  self.out = obj_new('consoleoutput')
  
  ;Checking that the provided file exist
  exist = File_test(file)
  if exist eq 1 then self.plsFilePath = file else begin
    while exist ne 1 do begin
      self.out->print, 3, "File doesn't seems to exist..."
      self.out->print, 3, "Please re-enter a file path string"
      newPath = ""
      read, newPath
      print, newPath
      exist = File_test(newPath)
    endwhile
    self.plsFilePath = newPath
  endelse

  
  ; Loading data into data members
  dum = self.readHeader()
  if (*self.plsheader).nvlrecords ne 0 then dum = self.readVLR()
  if (*self.plsheader).nPulses ne 0 then dum = self.readPulses()
  if (*self.plsheader).navlrecords ne 0 then dum = self.readAVLR()
  
  return, 1
  
End



;+
; Cleanup the object. This method is call automatically using the obj_destroy method.
;
; :Categories:
;   GENERAL
;
; :Returns:
;   Destroy the actual object
;
; :Uses:
;   obj_destroy, Obj
;
; :Examples:
;     setup the object
;       plsObj = obj_new('pulsewaves', inputFile = '/Path/To/The/File.pls')
;
;     Destroying the object
;       obj_destroy, plsObj
;       
;  :Author:
;     Antoine Cottin
;   
;  :History:
;     -01/03/2014: Creation
;
;-
Function pulsewaves::cleanup

  ; Removing the temporary files
  self.out->print,1 , 'Destroying PulseWaves object...'
  self.out->print,1 , 'Cleaning memory...'
  plsFilePath = 0
  ptr_free, $
    self.plsHeader,$
    self.plsvlrarray,$
    self.plspulserec,$
    self.plsavlrarray

  ; Destroying the consoleOutput object
  self.out->print,1 , 'Destroying remaining objects...'
  self.out->print,1 , 'Bye :)'
  obj_destroy, self.out
  
End



Function pulsewaves::initplsheader

void = { plsheader, $
  signature       : byte('PulseWavesPulse0'), $   ; File signature
  globalPram      : 0UL, $
  fileSource      : 0UL,  $                       ; File source ID
  guid1           : 0UL, $                        ; Project ID - GUID data 1
  guid2           : 0US,  $                       ; Project ID - GUID data 2
  guid3           : 0US,  $                       ; Project ID - GUID data 3
  guid4           : bytarr(8), $                  ; Project ID - GUID data 4
  systemID        : bytarr(64), $                 ; System identifier
  softwareID      : bytarr(64), $                 ; Generating software
  day             : 0US,    $                     ; File creation day of year
  year            : 0US,    $                     ; File creation year
  versionMajor    : 1B, $                         ; Version major
  versionMinor    : 1B, $                         ; Version minor
  headerSize      : 0US,  $                       ; Header size - 352 bytes at this version
  offsetPulse     : 0ULL, $                       ; Offset to pulse data -- 352 bytes at this version
  nPulses         : 0ULL, $                       ; Number of point records
  pulseFormat     : 0UL, $                        ; Pulse format
  pulseAttrib     : 0UL, $                        ; Pulse attribute
  pulseSize       : 0UL, $                        ; Pulse size
  pulseCompress   : 0UL, $                        ; Pulse compression
  reserved        : 0ULL,  $                      ; Reserved
  nvlrecords      : 0UL,   $                      ; Number of Variable Length Records
  navlrecords     : 0UL,   $                      ; Number of Append Variable Length Records
  tScale          : 0D, $                         ; T Scale Factor
  tOffset         : 0D, $                         ; T offset
  tMin            : 0ULL, $                       ; t minimum
  tMax            : 0ULL, $                       ; t maximum
  xScale          : 0D, $                         ; X scale factor
  yScale          : 0D, $                         ; Y scale factor
  zScale          : 0D, $                         ; Z scale factor
  xOffset         : 0D, $                         ; X offset
  yOffset         : 0D, $                         ; Y offset
  zOffset         : 0D, $                         ; Z offset
  xMin            : 0D, $                         ; Max X
  xMax            : 0D, $                         ; Min X
  yMin            : 0D, $                         ; Max Y
  yMax            : 0D, $                         ; Min Y
  zMin            : 0D, $                         ; Max Z
  zMax            : 0D  $                         ; Min Z
  }

return, void

End



Function pulsewaves::initplsvlr

void = {plsvlr, $
  userID          : bytarr(16), $                 ; User ID, any string, remaining characters must be set to null
  recordID        : 0UL, $                        ; ID for each vlr, define by manufacturer
  reserved        : 0Ul, $                        ; Reserved, must be set to 0
  recLengthAfter  : 0ULL, $                       ; Number of bytes contain in the vlr
  description     : bytarr(64) $                  ; Null terminated text description. Any characters not used must be null
  }

return, void

End



Function pulsewaves::initplsavlr

  void = {plsvlr, $
    userID          : bytarr(16), $                 ; User ID, any string, remaining characters must be set to null
    recordID        : 0UL, $                        ; ID for each vlr, define by manufacturer
    reserved        : 0Ul, $                        ; Reserved, must be set to 0
    recLengthBefore : 0ULL, $                       ; Number of bytes contain in the vlr
    description     : bytarr(64) $                  ; Null terminated text description. Any characters not used must be null
  }
  
  return, void
  
End



Function pulsewaves::initpulserecord

void = {pulserecord, $
  gpsTime           : 0ULL, $                     ; GPS time
  waveOffset        : 0ULL, $                     ; Bytes offset to wave record
  anchorX           : 0UL, $                      ; Anchor point of the wave
  anchorY           : 0UL, $                      ; Anchor point of the wave
  anchorZ           : 0UL, $                      ; Anchor point of the wave
  targetX           : 0UL, $                      ; Ending point of the wave
  targetY           : 0UL, $                      ; Ending point of the wave
  targetZ           : 0UL, $                      ; Ending point of the wave
  firstReturn       : 0US, $                      ; 
  lastReturn        : 0US, $                      ;
  pulseDesIndex     : 0US, $                      ; To check
  intensity         : 0B, $                       ; Intensity of the pulse in DN
  classification    : 0B  $                       ; Classification of the pulse
  }
  
return, void

End



Function pulsewaves::readHeader


  ; Open the file
  Openr, inputLun, self.plsFilePath, /get_lun, /swap_if_big_endian

  ; Check if the file is a PLS file
  signature = Bytarr(16)
  Readu, inputLun, signature

  if String(signature) eq 'PulseWavesPulse' then begin

    self.out->print,1, 'PulseWaves file detected..."
    self.out->print,1, "Looking for version number..."
    Point_lun,inputLun, 173
    majorVersion = 1B
    minorVersion = 1B
    Readu, inputLun, majorVersion
    Readu, inputLun, minorVersion
    self.out->print,1,Strcompress("PulseWaves Version " + String(Fix(majorVersion)) + "." + Strcompress(String(Fix(minorVersion)),/remove_all) + " detected.")
    self.out->print,1, "Initializing the header..."

    ; Closing and re-opening the file to reinitialize the pointer
    Free_lun,inputLun
    Openr, inputLun, self.plsFilePath, /get_lun, /swap_if_big_endian
    ; Putting file data into data member
    Readu, inputLun, (*self.plsheader)
    
    self.out->print,1, "Reading file header..."
    self.out->print,1, Strcompress("System identifier: " + String((*self.plsheader).systemID))
    self.out->print,1, Strcompress("Generating goftware: " + String((*self.plsheader).softwareID))
    self.out->print,1, Strcompress("Day of creation: " + String(Fix((*self.plsheader).day)))
    self.out->print,1, Strcompress("Year of creation: " + String(Fix((*self.plsheader).year)))
    self.out->print,1, Strcompress("Header size: " + String(Fix((*self.plsheader).headerSize)))   
    self.out->print,1, Strcompress("Byte offset to pulses block: " + String((*self.plsheader).offsetPulse))
    self.out->print,1, Strcompress("File contains " + String((*self.plsheader).nPulses) + " pulses.")
    self.out->print,1, Strcompress("Pulse format: " + String(Fix((*self.plsheader).pulseFormat)))
    self.out->print,1, Strcompress("Pulse attributes: " + String((*self.plsheader).pulseAttrib))
    self.out->print,1, Strcompress("Pulse size: " + String((*self.plsheader).pulseSize) + " bytes.")
    self.out->print,1, Strcompress("Pulse compression: " + String(Fix((*self.plsheader).pulseCompress)))
    self.out->print,1, Strcompress("Number of Variable Length Records: " + String(Fix((*self.plsheader).nvlrecords)))
    self.out->print,1, Strcompress("Number of Append Variable Length Records: " + String(Fix((*self.plsheader).navlrecords)))
    self.out->print,1, Strcompress("T(ime) scale factor: " + String((*self.plsheader).tScale))
    self.out->print,1, Strcompress("T(ime) offset: " + String((*self.plsheader).tOffset))
    self.out->print,1, Strcompress("Minimum T(ime): " + String((*self.plsheader).tMin)) 
    self.out->print,1, Strcompress("Maximum T(ime): " + String((*self.plsheader).tMax)) 
    self.out->print,1, Strcompress("X scale factor: " + String((*self.plsheader).xScale)) 
    self.out->print,1, Strcompress("Y scale factor: " + String((*self.plsheader).yScale))
    self.out->print,1, Strcompress("Z scale factor: " + String((*self.plsheader).zScale))
    self.Out->print,1, Strcompress("X offset factor: " + String((*self.plsheader).xOffset))
    self.Out->print,1, Strcompress("Y offset factor: " + String((*self.plsheader).yOffset))
    self.Out->print,1, Strcompress("Z offset factor: " + String((*self.plsheader).zOffset))   
    self.out->print,1, Strcompress("X Minimum: " + String((*self.plsheader).xMin))
    self.out->print,1, Strcompress("X Maximum: " + String((*self.plsheader).xMax))
    self.Out->print,1, Strcompress("Y Minimum: " + String((*self.plsheader).yMin))
    self.Out->print,1, Strcompress("Y Maximum: " + String((*self.plsheader).yMax))
    self.Out->print,1, Strcompress("Z Minimum: " + String((*self.plsheader).zMin))
    self.Out->print,1, Strcompress("Z Maximum: " + String((*self.plsheader).zMax))
    
;    (*self.initplsheader)


  Close, inputLun

  Return, 1
  
endif else begin
  
  
endelse

End



Function pulsewaves::readVLR

End



Function pulsewaves::readPulses

End


Function pulsewaves::readAVLR


End


Function pulsewaves::writePulse



End
Pro pulsewaves__define

void = { pulsewaves, $
  plsHeader     : ptr_new(),$
  plsvlrarray   : ptr_new(),$
  plspulserec   : ptr_new(),$
  plsavlrarray  : ptr_new() $
}

End


Function pulsewaves::init, inputfile = file

  Compile_opt idl2
  
  ; Checking the type of file, pls or wvs, and find the other accordingly
  
  ; Read the file and put everything into the object structure
  self.plsHeader = ptr_new(self.initplsheader())
  self.plspulserec = ptr_new(self.initpulserecord())
  
  return, 1
  
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
  nvlrecords      : 0UL,   $                      ; Number of variable length records
  navlrecords     : 0UL,   $                      ; Number of variable length records
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
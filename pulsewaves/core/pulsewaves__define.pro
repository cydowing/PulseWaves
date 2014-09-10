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
; 	March 2014
; 	 - More serious developements
;
; :Author:
;   Antoine Cottin
;-
Pro pulsewaves__define

void = { pulsewaves, $
  plsFilePath   : "",$                ; Fully qualified path to the PLS file
  wvsFilePath   : "",$                ; Fully qualified path to the WVS file
  plsHeader     : ptr_new(),$         ; Pointer to the PLS Header data
  plsVlrArray   : ptr_new(),$         ; Pointer to the Variable Length Records (in reading order - Header/Key)
  plsPulseRec   : ptr_new(),$         ; Pointer to the records of the PLS file hold in the data member
  plsPulseInd   : ptr_new(),$         ; Pointer to the index of the records from the PLS file hold in plsPulseRec
  plsAvlrarray  : ptr_new(),$         ; Pointer to the Appned Variable Length Records (in reading order - Header/Key)
  wvsHeader     : ptr_new(),$         ; Pointer to the header of the WVS file
  wvsWaveRec    : ptr_new(),$         ; Pointer to the records of the WVS file corresponding to the records in plsPulseRec
  wvsWaveInd    : ptr_new(),$         ; Pointer to the index corresponding to the records in plsPulseRec
;  out           : obj_new() $         ; Object that allow nice print out
  inherits consoleoutput $
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
Function pulsewaves::init, inputfile = file, _extra = console_options

  Compile_opt idl2
  
  ; Checking the type of file, pls or wvs, and find the other accordingly
  
  ; Initialazing data members
  self.plsHeader = ptr_new(self.initplsheader())
  ;self.plspulserec = ptr_new(self.initpulserecord())
;  self.out = obj_new('consoleoutput', _extra = console_options)
  
  ;Checking that the provided file exist
  exist = File_test(file)
  if exist eq 1 then begin
    self.plsFilePath = file
    self.wvsFilePath = self.getWaveFileName(self.plsFilePath)
  endif else begin
    while exist ne 1 do begin
      self.print, 3, "File doesn't seems to exist..."
      self.print, 3, "Please re-enter a file path string"
      newPath = ""
      read, newPath
      print, newPath
      exist = File_test(newPath)
    endwhile
    self.plsFilePath = newPath
    self.wvsFilePath = self.getWaveFileName(self.plsFilePath)
  endelse
  
  ; Loading data into data members
  dum = self.readHeader()
  if (*self.plsheader).nvlrecords ne 0 then dum = self.readVLR()
  if (*self.plsheader).nPulses ne 0 then begin
    dum = self.readPulses(/ALL)
    dum = self.readWaves(/ALL)
  endif
  if (*self.plsheader).navlrecords ne 0 then dum = self.readAVLR()
  
  close, /ALL
  
  return, 1
  
End



Function pulsewaves::getWaveFileName, plsfile

  if strlowcase(!version.OS_NAME) eq "linux" or strlowcase(!version.OS_NAME) eq "mac os x" then spath='/' else spath='\'
  sep=strcompress(strmid(spath, 0, 1,/reverse_offset))
  path = file_dirname(plsfile)
  file = file_basename(plsfile)
  return, strcompress( path + spath + strmid(file, 0, strpos(file, '.', /reverse_search )) + '.wvs')

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
  self.print,1 , 'Destroying PulseWaves object...'
  self.print,1 , 'Cleaning memory...'
  plsFilePath = 0
  ptr_free, $
    self.plsHeader,$
    self.plsvlrarray,$
    self.plspulserec,$
    self.plspulseInd,$
    self.wvsWaveRec,$
    self.wvsHeader,$
    self.plsavlrarray

  ; Destroying the consoleOutput object
  self.print,1 , 'Destroying remaining objects...'
  self.print,1 , 'Bye :)'
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
  firstReturn       : 0US, $                      ; Duration in sampling units from the anchor point to the first recorded waveform sample
  lastReturn        : 0US, $                      ; Duration in sampling units from the anchor point to the last recorded waveform sample
  pulseDesIndex     : 0US, $                      ; To check
  intensity         : 0B, $                       ; Intensity of the pulse in DN
  classification    : 0B  $                       ; Classification of the pulse
  }
  
return, void

End



Function pulsewaves::initwvsheader

void = {wvsheader, $
  signature         : bytarr(16), $
  compression       : 0UL, $
  reserve           : bytarr(44) $
  }
  
return, void

End




Function pulsewaves::readHeader


  ; Compiling dependant procedure and function files
;  Resolve_routine, 'consoleoutput__define', /COMPILE_FULL_FILE
;  out = Obj_new('consoleoutput')
  

  ; Open the file
  Openr, 1, self.plsFilePath, /swap_if_big_endian

  ; Check if the file is a PLS file
  signature = Bytarr(16)
  Readu, 1, signature

  if String(signature) eq 'PulseWavesPulse' then begin

    self.print,1, 'PulseWaves file detected..."
    self.print,1, "Looking for version number..."
    Point_lun,1, 173
    majorVersion = 1B
    minorVersion = 1B
    Readu, 1, majorVersion
    Readu, 1, minorVersion
    self.print,1,Strcompress("PulseWaves Version " + String(Fix(majorVersion)) + "." + Strcompress(String(Fix(minorVersion)),/remove_all) + " detected.")
    self.print,1, "Initializing the header..."

    ; Closing and re-opening the file to reinitialize the pointer
    Close,1
    Openr, 1, self.plsFilePath, /swap_if_big_endian
    ; Putting file data into data member
    Readu, 1, (*self.plsheader)
    
    self.print,1, "Reading file header..."
    self.print,1, Strcompress("System identifier: " + String((*self.plsheader).systemID))
    self.print,1, Strcompress("Generating software: " + String((*self.plsheader).softwareID))
    self.print,1, Strcompress("Day of creation: " + String(Fix((*self.plsheader).day)))
    self.print,1, Strcompress("Year of creation: " + String(Fix((*self.plsheader).year)))
    self.print,1, Strcompress("Header size: " + String(Fix((*self.plsheader).headerSize)))   
    self.print,1, Strcompress("Byte offset to pulses block: " + String((*self.plsheader).offsetPulse))
    self.print,1, Strcompress("File contains " + String((*self.plsheader).nPulses) + " pulses.")
    self.print,1, Strcompress("Pulse format: " + String(Fix((*self.plsheader).pulseFormat)))
    self.print,1, Strcompress("Pulse attributes: " + String((*self.plsheader).pulseAttrib))
    self.print,1, Strcompress("Pulse size: " + String((*self.plsheader).pulseSize) + " bytes.")
    self.print,1, Strcompress("Pulse compression: " + String(Fix((*self.plsheader).pulseCompress)))
    self.print,1, Strcompress("Number of Variable Length Records: " + String(Fix((*self.plsheader).nvlrecords)))
    self.print,1, Strcompress("Number of Append Variable Length Records: " + String(Fix((*self.plsheader).navlrecords)))
    self.print,1, Strcompress("T(ime) scale factor: " + String((*self.plsheader).tScale))
    self.print,1, Strcompress("T(ime) offset: " + String((*self.plsheader).tOffset))
    self.print,1, Strcompress("Minimum T(ime): " + String((*self.plsheader).tMin)) 
    self.print,1, Strcompress("Maximum T(ime): " + String((*self.plsheader).tMax)) 
    self.print,1, Strcompress("X scale factor: " + String((*self.plsheader).xScale)) 
    self.print,1, Strcompress("Y scale factor: " + String((*self.plsheader).yScale))
    self.print,1, Strcompress("Z scale factor: " + String((*self.plsheader).zScale))
    self.print,1, Strcompress("X offset factor: " + String((*self.plsheader).xOffset))
    self.print,1, Strcompress("Y offset factor: " + String((*self.plsheader).yOffset))
    self.print,1, Strcompress("Z offset factor: " + String((*self.plsheader).zOffset))   
    self.print,1, Strcompress("X Minimum: " + String((*self.plsheader).xMin))
    self.print,1, Strcompress("X Maximum: " + String((*self.plsheader).xMax))
    self.print,1, Strcompress("Y Minimum: " + String((*self.plsheader).yMin))
    self.print,1, Strcompress("Y Maximum: " + String((*self.plsheader).yMax))
    self.print,1, Strcompress("Z Minimum: " + String((*self.plsheader).zMin))
    self.print,1, Strcompress("Z Maximum: " + String((*self.plsheader).zMax))
    
;    (*self.initplsheader)
    ; Sanity check of the header
    point_lun, -1, dum
    self.print,1, "Sanity check of the header..."
    if dum eq (*self.plsHeader).headerSize then self.print,1, "Header' sanity check passed..." else begin
      self.print,2, "Header' sanity check NOT passed..."
      self.print,2, "The rest of the data might be wrong..."
      self.print,2, "We continue anyway to read (for now)..."
    endelse

  Close, 1
  
  self.print,1, "Reading Header of the associated Waves file..."
  ; Open the file
  Openr, 1, self.wvsFilePath, /swap_if_big_endian
  self.wvsHeader = ptr_new(self.initwvsheader())
  Readu, 1, (*self.wvsHeader)
  if String(signature) eq 'PulseWavesPulse' then self.print, 1, "Header's signature is valid..." else begin
    self.print, 2, "Header' signature is invalid !"
  endelse
  
  self.print,1, "Header read and stored..."

  Return, 1
  
endif else begin
  
  
endelse

End



;+
; :Description:
;    The purpose of this method is the read and extract the Variable Length Records from
;    the public header block of the LAS file.
;
; :Category:
;   LAS
;
; :Return:
;   Will return 4 arrays:
;    vlrFileID - A string array that the path to the temporary files that hold the files name
;    vlrByteSize - An Unsigned Long array that holds the byte size of each key
;    vlrId - A byte array that holds a byte flag to describ the geokey
;    vlrArr - A pointer array that contains the key in reading order header/key
;
; :Uses:
;   dum = readVRL(inputFile, header, vlrFileArr, vlrByteSizeArr, vlrId ,vlrArr)
;
; :Example:
;   This is not a public method
;
; :History:
;   September 2013
;    -First implementation
;   February 2014
;    -Remodeling of the whole procedure for better results
;
; :Author:
;   Antoine Cottin
;-
Function pulsewaves::readVLR

  
     self.print,1, "Reading Variable Length Records..."
    ; Init VLR Header structure
    vlrStruct = self.initplsvlr()
    close, 1
    openr, 1, self.plsFilePath, /swap_if_big_endian
    point_lun, 1, (*self.plsheader).headerSize
    
    ; This is pointer array that will hold the VLR, header/key, in reading order
    vlrArr = ptrarr(((*self.plsheader).nvlrecords) * 2)
    
    for w=0,((*self.plsheader).nvlrecords)-1 do begin
    
 
      readu, 1, vlrStruct
;      print, vlrStruct.recordID
      
      ; Creating a temp file that hold the nth VLR record - one file per record
      vlrArr[w] = ptr_new(vlrStruct)
      
      case 1 of
      
        (vlrStruct.recordID ge 100) and (vlrStruct.recordID lt 356): begin
            
            self.print,1, "Waveform packet descriptor found"
            
            wfDescriptor = {$
              bitsPerSample:0B,$
              waveformCompressionType:0B,$
              numberOfSamples:0UL,$
              temporalSampleSpacing:0UL,$
              digitizerGain:0.0D,$
              digitizerOffset:0.0D $
            }
            readu, 1, wfDescriptor
            
            vlrArr[w+1] = ptr_new(wfDescriptor)
            
        end
        
        (vlrStruct.recordID eq 34735): begin
          
            self.print,1,'"GeoKeyDirectoryTag Record" found'
            
            vlrGeoKeyHeader = vlrStruct
            
            geoKeyHeader = {$
              wKeyDirectoryVersion:0US,$
              wKeyRevision:0US,$
              wMinorRevision:0US,$
              wNumberOfKeys:0US$    ;TODO to update if we add fields in there
              }
          
            readu,1,geoKeyHeader
            
            sKeyEntry = {$
              wKeyID:0US,$
              wTIFFTagLocation:0US,$
              wCount:0US,$
              wValueOffset:0US$
              }
              
            geoKeyArray = replicate(sKeyEntry, gkdTag.wNumberOfKeys)
            readu,1,geoKeyArray
            
            tempStruc = {header:geoKeyHeader, key:geoKeyArray}
            vlrArr[w+1] = ptr_new(tempStruc)
            tempStruc = 0
          
        end
        
        ; PulseWaves_Spec - FIRST AVLR after pulses block
        (vlrStruct.recordID eq 4294967295): begin
            self.print,1,'First Appended Variable Length Record found'
          end
        
        ; PulseWaves_Spec - SCANNER  
        (vlrStruct.recordID ge 100001 and vlrStruct.recordID lt 100255): begin
            
            self.print,1,'Scanner descriptor found'
            
            ; This key has a size of 248 bytes
            scannerKey = {$
              sizeK       : 0UL,$
              reserver    : 0UL,$
              instrument  : bytarr(64),$
              serial      : bytarr(64),$
              wavelength  : 0. ,$
              outPlsWidth : 0. ,$
              scanPattern : 0UL,$
              nMirrorFace : 0UL,$
              scanFreq    : 0.,$
              scanMinAngle: 0.,$
              scanMaxAngle: 0.,$
              plsFreq     : 0.,$
              beamDiam    : 0.,$
              beamDiv     : 0.,$
              minRange    : 0.,$
              maxRange    : 0.,$
              description : bytarr(64) $
              }
            
            readu, 1, scannerKey 
            vlrArr[w+1] = ptr_new(scannerKey)
            
            
            ; Printing the information
            self.print,1, Strcompress("System identifier: " + String(scannerKey.instrument))
            self.print,1, Strcompress("System serial: " + String(scannerKey.serial))
            self.print,1, Strcompress("System wavelength: " + String(scannerKey.wavelength) + " nm")
            self.print,1, Strcompress("System outgoing pulse width: " + String(scannerKey.outPlsWidth) + " nm")
            self.print,1, Strcompress("System scan pattern: " + String(scannerKey.scanPattern))
            self.print,1, Strcompress("System number of mirror facets: " + String(scannerKey.nMirrorFace))
            self.print,1, Strcompress("System scan frequency: " + String(scannerKey.scanFreq) + " hz")
            self.print,1, Strcompress("System minimum scan angle: " + String(scannerKey.scanMinAngle) + " deg")
            self.print,1, Strcompress("System maximum scan angle: " + String(scannerKey.scanMaxAngle) + " deg")
            self.print,1, Strcompress("System pulse frequency: " + String(scannerKey.plsFreq) + " khz")
            self.print,1, Strcompress("System beam diameter at exit aperture: " + String(scannerKey.beamDiam) + " mm")
            self.print,1, Strcompress("System beam divergence: " + String(scannerKey.beamDiv) + " mrad")
            self.print,1, Strcompress("System minimum range: " + String(scannerKey.minRange) + " mrad")
            self.print,1, Strcompress("System maximum range: " + String(scannerKey.maxRange) + " mrad")
            self.print,1, Strcompress("System description (id any): " + String(scannerKey.description))
  
            
          end
        
        ; PulseWaves_Spec - PULSE DESCRIPTOR
        (vlrStruct.recordID ge 200001 and vlrStruct.recordID lt 200255): begin
          
            self.print,1,'Pulse descriptor found'
            
            ; This key has a size of 92 bytes
            pulseKey = {$
              sizeK       : 0UL,$           ;Size of the key
              reserver    : 0UL,$           ; Reserved
              opCentAnch  : 0L ,$           ; Optical Center to Anchor Point
              nEBytes     : 0US,$           ; Number of Extra Wave Bytes
              nSampling   : 0US,$           ; Number of Samplings
              sampleUnit  : 0. ,$           ; Sampling unit
              scanIndex   : 0UL,$           ; Scanner Index
              compression : 0UL,$           ; Compression
              description : bytarr(64) $
            }
            
            readu, 1, pulseKey
            ;vlrArr[w+1] = ptr_new(pulseKey)
            self.print,1,'Reading composition record...'
            
            ; Printing the information
            self.print,1, Strcompress("Pulse optical center to anchor: " + String(pulseKey.opCentAnch) + ' (sampling unit)')
            self.print,1, Strcompress("Pulse number of extra bytes: " + String(fix(pulseKey.nEBytes)) + ' bytes')
            self.print,1, Strcompress("Pulse number of sampling: " + String(fix(pulseKey.nSampling)))
            self.print,1, Strcompress("Pulse samples unit: " + String(pulseKey.sampleUnit) + " ns")
            self.print,1, Strcompress("Pulse scanner index : " + String(fix(pulseKey.scanIndex)))
            self.print,1, Strcompress("Pulse compression: " + String(fix(pulseKey.compression)))
            self.print,1, Strcompress("Pulse description: " + String(pulseKey.description))
            
            
            
            if pulseKey.nSampling eq 1 then begin
              self.print,1,'There is one Sampling Descriptor record...'
            endif else begin
              self.print, 1, 'There are ' + strcompress(string(pulseKey.nSampling), /REMOVE_ALL) + ' Sampling Descriptor records...'
            endelse
            
            ; This key has a size of 248 bytes
            samplingKey = {$
              sizeK                   : 0UL,$
              reserver                : 0UL,$
              type                    : 0B ,$
              channel                 : 0B ,$
              unused                  : 0B ,$
              bitDurationFromAnchor   : 0B ,$
              scaleDurationFromAnchor : 0. ,$
              offsetDurationFromAnchor: 0. ,$
              bitForNSegments         : 0B ,$
              bitForNSamples          : 0B ,$
              nSegments               : 0US,$
              nSamples                : 0UL,$
              bitPerSample            : 0US,$
              LookupTableInde         : 0US,$
              sampleUnits             : 0. ,$
              compression             : 0UL,$
              description : bytarr(64) $
            }
            
            samplingRecords = replicate(samplingKey, pulseKey.nSampling)
            readu, 1, samplingRecords
            vlrArr[w+1] = ptr_new({compositionRecord:pulseKey, samplingRecord:samplingRecords})
;            print, samplingRecords
            
            for k = 0, pulseKey.nSampling-1 do begin
              
              self.print,1,'========================================================='
              self.print,1,'Reading sampling record number ' + strcompress(string(k))
              
              ; Printing the information
              self.print,1, Strcompress("Sampling type: " + String(samplingRecords[k].type))
              self.print,1, Strcompress("Sampling channel: " + String(samplingRecords[k].channel))
              self.print,1, Strcompress("Sampling bits for duration from anchor: " + String(samplingRecords[k].bitDurationFromAnchor))
              self.print,1, Strcompress("Sampling scale for duration from anchor: : " + String(samplingRecords[k].scaleDurationFromAnchor))
              self.print,1, Strcompress("Sampling offset for duration from anchor: : " + String(samplingRecords[k].offsetDurationFromAnchor))
              self.print,1, Strcompress("Sampling bits for number of segments: " + String(samplingRecords[k].bitForNSegments))
              self.print,1, Strcompress("Sampling bits for number of samples : " + String(samplingRecords[k].bitForNSamples))
              self.print,1, Strcompress("Sampling number of segments : " + String(samplingRecords[k].nSegments))
              self.print,1, Strcompress("Sampling number of samples : " + String(samplingRecords[k].nSamples))
              self.print,1, Strcompress("Sampling bits per sample : " + String(samplingRecords[k].bitPerSample))
              self.print,1, Strcompress("Sampling lookup table : " + String(samplingRecords[k].LookupTableInde))
              self.print,1, Strcompress("Sampling sample unit : " + String(samplingRecords[k].sampleUnits) + ' ns')
              self.print,1, Strcompress("Sampling compression : " + String(samplingRecords[k].compression))
              self.print,1, Strcompress("Sampling description : " + String(samplingRecords[k].description))
              
              if k eq pulseKey.nSampling-1 then self.print,1,'========================================================='
              
            endfor
            
          end
          
        ; PulseWaves_Spec - TABLE  
        (vlrStruct.recordID ge 300001 and vlrStruct.recordID lt 300255): begin
          
          
          end
    
        else: begin
        
            generic = bytarr(vlrStruct.recLengthAfter)
            readu,1,generic

            vlrArr[w+1] = ptr_new(generic)
          
        end
        
  endcase
 
endfor

close,1
self.plsvlrarray = ptr_new(vlrArr)

return, 1

End



;+
; This function returns the number of flightlines of a specific survey day
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   An integer equals to the number of flightlines.
;
; :Uses:
;   Result=Obj->getNumberOfFlightline(day=surveyDay)
;
; :Examples:
;
;     Define the bounding coordinate for the selection
;     geoBox = [488401.968647,487901.968647,235421.265389,234921.265386]
;
;     Initialize the object
;     lasObj = obj_new('laslib')
;
;     Load the 5th flightline from December 5th 2003 data
;     lasObj->loadData, day='338', flightline=5, /QUIET
;
;     Select points that lies inside the bounding box.
;     Result = lasObj->getData(boundingBox=geoBox)
;
; :Keywords:
;   boundingBox : in, optional, type=dblarr(6)
;     Geographical limit to filter the data. It can be Easting and/or Northing and/or Elevation values.
;     The array need to be of one of the following format:
;       [xMax, xMin, yMax, yMin, zMax, zMin] or,
;       [zMax, zMin] or,
;       [zMax] or,
;       [zMin]
;   pointNumber : in, optional, type=long
;     PointNumber can be either one point index (long) and range of continuous points [pointMinIndex, pointMaxIndex],
;     or a collection of discrete points lonarr(n).
;   max : in, optional, type=boolean
;     Set the boundingBox value as the maximum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   min :
;     Set the boundingBox value as the minimum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   all : in, optional, type=boolean
;     If present, will return all the points of the LAS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-
Function pulsewaves::readPulses, ALL=ALL

; start time
T = SYSTIME(1)

openr, getDataLun, self.plsFilePath, /get_lun, /swap_if_big_endian

; keyword /all set -> returning all the points of the LAS file
if keyword_set(ALL) then begin

  self.print,1,"Formating pulse data..."
  
  ; Retriving the data packet
  plsStructure = self.initPulseRecord()
  pulseData = replicate(plsStructure, (*self.plsHeader).nPulses)
  point_lun, getDataLun, (*self.plsHeader).offsetPulse
  readu, getDataLun, pulseData
  
  index = lindgen((*(self.plsHeader)).nPulses)
  
  if (size(pulseData))[2] ne 8 then $
    self.print,2,"Nothing return !" else $
    self.print,1,strcompress('Number of pulse records returned: ' + string((*self.plsHeader).nPulses))
  self.print,1, strcompress("Loading Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif

free_lun, getDataLun
; Updating data members
self.plspulserec = ptr_new(pulseData)
self.plspulseInd = ptr_new(index)

Return, 1

End



;+
; This function returns the number of flightlines of a specific survey day
;
; :Categories:
;   GENERAL, GET
;
; :Returns:
;   An integer equals to the number of flightlines.
;
; :Uses:
;   Result=Obj->getNumberOfFlightline(day=surveyDay)
;
; :Examples:
;
;     Define the bounding coordinate for the selection
;     geoBox = [488401.968647,487901.968647,235421.265389,234921.265386]
;
;     Initialize the object
;     lasObj = obj_new('laslib')
;
;     Load the 5th flightline from December 5th 2003 data
;     lasObj->loadData, day='338', flightline=5, /QUIET
;
;     Select points that lies inside the bounding box.
;     Result = lasObj->getData(boundingBox=geoBox)
;
; :Keywords:
;   boundingBox : in, optional, type=dblarr(6)
;     Geographical limit to filter the data. It can be Easting and/or Northing and/or Elevation values.
;     The array need to be of one of the following format:
;       [xMax, xMin, yMax, yMin, zMax, zMin] or,
;       [zMax, zMin] or,
;       [zMax] or,
;       [zMin]
;   pointNumber : in, optional, type=long
;     PointNumber can be either one point index (long) and range of continuous points [pointMinIndex, pointMaxIndex],
;     or a collection of discrete points lonarr(n).
;   max : in, optional, type=boolean
;     Set the boundingBox value as the maximum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   min :
;     Set the boundingBox value as the minimum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   all : in, optional, type=boolean
;     If present, will return all the points of the LAS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-
Function pulsewaves::readWaves, ALL=ALL

  ; start time
  T = SYSTIME(1)
  
  openr, getDataLun, self.wvsFilePath, /get_lun, /swap_if_big_endian
  
  ; keyword /all set -> returning all the points of the LAS file
  if keyword_set(ALL) then begin
  
    self.print,1,"Formating waveform data..."
    
    ; Retriving the data packet
    plsStructure = self.initPulseRecord()
    pulseData = replicate(plsStructure, (*self.plsHeader).nPulses)
    point_lun, getDataLun, 60 ; 60 bytes is the size of the WVS file header
    readu, getDataLun, pulseData
    
    index = lindgen((*(self.plsHeader)).nPulses)
    
    if (size(pulseData))[2] ne 8 then $
      self.print,2,"Nothing return !" else $
      self.print,1,strcompress('Number of waveform records returned: ' + string((*self.plsHeader).nPulses))
    self.print,1, strcompress("Loading Time :"+string(SYSTIME(1) - T) +' Seconds')
    
  endif
  
  free_lun, getDataLun
  ; Updating data members
  self.wvsWaverec = ptr_new(pulseData)
  self.wvsWaveInd = ptr_new(index)
  
  Return, 1
  
End



Function pulsewaves::readAVLR

  ; WIP
  
End


Function pulsewaves::writePulse

; WIP

End



Function pulsewaves::getHeaderProperty, $
  ALL = ALL,$
  HEADERSIZE = HEADERSIZE,$
  OFFSETPULSE = OFFSETPULSE,$
  NPULSES = NPULSES,$
  NVLRECORDS = NVLRECORDS,$
  NAVLRECORDS = NAVLRECORDS,$
  TSCALE = TSCALE,$
  TOFFSET = TOFFSET,$
  TMIN = TMIN,$
  TMAX = TMAX,$
  XSCALE = XSCALE,$
  YSCALE = YSCALE,$
  ZSCALE = ZSCALE,$
  XOFFSET = XOFFSET,$
  YOFFSET = YOFFSET,$
  ZOFFSET = ZOFFSET,$
  XMIN = XMIN,$
  XMAX = XMAX,$
  YMIN = YMIN,$
  YMAX = YMAX,$
  ZMIN = ZMIN,$
  ZMAX = ZMAX,$
  BOUNDINGBOX = BOUNDINGBOX

if keyword_set(ALL) then return, (*self.plsHeader)
if keyword_set(HEADERSIZE) then return, (*self.plsHeader).headersize
if keyword_set(OFFSETPULSE) then return, (*self.plsHeader).offsetpulse
if keyword_set(NPULSES) then return, (*self.plsHeader).npulses
if keyword_set(NVLRECORDS) then return, (*self.plsHeader).nvlrecords
if keyword_set(NAVLRECORDS) then return, (*self.plsHeader).navlrecords
if keyword_set(TSCALE) then return, (*self.plsHeader).tscale
if keyword_set(TOFFSET) then return, (*self.plsHeader).toffset
if keyword_set(TMIN) then return, (*self.plsHeader).tmin
if keyword_set(TMAX) then return, (*self.plsHeader).tmax
if keyword_set(XSCALE) then return, (*self.plsHeader).xscale
if keyword_set(YSCALE) then return, (*self.plsHeader).yscale
if keyword_set(ZSCALE) then return, (*self.plsHeader).zscale
if keyword_set(XOFFSET) then return, (*self.plsHeader).xoffset
if keyword_set(YOFFSET) then return, (*self.plsHeader).yoffset
if keyword_set(ZOFFSET) then return, (*self.plsHeader).zoffset
if keyword_set(XMIN) then return, (*self.plsHeader).xmin
if keyword_set(XMAX) then return, (*self.plsHeader).xmax
if keyword_set(YMIN) then return, (*self.plsHeader).ymin
if keyword_set(YMAX) then return, (*self.plsHeader).ymax
if keyword_set(ZMIN) then return, (*self.plsHeader).zmin
if keyword_set(ZMAX) then return, (*self.plsHeader).zmax
if keyword_set(BOUNDINGBOX) then return, $
    [(*self.plsHeader).xmax, (*self.plsHeader).xmin,$
     (*self.plsHeader).ymax, (*self.plsHeader).ymin,$
     (*self.plsHeader).zmax, (*self.plsHeader).zmin ]

End



Function pulsewaves::getPulseIndex

  return, (*self.plspulseind)
  
End



;+
; :Description:
;    Return loaded Pulses records
;
; :Category:
; 	PULSEWAVES, READ
;
; :Return:
; 	an single structure or an array of structures
;
;	:Uses:
;		Result = plsobj.getPulses(index_number)
;
;	:Example:
;		a = obj_new('pulsewaves', inputFile = '/Path/To/File')
;		To return the 50th record
;		dum0 = a.getPulses(50)
;		To return the 100th to the 199th records
;		dum1 = a.getPulses(indgen(100)+100)
;
; :Params:
;    index : in, optional, type = 0L
;     index number of the pulse to return.
;     if a single value -> returns a structure 
;     if an array of value -> returns an array of structure
;     if not present -> returns an array of structure of structure that contains all the records
;
; :History:
;   March 2014
;    -First implementation
;   April 2014
;    - Adding index support
;    
; :Author: antoine
;-
Function pulsewaves::getPulses, index

  if N_elements(index) ne 0 then begin

    if max(index) ge (*self.plsHeader).npulses then begin
      self.print, 2, "Index out of range..."
      Return, 0
    endif else begin
      Return, (*self.plspulserec)[index]
    endelse

  endif else Return, *self.plspulserec
  
End



;+
; :Description:
;    Return loaded Waves records
;
; :Category:
;   PULSEWAVES, READ
;
; :Return:
;   an single structure or an array of structures
;
; :Uses:
;   Result = plsobj.getWaves(index_number)
;
; :Example:
;   a = obj_new('pulsewaves', inputFile = '/Path/To/File')
;   To return the 50th record
;   dum0 = a.getWaves(50)
;   To return the 100th to the 199th records
;   dum1 = a.getWaves(indgen(100)+100)
;
; :Params:
;    index : in, optional, type = 0L
;     index number of the pulse to return.
;     if a single value -> returns a structure 
;     if an array of value -> returns an array of structure
;     if not present -> returns an array of structure of structure that contains all the records
;
; :History:
;   March 2014
;    -First implementation
;   April 2014
;    - Adding index support
;    
; :Author: antoine
;-
Function pulsewaves::getWaves, index

  if N_elements(index) ne 0 then begin

    if Max(index) ge (*self.Plsheader).Npulses then begin
      self.print, 2, "Index out of range..."
      Return, 0
    endif else begin
      Return, (*self.wvswaverec)[index]
    endelse

  endif else Return, *self.wvswaverec
  
End



;+
; :Description:
;    Return loaded VLR records
;
; :Category:
;   PULSEWAVES, READ
;
; :Return:
;   an single structure or an array of structures
;
; :Uses:
;   Result = plsobj.getVlRecords(index_number)
;
; :Example:
;   a = obj_new('pulsewaves', inputFile = '/Path/To/File')
;   To return the 2th record
;   dum0 = a.getVlRecords(2)
;   To get all VLR records
;   dum1 = a.getVlRecords()
;
; :Params:
;    index : in, optional, type = 0L
;     index number of the pulse to return.
;     if a single value -> returns a structure
;     if an array of value -> returns an array of structure
;     if not present -> returns an array of structure of structure that contains all the records
;
; :History:
;   March 2014
;    -First implementation
;   April 2014
;    - Adding index support
;
; :Author: antoine
;-
Function pulsewaves::getVlRecords, index

  if N_elements(index) ne 0 then begin

    if Max(index) ge (*self.plsHeader).nvlrecords then begin
      self.print, 2, "Index out of range..."
      Return, 0
    endif else begin
      Return, (*self.plsvlrarray)[index]
    endelse

  endif else Return, *self.plsvlrarray
  
End



;+
; :Description:
;    Return loaded AVLR records
;
; :Category:
;   PULSEWAVES, READ
;
; :Return:
;   an single structure or an array of structures
;
; :Uses:
;   Result = plsobj.getAVlRecords(index_number)
;
; :Example:
;   a = obj_new('pulsewaves', inputFile = '/Path/To/File')
;   To return the 2th record
;   dum0 = a.getAVlRecords(2)
;   To get all VLR records
;   dum1 = a.getAVlRecords()
;
; :Params:
;    index : in, optional, type = 0L
;     index number of the pulse to return.
;     if a single value -> returns a structure
;     if an array of value -> returns an array of structure
;     if not present -> returns an array of structure of structure that contains all the records
;
; :History:
;   April 2014
;    -First implementation
;
; :Author: antoine
;-
Function pulsewaves::getAVlRecords, index

  if N_elements(index) ne 0 then begin

    if Max(index) ge (*self.plsHeader).navlrecords then begin
      self.print, 2, "Index out of range..."
      Return, 0
    endif else begin
      Return, (*self.Plsavlrarray)[index]
    endelse

  endif else Return, *self.Plsavlrarray

End
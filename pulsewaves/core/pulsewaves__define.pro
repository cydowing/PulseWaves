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
  plspulseInd   : ptr_new(),$
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
  if (*self.plsheader).nPulses ne 0 then dum = self.readPulses(/ALL)
  if (*self.plsheader).navlrecords ne 0 then dum = self.readAVLR()
  
  close, /ALL
  
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
    self.plspulseInd,$
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
  firstReturn       : 0US, $                      ; Duration in sampling units from the anchor point to the first recorded waveform sample
  lastReturn        : 0US, $                      ; Duration in sampling units from the anchor point to the last recorded waveform sample
  pulseDesIndex     : 0US, $                      ; To check
  intensity         : 0B, $                       ; Intensity of the pulse in DN
  classification    : 0B  $                       ; Classification of the pulse
  }
  
return, void

End



Function pulsewaves::readHeader


  ; Open the file
  Openr, 1, self.plsFilePath, /swap_if_big_endian

  ; Check if the file is a PLS file
  signature = Bytarr(16)
  Readu, 1, signature

  if String(signature) eq 'PulseWavesPulse' then begin

    self.out->print,1, 'PulseWaves file detected..."
    self.out->print,1, "Looking for version number..."
    Point_lun,1, 173
    majorVersion = 1B
    minorVersion = 1B
    Readu, 1, majorVersion
    Readu, 1, minorVersion
    self.out->print,1,Strcompress("PulseWaves Version " + String(Fix(majorVersion)) + "." + Strcompress(String(Fix(minorVersion)),/remove_all) + " detected.")
    self.out->print,1, "Initializing the header..."

    ; Closing and re-opening the file to reinitialize the pointer
    Close,1
    Openr, 1, self.plsFilePath, /swap_if_big_endian
    ; Putting file data into data member
    Readu, 1, (*self.plsheader)
    
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
  ; Sanity check of the header
  point_lun, -1, dum
  self.Out->print,1, "Sanity check of the header..."
  if dum eq (*self.plsHeader).headerSize then self.Out->print,1, "Header' sanity check passed..." else begin
    self.Out->print,2, "Header' sanity check NOT passed..."
    self.Out->print,2, "The rest of the data might be wrong..."
    self.Out->print,2, "We continue anyway to read (for now)..."
  endelse

  Close, 1

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
;   Create by Antoine Cottin, January 2012.
;   February 2014 : remodeling of the whole procedure for better results
;
; :Author:
;   Antoine Cottin
;-
Function pulsewaves::readVLR

  
    ; Init VLR Header structure
    vlrStruct = self.initplsvlr()
    
    openr, 1, self.plsFilePath, /swap_if_big_endian
    point_lun, 1, (*self.plsheader).headerSize
    
    ; This is pointer array that will hold the VLR, header/key, in reading order
    vlrArr = ptrarr(((*self.plsheader).nvlrecords) * 2)
    
    for w=0,((*self.plsheader).nvlrecords)-1 do begin
    
 
      readu, 1, vlrStruct
      print, vlrStruct
      
      ; Creating a temp file that hold the nth VLR record - one file per record
      vlrArr[w] = ptr_new(vlrStruct)
      
      case 1 of
      
        (vlrStruct.recordID ge 100) and (vlrStruct.recordID lt 356): begin
            
            self.out->print,1, "Waveform packet descriptor found"
            
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
          
            self.out->print,1,'"GeoKeyDirectoryTag Record" found'
            
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

  self.out->print,1,"Formating data..."
  
  ; Retriving the data packet
  plsStructure = self.initPulseRecord()
  pulseData = replicate(plsStructure, (*self.plsHeader).nPulses)
  point_lun, getDataLun, (*self.plsHeader).offsetPulse
  readu, getDataLun, pulseData
  
  index = lindgen((*(self.plsHeader)).nPulses)
  
  if (size(pulseData))[2] ne 8 then $
    self.out->print,2,"Nothing return !" else $
    self.out->print,1,strcompress("Number of point record(s) returned: " + string((*(self.plsHeader)).nPulses))
    
  self.out->print,1, strcompress("Time :"+string(SYSTIME(1) - T) +' Seconds')
  
endif

free_lun, getDataLun
; Updating data members
self.plspulserec = ptr_new(pulseData)
self.plspulseInd = ptr_new(index)

; As for now, returning the pulse block
Return, pulseData

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



Function pulsewaves::getPulses

  return, (*self.plspulserec)
  
End



Function pulsewaves::getVlRecords

  return, (*self.plsvlrarray)
  
End
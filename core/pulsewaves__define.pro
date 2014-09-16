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
; :History:
; 	September 2013
; 	 -First implementation
; 	March 2014
; 	 - More serious developements
;   September 2014
;    - More support on the VLR, implementing VLR 300001 <= n < 300255
;
; :Author:
;   Antoine Cottin
;   
;-
Pro pulsewaves__define

void = { pulsewaves, $
  plsFilePath   : "",$                ; Fully qualified path to the PLS file
  wvsFilePath   : "",$                ; Fully qualified path to the WVS file
  plsStrtConst  : ptr_new(),$         ; Pointer to the constant that define some structure fields
  plsHeader     : ptr_new(),$         ; Pointer to the PLS Header data
  plsVlrArray   : ptr_new(),$         ; Pointer to the Variable Length Records (in reading order - Header/Key)
  plsPulseDes   : ptr_new(),$         ; Pointer to an array of structure holding all the Pulse descriptor VLR
  plsPulseRec   : ptr_new(),$         ; Pointer to the records of the PLS file hold in the data member
  plsPulseInd   : ptr_new(),$         ; Pointer to the index of the records from the PLS file hold in plsPulseRec
  plsAvlrarray  : ptr_new(),$         ; Pointer to the Append Variable Length Records (in reading order - Header/Key)
  wvsHeader     : ptr_new(),$         ; Pointer to the header of the WVS file
  wvsWaveRec    : ptr_new(),$         ; Pointer to the records of the WVS file corresponding to the records in plsPulseRec
  wvsWaveInd    : ptr_new(),$         ; Pointer to the index corresponding to the records in plsPulseRec
  inherits consoleclass,$             ; Inherits from the consoleclass for formatted console and log ouptut
  inherits pulsewavestools $          ; Inherits from the pulsewavestools to access full-waveform processing tools
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
;    inputfile: in, required, type = string
;     This is the fully qualified path to the file
;    _extra: in, optional, type = multiple
;     This handles any parameters and/or keywords pass to the initialization procedure.
;     The first implementation of this was dedicate to handle the log mode, verbose (default), quiet (/QUIET), file (/FILE).
;     If the keyword /FILE is passed, then a LOG = '/pasth/to/file.log' is required. If parameter LOG not provided a default
;       log file will be created.
;    tools: in, optional, type = boolean
;     If set, then the pulsewavestool class will be associated with this class
;
;  :History:
;     -01/03/2014: Creation
;     -10/09/2014: Adding initialization of pulsewavestools and TOOLS keyword
;     
;  :Author:
;     Antoine Cottin
;
;-
Function pulsewaves::init, INPUTFILE = FILE, $
		CARBOMAP_TOOLS = CARBOMAP_TOOLS, $
		EXTERNAL_TOOLS = EXTERNAL_TOOLS, $
		RAPIDLASSO_API = RAPIDLASSO_API, $
		_EXTRA = CONSOLE_OPTIONS

  Compile_opt idl2
  
  ; Call consoleclass superclass Initialization method.
  dum = self->consoleclass::init(_extra = console_options)

  ; It the TOOLS keyword is set, then call pulsewavestools superclass Initialization method.
  ; Not sure this will be relevant in the near future
  if Keyword_set(carbomap_tools) then begin
      self.print, 1, "Linking pulsewavestools to pulsewaves enabling Carbomap add-on processing capability..."
      dum = self->pulsewavestools::init(Ptr_new(self))
  endif

  ; This keyword is dedicate to enable common C/C++ tools develop by the community - need more developments
  if Keyword_set(external_tools) then begin
      self.print,1,"Linking C++ external tools to pulsewaves..."
      ; To be implemented
      externalToolsPath = EXTERNAL_TOOLS
  endif

  ; Initialization of the constants for the structure definition
  dum = self.initDataConstant()
  
  ; Initialazing data members
  self.plsHeader = ptr_new(self.initplsheader())
  ;self.plspulserec = ptr_new(self.initpulserecord())
  
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



;Function pulsewaves::loadData
;
;  if (*self.plsheader).nPulses ne 0 then begin
;    self.print, 1, "Loading pulses records into data member..."
;    dum = self.readPulses(/ALL)
;    self.print, 1, "Loading waveforms records into data member..."
;    dum = self.readWaves(/ALL)
;  endif
;  
;
;  Return, 1
;
;End



;+
; :Description:
;    This function initialized some object wide constant.
;    Not sure if this is really usefull - defined from Pulsewaves_DLL
;
; :Category:
; 	GENERAL
;
; :Return:
; 	structure of constant
;
;	:Uses:
;		dum = pulsewaves::initDataConstant()
;
; :History:
; 	September 2014
; 	 9/11 - first implementation
;
; :Author:
;    Antoine Cottin
;    
;-
Function pulsewaves::initDataConstant

  void = {$
    PLS_USER_ID_SIZE                                : 16 ,$
    PLS_DESCRIPTION_SIZE                            : 64 ,$
    PLS_UNDEFINED                                   : 0 ,$
    PLS_OUTGOING                                    : 1 ,$
    PLS_RETURNING                                   : 2 ,$
    PLS_OSCILLATING                                 : 1 ,$
    PLS_LINE                                        : 2 ,$
    PLS_CONIC                                       : 3 ,$
    PLS_OPTICAL_CENTER_AND_ANCHOR_POINT_COINCIDE    : '0x0'XB ,$
    PLS_OPTICAL_CENTER_AND_ANCHOR_POINT_FLUCTUATE   : '0x8FFFFFFF'XUL ,$
    PLS_PULSE_FORMAT_0                              : 0 ,$
    PLS_PULSE_ATTRIBUTES_PULSE_SOURCE_ID_16BIT      : '0x00000001'XB ,$
    PLS_PULSE_ATTRIBUTES_PULSE_SOURCE_ID_32BIT      : '0x00000002'XB ,$
    PLS_PULSE_FORMAT_0_SIZE                         : 48 ,$
    PLS_PULSE_ATTRIBUTES_PULSE_SOURCE_ID_16BIT_SIZE : 2 ,$
    PLS_PULSE_ATTRIBUTES_PULSE_SOURCE_ID_32BIT_SIZE : 4 ,$
    PLS_EMPTY_TABLE_ENTRY                           : -2.0e+37 ,$
    PLS_TABLE_UNDEFINED                             : 0 ,$
    PLS_TABLE_INTENSITY_CORRECTION                  : 1 ,$
    PLS_TABLE_RANGE_CORRECTION                      : 2 $ ,$
  }
  
  self.plsStrtConst = ptr_new(void)
  return, 1
  
End



;+
; :Description:
;    This function returns the associated WVS file from a PLS file.
;
; :Category:
; 	GENERAL
;
; :Return:
; 	A String representing the fully qualified path to the WVS file.
;
;	:Uses:
;		Result = pulsewaves::getWaveFileName(plsfilename')
;
; :Params:
;    plsfile, in, required, type = string
;     A String representing the fully qualified path to the PLS file.
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;   
;-
Function pulsewaves::getWaveFileName, plsfile

  if strlowcase(!version.OS_NAME) eq "linux" or strlowcase(!version.OS_NAME) eq "mac os x" then spath='/' else spath='\'
  sep=strcompress(strmid(spath, 0, 1,/reverse_offset))
  path = file_dirname(plsfile)
  file = file_basename(plsfile)
  return, strcompress( path + spath + strmid(file, 0, strpos(file, '.', /reverse_search )) + '.wvs')

End




;+
; Cleanup the object. This method is call automatically using the obj_destroy method.
; This method is automatically called when the object is destroy - The user don't need
; to call this function.
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
;  :History:
;     -01/03/2014: Creation
;
;  :Author:
;     Antoine Cottin
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

  ; Destroying the consoleclass object
  self.print,1 , 'Destroying remaining objects...'
  self.print,1 , 'Bye :)'
  obj_destroy, self.out
  
End



;+
; :Description:
;    This function initialized the header structure.
;
; :Category:
; 	GENERAL
;
; :Return:
; 	A structure
;
;	:Uses:
;		Result = pulsewaves::initplsheader()
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;   
;-
Function pulsewaves::initplsheader

void = { plsheader, $
  signature       : byte('PulseWavesPulse0'), $   ; File signature
  globalPram      : 0UL, $
  fileSource      : 0UL,  $                       ; File source ID
  guid1           : 0UL, $                        ; Project ID - GUID data 1
  guid2           : 0US,  $                       ; Project ID - GUID data 2
  guid3           : 0US,  $                       ; Project ID - GUID data 3
  guid4           : bytarr(8), $                  ; Project ID - GUID data 4
  systemID        : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE), $                 ; System identifier
  softwareID      : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE), $                 ; Generating software
  day             : 0US,    $                     ; File creation day of year
  year            : 0US,    $                     ; File creation year
  versionMajor    : 1B, $                         ; Version major
  versionMinor    : 1B, $                         ; Version minor
  headerSize      : 0US,  $                       ; Header size - 352 bytes at this version
  offsetPulse     : 0ULL, $                       ; Offset to pulse data -- 352 bytes at this version
  nPulses         : 0ULL, $                       ; Number of point records
  pulseFormat     : 0UL, $                        ; Pulse format
  pulseAttrib     : 0UL, $                        ; Pulse attribute - a 16 bit pulse source ID (0x00000001) and a 32 bit pulse source ID (0x00000002)
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



;+
; :Description:
;    This function initialized the Variable Length Record structure.
;
; :Category:
;   GENERAL
;
; :Return:
;   A structure
;
; :Uses:
;   Result = pulsewaves::initplsvlr()
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;
;-
Function pulsewaves::initplsvlr

void = {plsvlr, $
  userID          : bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE), $                 ; User ID, any string, remaining characters must be set to null
  recordID        : 0UL, $                        ; ID for each vlr, define by manufacturer
  reserved        : 0Ul, $                        ; Reserved, must be set to 0
  recLengthAfter  : 0ULL, $                       ; Number of bytes contain in the vlr
  description     : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $                  ; Null terminated text description. Any characters not used must be null
  }

return, void

End



;+
; :Description:
;    This function initialized the Append Variable Length Record structure.
;
; :Category:
;   GENERAL
;
; :Return:
;   A structure
;
; :Uses:
;   Result = pulsewaves::initplsavlr()
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;
;-
Function pulsewaves::initplsavlr

  void = {plsvlr, $
    userID          : bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE), $                 ; User ID, any string, remaining characters must be set to null
    recordID        : 0UL, $                        ; ID for each vlr, define by manufacturer
    reserved        : 0Ul, $                        ; Reserved, must be set to 0
    recLengthBefore : 0ULL, $                       ; Number of bytes contain in the vlr
    description     : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $                  ; Null terminated text description. Any characters not used must be null
  }
  
  return, void
  
End



;+
; :Description:
;    This function initialized the Pulse structure record.
;
; :Category:
;   GENERAL
;
; :Return:
;   A structure
;
; :Uses:
;   Result = pulsewaves::initpulserecord()
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;
;-
Function pulsewaves::initpulserecord

void = {pulserecord, $
  gpsTime           : 0LL, $                     ; GPS time
  waveOffset        : 0LL, $                     ; Bytes offset to wave record
  anchorX           : 0L, $                      ; Anchor point of the wave
  anchorY           : 0L, $                      ; Anchor point of the wave
  anchorZ           : 0L, $                      ; Anchor point of the wave
  targetX           : 0L, $                      ; Ending point of the wave
  targetY           : 0L, $                      ; Ending point of the wave
  targetZ           : 0L, $                      ; Ending point of the wave
  firstReturn       : 0S, $                      ; Duration in sampling units from the anchor point to the first recorded waveform sample
  lastReturn        : 0S, $                      ; Duration in sampling units from the anchor point to the last recorded waveform sample
  pulseDesIndex     : 0US, $                     ; Contains information on: Pulse description index bit 0-7; Reserved bit 8-11; Edge of scan line bit 12; Scan direction bit 13; Mirror facet bit 14-15
  intensity         : 0B, $                      ; Intensity of the pulse in DN
  classification    : 0B  $                      ; Classification of the pulse
  }
  
return, void

End




Function pulsewaves::initwaverecord

keys = [$
  "extraWavesbytes",$
  "numberSegmentSampling0",$
  "DurationSampleSegmentFromSampling",$
  "SamplesSegmentfromSampling"$
  ]
;void = {waverecord, $
;  
;  }

End




;+
; :Description:
;    This function initialized the WVS file header.
;
; :Category:
;   GENERAL
;
; :Return:
;   A structure
;
; :Uses:
;   Result = pulsewaves::initwvsheader()
;
; :History:
;   -01/03/2014: Creation
;
; :Author:
;   Antoine Cottin
;
;-
Function pulsewaves::initwvsheader

void = {wvsheader, $
  signature         : bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE), $
  compression       : 0UL, $
  reserve           : bytarr(44) $
  }
  
return, void

End




Function pulsewaves::readHeader


  ; Compiling dependant procedure and function files
;  Resolve_routine, 'consoleclass__define', /COMPILE_FULL_FILE
;  out = Obj_new('consoleclass')
  

  ; Open the file
  Openr, 1, self.plsFilePath, /swap_if_big_endian

  ; Check if the file is a PLS file
  signature = bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE)
  Readu, 1, signature

self.print, 1, "Opening " + strcompress(self.plsFilePath, /REMOVE_ALL)

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
    
    self.print,1,"========================================================="
    self.print,1, "Reading file header..."
    self.print,1, Strcompress("System identifier: " + String((*self.plsheader).systemID))
    self.print,1, Strcompress("Generating software: " + String((*self.plsheader).softwareID))
    self.print,1, "Day/Year of creation: " + Strcompress(String(Fix((*self.plsheader).day)) + "/" + String(Fix((*self.plsheader).year))) 
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
    self.print,1, Strcompress("Min Max T(ime): " + String((*self.plsheader).tMin)  + " " + String((*self.plsheader).tMax))
    self.print,1, Strcompress("X Y Z scale factor: " + String((*self.Plsheader).Xscale) + " " + $
                                                       String((*self.Plsheader).Yscale) + " " + $
                                                       String((*self.Plsheader).Zscale))
    self.print,1, Strcompress("X Y Z offset factor: " + String((*self.Plsheader).Xoffset) + " " + $
                                                        String((*self.Plsheader).Yoffset) + " " + $
                                                        String((*self.Plsheader).Zoffset))
    self.print,1, Strcompress("X Y Z Minimum: " + String((*self.plsheader).xMin) + " " + $
                                                  String((*self.plsheader).yMin) + " " + $
                                                  String((*self.plsheader).zMin))                             
    self.print,1, Strcompress("X Y Z Maximum: " + String((*self.plsheader).xMax) + " " + $
                                                  String((*self.plsheader).yMax) + " " + $
                                                  String((*self.plsheader).zMax))
    
    
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

  self.print,1,"The associated waveform file is " + strcompress(self.wvsFilePath, /REMOVE_ALL)
  self.print,1, "Reading Header of the associated Waves file..."
  ; Open the file
  Openr, 1, self.wvsFilePath, /swap_if_big_endian
  self.wvsHeader = ptr_new(self.initwvsheader())
  Readu, 1, (*self.wvsHeader)
  if String((*self.wvsHeader).signature) eq 'PulseWavesWaves' then self.print, 1, "Header's signature is valid..." else begin
    self.print, 2, "Header' signature is invalid !"
  endelse
  if (*self.wvsHeader).compression eq 1 then self.print, 1, "Waveforms are compressed..." else self.print, 1, "Waveforms are not compressed..."
  self.print,1, "Header read and stored..."
  self.print,1,'========================================================='
  Return, 1
  
endif else begin
  
  
endelse

End



;+
; :Description:
;    The purpose of this method is the read and extract the Variable Length Records from
;    the header block of the PLS file.
;
; :Category:
;   PLS
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

    ; Defining a flag for the number of pulseTable
    plsTable = 0
  
     self.print,1, "Reading Variable Length Records..."
    ; Init VLR Header structure
    vlrStruct = self.initplsvlr()
    close, 1
    openr, 1, self.plsFilePath, /swap_if_big_endian
    point_lun, 1, (*self.plsheader).headerSize
    
    ; This is pointer array that will hold the VLR, header/key, in reading order
    vlrRecID = lonarr(((*self.plsheader).nvlrecords))
    vlrSt = {header:ptr_new(),key:ptr_new()}
    vlrStArr = replicate(vlrSt, ((*self.plsheader).nvlrecords))
    vlrArr = ptrarr(((*self.plsheader).nvlrecords) * 2)
    
    for w=0,((*self.plsheader).nvlrecords)-1 do begin
    
 
      readu, 1, vlrStruct
;      print, vlrStruct.recordID
      self.print,1,'========================================================='
      self.print, 1, "Variable length header record " + strcompress(string(w+1), /REMOVE_ALL) + " of " + strcompress(string(((*self.plsheader).nvlrecords)), /REMOVE_ALL)
      self.print, 1, "Reserved: " + strcompress(string(vlrStruct.reserved))
      self.print, 1, "User ID: " + strcompress(string(vlrStruct.userid))
      self.print, 1, "Record ID: " + strcompress(string(vlrStruct.recordid))
      self.print, 1, "Length after header: " + strcompress(string(vlrStruct.reclengthafter))
      self.print, 1, "Description: " + strcompress(string(vlrStruct.description))
      
      ; Creating a temp file that hold the nth VLR record - one file per record
      vlrRecID[w] = vlrStruct.recordid
      vlrStArr[w].header = ptr_new(vlrStruct)
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
            vlrStArr[w].key = ptr_new(wfDescriptor)
            
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
              
            geoKeyArray = replicate(sKeyEntry, geoKeyHeader.wNumberOfKeys)
            readu,1,geoKeyArray
            
            tempStruc = {header:geoKeyHeader, key:geoKeyArray}
            vlrStArr[w].key = ptr_new(tempStruc)
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
              reserved    : 0UL,$
              instrument  : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE),$
              serial      : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE),$
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
              description : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $
              }
            
            readu, 1, scannerKey 
            vlrStArr[w].key = ptr_new(scannerKey)
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
              sizeK       : 0UL,$           ; Size of the key
              reserved    : 0UL,$           ; Reserved
              opCentAnch  : 0L ,$           ; Optical Center to Anchor Point
              nEBytes     : 0US,$           ; Number of Extra Wave Bytes
              nSampling   : 0US,$           ; Number of Samplings
              sampleUnit  : 0. ,$           ; Sampling unit
              scanIndex   : 0UL,$           ; Scanner Index
              compression : 0UL,$           ; Compression
              description : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $
            }
            
            readu, 1, pulseKey
;            vlrArr[w+1] = ptr_new(pulseKey)
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
              reserved                : 0UL,$
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
              description : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $
            }
            
            samplingRecords = replicate(samplingKey, pulseKey.nSampling)
            readu, 1, samplingRecords
            vlrArr[w+1] = ptr_new({compositionRecord:pulseKey, samplingRecord:samplingRecords})
;            print, samplingRecords
            
            for k = 0, pulseKey.nSampling-1 do begin
              
              self.print,1,'========================================================='
              self.print,1,'Reading sampling record number ' + strcompress(string(k))
              
              if samplingRecords[k].type eq 1B then type = "OUTGOING" else type = "RETURNING"
              ; Printing the information
              self.print,1, Strcompress("Sampling type: " + type)
              self.print,1, Strcompress("Sampling channel: " + String(fix(samplingRecords[k].channel)))
              self.print,1, Strcompress("Sampling bits for duration from anchor: " + String(fix(samplingRecords[k].bitDurationFromAnchor)))
              self.print,1, Strcompress("Sampling scale for duration from anchor: : " + String(samplingRecords[k].scaleDurationFromAnchor))
              self.print,1, Strcompress("Sampling offset for duration from anchor: : " + String(samplingRecords[k].offsetDurationFromAnchor))
              self.print,1, Strcompress("Sampling bits for number of segments: " + String(fix(samplingRecords[k].bitForNSegments)))
              self.print,1, Strcompress("Sampling bits for number of samples : " + String(fix(samplingRecords[k].bitForNSamples)))
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
          
            ; This key has a size of 92 bytes
            pulseTable = {$
              sizeT       : 0UL,$           ; Size of table header
              reserved    : 0UL,$           ; Reserved
              nTables     : 0UL,$           ; Number of lookup tables
              description : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE)$     ; Description
            }
            
            readu, 1, pulseTable
            
            self.print,1,'  PULSETable ' + Strcompress(String(plsTable))

            ; Printing the information
            self.print,1, "   Number of tables: " + Strcompress(String(pulseTable.Ntables))
            self.print,1, "   Description: " + Strcompress(String(pulseTable.Description))          
          
            plsTable += 1
            
            for t = 0, pulseTable.Ntables-1 do begin
            
              pulseLookupTable = {$
                sizeLT      : 0UL,$           ; Size of table header
                reserved    : 0UL,$           ; Reserved
                nEntries    : 0UL,$           ; Number of entries in lookup tables, typically 256, 1024, 4096, or 65536
                unit        : 0US,$            ; Unit of measurement (0 = undefined, 1 = intensity correction, 2 = range correction)
                dataType    : 0B,$            ; Must be set to 8 indicating data of type float
                options     : 0B,$            ; Must be set to 0
                compression : 0UL,$           ; Must be set to 0. May be added in the future to compress large tables
                description :bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE)$      ; Description
               }
               
               readu, 1, pulseLookupTable
               
               self.print,1, "   PULSElookupTable " + Strcompress(String(t))
               
               self.print,1, "      Number of entries: " + Strcompress(String(pulseLookupTable.nEntries))
               self.print,1, "      Unit of measurement: " + Strcompress(String(pulseLookupTable.unit))
               if pulseLookupTable.dataType eq 8B then self.print,1, "      Data type: 8 ('float')"
               self.print,1, "      Description: " + Strcompress(String(pulseLookupTable.description))
               
               lookupTable = fltarr(pulseLookupTable.nEntries)
               readu, 1, lookupTable
;               self.print, 1, lookuptable
               
;               self.printLUT, 1, reform(lookupTable, 8, n_elements(lookupTable)/8), (*self.plsstrtconst).PLS_EMPTY_TABLE_ENTRY 
               
            endfor
                         
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



Function pulsewaves::createPlsPulseDescription, samplingHeader, samplingRecord


if self.plsPulseDes eq !NULL then self.plsPulseDes = ptr_new(result) else self.plsPulseDes = ptr_new([previous,result])
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
;     If present, will return all the points of the PLS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-
Function pulsewaves::readPulses, ALL=ALL

; start time
T = SYSTIME(1)

openr, getDataLun, self.plsFilePath, /get_lun, /swap_if_big_endian

; keyword /all set -> returning all the points of the PLS file
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
self.print,1,"Linking pulse data to object's data member..."
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
;     If present, will return all the points of the PLS file.
;   _ref_extra : in, optional, type=`strarr`
;     A `strarr[n]` that describ the n field(s) that need to be return.
;     If n=0 then all the fields are return.
;
;-
Function pulsewaves::readWaves, ALL=ALL

  ; start time
  T = SYSTIME(1)
  
  openr, getDataLun, self.wvsFilePath, /get_lun, /swap_if_big_endian
  
  ; keyword /all set -> returning all the points of the PLS file
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
  self.print,1,"Linking wave data to object's data member..."
  self.wvsWaverec = ptr_new(pulseData)
  self.wvsWaveInd = ptr_new(index)
  
  Return, 1
  
End



Function pulsewaves::seek_pulse, INDEX = INDEX




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
  XYZSCALE = XYZSCALE,$
  XOFFSET = XOFFSET,$
  YOFFSET = YOFFSET,$
  ZOFFSET = ZOFFSET,$
  XYZOFFSET = XYZOFFSET,$
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
if keyword_set(XYZSCALE) then return, {x:(*self.plsHeader).xscale,y:(*self.plsHeader).yscale,z:(*self.plsHeader).zscale}
if keyword_set(XYZOFFSET) then return, {x:(*self.plsHeader).xoffset,y:(*self.plsHeader).yoffset,z:(*self.plsHeader).zoffset}
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




Function pulsewaves::getCoordinates
  
  

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




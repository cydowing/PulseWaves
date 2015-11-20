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
;   May 2015
;    - Implementation of the INDEX keyword to pulsewaves::readPulses() plus some code cleaning
;    - Adding some support of AVLR event if not entirely used yet
;
; :Author:
;   Antoine Cottin
;   
;-

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
		NO_HEADER = NO_HEADER, $
		NO_VLR = NO_VLR,$
		NO_AVLR = NO_AVLR,$
		NO_GUI = NO_GUI,$
		_EXTRA = CONSOLE_OPTIONS

  Compile_opt idl2
  
  ; Getting OS information for cross-plateform compatibility
  os = os_define()
  self.sysSep = os.Sep
  self.pathRoot = File_dirname(Routine_filepath('pulsewaves__define', /either))
  self.osRoot = os.osRoot
  
  
  self.plotColor = ["r","b","g","y"]
  self.plotFlag = 0B
  
  
  ; Call consoleclass superclass Initialization method.
  dum = self->consoleclass::init(_extra = console_options)

  ; It the TOOLS keyword is set, then call pulsewavestools superclass Initialization method.
  ; Not sure this will be relevant in the near future
;  if Keyword_set(carbomap_tools) then begin
;      self.print, 1, "Linking pulsewavestools to pulsewaves enabling Carbomap add-on processing capability..."
;      dum = self->pulsewavestools::init(Ptr_new(self))
;  endif

  ; This keyword is dedicate to enable common C/C++ tools develop by the community - need more developments
  if Keyword_set(external_tools) then begin
      self.print,1,"Linking C++ external tools to pulsewaves..."
      ; To be implemented
      externalToolsPath = EXTERNAL_TOOLS
  endif

  ; Set the bit to not print the header
  if keyword_set(NO_HEADER) then begin
    self.BitNoPrint = self.BitNoPrint or '00000001'bb
  endif
  ; Set the bit to not print the vlr
  if Keyword_set(NO_VLR) then begin
    self.Bitnoprint = self.Bitnoprint or '00000010'bb
  endif
  ; Set the bit to not print the avlr  
  if Keyword_set(NO_AVLR) then begin
    self.Bitnoprint = self.Bitnoprint or '00000100'bb
  endif

  ; Initialization of the constants for the structure definition
  dum = self.initDataConstant()
  
  ; Initialazing data members
  self.plsHeader = ptr_new(self.initplsheader())
  ;self.plspulserec = ptr_new(self.initpulserecord())
  
  ;Checking that the provided file exist
  if keyword_set(INPUTFILE) eq 0 and n_elements(FILE) eq 0 then file = ''
  exist = File_test(file)
  if exist eq 1 then begin
    self.plsFilePath = file
    self.wvsFilePath = self.getWaveFileName(self.plsFilePath)
  endif else begin
    while exist ne 1 do begin
      
      if keyword_set(NO_GUI) then begin
        self.print, 3, "No INPUT_FILE provided or the file doesn't exist..."
        self.print, 3, "Please re-enter a file path string"
        newPath = ""
        read, newPath
        print, newPath
        exist = File_test(newPath)
      endif else begin
        ; If not inputfile is provided, then open a dialog pickfile
        newPath = DIALOG_PICKFILE(/READ, FILTER = '*.pls')
        exist = File_test(newPath)
      endelse
      
    endwhile
    
    self.plsFilePath = newPath
    self.wvsFilePath = self.getWaveFileName(self.plsFilePath)
  endelse
  
  ; Loading data into data members
  dum = self.readHeader()
  if (*self.plsheader).nvlrecords ne 0 then dum = self.readVLR()

  if (*self.plsheader).nPulses ne 0 then begin
    dum = self.readPulses(/ALL)
;    dum = self.readWaves(/ALL)
  endif
  if (*self.plsheader).navlrecords ne 0 then dum = self.readAVLR()
  
  close, /ALL
 
  sysIdString = self.getHeaderProperty(/SYSTEMID)
  
  case 1 of 
    
    STRMATCH(sysIdString, 'riprocess*', /FOLD_CASE): begin
      ; Pulsewaves & Riegl : FORMATORIGIN = 2, MANUFACTURER = 1
      self.FORIGIN = 2
      self.MANUTER = 1
    end
  
    STRMATCH(sysIdString, 'optechlms*', /FOLD_CASE): begin
      ; Pulsewaves & Riegl : FORMATORIGIN = 2, MANUFACTURER = 2
      self.FORIGIN = 2
      self.MANUTER = 2
    end
  
    else: begin
      self.FORIGIN = 2
      self.MANUTER = 2
    end

endcase
  
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
;  
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
  navlrecords     : 0L,   $                       ; Number of Append Variable Length Records
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
    recordID        : 0UL, $                        ; ID for each avlr, define by manufacturer
    reserved        : 0UL, $                        ; Reserved, must be set to 0
    recLengthBefore : 0ULL, $                       ; Number of bytes contain in the avlr
    description     : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $                  ; Null terminated text description. Any characters not used must be null
  }
  
;  keys = ['userID', 'recordID', 'reserved', 'recLengthBefore', 'description']
;  values = List(bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE), 0UL, 0UL,0ULL, bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE))
;  void = orderedhash(keys, values)

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
  
  ; Checking if the keyword NO_HEADER has been passed - if so, disable the printing
  if self.bitnoprint and '00000001'bb eq 1 then dum = self.setMode(2)
  
  ; Open the file
  Openr, rLun, self.plsFilePath, /swap_if_big_endian, /get_lun

  ; Check if the file is a PLS file
  signature = bytarr((*self.Plsstrtconst).PLS_USER_ID_SIZE)
  Readu, rLun, signature

self.print, 1, "Opening " + strcompress(self.plsFilePath, /REMOVE_ALL)

  if String(signature) eq 'PulseWavesPulse' then begin

    self.print,1, 'PulseWaves file detected..."
    self.print,1, "Looking for version number..."
    Point_lun, rLun, 173
    majorVersion = 1B
    minorVersion = 1B
    Readu, rLun, majorVersion
    Readu, rLun, minorVersion
    self.print,1,Strcompress("PulseWaves Version " + String(Fix(majorVersion)) + "." + Strcompress(String(Fix(minorVersion)),/remove_all) + " detected.")
    self.print,1, "Initializing the header..."

    ; Closing and re-opening the file to reinitialize the pointer
    Close, rLun
    Openr, rLun, self.plsFilePath, /swap_if_big_endian, /get_lun
    ; Putting file data into data member
    Readu, rLun, (*self.plsheader)
    
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
    point_lun, -rLun, dum
    self.print,1, "Sanity check of the header..."
    if dum eq (*self.plsHeader).headerSize then self.print,1, "Header' sanity check passed..." else begin
      self.print,2, "Header' sanity check NOT passed..."
      self.print,2, "The rest of the data might be wrong..."
      self.print,2, "We continue anyway to read (for now)..."
    endelse

  Close, rLun

  self.print,1,"The associated waveform file is " + strcompress(self.wvsFilePath, /REMOVE_ALL)
  self.print,1, "Reading Header of the associated Waves file..."
  if file_test(self.wvsFilePath) then begin
    ; Open the file
    Openr, rLun, self.wvsFilePath, /swap_if_big_endian, /get_lun
    self.wvsHeader = ptr_new(self.initwvsheader())
    Readu, rLun, (*self.wvsHeader)
    if String((*self.wvsHeader).signature) eq 'PulseWavesWaves' then self.print, 1, "Header's signature is valid..." else begin
      self.print, 2, "Header' signature is invalid !"
    endelse
    if (*self.wvsHeader).compression eq 1 then self.print, 1, "Waveforms are compressed..." else self.print, 1, "Waveforms are not compressed..."
    self.print,1, "Header read and stored..."
    self.printsep
  endif else begin
    self.print, 2, "Associated Waves file not found!"
    self.print, 2, "Skipping this part but you won't be able to do anything interesting..."
  endelse
  
  ; Checking if the keyword NO_HEADER has been passed - if so, disable the printing
  if self.Bitnoprint and '00000001'bb eq 1 then dum = self.restoreMode()
  
  free_lun, rLun
  
  Return, 1
  
endif else begin
  
  
endelse

 free_lun, rLun

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

    ; Checking if the keyword NO_HEADER has been passed - if so, disable the printing
    if self.Bitnoprint and '00000010'bb eq 2 then dum = self.setMode(2)
  
    ; Defining a flag for the number of pulseTable
    plsTable = 0
  
    self.print,1, "Reading Variable Length Records..."
    ; Init VLR Header structure
    vlrStruct = self.initplsvlr()

    openr, rLun, self.plsFilePath, /swap_if_big_endian, /get_lun
    point_lun, rLun, (*self.plsheader).headerSize
    
    ; This is pointer array that will hold the VLR, header/key, in reading order
    vlrRecID = lonarr(((*self.plsheader).nvlrecords))
;    vlrSt = {header:ptr_new(),key:ptr_new()}
;    vlrStArr = replicate(vlrSt, ((*self.plsheader).nvlrecords))
    vlrArr = ptrarr(((*self.plsheader).nvlrecords) * 2)
    
    for w=0,((*self.plsheader).nvlrecords)-1 do begin
    
 
      readu, rLun, vlrStruct
;      print, vlrStruct.recordID
      self.printsep
      self.print, 1, "Variable length header record " + strcompress(string(w+1), /REMOVE_ALL) + " of " + strcompress(string(((*self.plsheader).nvlrecords)), /REMOVE_ALL)
      self.print, 1, "Reserved: " + strcompress(string(vlrStruct.reserved))
      self.print, 1, "User ID: " + strcompress(string(vlrStruct.userid))
      self.print, 1, "Record ID: " + strcompress(string(vlrStruct.recordid))
      self.print, 1, "Length after header: " + strcompress(string(vlrStruct.reclengthafter))
      self.print, 1, "Description: " + strcompress(string(vlrStruct.description))
      
      ; Creating a temp file that hold the nth VLR record - one file per record
      vlrRecID[w] = vlrStruct.recordid
;      vlrStArr[w].header = ptr_new(vlrStruct)
      vlrArr[w*2] = ptr_new(vlrStruct)

      
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
            readu, rLun, wfDescriptor
            
            vlrArr[(w*2)+1] = ptr_new(wfDescriptor)
;            vlrStArr[w].key = ptr_new(wfDescriptor)
            
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
          
            readu, rLun,geoKeyHeader
            
            sKeyEntry = {$
              wKeyID:0US,$
              wTIFFTagLocation:0US,$
              wCount:0US,$
              wValueOffset:0US$
              }
              
            geoKeyArray = replicate(sKeyEntry, geoKeyHeader.wNumberOfKeys)
            readu,rLun,geoKeyArray
            
            tempStruc = {header:geoKeyHeader, key:geoKeyArray}
;            vlrStArr[w].key = ptr_new(tempStruc)
            vlrArr[(w*2)+1] = ptr_new(tempStruc)
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
            
            readu, rLun, scannerKey 
;            vlrStArr[w].key = ptr_new(scannerKey)
            vlrArr[(w*2)+1] = ptr_new(scannerKey)
            
            
            
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
              compression : 0UL,$           ; Compression
              scanIndex   : 0UL,$           ; Scanner Index
              description : bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE) $
            }
            
            readu, rLun, pulseKey
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
            readu, rLun, samplingRecords
            vlrArr[(w*2)+1] = ptr_new({compositionRecord:pulseKey, samplingRecord:samplingRecords})
;            print, samplingRecords
            
            for k = 0, pulseKey.nSampling-1 do begin
              
              self.printsep
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
              
              if k eq pulseKey.nSampling-1 then self.printsep
              
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
            
            readu, rLun, pulseTable
            
            self.print,1,'  PULSETable ' + Strcompress(String(plsTable), /REMOVE_ALL)

            ; Printing the information
            self.print,1, "   Number of tables: " + Strcompress(String(pulseTable.Ntables), /REMOVE_ALL)
            self.print,1, "   Description: " + Strcompress(String(pulseTable.Description))          
          
            plsTable += 1
            
            for t = 0, pulseTable.Ntables-1 do begin
            
              pulseLookupTable = {$
                sizeLT      : 0UL,$           ; Size of table header
                reserved    : 0UL,$           ; Reserved
                nEntries    : 0UL,$           ; Number of entries in lookup tables, typically 256, 1024, 4096, or 65536
                unit        : 0US,$           ; Unit of measurement (0 = undefined, 1 = intensity correction, 2 = range correction)
                dataType    : 0B,$            ; Must be set to 8 indicating data of type float
                options     : 0B,$            ; Must be set to 0
                compression : 0UL,$           ; Must be set to 0. May be added in the future to compress large tables
                description :bytarr((*self.Plsstrtconst).PLS_DESCRIPTION_SIZE)$      ; Description
               }
               
               readu, rLun, pulseLookupTable
               
               self.print,1, "   PULSElookupTable " + Strcompress(String(t), /REMOVE_ALL)
               
               self.print,1, "      Number of entries: " + Strcompress(String(pulseLookupTable.nEntries), /REMOVE_ALL)
               self.print,1, "      Unit of measurement: " + Strcompress(String(pulseLookupTable.unit), /REMOVE_ALL)
               if pulseLookupTable.dataType eq 8B then self.print,1, "      Data type: 8 ('float')"
               self.print,1, "      Description: " + Strcompress(String(pulseLookupTable.description))
               
               lookupTable = fltarr(pulseLookupTable.nEntries)
               readu, rLun, lookupTable
;               self.print, 1, lookuptable

               if t eq 0 then void = {LUTheader:pulseLookupTable,LUTTable:lookupTable} else void = [void, {LUTheader:pulseLookupTable,LUTTable:lookupTable}]


               self.printLUT, 1, lookupTable, (*self.plsstrtconst).PLS_EMPTY_TABLE_ENTRY 
               
            endfor
            
            vlrArr[(w*2)+1] = ptr_new({compositionRecord:pulseTable, tableRecord:void})
                         
          end
    
        else: begin
        
            generic = bytarr(vlrStruct.recLengthAfter)
            readu, rLun,generic

            vlrArr[(w*2)+1] = ptr_new(generic)
          
        end
        
  endcase
 
endfor

close, rLun

self.Plspulsedes = ptr_new(vlrRecID)
self.plsvlrarray = ptr_new(vlrArr)

; Checking if the keyword NO_HEADER has been passed - if so, disable the printing
if self.Bitnoprint and '00000010'bb eq 2 then dum = self.restoreMode()

return, 1

End



Function pulsewaves::createPlsPulseDescription, samplingHeader, samplingRecord


if self.plsPulseDes eq !NULL then self.plsPulseDes = ptr_new(result) else self.plsPulseDes = ptr_new([previous,result])
return, 1

End



;+
; This function will read the pulse and put the result into the object data member
; Filtering options are available to filter the pulses. Note that for the moment the
; filtering is based on the Anchor point of the pulses, meaning that the selected pulses
; might end up outside the BOUNDINGBOX.
;
; :Categories:
;   GENERAL
;
; :Returns:
;   An bit value to specify that everything went well (1) or not (0).
;
; :Uses:
;   Result=Obj->readPulses()
;
; :Examples:
;
;     Define the bounding coordinate for the selection
;     geoBox = [488401.968647,487901.968647,235421.265389,234921.265386]
;
;     Initialize the object
;     plsObj = obj_new('pulsewaves')
;
;     Select points that lies inside the bounding box.
;     Result = plsObj->readPulses(BOUNDINGBOX=geoBox)
;
; :Keywords:
;   BOUNDINGBOX : in, optional, type=dblarr(6)
;     Geographical limit to filter the data. It can be Easting and/or Northing and/or Elevation values.
;     The array need to be of one of the following format:
;       [xMax, xMin, yMax, yMin, zMax, zMin] or, [xMax, xMin, yMax, yMin]
;       [xMax, xMin], /XBOUND or [yMax, yMin], /YBOUND or [zMax, zMin], /ZBOUND
;       [zMax], /MAX, or [zMin], /MIN
;   BETAPOINTSCLOUD : in, optional, type=bool
;     if setup, then a raw lastreturn points cloud is compute as used a base for geographic filtering   
;   INDEX : in, optional, type=long
;     INDEX can be either one point index (long) and range of continuous points [pointMinIndex, pointMaxIndex],
;     or a collection of discrete points lonarr(n).
;   XBOUND, YBOUND or ZBOUND: in, optional, type=boolean
;     Set the axe to use (X,Y or Z) for the filtering
;   MAX : in, optional, type=boolean
;     Set the boundingBox value as the maximum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   MIN :
;     Set the boundingBox value as the minimum (cutoff) elevation value.
;     This is a required Keyword if the boundingBox has only one value.
;   ALL : in, optional, type=boolean
;     If present, will return all the points of the PLS file.
;   CURRENT : in, NOT IN USE NOW
;   NO_SAVE : in, optional, type=boolean
;     if set, the pulseBlock is not store in the object data member, but it's return to the caller
;
;-
Function pulsewaves::readPulses, INDEX = INDEX, CURRENT = CURRENT, ALL=ALL, $
            BOUNDINGBOX = BOUNDINGBOX, BETAPOINTSCLOUD = BETAPOINTSCLOUD, $
            XBOUND = XBOUND, YBOUND = YBOUND, ZBOUND = ZBOUND, $
            MAX = MAX, MIN = MIN, $
            OUTPUTID = OUTPUTID, $
            NO_SAVE = NO_SAVE

; start time
T = SYSTIME(1)

openr, getDataLun, self.plsFilePath, /get_lun, /swap_if_big_endian

; keyword /all set -> returning all the points of the PLS file
if Keyword_set(INDEX) then begin
  
  case 1 of
    n_elements(INDEX) eq 1: begin
      point_lun, getDataLun, (*self.plsHeader).offsetPulse + (INDEX * (*self.plsHeader).pulseSize)
      pulseData = self.initpulserecord()
      readu, getDataLun, pulseData
    end
    
    N_elements(INDEX) eq 2: begin
      Point_lun, getDataLun, (*self.Plsheader).Offsetpulse + (INDEX[0] * (*self.Plsheader).Pulsesize)
      tempPulseData = self.initpulserecord()
      pulseData = replicate(tempPulseData, INDEX[1]-INDEX[0])
      Readu, getDataLun, pulseData
      INDEX = [INDEX[0]:INDEX[1]]
    end
    
    N_elements(INDEX) gt 2: begin
      dum = self.readPulses(/ALL, /NO_SAVE)
      pulseData = dum[INDEX]
    end
    
    Else:
    
  endcase

endif


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

; If the keyword BOUNDING box is pass
if keyword_set(BOUNDINGBOX) then begin
  
  if keyword_set(BETAPOINTSCLOUD) then begin
    tpc = self.getRawCoordinates(/LASTRETURN)
    x = tpc.x()
    y = tpc.y()
    z = tpc.z()
  endif else begin
    ; Checking is the data has been loaded already
    if not ptr_valid(self.plsPulseRec) then pulseBlock = self.readPulses(/ALL) else pulseBlock = self.readPulses(/ALL, /NO_SAVE)
    ;if self.plsPulseRec eq !NULL then pulseBlock = self.readPulses(/ALL, /NO_SAVE) else pulseBlock = *self.plsPulseRec
    x = ( pulseBlock.anchorX * (*self.plsheader).xscale ) + (*self.plsheader).xoffset
    y = ( pulseBlock.anchorY * (*self.plsheader).yscale ) + (*self.plsheader).yoffset
    z = ( pulseBlock.anchorZ * (*self.plsheader).zscale ) + (*self.plsheader).zoffset
  endelse
 
  
  ; Determining the size of the boundingbox
  nbbox = n_elements(BOUNDINGBOX)
  
  case 1 of
    nbbox eq 6: begin
                  
                  self.print, 1,"Filtering data by coordinates and height..."
                  index = Where((x le BOUNDINGBOX[0]) and (x ge BOUNDINGBOX[1]) and $
                                (y le BOUNDINGBOX[2]) and (y ge BOUNDINGBOX[3]) and $
                                (z le BOUNDINGBOX[4]) and (z ge BOUNDINGBOX[5]), $
                                indexCount, /NULL)
                  
                  if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
                  
                end
                
    nbbox eq 4: begin
      
                  self.print, 1,"Filtering data by coordinates and height..."
                  index = Where((x le BOUNDINGBOX[0]) and (x ge BOUNDINGBOX[1]) and $
                                (y le BOUNDINGBOX[2]) and (y ge BOUNDINGBOX[3]), $
                                indexCount, /NULL)
            
                  if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
      
      
                end
                
    nbbox eq 2: begin
                  
                  case 1 of
                  ; If XBOUND is set => keeping all points between these two values
                  keyword_set(XBOUND): begin
                    
                                         self.print, 1,"Filtering data by X/Easting dimension..."
                                         index = Where((x le BOUNDINGBOX[0]) and (x ge BOUNDINGBOX[1]), $
                                                       indexCount, /NULL)
                                         if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
                    
                                       end
                  ; If YBOUND is set => keeping all points between these two values                      
                  keyword_set(YBOUND): begin

                                         self.print, 1,"Filtering data by Y/Northing dimension..."
                                         index = Where((y le BOUNDINGBOX[0]) and (y ge BOUNDINGBOX[1]), $
                                                        indexCount, /NULL)
                                         if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
                 
                                       end
                   ; If ZBOUND is set => keeping all points between these two values
                   keyword_set(ZBOUND): begin

                                         self.print, 1,"Filtering data by Z/Height dimension..."
                                         index = Where((z le BOUNDINGBOX[0]) and (z ge BOUNDINGBOX[1]), $
                                           indexCount, /NULL)
                                         if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
  
                   end
                  ; If no associated keyword is set, then the altitude is considered and
                  ; all the points lying within the two altitude are kept                      
                  ELSE: begin
                    
                          self.print, 1,"No Keyword set -> Filtering data by Z/Height dimension..."
                          index = Where((z le BOUNDINGBOX[0]) and (z ge BOUNDINGBOX[1]), $
                            indexCount, /NULL)
                          if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL

                         end
                         
                  endcase

                end
                
                
    nbbox eq 1: begin

                  case 1 of
                    ; If MAX is set => keeping all points under this cutoff value
                    ; The cutoff value is inclusive (gt)
                    keyword_set(MAX): begin
                      
                                        self.print, 1,"Applying maximum height threshold..."
                                        index = Where(z le BOUNDINGBOX, indexCount, /NULL)
                                        if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL
            
                    end
                    ; If MIN is set => keeping all points above this cutoff value
                    ; The cutoff value is inclusive (lt)
                    keyword_set(MIN): begin
            
                                        self.print, 1,"Applying minimum height threshold..."
                                        index = Where(z ge BOUNDINGBOX, indexCount, /NULL)
                                        if index ne !NULL then pulseData = pulseBlock[index] else pulseData = !NULL

                    end
                    ELSE:
                  Endcase
      
                end
      
    ELSE:
      
  endcase

if size(pulseData, /TYPE) ne 8 then $
  self.print, 2, "Nothing to return..." $
else self.print, 1, strcompress("Number of pulse records returned by selection: " + string(n_elements(pulseData)))

endif

self.print,1, Strcompress("Time :"+String(Systime(1) - T) +' Seconds')

free_lun, getDataLun

; Updating data members - if NO_SAVE keyword not set
; If NO_SAVE is set, then the function will return the array of structure pulseData
if keyword_set(NO_SAVE) ne 1 then begin
  
  self.print,1,"Saving pulse data to object's data member..."
  self.plspulserec = ptr_new(pulseData)
  self.plspulseInd = ptr_new(index)
  self.plspulseIndSel = ptr_new(index)
  Return, 1
  
endif else begin
  
  if keyword_set(OUTPUTID) then begin
    self.print,1,"Returning data index to caller..."
    Return, index
  endif else begin
    self.print,1,"Returning pulse data to caller..."
    Return, pulseData
  endelse

endelse

End



;+
; This function the waves associated to the pulses read from the previous pulsewaves::readPulses().
; The return waves are store in the object's data member. To retreive the waves, user needs
; to call pulsewaves::getPulses()
;
; :Categories:
;   GENERAL
;
; :Returns:
;   An bit value to specify that everything went well (1) or not (0).
;
; :Uses:
;   Result=Obj->readWaves()
;
;
; :Keywords:
;
;-
Function pulsewaves::readWaves, $
                        NO_PLOT = NO_PLOT, $
                        _EXTRA = ex

;  close, getDataLun, /FORCE
  
  ; start time
  T = SYSTIME(1)
  
  ; Some constant for the plotting
  plotColor = ["r","b","g","y"]
  plotFlag = 0B
  returnPulse = {returnPulse, n:ptr_new(), pulse:ptr_new(), durationFromAnchor:ptr_new(), lut:ptr_new()}
;  free_lun, getDataLun
  openr, getDataLun, self.wvsFilePath, /get_lun, /swap_if_big_endian
  ; Retriving the wave data packet
  plsStructure = self.initWaveRecord()
  
  self.printsep
    
    nIndex = N_elements(*(self.Plspulseindsel))
    
    case 1 of
      
      nIndex eq 1 : begin
        
            ; Need to get the wavefrom bytes offset
;            byteOffset = (((*self.plsPulseRec)[0]).waveOffset)
            byteOffset = (((*self.plsPulseRec)[*(self.Plspulseindsel)]).waveOffset)
    
            ; Need to format the waveform data block based on Pulse Descriptor information
            ; Getting the pulse descriptor value
;            pDes = ((*self.plsPulseRec)[0]).pulseDesIndex
            pDes = ((*self.plsPulseRec)[*self.Plspulseindsel]).pulseDesIndex
            descriptorVal = (pDes AND '11111111'bb) + 200000
            
            ; Retreiving the corresponding VLR
            vlr = self.getVLRecords(RECORDID = descriptorVal)    
            
            ; Because the pulse can be fragmented it might not be possible to read the pulse as a block
            ; Checking pulse descriptor for information
            
            ; Reading the Number of Extra Waves Bytes information
            extraBytes = ((*vlr[1]).(0)).(3)
            movingTo = byteOffset + extraBytes
            ; Read the waveform block
            self.print, 1, "Waves block #" + Strcompress(string(*self.Plspulseindsel),/REMOVE_ALL) + " is located at byte " + Strcompress(string(movingTo),/REMOVE_ALL)
            self.printsep
            Point_lun, getDataLun, movingTo       

            ; Getting information for all the Sampling Records
            pulseType = ((*vlr[1]).(1)).(2)
            plsBtDura = ((*vlr[1]).(1)).(5)
            pulseScal = ((*vlr[1]).(1)).(6)
            pulseOffs = ((*vlr[1]).(1)).(7)
            plsBtNSeg = ((*vlr[1]).(1)).(8)
            plsBtNSam = ((*vlr[1]).(1)).(9)
            pulseNSeg = ((*vlr[1]).(1)).(10)
            pulseNSam = ((*vlr[1]).(1)).(11)
            bitsPerSample = ((*vlr[1]).(1)).(12)
            pulseLUTN = ((*vlr[1]).(1)).(13)
            nSampling = N_elements(pulseType)
            self.printsep
            self.print, 1, "The wave block has Pulse Descriptor #" + Strcompress(string(descriptorVal),/REMOVE_ALL)
            self.print, 1, "The wave block has " + Strcompress(string(nSampling),/REMOVE_ALL) + " sampling..."
            
               
            ; Reading for each pulse the information            
            for p = 0, nSampling-1 do begin

              self.printsep
              
              if pulseType[p] eq 1 then self.print, 1, Strcompress("The pulse is OUTGOING...") else self.print, 1, Strcompress("The pulse is RETURNING...") 
              ; Number of Segments in Sampling 0:
              ; Exist only if “Bits for Number of Segments” in Sampling != 0 => Specifies the Number of Segments in this Sampling.
              ; If “Bits for Number of Segments” == 0 => Number of Segment = constant and specified in Number of Segments in Sampling Record
                            
              if plsBtNSeg[p] eq 0 then begin
                
                self.print, 1, "Pulse has a fixed segmentation..."
                pulseNumberSegment = pulseNSeg[p]
                self.print, 1, "Number of segment(s) in Sampling #" + Strcompress(string(p+1),/REMOVE_ALL) + " : " + Strcompress(string(pulseNumberSegment),/REMOVE_ALL) + "..."
                
              endif else begin
                
                self.print, 1, 'Pulse has a variable segmentation..."
                self.print, 1, 'Reading information from file...'
                pulseNumberSegment = self.fieldDataCreator(plsBtNSeg[p])
                readu, getDataLun, pulseNumberSegment
                self.print, 1, Strcompress("Number of segment(s) in Sampling #" + string(p+1) + ": " + string(pulseNumberSegment) + "...")
                
              endelse
              
;              self.printsep
              
              for seg = 0,pulseNumberSegment-1 do begin
                
                ; Duration from Anchor for Segment k of Sampling m:
                ; Exist only if “Bits for Duration from Anchor” in Sampling Record != 0
                ; if “Bits for Duration from Anchor” == 0 in Sampling Record => then duration == 0 => anchor point coincide with first Sample of Sampling
                if plsBtDura[p] eq 0 then begin
                
  ;                self.print, 1, 'Bits for Duration from Anchor = 0'
  ;                  
                  self.print, 1, 'The first sample of the pulse coincides with the Anchor Point...'
                  durationFromAnchor = 0.
                  dFAnchor = 0
                  
                endif else begin
                
                  self.print, 1, "Pulse has a variable segmentation..."
                  self.print, 1, "Reading information from file..."
                  durationFromAnchor = self.fieldDataCreator(plsBtDura[p])
                  Readu, getDataLun, durationFromAnchor
                  self.print, 1, Strcompress("Duration from Anchor: " + string(durationFromAnchor) + "...")
                  self.print, 1, Strcompress("Scale & offset: " + String(pulseScal[p]) + "  " + String(pulseOffs[p]) )
                  dFAnchor = (durationFromAnchor *pulseScal[p]) + pulseOffs[p]
                  self.print, 1, Strcompress("Final duration from anchor: " + String(dFAnchor))
                  
                endelse
                
;                for s = 0, pulseNumberSegment-1 do begin
                  
                  ; Number of Samples in Segment k from Sampling m:
                  ; Exist if “bits for Number of Samples” in Sampling Record is non-zero => Number of Samples in the next Segment.
                  ; If Sampling Record is == 0 => Number of Sample is fixed and is == “Number of Samples” in Sampling Record
                  if plsBtNSam[p] eq 0 then begin
                    
                    self.print, 1, "Pulse has a fixed sampling..."
                    pulseNumberSample = pulseNSam[p]
                    self.print, 1, Strcompress("Number of samples in the Segment: " + String(pulseNumberSample) + "...")
                    pulseNSam
                    
                  endif else begin
    
                    self.print, 1, "Pulse has a variable sampling..."
                    self.print, 1, 'Reading information from file...'
                    pulseNumberSample = self.fieldDataCreator(plsBtNSam[p])
                    Readu, getDataLun, pulseNumberSample
                    self.print, 1, "Number of samples in Segment #" + Strcompress(String(seg+1),/REMOVE_ALL) + " of Sampling #" + Strcompress(String(p+1),/REMOVE_ALL) + ": " + Strcompress(string(pulseNumberSample),/REMOVE_ALL)
                    
                  endelse

                  ; formating the waves array based on the data type and reading the waves
                  case 1 of
                    bitsPerSample[p] eq 8: waves = bytarr(pulseNumberSample)
                    bitsPerSample[p] eq 16: waves = intarr(pulseNumberSample)
                    bitsPerSample[p] eq 32: waves = lonarr(pulseNumberSample)
                    else: waves = bytarr(pulseNumberSample)
                  endcase
                  
                  Readu, getDataLun, waves
                  
                  self.print, 1, "Reading Waves of Segment #" + Strcompress(String(seg+1),/REMOVE_ALL) + " of Sampling #" + Strcompress(String(p+1),/REMOVE_ALL)
                  
                  ; Getting the lookup table in VLR
                  LUT = self.getVLRecords(RECORDI = (pulseLUTN[p] + 300000))
                  ; To print the table - n being the table
                  ; ((*lut[n]).(1)).(1)
                  ;            IDL> String((((*lut[2]).(1)).(0)).(7))
                  ;            amplitude conversion table for low channel
                  ;            IDL> String((((*lut[3]).(1)).(0)).(7))
                  ;            amplitude conversion table for high channel
                  
                  ; check here is the LUT is present, if not, then just create a fake one full of 1's
                  if size(LUT, /TYPE) eq 0 then begin
                    case 1 of
                      bitsPerSample[p] eq 8: newLUT = indgen(256, /BYTE)
                      bitsPerSample[p] eq 16: newLUT = indgen(65535, /UINT)
                      bitsPerSample[p] eq 32: newLUT = indgen(4294967296, /ULONG)
                      else: 
                    endcase
                  endif else newLUT = (((*lut[1]).(1)).(1))

                  ; Check if something has been pass to ex if so set up the correct values
                  if size(ex, /TYPE) eq 0 then begin
                    FTON = self.Forigin
                    MFTR = self.Manuter
                  endif else begin
                    FTON = ex.FORMATORIGIN
                    MFTR = ex.MANUFACTURER
                  endelse
                  
;                  tempPulseClass = waveformclass(WAVE=waves, NSAMPLES = pulseNumberSample, DFA = dFAnchor, LUT = (((*lut[1]).(1)).(1)), FORMATORIGIN = FTON, MANUFACTURER = MFTR, SEGMENTNUMBER = p+1)
                  tempPulseClass = waveformclass(WAVE=waves, NSAMPLES = pulseNumberSample, DFA = dFAnchor, LUT = newLUT, FORMATORIGIN = FTON, MANUFACTURER = MFTR, SEGMENTNUMBER = p+1)

                  if not keyword_set(NO_PLOT) then begin
                    
                       if p eq 0 then begin
                          dum1 = tempPulseClass.plotwave()
                       endif else begin
                          dum2 = tempPulseClass.plotwave(/OVERPLOT)
                       endelse
                    
                  endif
                  
                  ; Saving pulse information into a structure that will be return
                  if p eq 0 then begin
                     retPulse = tempPulseClass
                  endif else begin
                     retPulse = [retPulse, tempPulseClass] 
                  endelse
                
                self.printsep
                
              endfor
                
            endfor
            
          end
             
      nIndex eq 2 : begin

          end
           
     nIndex gt 2 : begin

          end
             
    endcase
  
;    self.print,1,"Formating waveform data..."
;    
;    
;    pulseData = replicate(plsStructure, (*self.plsHeader).nPulses)
;    point_lun, getDataLun, 60 ; 60 bytes is the size of the WVS file header
;    readu, getDataLun, pulseData
;    
;    index = lindgen((*(self.plsHeader)).nPulses)
    
    if size(returnPulse, /TYPE) ne 8 then $
      self.print,2,"Nothing return !" else $
      self.print,1,strcompress('Returning waveform of pulse: ')
    self.print,1, strcompress("Loading Time :"+string(SYSTIME(1) - T) +' Seconds')
    

  close, getDataLun 
  free_lun, getDataLun, EXIT_STATUS=variable, /FORCE
;  print, getDataLun, variable
  
;  ; Updating data members
  self.print,1,"Linking wave data to object's data member..."
  self.wvsWaverec = ptr_new(retPulse)
;  self.wvsWaveInd = ptr_new(index)
  
  Return, *self.wvsWaverec

  
End



Function pulsewaves::plotWaves, p, lut, waves

;if p eq 0 then begin
;  ;if n_elements(waves) gt 1 and plotFlag eq 0 then begin
;  ;plt = plot((((*lut[2]).(1)).(1))[waves], color=colarray[plotFlag])
;;  plt = plot(waves, color=(self.plotColor)[self.plotFlag], YRANGE = [0., Max(waves)])
;  self.plotFlag += 1B
;  ;endif
;endif else begin
;  ;                    pgt = plot((((*lut[2]).(1)).(1))[waves], color=(self.plotColor)[plotFlag], /OVERPLOT)
;  plt = plot((((*lut[1]).(1)).(1))[waves], color=(self.plotColor)[self.plotFlag], /OVERPLOT)
;  self.plotFlag += 1B
;endelse
  
return, 1

End



Function pulsewaves::fieldDataCreator, bitSize

case bitSize of
  8 : data = 0B
  16: data = 0
  32: data = 0L
  ELSE:
endcase

return, data 

end



Function pulsewaves::seek_pulse, INDEX = INDEX




End



Function pulsewaves::readAVLR

  ; Checking if the keyword NO_AVLR has been passed - if so, disable the printing
  if self.Bitnoprint and '00000100'bb eq 4 then dum = self.setMode(2)
  
    ; WIP
  ; Getting file size and pointing at the end of the file
  fInfo = file_info(self.plsFilePath)
  
  self.print,1, "Reading Append Variable Length Records..."
  
  ; Init VLR Header structure
  avlrStruct = self.initplsavlr()
  
  openr, ralun, self.plsFilePath, /GET_LUN, /SWAP_IF_BIG_ENDIAN
  point_lun, ralun, (fInfo.size)-1
   

  ; This is pointer array that will hold the VLR, header/key, in reading order
  avlrRecID = Lonarr(((*self.Plsheader).Navlrecords))
  ;    vlrSt = {header:ptr_new(),key:ptr_new()}
  ;    vlrStArr = replicate(vlrSt, ((*self.plsheader).nvlrecords))
  avlrArr = Ptrarr(((*self.Plsheader).Navlrecords) * 2)

  for w=0,((*self.Plsheader).Navlrecords)-1 do begin


    Readu, ralun, avlrStruct
    ;      print, vlrStruct.recordID
    self.printsep
    self.print, 1, "Variable length header record " + Strcompress(String(w+1), /REMOVE_ALL) + " of " + Strcompress(String(((*self.Plsheader).Nvlrecords)), /REMOVE_ALL)
    self.print, 1, "Reserved: " + Strcompress(String(vlrStruct.Reserved))
    self.print, 1, "User ID: " + Strcompress(String(vlrStruct.Userid))
    self.print, 1, "Record ID: " + Strcompress(String(vlrStruct.Recordid))
    self.print, 1, "Length after header: " + Strcompress(String(vlrStruct.Reclengthafter))
    self.print, 1, "Description: " + Strcompress(String(vlrStruct.Description))

    ; Creating a temp file that hold the nth VLR record - one file per record
    vlrRecID[w] = vlrStruct.Recordid
    ;      vlrStArr[w].header = ptr_new(vlrStruct)
    vlrArr[w*2] = Ptr_new(vlrStruct)

  endfor
  
  
  
  ; restoring previous console mode
  if self.Bitnoprint and '00000100'bb eq 4 then dum = self.restoreMode()
  
End


Function pulsewaves::writePulse

; WIP

End



Function pulsewaves::getHeaderProperty, $
  ALL = ALL,$
  SYSTEMID = SYSTEMID, $
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
if keyword_set(SYSTEMID) then return, string( (*self.plsHeader).systemID)
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
      self.plsPulseIndSel = ptr_new(index)
      Return, (*self.plspulserec)[index]
    endelse

  endif else begin
    self.plsPulseIndSel = self.plspulseind
    Return, *self.plspulserec
  endelse
  
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




Function pulsewaves::getRawCoordinates, $
                     FIRSTRETURN = FIRSTRETURN, $
                     LASTRETURN = LASTRETURN
  
  if not ptr_valid(self.plsPulseRec) then begin
    self.print, 3, "No records loaded in the object data member..."
    self.print, 2, "Load some record(s) using pulsewaves::readPulses()..."
    Return, 0
  endif
  
  ; creating the anchor points array
  anchPoints = pointarrayclass_sazerac(((*self.plsPulseRec).anchorx),((*self.plsPulseRec).anchory),((*self.plsPulseRec).anchorz))
  ; Creating the direction vectors array
  dirVec = vectorarrayclass($
    ((*self.plsPulseRec).targetx) - ((*self.plsPulseRec).anchorx), $
    ((*self.plsPulseRec).targety) - ((*self.plsPulseRec).anchory), $
    ((*self.plsPulseRec).targetz) - ((*self.plsPulseRec).anchorz)  $
    )
  ; normalize the length of the dir vectors
  dum =  dirVec.normalizeLengthBy(1000.)
  
  if keyword_set(FIRSTRETURN) then begin
  ; computing the coordinates of the lastReturn
  betaPointsCloud = pointarrayclass_sazerac($
    ((*self.plsPulseRec).anchorx) + ((*self.plsPulseRec).firstreturn) * dirVec.x(), $
    ((*self.plsPulseRec).anchory) + ((*self.plsPulseRec).firstreturn) * dirVec.y(), $
    ((*self.plsPulseRec).anchorz) + ((*self.plsPulseRec).firstreturn) * dirVec.z()  $
    )
  endif
  
  if keyword_set(LASTRETURN) then begin
    ; computing the coordinates of the lastReturn
    betaPointsCloud = pointarrayclass_sazerac($
      ((*self.plsPulseRec).anchorx) + ((*self.plsPulseRec).lastreturn) * dirVec.x(), $
      ((*self.plsPulseRec).anchory) + ((*self.plsPulseRec).lastreturn) * dirVec.y(), $
      ((*self.plsPulseRec).anchorz) + ((*self.plsPulseRec).lastreturn) * dirVec.z()  $
      )
  endif
  
  ; getting scale and offset factors
  xyzScale = vectorclass(self.getHeaderProperty(/XSCALE),self.getHeaderProperty(/YSCALE),self.getHeaderProperty(/ZSCALE))
  xyzOffset = vectorclass(self.getHeaderProperty(/XOFFSET),self.getHeaderProperty(/YOFFSET),self.getHeaderProperty(/ZOFFSET))
  
  ; applying scale and offset to points cloud
  dum = betaPointsCloud.multiVector(xyzScale)
  dum = betaPointsCloud.addVector(xyzOffset)
  
  return, betaPointsCloud
  
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
Function pulsewaves::getVlRecords, INDEX = INDEX, RECORDID = RECORDID

  if keyword_set(INDEX) then begin

    if Max(INDEX) ge (*self.plsHeader).nvlrecords then begin
      self.print, 2, "Index out of range..."
      Return, 0
    endif else begin
      Return, (*self.plsvlrarray)[INDEX]
    endelse

  endif 
  
  if keyword_set(RECORDID) then begin
    
    nRecordID = n_elements(RECORDID)
    case 1 of
      
      nRecordID eq 1: begin
        
          id = Where( (*self.Plspulsedes) eq RECORDID, /NULL)
          if id ne !NULL then begin
            Return, [(*self.Plsvlrarray)[id*2], (*self.Plsvlrarray)[(id*2)+1]]
          endif
        
        End
        
      nRecordID eq 2: begin
        
        id = Where( (*self.Plspulsedes) ge RECORDID[0] and (*self.Plspulsedes) le RECORDID[1], /NULL)
        if id ne !NULL then begin
          Return, [(*self.Plsvlrarray)[id*2], (*self.Plsvlrarray)[(id*2)+1]]
        endif        
        
        End
    endcase
    
    
    
  endif
  
  Return, !NULL
  
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



Pro pulsewaves__define

  void = { pulsewaves, $
    plsFilePath   : "",$                ; Fully qualified path to the PLS file
    wvsFilePath   : "",$                ; Fully qualified path to the WVS file
    sysSep        : "",$                ; OS path separator
    pathRoot      : "",$                ; String the represents the object root path.
    osRoot        : "",$                ; String the represents the OS root path.
    plsStrtConst  : ptr_new(),$         ; Pointer to the constant that define some structure fields
    plsHeader     : ptr_new(),$         ; Pointer to the PLS Header data
    plsVlrArray   : ptr_new(),$         ; Pointer to the Variable Length Records (in reading order - Header/Key)
    plsPulseDes   : ptr_new(),$         ; Pointer to an array of structure holding all the Pulse descriptor VLR
    plsPulseRec   : ptr_new(),$         ; Pointer to the records of the PLS file hold in the data member
    plsPulseInd   : ptr_new(),$         ; Pointer to the index of the records from the PLS file hold in plsPulseRec
    plsPulseIndSel: ptr_new(),$         ; Pointer to an index of the selected pulses defined by pulsewaves::getPulses
    plsAvlrarray  : ptr_new(),$         ; Pointer to the Append Variable Length Records (in reading order - Header/Key)
    wvsHeader     : ptr_new(),$         ; Pointer to the header of the WVS file
    wvsWaveRec    : ptr_new(),$         ; Pointer to the records of the WVS file corresponding to the records in plsPulseRec
    wvsWaveInd    : ptr_new(),$         ; Pointer to the index corresponding to the records in plsPulseRec
    plotColor     : strarr(4),$         ; String array containing the color index for the plot of the waves
    bitNoPrint    : 0B,$                ; Byte that specify if the print out of the HEADER/VLR/AVLR is enable or disable
    plotFlag      : 0B,$                ; A bit to count how many segment have been  plot
    Forigin       : 0B, $               ; A bit that represents the format of the waveform
    Manuter       : 0B, $               ; Bit that represents the manufacturer of the waveform
    inherits consoleclass $             ; Inherits from the consoleclass for formatted console and log ouptut
  }

End



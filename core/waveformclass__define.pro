; docformat = 'rst'
;+
; :Description:
;    This object will be use to manipulate any full-wavefrom data
;
; :Category:
; 	WAVEFORM, GENERAL
;
; :Return:
; 	If any, what is the output of this method
;
;	:Uses:
;		waveObj = waveformclass(wave, ORIGIN = ORIGIN, MANUFACTURER = MANUFACTURER)
;		
; :Params:
;    wave: in, required, type=array
;     an array representing the full-wavefrom
;
; :Keywords:
;    origin: in, required, type=byte
;     a bit flag to specify the type of file : 0 (LAS), 1 (PULSEWAVES), 2 (INW), UNKNOWN (99)
;    manufacturer: in, required, type=byte
;     a bit flag to specify the system manufacturer : 0 (OPTECH), 1 (RIEGL), UNKNOWN (99)
;     
; :History:
; 	Septembre 2014 - Initial development
; 	June 2015
; 	   adapting the class to be integrated with Pulsewaves::readWaves() method
; 	   inherit rayclass for better functionnality
;
; :Author: antoine
;-




;+
; :Description:
;    Initialisation function of the object
;
; :Category:
; 	GENERAL
;
; :Return:
; 	none
;
;	:Uses:
;		dum = waveformclass(wave, origin=origin)
;
;	:Example:
;		A quick example on how to use this method
;
; :Params:

;
; :Keywords:
;    wave: in, required, type=array
;     an array representing the full-wavefrom
;    nsamples: in, optional, type=int
;     if provided, represents the number of samples in the waveform. If not provided this value is
;     estimate from the wave array
;    DurationFromAnchor: in, required, type=int
;     represents the sample offset for the considered segment
;    formatorigin: in, required, type=byte
;     a bit flag to specify the type of file : 0 (LAS), 1 (PULSEWAVES), 2 (INW), UNKNOWN (99)
;    manufacturer: in, required, type=byte
;     a bit flag to specify the system manufacturer : 0 (OPTECH), 1 (RIEGL), UNKNOWN (99)
;     
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::init, WAVE = WAVE, NSAMPLES = NSAMPLES, $
                              DFA = DFA, LUT = LUT, NAN = NAN, $
                              FORMATORIGIN = FORMATORIGIN, MANUFACTURER = MANUFACTURER, $
                              SEGMENTNUMBER = SEGMENTNUMBER, $
                              REMOVE_POINTS_BELOW = REMOVE_POINTS_BELOW

    
    
  Compile_opt idl2
  
  self.Out = Obj_new('consoleclass', /Quiet)
  self.plotColor = ["r","b","g","y"]
  IF Keyword_set(SEGMENTNUMBER) then self.plotFlag = SEGMENTNUMBER else self.plotflag = 0B
  
;  ; Initialize the rayclass - if no arguments are pass then some plain ones are created
;  if keyword_set(ORIGIN) and keyword_set(DIRECTION) then begin
;    dumm = self->rayclass::init(ORIGIN, DIRECTION)
;  endif else begin
;    ORIGIN = pointclass_sazerac()
;    DIRECTION = vectorclass()
;    dumm = self->rayclass::init(ORIGIN, DIRECTION)
;  endelse
  
  
  self.Out->printsep
  
  if keyword_set(SEGMENTNUMBER) then self.Out->print,1 , 'Creating wavefrom object for segment #' + strcompress(string(segmentNumber-1), /REMOVE_ALL) + '...' else $
                                     self.Out->print,1 , 'Creating wavefrom object...'
;  if Keyword_set(SEGMENTNUMBER) then self.plotFlag = SEGMENTNUMBER
  
  if not keyword_set(FORMATORIGIN) then FORMATORIGIN = 99
  if not keyword_set(MANUFACTURER) then MANUFACTURER = 99
  
  case FORMATORIGIN of
    1: fileType = 'LAS'
    2: fileType = 'PULSEWAVES'
    3: fileType = 'INW'
    ELSE: begin
            fileType = 'UNKNOWN'
            formatorigin = 99
          end
  endcase
  self.Out->print, 1, 'The pulse originated from ' + fileType + ' file...'
  self.formatOrigin = FORMATORIGIN
  
  case MANUFACTURER of
    1: manufac = 'RIEGL'
    2: manufac = 'OPTECH'
    ELSE: begin
      manufac = 'UNKNOWN'
      manufacturer = 99
    end
  endcase
  self.Out->print, 1, 'The pulse originated from ' + manufac + ' system...'
  self.scanManufact = MANUFACTURER
  
  if keyword_set(WAVE) then self.Wave = Ptr_new(WAVE) else self.wave = ptr_new(!NULL)
  if keyword_set(NSAMPLES) then self.Nsamples = NSAMPLES else self.Nsamples = n_elements(wave)
  if keyword_set(DFA) then self.Durationfromanchor = DFA else self.Durationfromanchor = 0
  if keyword_set(LUT) then self.Lut = Ptr_new(LUT) else self.Lut = ptr_new(!NULL)
  
  ; Implementing the NAN threshold value - not fully functional
  if keyword_set(NAN) then self.NANValue = NAN else begin
    case MANUFACTURER of
      0: self.NANvalue = ptr_new(!NULL)
      1: self.NANvalue = ptr_new(-2.000000e+037)
      2: self.NANvalue = ptr_new(-999)
      Else:
    endcase
  endelse
  
  ; Implementing the NAN threshold value - not fully functional
  if keyword_set(REMOVE_POINTS_BELOW) then self.pointFindThreshod = REMOVE_POINTS_BELOW else begin
    case MANUFACTURER of
      0: self.pointFindThreshod = 0.
      1: self.pointFindThreshod = 0.
      2: self.pointFindThreshod = 235.
      Else:
    endcase
  endelse
  
  self.Out->print, 1, 'The pulse has ' + strcompress(self.nSamples, /REMOVE_ALL) + ' samples...'
  
  self.Out->printsep
   
  Return, 1

End



;+
; :Description:
;    this function cleans up the object before being destroy
;
; :Category:
; 	GENERAL
;
; :Return:
; 	None
;
;	:Uses:
;		It is called automatically when the object is destroy
;		
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::cleanup

  Compile_opt idl2

  ; Freeing all data member pointers
  print,1 , 'Cleaning memory...'
  Ptr_free, $
    self.wave, $
    self.Lut, $
    self.Nanvalue, $
    self.samplingInfo, $
    self.points

  dum = self->consoleclass::cleanup()

  ; Destroying the consoleclass object
  print,1 , 'Destroying remaining objects...'
  print,1 , 'Bye :)'

End




Function waveformclass::getNanValue

return, self.NANValue

End


;+
; :Description:
;    This function returns the wave array.
;    If an array is provided, then the wave field is updated
;    If a index value is pass, the corresponding wavefrom value is returned
;
; :Category:
; 	GENERAL
;
; :Return:
;   The wavefrom contains in the class object or nothing if a new waveform is provided
;
;	:Uses:
;		Result = waveformclass::wave()
;		 or
;		Dum = waveformclass::wave(newWaveform)
;    or
;   Result = waveformclass::wave(index = indexValue)
;   
; :Params:
;    value: in, optional, type=array
;     an array of int or float
;
; :Keywords:
;    index: in, optional, type=int
;     scalar or array of index value
;    
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::wave, value, INDEX = INDEX

  if n_elements(value) eq 0 then begin
    return, *self.wave
  endif else begin
    self.wave = ptr_new(value)
    self.n = n_elements(value)
    return, 1
  endelse
  
  if n_elements(index) gt 0 then begin
    return, (*self.wave)[index]
  endif
  
  return, 1
  
End



;+
; :Description:
;    This function returns the number of samples of the waveform.
;
; :Category:
;   GENERAL
;
; :Return:
;   the number of samples
;
; :Uses:
;   Result = waveformclass::n()
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::n

  Return, self.nSamples

End



;+
; :Description:
;    This function returns the duration from anchor of the waveform.
;
; :Category:
;   GENERAL
;
; :Return:
;   Duration from Anchor
;
; :Uses:
;   Result = waveformclass::dfa()
;
; :History:
;   June 2015 : Original development
;
; :Author: antoine
;-
Function waveformclass::durationFromAnchor

  Return, self.durationFromAnchor

End



;+
; :Description:
;    This function returns the Look-Up Table of the waveform.
;
; :Category:
;   GENERAL
;
; :Return:
;   The look-up table
;
; :Uses:
;   Result = waveformclass::lut()
;
; :History:
;   June 2015 : Original development
;
; :Author: antoine
;-
Function waveformclass::lut

  if ptr_valid(self.lut) then Return, *(self.lut) else return, !NULL

End



;+
; :Description:
;    This function returns the origin of the waveform.
;    If an array is provided, then the ori field is updated
;
; :Category:
;   GENERAL
;
; :Return:
;   a bit value corresponding to the file type origin
;
; :Uses:
;   Result = waveformclass::origin()
;   or
;   Dum = waveformclass::origin(newOrigin)
;
; :Params:
;    value: in, optional, type=byte
;     a scalar value
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::originFileType, value

  if N_elements(value) eq 0 then begin
    Return, self.formatOrigin
  endif else begin
    self.formatOrigin = value
  endelse

  Return, 1

End



;+
; :Description:
;    This function returns the manufacturer of the waveform.
;    If an array is provided, then the man field is updated
;
; :Category:
;   GENERAL
;
; :Return:
;   a bit value corresponding to the file type origin
;
; :Uses:
;   Result = waveformclass::manufacturer()
;   or
;   Dum = waveformclass::manufacturer(newMan)
;
; :Params:
;    value: in, optional, type=byte
;     a b value
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::manufacturer, value

  if N_elements(value) eq 0 then begin
    Return, self.scanmanufact
  endif else begin
    self.scanmanufact = value
  endelse

  Return, 1

End



;+
; :Description:
;    This function returns the points detected in the waveform of the waveform.
;
; :Category:
;   GENERAL, PROCESSING
;
; :Return:
;   a structure containing the points of interest
;
; :Uses:
;   Result = waveformclass::points()
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::points

  if ptr_valid(*self.points) then return, *self.points else return, !NULL

End




;+
; :Description:
;    This function crop/filter the waveform.
;
; :Category:
; 	GENERAL, PROCESSING
;
; :Return:
; 	The cropped/filtered waveform
;
;	:Uses:
;		Result = waveformclass::crop()
;		
; :Keywords:
;    BOX: in, required, type=array
;     this is an array or scalar value for the cropping
;     if 4 values are provided, they must be as (xmin, xmax, ymin, ymax)
;     if 2 values are provided, they must be as (x/ymin, x/ymax), and XAXIS or YAXIS keyword must be provided
;     if 1 value is provided, one of the following keyword ( XMIN, XMAX, YMIN, YMAX) must be provided
;    APPLY: in, optional, type=boolean
;     if present, the amplified wave is store in the object and overwrite to raw wavefrom    
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::crop, $
          BOX = B, $
          XAXIS = XAXIS, YAXIS = YAXIS, $
          XMIN = XMIN, XMAX = XMAX, YMIN = YMIN, YMAX = YMAX, $
          APPLY = APPLY

case N_elements(b) of
  
  4:begin
    
      ; Filtering the X
      newWave = (*self.wave)[b[0:1]]
      ; Filtering the Y
      temp = where(newWave ge b[2] and newWave le b[3], /NULL)
      newWave = newWave[temp]  
    
    end
    
  2:begin
    
      if keyword_set(XAXIS) eq 0 and keyword_set(YAXIS) eq 0 then begin
        self.Out->print, 3, 'Missing keyword to properly crop the waveform...'
        self.Out->print, 3, 'Please specify if we need to crop among the X or Y axe...'
        return, 0
      end
    
      if keyword_set(XAXIS) then begin
        ; Filtering the X
        newWave = (*self.Wave)[b[0:1]]
      endif
      
      if keyword_set(YAXIS) then begin
        ; Filtering the Y
        temp = Where(newWave ge b[2] and newWave le b[3], /NULL)
        newWave = newWave[temp]
      endif
    
    end
    
  1:begin

      if Keyword_set(XMIN) eq 0 and Keyword_set(XMAX) eq 0 and $
         Keyword_set(YMIN) eq 0 and Keyword_set(YMAX) eq 0 then begin
        self.Out->print, 3, 'Missing keyword to properly crop the waveform...'
        self.Out->print, 3, 'Please specify if we need to crop among the X or Y axe...'
        Return, 0
      end    

      if Keyword_set(XMIN) then begin
        ; Filtering the XMIN
        newWave = (*self.Wave)[b:*]
      endif

      if Keyword_set(XMAX) then begin
        ; Filtering the XMAX
        newWave = (*self.Wave)[0:b]
      endif
      
      if Keyword_set(YMIN) then begin
        ; Filtering the YMIN
        temp = Where(*self.wave ge b, /NULL)
        newWave = newWave[temp]
      endif

      if Keyword_set(YMAX) then begin
        ; Filtering the YMAX
        temp = Where(*self.wave le b, /NULL)
        newWave = newWave[temp]
      endif      
    
    
    end
   
  ELSE:
  
endcase

if keyword_set(apply) then begin
  self.Out->print, 3, 'The original wavefrom has been overwrite...'
  self.wave = ptr_new(newWave)
endif

  return, newWave
  
End



;+
; :Description:
;    This function apply an amplification algorithm to the raw waveform
;
; :Category:
; 	GENERAL, PROCESSING
;
; :Return:
; 	The amplified waveform
;
;	:Uses:
;		Result = waveformclass::ampli()
;
; :Keywords:
;    APPLY: in, optional, type=boolean
;     if present, the amplified wave is store in the object and overwrite to raw wavefrom
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::ampli, APPLY = APPLY, FILTER = FILTER, NANVALUE = NANVALUE


  case self.formatOrigin of
    1: newWave = *self.wave
    2: newWave = (*(self.lut))[(*(self.wave))]
    3: newWave = inw_logAmpOptimize(*self.wave)
    99: newWave = *self.wave
    ELSE:
  endcase
  
  
  if Keyword_set(FILTER) then begin
    
    isIndex = where(newWave ne NANVALUE, COMPLEMENT = COMPLEMENT, /NULL)
    newWave[COMPLEMENT] = min(newWave[isIndex])
    
  endif
  
  
  if keyword_set(APPLY) then begin
    self.Out->print, 2, 'The original wavefrom has been overwrite...'
    self.wave = ptr_new(newWave)
  endif

  return, newWave
  
End



;+
; :Description:
;    This function finds the different points in the wavefrom
;
; :Category:
; 	GENERAL, PROCESSING
;
; :Return:
; 	an structure that contains the points
;
;	:Uses:
;		Result = waveformclass::findPoint()
;
; :Keywords:
;    _REF_EXTRA: will accept any keyword and will be passed to the point finding function
;       Pointocator required two keywords: 
;       THRES: in, optional, type=float
;         this is a cutting value under which no point are detected
;       SIMPLIFY: in, optional, type=boolean 
;         if present, some duplicate points are remove
;
; :History:
;   September 2014 : Original development
;
; :Author: antoine
;-
Function waveformclass::findPoint, THRES = THRES, NANVALUE = NANVALUE, SIMPLIFY = SIMPLIFY, NOSMOOTH = NOSMOOTH, ADD_TAIL = ADD_TAIL

  if keyword_set(NANVALUE) then begin
    self.Out->print, 2, 'The wavefrom his filtered by a provided NAN value...'
    filterValue = NANVALUE
  endif else begin
    self.Out->print, 2, 'The wavefrom his filtered by its own NAN value...'
    filterValue = *(self.NANValue)
  endelse
  
  ; Apply any amplification and filtering to get a proper signal
  if keyword_set(THRES) then ampliWave = self.ampli(/FILTER, NANVALUE = filterValue) else ampliWave = self.ampli(/FILTER)
  
  ; Calling the pointocator function to extract the points
  pointResult = pointocator(ampliWave, SIMPLIFY = SIMPLIFY, NOSMOOTH = NOSMOOTH, ADD_TAIL = ADD_TAIL)
  
  ; Remove points if the keyword is set REMOVE_POINTS_BELOW
  if *(pointResult.rawUpPoints) ne !NULL then begin
    tempIndex = where( *(pointResult.rawUpPoints) ge self.pointFindThreshod, count, /NULL)
    if tempIndex ne !NULL then begin
      pointResult.rawUpPoints = ptr_new((*(pointResult.rawUpPoints))[tempIndex])
      pointResult.n[1] = count
    endif
  endif
  
  ; Updating the internal data field
  self.points = ptr_new(pointResult)
  ; Returning the result
  return, pointResult
  
End



Function waveformclass::plotwave, OVERPLOT = OVERPLOT, COLORINDEX = COLORINDEX, POINTSOVERPLOT = POINTSOVERPLOT

  newWave = (*(self.lut))[(*(self.wave))]

  if keyword_set(POINTSOVERPLOT) then begin
    ppointru = plot( *((*(self.points)).rawuppoints) + self.durationFromAnchor, newWave[*((*(self.points)).rawuppoints)], "r*", /OVERPLOT)
;    ppointrd = plot( *((*(self.points)).rawDownPoints) + self.durationFromAnchor, newWave[*((*(self.points)).rawDownPoints)], "gD", /OVERPLOT)
;    ppointsd = plot( *((*(self.points)).secDownPoints) + self.durationFromAnchor, newWave[*((*(self.points)).secDownPoints)], "btu", /OVERPLOT)

  endif else begin
    
    if keyword_set(OVERPLOT) or self.plotFlag gt 1 then begin
      
  ;    newWave = (*(self.lut))[(*(self.wave))]

      plt = plot((where(newWave ne *(self.NANValue)))+self.durationFromAnchor, newWave[where(newWave ne *self.NANValue)], color=(self.plotColor)[self.plotFlag-1], /OVERPLOT)

      self.plotFlag += 1B
      
    endif else begin
      
  ;    newWave = (*(self.lut))[(*(self.wave))]
      plp = plot((where(newWave ne *self.NANValue))+ self.durationFromAnchor, newWave[where(newWave ne *self.NANValue)], color=(self.plotColor)[self.plotFlag-1])
      self.plotFlag += 1B
      
    endelse
  
  endelse
   

Return, 1

End



Pro waveformclass__define

  ; Definition of the data hold by the object
  void = {waveformclass, $
    wave                : ptr_new(), $          ; pointer to the full-waveform
    nSamples            : 0, $                  ; Number of samples in the wavefrom
    durationFromAnchor  : 0U, $                 ; Sample offset to the segment
    LUT                 : ptr_new(), $          ; Pointer to the lookup table for the pulse
    NANValue            : ptr_new(), $          ; pointer to the NAN value associated to the file type and/or manufacturer
    pointFindThreshod   : 0.0, $                ; Value below which the found points will be remove
    plotColor           : strarr(4), $
    plotFlag            : 0B, $
    samplingInfo        : ptr_new(), $          ; Pointer to the sampling information associated with the wave
    formatOrigin        : 0B, $                 ; bit flag describing wave origin : 0 (LAS), 1 (PULSEWAVES), 2 (INW)
    scanManufact        : 0B, $                 ; bit flag describing system manufacturer : 0 (Optech), 1 (Riegl)
    outX                : ptr_new(),$         ; Array of the time values for the OUTGOING pulse
    outY                : ptr_new(),$         ; Array of the energy/amplitude of the OUTGOING pulse
    inX                 : ptr_new(),$         ; Array of the time values for the RETURNING pulse - note if multiple segement, then it will be an array of structure
    inY                 : ptr_new(),$         ; Array of the time values for the RETURNING pulse - note if multiple segement, then it will be an array of structure
    points              : ptr_new(),$           ; Pointer to a structure that holds the trougths and peaks of the waveform
    out                 : obj_new() $           ; inherits consoleclass data and methodes
  }


End


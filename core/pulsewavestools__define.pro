; docformat = 'rst'
;+
; This is a class object that gather the processing tools 
; associated with the Pulsewaves file.
; This is an IDL implementation of the Open Source
; PulseWaves format created by Martin Isenburg Creator
; of LASTools and LASZip.
; The Class object will be initialized automatically when
; a pulsewaves object is initialized.
;
; :Category:
;   WAVEFORM, PROCESSING
;
; :Uses:
;   plsObj = obj_new('pulsewaves', inputfile = '/Path/To/PLS/File')
;
; :History:
;   September 2014
;    - 9/11 - Initial development
;
; :Author:
;   Antoine Cottin
;-
Function pulsewavestools::init, INPUTFILE = FILE, $
    NO_VLR = NO_VLR, $
    _EXTRA = CONSOLE_OPTIONS
    
dum = self->pulsewaves::init(INPUTFILE = FILE, NO_VLR = NO_VLR, _extra = console_options)
;self.pPulseWaves = pPulsewaves
self.plsAnchors = ptr_new(!NULL)
self.plsTargets = ptr_new(!NULL)
self.plsDir = ptr_new(!NULL)
self.plsRays = ptr_new(!NULL)
self.plsTrajectory = ptr_new(!NULL)
return, 1

End



; This function will get the Anchor(s)
Function pulsewavestools::getAnchor

return, *self.plsAnchors

End



; This function will get the direction vector(s)
Function pulsewavestools::getDirection

  Return, *self.Plsdir

End



; This function will get the ray(s)
Function pulsewavestools::getRay

  Return, *self.Plsrays

End




; This function will get the Anchors points which represent the trajectory
Function pulsewavestools::getTrajectory

  Return, *self.Plstrajectory

End


; This function will computes the anchors point(s) contains in the pulse record(s)
Function pulsewavestools::computeAnchorPoints

  scale = self.getHeaderProperty(/XYZSCALE)
  offset = self.getHeaderProperty(/XYZOFFSET)
  pulses = self.getPulses()
  
  self.plsAnchors = ptr_new( pointarrayclass_sazerac($
    [ (pulses.anchorX * scale.x) + offset.x ],$
    [ (pulses.anchorY * scale.y) + offset.y ],$
    [ (pulses.anchorZ * scale.z) + offset.z ] $
    ) )
;  print, 'Samples of Anchor Points'
;  print, ((*self.plsAnchors).xyz())[0:5,*]
  
  ; The result is effectively the trajectory of the optical center
  self.plsTrajectory = self.plsAnchors
  
  return, 1
  
End



; This function will computes the target point contains in the pulse record
Function pulsewavestools::computeTargetPoints

  scale = self.getHeaderProperty(/XYZSCALE)
  offset = self.getHeaderProperty(/XYZOFFSET)
  pulses = self.getPulses()

  self.Plstargets = Ptr_new( pointarrayclass_sazerac($
    [ (pulses.targetX * scale.X) + offset.X ],$
    [ (pulses.targetY * scale.Y) + offset.Y ],$
    [ (pulses.targetZ * scale.Z) + offset.Z ] $
    ) )

  Return, 1

End



; This function will computes the anchors points contains in the PLS file
Function pulsewavestools::computeVectors, $
                          INDEX = INDEX, $
                          UNIT = UNIT


if keyword_set(INDEX) then begin
  
  anchorPoint = (*self.Plsanchors).extractPoint(INDEX)
  targetPoint = (*self.Plstargets).extractPoint(INDEX)
  pulseDir = anchorPoint.makeVector(targetPoint)
  
endif else begin
  
  pulseDir = (*self.Plsanchors).makeVectorArrayFromPointArray((*self.Plstargets))

endelse
    
  if keyword_set(unit) then dum = pulseDir.normalizeLengthBy(1000.)

  self.Plsdir = Ptr_new(pulseDir)
  Return, 1

End



; This function will computes the pulses contains in the PLS file
Function pulsewavestools::computePulses, $
                          INDEX = INDEX, $
                          NO_PLOT = NO_PLOT, $
                          UNIT = UNIT

  ; If the anchors points and the target points have been computed yet, then do it
  if *(self.plsAnchors) eq !NULL then origin = self.computeAnchorPoints()
  if *(self.plsTargets) eq !NULL then target = self.computeTargetPoints()

  ; If the direction vector(s)haven't been computed yet, then do it
  if *(self.plsDir) eq !NULL then begin
    if keyword_set(unit) then direct = self.computeVectors(/UNIT) else $
      direct = self.computeVectors()
  endif
    
  ; Creating the ray class object - if the keyword INDEX is set, then only
  ; one ray is created. If no keyword is set, then an array of ray is created     
  if keyword_set(INDEX) then begin
    
    anchorPoint = (*self.Plsanchors).extractPoint(INDEX)
    targetPoint = (*self.Plstargets).extractPoint(INDEX)
    dum = self.getPulses(INDEX)

    dirVec = (*self.Plsdir).getSubArray(INDEX)
    
    if keyword_set(NO_PLOT) then returnWave = self->pulsewaves::readWaves(/NO_PLOT) else returnWave = self->pulsewaves::readWaves()
    
    if size(anchorPoint,/type) eq 11 and size(dirVec,/type) eq 11 then begin
;        if strlowcase(obj_class(anchorPoint)) eq 'pointclass_sazerac' and strlowcase(obj_class(dirVec)) eq 'vectorclass' then $
          self.plsRays = ptr_new(plsrayclass(anchorPoint, dirVec, returnWave)) ;else return, !NULL
        endif
 

  endif else begin
    
    anchorPoint = (*self.plsAnchors)
    targetPoint = (*self.plsTargets)
    self.plsRays = ptr_new(plsrayarrayclass(*self.PlsAnchors, dirVec))
    
  endelse
  

  
  ; Returning the Ray to the caller
  Return, *self.Plsrays

End




Function pulsewavestools::getReturnSampleCoordinates, $
            FIRST = FIRST, $
            LAST = LAST, $
            ALL = ALL
            
x = anchorX + firstReturnSample * ((*self.Plsrays).getDirection).X()
y = anchorY + firstReturnSample * ((*self.Plsrays).getDirection).Y()
z = anchorZ + firstReturnSample * ((*self.Plsrays).getDirection).Z()


End


Function pulsewavestools::getNumberOfPoints

return, (*self.PlsAnchors).getDim()

End


Function pulsewavestools::readWaves



End



Function pulsewavestools::plotWaves


  
End



Function pulsewavestools::extractPoints

; Setting some constants
flag = 0B
nLoop = 1UL ; the loop is setup to one as passed as a keyword and therefore will provoke a bug

; Initializing pulsewavetools with the input file - no print of the VLR, HEADER and the log output is turned to QUIET
; TODO: adapt keyword to object
;plsObj = pulsewavestools(INPUTFILE = self.plsFilePath, /NO_VLR, /NO_HEADER, /QUIET)

; Getting the number of pulse hold in the file
nRef = self.getHeaderProperty(/NPULSES)


While nLoop lt nRef do begin

  ; Error catcher initialization
  Catch, Error_status

  IF Error_status NE 0 THEN BEGIN
    self.print, 3, 'Error catcher stepping in...'
    traceback = Scope_Traceback(/STRUCTURE)
    self.print, 3, 'Error caught in ' + traceback[-1].ROUTINE + ', scope level ' + StrTrim(traceback[-1].LEVEL, 2)
    print,  'Error index: ', Error_status
    print,  'Error message: ', !ERROR_STATE.MSG
    nLoop += 1UL
    if nLoop ge nRef then return, 0
    Catch, /CANCEL
  ENDIF

  ; Compute the pulse
  pulse = self.computePulses(INDEX = nLoop, /UNIT)


  ;########################################################
  if size(pulse,/TYPE) eq 11 then begin
    
    nSeg = pulse.getNumberOfSegment()
    
    ; We starting loop from 1 as 0 is the emitted pulse
    for segLoop = 1, nSeg-1 do begin
    
      value = pulse.getNthSegment(segLoop)
      peaks = pointocator(value.Int, thres = -2.0e+037, /NOSMOOTH, /ADD_TAIL)
      
      if (*(peaks.rawuppoints)) ne !NULL then begin
        
        savedIntensity = (value.int)((*(peaks.rawuppoints))[0])
        savedCoordinates = (value.coor).extractPoint( (*(peaks.rawuppoints))[0] )
        
      endif
      
    endfor
    
    
;    ; Get the last segment
;    ;        value = pulse.getLastSegment()
;    value = pulse.getFirstSegment()
;    peaks = pointocator(value.Int, thres = -2.0e+037, /NOSMOOTH, /ADD_TAIL)
;    if (*(peaks.rawuppoints)) ne !NULL then begin
;      ;          ; Getting the last pulse of the segment
;      ;          savedIntensity = (value.int)((*(peaks.rawuppoints))[-1])
;      ;          savedCoordinates = (value.coor).extractPoint( (*(peaks.rawuppoints))[-1] )
;      ; Getting the first pusle of the segment
;      savedIntensity = (value.int)((*(peaks.rawuppoints))[0])
;      savedCoordinates = (value.coor).extractPoint( (*(peaks.rawuppoints))[0] )
;
;      ; Progressive result printing
;      if flag eq 0 then begin
;        x = savedCoordinates.x()
;        y = savedCoordinates.y()
;        z = savedCoordinates.z()
;        i = savedIntensity
;
;        name = 'band_' + Strcompress(String(j), /remove_all) + '.csv'
;        Openw, rLun, name, /get_lun
;        Printf, rLun, x, y, z, i
;        Close, rLun
;
;        flag += 1B
;      endif else begin
;        x = savedCoordinates.x()
;        y = savedCoordinates.y()
;        z = savedCoordinates.z()
;        i = savedIntensity
;
;        name = 'band_' + Strcompress(String(j), /remove_all) + '.csv'
;        Openw, rLun, name, /get_lun, /APPEND
;        Printf, rLun, x, y, z, i
;        Close, rLun
;      endelse
;
;      ;             Concatenation for final print
;      if flag eq 0 then begin
;        x = savedCoordinates.x()
;        y = savedCoordinates.y()
;        z = savedCoordinates.z()
;        i = savedIntensity
;        flag += 1B
;      endif else begin
;        x = [x,savedCoordinates.x()]
;        y = [y,savedCoordinates.y()]
;        z = [z,savedCoordinates.z()]
;        i = [i,savedIntensity]
;      endelse
;
;    endif
;
  endif
  ;########################################################


  nLoop += 1UL
Endwhile


End



Pro pulsewavestools__define

  void = {pulsewavestools,$
    plsAnchors    : ptr_new(),$         ; Pointer to a pointarrayclass_sazerac that holds the anchor points coordinates
    plsTargets    : ptr_new(),$         ; Pointer to a pointarrayclass_sazerac that holds the target points coordinates
    plsDir        : ptr_new(),$         ; Pointer to a vectorarrayclass that holds the direction of the pulses
    plsRays       : ptr_new(),$         ; Pointer to a rayarrayclass that will holds the ray direction normilized
    plsTrajectory : ptr_new(),$         ; Pointer to an array (n,3) representing the trajectory of the optical center
    inherits pulsewaves $               ; Inherits from the pulsewaves to access pulsewaves file
  }

End


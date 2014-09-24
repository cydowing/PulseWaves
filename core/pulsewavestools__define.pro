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
    _EXTRA = CONSOLE_OPTIONS
    
dum = self->pulsewaves::init(INPUTFILE = FILE, _extra = console_options)
;self.pPulseWaves = pPulsewaves
self.plsAnchors = ptr_new(!NULL)
self.plsTargets = ptr_new(!NULL)
self.plsDir = ptr_new(!NULL)
self.plsRays = ptr_new(!NULL)
self.plsTrajectory = ptr_new(!NULL)
return, 1

End



; This function will get the Anchors points
Function pulsewavestools::getAnchors

return, *self.plsAnchors

End



; This function will get the Anchors points
Function pulsewavestools::getVector

  Return, *self.Plsdir

End



; This function will get the Anchors points
Function pulsewavestools::getRays

  Return, *self.Plsrays

End




; This function will get the Anchors points which represent the trajectory
Function pulsewavestools::getTrajectory

  Return, *self.Plstrajectory

End


; This function will computes the anchors points contains in the PLS file
Function pulsewavestools::computeAnchorPoints

  scale = self.getHeaderProperty(/XYZSCALE)
  offset = self.getHeaderProperty(/XYZOFFSET)
  pulses = self.getPulses()
  
  self.plsAnchors = ptr_new( pointarrayclass($
    [ (pulses.anchorX * scale.x) + offset.x ],$
    [ (pulses.anchorY * scale.y) + offset.y ],$
    [ (pulses.anchorZ * scale.z) + offset.z ] $
    ) )
  print, ((*self.plsAnchors).xyz())[0:5,*]
  
  ; The result is effectively the trajectory of the optical center
  self.plsTrajectory = self.plsAnchors
  
  return, 1
  
End



; This function will computes the anchors points contains in the PLS file
Function pulsewavestools::computeTargetPoints

  scale = self.getHeaderProperty(/XYZSCALE)
  offset = self.getHeaderProperty(/XYZOFFSET)
  pulses = self.getPulses()

  self.Plstargets = Ptr_new( pointarrayclass($
    [ (pulses.targetX * scale.X) + offset.X ],$
    [ (pulses.targetY * scale.Y) + offset.Y ],$
    [ (pulses.targetZ * scale.Z) + offset.Z ] $
    ) )
  print, ((*self.Plstargets).xyz())[0:5,*]  
  Return, 1

End



; This function will computes the anchors points contains in the PLS file
; A pointer to the pulsewaves object need to be passed
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

;  pulseDir = [$
;    [ (pulses.Targetx * scale.X) + offset.X ],$
;    [ (pulses.Targety * scale.Y) + offset.Y ],$
;    [ (pulses.Targetz * scale.Z) + offset.Z ] $
;    ]
    
    
  if keyword_set(unit) then dum = pulseDir.normalizeLengthBy(1000.)

  self.Plsdir = Ptr_new(pulseDir)
  Return, 1

End



; This function will computes the pulses contains in the PLS file
; A pointer to the pulsewaves object need to be passed
Function pulsewavestools::computePulses, $
                          INDEX = INDEX, $
                          UNIT = UNIT

  ; If the anchors points and the target points have been computed yet, then do it
  if *(self.plsAnchors) eq !NULL then origin = self.computeAnchorPoints()
  if *(self.plsTargets) eq !NULL then target = self.computeTargetPoints()

  ; If the direction vector(s)haven't been computed yet, then do it
  if *(self.plsDir) eq !NULL then begin
    if keyword_set(unit) then direct = self.computeVectors(/UNIT) else $
      dum = self.computeVectors()
  endif
    
  ; Creating the ray class object - if the keyword INDEX is set, then only
  ; one ray is created. If no keyword is set, then an array of ray is created     
  if keyword_set(INDEX) then begin
    
    anchorPoint = (*self.Plsanchors).extractPoint(INDEX)
    targetPoint = (*self.Plstargets).extractPoint(INDEX)
    dum = self.getPulses(INDEX)
    returnWave = self->pulsewaves::readWaves()
    self.plsRays = ptr_new(plsrayclass(anchorPoint, *self.Plsdir, returnWave))

  endif else begin
    
    anchorPoint = (*self.plsAnchors)
    targetPoint = (*self.plsTargets)
    self.plsRays = ptr_new(plsrayarrayclass(*self.PlsAnchors, *self.Plsdir))
    
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



Function pulsewavestools::readWaves



End



Function pulsewavestools::plotWaves

  if p eq 0 then begin
    ;                    if n_elements(waves) gt 1 then begin
    ;plt = plot((((*lut[2]).(1)).(1))[waves], color=self.colarray[self.plotFlag])
    plt = plot(waves, color=(plotColor)[plotFlag])
    plotFlag += 1B
    ;                    endif
  endif else begin

    newWave = (((*lut[1]).(1)).(1))[waves]
    plt = plot(where(newWave ne -2.000000e+037), newWave[where(newWave ne -2.000000e+037)], color=(plotColor)[plotFlag], /OVERPLOT)

    plotFlag += 1B

  endelse
  
End



Pro pulsewavestools__define

  void = {pulsewavestools,$
    pPulseWaves   : ptr_new(),$         ; Pointer to the pulsewaves object
    plsAnchors    : ptr_new(),$         ; Pointer to a pointarrayclass that holds the anchor points coordinates
    plsTargets    : ptr_new(),$         ; Pointer to a pointarrayclass that holds the target points coordinates
    plsDir        : ptr_new(),$         ; Pointer to a vectorarrayclass that holds the direction of the pulses
    plsRays       : ptr_new(),$         ; Pointer to a rayarrayclass that will holds the ray direction normilized
    plsTrajectory : ptr_new(),$         ; Pointer to an array (n,3) representing the trajectory of the optical center
    inherits pulsewaves $               ; Inherits from the pulsewaves to access pulsewaves file
  }

End


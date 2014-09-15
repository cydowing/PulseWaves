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
Pro pulsewavestools__define

void = {pulsewavestools,$
  pPulseWaves   : ptr_new(),$         ; Pointer to the pulsewaves object
  plsAnchors    : ptr_new(),$         ; Pointer to a pointarrayclass that holds the anchor points coordinates
  plsTargets    : ptr_new(),$         ; Pointer to a pointarrayclass that holds the target points coordinates
  plsDir        : ptr_new(),$         ; Pointer to a vectorarrayclass that holds the direction of the pulses
  plsRays       : ptr_new(),$         ; Pointer to a rayarrayclass that will holds the ray direction normilized
  plsTrajectory : ptr_new() $         ; Pointer to an array (n,3) representing the trajectory of the optical center
       }

End


Function pulsewavestools::init, pPulsewaves

self.pPulseWaves = pPulsewaves
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




; This function will get the Anchors points
Function pulsewavestools::getTrajectory

  Return, *self.Plstrajectory

End


; This function will computes the anchors points contains in the PLS file
; A pointer to the pulsewaves object need to be passed
Function pulsewavestools::computeAnchorPoints, pPulsewaves

  scale = (*pPulsewaves).getHeaderProperty(/XYZSCALE)
  offset = (*pPulsewaves).getHeaderProperty(/XYZOFFSET)
  pulses = (*pPulsewaves).getPulses()
  
  self.plsAnchors = ptr_new( pointarrayclass($
    [ (pulses.anchorX * scale.x) + offset.x ],$
    [ (pulses.anchorY * scale.y) + offset.y ],$
    [ (pulses.anchorZ * scale.z) + offset.z ] $
    ) )
  print, ((*self.plsAnchors).xyz())[0:5,*]
  ; The result is effectively the optical center trajectory
  self.plsTrajectory = self.plsAnchors
  
  return, 1
  
End



; This function will computes the anchors points contains in the PLS file
; A pointer to the pulsewaves object need to be passed
Function pulsewavestools::computeTargetPoints, pPulsewaves

  scale = (*pPulsewaves).getHeaderProperty(/XYZSCALE)
  offset = (*pPulsewaves).getHeaderProperty(/XYZOFFSET)
  pulses = (*pPulsewaves).getPulses()

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
Function pulsewavestools::computeVectors, pPulsewaves, UNIT = UNIT

  pulseDir = (*self.Plsanchors).makeVectorArrayFromPointArray((*self.Plstargets))
  
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
Function pulsewavestools::computePulses, pPulsewaves, UNIT = UNIT

  if self.plsAnchors eq !NULL then origin = self.computeAnchorPoints(pPulsewaves)
  if self.plsTargets eq !NULL then target = self.computeTargetPoints(pPulsewaves)
  
  print, ((*self.Plsanchors).xyz())[0:5,*]  
  print, ((*self.Plstargets).xyz())[0:5,*]  
  
  if keyword_set(unit) then direct = self.computeVectors(pPulsewaves, /UNIT) else $
    direct = self.computeVectors(pPulsewaves)
    
;  dumRay = plsrayarrayclass(origin, direct)
  self.plsRays = ptr_new(plsrayarrayclass(*self.PlsAnchors, *self.Plsdir))
  
  Return, *self.Plsrays

End



Function pulsewavestools::getReturnSampleCoordinates, pPulsewaves, $
            FIRST = FIRST, $
            LAST = LAST, $
            ALL = ALL
            
x = anchorX + firstReturnSample * ((*self.Plsrays).getDirection).X()
y = anchorY + firstReturnSample * ((*self.Plsrays).getDirection).Y()
z = anchorZ + firstReturnSample * ((*self.Plsrays).getDirection).Z()


End




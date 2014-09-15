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
  plsDir        : ptr_new(),$         ; Pointer to a vectorarrayclass that holds the direction of the pulses
  plsRays       : ptr_new(),$         ; Pointer to a rayarrayclass that will holds the ray direction normilized
  plsTrajectory : ptr_new() $         ; Pointer to an array (n,3) representing the trajectory of the optical center
       }

End


Function pulsewavestools::init, pPulsewaves

self.pPulseWaves = pPulsewaves
return, 1

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
  
  ; The result is effectively the optical center trajectory
  self.plsTrajectory = self.plsAnchors
  
  return, *self.plsAnchors
  
End



; This function will computes the anchors points contains in the PLS file
; A pointer to the pulsewaves object need to be passed
Function pulsewavestools::computeVectors, pPulsewaves, UNIT = UNIT

  scale = (*pPulsewaves).getHeaderProperty(/XYZSCALE)
  offset = (*pPulsewaves).getHeaderProperty(/XYZOFFSET)
  pulses = (*pPulsewaves).getPulses()

  pulseDir = vectorarrayclass($
    [ (pulses.Targetx * scale.X) + offset.X ],$
    [ (pulses.Targety * scale.Y) + offset.Y ],$
    [ (pulses.Targetz * scale.Z) + offset.Z ] $
    )
    
    
  if keyword_set(unit) then dum = pulseDir.normalizeLength()

  self.Plsdir = Ptr_new(pulseDir)
  Return, *self.Plsdir

End



; This function will computes the pulses contains in the PLS file
; A pointer to the pulsewaves object need to be passed
Function pulsewavestools::computePulses, pPulsewaves, UNIT = UNIT

  if ptr_valid(self.plsAnchors) then print, "valid..."
  origin = self.computeAnchorPoints(pPulsewaves)
  if keyword_set(unit) then direct = self.computeVectors(pPulsewaves, /UNIT) else $
    direct = self.computeVectors(pPulsewaves)

  self.plsRays = ptr_new(plsrayarrayclass(origin, direct))
  
  Return, *self.Plsrays

End




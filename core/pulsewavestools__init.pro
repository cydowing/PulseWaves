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
  
  self.plsAnchors = ptr_new( vectorarrayclass($
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
Function pulsewavestools::computeVectors, pPulsewaves

  scale = (*pPulsewaves).getHeaderProperty(/XYZSCALE)
  offset = (*pPulsewaves).getHeaderProperty(/XYZOFFSET)
  pulses = (*pPulsewaves).getPulses()

  self.Plstarget = Ptr_new( vectorarrayclass($
    [ (pulses.Targetx * scale.X) + offset.X ],$
    [ (pulses.Targety * scale.Y) + offset.Y ],$
    [ (pulses.Targetz * scale.Z) + offset.Z ] $
    ) )



  Return, *self.Plsanchors

End

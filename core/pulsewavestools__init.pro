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
          null:0B$                            ; Void field to put something here
       }

End


Function pulsewavestools::init


return, 1

End
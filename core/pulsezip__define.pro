Pro pulsezip__define

void = {pulsezip, $
        pulseZipPath    : '',$
        inFilePath      : '',$
        spawnCommand    : '',$
        bitParsError    : 0B,$
        bitIsKeyword    : 0B,$
        inherits consoleclass $
        }
        
End


Function pulsezip::init, $
                   I = I, $                         ; Input file keyword mandatory 
                   O = O, $                         ; Output zip file keyword, optional
                   NO_GUI = NO_GUI, $               ; If set use no GUI interface
                   PULSEZIP_PATH = PULSEZIP_PATH, $ ; Fully qualified path to pulsezip - if not provided - will use pulsezip.exe provided with the code
                   WINE_PATH = WINE_PATH , $        ; Fully qualified path to wine - Only for OsX and linux environments
                   _EXTRA = PARM                    ; Pulsezip parameters
                   
; locating the dep - in case of OsX and Linux plateform, the pulsezip.exe is provided here. But it's to the user
; to make sure that wine is installed on the system

; in case of linux or osx environment, make sure that wine is install


; Making sure that an input has been provided
;Checking that the provided file exist
if Keyword_set(I) eq 0 then file = '' else file = I
exist = File_test(file)
if exist eq 1 then begin
  self.inFilePath = file
endif else begin
  while exist ne 1 do begin

    if Keyword_set(NO_GUI) then begin
      self.print, 3, "No INPUT_FILE provided or the file doesn't exist..."
      self.print, 3, "Please re-enter a file path string"
      newPath = ""
      Read, newPath
      Print, newPath
      exist = File_test(newPath)
    endif else begin
      ; If not inputfile is provided, then open a dialog pickfile
      newPath = Dialog_pickfile(/READ, FILTER = ['*.pls', '*.plz'])
      exist = File_test(newPath)
    endelse
  endwhile
  self.inFilePath = newPath
endelse

self.spawnCommand = 'pulsezip -i ' + self.inFilePath + ' '

dum = self.parametersParser(_extra = parm)

; If no errors returned by parsing the arguments, then executing the command
if self.bitParsError ne 1 then  dum = self.go()

return, 1

End



Function pulsezip::stringCommandCreator, tag, array

if self.bitIsKeyword eq 0 then begin
  
  for j = 0, n_elements(array)-1 do begin
    if j eq 0 then result = '-' + tag + ' ' + strcompress(string(array[j]), /REMOVE_ALL) else $
                   result = result + ' ' + strcompress(string(array[j]), /REMOVE_ALL)
  endfor
  
endif else begin
  
for j = 0, n_elements(array)-1 do begin
  if j eq 0 then result = '-' + strlowcase(tag)  else $
    result = result + ' ' + '-' + strlowcase(tag)
endfor
  
endelse

return, result

End



Function pulsezip::errorParsingArgument, tName, parm, n

self.print, 3, 'The keyword ' + tName + ' takes ' + strcompress(string(n),/REMOVE_ALL) + ' parameters and ' + strcompress(string(n_elements(parm)),/REMOVE_ALL) + ' have been provided...'
self.bitParsError = 1B 
return, 0

End


Function pulsezip::parametersParser, _extra = parm

tName = tag_names(parm)
nTags = n_tags(parm)

for i = 0, nTags -1 do begin
  
case 1 of
  
;CLIP_TILE 631000 4834000 1000 (LL_X LL_Y SIZE)
tName[i] eq 'CLIP_TILE':      if n_elements(parm.(i)) ne 3 then dum = self.errorParsingArgument(tName[i], parm.(i), 3) 
;-CLIP_CIRCLE 630250.00 4834750.00 100 (X Y RADIUS)
tName[i] eq 'CLIP_CIRCLE':    if N_elements(parm.(i)) ne 3 then dum = self.errorParsingArgument(tName[i], parm.(i), 3)
;-CLIP_BOX 620000 4830000 100 621000 4831000 200 (MIN_X MIN_Y MIN_Z MAX_X MAX_Y MAX_Z)
tName[i] eq 'CLIP_BOX':       if N_elements(parm.(i)) ne 6 then dum = self.errorParsingArgument(tName[i], parm.(i), 6)
;-CLIP 630000 4834000 631000 4836000 (MIN_X MIN_Y MAX_X MAX_Y)
tName[i] eq 'CLIP':           if N_elements(parm.(i)) ne 4 then dum = self.errorParsingArgument(tName[i], parm.(i), 4)
;-CLIP_X_BELOW 630000.50 (MIN_X)
tName[i] eq 'CLIP_X_BELOW':   if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_Y_BELOW 4834500.25 (MIN_Y)
tName[i] eq 'CLIP_Y_BELOW':   if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_X_ABOVE 630500.50 (MAX_X)
tName[i] eq 'CLIP_X_ABOVE':   if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_Y_ABOVE 4836000.75 (MAX_Y)
tName[i] eq 'CLIP_Y_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_Z 11.125 130.725 (MIN_Z, MAX_Z)
tName[i] eq 'CLIP_Z':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-CLIP_Z_BELOW 11.125 (MIN_Z)
tName[i] eq 'CLIP_Z_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_Z_ABOVE 130.725 (MAX_Z)    
tName[i] eq 'CLIP_Z_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)


;FILTER PULSES BASED ON FIRST SAMPLE.
;-CLIP_FIRST 630000 4834000 631000 4836000 (MIN_X MIN_Y MAX_X MAX_Y)
tName[i] eq 'CLIP_FIRST':           if N_elements(parm.(i)) ne 4 then dum = self.errorParsingArgument(tName[i], parm.(i), 4)
;-CLIP_FIRST_X_BELOW 630000.50 (MIN_X)
tName[i] eq 'CLIP_FIRST_X_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_FIRST_Y_BELOW 4834500.25 (MIN_Y)
tName[i] eq 'CLIP_FIRST_Y_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_FIRST_X_ABOVE 630500.50 (MAX_X)
tName[i] eq 'CLIP_FIRST_X_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_FIRST_Y_ABOVE 4836000.75 (MAX_Y)
tName[i] eq 'CLIP_FIRST_Y_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_FIRST_Z 11.125 130.725 (MIN_Z, MAX_Z)
tName[i] eq 'CLIP_FIRST_Z':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-CLIP_FIRST_Z_BELOW 11.125 (MIN_Z)
tName[i] eq 'CLIP_FIRST_Z_BELOW':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-CLIP_FIRST_Z_ABOVE 130.725 (MAX_Z)
tName[i] eq 'CLIP_FIRST_Z_ABOVE':           if N_elements(parm.(i)) ne 3 then dum = self.errorParsingArgument(tName[i], parm.(i), 3)

;FILTER PULSES BASED ON LAST SAMPLE.
;-CLIP_LAST 630000 4834000 631000 4836000 (MIN_X MIN_Y MAX_X MAX_Y)
tName[i] eq 'CLIP_LAST':           if N_elements(parm.(i)) ne 4 then dum = self.errorParsingArgument(tName[i], parm.(i), 4)
;-CLIP_LAST_X_BELOW 630000.50 (MIN_X)
tName[i] eq 'CLIP_LAST_X_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_LAST_Y_BELOW 4834500.25 (MIN_Y)
tName[i] eq 'CLIP_LAST_Y_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_LAST_X_ABOVE 630500.50 (MAX_X)
tName[i] eq 'CLIP_LAST_X_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_LAST_Y_ABOVE 4836000.75 (MAX_Y)
tName[i] eq 'CLIP_LAST_Y_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_LAST_Z 11.125 130.725 (MIN_Z, MAX_Z)
tName[i] eq 'CLIP_LAST_Z':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-CLIP_LAST_Z_BELOW 11.125 (MIN_Z)
tName[i] eq 'CLIP_LAST_Z_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_LAST_Z_ABOVE 130.725 (MAX_Z)
tName[i] eq 'CLIP_LAST_Z_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
 
;FILTER PULSES BASED ON ANCHOR POINT.
;-CLIP_ANCHOR 630000 4834000 631000 4836000 (MIN_X MIN_Y MAX_X MAX_Y)
tName[i] eq 'CLIP_ANCHOR':           if N_elements(parm.(i)) ne 4 then dum = self.errorParsingArgument(tName[i], parm.(i), 4)
;-CLIP_ANCHOR_X_BELOW 630000.50 (MIN_X)
tName[i] eq 'CLIP_ANCHOR_X_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_ANCHOR_Y_BELOW 4834500.25 (MIN_Y)
tName[i] eq 'CLIP_ANCHOR_Y_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_ANCHOR_X_ABOVE 630500.50 (MAX_X)
tName[i] eq 'CLIP_ANCHOR_X_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_ANCHOR_Y_ABOVE 4836000.75 (MAX_Y)
tName[i] eq 'CLIP_ANCHOR_Y_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_ANCHOR_Z 11.125 130.725 (MIN_Z, MAX_Z)
tName[i] eq 'CLIP_ANCHOR_Z':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-CLIP_ANCHOR_Z_BELOW 11.125 (MIN_Z)
tName[i] eq 'CLIP_ANCHOR_Z_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CLIP_ANCHOR_Z_ABOVE 130.725 (MAX_Z)
tName[i] eq 'CLIP_ANCHOR_Z_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
 
;FILTER PULSES BASED ON THE SCANLINE FLAGS.
;-DROP_SCAN_DIRECTION 0
tName[i] eq 'DROP_SCAN_DIRECTION':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-SCAN_DIRECTION_CHANGE_ONLY
tName[i] eq 'SCAN_DIRECTION_CHANGE_ONLY':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-EDGE_OF_SCAN_LINE_ONLY
tName[i] eq 'EDGE_OF_SCAN_LINE_ONLY':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
 
;FILTER PULSES BASED ON THEIR TIME STAMP.
;-KEEP_TIME 11.125 130.725
tName[i] eq 'KEEP_TIME':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
;-DROP_TIME_BELOW 11.125
tName[i] eq 'DROP_TIME_BELOW':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-DROP_TIME_ABOVE 130.725
tName[i] eq 'DROP_TIME_ABOVE':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-DROP_TIME_BETWEEN 22.0 48.0
tName[i] eq 'DROP_TIME_BETWEEN':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
 
;FILTER PULSES BASED ON THEIR PULSE DESCRIPTOR.
;-KEEP_DESCRIPTOR 1 2
tName[i] eq 'KEEP_DESCRIPTOR':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-DROP_DESCRIPTOR 0
tName[i] eq 'DROP_DESCRIPTOR':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
 
;FILTER PULSES BASED ON SCAN DIRECTION OR MIRROR FACET.
;-KEEP_SCAN_DIRECTION 1
tName[i] eq 'KEEP_SCAN_DIRECTION':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-DROP_SCAN_DIRECTION 0
tName[i] eq 'DROP_SCAN_DIRECTION':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-KEEP_FACET 1 3
tName[i] eq 'KEEP_FACET':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-DROP_FACET 0
tName[i] eq 'DROP_FACET':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0) 
 
 
;FILTER PULSES WITH SIMPLE THINNING.
;-KEEP_EVERY_NTH 2
tName[i] eq 'KEEP_EVERY_NTH':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-KEEP_RANDOM_FRACTION 0.1
tName[i] eq 'KEEP_RANDOM_FRACTION':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-THIN_WITH_GRID 1.0
tName[i] eq 'THIN_WITH_GRID':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)


;TRANSFORM COORDINATES.
;-TRANSLATE_X -2.5
tName[i] eq 'TRANSLATE_X':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1) 
;-TRANSLATE_Z 42.1
tName[i] eq 'TRANSLATE_Z':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-TRANSLATE_XYZ 0.5 0.5 0.01
tName[i] eq 'TRANSLATE_XYZ':           if N_elements(parm.(i)) ne 3 then dum = self.errorParsingArgument(tName[i], parm.(i), 3)
;-SWITCH_X_Y
tName[i] eq 'SWITCH_X_Y':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-SWITCH_Y_Z
tName[i] eq 'SWITCH_Y_Z':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)
;-SWITCH_X_Z
tName[i] eq 'SWITCH_X_Z':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 0)


;TRANSFORM RAW XYZ INTEGERS.
;-TRANSLATE_RAW_Z 20
tName[i] eq 'TRANSLATE_RAW_Z':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1) 
;-TRANSLATE_RAW_XYZ 2 2 0
tName[i] eq 'TRANSLATE_RAW_XYZ':           if N_elements(parm.(i)) ne 3 then dum = self.errorParsingArgument(tName[i], parm.(i), 3)


;TRANSFORM INTENSITY.
;-SCALE_INTENSITY 2.5
tName[i] eq 'SCALE_INTENSITY':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1) 
;-TRANSLATE_INTENSITY 50
tName[i] eq 'TRANSLATE_INTENSITY':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-TRANSLATE_THEN_SCALE_INTENSITY 0.5 3.1
tName[i] eq 'TRANSLATE_THEN_SCALE_INTENSITY':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
 
;MODIFY THE CLASSIFICATION.
;-SET_CLASSIFICATION_TO 2
tName[i] eq 'SET_CLASSIFICATION_TO':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CHANGE_CLASSIFICATION_FROM_TO 2 4
tName[i] eq 'CHANGE_CLASSIFICATION_FROM_TO':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)

;MODIFY THE PULSE SOURCE ID.
;-SET_PULSE_SOURCE_TO 500
tName[i] eq 'SET_PULSE_SOURCE_TO':           if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
;-CHANGE_PULSE_SOURCE_FROM_TO 1023 1024
tName[i] eq 'CHANGE_PULSE_SOURCE_FROM_TO':           if N_elements(parm.(i)) ne 2 then dum = self.errorParsingArgument(tName[i], parm.(i), 2)
 
; Invoking the help print out
tName[i] eq 'HELP': begin
  if N_elements(parm.(i)) ne 1 then dum = self.errorParsingArgument(tName[i], parm.(i), 1)
  self.bitIsKeyword = 1
  
  end  
   
endcase

tempString = self.stringCommandCreator(tName[i], parm.(i))

if i eq 0 then commandString = tempString else commandString = commandString + ' ' + tempString
  
endfor

self.spawnCommand = self.spawnCommand + commandString



return, 1





;SUPPORTED PULSE INPUTS
;-I LIDAR.PLS
;-I LIDAR.PLZ
;-I LIDAR.LGW
;-I LIDAR.LGC
;-I LIDAR.SDF
;-I LIDAR.LAS
;-I LIDAR.LAZ
;-H
;SUPPORTED PULSE OUTPUTS
;-O PULSE.PLS
;-O PULSE.PLS -OWVZ
;-O HUMAN_READABLE.TXT
;PULSETOOLS (BY MARTIN@RAPIDLASSO.COM) VERSION 0.3 REV 11 (140921)
;USAGE:
;PULSEZIP -I IN.PLS -O OUT.PLZ
;PULSEZIP -I IN.PLZ -O OUT.PLS
;PULSEZIP -I IN.PLS -GUI
;PULSEZIP -I LINES*.PLS
;PULSEZIP -VERSION
;PULSEZIP -H
;
END



Function pulsezip::go

  self.print, 1, 'Executing command: ' + self.Spawncommand
  Spawn, self.Spawncommand
  return, 1
  
End
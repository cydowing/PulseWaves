Pro consoleOutput__define

; Definition of the data hold by the object
void = {consoleOutput, $
  consoleSetup     :0B,$                    ; Execution information output, 0:console,1:file,3:quiet
  consoleLun       :0B$                     ; Path to the LAS file
  }
  
End



Pro consoleOutput::cleanup

  Compile_opt idl2
   
  ;ptr_free, self.consoleSetup, self.consoleLun 
    
End  



Pro consoleOutput::help

  help, self, /obj
  return

End


; Function to output information on Log in the GUI
Pro consoleOutput::printLog, pointerState, code, stringText

codeString = ["::","INFO","WARNING","ERROR"]
tempString = string(codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)')
print, tempString
widget_control, (*pointerState).wtLog, set_value=tempString,/APPEND

End



; Function to output information on console
Pro consoleOutput::print, code, stringText

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]


case self.consoleSetup of
0:print, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
1:printf, self.consoleLun, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
2:
endcase



End

; Function to output information on console
Pro consoleOutput::printArray, code, stringArray

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]

n = n_elements(stringArray)
stringFormat= '(a-7,tr1,a2,tr3,'+string(n)+'(a, :, ", "))'

case self.consoleSetup of
0:print, FORMAT = stringFormat, codeString[code], codeString[0], stringArray
1:printf, self.consoleLun, codeString[code], codeString[0], stringArray, FORMAT = stringFormat
2:
endcase


End



Function consoleOutput::init,default=default,file=file,quiet=quiet

  Compile_opt idl2
  
  ; default mode
  if keyword_set(default) then self.consoleSetup = 0

  ; Open log file in user directory name lasLib.log
  if keyword_set(file) then begin
  openw, Lun, "lasLib.log",/get_lun
  self.consoleLun = Lun
  self.consoleSetup = 1
  print, "Log file mode enable"
  endif
  
  ; Enable quiet mode
  if keyword_set(quiet) then begin
  self.consoleSetup = 2
  print, "Quiet mode enable"
  endif
  
  ; Initializing the object
  return, 1

End



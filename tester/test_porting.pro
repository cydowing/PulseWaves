Pro test_porting

; Specific path separator
Result = PATH_SEP()

; determining the root path of the project of fleurdelas__define
rootdir = File_dirname(Routine_filepath('fleurdelas__define', /either))

CASE !VERSION.OS_FAMILY OF

  'unix'    : rootdir = '/'
  'Windows' : rootdir = STRMID(!DIR, 0, 2)

ENDCASE
file = FILEPATH('myapp.dat', ROOT=rootdir, SUBDIR='TEMP')

End
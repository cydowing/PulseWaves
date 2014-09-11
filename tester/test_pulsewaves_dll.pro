;    // create object in pulsewaves DLL
;    
;    pulsewaves_POINTER pulsewaves;
;    if (pulsewaves_create(&pulsewaves))
;    {
;    fprintf(stderr,"DLL ERROR: creating pulsewaves object\n");
;    byebye(true, argc==1);
;    }
;    
;    // open the reader
;    
;    pulsewaves_BOOL is_compressed;
;    
;    if (pulsewaves_reader_open(pulsewaves, file_name, &is_compressed))
;    {
;    fprintf(stderr,"DLL ERROR: opening pulsewaves reader for '%s'\n", file_name);
;    byebye(true, argc==1, pulsewaves);
;    }
;    
;    // get the header
;    
;    pulsewaves_header header;
;    
;    if (pulsewaves_header_get(pulsewaves, &header))
;    {
;    fprintf(stderr,"DLL ERROR: getting pulsewaves header from '%s'\n", file_name);
;    byebye(true, argc==1, pulsewaves);
;    }
;    
;    // report how many pulses the file has
;    
;    #ifdef _WIN32
;    fprintf(stderr,"file '%s' contains %I64d pulses\n", file_name, header.Number_of_pulses);
;    #else
;    fprintf(stderr,"file '%s' contains %lld pulses\n", file_name, header.Number_of_pulses);
;    #endif



Pro test_pulsewaves_dll

;cd, File_dirname((Routine_info('test_pulsewaves_dll', /SOURCE)).Path)

pulsewaves_POINTER = ptr_NEW()
Result = CALL_EXTERNAL('/Users/antoine/IDLWorkspace83/PulseWaves/pulsewaves/dll/pulsewaves.dll','pulsewaves_load_dll',pulsewaves_POINTER, /VERBOSE)


End
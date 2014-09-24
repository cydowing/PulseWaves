Function rapidlasso_api::init

plsObj = pulsewaves(INPUTFILE = INPUTFILE)
self.pPulsewaves = ptr_new(plsObj)
return, 1

End



;################################################
;###########  MARTIN API FUNCTIONS  #############
;################################################

Function rapidlasso_api::get_version

End

Function rapidlasso_api::create

End

Function rapidlasso_api::get_error

End

Function rapidlasso_api::destroy

End

Function rapidlasso_api::header_get

End

Function rapidlasso_api::header_set

End

Function rapidlasso_api::header_get_scanner

End

Function rapidlasso_api::header_add_scanner

End

Function rapidlasso_api::header_get_lookup_tables

End

Function rapidlasso_api::header_add_lookup_tables

End

Function rapidlasso_api::header_get_pulsedescriptor

End

Function rapidlasso_api::header_add_pulsedescriptor

End

Function rapidlasso_api::header_set_geokey_entries

End

Function rapidlasso_api::header_get_geokey_entries

End

Function rapidlasso_api::header_set_geodouble_params

End

Function rapidlasso_api::header_set_geoascii_params

End

Function rapidlasso_api::header_add_vlr

End

Function rapidlasso_api::writer_open

End

Function rapidlasso_api::writer_write_pulse

End

Function rapidlasso_api::writer_write_waves

End

Function rapidlasso_api::writer_close

End

Function rapidlasso_api::reader_open

End

Function rapidlasso_api::reader_seek_pulse

End

Function rapidlasso_api::reader_read_pulse

End

Function rapidlasso_api::reader_read_waves

End

Function rapidlasso_api::reader_close

End


Pro rapidlasso_api__define

  void = {rapidlasso_api,$
    pPulsewaves : ptr_new() $
  }

End


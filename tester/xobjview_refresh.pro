PRO xobjview_refresh_event, event
  Widget_control, event.Id, GET_UVALUE=uval
  Widget_control, event.Top, GET_UVALUE=state

  CASE uval OF
    'red': BEGIN
      Widget_control, event.Id, GET_VALUE=val
      state.lasData->GetProperty, VERT_COLOR=c
      state.lasData->SetProperty, VERT_COLOR=[c[0,*]/val,c[1,*],c[2,*]]
      XOBJVIEW, REFRESH=state.Tlb
    END
    'green': BEGIN
      Widget_control, event.Id, GET_VALUE=val
      state.lasData->GetProperty, COLOR=c
      state.lasData->SetProperty, COLOR=[c[0],val,c[2]]
      XOBJVIEW, REFRESH=state.Tlb
    END
    'blue': BEGIN
      Widget_control, event.Id, GET_VALUE=val
      state.lasData->GetProperty, COLOR=c
      state.lasData->SetProperty, COLOR=[c[0],c[1],val]
      XOBJVIEW, REFRESH=state.Tlb
    END
    'display': BEGIN
      
      Widget_control, event.Id, GET_VALUE=val
      CASE val OF
        0: BEGIN
            print, 'Using INTENSITY...'
        END
        1: BEGIN
            print, 'Using CLASSIFICATION...'
        END
        2: BEGIN
            print, 'Using ELEVATION...'
        END
        3: BEGIN
            print, 'Using FLIGHTLINE...'
        END
        4: BEGIN
            print, 'Using RETURN_NUMBER...'
        END
        5: BEGIN
            print, 'Using NUMBER OF RETURN...'
        END
        6: BEGIN
            print, 'Using SCAN ANGLE...'
        END
      ENDCASE
    END
  ENDCASE
END



PRO xobjview_refresh_cleanup, wID
  Widget_control, wID, GET_UVALUE=state
  Obj_destroy, state.lasData
END



PRO xobjview_refresh, lasData

  base = Widget_base(/COLUMN, TITLE='Adjust Object Color', $
    XOFFSET=420, XSIZE=200, GROUP=tlb)


  XOBJVIEW, lasData, TLB=tlb, GROUP=base, BACKGROUND=[0,0,0],TITLE='Fleurdelas Simple Viewer"

  red = Widget_slider(base, /DRAG, MIN=0, MAX=255, TITLE='Red', $
    UVALUE='red', VALUE=255)
  green = Widget_slider(base, /DRAG, MIN=0, MAX=255, $
    TITLE='Green', UVALUE='green', VALUE=60)
  blue = Widget_slider(base, /DRAG, MIN=0, MAX=255, $
    TITLE='Blue', UVALUE='blue', VALUE=60)
  
  values = ['intensity', 'classification', 'elevation', 'flightline', 'return number', 'number of return', 'scan angle']
  bgroup2 = CW_BGROUP(base, values, UVALUE='display',/COLUMN, /EXCLUSIVE, $
    LABEL_TOP='Color', /FRAME)
    
;  oneButton = WIDGET_BUTTON(base, UVALUE='class', VALUE='button')

  Widget_control, base, /REALIZE

  state = {lasData:lasData, tlb:tlb}
  Widget_control, base, SET_UVALUE=state

  XMANAGER, 'xobjview_refresh', base, /NO_BLOCK, $
    CLEANUP='xobjview_refresh_cleanup'
END
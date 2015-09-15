Function computeAtmAttenuation, wavelength, viz, q

Return, ( 3.92 / viz ) * ( wavelength / 0.55 )^( -q )

End


Function atmAttenationFromVisiblity, wavelength, viz, CONDITION = CONDITION,$
                                     TRANSMITTANCE = TRANSMITTANCE, DECIBEL = DECIBEL

out = consoleclass()

if size(viz, /TYPE) ne 4 then viz = fix(viz, TYPE = 4)

case 1 of

viz le 6.0: begin
  
  if not keyword_set(CONDITION) then begin
    out.print, 3, 'This range of visibility requires a condition keyword...'
    out.print, 3, 'Cannot continue...'
    Return, 0
  endif
  
  case 1 of
    strlowcase(CONDITION) eq 'fog': q = 0.0
    strlowcase(CONDITION) eq 'mist': q = viz - .5
    strlowcase(CONDITION) eq 'haze': q = 0.16 * viz + 0.34
  endcase
  
  S = computeAtmAttenuation( wavelength, viz, q )

end

viz gt 6.0 and viz le 50: begin
  
  q = 1.3
  S = computeAtmAttenuation( wavelength, viz, q )

end

viz lt 50.0: begin

  q = 1.6
  S = computeAtmAttenuation( wavelength, viz, q )

end

else:

endcase

if Keyword_set(TRANSMITTANCE) then S = 10^(S)
if Keyword_set(DECIBEL) then S *= 4.34

Return, S

End
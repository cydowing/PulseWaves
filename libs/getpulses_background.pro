Function getPulses_background, inputF

  plsObj = Obj_new('pulsewaves', inputFile = inputF)
  Return, plsObj.getPulses(/ALL)

End
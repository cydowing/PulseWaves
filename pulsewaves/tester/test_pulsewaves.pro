Pro test_pulsewaves

plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/pulsewaves/data/riegl_example1.pls')
dum = plsObj.readPulses(/ALL)

End

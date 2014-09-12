Pro test_pulsewaves

plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/data/riegl_example3.pls')
;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1.pls')
dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
dum = plsObj.readPulses(/ALL)

End

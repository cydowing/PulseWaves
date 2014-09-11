Pro test_pulsewaves

plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/data/optech_example2.pls')
;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1.pls')
dum = plsObj.readPulses(/ALL)

End

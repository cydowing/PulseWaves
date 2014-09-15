Pro test_pulsewaves

;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/data/riegl_example3.pls')
;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1_amplitude_UTM.pls', /CARBOMAP_TOOLS)
dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
write_csv, 'pulsewaves_coord.csv',transpose(dum.xyz())
End




Pro test_pulsewaves

plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/data/riegl_example3.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Volumes/FLYINGTREE/Corbin Final/Export_Pulsewaves/corbin_ln6_sc1_amplitude_UTM.pls')
dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
dum = plsObj.readPulses(/ALL)
write_csv, 'pulsewaves_coord.csv',transpose(dum.xyz())
End

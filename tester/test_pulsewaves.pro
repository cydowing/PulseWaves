Pro test_pulsewaves

;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/IDLWorkspace83/PulseWaves/data/riegl_example3.pls')
;plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Volumes/FLYINGTREE/Corbin Final/Export_Pulsewaves/corbin_ln2_sc3_reflectance_UTM.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Volumes/FLYINGTREE/Corbin Final/Export_Pulsewaves/corbin_ln2_sc3_amplitude_cartesian.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Volumes/FLYINGTREE/riegl_example1.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Volumes/FLYINGTREE/Rapidlasso/TreeMaps_UTM_47_Line13.pls')
plsObj = obj_new('pulsewaves', inputFile = 'F:\Corbin Final\Export_Pulsewaves\corbin_ln2_sc3_reflectance_UTM.pls')
plsObj = obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1_amplitude_UTM.pls')
plsObj = Obj_new('pulsewaves', inputFile = '/Users/antoine/Documents/Carbomap/Project/Riegl_Multispectral_lidar/pulsewaves/corbin_ln6_sc1_reflectance_UTM.pls')

dum = plsObj.computeAnchorPoints(ptr_new(plsObj))
dum = plsObj.getAnchors()
write_csv, 'pulsewaves_coord.csv',transpose(dum.xyz())


dum = plsObj.getPulses(0)
tump = plsObj.readWaves()


End




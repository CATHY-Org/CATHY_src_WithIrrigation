#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Oct 17 12:42:06 2025

@author: B. Mary (ICA-CSIC)
"""

# In[2]:

from pyCATHY import cathy_tools
from pyCATHY.plotters import cathy_plots as cplt
import pyvista as pv
from pyCATHY.importers import cathy_inputs as in_CT
# In[13]:
# path2prj = "./dataset_oldCATHY/"  # add your local path here
# simu = cathy_tools.CATHY(dirName=path2prj,
#                     prj_name=""
#                     )

# In[13]:


# path2prj = "dataset_newCATHY/"  # add your local path here
# simu_new = cathy_tools.CATHY(dirName=path2prj,
#                     prj_name="",
#                     # version='withIrr'
#                     )

path2prj = "Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6/"  # add your local path here
simu_new = cathy_tools.CATHY(dirName=path2prj,
                    prj_name="",
                    version='withIrr'
                    )
# ss
#%%

# simu_new.update_parm()

#%%
simu_new.run_preprocessor(verbose=True)

#%%
# simu_new.update_dem_parameters()
# simu_new.dem_parameters
# 'ic_flow' 76*10= nb of layers +1
# simu_new.read_inputs('atmbc')
# simu_new.read_inputs('dem_parameters')
# simu_new.read_inputs('dem_parameters')

# simu_new.read_inputs('soil',MAXVEG=1)
# in_CT.read_inputs('parm')

# in_CT.read_parm()
# in_CT.read_parm()
#%%
# in_CT.read_hapin()
# simu_new.parm


#%%
# simu_new.update_prepo_inputs()

#%%


# simu_new.update_dem_parameters()
# simu_new.dem_parameters

#%%

simu_new.update_veg_map()

simu_new.show_input('root_map')

import numpy as np
np.unique(simu_new.veg_map)
# len(np.unique(simu_new.veg_map))

#%%
simu_new.run_processor(IPRT1=3,verbose=True)

simu_new.read_inputs('atmbc')
# simu_new.read_inputs('root_map')
# simu.update_parm(TIMPRTi=[1800,7200],
#                  VTKF=2
#                  )
# #%%
# # simu.atmbc
# # simu.parm
# # simu.update_parm(VTKF=2)

# # simu.grid3d
# # len(simu.grid3d["mesh_tetra"])

#%%
simu_new.run_processor(IPRT1=2,
                    # DTMIN=1e-2,
                    # DTMAX=1e2,
                    # DELTAT=5,
                    TRAFLAG=0,
                    verbose=True
                    )
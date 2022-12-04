ALL: CrunchMain

FFLAGS  = -w -ffpe-trap=invalid,overflow,zero   

SOURCEF = \
          crunchtype.F90\
          params.F90\
          strings.F90\
          io.F90\
          concentration.F90\
          mineral.F90\
          transport.F90\
          medium.F90\
          modflowModule.F90\
          ReadFlow.F90\
          RunTime.F90\
          solver.F90\
          temperature.F90\
          flow.F90\
          crunch_interface.F90\
          CrunchFunctions.F90\
          isotope.F90\
          NanoCrystal.F90\
          CrunchTopeDriver.F90\
          CrunchTope.F90\
          AffinityNumerical.F90\
          AllocateALL.F90\
          AllocateOS3D.F90\
          AllocateGasesGimrt.F90\
          AllocateGasesOS3D.F90\
          AllocateGIMRT.F90\
          AqueousFluxInitialize.F90\
          AqueousFluxWrite.F90\
          AqueousToBulkConvert.F90\
          AssembleLocal.F90\
          AssembleGlobal.F90\
          AverageRo.F90\
          bd_diffuse.F90\
          bd_diffuse_by_grid.F90\
          bdcalc.F90\
          bdexchange.F90\
          bdgas.F90\
          bdgas_by_grid.F90\
          bdrecalc.F90\
          bdrecalc_by_grid.F90\
          bdsurf.F90\
          breakfind.F90\
          BreakFindPlusCount.F90\
          BreakthroughInitialize.F90\
          BreakthroughWrite.F90\
          CalciteBulkStoichiometry.F90\
          CalciteStoichiometry.F90\
          CalciteStoichiometryBoundary.F90\
          CalculateDHAD.F90\
          CalculateTortuosity.F90\
          coeff.F90\
          coeff_D.F90\
          coeffCylinder.F90\
          coeffCylinder_D.F90\
          coeffCylinderDiffuse.F90\
          coeffCylinderNUFT.F90\
          coeffDiffuse.F90\
          coeffNuft.F90\
          coeffSphericalNew.F90\
          coeffSphericalNew_d.F90\
          convan.F90\
          CourantStep.F90\
          CourantStepAlt.F90\
          CPUdefinition.F90\
          CrunchPETScFinalizeSolver.F90\
          CrunchPETScInitializeChemistry.F90\
          CrunchPETScInitializeDiffusion.F90\
          CrunchPETScInitializePressure.F90\
          CrunchPETScTolerances.F90\
          database.F90\
          decay.F90\
          decbt90.F90\
          density.F90\
          dispersivity.F90\
          duan.F90\
          equilib_co2.F90\
          erosion.F90\
          ex_activity.F90\
          exactivity_init.F90\
          exchange.F90\
          exchange_init.F90\
          ExchangeConcentration.F90\
          find_condition.F90\
          find_firststring.F90\
          find_string.F90\
          FindMaxFlow.F90\
          FirstAllocation.F90\
          fit.F90\
          fitgamma.F90\
          FluxWeightedConcentrationInitialize.F90\
          FluxWeightedConcentrationWrite.F90\
          FormatForNameList.F90\
          FxTopeGlobal.F90\
          FxTopeLocal.F90\
          gamma_co2.F90\
          gamma.F90\
          gamma_init_co2.F90\
          gamma_init.F90\
          gamma_local.F90\
          gascoeffCylinder.F90\
          gasdiff.F90\
          gases.F90\
          gases_init_co2.F90\
          gases_init.F90\
          GasInjection.F90\
          GetMineralNumber.F90\
          GasPartialPressure.F90\
          GasPartialPressure_Init.F90\
          GetPrimarySpeciesNumber.F90\
          GhostCells.F90\
          GIMRTCrunchPETScTolerances.F90\
          GlobalArrayAllocation.F90\
          GlobalDensity.F90\
          graphics3d.F90\
          GraphicsKaleidagraph.F90\
          GraphicsTecplot.F90\
          GraphicsVisit.F90\
          harmonic.F90\
          InitializeCalciteStoichiometry.F90\
          isotope-read_series.F90\
          IsotopeBreakthroughInitialize.F90\
          IsotopeBreakthroughWrite.F90\
          jac_exchange.F90\
          jac_exchange_local.F90\
          jac_local.F90\
          jacexchange.F90\
          jacexchange_init.F90\
          jacgas.F90\
          jacgas_local.F90\
          jacminTope.F90\
          jacobian.F90\
          jacobian_init.F90\
          jacobian_plus.F90\
          jacpotential.F90\
          JacPotentialLocal.F90\
          jacrkin.F90\
          jacsurf.F90\
          jacsurf_init.F90\
          jacsurf_local.F90\
          keqcalc2.F90\
          keqcalc2_init_co2.F90\
          keqcalc2_init.F90\
          keqcalcGasOnly.F90\
          lubksb90.F90\
          ludcmp90.F90\
          majuscules.F90\
          mindecay.F90\
          mineralfind.F90\
          mineral_update.F90\
          MineralBreakthroughWrite.F90\
          MineralBreakthroughInitialize.F90\
          modread.F90\
          ModScan.F90\
          movie.F90\
          newfile.F90\
          nrerror.F90\
          NviewInit.F90\
          NviewOutput.F90\
          oldcon.F90\
          oldcongas.F90\
          oldkd.F90\
          oldsurf.F90\
          oldsurface.F90\
          os3d_newton.F90\
          PauseFor.F90\
          PestExchange.F90\
          PestSurface.F90\
          PestScaleOutput.F90\
          porcalc.F90\
          porperm.F90\
          Pressure.F90\
          PrimarySpeciesCheck.F90\
          ratecheck.F90\
          reactionTope.F90\
          reactkin.F90\
          read98.F90\
          read_AqueousFluxSeries.F90\
          read_BoundaryConditionsByZone.F90\
          read_bound.F90\
          read_burialfile.F90\
          read_CatabolicPath.F90\
          read_concentration.F90\
          read_condition.F90\
          read_constantflow.F90\
          read_constantgasflow.F90\
          read_ConstantTortuosity.F90\
          read_coordinates.F90\
          read_dbs.F90\
          read_decay.F90\
          read_diffusion.F90\
          read_dispersion.F90\
          read_equil.F90\
          read_erosion.F90\
          read_exchange.F90\
          readFileName.F90\
          read_flowfile.F90\
          read_FluxWeightedConcentration.F90\
          read_gasdiffusion.F90\
          read_gasflowfile.F90\
          read_gaspump.F90\
          read_graphics.F90\
          read_gravity.F90\
          read_het.F90\
          read_infiltration.F90\
          read_integer.F90\
          read_ionexchangeBS.F90\
          read_ionexchangeMIN.F90\
          read_Isotopes.F90\
          read_Kd.F90\
          read_kinetics_Bio.F90\
          read_logical.F90\
          read_master.F90\
          read_mineral.F90\
          read_minkin.F90\
          read_minseries.F90\
          read_MODFLOWfile.F90\
          read_multpar.F90\
          read_multstring.F90\
          read_nuft.F90\
          read_par.F90\
          read_permfile.F90\
          read_permx.F90\
          read_permy.F90\
          read_permz.F90\
          read_pH.F90\
          read_porosity.F90\
          read_pressureAlternative.F90\
          read_pump.F90\
          read_restart.F90\
          read_series.F90\
          read_snapshot.F90\
          read_SolidDensity.F90\
          read_speciesdiffusion.F90\
          read_steady.F90\
          read_string.F90\
          read_surface.F90\
          read_surfacecomplex.F90\
          read_temp.F90\
          read_temperature.F90\
          read_title.F90\
          read_toperatio.F90\
          read_tortuosityfile.F90\
          read_TortuosityByZone.F90\
	  read_vgn.F90\
	  read_vga.F90\
	  read_wcr.F90\
          readblock.F90\
          readbreak.F90\
          readCaseSensitive.F90\
          readCaseSensitivePar.F90\
          ReadFileNameOnly.F90\
          ReadFlowField.F90\
          readModFlowParameters.F90\
          readModFlowStress.F90\
          ReadPressureCondition.F90\
          readRecharge.F90\
          ReadSaturation.F90\
          readSingleString.F90\
          readtype1.F90\
          readtype2.F90\
          readtype3.F90\
          readtypescan1.F90\
          reallocate.F90\
          restart.F90\
          rocalc.F90\
          satcalc.F90\
          solbt90.F90\
          SolveDiffuse.F90\
          SolveDiffuseSpherical.F90\
          SolveGasDiffuse.F90\
          speciation.F90\
          species.F90\
          species_init.F90\
          SpeciesLocal.F90\
          squeeze.F90\
          sschaine.F90\
          sschaine_hyph.F90\
          StartTope.F90\
          SteadyState.F90\
          stringlen.F90\
          stringtype.F90\
          SurfaceComplex.F90\
          surf_init.F90\
          SurfaceCharge.F90\
          SurfaceCharge_init.F90\
          SurfaceConcentration.F90\
          SurfLocal.F90\
          swap.F90\
          timestep.F90\
          tot_ex.F90\
          totconc.F90\
          totconc_init.F90\
          totconc_plus.F90\
          totexchange.F90\
          totexchange_init.F90\
          totexchange_local.F90\
          totgas.F90\
          totgas_init.F90\
          totsurf.F90\
          totsurf_init.F90\
          totsurf_local.F90\
          tridag_ser.F90\
          tvd.F90\
          tvdNuft.F90\
          units_concentration.F90\
          units_distance.F90\
          units_time.F90\
          units_timeOutput.F90\
          UpdateExchanger.F90\
          velocalc.F90\
          WaterReacted.F90\
          WaterReactedNodeByNode.F90\
          xmass.F90\
          xmassNodeByNode.F90\
          xtoolInit.F90\
          xtoolOutput.F90\
          redistributeRich.F90\
          vanGenuchten.F90\
          PressureRich.F90\
          gradhRich.F90\
          nexttoSatRich.F90\
          redist_recvRich.F90\
          velocalcRich.F90\
          watercontentRich.F90\
          PressureNS.F90\
          velocalcNS.F90\
          redist_sendRich.F90\
          read_pumptimeseriesfile.F90\
          read_pumplocationsfile.F90\
          read_pump_timeseries2.F90\
          read_watertablefile.F90\
          read_watertable_timeseries.F90\
          read_transpiration.F90\
          read_evaporation.F90\
          read_timeseries2.F90\
          interp3.F90\
          read_tempreg.F90\
          read_tempregion.F90\
          read_tempts.F90\
          read_pumptimeseries.F90\
          read_pumplocations.F90\
          FRACTUREAPERTURE.F90\
          read_vgnfile.F90\
          read_vgafile.F90\
          read_wcrfile.F90\
          read_permxfile.F90\
          read_permyfile.F90\
          read_actpressurefile.F90\
          read_activecellfile.F90\
          read_eastriver_param.F90\


OBJSF  =  crunchtype.o\
          params.o\
          strings.o\
          io.o\
          concentration.o\
          mineral.o\
          transport.o\
          medium.o\
          modflowModule.o\
          ReadFlow.o\
          RunTime.o\
          solver.o\
          temperature.o\
          flow.o\
          crunch_interface.o\
          CrunchFunctions.o\
          isotope.o\
          NanoCrystal.o\
          CrunchTopeDriver.o\
          CrunchTope.o\
          AffinityNumerical.o\
          AllocateOS3D.o\
          AllocateGIMRT.o\
          AllocateALL.o\
          AllocateGasesGimrt.o\
          AllocateGasesOS3D.o\
          AqueousFluxInitialize.o\
          AqueousFluxWrite.o\
          AqueousToBulkConvert.o\
          AssembleLocal.o\
          AssembleGlobal.o\
          AverageRo.o\
          bd_diffuse.o\
          bd_diffuse_by_grid.o\
          bdcalc.o\
          bdexchange.o\
          bdgas.o\
          bdgas_by_grid.o\
          bdrecalc.o\
          bdrecalc_by_grid.o\
          bdsurf.o\
          breakfind.o\
          BreakFindPlusCount.o\
          BreakthroughInitialize.o\
          BreakthroughWrite.o\
          CalciteBulkStoichiometry.o\
          CalciteStoichiometry.o\
          CalciteStoichiometryBoundary.o\
          CalculateDHAD.o\
          CalculateTortuosity.o\
          coeff.o\
          coeff_D.o\
          coeffCylinder.o\
          coeffCylinder_D.o\
          coeffCylinderDiffuse.o\
          coeffCylinderNUFT.o\
          coeffDiffuse.o\
          coeffNuft.o\
          coeffSphericalNew.o\
          coeffSphericalNew_d.o\
          convan.o\
          CourantStep.o\
          CourantStepAlt.o\
          CPUdefinition.o\
          CrunchPETScFinalizeSolver.o\
          CrunchPETScInitializeChemistry.o\
          CrunchPETScInitializeDiffusion.o\
          CrunchPETScInitializePressure.o\
          CrunchPETScTolerances.o\
          database.o\
          decay.o\
          decbt90.o\
          density.o\
          dispersivity.o\
          duan.o\
          equilib_co2.o\
          erosion.o\
          ex_activity.o\
          exactivity_init.o\
          exchange.o\
          exchange_init.o\
          ExchangeConcentration.o\
          find_condition.o\
          find_firststring.o\
          find_string.o\
          FindMaxFlow.o\
          FirstAllocation.o\
          fit.o\
          fitgamma.o\
          FluxWeightedConcentrationInitialize.o\
          FluxWeightedConcentrationWrite.o\
          FormatForNameList.o\
          FxTopeGlobal.o\
          FxTopeLocal.o\
          gamma_co2.o\
          gamma.o\
          gamma_init_co2.o\
          gamma_init.o\
          gamma_local.o\
          gascoeffCylinder.o\
          gasdiff.o\
          gases.o\
          gases_init_co2.o\
          gases_init.o\
          GasInjection.o\
          GetMineralNumber.o\
          GasPartialPressure.o\
          GasPartialPressure_Init.o\
          GetPrimarySpeciesNumber.o\
          GhostCells.o\
          GIMRTCrunchPETScTolerances.o\
          GlobalArrayAllocation.o\
          GlobalDensity.o\
          graphics3d.o\
          GraphicsKaleidagraph.o\
          GraphicsTecplot.o\
          GraphicsVisit.o\
          harmonic.o\
          InitializeCalciteStoichiometry.o\
          isotope-read_series.o\
          IsotopeBreakthroughInitialize.o\
          IsotopeBreakthroughWrite.o\
          jac_exchange.o\
          jac_exchange_local.o\
          jac_local.o\
          jacexchange.o\
          jacexchange_init.o\
          jacgas.o\
          jacgas_local.o\
          jacminTope.o\
          jacobian.o\
          jacobian_init.o\
          jacobian_plus.o\
          jacpotential.o\
          JacPotentialLocal.o\
          jacrkin.o\
          jacsurf.o\
          jacsurf_init.o\
          jacsurf_local.o\
          keqcalc2.o\
          keqcalc2_init_co2.o\
          keqcalc2_init.o\
          keqcalcGasOnly.o\
          lubksb90.o\
          ludcmp90.o\
          majuscules.o\
          mindecay.o\
          mineralfind.o\
          mineral_update.o\
          MineralBreakthroughWrite.o\
          MineralBreakthroughInitialize.o\
          modread.o\
          ModScan.o\
          movie.o\
          newfile.o\
          nrerror.o\
          NviewInit.o\
          NviewOutput.o\
          oldcon.o\
          oldcongas.o\
          oldkd.o\
          oldsurf.o\
          oldsurface.o\
          os3d_newton.o\
          PauseFor.o\
          PestExchange.o\
          PestSurface.o\
          PestScaleOutput.o\
          porcalc.o\
          porperm.o\
          Pressure.o\
          PrimarySpeciesCheck.o\
          ratecheck.o\
          reactionTope.o\
          reactkin.o\
          read98.o\
          read_AqueousFluxSeries.o\
          read_BoundaryConditionsByZone.o\
          read_bound.o\
          read_burialfile.o\
          read_CatabolicPath.o\
          read_concentration.o\
          read_condition.o\
          read_ConstantTortuosity.o\
          read_constantflow.o\
          read_constantgasflow.o\
          read_coordinates.o\
          read_dbs.o\
          read_decay.o\
          read_diffusion.o\
          read_dispersion.o\
          read_equil.o\
          read_erosion.o\
          read_exchange.o\
          readFileName.o\
          read_flowfile.o\
          read_FluxWeightedConcentration.o\
          read_gasdiffusion.o\
          read_gasflowfile.o\
          read_gaspump.o\
          read_graphics.o\
          read_gravity.o\
          read_het.o\
          read_infiltration.o\
          read_integer.o\
          read_ionexchangeBS.o\
          read_ionexchangeMIN.o\
          read_Isotopes.o\
          read_Kd.o\
          read_kinetics_Bio.o\
          read_logical.o\
          read_master.o\
          read_mineral.o\
          read_minkin.o\
          read_minseries.o\
          read_MODFLOWfile.o\
          read_multpar.o\
          read_multstring.o\
          read_nuft.o\
          read_par.o\
          read_permfile.o\
          read_permx.o\
          read_permy.o\
          read_permz.o\
          read_pH.o\
          read_porosity.o\
          read_pressureAlternative.o\
          read_pump.o\
          read_restart.o\
          read_series.o\
          read_snapshot.o\
          read_SolidDensity.o\
          read_speciesdiffusion.o\
          read_steady.o\
          read_string.o\
          read_surface.o\
          read_surfacecomplex.o\
          read_temp.o\
          read_temperature.o\
          read_title.o\
          read_toperatio.o\
          read_tortuosityfile.o\
          read_TortuosityByZone.o\
	  read_vgn.o\
	  read_vga.o\
	  read_wcr.o\
          readblock.o\
          readbreak.o\
          readCaseSensitive.o\
          readCaseSensitivePar.o\
          ReadFileNameOnly.o\
          ReadFlowField.o\
          readModFlowParameters.o\
          readModFlowStress.o\
          ReadPressureCondition.o\
          readRecharge.o\
          ReadSaturation.o\
          readSingleString.o\
          readtype1.o\
          readtype2.o\
          readtype3.o\
          readtypescan1.o\
          reallocate.o\
          restart.o\
          rocalc.o\
          satcalc.o\
          solbt90.o\
          SolveDiffuse.o\
          SolveDiffuseSpherical.o\
          SolveGasDiffuse.o\
          speciation.o\
          species.o\
          species_init.o\
          SpeciesLocal.o\
          squeeze.o\
          sschaine.o\
          sschaine_hyph.o\
          StartTope.o\
          SteadyState.o\
          stringlen.o\
          stringtype.o\
          SurfaceComplex.o\
          surf_init.o\
          SurfaceCharge.o\
          SurfaceCharge_init.o\
          SurfaceConcentration.o\
          SurfLocal.o\
          swap.o\
          timestep.o\
          tot_ex.o\
          totconc.o\
          totconc_init.o\
          totconc_plus.o\
          totexchange.o\
          totexchange_init.o\
          totexchange_local.o\
          totgas.o\
          totgas_init.o\
          totsurf.o\
          totsurf_init.o\
          totsurf_local.o\
          tridag_ser.o\
          tvd.o\
          tvdNuft.o\
          units_concentration.o\
          units_distance.o\
          units_time.o\
          units_timeOutput.o\
          UpdateExchanger.o\
          velocalc.o\
          WaterReacted.o\
          WaterReactedNodeByNode.o\
          xmass.o\
          xmassNodeByNode.o\
          xtoolInit.o\
          xtoolOutput.o\
          redistributeRich.o\
          vanGenuchten.o\
          PressureRich.o\
          gradhRich.o\
          nexttoSatRich.o\
          redist_recvRich.o\
          velocalcRich.o\
          watercontentRich.o\
          PressureNS.o\
          velocalcNS.o\
          redist_sendRich.o\
          read_pumptimeseriesfile.o\
          read_pumplocationsfile.o\
          read_pump_timeseries2.o\
          read_watertablefile.o\
          read_watertable_timeseries.o\
          interp3.o\
          read_transpiration.o\
          read_evaporation.o\
          read_timeseries2.o\
          read_tempreg.o\
          read_tempregion.o\
          read_tempts.o\
          read_pumptimeseries.o\
          read_pumplocations.o\
          FRACTUREAPERTURE.o\
          read_vgnfile.o\
          read_vgafile.o\
          read_wcrfile.o\
          read_permxfile.o\
          read_permyfile.o\
          read_actpressurefile.o\
          read_activecellfile.o\
          read_eastriver_param.o\

LOCDIR   = ${CrunchTope_Dir}

include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules

CrunchMain : ${OBJSF} chkopts
	-${FLINKER} -o CrunchTope ${OBJSF} ${PETSC_FORTRAN_LIB} ${PETSC_LIB} ${FFLAGS}

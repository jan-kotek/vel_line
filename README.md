# vel_line
HOW TO USE VEL_LINE 
-------------------
1)set folder of output
2)open emission profile
3)click on the left and right wing of the emission profile to remove noise
4)program should write output into file "parameters.par" and display and
save graph to "graphs" folder in PNG.

! recomended to set in header - wavelenght of analyzed line (default -
H-alpha) !
                              - path of output folder (nameout=)        
....................


INPUT : emission profile - array of intensities and wavelength
OUTPUT: Parameters of function: Gaussian:       G, max. intensity, lambda max.,HWHM, integrated intensity,dopp,CHI2
                                Lorentzian:     L, max. intensity, lambda max.,HWHM, integrated intensity,dopp,CHI2
				Voigt:		V, max. intensity, gaussian parameter, damping parameter, integrated intensity, dopp, chi2

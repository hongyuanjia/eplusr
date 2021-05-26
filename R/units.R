#' @importFrom data.table fread
NULL

# reg_custom_units {{{
reg_custom_units <- function () {
    tryCatch(
        {
            if (utils::packageVersion("units") < 0.7) {
                units::install_symbolic_unit("person")
                units::install_symbolic_unit("dollar")
                units::install_symbolic_unit("thousandths")
                units::install_conversion_constant("Wh", "J", 3.6E3)
                units::install_conversion_constant("inH2O", "inch_H2O_39F", 1)
            } else {
                units::install_unit("person")
                units::install_unit("dollar")
                units::install_unit("thousandths")
                units::install_unit("Wh", "3.6E3 J")
                units::install_unit("inH2O", "1 inch_H2O_39F")
            }
            TRUE
        },
        warning = function (w) NULL,
        error = function (e) NULL
    )
}
# }}}

# FIELD_UNIT_TABLE {{{
# nocov start
FIELD_UNIT_TABLE <- fread(
    "
    si_name                        	 si_standard_name 	 ip_name                        	 ip_standard_name
    m                              	 m                	 ft                             	 ft
    m                              	 m                	 in                             	 in
    W                              	 W                	 Btu/h                          	 Btu/hr
    W                              	 W                	 W                              	 W
    m3/s                           	 m^3/s            	 ft3/min                        	 ft^3/min
    m3/s                           	 m^3/s            	 gal/min                        	 gallon/min
    C                              	 degC             	 F                              	 degF
    kg/J                           	 kg/J             	 lb/Btu                         	 lb/Btu
    Pa                             	 Pa               	 psi                            	 psi
    Pa                             	 Pa               	 inHg                           	 inHg
    Pa                             	 Pa               	 inH2O                          	 inH2O
    Pa                             	 Pa               	 ftH2O                          	 ftH2O
    W/m-K                          	 W/m/degK         	 Btu-in/h-ft2-F                 	 Btu*in/h/ft^2/degF
    W/K                            	 W/degK           	 Btu/h-F                        	 Btu/h/degF
    deltaC                         	 degC             	 deltaF                         	 degF
    m2                             	 m^2              	 ft2                            	 ft^2
    K                              	 degK             	 R                              	 degR
    (kg/s)/W                       	 (kg/s)/W         	 (lbm/sec)/(Btu/hr)             	 (lb*m/sec)/(Btu/hr)
    J/kg                           	 J/kg             	 Btu/lb                         	 Btu/lb
    kgWater/kgDryAir               	 kg/kg            	 lbWater/lbDryAir               	 lb/lb
    kJ/kg                          	 kJ/kg            	 Btu/lb                         	 Btu/lb
    lux                            	 lux              	 foot-candles                   	 footcandle
    kg/m3                          	 kg/m^3           	 lb/ft3                         	 lb/ft^3
    kg/s                           	 kg/s             	 lb/s                           	 lb/s
    kg/s-m                         	 kg/s/m           	 lb/s-ft                        	 lb/s/ft
    m3                             	 m^3              	 ft3                            	 ft^3
    m3                             	 m^3              	 gal                            	 gallon
    W/m2-K                         	 W/m^2/degK       	 Btu/h-ft2-F                    	 Btu/h/ft^2/degF
    1/m                            	 1/m              	 1/ft                           	 1/ft
    J/kg-K                         	 J/kg/degK        	 Btu/lb-F                       	 Btu/lb/degF
    J/m3-K                         	 J/m^3/degK       	 Btu/ft3-F                      	 Btu/ft^3/degF
    m/s                            	 m/s              	 ft/min                         	 ft/min
    m/s                            	 m/s              	 miles/hr                       	 miles/hr
    m2-K/W                         	 m^2*degK/W       	 ft2-F-hr/Btu                   	 ft^2*degF*hr/Btu
    W/m2                           	 W/m^2            	 Btu/h-ft2                      	 Btu/h/ft^2
    A/K                            	 A/degK           	 A/F                            	 A/degF
    g/kg                           	 g/kg             	 grains/lb                      	 grains/lb
    g/m-s                          	 g/m/s            	 lb/ft-s                        	 lb/ft/s
    g/m-s-K                        	 g/m/s/degK       	 lb/ft-s-F                      	 lb/ft/s/degF
    J/K                            	 J/degK           	 Btu/F                          	 Btu/degF
    J/kg-K2                        	 J/kg/degK^2      	 Btu/lb-F2                      	 Btu/lb/degF^2
    J/m3                           	 J/m^3            	 Btu/ft3                        	 Btu/ft^3
    kg/kg-K                        	 kg/kg/degK       	 lb/lb-F                        	 lb/lb/degF
    kPa                            	 kPa              	 psi                            	 psi
    kPa                            	 kPa              	 inHg                           	 inHg
    m2/s                           	 m^2/s            	 ft2/s                          	 ft^2/s
    m3/kg                          	 m^3/kg           	 ft3/lb                         	 ft^3/lb
    m3/m3                          	 m^3/m^3          	 ft3/ft3                        	 ft^3/ft^3
    N-s/m2                         	 N*s/m^2          	 lbf-s/ft2                      	 lbf*s/ft^2
    V/K                            	 V/degK           	 V/F                            	 V/degF
    W/m-K2                         	 W/m/degK^2       	 Btu/h-F2-ft                    	 Btu/h/degF^2/ft
    m3/s-m                         	 m^3/s/m          	 ft3/min-ft                     	 ft^3/min/ft
    deg                            	 arc_degree       	 deg                            	 arc_degree
    hr                             	 hr               	 hr                             	 hr
    A                              	 A                	 A                              	 A
    dimensionless                  	 1                	 dimensionless                  	 1
    V                              	 V                	 V                              	 V
    A/V                            	 A/V              	 A/V                            	 A/V
    eV                             	 eV               	 eV                             	 eV
    percent                        	 percent          	 percent                        	 percent
    percentage (as a real decimal) 	 1                	 percentage (as a real decimal) 	 1
    s                              	 s                	 s                              	 s
    W/m2 or deg C                  	 1                	 unknown                        	 1
    W/m2, W or deg C               	 1                	 unknown                        	 1
    1/K                            	 1/degK           	 1/F                            	 1/degF
    J/m2-K                         	 J/m^2/degK       	 Btu/ft2-F                      	 Btu/ft^2/degF
    ohms                           	 ohms             	 ohms                           	 ohms
    cycles/hr                      	 cycles/hr        	 cycles/hr                      	 cycles/hr
    kg/kg                          	 kg/kg            	 lb/lb                          	 lb/lb
    J/J                            	 J/J              	 Btu/Btu                        	 Btu/Btu
    g/GJ                           	 g/GJ             	 lb/MWh                         	 lb/MWh
    L/GJ                           	 L/GJ             	 gal/kWh                        	 gallon/kWh
    m3/GJ                          	 m^3/GJ           	 ft3/MWh                        	 ft^3/MWh
    m3/s-m2                        	 m^3/s/m^2        	 ft3/min-ft2                    	 ft^3/min/ft^2
    m3/s-person                    	 m^3/s/person     	 ft3/min-person                 	 ft^3/min/person
    W/m2-K2                        	 W/m^2/K^2        	 Btu/h-ft2-F2                   	 Btu/h/ft^2/degF^2
    g/MJ                           	 g/MJ             	 lb/MWh                         	 lb/MWh
    L/MJ                           	 L/MJ             	 gal/kWh                        	 gallon/kWh
    m3/MJ                          	 m^3/MJ           	 ft3/kWh                        	 ft^3/kWh
    W/W                            	 W/W              	 Btuh/Btuh                      	 (Btu/h)/(Btu/h)
    $/m2                           	 dollar/m^2       	 $/ft2                          	 dollar/ft^2
    $                              	 dollar           	 $                              	 dollar
    $/kW                           	 dollar/kW        	 $/(kBtu/h)                    	 dollar/(kBtu/h)
    $/m3                           	 dollar/m3        	 $/ft3                          	 dollar/ft^3
    years                          	 years            	 years                          	 years
    $/(W/K)                        	 dollar/(W/degK)  	 $/(Btu/h-F)                    	 dollar/(Btu/h/degF)
    $/(m3/s)                       	 dollar/(m^3/s)   	 $/(ft3/min)                    	 dollar/(ft^3/min)
    W/m                            	 W/m              	 Btu/h-ft                       	 Btu/hr/ft
    minutes                        	 minutes          	 minutes                        	 minutes
    cm                             	 cm               	 in                             	 in
    K/m                            	 K/m              	 F/ft                           	 degF/ft
    W/s                            	 W/s              	 W/s                            	 W/s
    kmol                           	 kmol             	 kmol                           	 kmol
    J                              	 J                	 Wh                             	 W*hr
    Wh/m2                          	 W*h/m^2          	 Wh/m2                          	 W*hr/m^2
    cd/m2                          	 cd/m^2           	 cd/m2                          	 cd/m^2
    GJ                             	 GJ               	 ton-hrs                        	 ton*hr
    days                           	 days             	 days                           	 days
    kg/m2                          	 kg/m2            	 lb/ft2                         	 lb/ft^2
    kg                             	 kg               	 lb                             	 lb
    kmol/s                         	 kmol/s           	 kmol/s                         	 kmol/s
    percent/K                      	 percent/degK     	 percent/F                      	 percent/degF
    kg/s2                          	 kg/s^2           	 lb/s2                          	 lb/s^2
    g/mol                          	 g/mol            	 lb/mol                         	 lb/mol
    deltaJ/kg                      	 J/kg             	 deltaBtu/lb                    	 Btu/lb
    person/m2                      	 person/m^2       	 person/ft2                     	 person/ft^2
    m2/person                      	 m^2/person       	 ft2/person                     	 ft^2/person
    W/person                       	 W/person         	 Btu/h-person                   	 Btu/h/person
    W/person                       	 W/person         	 W/person                       	 W/person
    W/m2                           	 W/m^2            	 W/m2                           	 W/m^2
    m3/person                      	 m^3/person       	 ft3/person                     	 ft^3/person
    m3/hr-person                   	 m^3/hr/person    	 ft3/hr-person                  	 ft^3/hr/person
    m3/m2                          	 m^3/m^2          	 ft3/ft2                        	 ft^3/ft^2
    m3/hr-m2                       	 m^3/hr/m^2       	 ft3/hr-ft2                     	 ft^3/hr/ft^2
    m3/hr                          	 m^3/hr           	 ft3/hr                         	 ft^3/hr
    s/m                            	 s/m              	 s/ft                           	 s/ft
    W/m2                           	 W/m^2            	 W/ft2                          	 W/ft^2
    m2/m                           	 m^2/m            	 ft2/ft                         	 ft^2/ft
    L/day                          	 L/day            	 pint/day                       	 pint/day
    L/kWh                          	 L/kWh            	 pint/kWh                       	 pint/kWh
    kg/Pa-s-m2                     	 kg/Pa/s/m^2      	 lb/psi-s-ft2                   	 lb/psi/s/ft^2
    m/hr                           	 m/hr             	 ft/hr                          	 ft/hr
    Mode                           	 1                	 Mode                           	 1
    Control                        	 1                	 Control                        	 1
    Availability                   	 1                	 Availability                   	 1
    rev/min                        	 revolution/min   	 rev/min                        	 revolution/min
    W/(m3/s)                       	 W/(m^3/s)        	 W/(ft3/min)                    	 W/(ft^3/min)
    W/m-K                          	 W/m/degK         	 Btu/h-ft-F                     	 Btu/h/ft/degF
    VA                             	 VA               	 VA                             	 VA
    N-m                            	 N*m              	 lbf-in                         	 lbf*in
    m3/s-W                         	 m^3/s/W          	 (ft3/min)/(Btu/h)              	 (ft3/min)/(Btu/h)
    cm2                            	 cm^2             	 inch2                          	 inch^2
    kg/m                           	 kg/m             	 lb/ft                          	 lb/ft
    Pa                             	 Pa               	 Pa                             	 Pa
    m/yr                           	 m/yr             	 inch/yr                        	 inch/yr
    1/hr                           	 1/hr             	 1/hr                           	 1/hr
    ppm                            	 ppm              	 ppm                            	 ppm
    W/m-K3                         	 W/m/degK^3       	 Btu/h-F3-ft                    	 Btu/h/degF^3/ft
    kg/m-s                         	 kg/m/s           	 kg/m-s                         	 kg/m/s
    kg/m-s-K                       	 kg/m/s/degK      	 kg/m-s-F                       	 kg/m/s/degF
    kg/m-s-K2                      	 kg/m/s/degK^2    	 kg/m-s-F2                      	 kg/m/s/degF^2
    J/kg-K3                        	 J/kg/degK^3      	 J/kg-K3                        	 J/kg/degK^3
    ms                             	 ms               	 ms                             	 ms
    Ah                             	 A*hr             	 Ah                             	 A*hr
    deltaC/hr                      	 degC/hr          	 deltaF/hr                      	 degF/hr
    micron                         	 micron           	 micron                         	 micron
    W/(m3/s)                       	 W/(m^3/s)        	 W/(gal/min)                    	 W/(gallon/min)
    W/((m3/s)-Pa)                  	 W/((m^3/s)*Pa)   	 W/((gal/min)-ftH2O)            	 W/((gallon/min)*ftH2O)
    m3/s-m                         	 m^3/s/m          	 gal/min-ft                     	 gallon/min/ft
    m3/s-W                         	 m^3/s/W          	 (gal/min)/(Btu/h)              	 (gallon/min)/(Btu/h)
    m3/person                      	 m^3/person       	 gal/person                     	 gallon/person
    m3/hr-person                   	 m^3/hr/person    	 gal/hr-person                  	 gallon/hr/person
    m3/m2                          	 m^3/m^2          	 gal/ft2                        	 gallon/ft^2
    m3/hr-m2                       	 m^3/hr/m^2       	 gal/hr-ft2                     	 gallon/hr/ft^2
    m3/hr                          	 m^3/hr           	 gal/hr                         	 gallon/hr
    W/((m3/s)-Pa)                  	 W/((m3/s)*Pa)    	 W/((ft3/min)-inH2O)            	 W/((ft^3/min)*inH2O)
    "
)
# nocov end
# }}}

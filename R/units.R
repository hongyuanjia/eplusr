#' @importFrom data.table data.table
#' @importFrom units install_symbolic_unit install_conversion_constant
NULL

# reg_custom_units {{{
reg_custom_units <- function () {
    tryCatch(
        {
            install_symbolic_unit("person")
            install_symbolic_unit("dollar")
            install_conversion_constant("Wh", "J", 3.6E3)
            install_conversion_constant("inH2O", "inch_H2O_39F", 1)
        },
        warning = function (w) NULL,
        error = function (e) NULL
    )
}
# }}}

# UNIT_CONV_TABLE {{{
UNIT_CONV_TABLE <- fread(
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
    J                              	 J                	 Wh                             	 Wh
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
# }}}

# tablular_units_record {{{
# SI names {{{
si_name <- character(length = 115)
si_name[1] <- "%"
si_name[2] <- "?C"
si_name[3] <- "0=OFF 1=ON"
si_name[4] <- "0-NO  1-YES"
si_name[5] <- "1-YES 0-NO"
si_name[6] <- "A"
si_name[7] <- "ACH"
si_name[8] <- "ACH"
si_name[9] <- "BASE 10C"
si_name[10] <- "BASE 18C"
si_name[11] <- "C"
si_name[12] <- "CD/M2"
si_name[13] <- "DEG"
si_name[14] <- "FRAC"
si_name[15] <- "HOUR"
si_name[16] <- "HOURS"
si_name[17] <- "HR"
si_name[18] <- "HRS"
si_name[19] <- "J"
si_name[20] <- "J"
si_name[21] <- "J"
si_name[22] <- "J"
si_name[23] <- "J"
si_name[24] <- "J"
si_name[25] <- "J/KG"
si_name[26] <- "J/KG H2O"
si_name[27] <- "J/M2"
si_name[28] <- "K/M"
si_name[29] <- "KG"
si_name[30] <- "KG/KG"
si_name[31] <- "KG/M3"
si_name[32] <- "KG/S"
si_name[33] <- "KGWATER/KGAIR"
si_name[34] <- "KGWATER/SEC"
si_name[35] <- "KMOL/S"
si_name[36] <- "KMOL/SEC"
si_name[37] <- "KWH"
si_name[38] <- "L"
si_name[39] <- "L"
si_name[40] <- "LUM/W"
si_name[41] <- "LUX"
si_name[42] <- "M"
si_name[43] <- "M"
si_name[44] <- "M/S"
si_name[45] <- "M/S"
si_name[46] <- "M2"
si_name[47] <- "M2/PERSON"
si_name[48] <- "M3"
si_name[49] <- "M3"
si_name[50] <- "M3/M2"
si_name[51] <- "M3/S"
si_name[52] <- "M3/S"
si_name[53] <- "M3/S-M2"
si_name[54] <- "M3/S-PERSON"
si_name[55] <- "M3/S-PERSON"
si_name[56] <- "PA"
si_name[57] <- "PA"
si_name[58] <- "PA"
si_name[59] <- "PA"
si_name[60] <- "PA"
si_name[61] <- "PA"
si_name[62] <- "PA"
si_name[63] <- "PA"
si_name[64] <- "S"
si_name[65] <- "V"
si_name[66] <- "W"
si_name[67] <- "W"
si_name[68] <- "W"
si_name[69] <- "W"
si_name[70] <- "W"
si_name[71] <- "W/KG"
si_name[72] <- "W/KG H2O"
si_name[73] <- "W/K"
si_name[74] <- "W/M2"
si_name[75] <- "W/M2"
si_name[76] <- "W/M2-C"
si_name[77] <- "W/M2-K"
si_name[78] <- "W/W"
si_name[79] <- "deltaC"
si_name[80] <- "KJ/KG"
si_name[81] <- "W-S/M3"
si_name[82] <- "W-S/M3"
si_name[83] <- "~~$~~/m2"
si_name[84] <- "GJ"
si_name[85] <- "GJ"
si_name[86] <- "GJ"
si_name[87] <- "GJ"
si_name[88] <- "GJ"
si_name[89] <- "GJ"
si_name[90] <- "GJ"
si_name[91] <- "MJ/m2"
si_name[92] <- "MJ/m2"
si_name[93] <- "MJ/m2"
si_name[94] <- "MJ/m2"
si_name[95] <- "Invalid/Undefined"
si_name[96] <- ""
si_name[97] <- "W/C"
si_name[98] <- "DAY"
si_name[99] <- "MIN"
si_name[100] <- "HR/WK"
si_name[101] <- "$"
si_name[102] <- "$/UNIT ENERGY"
si_name[103] <- "KW"
si_name[104] <- "KGWATER/KGDRYAIR"
si_name[105] <- " "
si_name[106] <- "AH"
si_name[107] <- "CLO"
si_name[108] <- "J/KG-K"
si_name[109] <- "J/KGWATER"
si_name[110] <- "KGWATER/S"
si_name[111] <- "PPM"
si_name[112] <- "RAD"
si_name[113] <- "REV/MIN"
si_name[114] <- "NM"
si_name[115] <- "BTU/W-H"
# }}}

# IP names {{{
ip_name <- character(length = 115)
ip_name[1] <- "%"
ip_name[2] <- "F"
ip_name[3] <- "0=Off 1=On"
ip_name[4] <- "0-No  1-Yes"
ip_name[5] <- "1-Yes 0-No"
ip_name[6] <- "A"
ip_name[7] <- "ACH"
ip_name[8] <- "ach"
ip_name[9] <- "base 50F"
ip_name[10] <- "base 65F"
ip_name[11] <- "F"
ip_name[12] <- "cd/in2"
ip_name[13] <- "deg"
ip_name[14] <- "Frac"
ip_name[15] <- "Hour"
ip_name[16] <- "Hours"
ip_name[17] <- "hr"
ip_name[18] <- "hrs"
ip_name[19] <- "kBtu"
ip_name[20] <- "kWh"
ip_name[21] <- "therm"
ip_name[22] <- "MMBtu"
ip_name[23] <- "Wh"
ip_name[24] <- "ton-hrs"
ip_name[25] <- "Btu/lb"
ip_name[26] <- "Btu/lbWater"
ip_name[27] <- "kBtu/sqft"
ip_name[28] <- "F/ft"
ip_name[29] <- "lb"
ip_name[30] <- "lb/lb"
ip_name[31] <- "lb/ft3"
ip_name[32] <- "lb/s"
ip_name[33] <- "lbWater/lbAir"
ip_name[34] <- "lbWater/s"
ip_name[35] <- "kmol/s"
ip_name[36] <- "kmol/sec"
ip_name[37] <- "kWh"
ip_name[38] <- "gal"
ip_name[39] <- "ft3"
ip_name[40] <- "lum/W"
ip_name[41] <- "foot-candles"
ip_name[42] <- "ft"
ip_name[43] <- "in"
ip_name[44] <- "ft/min"
ip_name[45] <- "miles/hr"
ip_name[46] <- "ft2"
ip_name[47] <- "ft2/person"
ip_name[48] <- "ft3"
ip_name[49] <- "gal"
ip_name[50] <- "f3/f2"
ip_name[51] <- "ft3/min"
ip_name[52] <- "gal/min"
ip_name[53] <- "ft3/min-ft2"
ip_name[54] <- "ft3/min-person"
ip_name[55] <- "gal/min-person"
ip_name[56] <- "psi"
ip_name[57] <- "inHg"
ip_name[58] <- "inH2O"
ip_name[59] <- "ftH2O"
ip_name[60] <- "psi"
ip_name[61] <- "inHg"
ip_name[62] <- "inH2O"
ip_name[63] <- "ftH2O"
ip_name[64] <- "s"
ip_name[65] <- "V"
ip_name[66] <- "Btu/h"
ip_name[67] <- "W"
ip_name[68] <- "kW"
ip_name[69] <- "kBtuh"
ip_name[70] <- "ton"
ip_name[71] <- "kBtuh/lb"
ip_name[72] <- "kBtuh/lb"
ip_name[73] <- "Btu/h-F"
ip_name[74] <- "Btu/h-ft2"
ip_name[75] <- "kBtuh/ft2"
ip_name[76] <- "Btu/h-ft2-F"
ip_name[77] <- "Btu/h-ft2-F"
ip_name[78] <- "Btuh/Btuh"
ip_name[79] <- "deltaF"
ip_name[80] <- "Btu/lb"
ip_name[81] <- "W-min/ft3"
ip_name[82] <- "W-min/gal"
ip_name[83] <- "~~$~~/ft2"
ip_name[84] <- "kBtu"
ip_name[85] <- "kWh"
ip_name[86] <- "kWh"
ip_name[87] <- "therm"
ip_name[88] <- "MMBtu"
ip_name[89] <- "Wh"
ip_name[90] <- "ton-hrs"
ip_name[91] <- "kWh/ft2"
ip_name[92] <- "kBtu/ft2"
ip_name[93] <- "kBtu/ft2"
ip_name[94] <- "kWh/m2"
ip_name[95] <- "Invalid/Undefined"
ip_name[96] <- ""
ip_name[97] <- "Btu/h-F"
ip_name[98] <- "day"
ip_name[99] <- "min"
ip_name[100] <- "hr/wk"
ip_name[101] <- "$"
ip_name[102] <- "$/unit energy"
ip_name[103] <- "kW"
ip_name[104] <- "lbWater/lbDryAir"
ip_name[105] <- " "
ip_name[106] <- "Ah"
ip_name[107] <- "clo"
ip_name[108] <- "Btu/lbm-R"
ip_name[109] <- "Btu/lbWater"
ip_name[110] <- "lbWater/s"
ip_name[111] <- "ppm"
ip_name[112] <- "rad"
ip_name[113] <- "rev/min"
ip_name[114] <- "lbf-ft"
ip_name[115] <- "Btu/W-h"
# }}}

# mult {{{
mult <- double(length = 115)
mult[1] <- 1.0
mult[2] <- 1.8
mult[3] <- 1.0
mult[4] <- 1.0
mult[5] <- 1.0
mult[6] <- 1.0
mult[7] <- 1.0
mult[8] <- 1.0
mult[9] <- 1.8
mult[10] <- 1.8
mult[11] <- 1.8
mult[12] <- 0.000645160041625726
mult[13] <- 1.0
mult[14] <- 1.0
mult[15] <- 1.0
mult[16] <- 1.0
mult[17] <- 1.0
mult[18] <- 1.0
mult[19] <- 0.00000094845
mult[20] <- 0.000000277778
mult[21] <- 0.0000000094845
mult[22] <- 0.00000000094845
mult[23] <- 0.000277777777777778
mult[24] <- 0.0000000789847
mult[25] <- 0.00042956
mult[26] <- 0.0000004302105
mult[27] <- 0.00000008811404
mult[28] <- 0.54861322767449
mult[29] <- 2.2046
mult[30] <- 1.0
mult[31] <- 0.062428
mult[32] <- 2.2046
mult[33] <- 1.0
mult[34] <- 2.2046
mult[35] <- 1.0
mult[36] <- 1.0
mult[37] <- 1.0
mult[38] <- 0.264172037284185
mult[39] <- 0.0353146624712848
mult[40] <- 1.0
mult[41] <- 0.092902267
mult[42] <- 3.281
mult[43] <- 39.37
mult[44] <- 196.86
mult[45] <- 2.2369
mult[46] <- 10.764961
mult[47] <- 10.764961
mult[48] <- 35.319837041
mult[49] <- 264.172
mult[50] <- 3.281
mult[51] <- 2118.6438
mult[52] <- 15852.0
mult[53] <- 196.85
mult[54] <- 2118.6438
mult[55] <- 15852.0
mult[56] <- 0.0001450377
mult[57] <- 0.00029613
mult[58] <- 0.00401463
mult[59] <- 0.00033455
mult[60] <- 0.0001450377
mult[61] <- 0.00029613
mult[62] <- 0.00401463
mult[63] <- 0.00033455
mult[64] <- 1.0
mult[65] <- 1.0
mult[66] <- 3.412
mult[67] <- 1.0
mult[68] <- 0.001
mult[69] <- 0.00341442
mult[70] <- 0.0002843333
mult[71] <- 0.001547673
mult[72] <- 0.001547673
mult[73] <- 1.8987
mult[74] <- 0.316954237
mult[75] <- 0.000316954237
mult[76] <- 0.176085687
mult[77] <- 0.176085687
mult[78] <- 1.0
mult[79] <- 1.8
mult[80] <- 0.42956
mult[81] <- 1.0 / 2118.6438
mult[82] <- 1.0 / 15852
mult[83] <- 1.0 / 10.764961
mult[84] <- 0.00000094845 * 1000000000
mult[85] <- 0.000000277778 * 1000000000
mult[86] <- 0.000000277778 * 1000000000
mult[87] <- 0.0000000094845 * 1000000000
mult[88] <- 0.00000000094845 * 1000000000
mult[89] <- 0.000277777777777778 * 1000000000
mult[90] <- 0.0000000789847 * 1000000000
mult[91] <- 0.277777777777778 / 10.764961
mult[92] <- 0.94708628903179 / 10.764961
mult[93] <- 0.94708628903179 / 10.764961
mult[94] <- 0.27777777777778
mult[95] <- 1.0
mult[96] <- 1.0
mult[97] <- 1.8987
mult[98] <- 1.0
mult[99] <- 1.0
mult[100] <- 1.0
mult[101] <- 1.0
mult[102] <- 1.0
mult[103] <- 1.0
mult[104] <- 1.0
mult[105] <- 1.0
mult[106] <- 1.0
mult[107] <- 1.0
mult[108] <- 0.000238845896627
mult[109] <- 0.0000004302105
mult[110] <- 2.2046
mult[111] <- 1.0
mult[112] <- 1.0
mult[113] <- 1.0
mult[114] <- 0.737562149277
mult[115] <- 1.0
# }}}

# offset {{{
offset <- double(length = 115)
offset[2] <- 32.0
offset[11] <- 32.0
offset[25] <- 7.6736
offset[80] <- 7.6736
# }}}

# hint {{{
hint <- character(115)
hint[20] <- "ELEC"
hint[21] <- "GAS"
hint[24] <- "COOL"
hint[38] <- "WATER"
hint[49] <- "WATER"
hint[52] <- "WATER"
hint[67] <- "ELEC"
hint[70] <- "COOL"
hint[82] <- "WATER"
hint[85] <- "CONSUMP"
hint[86] <- "ELEC"
hint[87] <- "GAS"
hint[90] <- "COOL"
hint[91] <- "ELEC"
hint[92] <- "GAS"
hint[92] <- "ADDITIONAL"
# }}}

# several {{{
several <- logical(115)
several[19] <- TRUE
several[20] <- TRUE
several[21] <- TRUE
several[22] <- TRUE
several[23] <- TRUE
several[24] <- TRUE
several[38] <- TRUE
several[39] <- TRUE
several[42] <- TRUE
several[43] <- TRUE
several[44] <- TRUE
several[45] <- TRUE
several[48] <- TRUE
several[49] <- TRUE
several[51] <- TRUE
several[52] <- TRUE
several[54] <- TRUE
several[55] <- TRUE
several[56] <- TRUE
several[57] <- TRUE
several[58] <- TRUE
several[59] <- TRUE
several[60] <- TRUE
several[61] <- TRUE
several[62] <- TRUE
several[63] <- TRUE
several[66] <- TRUE
several[67] <- TRUE
several[68] <- TRUE
several[69] <- TRUE
several[70] <- TRUE
several[74] <- TRUE
several[75] <- TRUE
several[81] <- TRUE
several[82] <- TRUE
several[84] <- TRUE
several[85] <- TRUE
several[86] <- TRUE
several[87] <- TRUE
several[88] <- TRUE
several[89] <- TRUE
several[90] <- TRUE
several[91] <- TRUE
several[92] <- TRUE
several[93] <- TRUE
several[94] <- TRUE
# }}}

tabular_unit_conv_table <- data.table(
    si_name, ip_name, mult, offset, hint, several
)
# }}}

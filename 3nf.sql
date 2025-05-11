-- Create a new normalized table for the chemistry data
CREATE TABLE normalized_chemistry (
    waterbodyID INTEGER,
    sampleID REAL,
    inlet_outlet TEXT,
    parameter TEXT,
    value REAL
);

-- Insert WaterTemperature_C values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'WaterTemperature_C', WaterTemperature_C
FROM water_samples
WHERE WaterTemperature_C IS NOT NULL;

-- Insert Air_Temperature_C values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Air_Temperature_C', Air_Temperature_C
FROM water_samples
WHERE Air_Temperature_C IS NOT NULL;

-- Insert Pressure_Bar values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Pressure_Bar', Pressure_Bar
FROM water_samples
WHERE Pressure_Bar IS NOT NULL;

-- Insert pH values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'pH', pH
FROM water_samples
WHERE pH IS NOT NULL;

-- Insert O2_pct values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'O2_pct', O2_pct
FROM water_samples
WHERE O2_pct IS NOT NULL;

-- Insert O2_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'O2_mg_l', O2_mg_l
FROM water_samples
WHERE O2_mg_l IS NOT NULL;

-- Insert EC_us_cm values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'EC_us_cm', EC_us_cm
FROM water_samples
WHERE EC_us_cm IS NOT NULL;

-- Insert Chla values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Chla', Chla
FROM water_samples
WHERE Chla IS NOT NULL;

-- Insert Alkalinitet_mekv_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Alkalinitet_mekv_l', Alkalinitet_mekv_l
FROM water_samples
WHERE Alkalinitet_mekv_l IS NOT NULL;

-- Insert Kond_ms_m_25_C values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Kond_ms_m_25_C', Kond_ms_m_25_C
FROM water_samples
WHERE Kond_ms_m_25_C IS NOT NULL;

-- Insert PO4_P filtr_mg_l values (note the space in column name requires quotes)
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'PO4_P filtr_mg_l', "PO4_P filtr_mg_l"
FROM water_samples
WHERE "PO4_P filtr_mg_l" IS NOT NULL;

-- Insert TotP_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'TotP_mg_l', TotP_mg_l
FROM water_samples
WHERE TotP_mg_l IS NOT NULL;

-- Insert Tot_P_filtr_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Tot_P_filtr_mg_l', Tot_P_filtr_mg_l
FROM water_samples
WHERE Tot_P_filtr_mg_l IS NOT NULL;

-- Insert NH4_N_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'NH4_N_mg_l', NH4_N_mg_l
FROM water_samples
WHERE NH4_N_mg_l IS NOT NULL;

-- Insert NO2_NO3_N_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'NO2_NO3_N_mg_l', NO2_NO3_N_mg_l
FROM water_samples
WHERE NO2_NO3_N_mg_l IS NOT NULL;

-- Insert TOC_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'TOC_mg_l', TOC_mg_l
FROM water_samples
WHERE TOC_mg_l IS NOT NULL;

-- Insert Tot_N_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Tot_N_mg_l', Tot_N_mg_l
FROM water_samples
WHERE Tot_N_mg_l IS NOT NULL;

-- Insert Susp_mg_l values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Susp_mg_l', Susp_mg_l
FROM water_samples
WHERE Susp_mg_l IS NOT NULL;

-- Insert Dissolved_CH4_mg_L values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Dissolved_CH4_mg_L', Dissolved_CH4_mg_L
FROM water_samples
WHERE Dissolved_CH4_mg_L IS NOT NULL;

-- Insert Dissolved_N2O_ug_L values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Dissolved_N2O_ug_L', Dissolved_N2O_ug_L
FROM water_samples
WHERE Dissolved_N2O_ug_L IS NOT NULL;

-- Insert UorV values (note: this is a text field but including for completeness)
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'UorV', NULL
FROM water_samples
WHERE UorV IS NOT NULL;

-- Insert Methanotrophs_water_sample values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Methanotrophs_water_sample', Methanotrophs_water_sample
FROM water_samples
WHERE Methanotrophs_water_sample IS NOT NULL;

-- Insert Methanotrophs_sediment_sample values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Methanotrophs_sediment_sample', Methanotrophs_sediment_sample
FROM water_samples
WHERE Methanotrophs_sediment_sample IS NOT NULL;

-- Insert Methanogens_water_sample values
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Methanogens_water_sample', Methanogens_water_sample
FROM water_samples
WHERE Methanogens_water_sample IS NOT NULL;

-- Insert Methanogens_sediment _sample values (note the space in column name requires quotes)
INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)
SELECT WaterbodyID, SampleID, InletOrOutlet, 'Methanogens_sediment_sample', "Methanogens_sediment _sample"
FROM water_samples
WHERE "Methanogens_sediment _sample" IS NOT NULL;

-- Create indexes for better performance
CREATE INDEX idx_norm_waterbody ON normalized_chemistry(waterbodyID);
CREATE INDEX idx_norm_sample ON normalized_chemistry(sampleID);
CREATE INDEX idx_norm_parameter ON normalized_chemistry(parameter);
from importlib.resources import files
import pandas as pd
import geopandas as gpd

FTAALOOKUP = pd.read_csv(files('ftypeaa.data').joinpath('ftype_to_assessment_crosswalk.csv'))

SSECT = gpd.read_file(files('ftypeaa.data.CAR_supersections').joinpath('CAR_supersections_coded.shp'))

def attributeSS(inputLayer, dissolveOn = 'Stand_ID'):
    """
    
    Returns:
        

    """
    toCrs = inputLayer.crs
    ssReproj = SSECT.to_crs(toCrs)
    ssLayer = inputLayer.sjoin(ssReproj, how='left').dissolve(by=dissolveOn).reset_index()

    # inputLayer['rep_point'] = inputLayer.centroid
    # inputPoints = inputLayer.set_geometry('rep_point')
    # ptIntersect = inputPoints.sjoin(ssReproj, how='left')
    # jl = pd.merge(inputLayer,
    #               ptIntersect[['Stand_ID', 'SSection','SS_Name2','ss_code']],
    #               left_on='Stand_ID',
    #               right_on='Stand_ID',
    #               how='left')
    # jl.drop(columns=['rep_point'], inplace=True)
    return ssLayer

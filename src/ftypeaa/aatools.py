from importlib.resources import files
import pandas as pd

FTAALOOKUP = pd.read_csv(files('ftypeaa.data').joinpath('ftype_to_assessment_crosswalk.csv'))

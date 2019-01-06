#!/usr/bin/python3

import urllib.request
import pandas as pd
import io

dataUrl = "http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data"
labelUrl = "http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data"
vendorUrl = "./data/vendordata.json"


# tried pd.read_html(), but no tables found?
def PandasFromUrl(url):
    return pd.read_csv(io.BytesIO(urllib.request.urlopen(url).read()), encoding="utf8", sep=" ", header=None)


data = PandasFromUrl(dataUrl)
data.columns = ["f" + str(i) for i in range(len(data.columns))]  # prefix feature columns with "f"
data['response'] = PandasFromUrl(labelUrl)[0]
vendors = pd.read_json(vendorUrl).sort_index()
df = data.merge(vendors, left_index=True, right_index=True).set_index('datetime')


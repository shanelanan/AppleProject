#!/usr/bin/python3

import urllib.request
import pandas as pd
import io
import sqlite3
from dfUpload import dfUpload

dataUrl = "http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data"
labelUrl = "http://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data"
vendorUrl = "./data/vendordata.json"

# column names for sqlite
newCols = {
    "datetime": "MFG_DATE",
    "mat vendor": "MAT_VENDOR",
    "part vendor": "PART_VENDOR",
    "sil vendor": "SIL_VENDOR",
    "adhs vendor": "ADHS_VENDOR",
    "sop vendor": "SOP_VENDOR",
}

def main():

    # tried pd.read_html(), but no tables found?
    def PandasFromUrl(url):
        return pd.read_csv(io.BytesIO(urllib.request.urlopen(url).read()), encoding="utf8", sep=" ", header=None)

    print("Fetching data from web and formatting...")
    data = PandasFromUrl(dataUrl)
    data.columns = ["F" + str(i) for i in range(len(data.columns))]  # prefix feature columns with "F"
    data['PASS_FAIL'] = PandasFromUrl(labelUrl)[0]
    vendors = pd.read_json(vendorUrl).sort_index()
    df = data.merge(vendors, left_index=True, right_index=True)
    df.rename(index=str, columns=newCols, inplace=True)
    df['ID'] = list(range(len(df)))

    print("Connecting to Sqlite...")
    con = sqlite3.connect("apple.db")
    dfUpload(df, con, "SAMPLE", clearTable=True)
    print("Disconnecting from Sqlite...")
    con.close()
    print("Done!")


if __name__ == '__main__':
    main()

#!/usr/bin/python3

import urllib.request
import pandas as pd
import io
import sqlite3
from datetime import datetime
from tqdm import tqdm


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


def dfUpload(df, con, table, timeStamp=True, clearTable=False, debug=False):
    if timeStamp:
        df['INSERTED_ON'] = datetime.now()

    df = df.where(pd.notnull(df), None)  # convert NaN to None, for SQL Nulls
    # just to fix pd.NaT to insert as NULLS
    for col in df.columns:
        if df[col].dtype.kind == 'M':
            df[col] = df[col].astype(object).where(df[col].notnull(), None)
            df[col] = df[col].dt.strftime('%Y-%m-%d %h:%m:%s')

    sqlColumns = '(' + ','.join([col for col in df.columns]) + ')'
    sqlValues = '(' + ','.join([':' + str(x + 1) for x in list(range(len(df.columns)))]) + ')'
    sqlInsert = "INSERT INTO %s %s VALUES %s" % (table, sqlColumns, sqlValues)
    crsr = con.cursor()

    # uploading
    if clearTable:
        crsr.execute("DELETE FROM %s" % table)

    for row in tqdm(df.values.tolist(), desc="Uploading data", unit="row"):
        if debug:
            try:
                crsr.executemany(sqlInsert, [row])
            except:
                print(row)
                pass
        else:
            crsr.executemany(sqlInsert, [row])

    con.commit()
    crsr.close()


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

import pandas as pd
from datetime import datetime
from tqdm import tqdm


def dfUpload(df, con, table, timeStamp=True, clearTable=False, debug=False):
    if timeStamp:
        df['INSERTED_ON'] = datetime.now()

    df = df.where(pd.notnull(df), None)  # convert NaN to None, for SQL Nulls
    # just to fix pd.NaT to insert as NULLS in Oracle
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

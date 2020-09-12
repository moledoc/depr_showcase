import mysql.connector

def open_conn():
    config = {
            'host':'localhost',
            'database':'mysql',
            'user':'meelis',
            'password':'&(uvh2q{b6$vCLY;'
            }

    conn = mysql.connector.connect(**config)
    if conn.is_connected():
        cursor = conn.cursor()
        print("==========================")
        print("MySQL connection is opened")
        print("==========================")
        return({'conn':conn,'cursor':cursor})
    


def close_conn(conn_info):
    conn = conn_info["conn"]
    cursor = conn_info["cursor"]
    if (conn.is_connected()):
        cursor.close()
        conn.close()
        print("==========================")
        print("MySQL connection is closed")
        print("==========================")

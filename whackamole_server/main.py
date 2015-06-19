from flask import Flask, request
from flask import json
import psycopg2
import ConfigParser, os

config = ConfigParser.ConfigParser()
config.readfp(open('db.cfg'))

db = psycopg2.connect(host   = config.get('Db','host'),
                      user   = config.get('Db','user'),
                      passwd = config.get('Db','passwd'),
                      db     = config.get('Db','db'))
cur = db.cursor()

app = Flask(__name__)
with open('db.cfg') as f:
    config = f.read()

@app.route('/')
def index():
    return "Index!"

@app.route('/game/<int:difficulty>')
def game(difficulty):
    return 'Game difficulty %d' % difficulty

@app.route('/report', methods=['POST','GET'])
def report():
    if request.method == 'POST':
        payload = request.get_json()
        try:
            cur.execute("INSERT INTO scores(username,score,difficulty) VALUES (%s,%s,%s);",
                        (payload["username"],payload["score"],payload["difficulty"]))
            db.commit()
            return "Ok!\n"
        except:
            return "SQL error"
    if request.method == 'GET':
        cur.execute("SELECT * FROM scores;")
        rows = cur.fetchall()
        a = ""
        for row in rows:
            for col in row:
                a = a + str(col) + " "
            a = a + "\n"
        return a


if __name__ == "__main__":
    app.debug = True
    app.run()

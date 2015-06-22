from flask import Flask, request, render_template, make_response, jsonify
from flask import json
import psycopg2
import ConfigParser, os

package_dir = os.path.dirname(os.path.abspath(__file__))

config = ConfigParser.ConfigParser()
config.readfp(open(package_dir + '/db.cfg'))

db = psycopg2.connect(host     = config.get('Db','host'),
                      user     = config.get('Db','user'),
                      password = config.get('Db','password'),
                      dbname   = config.get('Db','dbname'))
cur = db.cursor()

app = Flask(__name__)

@app.route('/')
def index():
    return "Index!"

@app.route('/game/<int:difficulty>')
def game(difficulty):
    return render_template('game.html', difficulty=difficulty)

@app.route('/report/<int:difficulty>', methods=['POST'])
def report():
    payload = request.get_json()
    try:
        cur.execute("INSERT INTO scores(username,score,difficulty) VALUES (%s,%s,%s);",
                    (payload["username"],payload["score"],payload["difficulty"]))
        db.commit()
        return "Ok!\n"
    except:
        return "SQL error"

@app.route('/scores/<int:difficulty>')
def scores(difficulty):
    cur.execute("""SELECT * FROM scores
                WHERE difficulty = (%s)
                ORDER BY score DESC
                LIMIT 10;""",
	str(difficulty))
    rows = cur.fetchall()
    scoresJson = []
    for row in rows:
        thisScore = {}
        thisScore["username"]   = row[0]
        thisScore["score"]      = row[1]
        thisScore["difficulty"] = row[2]
        thisScore["time"]       = row[3]
        scoresJson.append(thisScore)
    resp = make_response(json.dumps(scoresJson))
    resp.headers["Content-Type"] = "application/json"
    return resp


if __name__ == "__main__":
    app.run(host='0.0.0.0')

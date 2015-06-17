from flask import Flask
app = Flask(__name__)
with open('db.cfg') as f:
    config = f.read()

@app.route('/')
def index():
    return "Index!"

@app.route('/game/<int:difficulty>')
def game(difficulty):
    return 'Game difficulty %d' % difficulty

@app.route('/report/<')
if __name__ == "__main__":
   app.run()

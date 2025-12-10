from flask import Flask, request, jsonify
import time

app = Flask(__name__)
peers = {}

@app.route("/announce", methods=["POST"])
def announce():
    data = request.json
    pid = data["peer_id"]
    peers[pid] = {
        "ip": request.remote_addr,
        "port": data["port"],
        "name": data["name"],
        "last": time.time()
    }
    return {"status": "ok"}

@app.route("/peers")
def get_peers():
    now = time.time()
    drop = [p for p in peers if now - peers[p]["last"] > 30]
    for p in drop:
        del peers[p]
    return jsonify(peers)

@app.route("/")
def index():
    return "Super P2P Bootstrap Server Online"

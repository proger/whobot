
module.exports = (robot) ->
  robot.hear /^перекличка!$/, (msg) ->
    reply msg

  robot.respond /who is in the office?$/, (msg) ->
    reply msg

reply = (msg) ->
  child = require 'child_process'
  
  wb = child.exec '/tank/proger/whobot/whobot', cwd: "/tank/proger/whobot", (error, stdout, stderr) ->
    msg.send "whobot error\n#{error.stack}\ncode: #{error.code}, sig: #{error.signal}" if error
    msg.send(("#{n} - #{h} (#{ts})" for [n,ts,h] in JSON.parse stdout).sort().join "\n")
    msg.send(stderr) if stderr

  wb.on 'close', (code) ->
    console.log "wb exited with #{code}"

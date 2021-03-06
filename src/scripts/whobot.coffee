module.exports = (robot) ->
  robot.hear /^перекличка!$/, (msg) ->
    reply msg

  robot.respond /who is in the office?$/, (msg) ->
    reply msg

  robot.respond /pick someone$/, (msg) ->
    pick msg

reply = (msg) ->
  who (error, ppls, stderr) ->
    msg.send "whobot error\n#{error.stack}\ncode: #{error.code}, sig: #{error.signal}" if error
    msg.send(("#{n} - #{h} (#{ts})" for [n,ts,h] in ppls).sort().join "\n")
    msg.send(stderr) if stderr

Array::uniq = ->
  this.reduce ((acc, x) -> if acc[acc.length-1] == x then acc else acc.concat([x])), []

Array::any = ->
  this[Math.floor(this.length * Math.random())]

pick = (msg) ->
  who (error, ppls, stderr) ->
    msg.send "whobot error\n#{error.stack}\ncode: #{error.code}, sig: #{error.signal}" if error
    lucky = (n for [n, _, __] in ppls).sort().uniq().any()
    msg.send "#{lucky} is lucky"
    msg.send(stderr) if stderr

gettimeofday = ->
  new Date().getTime() / 1000

who = (cb) ->
  child = require 'child_process'

  start = gettimeofday()
  
  wb = child.exec 'whobot', {}, (error, stdout, stderr) ->
    cb error, JSON.parse stdout, stderr

  wb.on 'close', (code) ->
    end = gettimeofday()
    console.log "whobot finished with #{code} in #{end - start} s"

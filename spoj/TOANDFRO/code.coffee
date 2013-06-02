
eachLine = (input, cb) ->
  input.resume() if input.resume?
  input.setEncoding 'ascii'

  buffer = ''
  sendLines = ->
    while (endOfLine = buffer.indexOf '\n') > -1
      cb buffer.substring(0, endOfLine)
      buffer = buffer.substring(endOfLine+1)

  input.on 'data', (chunk) ->
    buffer += chunk
    sendLines()

  input.on 'end', ->
    cb buffer if buffer.length > 0


decode = (columns, text) ->
  rows = text.length/columns

  unreverseGetter = (n) ->
    i = (n/columns) >> 0
    j = n%columns
    j = columns - j - 1 if i%2 == 1
    text[i*columns+j]

  result = ''
  for j in [0...columns]
    for i in [0...rows]
      result += unreverseGetter(i*columns + j)
      
  result


main = ->
  wantColumns = true
  columns = -1
  eachLine process.stdin, (line) ->
    if wantColumns
      columns = line|0
    else
      process.stdout.write decode(columns, line) + "\n"
    wantColumns = !wantColumns

main()

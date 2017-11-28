let funRe = /return\s+\w+(.*);/

function getByKeys (record, keys) {
  for (let i = 0; i < keys.length; i++) {
    const key = keys[i];
    record = record[key]
  }
  return record
}

export function setIn(setter, value, record) {
  if (arguments.length < 3) {
    let args = arguments
    return function curriedSetIn() {
      return setIn(Array.prototype.concat.call(args, arguments))
    }
  }
  setter = setter.toString()
  let m = setter.match(funRe)
  if (!m) {
    throw new Error('Invalid setter:' + setter)
  }
  let path = m[1]
  let keys = []
  let start = 0
  let push = str => str && keys.push(str)
  for (let i = 0; i < path.length; i++) {
    const c = path[i]
    if (c === '.') {
      push(path.slice(start, i))
      start = i + 1
    } else if (c === '[') {
      push(path.slice(start, i))
      start = i + 2
    } else if (c === ']') {
      push(path.slice(start, i - 1))
      start = i + 1
    }
  }
  push(path.slice(start))
  let newRecord = new record.constructor()
  Object.assign(newRecord, record)
  let oldCursor = record
  let newCursor = newRecord
  for (let i = 0; i < keys.length - 1; i++) {
    const key = keys[i]
    oldCursor = oldCursor[key]
    newCursor = newCursor[key] = new oldCursor.constructor()
    Object.assign(newCursor, oldCursor)
  }
  let lastKey = keys[keys.length - 1]
  newCursor[lastKey] = value
  return newRecord
}

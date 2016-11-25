// Chat server

const ws = require('nodejs-websocket')

var uuid = 0; // TODO: Fix this

const conversation = {
  roster:  {},
  current: new Set(),
  history: []
}


const server = ws.createServer((conn) => {

 'use strict';

 console.log('New connection')
 let token = uuid++;
 conversation.current.add(conn)


  conn.on('connection', (u) => {
    console.log(`New connection: ${u}`)
  })

  conn.on('text', (str) => {
    // Echo back the message
    let message = JSON.parse(str);
    console.log(`Received ${message}`)
    switch (message.kind) {
      case 'user-joins': break;
      case 'user-leaves': break;
      default: break;
    }
  })

  conn.on('error', (err) => {
    console.log(`Something went awry: ${err}`);
  })

  conn.on('close', (code, reason) => {
    console.log(`Connection closed (${code}, ${reason})`)
  })
}).listen(8001)


function broadcast(message) {
  server.connections.forEach(conn => conn.)
}

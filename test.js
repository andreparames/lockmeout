l = require('./index.js');
eve = {"name": "mary"}
f = (function(a, b) {
    console.log("A:"+a);
    console.log("B:"+b);
    return console.log;
})
l.handler(eve, eve, f)

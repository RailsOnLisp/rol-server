var fs = require('fs');
var pagedown = require('pagedown');
var converter = new pagedown.Converter();

var print_error = function (e) {
    console.error(e);
    process.exit(2);
}

try {
  process.stdin.setEncoding('utf8');
  process.stdin.on('readable', function() {
    var data = '';
    var chunk;
    while (null !== (chunk = process.stdin.read())) {
      data += chunk;
    }
    var html = converter.makeHtml(data);
    process.stdout.write(html);
  });
} catch (e) {
  print_error(e);
}

/*
  process.stdin.on('readable', function() {
    var data = '';
    var chunk;
  while (null !== (chunk = process.stdin.read())) {
    process.stdout.write(chunk);
    data += chunk;
  }
*/

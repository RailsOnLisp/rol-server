var rw = require('rw');
var pagedown = require('pagedown');
var converter = new pagedown.Converter();

var print_error = function (e) {
    console.error(e);
    process.exit(2);
}

try {
  var data = rw.readFileSync('/dev/stdin', 'utf8');
  var html = converter.makeHtml(data);
  process.stdout.write(html);
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

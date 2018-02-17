module.exports = function() {
  this.hello = function(input) {
    return 'Hello, ' + (input || 'world') + '!'
  }
}

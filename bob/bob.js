// Very hackish: no bounds checks, etc...
function cond() {
  for (var i = 0; i < arguments.length; i++) {
    if (arguments[i][0]) { return arguments[i][1] }
  }
}

module.exports = function() {
  this.hey = function(s) {
    return cond(
      [!/\S/.test(s),                             'Fine. Be that way!'],
      [(/[A-Z]/.test(s) && s == s.toUpperCase()), 'Whoa, chill out!'  ],
      [/\?$/.test(s),                             'Sure.'             ],
      [true,                                      'Whatever.'         ]
    )
  }
}

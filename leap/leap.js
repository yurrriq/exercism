// Truthiness in JavaScript is easily exploited.
// Also, who needs whitespace?
module.exports=function(y){return!!(!(y%4)&&y%100||!(y%400))}
